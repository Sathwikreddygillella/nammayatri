-- | A fleet's auto-assign and preference settings, applied to one of its drivers in a single place: when a vehicle is
-- assigned to the driver, and when a driver who already has a vehicle becomes active in the fleet.
--
-- Only the tiers that driver's vehicle serves get auto-assign, and nothing is removed when the driver leaves the fleet.
module SharedLogic.FleetDriverSettings
  ( autoAssignTagName,
    vehicleAutoAcceptTiers,
    fleetAutoAssignTiers,
    autoAssignedTiers,
    applyFleetSettings,
    applyFleetSettingsToDriver,
    applyLockedFleetSettingsToDriver,
    removeAutoAssign,
    FleetLock (..),
    getFleetLock,
    getFleetLockWith,
  )
where

import qualified Data.Aeson as A
import qualified Data.List as DL
import qualified Data.Vector as V
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (CounterpartyType (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
import SharedLogic.Finance.WalletAccount (hasMinWalletBalance)
import qualified SharedLogic.VehicleServiceTier as SVST
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

autoAssignTagName :: Text
autoAssignTagName = "AutoAssign"

-- | The city's auto-accept tiers this vehicle serves.
vehicleAutoAcceptTiers :: [DVST.VehicleServiceTier] -> DV.Vehicle -> [ServiceTierType]
vehicleAutoAcceptTiers cityTiers vehicle =
  [ tier.serviceTierType
    | tier <- cityTiers,
      maybe False (.enabled) tier.autoAcceptanceConfig,
      tier.serviceTierType `elem` vehicle.selectedServiceTiers
  ]

-- | Tiers on the driver's AutoAssign tag.
autoAssignedTiers :: [LYT.TagNameValueExpiry] -> [Text]
autoAssignedTiers tags = case Yudhishthira.accessTagKey (LYT.TagName autoAssignTagName) (Yudhishthira.convertTags tags) of
  Just (A.String value) -> [value]
  Just (A.Array values) -> [value | A.String value <- V.toList values]
  _ -> []

-- | Applies the driver's fleet settings to the driver and this vehicle, one after another on the same tags: auto-assign
-- on the vehicle's auto-accept tiers when the fleet has it on by default (or 'forceAutoAssign', the fleet's per-driver
-- switch), then area preference off and trip-distance preference off when the fleet ignores them. No-op for a driver
-- whose fleet has no auto-assign settings, unless forced.
applyFleetSettings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m ()
applyFleetSettings forceAutoAssign cityTiers driver vehicle = do
  mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  when (forceAutoAssign || isJust mbFleetAutoAssign) $ do
    FleetTiers {autoAssignTiers = tiers, forceSelectedTiers} <- planFleetTiers forceAutoAssign mbFleetAutoAssign cityTiers driver vehicle
    let ignorePreferences = maybe False (.disableDriverAreaAndDistancePreference) mbFleetAutoAssign
        oldTags = fromMaybe [] driver.driverTag
        newTags =
          oldTags
            & addAutoAssign tiers
            & disablePreferenceIf ignorePreferences AreaPref.areaPreferenceEnabledTagName
            & disablePreferenceIf ignorePreferences AreaPref.distancePreferenceEnabledTagName
    when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
    let selectedTiers = DL.nub (vehicle.selectedServiceTiers <> forceSelectedTiers)
    when (selectedTiers /= vehicle.selectedServiceTiers) $ QVehicle.updateSelectedServiceTiers selectedTiers vehicle.driverId
    updateAutoAcceptTiers vehicle (DL.nub (fromMaybe [] vehicle.selectedAutoAcceptTiers <> tiers))
  where
    addAutoAssign [] tags = tags
    addAutoAssign tiers tags = setTag autoAssignTagName (LYT.ArrayValue (DL.nub (autoAssignedTiers tags <> map show tiers))) tags
    disablePreferenceIf ignore name tags =
      if ignore then setTag name (LYT.TextValue (AreaPref.mkEnabledTagValue False)) tags else tags

-- | The tiers 'applyFleetSettings' would give auto-assign on for this driver and vehicle (with 'forceAutoAssign' for the
-- fleet's per-driver switch). Empty means the switch has nothing to turn on.
fleetAutoAssignTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m [ServiceTierType]
fleetAutoAssignTiers forceAutoAssign cityTiers driver vehicle = do
  mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  (.autoAssignTiers) <$> planFleetTiers forceAutoAssign mbFleetAutoAssign cityTiers driver vehicle

data FleetTiers = FleetTiers
  { autoAssignTiers :: [ServiceTierType],
    -- | Tiers added to the vehicle's selected tiers (locked fleets only).
    forceSelectedTiers :: [ServiceTierType]
  }

planFleetTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r) =>
  Bool ->
  Maybe DFC.AutoAssignConfig ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m FleetTiers
planFleetTiers forceAutoAssign mbFleetAutoAssign cityTiers driver vehicle = do
  let assignsTiers = forceAutoAssign || maybe False (.defaultOn) mbFleetAutoAssign
      fleetLocksAutoAssign = maybe False (not . (.allowDriverToToggle)) mbFleetAutoAssign
      selectedTiers = vehicleAutoAcceptTiers cityTiers vehicle
  if not assignsTiers
    then pure FleetTiers {autoAssignTiers = [], forceSelectedTiers = []}
    else
      if not fleetLocksAutoAssign
        then pure FleetTiers {autoAssignTiers = selectedTiers, forceSelectedTiers = []}
        else do
          -- A fleet that doesn't let its drivers toggle gets every auto-accept tier the vehicle can serve, turned off or
          -- not, behind the same min-wallet gate as the driver's own tier update (postDriverUpdateServiceTiers): a tier the
          -- wallet can't cover isn't force-selected and gets no auto-accept.
          driverTiers <- SVST.fetchVehicleTierForDriverWithUsageRestriction SVST.AllowedVariants Nothing (Just vehicle) Nothing (Just cityTiers) driver.id driver.merchantOperatingCityId
          let eligibleTiers = [tier | (tier, isUsageRestricted) <- driverTiers, not isUsageRestricted, maybe False (.enabled) tier.autoAcceptanceConfig]
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = driver.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
          walletFailedTiers <-
            if fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
              then
                let candidateTypes = selectedTiers <> map (.serviceTierType) eligibleTiers
                    gatedTiers = [tier | tier <- cityTiers, tier.serviceTierType `elem` candidateTypes, isJust (tier.autoAcceptanceConfig >>= (.minWalletBalance))]
                 in map (.serviceTierType) <$> filterM (fmap not . walletCovers) gatedTiers
              else pure []
          let forceSelectedTiers = [tier.serviceTierType | tier <- eligibleTiers, tier.serviceTierType `notElem` walletFailedTiers]
          pure
            FleetTiers
              { autoAssignTiers = filter (`notElem` walletFailedTiers) (DL.nub (selectedTiers <> forceSelectedTiers)),
                forceSelectedTiers
              }
  where
    walletCovers tier = hasMinWalletBalance DRIVER (tier.autoAcceptanceConfig >>= (.minWalletBalance)) driver.id.getId

-- | For a driver who becomes active in a fleet, or whose vehicle was just written: applies the fleet's settings to the
-- vehicle they have, if any. Re-reads the driver and vehicle under the selected-tiers lock (see 'applyFleetSettingsLocked').
applyFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsToDriver = applyFleetSettingsLocked (\_ _ -> pure (Just False))

-- | 'applyFleetSettingsToDriver', only for a driver on auto-assign in a fleet that doesn't let its drivers toggle it. For
-- use after an event that makes more tiers eligible (AC back on): a fleet that allows toggling keeps the driver's choices,
-- and a driver the fleet switched off stays off. Forced, like the fleet's per-driver switch that put the driver on
-- auto-assign: with the fleet's default off, an unforced apply would do nothing.
applyLockedFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyLockedFleetSettingsToDriver = applyFleetSettingsLocked \mbFleetAutoAssign driver -> do
  now <- getCurrentTime
  let onAutoAssign = not (null (autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))))
  pure $ if maybe False (not . (.allowDriverToToggle)) mbFleetAutoAssign && onAutoAssign then Just True else Nothing

-- | Runs 'applyFleetSettings' under the driver's selected-tiers lock (the one 'postDriverUpdateServiceTiers', the wallet
-- strip and the recomputes take), on a fresh read of the driver and vehicle, so a forked caller's older vehicle can't undo
-- a concurrent tier change. 'decide' sees the fleet's auto-assign config and the driver: Nothing skips, Just force applies.
-- Not for a caller that already holds the lock (the lock isn't re-entrant): call 'applyFleetSettings' there.
applyFleetSettingsLocked ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  (Maybe DFC.AutoAssignConfig -> Person -> m (Maybe Bool)) ->
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsLocked decide driverId merchantOpCityId =
  Redis.withWaitAndLockRedisWithTimeout (SVST.selectedServiceTiersLockKey driverId) 5 10 50000 $
    QVehicle.findById driverId >>= mapM_ \vehicle -> do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driverId merchantOpCityId
      decide mbFleetAutoAssign driver >>= mapM_ \forceAutoAssign -> do
        cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
        applyFleetSettings forceAutoAssign cityTiers driver vehicle

-- | The fleet's per-driver switch turned off: these tiers come off the AutoAssign tag and the vehicle's auto-accept tiers.
removeAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  [ServiceTierType] ->
  Person ->
  DV.Vehicle ->
  m ()
removeAutoAssign tiers driver vehicle = do
  let oldTags = fromMaybe [] driver.driverTag
      remaining = filter (`notElem` map show tiers) (autoAssignedTiers oldTags)
      newTags =
        if null remaining
          then Yudhishthira.removeTagName (Just oldTags) (LYT.TagNameValue autoAssignTagName)
          else setTag autoAssignTagName (LYT.ArrayValue remaining) oldTags
  when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
  updateAutoAcceptTiers vehicle (filter (`notElem` tiers) (fromMaybe [] vehicle.selectedAutoAcceptTiers))

-- | What a fleet that doesn't let its drivers toggle auto-assign locks for one of its drivers. It holds the driver only:
-- ops edits, the rating/AC recomputes, the wallet strip and the fleet's own switch still change their tiers.
data FleetLock = FleetLock
  { -- | Tiers the fleet gave auto-assign on: the driver can't turn off the tier or its auto-accept.
    fleetTiers :: [ServiceTierType],
    -- | The driver can't turn AC off, unless rider AC complaints have restricted it.
    acLocked :: Bool,
    -- | The driver can't turn intracity rides off.
    intraCityLocked :: Bool
  }

-- | The fleet lock on this driver, if their fleet doesn't let drivers toggle auto-assign and the driver is on it (has the
-- AutoAssign tag): a driver the fleet switched off with its per-driver switch isn't locked.
getFleetLock ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Person ->
  DI.DriverInformation ->
  DV.Vehicle ->
  [DVST.VehicleServiceTier] ->
  m (Maybe FleetLock)
getFleetLock merchantOpCityId driver driverInfo vehicle cityTiers = do
  mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driver.id merchantOpCityId
  getFleetLockWith mbFleetAutoAssign driver driverInfo vehicle cityTiers

-- | 'getFleetLock' with the fleet's auto-assign config already read.
getFleetLockWith ::
  MonadFlow m =>
  Maybe DFC.AutoAssignConfig ->
  Person ->
  DI.DriverInformation ->
  DV.Vehicle ->
  [DVST.VehicleServiceTier] ->
  m (Maybe FleetLock)
getFleetLockWith mbFleetAutoAssign driver driverInfo vehicle cityTiers = do
  now <- getCurrentTime
  let tagTiers = autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))
      fleetTiers = [tier | tier <- fromMaybe [] vehicle.selectedAutoAcceptTiers, show tier `elem` tagTiers]
  pure $
    if maybe True (.allowDriverToToggle) mbFleetAutoAssign || null tagTiers
      then Nothing
      else Just FleetLock {fleetTiers, acLocked = not acRestrictedByComplaints, intraCityLocked = True}
  where
    -- The same rule as SharedLogic.DriverOnboarding.checkIfACAllowedForDriver, which the GET and the AC tier restriction
    -- use: the score over every threshold of the vehicle's tiers. Not the restriction type, which is set only past the
    -- city-wide maximum. An ops lift resets the score to 0, so the lock holds again.
    acThresholds = mapMaybe (.airConditionedThreshold) (filter (\tier -> vehicle.variant `elem` tier.allowedVehicleVariant) cityTiers)
    acRestrictedByComplaints = not (null acThresholds || any (fromMaybe 0 driverInfo.airConditionScore <=) acThresholds)

setTag :: Text -> LYT.TagValue -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
setTag name value tags =
  Yudhishthira.removeTagName (Just tags) (LYT.TagNameValue name) <> [Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName name) value Nothing]

updateAutoAcceptTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DV.Vehicle ->
  [ServiceTierType] ->
  m ()
updateAutoAcceptTiers vehicle autoAcceptTiers =
  -- Only this column: the vehicle may be an older read than a concurrent service-tier update.
  when (autoAcceptTiers /= fromMaybe [] vehicle.selectedAutoAcceptTiers) $
    QVehicle.updateSelectedAutoAcceptTiers autoAcceptTiers vehicle.driverId
