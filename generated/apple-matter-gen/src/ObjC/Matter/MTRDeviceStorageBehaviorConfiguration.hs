{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class that configures how MTRDevice objects persist their attributes to storage, so as to not overwhelm the underlying storage system.
--
-- Generated bindings for @MTRDeviceStorageBehaviorConfiguration@.
module ObjC.Matter.MTRDeviceStorageBehaviorConfiguration
  ( MTRDeviceStorageBehaviorConfiguration
  , IsMTRDeviceStorageBehaviorConfiguration(..)
  , configurationWithDefaultStorageBehavior
  , configurationWithStorageBehaviorOptimizationDisabled
  , configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThreshold
  , disableStorageBehaviorOptimization
  , setDisableStorageBehaviorOptimization
  , reportToPersistenceDelayTime
  , setReportToPersistenceDelayTime
  , reportToPersistenceDelayTimeMax
  , setReportToPersistenceDelayTimeMax
  , recentReportTimesMaxCount
  , setRecentReportTimesMaxCount
  , timeBetweenReportsTooShortThreshold
  , setTimeBetweenReportsTooShortThreshold
  , timeBetweenReportsTooShortMinThreshold
  , setTimeBetweenReportsTooShortMinThreshold
  , reportToPersistenceDelayMaxMultiplier
  , setReportToPersistenceDelayMaxMultiplier
  , deviceReportingExcessivelyIntervalThreshold
  , setDeviceReportingExcessivelyIntervalThreshold
  , configurationWithDefaultStorageBehaviorSelector
  , configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector
  , configurationWithStorageBehaviorOptimizationDisabledSelector
  , deviceReportingExcessivelyIntervalThresholdSelector
  , disableStorageBehaviorOptimizationSelector
  , recentReportTimesMaxCountSelector
  , reportToPersistenceDelayMaxMultiplierSelector
  , reportToPersistenceDelayTimeMaxSelector
  , reportToPersistenceDelayTimeSelector
  , setDeviceReportingExcessivelyIntervalThresholdSelector
  , setDisableStorageBehaviorOptimizationSelector
  , setRecentReportTimesMaxCountSelector
  , setReportToPersistenceDelayMaxMultiplierSelector
  , setReportToPersistenceDelayTimeMaxSelector
  , setReportToPersistenceDelayTimeSelector
  , setTimeBetweenReportsTooShortMinThresholdSelector
  , setTimeBetweenReportsTooShortThresholdSelector
  , timeBetweenReportsTooShortMinThresholdSelector
  , timeBetweenReportsTooShortThresholdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create configuration with a default set of values. See description below for details.
--
-- ObjC selector: @+ configurationWithDefaultStorageBehavior@
configurationWithDefaultStorageBehavior :: IO (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithDefaultStorageBehavior  =
  do
    cls' <- getRequiredClass "MTRDeviceStorageBehaviorConfiguration"
    sendClassMessage cls' configurationWithDefaultStorageBehaviorSelector

-- | Create configuration that disables storage behavior optimizations.
--
-- ObjC selector: @+ configurationWithStorageBehaviorOptimizationDisabled@
configurationWithStorageBehaviorOptimizationDisabled :: IO (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithStorageBehaviorOptimizationDisabled  =
  do
    cls' <- getRequiredClass "MTRDeviceStorageBehaviorConfiguration"
    sendClassMessage cls' configurationWithStorageBehaviorOptimizationDisabledSelector

-- | Create configuration with specified values. See description below for details, and the list of properties below for valid ranges of these values.
--
-- ObjC selector: @+ configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:@
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThreshold :: CDouble -> CDouble -> CULong -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThreshold reportToPersistenceDelayTime reportToPersistenceDelayTimeMax recentReportTimesMaxCount timeBetweenReportsTooShortThreshold timeBetweenReportsTooShortMinThreshold reportToPersistenceDelayMaxMultiplier deviceReportingExcessivelyIntervalThreshold =
  do
    cls' <- getRequiredClass "MTRDeviceStorageBehaviorConfiguration"
    sendClassMessage cls' configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector reportToPersistenceDelayTime reportToPersistenceDelayTimeMax recentReportTimesMaxCount timeBetweenReportsTooShortThreshold timeBetweenReportsTooShortMinThreshold reportToPersistenceDelayMaxMultiplier deviceReportingExcessivelyIntervalThreshold

-- | If disableStorageBehaviorOptimization is set to YES, then all the waiting mechanism as described above is disabled.
--
-- ObjC selector: @- disableStorageBehaviorOptimization@
disableStorageBehaviorOptimization :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO Bool
disableStorageBehaviorOptimization mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration disableStorageBehaviorOptimizationSelector

-- | If disableStorageBehaviorOptimization is set to YES, then all the waiting mechanism as described above is disabled.
--
-- ObjC selector: @- setDisableStorageBehaviorOptimization:@
setDisableStorageBehaviorOptimization :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> Bool -> IO ()
setDisableStorageBehaviorOptimization mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setDisableStorageBehaviorOptimizationSelector value

-- | If any of these properties are set to be out of the documented limits, these default values will be used to replace all of them:
--
-- reportToPersistenceDelayTimeDefault (15) reportToPersistenceDelayTimeMaxDefault (20 * 15) recentReportTimesMaxCountDefault (12) timeBetweenReportsTooShortThresholdDefault (15) timeBetweenReportsTooShortMinThresholdDefault (5) reportToPersistenceDelayMaxMultiplierDefault (10) deviceReportingExcessivelyIntervalThresholdDefault (5 * 60)
--
-- ObjC selector: @- reportToPersistenceDelayTime@
reportToPersistenceDelayTime :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
reportToPersistenceDelayTime mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration reportToPersistenceDelayTimeSelector

-- | If any of these properties are set to be out of the documented limits, these default values will be used to replace all of them:
--
-- reportToPersistenceDelayTimeDefault (15) reportToPersistenceDelayTimeMaxDefault (20 * 15) recentReportTimesMaxCountDefault (12) timeBetweenReportsTooShortThresholdDefault (15) timeBetweenReportsTooShortMinThresholdDefault (5) reportToPersistenceDelayMaxMultiplierDefault (10) deviceReportingExcessivelyIntervalThresholdDefault (5 * 60)
--
-- ObjC selector: @- setReportToPersistenceDelayTime:@
setReportToPersistenceDelayTime :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setReportToPersistenceDelayTime mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setReportToPersistenceDelayTimeSelector value

-- | @- reportToPersistenceDelayTimeMax@
reportToPersistenceDelayTimeMax :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
reportToPersistenceDelayTimeMax mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration reportToPersistenceDelayTimeMaxSelector

-- | @- setReportToPersistenceDelayTimeMax:@
setReportToPersistenceDelayTimeMax :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setReportToPersistenceDelayTimeMax mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setReportToPersistenceDelayTimeMaxSelector value

-- | @- recentReportTimesMaxCount@
recentReportTimesMaxCount :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CULong
recentReportTimesMaxCount mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration recentReportTimesMaxCountSelector

-- | @- setRecentReportTimesMaxCount:@
setRecentReportTimesMaxCount :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CULong -> IO ()
setRecentReportTimesMaxCount mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setRecentReportTimesMaxCountSelector value

-- | @- timeBetweenReportsTooShortThreshold@
timeBetweenReportsTooShortThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
timeBetweenReportsTooShortThreshold mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration timeBetweenReportsTooShortThresholdSelector

-- | @- setTimeBetweenReportsTooShortThreshold:@
setTimeBetweenReportsTooShortThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setTimeBetweenReportsTooShortThreshold mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setTimeBetweenReportsTooShortThresholdSelector value

-- | @- timeBetweenReportsTooShortMinThreshold@
timeBetweenReportsTooShortMinThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
timeBetweenReportsTooShortMinThreshold mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration timeBetweenReportsTooShortMinThresholdSelector

-- | @- setTimeBetweenReportsTooShortMinThreshold:@
setTimeBetweenReportsTooShortMinThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setTimeBetweenReportsTooShortMinThreshold mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setTimeBetweenReportsTooShortMinThresholdSelector value

-- | @- reportToPersistenceDelayMaxMultiplier@
reportToPersistenceDelayMaxMultiplier :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
reportToPersistenceDelayMaxMultiplier mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration reportToPersistenceDelayMaxMultiplierSelector

-- | @- setReportToPersistenceDelayMaxMultiplier:@
setReportToPersistenceDelayMaxMultiplier :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setReportToPersistenceDelayMaxMultiplier mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setReportToPersistenceDelayMaxMultiplierSelector value

-- | @- deviceReportingExcessivelyIntervalThreshold@
deviceReportingExcessivelyIntervalThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> IO CDouble
deviceReportingExcessivelyIntervalThreshold mtrDeviceStorageBehaviorConfiguration =
  sendMessage mtrDeviceStorageBehaviorConfiguration deviceReportingExcessivelyIntervalThresholdSelector

-- | @- setDeviceReportingExcessivelyIntervalThreshold:@
setDeviceReportingExcessivelyIntervalThreshold :: IsMTRDeviceStorageBehaviorConfiguration mtrDeviceStorageBehaviorConfiguration => mtrDeviceStorageBehaviorConfiguration -> CDouble -> IO ()
setDeviceReportingExcessivelyIntervalThreshold mtrDeviceStorageBehaviorConfiguration value =
  sendMessage mtrDeviceStorageBehaviorConfiguration setDeviceReportingExcessivelyIntervalThresholdSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationWithDefaultStorageBehavior@
configurationWithDefaultStorageBehaviorSelector :: Selector '[] (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithDefaultStorageBehaviorSelector = mkSelector "configurationWithDefaultStorageBehavior"

-- | @Selector@ for @configurationWithStorageBehaviorOptimizationDisabled@
configurationWithStorageBehaviorOptimizationDisabledSelector :: Selector '[] (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithStorageBehaviorOptimizationDisabledSelector = mkSelector "configurationWithStorageBehaviorOptimizationDisabled"

-- | @Selector@ for @configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:@
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector :: Selector '[CDouble, CDouble, CULong, CDouble, CDouble, CDouble, CDouble] (Id MTRDeviceStorageBehaviorConfiguration)
configurationWithReportToPersistenceDelayTime_reportToPersistenceDelayTimeMax_recentReportTimesMaxCount_timeBetweenReportsTooShortThreshold_timeBetweenReportsTooShortMinThreshold_reportToPersistenceDelayMaxMultiplier_deviceReportingExcessivelyIntervalThresholdSelector = mkSelector "configurationWithReportToPersistenceDelayTime:reportToPersistenceDelayTimeMax:recentReportTimesMaxCount:timeBetweenReportsTooShortThreshold:timeBetweenReportsTooShortMinThreshold:reportToPersistenceDelayMaxMultiplier:deviceReportingExcessivelyIntervalThreshold:"

-- | @Selector@ for @disableStorageBehaviorOptimization@
disableStorageBehaviorOptimizationSelector :: Selector '[] Bool
disableStorageBehaviorOptimizationSelector = mkSelector "disableStorageBehaviorOptimization"

-- | @Selector@ for @setDisableStorageBehaviorOptimization:@
setDisableStorageBehaviorOptimizationSelector :: Selector '[Bool] ()
setDisableStorageBehaviorOptimizationSelector = mkSelector "setDisableStorageBehaviorOptimization:"

-- | @Selector@ for @reportToPersistenceDelayTime@
reportToPersistenceDelayTimeSelector :: Selector '[] CDouble
reportToPersistenceDelayTimeSelector = mkSelector "reportToPersistenceDelayTime"

-- | @Selector@ for @setReportToPersistenceDelayTime:@
setReportToPersistenceDelayTimeSelector :: Selector '[CDouble] ()
setReportToPersistenceDelayTimeSelector = mkSelector "setReportToPersistenceDelayTime:"

-- | @Selector@ for @reportToPersistenceDelayTimeMax@
reportToPersistenceDelayTimeMaxSelector :: Selector '[] CDouble
reportToPersistenceDelayTimeMaxSelector = mkSelector "reportToPersistenceDelayTimeMax"

-- | @Selector@ for @setReportToPersistenceDelayTimeMax:@
setReportToPersistenceDelayTimeMaxSelector :: Selector '[CDouble] ()
setReportToPersistenceDelayTimeMaxSelector = mkSelector "setReportToPersistenceDelayTimeMax:"

-- | @Selector@ for @recentReportTimesMaxCount@
recentReportTimesMaxCountSelector :: Selector '[] CULong
recentReportTimesMaxCountSelector = mkSelector "recentReportTimesMaxCount"

-- | @Selector@ for @setRecentReportTimesMaxCount:@
setRecentReportTimesMaxCountSelector :: Selector '[CULong] ()
setRecentReportTimesMaxCountSelector = mkSelector "setRecentReportTimesMaxCount:"

-- | @Selector@ for @timeBetweenReportsTooShortThreshold@
timeBetweenReportsTooShortThresholdSelector :: Selector '[] CDouble
timeBetweenReportsTooShortThresholdSelector = mkSelector "timeBetweenReportsTooShortThreshold"

-- | @Selector@ for @setTimeBetweenReportsTooShortThreshold:@
setTimeBetweenReportsTooShortThresholdSelector :: Selector '[CDouble] ()
setTimeBetweenReportsTooShortThresholdSelector = mkSelector "setTimeBetweenReportsTooShortThreshold:"

-- | @Selector@ for @timeBetweenReportsTooShortMinThreshold@
timeBetweenReportsTooShortMinThresholdSelector :: Selector '[] CDouble
timeBetweenReportsTooShortMinThresholdSelector = mkSelector "timeBetweenReportsTooShortMinThreshold"

-- | @Selector@ for @setTimeBetweenReportsTooShortMinThreshold:@
setTimeBetweenReportsTooShortMinThresholdSelector :: Selector '[CDouble] ()
setTimeBetweenReportsTooShortMinThresholdSelector = mkSelector "setTimeBetweenReportsTooShortMinThreshold:"

-- | @Selector@ for @reportToPersistenceDelayMaxMultiplier@
reportToPersistenceDelayMaxMultiplierSelector :: Selector '[] CDouble
reportToPersistenceDelayMaxMultiplierSelector = mkSelector "reportToPersistenceDelayMaxMultiplier"

-- | @Selector@ for @setReportToPersistenceDelayMaxMultiplier:@
setReportToPersistenceDelayMaxMultiplierSelector :: Selector '[CDouble] ()
setReportToPersistenceDelayMaxMultiplierSelector = mkSelector "setReportToPersistenceDelayMaxMultiplier:"

-- | @Selector@ for @deviceReportingExcessivelyIntervalThreshold@
deviceReportingExcessivelyIntervalThresholdSelector :: Selector '[] CDouble
deviceReportingExcessivelyIntervalThresholdSelector = mkSelector "deviceReportingExcessivelyIntervalThreshold"

-- | @Selector@ for @setDeviceReportingExcessivelyIntervalThreshold:@
setDeviceReportingExcessivelyIntervalThresholdSelector :: Selector '[CDouble] ()
setDeviceReportingExcessivelyIntervalThresholdSelector = mkSelector "setDeviceReportingExcessivelyIntervalThreshold:"

