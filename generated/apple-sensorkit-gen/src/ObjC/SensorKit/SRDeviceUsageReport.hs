{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRDeviceUsageReport@.
module ObjC.SensorKit.SRDeviceUsageReport
  ( SRDeviceUsageReport
  , IsSRDeviceUsageReport(..)
  , duration
  , applicationUsageByCategory
  , notificationUsageByCategory
  , webUsageByCategory
  , totalScreenWakes
  , totalUnlocks
  , totalUnlockDuration
  , version
  , applicationUsageByCategorySelector
  , durationSelector
  , notificationUsageByCategorySelector
  , totalScreenWakesSelector
  , totalUnlockDurationSelector
  , totalUnlocksSelector
  , versionSelector
  , webUsageByCategorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The duration of this report
--
-- ObjC selector: @- duration@
duration :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CDouble
duration srDeviceUsageReport =
  sendMessage srDeviceUsageReport durationSelector

-- | Usage time of applications per category
--
-- category is the primary genre from the app's iTunesMetadata.plist.
--
-- ObjC selector: @- applicationUsageByCategory@
applicationUsageByCategory :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSDictionary)
applicationUsageByCategory srDeviceUsageReport =
  sendMessage srDeviceUsageReport applicationUsageByCategorySelector

-- | Usage time of notifications per category
--
-- category is the primary genre from the notifying app's iTunesMetadata.plist.
--
-- ObjC selector: @- notificationUsageByCategory@
notificationUsageByCategory :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSDictionary)
notificationUsageByCategory srDeviceUsageReport =
  sendMessage srDeviceUsageReport notificationUsageByCategorySelector

-- | Usage time of web domains per category
--
-- category based on the primary Screen Time category of the web domain
--
-- ObjC selector: @- webUsageByCategory@
webUsageByCategory :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSDictionary)
webUsageByCategory srDeviceUsageReport =
  sendMessage srDeviceUsageReport webUsageByCategorySelector

-- | Total number of screen wakes over this duration
--
-- ObjC selector: @- totalScreenWakes@
totalScreenWakes :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CLong
totalScreenWakes srDeviceUsageReport =
  sendMessage srDeviceUsageReport totalScreenWakesSelector

-- | Total number of unlocks over this duration
--
-- ObjC selector: @- totalUnlocks@
totalUnlocks :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CLong
totalUnlocks srDeviceUsageReport =
  sendMessage srDeviceUsageReport totalUnlocksSelector

-- | Total amount of time the device was unlocked over this duration
--
-- ObjC selector: @- totalUnlockDuration@
totalUnlockDuration :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CDouble
totalUnlockDuration srDeviceUsageReport =
  sendMessage srDeviceUsageReport totalUnlockDurationSelector

-- | Version of the algorithm used to produce the report
--
-- ObjC selector: @- version@
version :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSString)
version srDeviceUsageReport =
  sendMessage srDeviceUsageReport versionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @applicationUsageByCategory@
applicationUsageByCategorySelector :: Selector '[] (Id NSDictionary)
applicationUsageByCategorySelector = mkSelector "applicationUsageByCategory"

-- | @Selector@ for @notificationUsageByCategory@
notificationUsageByCategorySelector :: Selector '[] (Id NSDictionary)
notificationUsageByCategorySelector = mkSelector "notificationUsageByCategory"

-- | @Selector@ for @webUsageByCategory@
webUsageByCategorySelector :: Selector '[] (Id NSDictionary)
webUsageByCategorySelector = mkSelector "webUsageByCategory"

-- | @Selector@ for @totalScreenWakes@
totalScreenWakesSelector :: Selector '[] CLong
totalScreenWakesSelector = mkSelector "totalScreenWakes"

-- | @Selector@ for @totalUnlocks@
totalUnlocksSelector :: Selector '[] CLong
totalUnlocksSelector = mkSelector "totalUnlocks"

-- | @Selector@ for @totalUnlockDuration@
totalUnlockDurationSelector :: Selector '[] CDouble
totalUnlockDurationSelector = mkSelector "totalUnlockDuration"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

