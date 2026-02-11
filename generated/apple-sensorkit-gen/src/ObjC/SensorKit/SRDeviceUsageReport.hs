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
  , durationSelector
  , applicationUsageByCategorySelector
  , notificationUsageByCategorySelector
  , webUsageByCategorySelector
  , totalScreenWakesSelector
  , totalUnlocksSelector
  , totalUnlockDurationSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The duration of this report
--
-- ObjC selector: @- duration@
duration :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CDouble
duration srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "duration") retCDouble []

-- | Usage time of applications per category
--
-- category is the primary genre from the app's iTunesMetadata.plist.
--
-- ObjC selector: @- applicationUsageByCategory@
applicationUsageByCategory :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSDictionary)
applicationUsageByCategory srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "applicationUsageByCategory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Usage time of notifications per category
--
-- category is the primary genre from the notifying app's iTunesMetadata.plist.
--
-- ObjC selector: @- notificationUsageByCategory@
notificationUsageByCategory :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSDictionary)
notificationUsageByCategory srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "notificationUsageByCategory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Usage time of web domains per category
--
-- category based on the primary Screen Time category of the web domain
--
-- ObjC selector: @- webUsageByCategory@
webUsageByCategory :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSDictionary)
webUsageByCategory srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "webUsageByCategory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Total number of screen wakes over this duration
--
-- ObjC selector: @- totalScreenWakes@
totalScreenWakes :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CLong
totalScreenWakes srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "totalScreenWakes") retCLong []

-- | Total number of unlocks over this duration
--
-- ObjC selector: @- totalUnlocks@
totalUnlocks :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CLong
totalUnlocks srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "totalUnlocks") retCLong []

-- | Total amount of time the device was unlocked over this duration
--
-- ObjC selector: @- totalUnlockDuration@
totalUnlockDuration :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO CDouble
totalUnlockDuration srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "totalUnlockDuration") retCDouble []

-- | Version of the algorithm used to produce the report
--
-- ObjC selector: @- version@
version :: IsSRDeviceUsageReport srDeviceUsageReport => srDeviceUsageReport -> IO (Id NSString)
version srDeviceUsageReport  =
    sendMsg srDeviceUsageReport (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @applicationUsageByCategory@
applicationUsageByCategorySelector :: Selector
applicationUsageByCategorySelector = mkSelector "applicationUsageByCategory"

-- | @Selector@ for @notificationUsageByCategory@
notificationUsageByCategorySelector :: Selector
notificationUsageByCategorySelector = mkSelector "notificationUsageByCategory"

-- | @Selector@ for @webUsageByCategory@
webUsageByCategorySelector :: Selector
webUsageByCategorySelector = mkSelector "webUsageByCategory"

-- | @Selector@ for @totalScreenWakes@
totalScreenWakesSelector :: Selector
totalScreenWakesSelector = mkSelector "totalScreenWakes"

-- | @Selector@ for @totalUnlocks@
totalUnlocksSelector :: Selector
totalUnlocksSelector = mkSelector "totalUnlocks"

-- | @Selector@ for @totalUnlockDuration@
totalUnlockDurationSelector :: Selector
totalUnlockDurationSelector = mkSelector "totalUnlockDuration"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

