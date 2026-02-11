{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProcessInfo@.
module ObjC.Foundation.NSProcessInfo
  ( NSProcessInfo
  , IsNSProcessInfo(..)
  , operatingSystem
  , operatingSystemName
  , disableSuddenTermination
  , enableSuddenTermination
  , disableAutomaticTermination
  , enableAutomaticTermination
  , beginActivityWithOptions_reason
  , endActivity
  , performActivityWithOptions_reason_usingBlock
  , performExpiringActivityWithReason_usingBlock
  , processInfo
  , environment
  , arguments
  , hostName
  , processName
  , setProcessName
  , processIdentifier
  , globallyUniqueString
  , operatingSystemVersionString
  , processorCount
  , activeProcessorCount
  , physicalMemory
  , systemUptime
  , automaticTerminationSupportEnabled
  , setAutomaticTerminationSupportEnabled
  , macCatalystApp
  , iOSAppOnMac
  , iOSAppOnVision
  , lowPowerModeEnabled
  , thermalState
  , userName
  , fullUserName
  , operatingSystemSelector
  , operatingSystemNameSelector
  , disableSuddenTerminationSelector
  , enableSuddenTerminationSelector
  , disableAutomaticTerminationSelector
  , enableAutomaticTerminationSelector
  , beginActivityWithOptions_reasonSelector
  , endActivitySelector
  , performActivityWithOptions_reason_usingBlockSelector
  , performExpiringActivityWithReason_usingBlockSelector
  , processInfoSelector
  , environmentSelector
  , argumentsSelector
  , hostNameSelector
  , processNameSelector
  , setProcessNameSelector
  , processIdentifierSelector
  , globallyUniqueStringSelector
  , operatingSystemVersionStringSelector
  , processorCountSelector
  , activeProcessorCountSelector
  , physicalMemorySelector
  , systemUptimeSelector
  , automaticTerminationSupportEnabledSelector
  , setAutomaticTerminationSupportEnabledSelector
  , macCatalystAppSelector
  , iOSAppOnMacSelector
  , iOSAppOnVisionSelector
  , lowPowerModeEnabledSelector
  , thermalStateSelector
  , userNameSelector
  , fullUserNameSelector

  -- * Enum types
  , NSActivityOptions(NSActivityOptions)
  , pattern NSActivityIdleDisplaySleepDisabled
  , pattern NSActivityIdleSystemSleepDisabled
  , pattern NSActivitySuddenTerminationDisabled
  , pattern NSActivityAutomaticTerminationDisabled
  , pattern NSActivityAnimationTrackingEnabled
  , pattern NSActivityTrackingEnabled
  , pattern NSActivityUserInitiated
  , pattern NSActivityUserInitiatedAllowingIdleSystemSleep
  , pattern NSActivityBackground
  , pattern NSActivityLatencyCritical
  , pattern NSActivityUserInteractive
  , NSProcessInfoThermalState(NSProcessInfoThermalState)
  , pattern NSProcessInfoThermalStateNominal
  , pattern NSProcessInfoThermalStateFair
  , pattern NSProcessInfoThermalStateSerious
  , pattern NSProcessInfoThermalStateCritical

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- operatingSystem@
operatingSystem :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
operatingSystem nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "operatingSystem") retCULong []

-- | @- operatingSystemName@
operatingSystemName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
operatingSystemName nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "operatingSystemName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- disableSuddenTermination@
disableSuddenTermination :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO ()
disableSuddenTermination nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "disableSuddenTermination") retVoid []

-- | @- enableSuddenTermination@
enableSuddenTermination :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO ()
enableSuddenTermination nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "enableSuddenTermination") retVoid []

-- | @- disableAutomaticTermination:@
disableAutomaticTermination :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> reason -> IO ()
disableAutomaticTermination nsProcessInfo  reason =
  withObjCPtr reason $ \raw_reason ->
      sendMsg nsProcessInfo (mkSelector "disableAutomaticTermination:") retVoid [argPtr (castPtr raw_reason :: Ptr ())]

-- | @- enableAutomaticTermination:@
enableAutomaticTermination :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> reason -> IO ()
enableAutomaticTermination nsProcessInfo  reason =
  withObjCPtr reason $ \raw_reason ->
      sendMsg nsProcessInfo (mkSelector "enableAutomaticTermination:") retVoid [argPtr (castPtr raw_reason :: Ptr ())]

-- | @- beginActivityWithOptions:reason:@
beginActivityWithOptions_reason :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> NSActivityOptions -> reason -> IO RawId
beginActivityWithOptions_reason nsProcessInfo  options reason =
  withObjCPtr reason $ \raw_reason ->
      fmap (RawId . castPtr) $ sendMsg nsProcessInfo (mkSelector "beginActivityWithOptions:reason:") (retPtr retVoid) [argCULong (coerce options), argPtr (castPtr raw_reason :: Ptr ())]

-- | @- endActivity:@
endActivity :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> RawId -> IO ()
endActivity nsProcessInfo  activity =
    sendMsg nsProcessInfo (mkSelector "endActivity:") retVoid [argPtr (castPtr (unRawId activity) :: Ptr ())]

-- | @- performActivityWithOptions:reason:usingBlock:@
performActivityWithOptions_reason_usingBlock :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> NSActivityOptions -> reason -> Ptr () -> IO ()
performActivityWithOptions_reason_usingBlock nsProcessInfo  options reason block =
  withObjCPtr reason $ \raw_reason ->
      sendMsg nsProcessInfo (mkSelector "performActivityWithOptions:reason:usingBlock:") retVoid [argCULong (coerce options), argPtr (castPtr raw_reason :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- performExpiringActivityWithReason:usingBlock:@
performExpiringActivityWithReason_usingBlock :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> reason -> Ptr () -> IO ()
performExpiringActivityWithReason_usingBlock nsProcessInfo  reason block =
  withObjCPtr reason $ \raw_reason ->
      sendMsg nsProcessInfo (mkSelector "performExpiringActivityWithReason:usingBlock:") retVoid [argPtr (castPtr raw_reason :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @+ processInfo@
processInfo :: IO (Id NSProcessInfo)
processInfo  =
  do
    cls' <- getRequiredClass "NSProcessInfo"
    sendClassMsg cls' (mkSelector "processInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- environment@
environment :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSDictionary)
environment nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "environment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arguments@
arguments :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSArray)
arguments nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hostName@
hostName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
hostName nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "hostName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- processName@
processName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
processName nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "processName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProcessName:@
setProcessName :: (IsNSProcessInfo nsProcessInfo, IsNSString value) => nsProcessInfo -> value -> IO ()
setProcessName nsProcessInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProcessInfo (mkSelector "setProcessName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- processIdentifier@
processIdentifier :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CInt
processIdentifier nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "processIdentifier") retCInt []

-- | @- globallyUniqueString@
globallyUniqueString :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
globallyUniqueString nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "globallyUniqueString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- operatingSystemVersionString@
operatingSystemVersionString :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
operatingSystemVersionString nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "operatingSystemVersionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- processorCount@
processorCount :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
processorCount nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "processorCount") retCULong []

-- | @- activeProcessorCount@
activeProcessorCount :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
activeProcessorCount nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "activeProcessorCount") retCULong []

-- | @- physicalMemory@
physicalMemory :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
physicalMemory nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "physicalMemory") retCULong []

-- | @- systemUptime@
systemUptime :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CDouble
systemUptime nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "systemUptime") retCDouble []

-- | @- automaticTerminationSupportEnabled@
automaticTerminationSupportEnabled :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
automaticTerminationSupportEnabled nsProcessInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "automaticTerminationSupportEnabled") retCULong []

-- | @- setAutomaticTerminationSupportEnabled:@
setAutomaticTerminationSupportEnabled :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> Bool -> IO ()
setAutomaticTerminationSupportEnabled nsProcessInfo  value =
    sendMsg nsProcessInfo (mkSelector "setAutomaticTerminationSupportEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- macCatalystApp@
macCatalystApp :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
macCatalystApp nsProcessInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "macCatalystApp") retCULong []

-- | @- iOSAppOnMac@
iOSAppOnMac :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
iOSAppOnMac nsProcessInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "iOSAppOnMac") retCULong []

-- | @- iOSAppOnVision@
iOSAppOnVision :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
iOSAppOnVision nsProcessInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "iOSAppOnVision") retCULong []

-- | @- lowPowerModeEnabled@
lowPowerModeEnabled :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
lowPowerModeEnabled nsProcessInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "lowPowerModeEnabled") retCULong []

-- | @- thermalState@
thermalState :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO NSProcessInfoThermalState
thermalState nsProcessInfo  =
    fmap (coerce :: CLong -> NSProcessInfoThermalState) $ sendMsg nsProcessInfo (mkSelector "thermalState") retCLong []

-- | @- userName@
userName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
userName nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "userName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fullUserName@
fullUserName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
fullUserName nsProcessInfo  =
    sendMsg nsProcessInfo (mkSelector "fullUserName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operatingSystem@
operatingSystemSelector :: Selector
operatingSystemSelector = mkSelector "operatingSystem"

-- | @Selector@ for @operatingSystemName@
operatingSystemNameSelector :: Selector
operatingSystemNameSelector = mkSelector "operatingSystemName"

-- | @Selector@ for @disableSuddenTermination@
disableSuddenTerminationSelector :: Selector
disableSuddenTerminationSelector = mkSelector "disableSuddenTermination"

-- | @Selector@ for @enableSuddenTermination@
enableSuddenTerminationSelector :: Selector
enableSuddenTerminationSelector = mkSelector "enableSuddenTermination"

-- | @Selector@ for @disableAutomaticTermination:@
disableAutomaticTerminationSelector :: Selector
disableAutomaticTerminationSelector = mkSelector "disableAutomaticTermination:"

-- | @Selector@ for @enableAutomaticTermination:@
enableAutomaticTerminationSelector :: Selector
enableAutomaticTerminationSelector = mkSelector "enableAutomaticTermination:"

-- | @Selector@ for @beginActivityWithOptions:reason:@
beginActivityWithOptions_reasonSelector :: Selector
beginActivityWithOptions_reasonSelector = mkSelector "beginActivityWithOptions:reason:"

-- | @Selector@ for @endActivity:@
endActivitySelector :: Selector
endActivitySelector = mkSelector "endActivity:"

-- | @Selector@ for @performActivityWithOptions:reason:usingBlock:@
performActivityWithOptions_reason_usingBlockSelector :: Selector
performActivityWithOptions_reason_usingBlockSelector = mkSelector "performActivityWithOptions:reason:usingBlock:"

-- | @Selector@ for @performExpiringActivityWithReason:usingBlock:@
performExpiringActivityWithReason_usingBlockSelector :: Selector
performExpiringActivityWithReason_usingBlockSelector = mkSelector "performExpiringActivityWithReason:usingBlock:"

-- | @Selector@ for @processInfo@
processInfoSelector :: Selector
processInfoSelector = mkSelector "processInfo"

-- | @Selector@ for @environment@
environmentSelector :: Selector
environmentSelector = mkSelector "environment"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @hostName@
hostNameSelector :: Selector
hostNameSelector = mkSelector "hostName"

-- | @Selector@ for @processName@
processNameSelector :: Selector
processNameSelector = mkSelector "processName"

-- | @Selector@ for @setProcessName:@
setProcessNameSelector :: Selector
setProcessNameSelector = mkSelector "setProcessName:"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @globallyUniqueString@
globallyUniqueStringSelector :: Selector
globallyUniqueStringSelector = mkSelector "globallyUniqueString"

-- | @Selector@ for @operatingSystemVersionString@
operatingSystemVersionStringSelector :: Selector
operatingSystemVersionStringSelector = mkSelector "operatingSystemVersionString"

-- | @Selector@ for @processorCount@
processorCountSelector :: Selector
processorCountSelector = mkSelector "processorCount"

-- | @Selector@ for @activeProcessorCount@
activeProcessorCountSelector :: Selector
activeProcessorCountSelector = mkSelector "activeProcessorCount"

-- | @Selector@ for @physicalMemory@
physicalMemorySelector :: Selector
physicalMemorySelector = mkSelector "physicalMemory"

-- | @Selector@ for @systemUptime@
systemUptimeSelector :: Selector
systemUptimeSelector = mkSelector "systemUptime"

-- | @Selector@ for @automaticTerminationSupportEnabled@
automaticTerminationSupportEnabledSelector :: Selector
automaticTerminationSupportEnabledSelector = mkSelector "automaticTerminationSupportEnabled"

-- | @Selector@ for @setAutomaticTerminationSupportEnabled:@
setAutomaticTerminationSupportEnabledSelector :: Selector
setAutomaticTerminationSupportEnabledSelector = mkSelector "setAutomaticTerminationSupportEnabled:"

-- | @Selector@ for @macCatalystApp@
macCatalystAppSelector :: Selector
macCatalystAppSelector = mkSelector "macCatalystApp"

-- | @Selector@ for @iOSAppOnMac@
iOSAppOnMacSelector :: Selector
iOSAppOnMacSelector = mkSelector "iOSAppOnMac"

-- | @Selector@ for @iOSAppOnVision@
iOSAppOnVisionSelector :: Selector
iOSAppOnVisionSelector = mkSelector "iOSAppOnVision"

-- | @Selector@ for @lowPowerModeEnabled@
lowPowerModeEnabledSelector :: Selector
lowPowerModeEnabledSelector = mkSelector "lowPowerModeEnabled"

-- | @Selector@ for @thermalState@
thermalStateSelector :: Selector
thermalStateSelector = mkSelector "thermalState"

-- | @Selector@ for @userName@
userNameSelector :: Selector
userNameSelector = mkSelector "userName"

-- | @Selector@ for @fullUserName@
fullUserNameSelector :: Selector
fullUserNameSelector = mkSelector "fullUserName"

