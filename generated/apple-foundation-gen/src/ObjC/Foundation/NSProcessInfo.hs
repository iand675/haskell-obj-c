{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activeProcessorCountSelector
  , argumentsSelector
  , automaticTerminationSupportEnabledSelector
  , beginActivityWithOptions_reasonSelector
  , disableAutomaticTerminationSelector
  , disableSuddenTerminationSelector
  , enableAutomaticTerminationSelector
  , enableSuddenTerminationSelector
  , endActivitySelector
  , environmentSelector
  , fullUserNameSelector
  , globallyUniqueStringSelector
  , hostNameSelector
  , iOSAppOnMacSelector
  , iOSAppOnVisionSelector
  , lowPowerModeEnabledSelector
  , macCatalystAppSelector
  , operatingSystemNameSelector
  , operatingSystemSelector
  , operatingSystemVersionStringSelector
  , performActivityWithOptions_reason_usingBlockSelector
  , performExpiringActivityWithReason_usingBlockSelector
  , physicalMemorySelector
  , processIdentifierSelector
  , processInfoSelector
  , processNameSelector
  , processorCountSelector
  , setAutomaticTerminationSupportEnabledSelector
  , setProcessNameSelector
  , systemUptimeSelector
  , thermalStateSelector
  , userNameSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- operatingSystem@
operatingSystem :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
operatingSystem nsProcessInfo =
  sendMessage nsProcessInfo operatingSystemSelector

-- | @- operatingSystemName@
operatingSystemName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
operatingSystemName nsProcessInfo =
  sendMessage nsProcessInfo operatingSystemNameSelector

-- | @- disableSuddenTermination@
disableSuddenTermination :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO ()
disableSuddenTermination nsProcessInfo =
  sendMessage nsProcessInfo disableSuddenTerminationSelector

-- | @- enableSuddenTermination@
enableSuddenTermination :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO ()
enableSuddenTermination nsProcessInfo =
  sendMessage nsProcessInfo enableSuddenTerminationSelector

-- | @- disableAutomaticTermination:@
disableAutomaticTermination :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> reason -> IO ()
disableAutomaticTermination nsProcessInfo reason =
  sendMessage nsProcessInfo disableAutomaticTerminationSelector (toNSString reason)

-- | @- enableAutomaticTermination:@
enableAutomaticTermination :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> reason -> IO ()
enableAutomaticTermination nsProcessInfo reason =
  sendMessage nsProcessInfo enableAutomaticTerminationSelector (toNSString reason)

-- | @- beginActivityWithOptions:reason:@
beginActivityWithOptions_reason :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> NSActivityOptions -> reason -> IO RawId
beginActivityWithOptions_reason nsProcessInfo options reason =
  sendMessage nsProcessInfo beginActivityWithOptions_reasonSelector options (toNSString reason)

-- | @- endActivity:@
endActivity :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> RawId -> IO ()
endActivity nsProcessInfo activity =
  sendMessage nsProcessInfo endActivitySelector activity

-- | @- performActivityWithOptions:reason:usingBlock:@
performActivityWithOptions_reason_usingBlock :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> NSActivityOptions -> reason -> Ptr () -> IO ()
performActivityWithOptions_reason_usingBlock nsProcessInfo options reason block =
  sendMessage nsProcessInfo performActivityWithOptions_reason_usingBlockSelector options (toNSString reason) block

-- | @- performExpiringActivityWithReason:usingBlock:@
performExpiringActivityWithReason_usingBlock :: (IsNSProcessInfo nsProcessInfo, IsNSString reason) => nsProcessInfo -> reason -> Ptr () -> IO ()
performExpiringActivityWithReason_usingBlock nsProcessInfo reason block =
  sendMessage nsProcessInfo performExpiringActivityWithReason_usingBlockSelector (toNSString reason) block

-- | @+ processInfo@
processInfo :: IO (Id NSProcessInfo)
processInfo  =
  do
    cls' <- getRequiredClass "NSProcessInfo"
    sendClassMessage cls' processInfoSelector

-- | @- environment@
environment :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSDictionary)
environment nsProcessInfo =
  sendMessage nsProcessInfo environmentSelector

-- | @- arguments@
arguments :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSArray)
arguments nsProcessInfo =
  sendMessage nsProcessInfo argumentsSelector

-- | @- hostName@
hostName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
hostName nsProcessInfo =
  sendMessage nsProcessInfo hostNameSelector

-- | @- processName@
processName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
processName nsProcessInfo =
  sendMessage nsProcessInfo processNameSelector

-- | @- setProcessName:@
setProcessName :: (IsNSProcessInfo nsProcessInfo, IsNSString value) => nsProcessInfo -> value -> IO ()
setProcessName nsProcessInfo value =
  sendMessage nsProcessInfo setProcessNameSelector (toNSString value)

-- | @- processIdentifier@
processIdentifier :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CInt
processIdentifier nsProcessInfo =
  sendMessage nsProcessInfo processIdentifierSelector

-- | @- globallyUniqueString@
globallyUniqueString :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
globallyUniqueString nsProcessInfo =
  sendMessage nsProcessInfo globallyUniqueStringSelector

-- | @- operatingSystemVersionString@
operatingSystemVersionString :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
operatingSystemVersionString nsProcessInfo =
  sendMessage nsProcessInfo operatingSystemVersionStringSelector

-- | @- processorCount@
processorCount :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
processorCount nsProcessInfo =
  sendMessage nsProcessInfo processorCountSelector

-- | @- activeProcessorCount@
activeProcessorCount :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
activeProcessorCount nsProcessInfo =
  sendMessage nsProcessInfo activeProcessorCountSelector

-- | @- physicalMemory@
physicalMemory :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CULong
physicalMemory nsProcessInfo =
  sendMessage nsProcessInfo physicalMemorySelector

-- | @- systemUptime@
systemUptime :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO CDouble
systemUptime nsProcessInfo =
  sendMessage nsProcessInfo systemUptimeSelector

-- | @- automaticTerminationSupportEnabled@
automaticTerminationSupportEnabled :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
automaticTerminationSupportEnabled nsProcessInfo =
  sendMessage nsProcessInfo automaticTerminationSupportEnabledSelector

-- | @- setAutomaticTerminationSupportEnabled:@
setAutomaticTerminationSupportEnabled :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> Bool -> IO ()
setAutomaticTerminationSupportEnabled nsProcessInfo value =
  sendMessage nsProcessInfo setAutomaticTerminationSupportEnabledSelector value

-- | @- macCatalystApp@
macCatalystApp :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
macCatalystApp nsProcessInfo =
  sendMessage nsProcessInfo macCatalystAppSelector

-- | @- iOSAppOnMac@
iOSAppOnMac :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
iOSAppOnMac nsProcessInfo =
  sendMessage nsProcessInfo iOSAppOnMacSelector

-- | @- iOSAppOnVision@
iOSAppOnVision :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
iOSAppOnVision nsProcessInfo =
  sendMessage nsProcessInfo iOSAppOnVisionSelector

-- | @- lowPowerModeEnabled@
lowPowerModeEnabled :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO Bool
lowPowerModeEnabled nsProcessInfo =
  sendMessage nsProcessInfo lowPowerModeEnabledSelector

-- | @- thermalState@
thermalState :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO NSProcessInfoThermalState
thermalState nsProcessInfo =
  sendMessage nsProcessInfo thermalStateSelector

-- | @- userName@
userName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
userName nsProcessInfo =
  sendMessage nsProcessInfo userNameSelector

-- | @- fullUserName@
fullUserName :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> IO (Id NSString)
fullUserName nsProcessInfo =
  sendMessage nsProcessInfo fullUserNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operatingSystem@
operatingSystemSelector :: Selector '[] CULong
operatingSystemSelector = mkSelector "operatingSystem"

-- | @Selector@ for @operatingSystemName@
operatingSystemNameSelector :: Selector '[] (Id NSString)
operatingSystemNameSelector = mkSelector "operatingSystemName"

-- | @Selector@ for @disableSuddenTermination@
disableSuddenTerminationSelector :: Selector '[] ()
disableSuddenTerminationSelector = mkSelector "disableSuddenTermination"

-- | @Selector@ for @enableSuddenTermination@
enableSuddenTerminationSelector :: Selector '[] ()
enableSuddenTerminationSelector = mkSelector "enableSuddenTermination"

-- | @Selector@ for @disableAutomaticTermination:@
disableAutomaticTerminationSelector :: Selector '[Id NSString] ()
disableAutomaticTerminationSelector = mkSelector "disableAutomaticTermination:"

-- | @Selector@ for @enableAutomaticTermination:@
enableAutomaticTerminationSelector :: Selector '[Id NSString] ()
enableAutomaticTerminationSelector = mkSelector "enableAutomaticTermination:"

-- | @Selector@ for @beginActivityWithOptions:reason:@
beginActivityWithOptions_reasonSelector :: Selector '[NSActivityOptions, Id NSString] RawId
beginActivityWithOptions_reasonSelector = mkSelector "beginActivityWithOptions:reason:"

-- | @Selector@ for @endActivity:@
endActivitySelector :: Selector '[RawId] ()
endActivitySelector = mkSelector "endActivity:"

-- | @Selector@ for @performActivityWithOptions:reason:usingBlock:@
performActivityWithOptions_reason_usingBlockSelector :: Selector '[NSActivityOptions, Id NSString, Ptr ()] ()
performActivityWithOptions_reason_usingBlockSelector = mkSelector "performActivityWithOptions:reason:usingBlock:"

-- | @Selector@ for @performExpiringActivityWithReason:usingBlock:@
performExpiringActivityWithReason_usingBlockSelector :: Selector '[Id NSString, Ptr ()] ()
performExpiringActivityWithReason_usingBlockSelector = mkSelector "performExpiringActivityWithReason:usingBlock:"

-- | @Selector@ for @processInfo@
processInfoSelector :: Selector '[] (Id NSProcessInfo)
processInfoSelector = mkSelector "processInfo"

-- | @Selector@ for @environment@
environmentSelector :: Selector '[] (Id NSDictionary)
environmentSelector = mkSelector "environment"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @hostName@
hostNameSelector :: Selector '[] (Id NSString)
hostNameSelector = mkSelector "hostName"

-- | @Selector@ for @processName@
processNameSelector :: Selector '[] (Id NSString)
processNameSelector = mkSelector "processName"

-- | @Selector@ for @setProcessName:@
setProcessNameSelector :: Selector '[Id NSString] ()
setProcessNameSelector = mkSelector "setProcessName:"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector '[] CInt
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @globallyUniqueString@
globallyUniqueStringSelector :: Selector '[] (Id NSString)
globallyUniqueStringSelector = mkSelector "globallyUniqueString"

-- | @Selector@ for @operatingSystemVersionString@
operatingSystemVersionStringSelector :: Selector '[] (Id NSString)
operatingSystemVersionStringSelector = mkSelector "operatingSystemVersionString"

-- | @Selector@ for @processorCount@
processorCountSelector :: Selector '[] CULong
processorCountSelector = mkSelector "processorCount"

-- | @Selector@ for @activeProcessorCount@
activeProcessorCountSelector :: Selector '[] CULong
activeProcessorCountSelector = mkSelector "activeProcessorCount"

-- | @Selector@ for @physicalMemory@
physicalMemorySelector :: Selector '[] CULong
physicalMemorySelector = mkSelector "physicalMemory"

-- | @Selector@ for @systemUptime@
systemUptimeSelector :: Selector '[] CDouble
systemUptimeSelector = mkSelector "systemUptime"

-- | @Selector@ for @automaticTerminationSupportEnabled@
automaticTerminationSupportEnabledSelector :: Selector '[] Bool
automaticTerminationSupportEnabledSelector = mkSelector "automaticTerminationSupportEnabled"

-- | @Selector@ for @setAutomaticTerminationSupportEnabled:@
setAutomaticTerminationSupportEnabledSelector :: Selector '[Bool] ()
setAutomaticTerminationSupportEnabledSelector = mkSelector "setAutomaticTerminationSupportEnabled:"

-- | @Selector@ for @macCatalystApp@
macCatalystAppSelector :: Selector '[] Bool
macCatalystAppSelector = mkSelector "macCatalystApp"

-- | @Selector@ for @iOSAppOnMac@
iOSAppOnMacSelector :: Selector '[] Bool
iOSAppOnMacSelector = mkSelector "iOSAppOnMac"

-- | @Selector@ for @iOSAppOnVision@
iOSAppOnVisionSelector :: Selector '[] Bool
iOSAppOnVisionSelector = mkSelector "iOSAppOnVision"

-- | @Selector@ for @lowPowerModeEnabled@
lowPowerModeEnabledSelector :: Selector '[] Bool
lowPowerModeEnabledSelector = mkSelector "lowPowerModeEnabled"

-- | @Selector@ for @thermalState@
thermalStateSelector :: Selector '[] NSProcessInfoThermalState
thermalStateSelector = mkSelector "thermalState"

-- | @Selector@ for @userName@
userNameSelector :: Selector '[] (Id NSString)
userNameSelector = mkSelector "userName"

-- | @Selector@ for @fullUserName@
fullUserNameSelector :: Selector '[] (Id NSString)
fullUserNameSelector = mkSelector "fullUserName"

