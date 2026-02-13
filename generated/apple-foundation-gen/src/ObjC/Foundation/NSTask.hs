{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTask@.
module ObjC.Foundation.NSTask
  ( NSTask
  , IsNSTask(..)
  , init_
  , launchAndReturnError
  , interrupt
  , terminate
  , suspend
  , resume
  , launch
  , launchedTaskWithLaunchPath_arguments
  , launchedTaskWithExecutableURL_arguments_error_terminationHandler
  , waitUntilExit
  , executableURL
  , setExecutableURL
  , arguments
  , setArguments
  , environment
  , setEnvironment
  , currentDirectoryURL
  , setCurrentDirectoryURL
  , launchRequirementData
  , setLaunchRequirementData
  , standardInput
  , setStandardInput
  , standardOutput
  , setStandardOutput
  , standardError
  , setStandardError
  , processIdentifier
  , running
  , terminationStatus
  , terminationReason
  , terminationHandler
  , setTerminationHandler
  , qualityOfService
  , setQualityOfService
  , launchPath
  , setLaunchPath
  , currentDirectoryPath
  , setCurrentDirectoryPath
  , argumentsSelector
  , currentDirectoryPathSelector
  , currentDirectoryURLSelector
  , environmentSelector
  , executableURLSelector
  , initSelector
  , interruptSelector
  , launchAndReturnErrorSelector
  , launchPathSelector
  , launchRequirementDataSelector
  , launchSelector
  , launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector
  , launchedTaskWithLaunchPath_argumentsSelector
  , processIdentifierSelector
  , qualityOfServiceSelector
  , resumeSelector
  , runningSelector
  , setArgumentsSelector
  , setCurrentDirectoryPathSelector
  , setCurrentDirectoryURLSelector
  , setEnvironmentSelector
  , setExecutableURLSelector
  , setLaunchPathSelector
  , setLaunchRequirementDataSelector
  , setQualityOfServiceSelector
  , setStandardErrorSelector
  , setStandardInputSelector
  , setStandardOutputSelector
  , setTerminationHandlerSelector
  , standardErrorSelector
  , standardInputSelector
  , standardOutputSelector
  , suspendSelector
  , terminateSelector
  , terminationHandlerSelector
  , terminationReasonSelector
  , terminationStatusSelector
  , waitUntilExitSelector

  -- * Enum types
  , NSQualityOfService(NSQualityOfService)
  , pattern NSQualityOfServiceUserInteractive
  , pattern NSQualityOfServiceUserInitiated
  , pattern NSQualityOfServiceUtility
  , pattern NSQualityOfServiceBackground
  , pattern NSQualityOfServiceDefault
  , NSTaskTerminationReason(NSTaskTerminationReason)
  , pattern NSTaskTerminationReasonExit
  , pattern NSTaskTerminationReasonUncaughtSignal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSTask nsTask => nsTask -> IO (Id NSTask)
init_ nsTask =
  sendOwnedMessage nsTask initSelector

-- | @- launchAndReturnError:@
launchAndReturnError :: (IsNSTask nsTask, IsNSError error_) => nsTask -> error_ -> IO Bool
launchAndReturnError nsTask error_ =
  sendMessage nsTask launchAndReturnErrorSelector (toNSError error_)

-- | @- interrupt@
interrupt :: IsNSTask nsTask => nsTask -> IO ()
interrupt nsTask =
  sendMessage nsTask interruptSelector

-- | @- terminate@
terminate :: IsNSTask nsTask => nsTask -> IO ()
terminate nsTask =
  sendMessage nsTask terminateSelector

-- | @- suspend@
suspend :: IsNSTask nsTask => nsTask -> IO Bool
suspend nsTask =
  sendMessage nsTask suspendSelector

-- | @- resume@
resume :: IsNSTask nsTask => nsTask -> IO Bool
resume nsTask =
  sendMessage nsTask resumeSelector

-- | @- launch@
launch :: IsNSTask nsTask => nsTask -> IO ()
launch nsTask =
  sendMessage nsTask launchSelector

-- | @+ launchedTaskWithLaunchPath:arguments:@
launchedTaskWithLaunchPath_arguments :: (IsNSString path, IsNSArray arguments) => path -> arguments -> IO (Id NSTask)
launchedTaskWithLaunchPath_arguments path arguments =
  do
    cls' <- getRequiredClass "NSTask"
    sendClassMessage cls' launchedTaskWithLaunchPath_argumentsSelector (toNSString path) (toNSArray arguments)

-- | @+ launchedTaskWithExecutableURL:arguments:error:terminationHandler:@
launchedTaskWithExecutableURL_arguments_error_terminationHandler :: (IsNSURL url, IsNSArray arguments, IsNSError error_) => url -> arguments -> error_ -> Ptr () -> IO (Id NSTask)
launchedTaskWithExecutableURL_arguments_error_terminationHandler url arguments error_ terminationHandler =
  do
    cls' <- getRequiredClass "NSTask"
    sendClassMessage cls' launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector (toNSURL url) (toNSArray arguments) (toNSError error_) terminationHandler

-- | @- waitUntilExit@
waitUntilExit :: IsNSTask nsTask => nsTask -> IO ()
waitUntilExit nsTask =
  sendMessage nsTask waitUntilExitSelector

-- | @- executableURL@
executableURL :: IsNSTask nsTask => nsTask -> IO (Id NSURL)
executableURL nsTask =
  sendMessage nsTask executableURLSelector

-- | @- setExecutableURL:@
setExecutableURL :: (IsNSTask nsTask, IsNSURL value) => nsTask -> value -> IO ()
setExecutableURL nsTask value =
  sendMessage nsTask setExecutableURLSelector (toNSURL value)

-- | @- arguments@
arguments :: IsNSTask nsTask => nsTask -> IO (Id NSArray)
arguments nsTask =
  sendMessage nsTask argumentsSelector

-- | @- setArguments:@
setArguments :: (IsNSTask nsTask, IsNSArray value) => nsTask -> value -> IO ()
setArguments nsTask value =
  sendMessage nsTask setArgumentsSelector (toNSArray value)

-- | @- environment@
environment :: IsNSTask nsTask => nsTask -> IO (Id NSDictionary)
environment nsTask =
  sendMessage nsTask environmentSelector

-- | @- setEnvironment:@
setEnvironment :: (IsNSTask nsTask, IsNSDictionary value) => nsTask -> value -> IO ()
setEnvironment nsTask value =
  sendMessage nsTask setEnvironmentSelector (toNSDictionary value)

-- | @- currentDirectoryURL@
currentDirectoryURL :: IsNSTask nsTask => nsTask -> IO (Id NSURL)
currentDirectoryURL nsTask =
  sendMessage nsTask currentDirectoryURLSelector

-- | @- setCurrentDirectoryURL:@
setCurrentDirectoryURL :: (IsNSTask nsTask, IsNSURL value) => nsTask -> value -> IO ()
setCurrentDirectoryURL nsTask value =
  sendMessage nsTask setCurrentDirectoryURLSelector (toNSURL value)

-- | @- launchRequirementData@
launchRequirementData :: IsNSTask nsTask => nsTask -> IO (Id NSData)
launchRequirementData nsTask =
  sendMessage nsTask launchRequirementDataSelector

-- | @- setLaunchRequirementData:@
setLaunchRequirementData :: (IsNSTask nsTask, IsNSData value) => nsTask -> value -> IO ()
setLaunchRequirementData nsTask value =
  sendMessage nsTask setLaunchRequirementDataSelector (toNSData value)

-- | @- standardInput@
standardInput :: IsNSTask nsTask => nsTask -> IO RawId
standardInput nsTask =
  sendMessage nsTask standardInputSelector

-- | @- setStandardInput:@
setStandardInput :: IsNSTask nsTask => nsTask -> RawId -> IO ()
setStandardInput nsTask value =
  sendMessage nsTask setStandardInputSelector value

-- | @- standardOutput@
standardOutput :: IsNSTask nsTask => nsTask -> IO RawId
standardOutput nsTask =
  sendMessage nsTask standardOutputSelector

-- | @- setStandardOutput:@
setStandardOutput :: IsNSTask nsTask => nsTask -> RawId -> IO ()
setStandardOutput nsTask value =
  sendMessage nsTask setStandardOutputSelector value

-- | @- standardError@
standardError :: IsNSTask nsTask => nsTask -> IO RawId
standardError nsTask =
  sendMessage nsTask standardErrorSelector

-- | @- setStandardError:@
setStandardError :: IsNSTask nsTask => nsTask -> RawId -> IO ()
setStandardError nsTask value =
  sendMessage nsTask setStandardErrorSelector value

-- | @- processIdentifier@
processIdentifier :: IsNSTask nsTask => nsTask -> IO CInt
processIdentifier nsTask =
  sendMessage nsTask processIdentifierSelector

-- | @- running@
running :: IsNSTask nsTask => nsTask -> IO Bool
running nsTask =
  sendMessage nsTask runningSelector

-- | @- terminationStatus@
terminationStatus :: IsNSTask nsTask => nsTask -> IO CInt
terminationStatus nsTask =
  sendMessage nsTask terminationStatusSelector

-- | @- terminationReason@
terminationReason :: IsNSTask nsTask => nsTask -> IO NSTaskTerminationReason
terminationReason nsTask =
  sendMessage nsTask terminationReasonSelector

-- | @- terminationHandler@
terminationHandler :: IsNSTask nsTask => nsTask -> IO (Ptr ())
terminationHandler nsTask =
  sendMessage nsTask terminationHandlerSelector

-- | @- setTerminationHandler:@
setTerminationHandler :: IsNSTask nsTask => nsTask -> Ptr () -> IO ()
setTerminationHandler nsTask value =
  sendMessage nsTask setTerminationHandlerSelector value

-- | @- qualityOfService@
qualityOfService :: IsNSTask nsTask => nsTask -> IO NSQualityOfService
qualityOfService nsTask =
  sendMessage nsTask qualityOfServiceSelector

-- | @- setQualityOfService:@
setQualityOfService :: IsNSTask nsTask => nsTask -> NSQualityOfService -> IO ()
setQualityOfService nsTask value =
  sendMessage nsTask setQualityOfServiceSelector value

-- | @- launchPath@
launchPath :: IsNSTask nsTask => nsTask -> IO (Id NSString)
launchPath nsTask =
  sendMessage nsTask launchPathSelector

-- | @- setLaunchPath:@
setLaunchPath :: (IsNSTask nsTask, IsNSString value) => nsTask -> value -> IO ()
setLaunchPath nsTask value =
  sendMessage nsTask setLaunchPathSelector (toNSString value)

-- | @- currentDirectoryPath@
currentDirectoryPath :: IsNSTask nsTask => nsTask -> IO (Id NSString)
currentDirectoryPath nsTask =
  sendMessage nsTask currentDirectoryPathSelector

-- | @- setCurrentDirectoryPath:@
setCurrentDirectoryPath :: (IsNSTask nsTask, IsNSString value) => nsTask -> value -> IO ()
setCurrentDirectoryPath nsTask value =
  sendMessage nsTask setCurrentDirectoryPathSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTask)
initSelector = mkSelector "init"

-- | @Selector@ for @launchAndReturnError:@
launchAndReturnErrorSelector :: Selector '[Id NSError] Bool
launchAndReturnErrorSelector = mkSelector "launchAndReturnError:"

-- | @Selector@ for @interrupt@
interruptSelector :: Selector '[] ()
interruptSelector = mkSelector "interrupt"

-- | @Selector@ for @terminate@
terminateSelector :: Selector '[] ()
terminateSelector = mkSelector "terminate"

-- | @Selector@ for @suspend@
suspendSelector :: Selector '[] Bool
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] Bool
resumeSelector = mkSelector "resume"

-- | @Selector@ for @launch@
launchSelector :: Selector '[] ()
launchSelector = mkSelector "launch"

-- | @Selector@ for @launchedTaskWithLaunchPath:arguments:@
launchedTaskWithLaunchPath_argumentsSelector :: Selector '[Id NSString, Id NSArray] (Id NSTask)
launchedTaskWithLaunchPath_argumentsSelector = mkSelector "launchedTaskWithLaunchPath:arguments:"

-- | @Selector@ for @launchedTaskWithExecutableURL:arguments:error:terminationHandler:@
launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector :: Selector '[Id NSURL, Id NSArray, Id NSError, Ptr ()] (Id NSTask)
launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector = mkSelector "launchedTaskWithExecutableURL:arguments:error:terminationHandler:"

-- | @Selector@ for @waitUntilExit@
waitUntilExitSelector :: Selector '[] ()
waitUntilExitSelector = mkSelector "waitUntilExit"

-- | @Selector@ for @executableURL@
executableURLSelector :: Selector '[] (Id NSURL)
executableURLSelector = mkSelector "executableURL"

-- | @Selector@ for @setExecutableURL:@
setExecutableURLSelector :: Selector '[Id NSURL] ()
setExecutableURLSelector = mkSelector "setExecutableURL:"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector '[Id NSArray] ()
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @environment@
environmentSelector :: Selector '[] (Id NSDictionary)
environmentSelector = mkSelector "environment"

-- | @Selector@ for @setEnvironment:@
setEnvironmentSelector :: Selector '[Id NSDictionary] ()
setEnvironmentSelector = mkSelector "setEnvironment:"

-- | @Selector@ for @currentDirectoryURL@
currentDirectoryURLSelector :: Selector '[] (Id NSURL)
currentDirectoryURLSelector = mkSelector "currentDirectoryURL"

-- | @Selector@ for @setCurrentDirectoryURL:@
setCurrentDirectoryURLSelector :: Selector '[Id NSURL] ()
setCurrentDirectoryURLSelector = mkSelector "setCurrentDirectoryURL:"

-- | @Selector@ for @launchRequirementData@
launchRequirementDataSelector :: Selector '[] (Id NSData)
launchRequirementDataSelector = mkSelector "launchRequirementData"

-- | @Selector@ for @setLaunchRequirementData:@
setLaunchRequirementDataSelector :: Selector '[Id NSData] ()
setLaunchRequirementDataSelector = mkSelector "setLaunchRequirementData:"

-- | @Selector@ for @standardInput@
standardInputSelector :: Selector '[] RawId
standardInputSelector = mkSelector "standardInput"

-- | @Selector@ for @setStandardInput:@
setStandardInputSelector :: Selector '[RawId] ()
setStandardInputSelector = mkSelector "setStandardInput:"

-- | @Selector@ for @standardOutput@
standardOutputSelector :: Selector '[] RawId
standardOutputSelector = mkSelector "standardOutput"

-- | @Selector@ for @setStandardOutput:@
setStandardOutputSelector :: Selector '[RawId] ()
setStandardOutputSelector = mkSelector "setStandardOutput:"

-- | @Selector@ for @standardError@
standardErrorSelector :: Selector '[] RawId
standardErrorSelector = mkSelector "standardError"

-- | @Selector@ for @setStandardError:@
setStandardErrorSelector :: Selector '[RawId] ()
setStandardErrorSelector = mkSelector "setStandardError:"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector '[] CInt
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @terminationStatus@
terminationStatusSelector :: Selector '[] CInt
terminationStatusSelector = mkSelector "terminationStatus"

-- | @Selector@ for @terminationReason@
terminationReasonSelector :: Selector '[] NSTaskTerminationReason
terminationReasonSelector = mkSelector "terminationReason"

-- | @Selector@ for @terminationHandler@
terminationHandlerSelector :: Selector '[] (Ptr ())
terminationHandlerSelector = mkSelector "terminationHandler"

-- | @Selector@ for @setTerminationHandler:@
setTerminationHandlerSelector :: Selector '[Ptr ()] ()
setTerminationHandlerSelector = mkSelector "setTerminationHandler:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector '[] NSQualityOfService
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector '[NSQualityOfService] ()
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @launchPath@
launchPathSelector :: Selector '[] (Id NSString)
launchPathSelector = mkSelector "launchPath"

-- | @Selector@ for @setLaunchPath:@
setLaunchPathSelector :: Selector '[Id NSString] ()
setLaunchPathSelector = mkSelector "setLaunchPath:"

-- | @Selector@ for @currentDirectoryPath@
currentDirectoryPathSelector :: Selector '[] (Id NSString)
currentDirectoryPathSelector = mkSelector "currentDirectoryPath"

-- | @Selector@ for @setCurrentDirectoryPath:@
setCurrentDirectoryPathSelector :: Selector '[Id NSString] ()
setCurrentDirectoryPathSelector = mkSelector "setCurrentDirectoryPath:"

