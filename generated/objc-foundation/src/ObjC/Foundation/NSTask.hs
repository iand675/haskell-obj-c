{-# LANGUAGE PatternSynonyms #-}
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
  , arguments
  , setArguments
  , environment
  , setEnvironment
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
  , initSelector
  , launchAndReturnErrorSelector
  , interruptSelector
  , terminateSelector
  , suspendSelector
  , resumeSelector
  , launchSelector
  , launchedTaskWithLaunchPath_argumentsSelector
  , launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector
  , waitUntilExitSelector
  , argumentsSelector
  , setArgumentsSelector
  , environmentSelector
  , setEnvironmentSelector
  , standardInputSelector
  , setStandardInputSelector
  , standardOutputSelector
  , setStandardOutputSelector
  , standardErrorSelector
  , setStandardErrorSelector
  , processIdentifierSelector
  , runningSelector
  , terminationStatusSelector
  , terminationReasonSelector
  , terminationHandlerSelector
  , setTerminationHandlerSelector
  , qualityOfServiceSelector
  , setQualityOfServiceSelector

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

-- | @- init@
init_ :: IsNSTask nsTask => nsTask -> IO (Id NSTask)
init_ nsTask  =
  sendMsg nsTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- launchAndReturnError:@
launchAndReturnError :: (IsNSTask nsTask, IsNSError error_) => nsTask -> error_ -> IO Bool
launchAndReturnError nsTask  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTask (mkSelector "launchAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- interrupt@
interrupt :: IsNSTask nsTask => nsTask -> IO ()
interrupt nsTask  =
  sendMsg nsTask (mkSelector "interrupt") retVoid []

-- | @- terminate@
terminate :: IsNSTask nsTask => nsTask -> IO ()
terminate nsTask  =
  sendMsg nsTask (mkSelector "terminate") retVoid []

-- | @- suspend@
suspend :: IsNSTask nsTask => nsTask -> IO Bool
suspend nsTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTask (mkSelector "suspend") retCULong []

-- | @- resume@
resume :: IsNSTask nsTask => nsTask -> IO Bool
resume nsTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTask (mkSelector "resume") retCULong []

-- | @- launch@
launch :: IsNSTask nsTask => nsTask -> IO ()
launch nsTask  =
  sendMsg nsTask (mkSelector "launch") retVoid []

-- | @+ launchedTaskWithLaunchPath:arguments:@
launchedTaskWithLaunchPath_arguments :: (IsNSString path, IsNSArray arguments) => path -> arguments -> IO (Id NSTask)
launchedTaskWithLaunchPath_arguments path arguments =
  do
    cls' <- getRequiredClass "NSTask"
    withObjCPtr path $ \raw_path ->
      withObjCPtr arguments $ \raw_arguments ->
        sendClassMsg cls' (mkSelector "launchedTaskWithLaunchPath:arguments:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr

-- | @+ launchedTaskWithExecutableURL:arguments:error:terminationHandler:@
launchedTaskWithExecutableURL_arguments_error_terminationHandler :: (IsNSURL url, IsNSArray arguments, IsNSError error_) => url -> arguments -> error_ -> Ptr () -> IO (Id NSTask)
launchedTaskWithExecutableURL_arguments_error_terminationHandler url arguments error_ terminationHandler =
  do
    cls' <- getRequiredClass "NSTask"
    withObjCPtr url $ \raw_url ->
      withObjCPtr arguments $ \raw_arguments ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "launchedTaskWithExecutableURL:arguments:error:terminationHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr terminationHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- waitUntilExit@
waitUntilExit :: IsNSTask nsTask => nsTask -> IO ()
waitUntilExit nsTask  =
  sendMsg nsTask (mkSelector "waitUntilExit") retVoid []

-- | @- arguments@
arguments :: IsNSTask nsTask => nsTask -> IO (Id NSArray)
arguments nsTask  =
  sendMsg nsTask (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArguments:@
setArguments :: (IsNSTask nsTask, IsNSArray value) => nsTask -> value -> IO ()
setArguments nsTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTask (mkSelector "setArguments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- environment@
environment :: IsNSTask nsTask => nsTask -> IO (Id NSDictionary)
environment nsTask  =
  sendMsg nsTask (mkSelector "environment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnvironment:@
setEnvironment :: (IsNSTask nsTask, IsNSDictionary value) => nsTask -> value -> IO ()
setEnvironment nsTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTask (mkSelector "setEnvironment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standardInput@
standardInput :: IsNSTask nsTask => nsTask -> IO RawId
standardInput nsTask  =
  fmap (RawId . castPtr) $ sendMsg nsTask (mkSelector "standardInput") (retPtr retVoid) []

-- | @- setStandardInput:@
setStandardInput :: IsNSTask nsTask => nsTask -> RawId -> IO ()
setStandardInput nsTask  value =
  sendMsg nsTask (mkSelector "setStandardInput:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- standardOutput@
standardOutput :: IsNSTask nsTask => nsTask -> IO RawId
standardOutput nsTask  =
  fmap (RawId . castPtr) $ sendMsg nsTask (mkSelector "standardOutput") (retPtr retVoid) []

-- | @- setStandardOutput:@
setStandardOutput :: IsNSTask nsTask => nsTask -> RawId -> IO ()
setStandardOutput nsTask  value =
  sendMsg nsTask (mkSelector "setStandardOutput:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- standardError@
standardError :: IsNSTask nsTask => nsTask -> IO RawId
standardError nsTask  =
  fmap (RawId . castPtr) $ sendMsg nsTask (mkSelector "standardError") (retPtr retVoid) []

-- | @- setStandardError:@
setStandardError :: IsNSTask nsTask => nsTask -> RawId -> IO ()
setStandardError nsTask  value =
  sendMsg nsTask (mkSelector "setStandardError:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- processIdentifier@
processIdentifier :: IsNSTask nsTask => nsTask -> IO CInt
processIdentifier nsTask  =
  sendMsg nsTask (mkSelector "processIdentifier") retCInt []

-- | @- running@
running :: IsNSTask nsTask => nsTask -> IO Bool
running nsTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTask (mkSelector "running") retCULong []

-- | @- terminationStatus@
terminationStatus :: IsNSTask nsTask => nsTask -> IO CInt
terminationStatus nsTask  =
  sendMsg nsTask (mkSelector "terminationStatus") retCInt []

-- | @- terminationReason@
terminationReason :: IsNSTask nsTask => nsTask -> IO NSTaskTerminationReason
terminationReason nsTask  =
  fmap (coerce :: CLong -> NSTaskTerminationReason) $ sendMsg nsTask (mkSelector "terminationReason") retCLong []

-- | @- terminationHandler@
terminationHandler :: IsNSTask nsTask => nsTask -> IO (Ptr ())
terminationHandler nsTask  =
  fmap castPtr $ sendMsg nsTask (mkSelector "terminationHandler") (retPtr retVoid) []

-- | @- setTerminationHandler:@
setTerminationHandler :: IsNSTask nsTask => nsTask -> Ptr () -> IO ()
setTerminationHandler nsTask  value =
  sendMsg nsTask (mkSelector "setTerminationHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- qualityOfService@
qualityOfService :: IsNSTask nsTask => nsTask -> IO NSQualityOfService
qualityOfService nsTask  =
  fmap (coerce :: CLong -> NSQualityOfService) $ sendMsg nsTask (mkSelector "qualityOfService") retCLong []

-- | @- setQualityOfService:@
setQualityOfService :: IsNSTask nsTask => nsTask -> NSQualityOfService -> IO ()
setQualityOfService nsTask  value =
  sendMsg nsTask (mkSelector "setQualityOfService:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @launchAndReturnError:@
launchAndReturnErrorSelector :: Selector
launchAndReturnErrorSelector = mkSelector "launchAndReturnError:"

-- | @Selector@ for @interrupt@
interruptSelector :: Selector
interruptSelector = mkSelector "interrupt"

-- | @Selector@ for @terminate@
terminateSelector :: Selector
terminateSelector = mkSelector "terminate"

-- | @Selector@ for @suspend@
suspendSelector :: Selector
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @launch@
launchSelector :: Selector
launchSelector = mkSelector "launch"

-- | @Selector@ for @launchedTaskWithLaunchPath:arguments:@
launchedTaskWithLaunchPath_argumentsSelector :: Selector
launchedTaskWithLaunchPath_argumentsSelector = mkSelector "launchedTaskWithLaunchPath:arguments:"

-- | @Selector@ for @launchedTaskWithExecutableURL:arguments:error:terminationHandler:@
launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector :: Selector
launchedTaskWithExecutableURL_arguments_error_terminationHandlerSelector = mkSelector "launchedTaskWithExecutableURL:arguments:error:terminationHandler:"

-- | @Selector@ for @waitUntilExit@
waitUntilExitSelector :: Selector
waitUntilExitSelector = mkSelector "waitUntilExit"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @environment@
environmentSelector :: Selector
environmentSelector = mkSelector "environment"

-- | @Selector@ for @setEnvironment:@
setEnvironmentSelector :: Selector
setEnvironmentSelector = mkSelector "setEnvironment:"

-- | @Selector@ for @standardInput@
standardInputSelector :: Selector
standardInputSelector = mkSelector "standardInput"

-- | @Selector@ for @setStandardInput:@
setStandardInputSelector :: Selector
setStandardInputSelector = mkSelector "setStandardInput:"

-- | @Selector@ for @standardOutput@
standardOutputSelector :: Selector
standardOutputSelector = mkSelector "standardOutput"

-- | @Selector@ for @setStandardOutput:@
setStandardOutputSelector :: Selector
setStandardOutputSelector = mkSelector "setStandardOutput:"

-- | @Selector@ for @standardError@
standardErrorSelector :: Selector
standardErrorSelector = mkSelector "standardError"

-- | @Selector@ for @setStandardError:@
setStandardErrorSelector :: Selector
setStandardErrorSelector = mkSelector "setStandardError:"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @terminationStatus@
terminationStatusSelector :: Selector
terminationStatusSelector = mkSelector "terminationStatus"

-- | @Selector@ for @terminationReason@
terminationReasonSelector :: Selector
terminationReasonSelector = mkSelector "terminationReason"

-- | @Selector@ for @terminationHandler@
terminationHandlerSelector :: Selector
terminationHandlerSelector = mkSelector "terminationHandler"

-- | @Selector@ for @setTerminationHandler:@
setTerminationHandlerSelector :: Selector
setTerminationHandlerSelector = mkSelector "setTerminationHandler:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

