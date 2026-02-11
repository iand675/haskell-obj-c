{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMAction@.
module ObjC.Automator.AMAction
  ( AMAction
  , IsAMAction(..)
  , initWithDefinition_fromArchive
  , initWithContentsOfURL_error
  , runWithInput_fromAction_error
  , runWithInput_error
  , runAsynchronouslyWithInput
  , willFinishRunning
  , didFinishRunningWithError
  , finishRunningWithError
  , stop
  , reset
  , writeToDictionary
  , opened
  , activated
  , closed
  , updateParameters
  , parametersUpdated
  , logMessageWithLevel_format
  , name
  , ignoresInput
  , selectedInputType
  , setSelectedInputType
  , selectedOutputType
  , setSelectedOutputType
  , progressValue
  , setProgressValue
  , output
  , setOutput
  , stopped
  , initWithDefinition_fromArchiveSelector
  , initWithContentsOfURL_errorSelector
  , runWithInput_fromAction_errorSelector
  , runWithInput_errorSelector
  , runAsynchronouslyWithInputSelector
  , willFinishRunningSelector
  , didFinishRunningWithErrorSelector
  , finishRunningWithErrorSelector
  , stopSelector
  , resetSelector
  , writeToDictionarySelector
  , openedSelector
  , activatedSelector
  , closedSelector
  , updateParametersSelector
  , parametersUpdatedSelector
  , logMessageWithLevel_formatSelector
  , nameSelector
  , ignoresInputSelector
  , selectedInputTypeSelector
  , setSelectedInputTypeSelector
  , selectedOutputTypeSelector
  , setSelectedOutputTypeSelector
  , progressValueSelector
  , setProgressValueSelector
  , outputSelector
  , setOutputSelector
  , stoppedSelector

  -- * Enum types
  , AMLogLevel(AMLogLevel)
  , pattern AMLogLevelDebug
  , pattern AMLogLevelInfo
  , pattern AMLogLevelWarn
  , pattern AMLogLevelError

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

import ObjC.Automator.Internal.Classes
import ObjC.Automator.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDefinition:fromArchive:@
initWithDefinition_fromArchive :: (IsAMAction amAction, IsNSDictionary dict) => amAction -> dict -> Bool -> IO (Id AMAction)
initWithDefinition_fromArchive amAction  dict archived =
  withObjCPtr dict $ \raw_dict ->
      sendMsg amAction (mkSelector "initWithDefinition:fromArchive:") (retPtr retVoid) [argPtr (castPtr raw_dict :: Ptr ()), argCULong (if archived then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsAMAction amAction, IsNSURL fileURL, IsNSError outError) => amAction -> fileURL -> outError -> IO (Id AMAction)
initWithContentsOfURL_error amAction  fileURL outError =
  withObjCPtr fileURL $ \raw_fileURL ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg amAction (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- runWithInput:fromAction:error:@
runWithInput_fromAction_error :: (IsAMAction amAction, IsAMAction anAction, IsNSDictionary errorInfo) => amAction -> RawId -> anAction -> errorInfo -> IO RawId
runWithInput_fromAction_error amAction  input anAction errorInfo =
  withObjCPtr anAction $ \raw_anAction ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        fmap (RawId . castPtr) $ sendMsg amAction (mkSelector "runWithInput:fromAction:error:") (retPtr retVoid) [argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr raw_anAction :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- runWithInput:error:@
runWithInput_error :: (IsAMAction amAction, IsNSError error_) => amAction -> RawId -> error_ -> IO RawId
runWithInput_error amAction  input error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg amAction (mkSelector "runWithInput:error:") (retPtr retVoid) [argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- runAsynchronouslyWithInput:@
runAsynchronouslyWithInput :: IsAMAction amAction => amAction -> RawId -> IO ()
runAsynchronouslyWithInput amAction  input =
    sendMsg amAction (mkSelector "runAsynchronouslyWithInput:") retVoid [argPtr (castPtr (unRawId input) :: Ptr ())]

-- | @- willFinishRunning@
willFinishRunning :: IsAMAction amAction => amAction -> IO ()
willFinishRunning amAction  =
    sendMsg amAction (mkSelector "willFinishRunning") retVoid []

-- | @- didFinishRunningWithError:@
didFinishRunningWithError :: (IsAMAction amAction, IsNSDictionary errorInfo) => amAction -> errorInfo -> IO ()
didFinishRunningWithError amAction  errorInfo =
  withObjCPtr errorInfo $ \raw_errorInfo ->
      sendMsg amAction (mkSelector "didFinishRunningWithError:") retVoid [argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- finishRunningWithError:@
finishRunningWithError :: (IsAMAction amAction, IsNSError error_) => amAction -> error_ -> IO ()
finishRunningWithError amAction  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg amAction (mkSelector "finishRunningWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- stop@
stop :: IsAMAction amAction => amAction -> IO ()
stop amAction  =
    sendMsg amAction (mkSelector "stop") retVoid []

-- | @- reset@
reset :: IsAMAction amAction => amAction -> IO ()
reset amAction  =
    sendMsg amAction (mkSelector "reset") retVoid []

-- | @- writeToDictionary:@
writeToDictionary :: (IsAMAction amAction, IsNSMutableDictionary dictionary) => amAction -> dictionary -> IO ()
writeToDictionary amAction  dictionary =
  withObjCPtr dictionary $ \raw_dictionary ->
      sendMsg amAction (mkSelector "writeToDictionary:") retVoid [argPtr (castPtr raw_dictionary :: Ptr ())]

-- | @- opened@
opened :: IsAMAction amAction => amAction -> IO ()
opened amAction  =
    sendMsg amAction (mkSelector "opened") retVoid []

-- | @- activated@
activated :: IsAMAction amAction => amAction -> IO ()
activated amAction  =
    sendMsg amAction (mkSelector "activated") retVoid []

-- | @- closed@
closed :: IsAMAction amAction => amAction -> IO ()
closed amAction  =
    sendMsg amAction (mkSelector "closed") retVoid []

-- | @- updateParameters@
updateParameters :: IsAMAction amAction => amAction -> IO ()
updateParameters amAction  =
    sendMsg amAction (mkSelector "updateParameters") retVoid []

-- | @- parametersUpdated@
parametersUpdated :: IsAMAction amAction => amAction -> IO ()
parametersUpdated amAction  =
    sendMsg amAction (mkSelector "parametersUpdated") retVoid []

-- | @- logMessageWithLevel:format:@
logMessageWithLevel_format :: (IsAMAction amAction, IsNSString format) => amAction -> AMLogLevel -> format -> IO ()
logMessageWithLevel_format amAction  level format =
  withObjCPtr format $ \raw_format ->
      sendMsg amAction (mkSelector "logMessageWithLevel:format:") retVoid [argCULong (coerce level), argPtr (castPtr raw_format :: Ptr ())]

-- | @- name@
name :: IsAMAction amAction => amAction -> IO RawId
name amAction  =
    fmap (RawId . castPtr) $ sendMsg amAction (mkSelector "name") (retPtr retVoid) []

-- | @- ignoresInput@
ignoresInput :: IsAMAction amAction => amAction -> IO Bool
ignoresInput amAction  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg amAction (mkSelector "ignoresInput") retCULong []

-- | @- selectedInputType@
selectedInputType :: IsAMAction amAction => amAction -> IO RawId
selectedInputType amAction  =
    fmap (RawId . castPtr) $ sendMsg amAction (mkSelector "selectedInputType") (retPtr retVoid) []

-- | @- setSelectedInputType:@
setSelectedInputType :: IsAMAction amAction => amAction -> RawId -> IO ()
setSelectedInputType amAction  value =
    sendMsg amAction (mkSelector "setSelectedInputType:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- selectedOutputType@
selectedOutputType :: IsAMAction amAction => amAction -> IO RawId
selectedOutputType amAction  =
    fmap (RawId . castPtr) $ sendMsg amAction (mkSelector "selectedOutputType") (retPtr retVoid) []

-- | @- setSelectedOutputType:@
setSelectedOutputType :: IsAMAction amAction => amAction -> RawId -> IO ()
setSelectedOutputType amAction  value =
    sendMsg amAction (mkSelector "setSelectedOutputType:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- progressValue@
progressValue :: IsAMAction amAction => amAction -> IO CDouble
progressValue amAction  =
    sendMsg amAction (mkSelector "progressValue") retCDouble []

-- | @- setProgressValue:@
setProgressValue :: IsAMAction amAction => amAction -> CDouble -> IO ()
setProgressValue amAction  value =
    sendMsg amAction (mkSelector "setProgressValue:") retVoid [argCDouble value]

-- | @- output@
output :: IsAMAction amAction => amAction -> IO RawId
output amAction  =
    fmap (RawId . castPtr) $ sendMsg amAction (mkSelector "output") (retPtr retVoid) []

-- | @- setOutput:@
setOutput :: IsAMAction amAction => amAction -> RawId -> IO ()
setOutput amAction  value =
    sendMsg amAction (mkSelector "setOutput:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- stopped@
stopped :: IsAMAction amAction => amAction -> IO Bool
stopped amAction  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg amAction (mkSelector "stopped") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDefinition:fromArchive:@
initWithDefinition_fromArchiveSelector :: Selector
initWithDefinition_fromArchiveSelector = mkSelector "initWithDefinition:fromArchive:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @runWithInput:fromAction:error:@
runWithInput_fromAction_errorSelector :: Selector
runWithInput_fromAction_errorSelector = mkSelector "runWithInput:fromAction:error:"

-- | @Selector@ for @runWithInput:error:@
runWithInput_errorSelector :: Selector
runWithInput_errorSelector = mkSelector "runWithInput:error:"

-- | @Selector@ for @runAsynchronouslyWithInput:@
runAsynchronouslyWithInputSelector :: Selector
runAsynchronouslyWithInputSelector = mkSelector "runAsynchronouslyWithInput:"

-- | @Selector@ for @willFinishRunning@
willFinishRunningSelector :: Selector
willFinishRunningSelector = mkSelector "willFinishRunning"

-- | @Selector@ for @didFinishRunningWithError:@
didFinishRunningWithErrorSelector :: Selector
didFinishRunningWithErrorSelector = mkSelector "didFinishRunningWithError:"

-- | @Selector@ for @finishRunningWithError:@
finishRunningWithErrorSelector :: Selector
finishRunningWithErrorSelector = mkSelector "finishRunningWithError:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @writeToDictionary:@
writeToDictionarySelector :: Selector
writeToDictionarySelector = mkSelector "writeToDictionary:"

-- | @Selector@ for @opened@
openedSelector :: Selector
openedSelector = mkSelector "opened"

-- | @Selector@ for @activated@
activatedSelector :: Selector
activatedSelector = mkSelector "activated"

-- | @Selector@ for @closed@
closedSelector :: Selector
closedSelector = mkSelector "closed"

-- | @Selector@ for @updateParameters@
updateParametersSelector :: Selector
updateParametersSelector = mkSelector "updateParameters"

-- | @Selector@ for @parametersUpdated@
parametersUpdatedSelector :: Selector
parametersUpdatedSelector = mkSelector "parametersUpdated"

-- | @Selector@ for @logMessageWithLevel:format:@
logMessageWithLevel_formatSelector :: Selector
logMessageWithLevel_formatSelector = mkSelector "logMessageWithLevel:format:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @ignoresInput@
ignoresInputSelector :: Selector
ignoresInputSelector = mkSelector "ignoresInput"

-- | @Selector@ for @selectedInputType@
selectedInputTypeSelector :: Selector
selectedInputTypeSelector = mkSelector "selectedInputType"

-- | @Selector@ for @setSelectedInputType:@
setSelectedInputTypeSelector :: Selector
setSelectedInputTypeSelector = mkSelector "setSelectedInputType:"

-- | @Selector@ for @selectedOutputType@
selectedOutputTypeSelector :: Selector
selectedOutputTypeSelector = mkSelector "selectedOutputType"

-- | @Selector@ for @setSelectedOutputType:@
setSelectedOutputTypeSelector :: Selector
setSelectedOutputTypeSelector = mkSelector "setSelectedOutputType:"

-- | @Selector@ for @progressValue@
progressValueSelector :: Selector
progressValueSelector = mkSelector "progressValue"

-- | @Selector@ for @setProgressValue:@
setProgressValueSelector :: Selector
setProgressValueSelector = mkSelector "setProgressValue:"

-- | @Selector@ for @output@
outputSelector :: Selector
outputSelector = mkSelector "output"

-- | @Selector@ for @setOutput:@
setOutputSelector :: Selector
setOutputSelector = mkSelector "setOutput:"

-- | @Selector@ for @stopped@
stoppedSelector :: Selector
stoppedSelector = mkSelector "stopped"

