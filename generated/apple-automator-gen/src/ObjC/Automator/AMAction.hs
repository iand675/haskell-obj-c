{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activatedSelector
  , closedSelector
  , didFinishRunningWithErrorSelector
  , finishRunningWithErrorSelector
  , ignoresInputSelector
  , initWithContentsOfURL_errorSelector
  , initWithDefinition_fromArchiveSelector
  , logMessageWithLevel_formatSelector
  , nameSelector
  , openedSelector
  , outputSelector
  , parametersUpdatedSelector
  , progressValueSelector
  , resetSelector
  , runAsynchronouslyWithInputSelector
  , runWithInput_errorSelector
  , runWithInput_fromAction_errorSelector
  , selectedInputTypeSelector
  , selectedOutputTypeSelector
  , setOutputSelector
  , setProgressValueSelector
  , setSelectedInputTypeSelector
  , setSelectedOutputTypeSelector
  , stopSelector
  , stoppedSelector
  , updateParametersSelector
  , willFinishRunningSelector
  , writeToDictionarySelector

  -- * Enum types
  , AMLogLevel(AMLogLevel)
  , pattern AMLogLevelDebug
  , pattern AMLogLevelInfo
  , pattern AMLogLevelWarn
  , pattern AMLogLevelError

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Automator.Internal.Classes
import ObjC.Automator.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDefinition:fromArchive:@
initWithDefinition_fromArchive :: (IsAMAction amAction, IsNSDictionary dict) => amAction -> dict -> Bool -> IO (Id AMAction)
initWithDefinition_fromArchive amAction dict archived =
  sendOwnedMessage amAction initWithDefinition_fromArchiveSelector (toNSDictionary dict) archived

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsAMAction amAction, IsNSURL fileURL, IsNSError outError) => amAction -> fileURL -> outError -> IO (Id AMAction)
initWithContentsOfURL_error amAction fileURL outError =
  sendOwnedMessage amAction initWithContentsOfURL_errorSelector (toNSURL fileURL) (toNSError outError)

-- | @- runWithInput:fromAction:error:@
runWithInput_fromAction_error :: (IsAMAction amAction, IsAMAction anAction, IsNSDictionary errorInfo) => amAction -> RawId -> anAction -> errorInfo -> IO RawId
runWithInput_fromAction_error amAction input anAction errorInfo =
  sendMessage amAction runWithInput_fromAction_errorSelector input (toAMAction anAction) (toNSDictionary errorInfo)

-- | @- runWithInput:error:@
runWithInput_error :: (IsAMAction amAction, IsNSError error_) => amAction -> RawId -> error_ -> IO RawId
runWithInput_error amAction input error_ =
  sendMessage amAction runWithInput_errorSelector input (toNSError error_)

-- | @- runAsynchronouslyWithInput:@
runAsynchronouslyWithInput :: IsAMAction amAction => amAction -> RawId -> IO ()
runAsynchronouslyWithInput amAction input =
  sendMessage amAction runAsynchronouslyWithInputSelector input

-- | @- willFinishRunning@
willFinishRunning :: IsAMAction amAction => amAction -> IO ()
willFinishRunning amAction =
  sendMessage amAction willFinishRunningSelector

-- | @- didFinishRunningWithError:@
didFinishRunningWithError :: (IsAMAction amAction, IsNSDictionary errorInfo) => amAction -> errorInfo -> IO ()
didFinishRunningWithError amAction errorInfo =
  sendMessage amAction didFinishRunningWithErrorSelector (toNSDictionary errorInfo)

-- | @- finishRunningWithError:@
finishRunningWithError :: (IsAMAction amAction, IsNSError error_) => amAction -> error_ -> IO ()
finishRunningWithError amAction error_ =
  sendMessage amAction finishRunningWithErrorSelector (toNSError error_)

-- | @- stop@
stop :: IsAMAction amAction => amAction -> IO ()
stop amAction =
  sendMessage amAction stopSelector

-- | @- reset@
reset :: IsAMAction amAction => amAction -> IO ()
reset amAction =
  sendMessage amAction resetSelector

-- | @- writeToDictionary:@
writeToDictionary :: (IsAMAction amAction, IsNSMutableDictionary dictionary) => amAction -> dictionary -> IO ()
writeToDictionary amAction dictionary =
  sendMessage amAction writeToDictionarySelector (toNSMutableDictionary dictionary)

-- | @- opened@
opened :: IsAMAction amAction => amAction -> IO ()
opened amAction =
  sendMessage amAction openedSelector

-- | @- activated@
activated :: IsAMAction amAction => amAction -> IO ()
activated amAction =
  sendMessage amAction activatedSelector

-- | @- closed@
closed :: IsAMAction amAction => amAction -> IO ()
closed amAction =
  sendMessage amAction closedSelector

-- | @- updateParameters@
updateParameters :: IsAMAction amAction => amAction -> IO ()
updateParameters amAction =
  sendMessage amAction updateParametersSelector

-- | @- parametersUpdated@
parametersUpdated :: IsAMAction amAction => amAction -> IO ()
parametersUpdated amAction =
  sendMessage amAction parametersUpdatedSelector

-- | @- logMessageWithLevel:format:@
logMessageWithLevel_format :: (IsAMAction amAction, IsNSString format) => amAction -> AMLogLevel -> format -> IO ()
logMessageWithLevel_format amAction level format =
  sendMessage amAction logMessageWithLevel_formatSelector level (toNSString format)

-- | @- name@
name :: IsAMAction amAction => amAction -> IO RawId
name amAction =
  sendMessage amAction nameSelector

-- | @- ignoresInput@
ignoresInput :: IsAMAction amAction => amAction -> IO Bool
ignoresInput amAction =
  sendMessage amAction ignoresInputSelector

-- | @- selectedInputType@
selectedInputType :: IsAMAction amAction => amAction -> IO RawId
selectedInputType amAction =
  sendMessage amAction selectedInputTypeSelector

-- | @- setSelectedInputType:@
setSelectedInputType :: IsAMAction amAction => amAction -> RawId -> IO ()
setSelectedInputType amAction value =
  sendMessage amAction setSelectedInputTypeSelector value

-- | @- selectedOutputType@
selectedOutputType :: IsAMAction amAction => amAction -> IO RawId
selectedOutputType amAction =
  sendMessage amAction selectedOutputTypeSelector

-- | @- setSelectedOutputType:@
setSelectedOutputType :: IsAMAction amAction => amAction -> RawId -> IO ()
setSelectedOutputType amAction value =
  sendMessage amAction setSelectedOutputTypeSelector value

-- | @- progressValue@
progressValue :: IsAMAction amAction => amAction -> IO CDouble
progressValue amAction =
  sendMessage amAction progressValueSelector

-- | @- setProgressValue:@
setProgressValue :: IsAMAction amAction => amAction -> CDouble -> IO ()
setProgressValue amAction value =
  sendMessage amAction setProgressValueSelector value

-- | @- output@
output :: IsAMAction amAction => amAction -> IO RawId
output amAction =
  sendMessage amAction outputSelector

-- | @- setOutput:@
setOutput :: IsAMAction amAction => amAction -> RawId -> IO ()
setOutput amAction value =
  sendMessage amAction setOutputSelector value

-- | @- stopped@
stopped :: IsAMAction amAction => amAction -> IO Bool
stopped amAction =
  sendMessage amAction stoppedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDefinition:fromArchive:@
initWithDefinition_fromArchiveSelector :: Selector '[Id NSDictionary, Bool] (Id AMAction)
initWithDefinition_fromArchiveSelector = mkSelector "initWithDefinition:fromArchive:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id AMAction)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @runWithInput:fromAction:error:@
runWithInput_fromAction_errorSelector :: Selector '[RawId, Id AMAction, Id NSDictionary] RawId
runWithInput_fromAction_errorSelector = mkSelector "runWithInput:fromAction:error:"

-- | @Selector@ for @runWithInput:error:@
runWithInput_errorSelector :: Selector '[RawId, Id NSError] RawId
runWithInput_errorSelector = mkSelector "runWithInput:error:"

-- | @Selector@ for @runAsynchronouslyWithInput:@
runAsynchronouslyWithInputSelector :: Selector '[RawId] ()
runAsynchronouslyWithInputSelector = mkSelector "runAsynchronouslyWithInput:"

-- | @Selector@ for @willFinishRunning@
willFinishRunningSelector :: Selector '[] ()
willFinishRunningSelector = mkSelector "willFinishRunning"

-- | @Selector@ for @didFinishRunningWithError:@
didFinishRunningWithErrorSelector :: Selector '[Id NSDictionary] ()
didFinishRunningWithErrorSelector = mkSelector "didFinishRunningWithError:"

-- | @Selector@ for @finishRunningWithError:@
finishRunningWithErrorSelector :: Selector '[Id NSError] ()
finishRunningWithErrorSelector = mkSelector "finishRunningWithError:"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @writeToDictionary:@
writeToDictionarySelector :: Selector '[Id NSMutableDictionary] ()
writeToDictionarySelector = mkSelector "writeToDictionary:"

-- | @Selector@ for @opened@
openedSelector :: Selector '[] ()
openedSelector = mkSelector "opened"

-- | @Selector@ for @activated@
activatedSelector :: Selector '[] ()
activatedSelector = mkSelector "activated"

-- | @Selector@ for @closed@
closedSelector :: Selector '[] ()
closedSelector = mkSelector "closed"

-- | @Selector@ for @updateParameters@
updateParametersSelector :: Selector '[] ()
updateParametersSelector = mkSelector "updateParameters"

-- | @Selector@ for @parametersUpdated@
parametersUpdatedSelector :: Selector '[] ()
parametersUpdatedSelector = mkSelector "parametersUpdated"

-- | @Selector@ for @logMessageWithLevel:format:@
logMessageWithLevel_formatSelector :: Selector '[AMLogLevel, Id NSString] ()
logMessageWithLevel_formatSelector = mkSelector "logMessageWithLevel:format:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] RawId
nameSelector = mkSelector "name"

-- | @Selector@ for @ignoresInput@
ignoresInputSelector :: Selector '[] Bool
ignoresInputSelector = mkSelector "ignoresInput"

-- | @Selector@ for @selectedInputType@
selectedInputTypeSelector :: Selector '[] RawId
selectedInputTypeSelector = mkSelector "selectedInputType"

-- | @Selector@ for @setSelectedInputType:@
setSelectedInputTypeSelector :: Selector '[RawId] ()
setSelectedInputTypeSelector = mkSelector "setSelectedInputType:"

-- | @Selector@ for @selectedOutputType@
selectedOutputTypeSelector :: Selector '[] RawId
selectedOutputTypeSelector = mkSelector "selectedOutputType"

-- | @Selector@ for @setSelectedOutputType:@
setSelectedOutputTypeSelector :: Selector '[RawId] ()
setSelectedOutputTypeSelector = mkSelector "setSelectedOutputType:"

-- | @Selector@ for @progressValue@
progressValueSelector :: Selector '[] CDouble
progressValueSelector = mkSelector "progressValue"

-- | @Selector@ for @setProgressValue:@
setProgressValueSelector :: Selector '[CDouble] ()
setProgressValueSelector = mkSelector "setProgressValue:"

-- | @Selector@ for @output@
outputSelector :: Selector '[] RawId
outputSelector = mkSelector "output"

-- | @Selector@ for @setOutput:@
setOutputSelector :: Selector '[RawId] ()
setOutputSelector = mkSelector "setOutput:"

-- | @Selector@ for @stopped@
stoppedSelector :: Selector '[] Bool
stoppedSelector = mkSelector "stopped"

