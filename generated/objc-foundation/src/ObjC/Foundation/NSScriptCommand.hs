{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptCommand@.
module ObjC.Foundation.NSScriptCommand
  ( NSScriptCommand
  , IsNSScriptCommand(..)
  , initWithCommandDescription
  , initWithCoder
  , performDefaultImplementation
  , executeCommand
  , currentCommand
  , suspendExecution
  , resumeExecutionWithResult
  , commandDescription
  , directParameter
  , setDirectParameter
  , receiversSpecifier
  , setReceiversSpecifier
  , evaluatedReceivers
  , arguments
  , setArguments
  , evaluatedArguments
  , wellFormed
  , scriptErrorNumber
  , setScriptErrorNumber
  , scriptErrorString
  , setScriptErrorString
  , appleEvent
  , initWithCommandDescriptionSelector
  , initWithCoderSelector
  , performDefaultImplementationSelector
  , executeCommandSelector
  , currentCommandSelector
  , suspendExecutionSelector
  , resumeExecutionWithResultSelector
  , commandDescriptionSelector
  , directParameterSelector
  , setDirectParameterSelector
  , receiversSpecifierSelector
  , setReceiversSpecifierSelector
  , evaluatedReceiversSelector
  , argumentsSelector
  , setArgumentsSelector
  , evaluatedArgumentsSelector
  , wellFormedSelector
  , scriptErrorNumberSelector
  , setScriptErrorNumberSelector
  , scriptErrorStringSelector
  , setScriptErrorStringSelector
  , appleEventSelector


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

-- | @- initWithCommandDescription:@
initWithCommandDescription :: (IsNSScriptCommand nsScriptCommand, IsNSScriptCommandDescription commandDef) => nsScriptCommand -> commandDef -> IO (Id NSScriptCommand)
initWithCommandDescription nsScriptCommand  commandDef =
withObjCPtr commandDef $ \raw_commandDef ->
    sendMsg nsScriptCommand (mkSelector "initWithCommandDescription:") (retPtr retVoid) [argPtr (castPtr raw_commandDef :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptCommand nsScriptCommand, IsNSCoder inCoder) => nsScriptCommand -> inCoder -> IO (Id NSScriptCommand)
initWithCoder nsScriptCommand  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsScriptCommand (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- performDefaultImplementation@
performDefaultImplementation :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
performDefaultImplementation nsScriptCommand  =
  fmap (RawId . castPtr) $ sendMsg nsScriptCommand (mkSelector "performDefaultImplementation") (retPtr retVoid) []

-- | @- executeCommand@
executeCommand :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
executeCommand nsScriptCommand  =
  fmap (RawId . castPtr) $ sendMsg nsScriptCommand (mkSelector "executeCommand") (retPtr retVoid) []

-- | @+ currentCommand@
currentCommand :: IO (Id NSScriptCommand)
currentCommand  =
  do
    cls' <- getRequiredClass "NSScriptCommand"
    sendClassMsg cls' (mkSelector "currentCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- suspendExecution@
suspendExecution :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO ()
suspendExecution nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "suspendExecution") retVoid []

-- | @- resumeExecutionWithResult:@
resumeExecutionWithResult :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> RawId -> IO ()
resumeExecutionWithResult nsScriptCommand  result =
  sendMsg nsScriptCommand (mkSelector "resumeExecutionWithResult:") retVoid [argPtr (castPtr (unRawId result) :: Ptr ())]

-- | @- commandDescription@
commandDescription :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSScriptCommandDescription)
commandDescription nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "commandDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- directParameter@
directParameter :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
directParameter nsScriptCommand  =
  fmap (RawId . castPtr) $ sendMsg nsScriptCommand (mkSelector "directParameter") (retPtr retVoid) []

-- | @- setDirectParameter:@
setDirectParameter :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> RawId -> IO ()
setDirectParameter nsScriptCommand  value =
  sendMsg nsScriptCommand (mkSelector "setDirectParameter:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- receiversSpecifier@
receiversSpecifier :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSScriptObjectSpecifier)
receiversSpecifier nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "receiversSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReceiversSpecifier:@
setReceiversSpecifier :: (IsNSScriptCommand nsScriptCommand, IsNSScriptObjectSpecifier value) => nsScriptCommand -> value -> IO ()
setReceiversSpecifier nsScriptCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptCommand (mkSelector "setReceiversSpecifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- evaluatedReceivers@
evaluatedReceivers :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
evaluatedReceivers nsScriptCommand  =
  fmap (RawId . castPtr) $ sendMsg nsScriptCommand (mkSelector "evaluatedReceivers") (retPtr retVoid) []

-- | @- arguments@
arguments :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSDictionary)
arguments nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArguments:@
setArguments :: (IsNSScriptCommand nsScriptCommand, IsNSDictionary value) => nsScriptCommand -> value -> IO ()
setArguments nsScriptCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptCommand (mkSelector "setArguments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- evaluatedArguments@
evaluatedArguments :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSDictionary)
evaluatedArguments nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "evaluatedArguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- wellFormed@
wellFormed :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO Bool
wellFormed nsScriptCommand  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptCommand (mkSelector "wellFormed") retCULong []

-- | @- scriptErrorNumber@
scriptErrorNumber :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO CLong
scriptErrorNumber nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "scriptErrorNumber") retCLong []

-- | @- setScriptErrorNumber:@
setScriptErrorNumber :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> CLong -> IO ()
setScriptErrorNumber nsScriptCommand  value =
  sendMsg nsScriptCommand (mkSelector "setScriptErrorNumber:") retVoid [argCLong (fromIntegral value)]

-- | @- scriptErrorString@
scriptErrorString :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSString)
scriptErrorString nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "scriptErrorString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScriptErrorString:@
setScriptErrorString :: (IsNSScriptCommand nsScriptCommand, IsNSString value) => nsScriptCommand -> value -> IO ()
setScriptErrorString nsScriptCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptCommand (mkSelector "setScriptErrorString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- appleEvent@
appleEvent :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSAppleEventDescriptor)
appleEvent nsScriptCommand  =
  sendMsg nsScriptCommand (mkSelector "appleEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCommandDescription:@
initWithCommandDescriptionSelector :: Selector
initWithCommandDescriptionSelector = mkSelector "initWithCommandDescription:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @performDefaultImplementation@
performDefaultImplementationSelector :: Selector
performDefaultImplementationSelector = mkSelector "performDefaultImplementation"

-- | @Selector@ for @executeCommand@
executeCommandSelector :: Selector
executeCommandSelector = mkSelector "executeCommand"

-- | @Selector@ for @currentCommand@
currentCommandSelector :: Selector
currentCommandSelector = mkSelector "currentCommand"

-- | @Selector@ for @suspendExecution@
suspendExecutionSelector :: Selector
suspendExecutionSelector = mkSelector "suspendExecution"

-- | @Selector@ for @resumeExecutionWithResult:@
resumeExecutionWithResultSelector :: Selector
resumeExecutionWithResultSelector = mkSelector "resumeExecutionWithResult:"

-- | @Selector@ for @commandDescription@
commandDescriptionSelector :: Selector
commandDescriptionSelector = mkSelector "commandDescription"

-- | @Selector@ for @directParameter@
directParameterSelector :: Selector
directParameterSelector = mkSelector "directParameter"

-- | @Selector@ for @setDirectParameter:@
setDirectParameterSelector :: Selector
setDirectParameterSelector = mkSelector "setDirectParameter:"

-- | @Selector@ for @receiversSpecifier@
receiversSpecifierSelector :: Selector
receiversSpecifierSelector = mkSelector "receiversSpecifier"

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @evaluatedReceivers@
evaluatedReceiversSelector :: Selector
evaluatedReceiversSelector = mkSelector "evaluatedReceivers"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @evaluatedArguments@
evaluatedArgumentsSelector :: Selector
evaluatedArgumentsSelector = mkSelector "evaluatedArguments"

-- | @Selector@ for @wellFormed@
wellFormedSelector :: Selector
wellFormedSelector = mkSelector "wellFormed"

-- | @Selector@ for @scriptErrorNumber@
scriptErrorNumberSelector :: Selector
scriptErrorNumberSelector = mkSelector "scriptErrorNumber"

-- | @Selector@ for @setScriptErrorNumber:@
setScriptErrorNumberSelector :: Selector
setScriptErrorNumberSelector = mkSelector "setScriptErrorNumber:"

-- | @Selector@ for @scriptErrorString@
scriptErrorStringSelector :: Selector
scriptErrorStringSelector = mkSelector "scriptErrorString"

-- | @Selector@ for @setScriptErrorString:@
setScriptErrorStringSelector :: Selector
setScriptErrorStringSelector = mkSelector "setScriptErrorString:"

-- | @Selector@ for @appleEvent@
appleEventSelector :: Selector
appleEventSelector = mkSelector "appleEvent"

