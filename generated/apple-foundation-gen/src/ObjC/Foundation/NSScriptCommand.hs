{-# LANGUAGE DataKinds #-}
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
  , scriptErrorOffendingObjectDescriptor
  , setScriptErrorOffendingObjectDescriptor
  , scriptErrorExpectedTypeDescriptor
  , setScriptErrorExpectedTypeDescriptor
  , scriptErrorString
  , setScriptErrorString
  , appleEvent
  , appleEventSelector
  , argumentsSelector
  , commandDescriptionSelector
  , currentCommandSelector
  , directParameterSelector
  , evaluatedArgumentsSelector
  , evaluatedReceiversSelector
  , executeCommandSelector
  , initWithCoderSelector
  , initWithCommandDescriptionSelector
  , performDefaultImplementationSelector
  , receiversSpecifierSelector
  , resumeExecutionWithResultSelector
  , scriptErrorExpectedTypeDescriptorSelector
  , scriptErrorNumberSelector
  , scriptErrorOffendingObjectDescriptorSelector
  , scriptErrorStringSelector
  , setArgumentsSelector
  , setDirectParameterSelector
  , setReceiversSpecifierSelector
  , setScriptErrorExpectedTypeDescriptorSelector
  , setScriptErrorNumberSelector
  , setScriptErrorOffendingObjectDescriptorSelector
  , setScriptErrorStringSelector
  , suspendExecutionSelector
  , wellFormedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCommandDescription:@
initWithCommandDescription :: (IsNSScriptCommand nsScriptCommand, IsNSScriptCommandDescription commandDef) => nsScriptCommand -> commandDef -> IO (Id NSScriptCommand)
initWithCommandDescription nsScriptCommand commandDef =
  sendOwnedMessage nsScriptCommand initWithCommandDescriptionSelector (toNSScriptCommandDescription commandDef)

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptCommand nsScriptCommand, IsNSCoder inCoder) => nsScriptCommand -> inCoder -> IO (Id NSScriptCommand)
initWithCoder nsScriptCommand inCoder =
  sendOwnedMessage nsScriptCommand initWithCoderSelector (toNSCoder inCoder)

-- | @- performDefaultImplementation@
performDefaultImplementation :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
performDefaultImplementation nsScriptCommand =
  sendMessage nsScriptCommand performDefaultImplementationSelector

-- | @- executeCommand@
executeCommand :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
executeCommand nsScriptCommand =
  sendMessage nsScriptCommand executeCommandSelector

-- | @+ currentCommand@
currentCommand :: IO (Id NSScriptCommand)
currentCommand  =
  do
    cls' <- getRequiredClass "NSScriptCommand"
    sendClassMessage cls' currentCommandSelector

-- | @- suspendExecution@
suspendExecution :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO ()
suspendExecution nsScriptCommand =
  sendMessage nsScriptCommand suspendExecutionSelector

-- | @- resumeExecutionWithResult:@
resumeExecutionWithResult :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> RawId -> IO ()
resumeExecutionWithResult nsScriptCommand result =
  sendMessage nsScriptCommand resumeExecutionWithResultSelector result

-- | @- commandDescription@
commandDescription :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSScriptCommandDescription)
commandDescription nsScriptCommand =
  sendMessage nsScriptCommand commandDescriptionSelector

-- | @- directParameter@
directParameter :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
directParameter nsScriptCommand =
  sendMessage nsScriptCommand directParameterSelector

-- | @- setDirectParameter:@
setDirectParameter :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> RawId -> IO ()
setDirectParameter nsScriptCommand value =
  sendMessage nsScriptCommand setDirectParameterSelector value

-- | @- receiversSpecifier@
receiversSpecifier :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSScriptObjectSpecifier)
receiversSpecifier nsScriptCommand =
  sendMessage nsScriptCommand receiversSpecifierSelector

-- | @- setReceiversSpecifier:@
setReceiversSpecifier :: (IsNSScriptCommand nsScriptCommand, IsNSScriptObjectSpecifier value) => nsScriptCommand -> value -> IO ()
setReceiversSpecifier nsScriptCommand value =
  sendMessage nsScriptCommand setReceiversSpecifierSelector (toNSScriptObjectSpecifier value)

-- | @- evaluatedReceivers@
evaluatedReceivers :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO RawId
evaluatedReceivers nsScriptCommand =
  sendMessage nsScriptCommand evaluatedReceiversSelector

-- | @- arguments@
arguments :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSDictionary)
arguments nsScriptCommand =
  sendMessage nsScriptCommand argumentsSelector

-- | @- setArguments:@
setArguments :: (IsNSScriptCommand nsScriptCommand, IsNSDictionary value) => nsScriptCommand -> value -> IO ()
setArguments nsScriptCommand value =
  sendMessage nsScriptCommand setArgumentsSelector (toNSDictionary value)

-- | @- evaluatedArguments@
evaluatedArguments :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSDictionary)
evaluatedArguments nsScriptCommand =
  sendMessage nsScriptCommand evaluatedArgumentsSelector

-- | @- wellFormed@
wellFormed :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO Bool
wellFormed nsScriptCommand =
  sendMessage nsScriptCommand wellFormedSelector

-- | @- scriptErrorNumber@
scriptErrorNumber :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO CLong
scriptErrorNumber nsScriptCommand =
  sendMessage nsScriptCommand scriptErrorNumberSelector

-- | @- setScriptErrorNumber:@
setScriptErrorNumber :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> CLong -> IO ()
setScriptErrorNumber nsScriptCommand value =
  sendMessage nsScriptCommand setScriptErrorNumberSelector value

-- | @- scriptErrorOffendingObjectDescriptor@
scriptErrorOffendingObjectDescriptor :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSAppleEventDescriptor)
scriptErrorOffendingObjectDescriptor nsScriptCommand =
  sendMessage nsScriptCommand scriptErrorOffendingObjectDescriptorSelector

-- | @- setScriptErrorOffendingObjectDescriptor:@
setScriptErrorOffendingObjectDescriptor :: (IsNSScriptCommand nsScriptCommand, IsNSAppleEventDescriptor value) => nsScriptCommand -> value -> IO ()
setScriptErrorOffendingObjectDescriptor nsScriptCommand value =
  sendMessage nsScriptCommand setScriptErrorOffendingObjectDescriptorSelector (toNSAppleEventDescriptor value)

-- | @- scriptErrorExpectedTypeDescriptor@
scriptErrorExpectedTypeDescriptor :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSAppleEventDescriptor)
scriptErrorExpectedTypeDescriptor nsScriptCommand =
  sendMessage nsScriptCommand scriptErrorExpectedTypeDescriptorSelector

-- | @- setScriptErrorExpectedTypeDescriptor:@
setScriptErrorExpectedTypeDescriptor :: (IsNSScriptCommand nsScriptCommand, IsNSAppleEventDescriptor value) => nsScriptCommand -> value -> IO ()
setScriptErrorExpectedTypeDescriptor nsScriptCommand value =
  sendMessage nsScriptCommand setScriptErrorExpectedTypeDescriptorSelector (toNSAppleEventDescriptor value)

-- | @- scriptErrorString@
scriptErrorString :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSString)
scriptErrorString nsScriptCommand =
  sendMessage nsScriptCommand scriptErrorStringSelector

-- | @- setScriptErrorString:@
setScriptErrorString :: (IsNSScriptCommand nsScriptCommand, IsNSString value) => nsScriptCommand -> value -> IO ()
setScriptErrorString nsScriptCommand value =
  sendMessage nsScriptCommand setScriptErrorStringSelector (toNSString value)

-- | @- appleEvent@
appleEvent :: IsNSScriptCommand nsScriptCommand => nsScriptCommand -> IO (Id NSAppleEventDescriptor)
appleEvent nsScriptCommand =
  sendMessage nsScriptCommand appleEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCommandDescription:@
initWithCommandDescriptionSelector :: Selector '[Id NSScriptCommandDescription] (Id NSScriptCommand)
initWithCommandDescriptionSelector = mkSelector "initWithCommandDescription:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScriptCommand)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @performDefaultImplementation@
performDefaultImplementationSelector :: Selector '[] RawId
performDefaultImplementationSelector = mkSelector "performDefaultImplementation"

-- | @Selector@ for @executeCommand@
executeCommandSelector :: Selector '[] RawId
executeCommandSelector = mkSelector "executeCommand"

-- | @Selector@ for @currentCommand@
currentCommandSelector :: Selector '[] (Id NSScriptCommand)
currentCommandSelector = mkSelector "currentCommand"

-- | @Selector@ for @suspendExecution@
suspendExecutionSelector :: Selector '[] ()
suspendExecutionSelector = mkSelector "suspendExecution"

-- | @Selector@ for @resumeExecutionWithResult:@
resumeExecutionWithResultSelector :: Selector '[RawId] ()
resumeExecutionWithResultSelector = mkSelector "resumeExecutionWithResult:"

-- | @Selector@ for @commandDescription@
commandDescriptionSelector :: Selector '[] (Id NSScriptCommandDescription)
commandDescriptionSelector = mkSelector "commandDescription"

-- | @Selector@ for @directParameter@
directParameterSelector :: Selector '[] RawId
directParameterSelector = mkSelector "directParameter"

-- | @Selector@ for @setDirectParameter:@
setDirectParameterSelector :: Selector '[RawId] ()
setDirectParameterSelector = mkSelector "setDirectParameter:"

-- | @Selector@ for @receiversSpecifier@
receiversSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
receiversSpecifierSelector = mkSelector "receiversSpecifier"

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @evaluatedReceivers@
evaluatedReceiversSelector :: Selector '[] RawId
evaluatedReceiversSelector = mkSelector "evaluatedReceivers"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSDictionary)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector '[Id NSDictionary] ()
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @evaluatedArguments@
evaluatedArgumentsSelector :: Selector '[] (Id NSDictionary)
evaluatedArgumentsSelector = mkSelector "evaluatedArguments"

-- | @Selector@ for @wellFormed@
wellFormedSelector :: Selector '[] Bool
wellFormedSelector = mkSelector "wellFormed"

-- | @Selector@ for @scriptErrorNumber@
scriptErrorNumberSelector :: Selector '[] CLong
scriptErrorNumberSelector = mkSelector "scriptErrorNumber"

-- | @Selector@ for @setScriptErrorNumber:@
setScriptErrorNumberSelector :: Selector '[CLong] ()
setScriptErrorNumberSelector = mkSelector "setScriptErrorNumber:"

-- | @Selector@ for @scriptErrorOffendingObjectDescriptor@
scriptErrorOffendingObjectDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
scriptErrorOffendingObjectDescriptorSelector = mkSelector "scriptErrorOffendingObjectDescriptor"

-- | @Selector@ for @setScriptErrorOffendingObjectDescriptor:@
setScriptErrorOffendingObjectDescriptorSelector :: Selector '[Id NSAppleEventDescriptor] ()
setScriptErrorOffendingObjectDescriptorSelector = mkSelector "setScriptErrorOffendingObjectDescriptor:"

-- | @Selector@ for @scriptErrorExpectedTypeDescriptor@
scriptErrorExpectedTypeDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
scriptErrorExpectedTypeDescriptorSelector = mkSelector "scriptErrorExpectedTypeDescriptor"

-- | @Selector@ for @setScriptErrorExpectedTypeDescriptor:@
setScriptErrorExpectedTypeDescriptorSelector :: Selector '[Id NSAppleEventDescriptor] ()
setScriptErrorExpectedTypeDescriptorSelector = mkSelector "setScriptErrorExpectedTypeDescriptor:"

-- | @Selector@ for @scriptErrorString@
scriptErrorStringSelector :: Selector '[] (Id NSString)
scriptErrorStringSelector = mkSelector "scriptErrorString"

-- | @Selector@ for @setScriptErrorString:@
setScriptErrorStringSelector :: Selector '[Id NSString] ()
setScriptErrorStringSelector = mkSelector "setScriptErrorString:"

-- | @Selector@ for @appleEvent@
appleEventSelector :: Selector '[] (Id NSAppleEventDescriptor)
appleEventSelector = mkSelector "appleEvent"

