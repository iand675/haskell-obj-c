{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMWorkflow@.
module ObjC.Automator.AMWorkflow
  ( AMWorkflow
  , IsAMWorkflow(..)
  , runWorkflowAtURL_withInput_error
  , init_
  , initWithContentsOfURL_error
  , writeToURL_error
  , setValue_forVariableWithName
  , valueForVariableWithName
  , addAction
  , removeAction
  , insertAction_atIndex
  , moveActionAtIndex_toIndex
  , fileURL
  , actions
  , input
  , setInput
  , output
  , actionsSelector
  , addActionSelector
  , fileURLSelector
  , initSelector
  , initWithContentsOfURL_errorSelector
  , inputSelector
  , insertAction_atIndexSelector
  , moveActionAtIndex_toIndexSelector
  , outputSelector
  , removeActionSelector
  , runWorkflowAtURL_withInput_errorSelector
  , setInputSelector
  , setValue_forVariableWithNameSelector
  , valueForVariableWithNameSelector
  , writeToURL_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Automator.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ runWorkflowAtURL:withInput:error:@
runWorkflowAtURL_withInput_error :: (IsNSURL fileURL, IsNSError error_) => fileURL -> RawId -> error_ -> IO RawId
runWorkflowAtURL_withInput_error fileURL input error_ =
  do
    cls' <- getRequiredClass "AMWorkflow"
    sendClassMessage cls' runWorkflowAtURL_withInput_errorSelector (toNSURL fileURL) input (toNSError error_)

-- | @- init@
init_ :: IsAMWorkflow amWorkflow => amWorkflow -> IO (Id AMWorkflow)
init_ amWorkflow =
  sendOwnedMessage amWorkflow initSelector

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsAMWorkflow amWorkflow, IsNSURL fileURL, IsNSError outError) => amWorkflow -> fileURL -> outError -> IO (Id AMWorkflow)
initWithContentsOfURL_error amWorkflow fileURL outError =
  sendOwnedMessage amWorkflow initWithContentsOfURL_errorSelector (toNSURL fileURL) (toNSError outError)

-- | @- writeToURL:error:@
writeToURL_error :: (IsAMWorkflow amWorkflow, IsNSURL fileURL, IsNSError outError) => amWorkflow -> fileURL -> outError -> IO Bool
writeToURL_error amWorkflow fileURL outError =
  sendMessage amWorkflow writeToURL_errorSelector (toNSURL fileURL) (toNSError outError)

-- | @- setValue:forVariableWithName:@
setValue_forVariableWithName :: (IsAMWorkflow amWorkflow, IsNSString variableName) => amWorkflow -> RawId -> variableName -> IO Bool
setValue_forVariableWithName amWorkflow value variableName =
  sendMessage amWorkflow setValue_forVariableWithNameSelector value (toNSString variableName)

-- | @- valueForVariableWithName:@
valueForVariableWithName :: (IsAMWorkflow amWorkflow, IsNSString variableName) => amWorkflow -> variableName -> IO RawId
valueForVariableWithName amWorkflow variableName =
  sendMessage amWorkflow valueForVariableWithNameSelector (toNSString variableName)

-- | @- addAction:@
addAction :: (IsAMWorkflow amWorkflow, IsAMAction action) => amWorkflow -> action -> IO ()
addAction amWorkflow action =
  sendMessage amWorkflow addActionSelector (toAMAction action)

-- | @- removeAction:@
removeAction :: (IsAMWorkflow amWorkflow, IsAMAction action) => amWorkflow -> action -> IO ()
removeAction amWorkflow action =
  sendMessage amWorkflow removeActionSelector (toAMAction action)

-- | @- insertAction:atIndex:@
insertAction_atIndex :: (IsAMWorkflow amWorkflow, IsAMAction action) => amWorkflow -> action -> CULong -> IO ()
insertAction_atIndex amWorkflow action index =
  sendMessage amWorkflow insertAction_atIndexSelector (toAMAction action) index

-- | @- moveActionAtIndex:toIndex:@
moveActionAtIndex_toIndex :: IsAMWorkflow amWorkflow => amWorkflow -> CULong -> CULong -> IO ()
moveActionAtIndex_toIndex amWorkflow startIndex endIndex =
  sendMessage amWorkflow moveActionAtIndex_toIndexSelector startIndex endIndex

-- | @- fileURL@
fileURL :: IsAMWorkflow amWorkflow => amWorkflow -> IO (Id NSURL)
fileURL amWorkflow =
  sendMessage amWorkflow fileURLSelector

-- | @- actions@
actions :: IsAMWorkflow amWorkflow => amWorkflow -> IO (Id NSArray)
actions amWorkflow =
  sendMessage amWorkflow actionsSelector

-- | @- input@
input :: IsAMWorkflow amWorkflow => amWorkflow -> IO RawId
input amWorkflow =
  sendMessage amWorkflow inputSelector

-- | @- setInput:@
setInput :: IsAMWorkflow amWorkflow => amWorkflow -> RawId -> IO ()
setInput amWorkflow value =
  sendMessage amWorkflow setInputSelector value

-- | @- output@
output :: IsAMWorkflow amWorkflow => amWorkflow -> IO RawId
output amWorkflow =
  sendMessage amWorkflow outputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWorkflowAtURL:withInput:error:@
runWorkflowAtURL_withInput_errorSelector :: Selector '[Id NSURL, RawId, Id NSError] RawId
runWorkflowAtURL_withInput_errorSelector = mkSelector "runWorkflowAtURL:withInput:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AMWorkflow)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id AMWorkflow)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @setValue:forVariableWithName:@
setValue_forVariableWithNameSelector :: Selector '[RawId, Id NSString] Bool
setValue_forVariableWithNameSelector = mkSelector "setValue:forVariableWithName:"

-- | @Selector@ for @valueForVariableWithName:@
valueForVariableWithNameSelector :: Selector '[Id NSString] RawId
valueForVariableWithNameSelector = mkSelector "valueForVariableWithName:"

-- | @Selector@ for @addAction:@
addActionSelector :: Selector '[Id AMAction] ()
addActionSelector = mkSelector "addAction:"

-- | @Selector@ for @removeAction:@
removeActionSelector :: Selector '[Id AMAction] ()
removeActionSelector = mkSelector "removeAction:"

-- | @Selector@ for @insertAction:atIndex:@
insertAction_atIndexSelector :: Selector '[Id AMAction, CULong] ()
insertAction_atIndexSelector = mkSelector "insertAction:atIndex:"

-- | @Selector@ for @moveActionAtIndex:toIndex:@
moveActionAtIndex_toIndexSelector :: Selector '[CULong, CULong] ()
moveActionAtIndex_toIndexSelector = mkSelector "moveActionAtIndex:toIndex:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @actions@
actionsSelector :: Selector '[] (Id NSArray)
actionsSelector = mkSelector "actions"

-- | @Selector@ for @input@
inputSelector :: Selector '[] RawId
inputSelector = mkSelector "input"

-- | @Selector@ for @setInput:@
setInputSelector :: Selector '[RawId] ()
setInputSelector = mkSelector "setInput:"

-- | @Selector@ for @output@
outputSelector :: Selector '[] RawId
outputSelector = mkSelector "output"

