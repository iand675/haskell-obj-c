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
  , runWorkflowAtURL_withInput_errorSelector
  , initSelector
  , initWithContentsOfURL_errorSelector
  , writeToURL_errorSelector
  , setValue_forVariableWithNameSelector
  , valueForVariableWithNameSelector
  , addActionSelector
  , removeActionSelector
  , insertAction_atIndexSelector
  , moveActionAtIndex_toIndexSelector
  , fileURLSelector
  , actionsSelector
  , inputSelector
  , setInputSelector
  , outputSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ runWorkflowAtURL:withInput:error:@
runWorkflowAtURL_withInput_error :: (IsNSURL fileURL, IsNSError error_) => fileURL -> RawId -> error_ -> IO RawId
runWorkflowAtURL_withInput_error fileURL input error_ =
  do
    cls' <- getRequiredClass "AMWorkflow"
    withObjCPtr fileURL $ \raw_fileURL ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "runWorkflowAtURL:withInput:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- init@
init_ :: IsAMWorkflow amWorkflow => amWorkflow -> IO (Id AMWorkflow)
init_ amWorkflow  =
  sendMsg amWorkflow (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsAMWorkflow amWorkflow, IsNSURL fileURL, IsNSError outError) => amWorkflow -> fileURL -> outError -> IO (Id AMWorkflow)
initWithContentsOfURL_error amWorkflow  fileURL outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg amWorkflow (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- writeToURL:error:@
writeToURL_error :: (IsAMWorkflow amWorkflow, IsNSURL fileURL, IsNSError outError) => amWorkflow -> fileURL -> outError -> IO Bool
writeToURL_error amWorkflow  fileURL outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg amWorkflow (mkSelector "writeToURL:error:") retCULong [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- setValue:forVariableWithName:@
setValue_forVariableWithName :: (IsAMWorkflow amWorkflow, IsNSString variableName) => amWorkflow -> RawId -> variableName -> IO Bool
setValue_forVariableWithName amWorkflow  value variableName =
withObjCPtr variableName $ \raw_variableName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg amWorkflow (mkSelector "setValue:forVariableWithName:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_variableName :: Ptr ())]

-- | @- valueForVariableWithName:@
valueForVariableWithName :: (IsAMWorkflow amWorkflow, IsNSString variableName) => amWorkflow -> variableName -> IO RawId
valueForVariableWithName amWorkflow  variableName =
withObjCPtr variableName $ \raw_variableName ->
    fmap (RawId . castPtr) $ sendMsg amWorkflow (mkSelector "valueForVariableWithName:") (retPtr retVoid) [argPtr (castPtr raw_variableName :: Ptr ())]

-- | @- addAction:@
addAction :: (IsAMWorkflow amWorkflow, IsAMAction action) => amWorkflow -> action -> IO ()
addAction amWorkflow  action =
withObjCPtr action $ \raw_action ->
    sendMsg amWorkflow (mkSelector "addAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- | @- removeAction:@
removeAction :: (IsAMWorkflow amWorkflow, IsAMAction action) => amWorkflow -> action -> IO ()
removeAction amWorkflow  action =
withObjCPtr action $ \raw_action ->
    sendMsg amWorkflow (mkSelector "removeAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- | @- insertAction:atIndex:@
insertAction_atIndex :: (IsAMWorkflow amWorkflow, IsAMAction action) => amWorkflow -> action -> CULong -> IO ()
insertAction_atIndex amWorkflow  action index =
withObjCPtr action $ \raw_action ->
    sendMsg amWorkflow (mkSelector "insertAction:atIndex:") retVoid [argPtr (castPtr raw_action :: Ptr ()), argCULong (fromIntegral index)]

-- | @- moveActionAtIndex:toIndex:@
moveActionAtIndex_toIndex :: IsAMWorkflow amWorkflow => amWorkflow -> CULong -> CULong -> IO ()
moveActionAtIndex_toIndex amWorkflow  startIndex endIndex =
  sendMsg amWorkflow (mkSelector "moveActionAtIndex:toIndex:") retVoid [argCULong (fromIntegral startIndex), argCULong (fromIntegral endIndex)]

-- | @- fileURL@
fileURL :: IsAMWorkflow amWorkflow => amWorkflow -> IO (Id NSURL)
fileURL amWorkflow  =
  sendMsg amWorkflow (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- actions@
actions :: IsAMWorkflow amWorkflow => amWorkflow -> IO (Id NSArray)
actions amWorkflow  =
  sendMsg amWorkflow (mkSelector "actions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- input@
input :: IsAMWorkflow amWorkflow => amWorkflow -> IO RawId
input amWorkflow  =
  fmap (RawId . castPtr) $ sendMsg amWorkflow (mkSelector "input") (retPtr retVoid) []

-- | @- setInput:@
setInput :: IsAMWorkflow amWorkflow => amWorkflow -> RawId -> IO ()
setInput amWorkflow  value =
  sendMsg amWorkflow (mkSelector "setInput:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- output@
output :: IsAMWorkflow amWorkflow => amWorkflow -> IO RawId
output amWorkflow  =
  fmap (RawId . castPtr) $ sendMsg amWorkflow (mkSelector "output") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWorkflowAtURL:withInput:error:@
runWorkflowAtURL_withInput_errorSelector :: Selector
runWorkflowAtURL_withInput_errorSelector = mkSelector "runWorkflowAtURL:withInput:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @setValue:forVariableWithName:@
setValue_forVariableWithNameSelector :: Selector
setValue_forVariableWithNameSelector = mkSelector "setValue:forVariableWithName:"

-- | @Selector@ for @valueForVariableWithName:@
valueForVariableWithNameSelector :: Selector
valueForVariableWithNameSelector = mkSelector "valueForVariableWithName:"

-- | @Selector@ for @addAction:@
addActionSelector :: Selector
addActionSelector = mkSelector "addAction:"

-- | @Selector@ for @removeAction:@
removeActionSelector :: Selector
removeActionSelector = mkSelector "removeAction:"

-- | @Selector@ for @insertAction:atIndex:@
insertAction_atIndexSelector :: Selector
insertAction_atIndexSelector = mkSelector "insertAction:atIndex:"

-- | @Selector@ for @moveActionAtIndex:toIndex:@
moveActionAtIndex_toIndexSelector :: Selector
moveActionAtIndex_toIndexSelector = mkSelector "moveActionAtIndex:toIndex:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @actions@
actionsSelector :: Selector
actionsSelector = mkSelector "actions"

-- | @Selector@ for @input@
inputSelector :: Selector
inputSelector = mkSelector "input"

-- | @Selector@ for @setInput:@
setInputSelector :: Selector
setInputSelector = mkSelector "setInput:"

-- | @Selector@ for @output@
outputSelector :: Selector
outputSelector = mkSelector "output"

