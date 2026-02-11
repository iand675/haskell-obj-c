{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLMaterialPropertyNode@.
module ObjC.ModelIO.MDLMaterialPropertyNode
  ( MDLMaterialPropertyNode
  , IsMDLMaterialPropertyNode(..)
  , init_
  , initWithInputs_outputs_evaluationFunction
  , evaluationFunction
  , setEvaluationFunction
  , inputs
  , outputs
  , initSelector
  , initWithInputs_outputs_evaluationFunctionSelector
  , evaluationFunctionSelector
  , setEvaluationFunctionSelector
  , inputsSelector
  , outputsSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Id MDLMaterialPropertyNode)
init_ mdlMaterialPropertyNode  =
  sendMsg mdlMaterialPropertyNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithInputs:outputs:evaluationFunction:@
initWithInputs_outputs_evaluationFunction :: (IsMDLMaterialPropertyNode mdlMaterialPropertyNode, IsNSArray inputs, IsNSArray outputs) => mdlMaterialPropertyNode -> inputs -> outputs -> Ptr () -> IO (Id MDLMaterialPropertyNode)
initWithInputs_outputs_evaluationFunction mdlMaterialPropertyNode  inputs outputs function =
withObjCPtr inputs $ \raw_inputs ->
  withObjCPtr outputs $ \raw_outputs ->
      sendMsg mdlMaterialPropertyNode (mkSelector "initWithInputs:outputs:evaluationFunction:") (retPtr retVoid) [argPtr (castPtr raw_inputs :: Ptr ()), argPtr (castPtr raw_outputs :: Ptr ()), argPtr (castPtr function :: Ptr ())] >>= ownedObject . castPtr

-- | @- evaluationFunction@
evaluationFunction :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Ptr ())
evaluationFunction mdlMaterialPropertyNode  =
  fmap castPtr $ sendMsg mdlMaterialPropertyNode (mkSelector "evaluationFunction") (retPtr retVoid) []

-- | @- setEvaluationFunction:@
setEvaluationFunction :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> Ptr () -> IO ()
setEvaluationFunction mdlMaterialPropertyNode  value =
  sendMsg mdlMaterialPropertyNode (mkSelector "setEvaluationFunction:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- inputs@
inputs :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Id NSArray)
inputs mdlMaterialPropertyNode  =
  sendMsg mdlMaterialPropertyNode (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- outputs@
outputs :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Id NSArray)
outputs mdlMaterialPropertyNode  =
  sendMsg mdlMaterialPropertyNode (mkSelector "outputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInputs:outputs:evaluationFunction:@
initWithInputs_outputs_evaluationFunctionSelector :: Selector
initWithInputs_outputs_evaluationFunctionSelector = mkSelector "initWithInputs:outputs:evaluationFunction:"

-- | @Selector@ for @evaluationFunction@
evaluationFunctionSelector :: Selector
evaluationFunctionSelector = mkSelector "evaluationFunction"

-- | @Selector@ for @setEvaluationFunction:@
setEvaluationFunctionSelector :: Selector
setEvaluationFunctionSelector = mkSelector "setEvaluationFunction:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector
outputsSelector = mkSelector "outputs"

