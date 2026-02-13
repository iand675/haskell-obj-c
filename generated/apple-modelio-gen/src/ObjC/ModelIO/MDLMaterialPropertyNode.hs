{-# LANGUAGE DataKinds #-}
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
  , evaluationFunctionSelector
  , initSelector
  , initWithInputs_outputs_evaluationFunctionSelector
  , inputsSelector
  , outputsSelector
  , setEvaluationFunctionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Id MDLMaterialPropertyNode)
init_ mdlMaterialPropertyNode =
  sendOwnedMessage mdlMaterialPropertyNode initSelector

-- | @- initWithInputs:outputs:evaluationFunction:@
initWithInputs_outputs_evaluationFunction :: (IsMDLMaterialPropertyNode mdlMaterialPropertyNode, IsNSArray inputs, IsNSArray outputs) => mdlMaterialPropertyNode -> inputs -> outputs -> Ptr () -> IO (Id MDLMaterialPropertyNode)
initWithInputs_outputs_evaluationFunction mdlMaterialPropertyNode inputs outputs function =
  sendOwnedMessage mdlMaterialPropertyNode initWithInputs_outputs_evaluationFunctionSelector (toNSArray inputs) (toNSArray outputs) function

-- | @- evaluationFunction@
evaluationFunction :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Ptr ())
evaluationFunction mdlMaterialPropertyNode =
  sendMessage mdlMaterialPropertyNode evaluationFunctionSelector

-- | @- setEvaluationFunction:@
setEvaluationFunction :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> Ptr () -> IO ()
setEvaluationFunction mdlMaterialPropertyNode value =
  sendMessage mdlMaterialPropertyNode setEvaluationFunctionSelector value

-- | @- inputs@
inputs :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Id NSArray)
inputs mdlMaterialPropertyNode =
  sendMessage mdlMaterialPropertyNode inputsSelector

-- | @- outputs@
outputs :: IsMDLMaterialPropertyNode mdlMaterialPropertyNode => mdlMaterialPropertyNode -> IO (Id NSArray)
outputs mdlMaterialPropertyNode =
  sendMessage mdlMaterialPropertyNode outputsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MDLMaterialPropertyNode)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInputs:outputs:evaluationFunction:@
initWithInputs_outputs_evaluationFunctionSelector :: Selector '[Id NSArray, Id NSArray, Ptr ()] (Id MDLMaterialPropertyNode)
initWithInputs_outputs_evaluationFunctionSelector = mkSelector "initWithInputs:outputs:evaluationFunction:"

-- | @Selector@ for @evaluationFunction@
evaluationFunctionSelector :: Selector '[] (Ptr ())
evaluationFunctionSelector = mkSelector "evaluationFunction"

-- | @Selector@ for @setEvaluationFunction:@
setEvaluationFunctionSelector :: Selector '[Ptr ()] ()
setEvaluationFunctionSelector = mkSelector "setEvaluationFunction:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSArray)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector '[] (Id NSArray)
outputsSelector = mkSelector "outputs"

