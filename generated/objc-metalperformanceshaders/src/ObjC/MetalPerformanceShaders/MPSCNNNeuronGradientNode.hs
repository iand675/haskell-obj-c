{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronGradient
--
-- We use one generic neuron gradient node              instead of having dozens of subclasses.
--
-- Generated bindings for @MPSCNNNeuronGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronGradientNode
  ( MPSCNNNeuronGradientNode
  , IsMPSCNNNeuronGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_descriptor
  , initWithSourceGradient_sourceImage_gradientState_descriptor
  , init_
  , descriptor
  , nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector
  , initWithSourceGradient_sourceImage_gradientState_descriptorSelector
  , initSelector
  , descriptorSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | create a new neuron gradient node
--
-- See also -[MPSCNNNeuronNode gradientFilterNodeWithSources:]              for an easier way to do this
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:descriptor:@
nodeWithSourceGradient_sourceImage_gradientState_descriptor :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState, IsMPSNNNeuronDescriptor descriptor) => sourceGradient -> sourceImage -> gradientState -> descriptor -> IO (Id MPSCNNNeuronGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_descriptor sourceGradient sourceImage gradientState descriptor =
  do
    cls' <- getRequiredClass "MPSCNNNeuronGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          withObjCPtr descriptor $ \raw_descriptor ->
            sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:descriptor:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | create a new neuron gradient node
--
-- See also -[MPSCNNNeuronNode gradientFilterNodeWithSources:]              for an easier way to do this
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:descriptor:@
initWithSourceGradient_sourceImage_gradientState_descriptor :: (IsMPSCNNNeuronGradientNode mpscnnNeuronGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState, IsMPSNNNeuronDescriptor descriptor) => mpscnnNeuronGradientNode -> sourceGradient -> sourceImage -> gradientState -> descriptor -> IO (Id MPSCNNNeuronGradientNode)
initWithSourceGradient_sourceImage_gradientState_descriptor mpscnnNeuronGradientNode  sourceGradient sourceImage gradientState descriptor =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
      withObjCPtr descriptor $ \raw_descriptor ->
          sendMsg mpscnnNeuronGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:descriptor:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSCNNNeuronGradientNode mpscnnNeuronGradientNode => mpscnnNeuronGradientNode -> IO (Id MPSCNNNeuronGradientNode)
init_ mpscnnNeuronGradientNode  =
  sendMsg mpscnnNeuronGradientNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The neuron descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMPSCNNNeuronGradientNode mpscnnNeuronGradientNode => mpscnnNeuronGradientNode -> IO (Id MPSNNNeuronDescriptor)
descriptor mpscnnNeuronGradientNode  =
  sendMsg mpscnnNeuronGradientNode (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:descriptor:@
nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:descriptor:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:descriptor:@
initWithSourceGradient_sourceImage_gradientState_descriptorSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_descriptorSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

