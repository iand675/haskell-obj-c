{-# LANGUAGE DataKinds #-}
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
  , descriptorSelector
  , initSelector
  , initWithSourceGradient_sourceImage_gradientState_descriptorSelector
  , nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) (toMPSNNNeuronDescriptor descriptor)

-- | create a new neuron gradient node
--
-- See also -[MPSCNNNeuronNode gradientFilterNodeWithSources:]              for an easier way to do this
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:descriptor:@
initWithSourceGradient_sourceImage_gradientState_descriptor :: (IsMPSCNNNeuronGradientNode mpscnnNeuronGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState, IsMPSNNNeuronDescriptor descriptor) => mpscnnNeuronGradientNode -> sourceGradient -> sourceImage -> gradientState -> descriptor -> IO (Id MPSCNNNeuronGradientNode)
initWithSourceGradient_sourceImage_gradientState_descriptor mpscnnNeuronGradientNode sourceGradient sourceImage gradientState descriptor =
  sendOwnedMessage mpscnnNeuronGradientNode initWithSourceGradient_sourceImage_gradientState_descriptorSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) (toMPSNNNeuronDescriptor descriptor)

-- | @- init@
init_ :: IsMPSCNNNeuronGradientNode mpscnnNeuronGradientNode => mpscnnNeuronGradientNode -> IO (Id MPSCNNNeuronGradientNode)
init_ mpscnnNeuronGradientNode =
  sendOwnedMessage mpscnnNeuronGradientNode initSelector

-- | The neuron descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMPSCNNNeuronGradientNode mpscnnNeuronGradientNode => mpscnnNeuronGradientNode -> IO (Id MPSNNNeuronDescriptor)
descriptor mpscnnNeuronGradientNode =
  sendMessage mpscnnNeuronGradientNode descriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:descriptor:@
nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, Id MPSNNNeuronDescriptor] (Id MPSCNNNeuronGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_descriptorSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:descriptor:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:descriptor:@
initWithSourceGradient_sourceImage_gradientState_descriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, Id MPSNNNeuronDescriptor] (Id MPSCNNNeuronGradientNode)
initWithSourceGradient_sourceImage_gradientState_descriptorSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSCNNNeuronGradientNode)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MPSNNNeuronDescriptor)
descriptorSelector = mkSelector "descriptor"

