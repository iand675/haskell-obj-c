{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | virtual base class for MPSCNNNeuron nodes
--
-- This is a virtual base class only. Please create a              subclass using +newNeuronNodeWithSouce:descriptor or              by making one of the subclasses directly. Better yet, skip              the node entirely and specify the neuron function directly in              your MPSCNNConvolutionDataSource.descriptor.neuronDescriptor.
--
-- MPSCNNNeuronNodes are provided as a representational convenience.              However, you are usually better off incorporating your neuron              into the MPSCNNConvolutionDataSource when possible. The MPSNNGraph              will attempt to optimize away the neuron pass by fusing it with a               preceeding convolution, but it might be prevented from doing so               if the neuron pass has a custom padding method or more than one               node reads from the convolution result. The graph -debugDescription              should reveal what happened.
--
-- Generated bindings for @MPSCNNNeuronNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronNode
  ( MPSCNNNeuronNode
  , IsMPSCNNNeuronNode(..)
  , nodeWithSource_descriptor
  , init_
  , a
  , b
  , c
  , aSelector
  , bSelector
  , cSelector
  , initSelector
  , nodeWithSource_descriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a neuron node of the appropriate type with a MPSNNNeuronDescriptor
--
-- ObjC selector: @+ nodeWithSource:descriptor:@
nodeWithSource_descriptor :: (IsMPSNNImageNode sourceNode, IsMPSNNNeuronDescriptor descriptor) => sourceNode -> descriptor -> IO (Id MPSCNNNeuronNode)
nodeWithSource_descriptor sourceNode descriptor =
  do
    cls' <- getRequiredClass "MPSCNNNeuronNode"
    sendClassMessage cls' nodeWithSource_descriptorSelector (toMPSNNImageNode sourceNode) (toMPSNNNeuronDescriptor descriptor)

-- | @- init@
init_ :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO (Id MPSCNNNeuronNode)
init_ mpscnnNeuronNode =
  sendOwnedMessage mpscnnNeuronNode initSelector

-- | filter parameter a
--
-- ObjC selector: @- a@
a :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO CFloat
a mpscnnNeuronNode =
  sendMessage mpscnnNeuronNode aSelector

-- | filter parameter b
--
-- ObjC selector: @- b@
b :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO CFloat
b mpscnnNeuronNode =
  sendMessage mpscnnNeuronNode bSelector

-- | filter parameter c
--
-- ObjC selector: @- c@
c :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO CFloat
c mpscnnNeuronNode =
  sendMessage mpscnnNeuronNode cSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:descriptor:@
nodeWithSource_descriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNNeuronDescriptor] (Id MPSCNNNeuronNode)
nodeWithSource_descriptorSelector = mkSelector "nodeWithSource:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSCNNNeuronNode)
initSelector = mkSelector "init"

-- | @Selector@ for @a@
aSelector :: Selector '[] CFloat
aSelector = mkSelector "a"

-- | @Selector@ for @b@
bSelector :: Selector '[] CFloat
bSelector = mkSelector "b"

-- | @Selector@ for @c@
cSelector :: Selector '[] CFloat
cSelector = mkSelector "c"

