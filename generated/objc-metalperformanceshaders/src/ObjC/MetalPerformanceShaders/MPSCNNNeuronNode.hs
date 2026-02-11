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
  , nodeWithSource_descriptorSelector
  , initSelector
  , aSelector
  , bSelector
  , cSelector


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

-- | Create a neuron node of the appropriate type with a MPSNNNeuronDescriptor
--
-- ObjC selector: @+ nodeWithSource:descriptor:@
nodeWithSource_descriptor :: (IsMPSNNImageNode sourceNode, IsMPSNNNeuronDescriptor descriptor) => sourceNode -> descriptor -> IO (Id MPSCNNNeuronNode)
nodeWithSource_descriptor sourceNode descriptor =
  do
    cls' <- getRequiredClass "MPSCNNNeuronNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      withObjCPtr descriptor $ \raw_descriptor ->
        sendClassMsg cls' (mkSelector "nodeWithSource:descriptor:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO (Id MPSCNNNeuronNode)
init_ mpscnnNeuronNode  =
  sendMsg mpscnnNeuronNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | filter parameter a
--
-- ObjC selector: @- a@
a :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO CFloat
a mpscnnNeuronNode  =
  sendMsg mpscnnNeuronNode (mkSelector "a") retCFloat []

-- | filter parameter b
--
-- ObjC selector: @- b@
b :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO CFloat
b mpscnnNeuronNode  =
  sendMsg mpscnnNeuronNode (mkSelector "b") retCFloat []

-- | filter parameter c
--
-- ObjC selector: @- c@
c :: IsMPSCNNNeuronNode mpscnnNeuronNode => mpscnnNeuronNode -> IO CFloat
c mpscnnNeuronNode  =
  sendMsg mpscnnNeuronNode (mkSelector "c") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:descriptor:@
nodeWithSource_descriptorSelector :: Selector
nodeWithSource_descriptorSelector = mkSelector "nodeWithSource:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @a@
aSelector :: Selector
aSelector = mkSelector "a"

-- | @Selector@ for @b@
bSelector :: Selector
bSelector = mkSelector "b"

-- | @Selector@ for @c@
cSelector :: Selector
cSelector = mkSelector "c"

