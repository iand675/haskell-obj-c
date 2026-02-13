{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronTanH kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * tanh(b * x)
--
-- Generated bindings for @MPSCNNNeuronTanHNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronTanHNode
  ( MPSCNNNeuronTanHNode
  , IsMPSCNNNeuronTanHNode(..)
  , nodeWithSource_a_b
  , initWithSource_a_b
  , nodeWithSource
  , initWithSource
  , initWithSourceSelector
  , initWithSource_a_bSelector
  , nodeWithSourceSelector
  , nodeWithSource_a_bSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:a:b:@
nodeWithSource_a_b :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronTanHNode)
nodeWithSource_a_b sourceNode a b =
  do
    cls' <- getRequiredClass "MPSCNNNeuronTanHNode"
    sendClassMessage cls' nodeWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Init a node representing a MPSCNNNeuronTanH kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * tanh(b * x)
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronTanH kernel.
--
-- ObjC selector: @- initWithSource:a:b:@
initWithSource_a_b :: (IsMPSCNNNeuronTanHNode mpscnnNeuronTanHNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronTanHNode -> sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronTanHNode)
initWithSource_a_b mpscnnNeuronTanHNode sourceNode a b =
  sendOwnedMessage mpscnnNeuronTanHNode initWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronTanHNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronTanHNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronTanHNode mpscnnNeuronTanHNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronTanHNode -> sourceNode -> IO (Id MPSCNNNeuronTanHNode)
initWithSource mpscnnNeuronTanHNode sourceNode =
  sendOwnedMessage mpscnnNeuronTanHNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:@
nodeWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronTanHNode)
nodeWithSource_a_bSelector = mkSelector "nodeWithSource:a:b:"

-- | @Selector@ for @initWithSource:a:b:@
initWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronTanHNode)
initWithSource_a_bSelector = mkSelector "initWithSource:a:b:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronTanHNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronTanHNode)
initWithSourceSelector = mkSelector "initWithSource:"

