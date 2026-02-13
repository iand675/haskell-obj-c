{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronLinear kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * x + b
--
-- Generated bindings for @MPSCNNNeuronLinearNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronLinearNode
  ( MPSCNNNeuronLinearNode
  , IsMPSCNNNeuronLinearNode(..)
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
nodeWithSource_a_b :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLinearNode)
nodeWithSource_a_b sourceNode a b =
  do
    cls' <- getRequiredClass "MPSCNNNeuronLinearNode"
    sendClassMessage cls' nodeWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Init a node representing a MPSCNNNeuronLinear kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronLinear kernel.
--
-- ObjC selector: @- initWithSource:a:b:@
initWithSource_a_b :: (IsMPSCNNNeuronLinearNode mpscnnNeuronLinearNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronLinearNode -> sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLinearNode)
initWithSource_a_b mpscnnNeuronLinearNode sourceNode a b =
  sendOwnedMessage mpscnnNeuronLinearNode initWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronLinearNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronLinearNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronLinearNode mpscnnNeuronLinearNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronLinearNode -> sourceNode -> IO (Id MPSCNNNeuronLinearNode)
initWithSource mpscnnNeuronLinearNode sourceNode =
  sendOwnedMessage mpscnnNeuronLinearNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:@
nodeWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronLinearNode)
nodeWithSource_a_bSelector = mkSelector "nodeWithSource:a:b:"

-- | @Selector@ for @initWithSource:a:b:@
initWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronLinearNode)
initWithSource_a_bSelector = mkSelector "initWithSource:a:b:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronLinearNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronLinearNode)
initWithSourceSelector = mkSelector "initWithSource:"

