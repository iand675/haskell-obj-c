{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronHardSigmoid kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = clamp((a * x) + b, 0, 1)
--
-- Generated bindings for @MPSCNNNeuronHardSigmoidNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronHardSigmoidNode
  ( MPSCNNNeuronHardSigmoidNode
  , IsMPSCNNNeuronHardSigmoidNode(..)
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
nodeWithSource_a_b :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronHardSigmoidNode)
nodeWithSource_a_b sourceNode a b =
  do
    cls' <- getRequiredClass "MPSCNNNeuronHardSigmoidNode"
    sendClassMessage cls' nodeWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Init a node representing a MPSCNNNeuronHardSigmoid kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronHardSigmoid kernel.
--
-- ObjC selector: @- initWithSource:a:b:@
initWithSource_a_b :: (IsMPSCNNNeuronHardSigmoidNode mpscnnNeuronHardSigmoidNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronHardSigmoidNode -> sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronHardSigmoidNode)
initWithSource_a_b mpscnnNeuronHardSigmoidNode sourceNode a b =
  sendOwnedMessage mpscnnNeuronHardSigmoidNode initWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronHardSigmoidNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronHardSigmoidNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronHardSigmoidNode mpscnnNeuronHardSigmoidNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronHardSigmoidNode -> sourceNode -> IO (Id MPSCNNNeuronHardSigmoidNode)
initWithSource mpscnnNeuronHardSigmoidNode sourceNode =
  sendOwnedMessage mpscnnNeuronHardSigmoidNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:@
nodeWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronHardSigmoidNode)
nodeWithSource_a_bSelector = mkSelector "nodeWithSource:a:b:"

-- | @Selector@ for @initWithSource:a:b:@
initWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronHardSigmoidNode)
initWithSource_a_bSelector = mkSelector "initWithSource:a:b:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronHardSigmoidNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronHardSigmoidNode)
initWithSourceSelector = mkSelector "initWithSource:"

