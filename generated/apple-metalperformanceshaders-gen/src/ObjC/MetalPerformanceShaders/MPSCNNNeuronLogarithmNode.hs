{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronLogarithm kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = log_c(a * x + b)
--
-- Generated bindings for @MPSCNNNeuronLogarithmNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronLogarithmNode
  ( MPSCNNNeuronLogarithmNode
  , IsMPSCNNNeuronLogarithmNode(..)
  , nodeWithSource_a_b_c
  , initWithSource_a_b_c
  , nodeWithSource
  , initWithSource
  , initWithSourceSelector
  , initWithSource_a_b_cSelector
  , nodeWithSourceSelector
  , nodeWithSource_a_b_cSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:a:b:c:@
nodeWithSource_a_b_c :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLogarithmNode)
nodeWithSource_a_b_c sourceNode a b c =
  do
    cls' <- getRequiredClass "MPSCNNNeuronLogarithmNode"
    sendClassMessage cls' nodeWithSource_a_b_cSelector (toMPSNNImageNode sourceNode) a b c

-- | Init a node representing a MPSCNNNeuronLogarithm kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = log_c(a * x + b)
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- @c@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronLogarithm kernel.
--
-- ObjC selector: @- initWithSource:a:b:c:@
initWithSource_a_b_c :: (IsMPSCNNNeuronLogarithmNode mpscnnNeuronLogarithmNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronLogarithmNode -> sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLogarithmNode)
initWithSource_a_b_c mpscnnNeuronLogarithmNode sourceNode a b c =
  sendOwnedMessage mpscnnNeuronLogarithmNode initWithSource_a_b_cSelector (toMPSNNImageNode sourceNode) a b c

-- | Create an autoreleased node with default values for parameters a, b, and c
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronLogarithmNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronLogarithmNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a, b, and c
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronLogarithmNode mpscnnNeuronLogarithmNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronLogarithmNode -> sourceNode -> IO (Id MPSCNNNeuronLogarithmNode)
initWithSource mpscnnNeuronLogarithmNode sourceNode =
  sendOwnedMessage mpscnnNeuronLogarithmNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:c:@
nodeWithSource_a_b_cSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat, CFloat] (Id MPSCNNNeuronLogarithmNode)
nodeWithSource_a_b_cSelector = mkSelector "nodeWithSource:a:b:c:"

-- | @Selector@ for @initWithSource:a:b:c:@
initWithSource_a_b_cSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat, CFloat] (Id MPSCNNNeuronLogarithmNode)
initWithSource_a_b_cSelector = mkSelector "initWithSource:a:b:c:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronLogarithmNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronLogarithmNode)
initWithSourceSelector = mkSelector "initWithSource:"

