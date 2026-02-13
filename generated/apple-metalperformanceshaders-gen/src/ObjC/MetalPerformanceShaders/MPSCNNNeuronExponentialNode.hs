{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronExponential kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = c ^ (a * x + b)
--
-- Generated bindings for @MPSCNNNeuronExponentialNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronExponentialNode
  ( MPSCNNNeuronExponentialNode
  , IsMPSCNNNeuronExponentialNode(..)
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
nodeWithSource_a_b_c :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronExponentialNode)
nodeWithSource_a_b_c sourceNode a b c =
  do
    cls' <- getRequiredClass "MPSCNNNeuronExponentialNode"
    sendClassMessage cls' nodeWithSource_a_b_cSelector (toMPSNNImageNode sourceNode) a b c

-- | Init a node representing a MPSCNNNeuronExponential kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = c ^ (a * x + b)
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- @c@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronExponential kernel.
--
-- ObjC selector: @- initWithSource:a:b:c:@
initWithSource_a_b_c :: (IsMPSCNNNeuronExponentialNode mpscnnNeuronExponentialNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronExponentialNode -> sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronExponentialNode)
initWithSource_a_b_c mpscnnNeuronExponentialNode sourceNode a b c =
  sendOwnedMessage mpscnnNeuronExponentialNode initWithSource_a_b_cSelector (toMPSNNImageNode sourceNode) a b c

-- | Create an autoreleased node with default values for parameters a, b, and c
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronExponentialNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronExponentialNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a, b, and c
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronExponentialNode mpscnnNeuronExponentialNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronExponentialNode -> sourceNode -> IO (Id MPSCNNNeuronExponentialNode)
initWithSource mpscnnNeuronExponentialNode sourceNode =
  sendOwnedMessage mpscnnNeuronExponentialNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:c:@
nodeWithSource_a_b_cSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat, CFloat] (Id MPSCNNNeuronExponentialNode)
nodeWithSource_a_b_cSelector = mkSelector "nodeWithSource:a:b:c:"

-- | @Selector@ for @initWithSource:a:b:c:@
initWithSource_a_b_cSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat, CFloat] (Id MPSCNNNeuronExponentialNode)
initWithSource_a_b_cSelector = mkSelector "initWithSource:a:b:c:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronExponentialNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronExponentialNode)
initWithSourceSelector = mkSelector "initWithSource:"

