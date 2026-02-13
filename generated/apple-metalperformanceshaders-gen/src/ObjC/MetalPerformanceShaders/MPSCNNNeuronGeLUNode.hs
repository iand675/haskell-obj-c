{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronGeLU kernel
--
-- For each pixel, applies the following function:
--
-- Generated bindings for @MPSCNNNeuronGeLUNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronGeLUNode
  ( MPSCNNNeuronGeLUNode
  , IsMPSCNNNeuronGeLUNode(..)
  , initWithSource
  , nodeWithSource
  , initWithSourceSelector
  , nodeWithSourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init a node representing a MPSCNNNeuronGeLU kernel
--
-- For each pixel, applies the following function:
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronLogarithm kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronGeLUNode mpscnnNeuronGeLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronGeLUNode -> sourceNode -> IO (Id MPSCNNNeuronGeLUNode)
initWithSource mpscnnNeuronGeLUNode sourceNode =
  sendOwnedMessage mpscnnNeuronGeLUNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Create an autoreleased node
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronGeLUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronGeLUNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronGeLUNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronGeLUNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

