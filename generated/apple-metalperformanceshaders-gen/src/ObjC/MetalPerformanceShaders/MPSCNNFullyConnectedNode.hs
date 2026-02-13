{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNFilterNode representing a MPSCNNFullyConnected kernel
--
-- Generated bindings for @MPSCNNFullyConnectedNode@.
module ObjC.MetalPerformanceShaders.MPSCNNFullyConnectedNode
  ( MPSCNNFullyConnectedNode
  , IsMPSCNNFullyConnectedNode(..)
  , nodeWithSource_weights
  , initWithSource_weights
  , initWithSource_weightsSelector
  , nodeWithSource_weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init an autoreleased not representing a MPSCNNFullyConnected kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- Returns: A new MPSNNFilter node for a MPSCNNConvolution kernel.
--
-- ObjC selector: @+ nodeWithSource:weights:@
nodeWithSource_weights :: IsMPSNNImageNode sourceNode => sourceNode -> RawId -> IO (Id MPSCNNFullyConnectedNode)
nodeWithSource_weights sourceNode weights =
  do
    cls' <- getRequiredClass "MPSCNNFullyConnectedNode"
    sendClassMessage cls' nodeWithSource_weightsSelector (toMPSNNImageNode sourceNode) weights

-- | Init a node representing a MPSCNNFullyConnected kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- Returns: A new MPSNNFilter node for a MPSCNNFullyConnected kernel.
--
-- ObjC selector: @- initWithSource:weights:@
initWithSource_weights :: (IsMPSCNNFullyConnectedNode mpscnnFullyConnectedNode, IsMPSNNImageNode sourceNode) => mpscnnFullyConnectedNode -> sourceNode -> RawId -> IO (Id MPSCNNFullyConnectedNode)
initWithSource_weights mpscnnFullyConnectedNode sourceNode weights =
  sendOwnedMessage mpscnnFullyConnectedNode initWithSource_weightsSelector (toMPSNNImageNode sourceNode) weights

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:weights:@
nodeWithSource_weightsSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNFullyConnectedNode)
nodeWithSource_weightsSelector = mkSelector "nodeWithSource:weights:"

-- | @Selector@ for @initWithSource:weights:@
initWithSource_weightsSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNFullyConnectedNode)
initWithSource_weightsSelector = mkSelector "initWithSource:weights:"

