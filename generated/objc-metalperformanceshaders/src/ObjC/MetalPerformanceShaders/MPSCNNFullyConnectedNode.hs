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
  , nodeWithSource_weightsSelector
  , initWithSource_weightsSelector


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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= retainedObject . castPtr

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
initWithSource_weights mpscnnFullyConnectedNode  sourceNode weights =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnFullyConnectedNode (mkSelector "initWithSource:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:weights:@
nodeWithSource_weightsSelector :: Selector
nodeWithSource_weightsSelector = mkSelector "nodeWithSource:weights:"

-- | @Selector@ for @initWithSource:weights:@
initWithSource_weightsSelector :: Selector
initWithSource_weightsSelector = mkSelector "initWithSource:weights:"

