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
initWithSource mpscnnNeuronGeLUNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronGeLUNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | Create an autoreleased node
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronGeLUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronGeLUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

