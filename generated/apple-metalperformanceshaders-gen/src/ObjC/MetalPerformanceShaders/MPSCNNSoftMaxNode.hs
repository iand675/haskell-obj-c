{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSCNNSoftMax kernel
--
-- Generated bindings for @MPSCNNSoftMaxNode@.
module ObjC.MetalPerformanceShaders.MPSCNNSoftMaxNode
  ( MPSCNNSoftMaxNode
  , IsMPSCNNSoftMaxNode(..)
  , nodeWithSource
  , initWithSource
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

-- | Init a node representing a autoreleased MPSCNNSoftMax kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNSoftMax kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNSoftMaxNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNSoftMaxNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node representing a MPSCNNSoftMax kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNSoftMax kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNSoftMaxNode mpscnnSoftMaxNode, IsMPSNNImageNode sourceNode) => mpscnnSoftMaxNode -> sourceNode -> IO (Id MPSCNNSoftMaxNode)
initWithSource mpscnnSoftMaxNode sourceNode =
  sendOwnedMessage mpscnnSoftMaxNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNSoftMaxNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNSoftMaxNode)
initWithSourceSelector = mkSelector "initWithSource:"

