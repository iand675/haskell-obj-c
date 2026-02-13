{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSCNNLogSoftMax kernel
--
-- Generated bindings for @MPSCNNLogSoftMaxNode@.
module ObjC.MetalPerformanceShaders.MPSCNNLogSoftMaxNode
  ( MPSCNNLogSoftMaxNode
  , IsMPSCNNLogSoftMaxNode(..)
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

-- | Init a node representing a autoreleased MPSCNNLogSoftMax kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNLogSoftMax kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNLogSoftMaxNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNLogSoftMaxNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node representing a MPSCNNLogSoftMax kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNLogSoftMax kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNLogSoftMaxNode mpscnnLogSoftMaxNode, IsMPSNNImageNode sourceNode) => mpscnnLogSoftMaxNode -> sourceNode -> IO (Id MPSCNNLogSoftMaxNode)
initWithSource mpscnnLogSoftMaxNode sourceNode =
  sendOwnedMessage mpscnnLogSoftMaxNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNLogSoftMaxNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNLogSoftMaxNode)
initWithSourceSelector = mkSelector "initWithSource:"

