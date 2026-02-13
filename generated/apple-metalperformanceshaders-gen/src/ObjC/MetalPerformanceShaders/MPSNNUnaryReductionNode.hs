{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node for a unary MPSNNReduce node.
--
-- This is an abstract base class that does not correspond with any              particular MPSCNNKernel. Please make one of the MPSNNReduction              subclasses instead.
--
-- Generated bindings for @MPSNNUnaryReductionNode@.
module ObjC.MetalPerformanceShaders.MPSNNUnaryReductionNode
  ( MPSNNUnaryReductionNode
  , IsMPSNNUnaryReductionNode(..)
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

-- | Create an autoreleased node representing an MPS reduction kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for an MPS reduction kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSNNUnaryReductionNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSNNUnaryReductionNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node representing an MPS reduction kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for an MPS reduction kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSNNUnaryReductionNode mpsnnUnaryReductionNode, IsMPSNNImageNode sourceNode) => mpsnnUnaryReductionNode -> sourceNode -> IO (Id MPSNNUnaryReductionNode)
initWithSource mpsnnUnaryReductionNode sourceNode =
  sendOwnedMessage mpsnnUnaryReductionNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNUnaryReductionNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNUnaryReductionNode)
initWithSourceSelector = mkSelector "initWithSource:"

