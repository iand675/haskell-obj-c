{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronELU kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * exp(x) - 1, x <  0
-- x             , x >= 0
--
-- Generated bindings for @MPSCNNNeuronELUNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronELUNode
  ( MPSCNNNeuronELUNode
  , IsMPSCNNNeuronELUNode(..)
  , nodeWithSource_a
  , nodeWithSource
  , initWithSource
  , initWithSource_a
  , initWithSourceSelector
  , initWithSource_aSelector
  , nodeWithSourceSelector
  , nodeWithSource_aSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:a:@
nodeWithSource_a :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> IO (Id MPSCNNNeuronELUNode)
nodeWithSource_a sourceNode a =
  do
    cls' <- getRequiredClass "MPSCNNNeuronELUNode"
    sendClassMessage cls' nodeWithSource_aSelector (toMPSNNImageNode sourceNode) a

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronELUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronELUNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronELUNode mpscnnNeuronELUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronELUNode -> sourceNode -> IO (Id MPSCNNNeuronELUNode)
initWithSource mpscnnNeuronELUNode sourceNode =
  sendOwnedMessage mpscnnNeuronELUNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | @- initWithSource:a:@
initWithSource_a :: (IsMPSCNNNeuronELUNode mpscnnNeuronELUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronELUNode -> sourceNode -> CFloat -> IO (Id MPSCNNNeuronELUNode)
initWithSource_a mpscnnNeuronELUNode sourceNode a =
  sendOwnedMessage mpscnnNeuronELUNode initWithSource_aSelector (toMPSNNImageNode sourceNode) a

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:@
nodeWithSource_aSelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSCNNNeuronELUNode)
nodeWithSource_aSelector = mkSelector "nodeWithSource:a:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronELUNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronELUNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:a:@
initWithSource_aSelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSCNNNeuronELUNode)
initWithSource_aSelector = mkSelector "initWithSource:a:"

