{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronReLU kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x            if x >= 0
-- = a * x        if x < 0
--
-- Generated bindings for @MPSCNNNeuronReLUNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLUNode
  ( MPSCNNNeuronReLUNode
  , IsMPSCNNNeuronReLUNode(..)
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
nodeWithSource_a :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> IO (Id MPSCNNNeuronReLUNode)
nodeWithSource_a sourceNode a =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNode"
    sendClassMessage cls' nodeWithSource_aSelector (toMPSNNImageNode sourceNode) a

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronReLUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronReLUNode mpscnnNeuronReLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNode -> sourceNode -> IO (Id MPSCNNNeuronReLUNode)
initWithSource mpscnnNeuronReLUNode sourceNode =
  sendOwnedMessage mpscnnNeuronReLUNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:a:@
initWithSource_a :: (IsMPSCNNNeuronReLUNode mpscnnNeuronReLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNode -> sourceNode -> CFloat -> IO (Id MPSCNNNeuronReLUNode)
initWithSource_a mpscnnNeuronReLUNode sourceNode a =
  sendOwnedMessage mpscnnNeuronReLUNode initWithSource_aSelector (toMPSNNImageNode sourceNode) a

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:@
nodeWithSource_aSelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSCNNNeuronReLUNode)
nodeWithSource_aSelector = mkSelector "nodeWithSource:a:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronReLUNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronReLUNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:a:@
initWithSource_aSelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSCNNNeuronReLUNode)
initWithSource_aSelector = mkSelector "initWithSource:a:"

