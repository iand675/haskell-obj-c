{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronReLUN kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = min((x >= 0 ? x : a * x), b)
--
-- Generated bindings for @MPSCNNNeuronReLUNNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLUNNode
  ( MPSCNNNeuronReLUNNode
  , IsMPSCNNNeuronReLUNNode(..)
  , nodeWithSource_a_b
  , initWithSource_a_b
  , nodeWithSource
  , initWithSource
  , initWithSourceSelector
  , initWithSource_a_bSelector
  , nodeWithSourceSelector
  , nodeWithSource_a_bSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:a:b:@
nodeWithSource_a_b :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronReLUNNode)
nodeWithSource_a_b sourceNode a b =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNNode"
    sendClassMessage cls' nodeWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | @- initWithSource:a:b:@
initWithSource_a_b :: (IsMPSCNNNeuronReLUNNode mpscnnNeuronReLUNNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNNode -> sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronReLUNNode)
initWithSource_a_b mpscnnNeuronReLUNNode sourceNode a b =
  sendOwnedMessage mpscnnNeuronReLUNNode initWithSource_a_bSelector (toMPSNNImageNode sourceNode) a b

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronReLUNNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronReLUNNode mpscnnNeuronReLUNNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNNode -> sourceNode -> IO (Id MPSCNNNeuronReLUNNode)
initWithSource mpscnnNeuronReLUNNode sourceNode =
  sendOwnedMessage mpscnnNeuronReLUNNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:@
nodeWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronReLUNNode)
nodeWithSource_a_bSelector = mkSelector "nodeWithSource:a:b:"

-- | @Selector@ for @initWithSource:a:b:@
initWithSource_a_bSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat] (Id MPSCNNNeuronReLUNNode)
initWithSource_a_bSelector = mkSelector "initWithSource:a:b:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronReLUNNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronReLUNNode)
initWithSourceSelector = mkSelector "initWithSource:"

