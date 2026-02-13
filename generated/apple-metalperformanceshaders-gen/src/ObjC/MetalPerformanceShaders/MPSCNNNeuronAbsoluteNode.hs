{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronAbsolute kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = fabs(x)
--
-- Generated bindings for @MPSCNNNeuronAbsoluteNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronAbsoluteNode
  ( MPSCNNNeuronAbsoluteNode
  , IsMPSCNNNeuronAbsoluteNode(..)
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

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronAbsoluteNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronAbsoluteNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronAbsoluteNode mpscnnNeuronAbsoluteNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronAbsoluteNode -> sourceNode -> IO (Id MPSCNNNeuronAbsoluteNode)
initWithSource mpscnnNeuronAbsoluteNode sourceNode =
  sendOwnedMessage mpscnnNeuronAbsoluteNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronAbsoluteNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronAbsoluteNode)
initWithSourceSelector = mkSelector "initWithSource:"

