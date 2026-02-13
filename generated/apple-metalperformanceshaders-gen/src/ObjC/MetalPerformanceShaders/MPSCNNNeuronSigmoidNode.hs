{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronSigmoid kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = 1 / (1 + e^-x)
--
-- Generated bindings for @MPSCNNNeuronSigmoidNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronSigmoidNode
  ( MPSCNNNeuronSigmoidNode
  , IsMPSCNNNeuronSigmoidNode(..)
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
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronSigmoidNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronSigmoidNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronSigmoidNode mpscnnNeuronSigmoidNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronSigmoidNode -> sourceNode -> IO (Id MPSCNNNeuronSigmoidNode)
initWithSource mpscnnNeuronSigmoidNode sourceNode =
  sendOwnedMessage mpscnnNeuronSigmoidNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronSigmoidNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronSigmoidNode)
initWithSourceSelector = mkSelector "initWithSource:"

