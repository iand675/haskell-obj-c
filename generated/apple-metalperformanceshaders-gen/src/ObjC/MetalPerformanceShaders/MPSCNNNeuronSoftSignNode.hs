{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronSoftSign kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x / (1 + abs(x))
--
-- Generated bindings for @MPSCNNNeuronSoftSignNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronSoftSignNode
  ( MPSCNNNeuronSoftSignNode
  , IsMPSCNNNeuronSoftSignNode(..)
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
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronSoftSignNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronSoftSignNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronSoftSignNode mpscnnNeuronSoftSignNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronSoftSignNode -> sourceNode -> IO (Id MPSCNNNeuronSoftSignNode)
initWithSource mpscnnNeuronSoftSignNode sourceNode =
  sendOwnedMessage mpscnnNeuronSoftSignNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronSoftSignNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronSoftSignNode)
initWithSourceSelector = mkSelector "initWithSource:"

