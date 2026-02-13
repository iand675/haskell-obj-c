{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronPower kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = (a * x + b) ^ c
--
-- Generated bindings for @MPSCNNNeuronPowerNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronPowerNode
  ( MPSCNNNeuronPowerNode
  , IsMPSCNNNeuronPowerNode(..)
  , nodeWithSource_a_b_c
  , initWithSource_a_b_c
  , nodeWithSource
  , initWithSource
  , initWithSourceSelector
  , initWithSource_a_b_cSelector
  , nodeWithSourceSelector
  , nodeWithSource_a_b_cSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:a:b:c:@
nodeWithSource_a_b_c :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronPowerNode)
nodeWithSource_a_b_c sourceNode a b c =
  do
    cls' <- getRequiredClass "MPSCNNNeuronPowerNode"
    sendClassMessage cls' nodeWithSource_a_b_cSelector (toMPSNNImageNode sourceNode) a b c

-- | Init a node representing a MPSCNNNeuronPower kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = (a * x + b) ^ c
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- @c@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronPower kernel.
--
-- ObjC selector: @- initWithSource:a:b:c:@
initWithSource_a_b_c :: (IsMPSCNNNeuronPowerNode mpscnnNeuronPowerNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronPowerNode -> sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronPowerNode)
initWithSource_a_b_c mpscnnNeuronPowerNode sourceNode a b c =
  sendOwnedMessage mpscnnNeuronPowerNode initWithSource_a_b_cSelector (toMPSNNImageNode sourceNode) a b c

-- | Create an autoreleased node with default values for parameters a, b, and c
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronPowerNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronPowerNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node with default values for parameters a, b, and c
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronPowerNode mpscnnNeuronPowerNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronPowerNode -> sourceNode -> IO (Id MPSCNNNeuronPowerNode)
initWithSource mpscnnNeuronPowerNode sourceNode =
  sendOwnedMessage mpscnnNeuronPowerNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:c:@
nodeWithSource_a_b_cSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat, CFloat] (Id MPSCNNNeuronPowerNode)
nodeWithSource_a_b_cSelector = mkSelector "nodeWithSource:a:b:c:"

-- | @Selector@ for @initWithSource:a:b:c:@
initWithSource_a_b_cSelector :: Selector '[Id MPSNNImageNode, CFloat, CFloat, CFloat] (Id MPSCNNNeuronPowerNode)
initWithSource_a_b_cSelector = mkSelector "initWithSource:a:b:c:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronPowerNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronPowerNode)
initWithSourceSelector = mkSelector "initWithSource:"

