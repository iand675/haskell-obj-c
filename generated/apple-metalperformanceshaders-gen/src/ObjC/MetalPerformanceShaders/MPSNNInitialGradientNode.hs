{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNInitialGradientNode
--
-- A node for a MPSNNInitialGradient kernel
--
-- This node can be used to generate a starting point for an arbitrary gradient computation.                  Simply add this node after the node for which you want to compute gradients and then                  call the function trainingGraphWithSourceGradient: of this node to automatically                  generate the nodes needed for gradient computations or add the desired nodes manually.                  This is generally used with MPSNNLossGradientNode and MPSNNForwardLossNode
--
-- Generated bindings for @MPSNNInitialGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNInitialGradientNode
  ( MPSNNInitialGradientNode
  , IsMPSNNInitialGradientNode(..)
  , nodeWithSource
  , initWithSource
  , gradientFilterWithSources
  , gradientFilterWithSourcesSelector
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

-- | Init a node representing a MPSNNInitialGradient MPSNNPad kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSNNInitialGradient kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode source => source -> IO (Id MPSNNInitialGradientNode)
nodeWithSource source =
  do
    cls' <- getRequiredClass "MPSNNInitialGradientNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode source)

-- | Init a node representing a MPSNNInitialGradient MPSNNPad kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSNNInitialGradient kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSNNInitialGradientNode mpsnnInitialGradientNode, IsMPSNNImageNode source) => mpsnnInitialGradientNode -> source -> IO (Id MPSNNInitialGradientNode)
initWithSource mpsnnInitialGradientNode source =
  sendOwnedMessage mpsnnInitialGradientNode initWithSourceSelector (toMPSNNImageNode source)

-- | The initial gradient filter is a gradient filter and we don't provide support for gradients of gradients currently.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNInitialGradientNode mpsnnInitialGradientNode, IsNSArray gradientImages) => mpsnnInitialGradientNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnInitialGradientNode gradientImages =
  sendMessage mpsnnInitialGradientNode gradientFilterWithSourcesSelector (toNSArray gradientImages)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNInitialGradientNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNInitialGradientNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

