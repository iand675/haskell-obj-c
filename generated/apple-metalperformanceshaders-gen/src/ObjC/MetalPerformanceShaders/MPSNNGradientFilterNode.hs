{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNGradientFilterNode
--
-- For each MPSNNFilterNode, there is a corresponding MPSNNGradientFilterNode              used for training that back propagates image gradients to refine the              various parameters in each node. Generally, it takes as input a gradient              corresponding to the result image from the MPSNNFilterNode and returns              a gradient image corresponding to the source image of the MPSNNFilterNode.              In addition, there is generally a MPSNNState produced by the MPSNNFilterNode              that is consumed by the MPSNNGradientNode and the MPSNNGradientNode generally              needs to look at the MPSNNFilterNode source image.
--
-- If you have a simple method to traverse your inference graph backwards, then              -[MPSNNFilterNode gradientFilterWithSource:] is a simple way to construct              these.
--
-- Generated bindings for @MPSNNGradientFilterNode@.
module ObjC.MetalPerformanceShaders.MPSNNGradientFilterNode
  ( MPSNNGradientFilterNode
  , IsMPSNNGradientFilterNode(..)
  , gradientFilterWithSources
  , gradientFiltersWithSources
  , gradientFilterWithSource
  , gradientFiltersWithSource
  , gradientFilterWithSourceSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourceSelector
  , gradientFiltersWithSourcesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsNSArray sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnGradientFilterNode sourceGradient =
  sendMessage mpsnnGradientFilterNode gradientFilterWithSourcesSelector (toNSArray sourceGradient)

-- | @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsNSArray sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSources mpsnnGradientFilterNode sourceGradient =
  sendMessage mpsnnGradientFilterNode gradientFiltersWithSourcesSelector (toNSArray sourceGradient)

-- | @- gradientFilterWithSource:@
gradientFilterWithSource :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsMPSNNImageNode sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSource mpsnnGradientFilterNode sourceGradient =
  sendMessage mpsnnGradientFilterNode gradientFilterWithSourceSelector (toMPSNNImageNode sourceGradient)

-- | @- gradientFiltersWithSource:@
gradientFiltersWithSource :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsMPSNNImageNode sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSource mpsnnGradientFilterNode sourceGradient =
  sendMessage mpsnnGradientFilterNode gradientFiltersWithSourceSelector (toMPSNNImageNode sourceGradient)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector '[Id NSArray] (Id NSArray)
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

-- | @Selector@ for @gradientFilterWithSource:@
gradientFilterWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNGradientFilterNode)
gradientFilterWithSourceSelector = mkSelector "gradientFilterWithSource:"

-- | @Selector@ for @gradientFiltersWithSource:@
gradientFiltersWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id NSArray)
gradientFiltersWithSourceSelector = mkSelector "gradientFiltersWithSource:"

