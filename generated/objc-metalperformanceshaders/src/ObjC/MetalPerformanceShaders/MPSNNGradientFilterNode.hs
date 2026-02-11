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
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourcesSelector
  , gradientFilterWithSourceSelector
  , gradientFiltersWithSourceSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsNSArray sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnGradientFilterNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnGradientFilterNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsNSArray sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSources mpsnnGradientFilterNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnGradientFilterNode (mkSelector "gradientFiltersWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- gradientFilterWithSource:@
gradientFilterWithSource :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsMPSNNImageNode sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSource mpsnnGradientFilterNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnGradientFilterNode (mkSelector "gradientFilterWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- gradientFiltersWithSource:@
gradientFiltersWithSource :: (IsMPSNNGradientFilterNode mpsnnGradientFilterNode, IsMPSNNImageNode sourceGradient) => mpsnnGradientFilterNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSource mpsnnGradientFilterNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnGradientFilterNode (mkSelector "gradientFiltersWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

-- | @Selector@ for @gradientFilterWithSource:@
gradientFilterWithSourceSelector :: Selector
gradientFilterWithSourceSelector = mkSelector "gradientFilterWithSource:"

-- | @Selector@ for @gradientFiltersWithSource:@
gradientFiltersWithSourceSelector :: Selector
gradientFiltersWithSourceSelector = mkSelector "gradientFiltersWithSource:"

