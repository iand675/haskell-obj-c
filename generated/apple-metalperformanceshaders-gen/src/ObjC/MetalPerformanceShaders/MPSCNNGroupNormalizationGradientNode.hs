{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNGroupNormalizationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNGroupNormalizationGradientNode
  ( MPSCNNGroupNormalizationGradientNode
  , IsMPSCNNGroupNormalizationGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState
  , initWithSourceGradient_sourceImage_gradientState
  , initWithSourceGradient_sourceImage_gradientStateSelector
  , nodeWithSourceGradient_sourceImage_gradientStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSCNNGroupNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- | @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSCNNGroupNormalizationGradientNode mpscnnGroupNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnGroupNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSCNNGroupNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState mpscnnGroupNormalizationGradientNode sourceGradient sourceImage gradientState =
  sendOwnedMessage mpscnnGroupNormalizationGradientNode initWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSCNNGroupNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSCNNGroupNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

