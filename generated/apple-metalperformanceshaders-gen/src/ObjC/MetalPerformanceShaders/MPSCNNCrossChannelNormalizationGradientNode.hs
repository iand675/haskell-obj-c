{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNCrossChannelNormalizationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNCrossChannelNormalizationGradientNode
  ( MPSCNNCrossChannelNormalizationGradientNode
  , IsMPSCNNCrossChannelNormalizationGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_kernelSize
  , initWithSourceGradient_sourceImage_gradientState_kernelSize
  , kernelSize
  , initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , kernelSizeSelector
  , nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNCrossChannelNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelSize sourceGradient sourceImage gradientState kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNCrossChannelNormalizationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelSize

-- | @- initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSCNNCrossChannelNormalizationGradientNode mpscnnCrossChannelNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnCrossChannelNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNCrossChannelNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelSize mpscnnCrossChannelNormalizationGradientNode sourceGradient sourceImage gradientState kernelSize =
  sendOwnedMessage mpscnnCrossChannelNormalizationGradientNode initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelSize

-- | @- kernelSize@
kernelSize :: IsMPSCNNCrossChannelNormalizationGradientNode mpscnnCrossChannelNormalizationGradientNode => mpscnnCrossChannelNormalizationGradientNode -> IO CULong
kernelSize mpscnnCrossChannelNormalizationGradientNode =
  sendMessage mpscnnCrossChannelNormalizationGradientNode kernelSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong] (Id MPSCNNCrossChannelNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong] (Id MPSCNNCrossChannelNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @kernelSize@
kernelSizeSelector :: Selector '[] CULong
kernelSizeSelector = mkSelector "kernelSize"

