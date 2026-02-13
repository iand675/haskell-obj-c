{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalizationGradientNode
--
-- A node representing batch normalization gradient for training
--
-- This filter encapsulates the MPSCNNBatchNormalizationStatisticsGradient              and MPSCNNBatchNormalizationGradient low level filters as a single              node. They will be called in sequence: statistics gradient until the              batch is complete, then batch normalization gradient on the result.
--
-- Generated bindings for @MPSCNNBatchNormalizationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalizationGradientNode
  ( MPSCNNBatchNormalizationGradientNode
  , IsMPSCNNBatchNormalizationGradientNode(..)
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
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSCNNBatchNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSCNNBatchNormalizationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- | @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSCNNBatchNormalizationGradientNode mpscnnBatchNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnBatchNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSCNNBatchNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState mpscnnBatchNormalizationGradientNode sourceGradient sourceImage gradientState =
  sendOwnedMessage mpscnnBatchNormalizationGradientNode initWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSCNNBatchNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSCNNBatchNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

