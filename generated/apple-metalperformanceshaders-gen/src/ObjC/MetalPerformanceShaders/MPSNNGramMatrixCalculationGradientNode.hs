{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSNNGramMatrixCalculationGradient kernel
--
-- Generated bindings for @MPSNNGramMatrixCalculationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNGramMatrixCalculationGradientNode
  ( MPSNNGramMatrixCalculationGradientNode
  , IsMPSNNGramMatrixCalculationGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState
  , initWithSourceGradient_sourceImage_gradientState
  , nodeWithSourceGradient_sourceImage_gradientState_alpha
  , initWithSourceGradient_sourceImage_gradientState_alpha
  , alpha
  , alphaSelector
  , initWithSourceGradient_sourceImage_gradientStateSelector
  , initWithSourceGradient_sourceImage_gradientState_alphaSelector
  , nodeWithSourceGradient_sourceImage_gradientStateSelector
  , nodeWithSourceGradient_sourceImage_gradientState_alphaSelector


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
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNGramMatrixCalculationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSNNGramMatrixCalculationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- | @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSNNGramMatrixCalculationGradientNode mpsnnGramMatrixCalculationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnGramMatrixCalculationGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNGramMatrixCalculationGradientNode)
initWithSourceGradient_sourceImage_gradientState mpsnnGramMatrixCalculationGradientNode sourceGradient sourceImage gradientState =
  sendOwnedMessage mpsnnGramMatrixCalculationGradientNode initWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:alpha:@
nodeWithSourceGradient_sourceImage_gradientState_alpha :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CFloat -> IO (Id MPSNNGramMatrixCalculationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_alpha sourceGradient sourceImage gradientState alpha =
  do
    cls' <- getRequiredClass "MPSNNGramMatrixCalculationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_alphaSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) alpha

-- | @- initWithSourceGradient:sourceImage:gradientState:alpha:@
initWithSourceGradient_sourceImage_gradientState_alpha :: (IsMPSNNGramMatrixCalculationGradientNode mpsnnGramMatrixCalculationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnGramMatrixCalculationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CFloat -> IO (Id MPSNNGramMatrixCalculationGradientNode)
initWithSourceGradient_sourceImage_gradientState_alpha mpsnnGramMatrixCalculationGradientNode sourceGradient sourceImage gradientState alpha =
  sendOwnedMessage mpsnnGramMatrixCalculationGradientNode initWithSourceGradient_sourceImage_gradientState_alphaSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) alpha

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNNGramMatrixCalculationGradientNode mpsnnGramMatrixCalculationGradientNode => mpsnnGramMatrixCalculationGradientNode -> IO CFloat
alpha mpsnnGramMatrixCalculationGradientNode =
  sendMessage mpsnnGramMatrixCalculationGradientNode alphaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSNNGramMatrixCalculationGradientNode)
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSNNGramMatrixCalculationGradientNode)
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:alpha:@
nodeWithSourceGradient_sourceImage_gradientState_alphaSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CFloat] (Id MPSNNGramMatrixCalculationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_alphaSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:alpha:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:alpha:@
initWithSourceGradient_sourceImage_gradientState_alphaSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CFloat] (Id MPSNNGramMatrixCalculationGradientNode)
initWithSourceGradient_sourceImage_gradientState_alphaSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:alpha:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

