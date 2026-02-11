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
  , nodeWithSourceGradient_sourceImage_gradientStateSelector
  , initWithSourceGradient_sourceImage_gradientStateSelector
  , nodeWithSourceGradient_sourceImage_gradientState_alphaSelector
  , initWithSourceGradient_sourceImage_gradientState_alphaSelector
  , alphaSelector


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

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNGramMatrixCalculationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSNNGramMatrixCalculationGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSNNGramMatrixCalculationGradientNode mpsnnGramMatrixCalculationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnGramMatrixCalculationGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNGramMatrixCalculationGradientNode)
initWithSourceGradient_sourceImage_gradientState mpsnnGramMatrixCalculationGradientNode  sourceGradient sourceImage gradientState =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpsnnGramMatrixCalculationGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= ownedObject . castPtr

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:alpha:@
nodeWithSourceGradient_sourceImage_gradientState_alpha :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CFloat -> IO (Id MPSNNGramMatrixCalculationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_alpha sourceGradient sourceImage gradientState alpha =
  do
    cls' <- getRequiredClass "MPSNNGramMatrixCalculationGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:alpha:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCFloat (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:gradientState:alpha:@
initWithSourceGradient_sourceImage_gradientState_alpha :: (IsMPSNNGramMatrixCalculationGradientNode mpsnnGramMatrixCalculationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnGramMatrixCalculationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CFloat -> IO (Id MPSNNGramMatrixCalculationGradientNode)
initWithSourceGradient_sourceImage_gradientState_alpha mpsnnGramMatrixCalculationGradientNode  sourceGradient sourceImage gradientState alpha =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpsnnGramMatrixCalculationGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:alpha:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCFloat (fromIntegral alpha)] >>= ownedObject . castPtr

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNNGramMatrixCalculationGradientNode mpsnnGramMatrixCalculationGradientNode => mpsnnGramMatrixCalculationGradientNode -> IO CFloat
alpha mpsnnGramMatrixCalculationGradientNode  =
  sendMsg mpsnnGramMatrixCalculationGradientNode (mkSelector "alpha") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:alpha:@
nodeWithSourceGradient_sourceImage_gradientState_alphaSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_alphaSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:alpha:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:alpha:@
initWithSourceGradient_sourceImage_gradientState_alphaSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_alphaSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:alpha:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

