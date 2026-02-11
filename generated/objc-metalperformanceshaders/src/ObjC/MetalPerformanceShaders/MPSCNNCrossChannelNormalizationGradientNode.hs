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
  , nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , kernelSizeSelector


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

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNCrossChannelNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelSize sourceGradient sourceImage gradientState kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNCrossChannelNormalizationGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSCNNCrossChannelNormalizationGradientNode mpscnnCrossChannelNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnCrossChannelNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNCrossChannelNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelSize mpscnnCrossChannelNormalizationGradientNode  sourceGradient sourceImage gradientState kernelSize =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnCrossChannelNormalizationGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= ownedObject . castPtr

-- | @- kernelSize@
kernelSize :: IsMPSCNNCrossChannelNormalizationGradientNode mpscnnCrossChannelNormalizationGradientNode => mpscnnCrossChannelNormalizationGradientNode -> IO CULong
kernelSize mpscnnCrossChannelNormalizationGradientNode  =
  sendMsg mpscnnCrossChannelNormalizationGradientNode (mkSelector "kernelSize") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @kernelSize@
kernelSizeSelector :: Selector
kernelSizeSelector = mkSelector "kernelSize"

