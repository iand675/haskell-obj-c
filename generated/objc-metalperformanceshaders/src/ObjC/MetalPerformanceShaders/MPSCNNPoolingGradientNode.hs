{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNPoolingGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingGradientNode
  ( MPSCNNPoolingGradientNode
  , IsMPSCNNPoolingGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy
  , kernelWidth
  , kernelHeight
  , strideInPixelsX
  , strideInPixelsY
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector
  , kernelWidthSelector
  , kernelHeightSelector
  , strideInPixelsXSelector
  , strideInPixelsYSelector


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

-- | make a pooling gradient node
--
-- It would be much easier to use [inferencePoolingNode gradientNodeForSourceGradient:] instead.
--
-- @sourceGradient@ — The gradient from the downstream gradient filter.
--
-- @sourceImage@ — The input image to the inference pooling filter
--
-- @gradientState@ — The gradient state produced by the inference poolin filter
--
-- @kernelWidth@ — The kernel width of the inference filter
--
-- @kernelHeight@ — The kernel height of the inference filter
--
-- @strideInPixelsX@ — The X stride from the inference filter
--
-- @strideInPixelsY@ — The Y stride from the inference filter
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> CULong -> CULong -> RawId -> IO (Id MPSCNNPoolingGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY paddingPolicy =
  do
    cls' <- getRequiredClass "MPSCNNPoolingGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY), argPtr (castPtr (unRawId paddingPolicy) :: Ptr ())] >>= retainedObject . castPtr

-- | make a pooling gradient node
--
-- It would be much easier to use [inferencePoolingNode gradientNodeForSourceGradient:] instead.
--
-- @sourceGradient@ — The gradient from the downstream gradient filter.
--
-- @sourceImage@ — The input image to the inference pooling filter
--
-- @gradientState@ — The gradient state produced by the inference poolin filter
--
-- @kernelWidth@ — The kernel width of the inference filter
--
-- @kernelHeight@ — The kernel height of the inference filter
--
-- @strideInPixelsX@ — The X stride from the inference filter
--
-- @strideInPixelsY@ — The Y stride from the inference filter
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy :: (IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnPoolingGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> CULong -> CULong -> RawId -> IO (Id MPSCNNPoolingGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy mpscnnPoolingGradientNode  sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY paddingPolicy =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnPoolingGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY), argPtr (castPtr (unRawId paddingPolicy) :: Ptr ())] >>= ownedObject . castPtr

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
kernelWidth mpscnnPoolingGradientNode  =
  sendMsg mpscnnPoolingGradientNode (mkSelector "kernelWidth") retCULong []

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
kernelHeight mpscnnPoolingGradientNode  =
  sendMsg mpscnnPoolingGradientNode (mkSelector "kernelHeight") retCULong []

-- | @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
strideInPixelsX mpscnnPoolingGradientNode  =
  sendMsg mpscnnPoolingGradientNode (mkSelector "strideInPixelsX") retCULong []

-- | @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
strideInPixelsY mpscnnPoolingGradientNode  =
  sendMsg mpscnnPoolingGradientNode (mkSelector "strideInPixelsY") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector :: Selector
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector
strideInPixelsYSelector = mkSelector "strideInPixelsY"

