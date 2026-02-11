{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that defines the parameters for  a 2D-depthwise convolution operation.
--
-- An @MPSGraphDepthwiseConvolution2DOpDescriptor@ defines constant parameters for 2D-depthwise convolutions. Use this class with ``MPSGraph/depthwiseConvolution2DWithSourceTensor:weightsTensor:descriptor:name:``, ``MPSGraph/depthwiseConvolution2DDataGradientWithIncomingGradientTensor:weightsTensor:outputShape:descriptor:name:``, and ``MPSGraph/depthwiseConvolution2DWeightsGradientWithIncomingGradientTensor:sourceTensor:outputShape:descriptor:name:`` methods.
--
-- Generated bindings for @MPSGraphDepthwiseConvolution2DOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphDepthwiseConvolution2DOpDescriptor
  ( MPSGraphDepthwiseConvolution2DOpDescriptor
  , IsMPSGraphDepthwiseConvolution2DOpDescriptor(..)
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout
  , descriptorWithDataLayout_weightsLayout
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom
  , strideInX
  , setStrideInX
  , strideInY
  , setStrideInY
  , dilationRateInX
  , setDilationRateInX
  , dilationRateInY
  , setDilationRateInY
  , paddingLeft
  , setPaddingLeft
  , paddingRight
  , setPaddingRight
  , paddingTop
  , setPaddingTop
  , paddingBottom
  , setPaddingBottom
  , paddingStyle
  , setPaddingStyle
  , dataLayout
  , setDataLayout
  , weightsLayout
  , setWeightsLayout
  , descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector
  , descriptorWithDataLayout_weightsLayoutSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , strideInXSelector
  , setStrideInXSelector
  , strideInYSelector
  , setStrideInYSelector
  , dilationRateInXSelector
  , setDilationRateInXSelector
  , dilationRateInYSelector
  , setDilationRateInYSelector
  , paddingLeftSelector
  , setPaddingLeftSelector
  , paddingRightSelector
  , setPaddingRightSelector
  , paddingTopSelector
  , setPaddingTopSelector
  , paddingBottomSelector
  , setPaddingBottomSelector
  , paddingStyleSelector
  , setPaddingStyleSelector
  , dataLayoutSelector
  , setDataLayoutSelector
  , weightsLayoutSelector
  , setWeightsLayoutSelector

  -- * Enum types
  , MPSGraphPaddingStyle(MPSGraphPaddingStyle)
  , pattern MPSGraphPaddingStyleExplicit
  , pattern MPSGraphPaddingStyleTF_VALID
  , pattern MPSGraphPaddingStyleTF_SAME
  , pattern MPSGraphPaddingStyleExplicitOffset
  , pattern MPSGraphPaddingStyleONNX_SAME_LOWER
  , MPSGraphTensorNamedDataLayout(MPSGraphTensorNamedDataLayout)
  , pattern MPSGraphTensorNamedDataLayoutNCHW
  , pattern MPSGraphTensorNamedDataLayoutNHWC
  , pattern MPSGraphTensorNamedDataLayoutOIHW
  , pattern MPSGraphTensorNamedDataLayoutHWIO
  , pattern MPSGraphTensorNamedDataLayoutCHW
  , pattern MPSGraphTensorNamedDataLayoutHWC
  , pattern MPSGraphTensorNamedDataLayoutHW
  , pattern MPSGraphTensorNamedDataLayoutNCDHW
  , pattern MPSGraphTensorNamedDataLayoutNDHWC
  , pattern MPSGraphTensorNamedDataLayoutOIDHW
  , pattern MPSGraphTensorNamedDataLayoutDHWIO

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a 2D-depthwise convolution descriptor with given values.
--
-- - Parameters:   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - dilationRateInX: See @dilationRateInX@ property.   - dilationRateInY: See @dilationRateInY@ property.   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.   - paddingStyle: See @paddingStyle@ property.   - dataLayout: See @dataLayout@ property.   - weightsLayout: See @weightsLayout@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphDepthwiseConvolution2DOpDescriptor)
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayout strideInX strideInY dilationRateInX dilationRateInY paddingLeft paddingRight paddingTop paddingBottom paddingStyle dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphDepthwiseConvolution2DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:") (retPtr retVoid) [argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom), argCULong (coerce paddingStyle), argCULong (coerce dataLayout), argCULong (coerce weightsLayout)] >>= retainedObject . castPtr

-- | Creates a 2D-depthwise convolution descriptor with given properties and default values.
--
-- - Parameters:   - dataLayout: See @dataLayout@ property.   - weightsLayout: See @weightsLayout@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithDataLayout:weightsLayout:@
descriptorWithDataLayout_weightsLayout :: MPSGraphTensorNamedDataLayout -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphDepthwiseConvolution2DOpDescriptor)
descriptorWithDataLayout_weightsLayout dataLayout weightsLayout =
  do
    cls' <- getRequiredClass "MPSGraphDepthwiseConvolution2DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithDataLayout:weightsLayout:") (retPtr retVoid) [argCULong (coerce dataLayout), argCULong (coerce weightsLayout)] >>= retainedObject . castPtr

-- | Sets the explicit padding values.
--
-- Note: this method also sets @paddingStyle@ to @MPSGraphPaddingStyleExplicit@ (see ``MPSGraphPaddingStyle``).
--
-- - Parameters:   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphDepthwiseConvolution2DOpDescriptor  paddingLeft paddingRight paddingTop paddingBottom =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:") retVoid [argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom)]

-- | The stride for the x dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
strideInX mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "strideInX") retCULong []

-- | The stride for the x dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setStrideInX:") retVoid [argCULong (fromIntegral value)]

-- | The stride for the y dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
strideInY mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "strideInY") retCULong []

-- | The stride for the y dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setStrideInY:") retVoid [argCULong (fromIntegral value)]

-- | The dilation rate for the x dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
dilationRateInX mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "dilationRateInX") retCULong []

-- | The dilation rate for the x dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setDilationRateInX:") retVoid [argCULong (fromIntegral value)]

-- | The dilation rate for the y dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
dilationRateInY mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "dilationRateInY") retCULong []

-- | The dilation rate for the y dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setDilationRateInY:") retVoid [argCULong (fromIntegral value)]

-- | The explicit padding value for the x dimension the operation adds before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
paddingLeft mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "paddingLeft") retCULong []

-- | The explicit padding value for the x dimension the operation adds before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setPaddingLeft:") retVoid [argCULong (fromIntegral value)]

-- | The explicit padding value for the x dimension operation adds after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
paddingRight mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "paddingRight") retCULong []

-- | The explicit padding value for the x dimension operation adds after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setPaddingRight:") retVoid [argCULong (fromIntegral value)]

-- | The explicit padding value for the y dimension operation adds before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
paddingTop mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "paddingTop") retCULong []

-- | The explicit padding value for the y dimension operation adds before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setPaddingTop:") retVoid [argCULong (fromIntegral value)]

-- | The explicit padding value for the y dimension operation adds after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO CULong
paddingBottom mpsGraphDepthwiseConvolution2DOpDescriptor  =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "paddingBottom") retCULong []

-- | The explicit padding value for the y dimension operation adds after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setPaddingBottom:") retVoid [argCULong (fromIntegral value)]

-- | The padding style for the operation.
--
-- Default value is @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphDepthwiseConvolution2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPaddingStyle) $ sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "paddingStyle") retCULong []

-- | The padding style for the operation.
--
-- Default value is @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setPaddingStyle:") retVoid [argCULong (coerce value)]

-- | The data layout of the input data in the forward pass.
--
-- See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphDepthwiseConvolution2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "dataLayout") retCULong []

-- | The data layout of the input data in the forward pass.
--
-- See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setDataLayout:") retVoid [argCULong (coerce value)]

-- | The data layout of the weights.
--
-- NOTE: 'O' index is channel multiplier index. See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- weightsLayout@
weightsLayout :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
weightsLayout mpsGraphDepthwiseConvolution2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "weightsLayout") retCULong []

-- | The data layout of the weights.
--
-- NOTE: 'O' index is channel multiplier index. See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- setWeightsLayout:@
setWeightsLayout :: IsMPSGraphDepthwiseConvolution2DOpDescriptor mpsGraphDepthwiseConvolution2DOpDescriptor => mpsGraphDepthwiseConvolution2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setWeightsLayout mpsGraphDepthwiseConvolution2DOpDescriptor  value =
  sendMsg mpsGraphDepthwiseConvolution2DOpDescriptor (mkSelector "setWeightsLayout:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:@
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector :: Selector
descriptorWithStrideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout_weightsLayoutSelector = mkSelector "descriptorWithStrideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:weightsLayout:"

-- | @Selector@ for @descriptorWithDataLayout:weightsLayout:@
descriptorWithDataLayout_weightsLayoutSelector :: Selector
descriptorWithDataLayout_weightsLayoutSelector = mkSelector "descriptorWithDataLayout:weightsLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @setStrideInX:@
setStrideInXSelector :: Selector
setStrideInXSelector = mkSelector "setStrideInX:"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @setStrideInY:@
setStrideInYSelector :: Selector
setStrideInYSelector = mkSelector "setStrideInY:"

-- | @Selector@ for @dilationRateInX@
dilationRateInXSelector :: Selector
dilationRateInXSelector = mkSelector "dilationRateInX"

-- | @Selector@ for @setDilationRateInX:@
setDilationRateInXSelector :: Selector
setDilationRateInXSelector = mkSelector "setDilationRateInX:"

-- | @Selector@ for @dilationRateInY@
dilationRateInYSelector :: Selector
dilationRateInYSelector = mkSelector "dilationRateInY"

-- | @Selector@ for @setDilationRateInY:@
setDilationRateInYSelector :: Selector
setDilationRateInYSelector = mkSelector "setDilationRateInY:"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @setPaddingLeft:@
setPaddingLeftSelector :: Selector
setPaddingLeftSelector = mkSelector "setPaddingLeft:"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @setPaddingRight:@
setPaddingRightSelector :: Selector
setPaddingRightSelector = mkSelector "setPaddingRight:"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @setPaddingTop:@
setPaddingTopSelector :: Selector
setPaddingTopSelector = mkSelector "setPaddingTop:"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @setPaddingBottom:@
setPaddingBottomSelector :: Selector
setPaddingBottomSelector = mkSelector "setPaddingBottom:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector
dataLayoutSelector = mkSelector "dataLayout"

-- | @Selector@ for @setDataLayout:@
setDataLayoutSelector :: Selector
setDataLayoutSelector = mkSelector "setDataLayout:"

-- | @Selector@ for @weightsLayout@
weightsLayoutSelector :: Selector
weightsLayoutSelector = mkSelector "weightsLayout"

-- | @Selector@ for @setWeightsLayout:@
setWeightsLayoutSelector :: Selector
setWeightsLayoutSelector = mkSelector "setWeightsLayout:"

