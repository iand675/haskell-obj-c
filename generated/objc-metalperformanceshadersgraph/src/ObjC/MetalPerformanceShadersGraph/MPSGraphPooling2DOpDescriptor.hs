{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a 2D pooling operation.
--
-- Use this descriptor with the following methods: - ``MPSGraph/maxPooling2DWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling2DGradientWithGradientTensor:sourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling2DGradientWithGradientTensor:indicesTensor:outputShape:descriptor:name:`` - ``MPSGraph/maxPooling2DGradientWithGradientTensor:indicesTensor:outputShapeTensor:descriptor:name:`` - ``MPSGraph/avgPooling2DWithSourceTensor:descriptor:name:`` - ``MPSGraph/avgPooling2DGradientWithGradientTensor:sourceTensor:descriptor:name:``
--
-- Generated bindings for @MPSGraphPooling2DOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphPooling2DOpDescriptor
  ( MPSGraphPooling2DOpDescriptor
  , IsMPSGraphPooling2DOpDescriptor(..)
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayout
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom
  , kernelWidth
  , setKernelWidth
  , kernelHeight
  , setKernelHeight
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
  , returnIndicesMode
  , setReturnIndicesMode
  , returnIndicesDataType
  , setReturnIndicesDataType
  , ceilMode
  , setCeilMode
  , includeZeroPadToAverage
  , setIncludeZeroPadToAverage
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , kernelWidthSelector
  , setKernelWidthSelector
  , kernelHeightSelector
  , setKernelHeightSelector
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
  , returnIndicesModeSelector
  , setReturnIndicesModeSelector
  , returnIndicesDataTypeSelector
  , setReturnIndicesDataTypeSelector
  , ceilModeSelector
  , setCeilModeSelector
  , includeZeroPadToAverageSelector
  , setIncludeZeroPadToAverageSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8
  , MPSGraphPaddingStyle(MPSGraphPaddingStyle)
  , pattern MPSGraphPaddingStyleExplicit
  , pattern MPSGraphPaddingStyleTF_VALID
  , pattern MPSGraphPaddingStyleTF_SAME
  , pattern MPSGraphPaddingStyleExplicitOffset
  , pattern MPSGraphPaddingStyleONNX_SAME_LOWER
  , MPSGraphPoolingReturnIndicesMode(MPSGraphPoolingReturnIndicesMode)
  , pattern MPSGraphPoolingReturnIndicesNone
  , pattern MPSGraphPoolingReturnIndicesGlobalFlatten1D
  , pattern MPSGraphPoolingReturnIndicesGlobalFlatten2D
  , pattern MPSGraphPoolingReturnIndicesGlobalFlatten3D
  , pattern MPSGraphPoolingReturnIndicesGlobalFlatten4D
  , pattern MPSGraphPoolingReturnIndicesLocalFlatten1D
  , pattern MPSGraphPoolingReturnIndicesLocalFlatten2D
  , pattern MPSGraphPoolingReturnIndicesLocalFlatten3D
  , pattern MPSGraphPoolingReturnIndicesLocalFlatten4D
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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a 2D pooling descriptor with given values.
--
-- - Parameters:   - kernelWidth: See @kernelWidth@ property.   - kernelHeight: See @kernelHeight@ property.   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - dilationRateInX: See @dilationRateInX@ property.   - dilationRateInY: See @dilationRateInY@ property.   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.   - paddingStyle: See @paddingStyle@ property.   - dataLayout: See @dataLayout@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout :: CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphPooling2DOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayout kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY paddingLeft paddingRight paddingTop paddingBottom paddingStyle dataLayout =
  do
    cls' <- getRequiredClass "MPSGraphPooling2DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:") (retPtr retVoid) [argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (fromIntegral dilationRateInX), argCULong (fromIntegral dilationRateInY), argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom), argCULong (coerce paddingStyle), argCULong (coerce dataLayout)] >>= retainedObject . castPtr

-- | Creates a 2D pooling descriptor with given values.
--
-- - Parameters:   - kernelWidth: See @kernelWidth@ property.   - kernelHeight: See @kernelHeight@` property.   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - paddingStyle: See @paddingStyle@ property.   - dataLayout: See @dataLayout@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayout :: CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphPooling2DOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayout kernelWidth kernelHeight strideInX strideInY paddingStyle dataLayout =
  do
    cls' <- getRequiredClass "MPSGraphPooling2DOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:") (retPtr retVoid) [argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY), argCULong (coerce paddingStyle), argCULong (coerce dataLayout)] >>= retainedObject . castPtr

-- | Sets the explicit padding values and sets padding style to explicit.
--
-- - Parameters:   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphPooling2DOpDescriptor  paddingLeft paddingRight paddingTop paddingBottom =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:") retVoid [argCULong (fromIntegral paddingLeft), argCULong (fromIntegral paddingRight), argCULong (fromIntegral paddingTop), argCULong (fromIntegral paddingBottom)]

-- | Defines the pooling window size for the width dimension.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
kernelWidth mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "kernelWidth") retCULong []

-- | Defines the pooling window size for the width dimension.
--
-- ObjC selector: @- setKernelWidth:@
setKernelWidth :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setKernelWidth mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setKernelWidth:") retVoid [argCULong (fromIntegral value)]

-- | Defines the pooling window size for the height dimension.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
kernelHeight mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "kernelHeight") retCULong []

-- | Defines the pooling window size for the height dimension.
--
-- ObjC selector: @- setKernelHeight:@
setKernelHeight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setKernelHeight mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setKernelHeight:") retVoid [argCULong (fromIntegral value)]

-- | Defines the stride for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
strideInX mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "strideInX") retCULong []

-- | Defines the stride for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setStrideInX:") retVoid [argCULong (fromIntegral value)]

-- | Defines the stride for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
strideInY mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "strideInY") retCULong []

-- | Defines the stride for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setStrideInY:") retVoid [argCULong (fromIntegral value)]

-- | Defines the dilation rate for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
dilationRateInX mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "dilationRateInX") retCULong []

-- | Defines the dilation rate for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setDilationRateInX:") retVoid [argCULong (fromIntegral value)]

-- | Defines the dilation rate for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
dilationRateInY mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "dilationRateInY") retCULong []

-- | Defines the dilation rate for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setDilationRateInY:") retVoid [argCULong (fromIntegral value)]

-- | Defines the explicit padding value for the width dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingLeft mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "paddingLeft") retCULong []

-- | Defines the explicit padding value for the width dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setPaddingLeft:") retVoid [argCULong (fromIntegral value)]

-- | Defines the explicit padding value for the width dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingRight mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "paddingRight") retCULong []

-- | Defines the explicit padding value for the width dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setPaddingRight:") retVoid [argCULong (fromIntegral value)]

-- | Defines the explicit padding value for the height dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingTop mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "paddingTop") retCULong []

-- | Defines the explicit padding value for the height dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setPaddingTop:") retVoid [argCULong (fromIntegral value)]

-- | Defines the explicit padding value for the height dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingBottom mpsGraphPooling2DOpDescriptor  =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "paddingBottom") retCULong []

-- | Defines the explicit padding value for the height dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setPaddingBottom:") retVoid [argCULong (fromIntegral value)]

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphPooling2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPaddingStyle) $ sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "paddingStyle") retCULong []

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setPaddingStyle:") retVoid [argCULong (coerce value)]

-- | Defines the data layout of the input data in the forward pass. See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphPooling2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphTensorNamedDataLayout) $ sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "dataLayout") retCULong []

-- | Defines the data layout of the input data in the forward pass. See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setDataLayout:") retVoid [argCULong (coerce value)]

-- | Defines the mode for returned indices of maximum values within each pooling window. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- returnIndicesMode@
returnIndicesMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSGraphPoolingReturnIndicesMode
returnIndicesMode mpsGraphPooling2DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPoolingReturnIndicesMode) $ sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "returnIndicesMode") retCULong []

-- | Defines the mode for returned indices of maximum values within each pooling window. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- setReturnIndicesMode:@
setReturnIndicesMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSGraphPoolingReturnIndicesMode -> IO ()
setReturnIndicesMode mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setReturnIndicesMode:") retVoid [argCULong (coerce value)]

-- | Defines the data type for returned indices. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- returnIndicesDataType@
returnIndicesDataType :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSDataType
returnIndicesDataType mpsGraphPooling2DOpDescriptor  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "returnIndicesDataType") retCUInt []

-- | Defines the data type for returned indices. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- setReturnIndicesDataType:@
setReturnIndicesDataType :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSDataType -> IO ()
setReturnIndicesDataType mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setReturnIndicesDataType:") retVoid [argCUInt (coerce value)]

-- | Affects how the graph computes the output size.
--
-- if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride. Default value: @NO@.
--
-- ObjC selector: @- ceilMode@
ceilMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO Bool
ceilMode mpsGraphPooling2DOpDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "ceilMode") retCULong []

-- | Affects how the graph computes the output size.
--
-- if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride. Default value: @NO@.
--
-- ObjC selector: @- setCeilMode:@
setCeilMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> Bool -> IO ()
setCeilMode mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setCeilMode:") retVoid [argCULong (if value then 1 else 0)]

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- includeZeroPadToAverage@
includeZeroPadToAverage :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO Bool
includeZeroPadToAverage mpsGraphPooling2DOpDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "includeZeroPadToAverage") retCULong []

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- setIncludeZeroPadToAverage:@
setIncludeZeroPadToAverage :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> Bool -> IO ()
setIncludeZeroPadToAverage mpsGraphPooling2DOpDescriptor  value =
  sendMsg mpsGraphPooling2DOpDescriptor (mkSelector "setIncludeZeroPadToAverage:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector :: Selector
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:"

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector :: Selector
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector
setKernelHeightSelector = mkSelector "setKernelHeight:"

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

-- | @Selector@ for @returnIndicesMode@
returnIndicesModeSelector :: Selector
returnIndicesModeSelector = mkSelector "returnIndicesMode"

-- | @Selector@ for @setReturnIndicesMode:@
setReturnIndicesModeSelector :: Selector
setReturnIndicesModeSelector = mkSelector "setReturnIndicesMode:"

-- | @Selector@ for @returnIndicesDataType@
returnIndicesDataTypeSelector :: Selector
returnIndicesDataTypeSelector = mkSelector "returnIndicesDataType"

-- | @Selector@ for @setReturnIndicesDataType:@
setReturnIndicesDataTypeSelector :: Selector
setReturnIndicesDataTypeSelector = mkSelector "setReturnIndicesDataType:"

-- | @Selector@ for @ceilMode@
ceilModeSelector :: Selector
ceilModeSelector = mkSelector "ceilMode"

-- | @Selector@ for @setCeilMode:@
setCeilModeSelector :: Selector
setCeilModeSelector = mkSelector "setCeilMode:"

-- | @Selector@ for @includeZeroPadToAverage@
includeZeroPadToAverageSelector :: Selector
includeZeroPadToAverageSelector = mkSelector "includeZeroPadToAverage"

-- | @Selector@ for @setIncludeZeroPadToAverage:@
setIncludeZeroPadToAverageSelector :: Selector
setIncludeZeroPadToAverageSelector = mkSelector "setIncludeZeroPadToAverage:"

