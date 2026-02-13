{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , ceilModeSelector
  , dataLayoutSelector
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector
  , descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , includeZeroPadToAverageSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , paddingBottomSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingStyleSelector
  , paddingTopSelector
  , returnIndicesDataTypeSelector
  , returnIndicesModeSelector
  , setCeilModeSelector
  , setDataLayoutSelector
  , setDilationRateInXSelector
  , setDilationRateInYSelector
  , setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector
  , setIncludeZeroPadToAverageSelector
  , setKernelHeightSelector
  , setKernelWidthSelector
  , setPaddingBottomSelector
  , setPaddingLeftSelector
  , setPaddingRightSelector
  , setPaddingStyleSelector
  , setPaddingTopSelector
  , setReturnIndicesDataTypeSelector
  , setReturnIndicesModeSelector
  , setStrideInXSelector
  , setStrideInYSelector
  , strideInXSelector
  , strideInYSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector kernelWidth kernelHeight strideInX strideInY dilationRateInX dilationRateInY paddingLeft paddingRight paddingTop paddingBottom paddingStyle dataLayout

-- | Creates a 2D pooling descriptor with given values.
--
-- - Parameters:   - kernelWidth: See @kernelWidth@ property.   - kernelHeight: See @kernelHeight@` property.   - strideInX: See @strideInX@ property.   - strideInY: See @strideInY@ property.   - paddingStyle: See @paddingStyle@ property.   - dataLayout: See @dataLayout@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayout :: CULong -> CULong -> CULong -> CULong -> MPSGraphPaddingStyle -> MPSGraphTensorNamedDataLayout -> IO (Id MPSGraphPooling2DOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayout kernelWidth kernelHeight strideInX strideInY paddingStyle dataLayout =
  do
    cls' <- getRequiredClass "MPSGraphPooling2DOpDescriptor"
    sendClassMessage cls' descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector kernelWidth kernelHeight strideInX strideInY paddingStyle dataLayout

-- | Sets the explicit padding values and sets padding style to explicit.
--
-- - Parameters:   - paddingLeft: See @paddingLeft@ property.   - paddingRight: See @paddingRight@ property.   - paddingTop: See @paddingTop@ property.   - paddingBottom: See @paddingBottom@ property.
--
-- ObjC selector: @- setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> CULong -> CULong -> CULong -> IO ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottom mpsGraphPooling2DOpDescriptor paddingLeft paddingRight paddingTop paddingBottom =
  sendMessage mpsGraphPooling2DOpDescriptor setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector paddingLeft paddingRight paddingTop paddingBottom

-- | Defines the pooling window size for the width dimension.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
kernelWidth mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor kernelWidthSelector

-- | Defines the pooling window size for the width dimension.
--
-- ObjC selector: @- setKernelWidth:@
setKernelWidth :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setKernelWidth mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setKernelWidthSelector value

-- | Defines the pooling window size for the height dimension.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
kernelHeight mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor kernelHeightSelector

-- | Defines the pooling window size for the height dimension.
--
-- ObjC selector: @- setKernelHeight:@
setKernelHeight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setKernelHeight mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setKernelHeightSelector value

-- | Defines the stride for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
strideInX mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor strideInXSelector

-- | Defines the stride for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setStrideInX mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setStrideInXSelector value

-- | Defines the stride for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
strideInY mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor strideInYSelector

-- | Defines the stride for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setStrideInY mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setStrideInYSelector value

-- | Defines the dilation rate for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
dilationRateInX mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor dilationRateInXSelector

-- | Defines the dilation rate for the width dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setDilationRateInX:@
setDilationRateInX :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setDilationRateInX mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setDilationRateInXSelector value

-- | Defines the dilation rate for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
dilationRateInY mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor dilationRateInYSelector

-- | Defines the dilation rate for the height dimension.
--
-- Default value: 1.
--
-- ObjC selector: @- setDilationRateInY:@
setDilationRateInY :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setDilationRateInY mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setDilationRateInYSelector value

-- | Defines the explicit padding value for the width dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingLeft mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor paddingLeftSelector

-- | Defines the explicit padding value for the width dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingLeft:@
setPaddingLeft :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingLeft mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setPaddingLeftSelector value

-- | Defines the explicit padding value for the width dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingRight mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor paddingRightSelector

-- | Defines the explicit padding value for the width dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingRight:@
setPaddingRight :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingRight mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setPaddingRightSelector value

-- | Defines the explicit padding value for the height dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingTop mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor paddingTopSelector

-- | Defines the explicit padding value for the height dimension to add before the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingTop:@
setPaddingTop :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingTop mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setPaddingTopSelector value

-- | Defines the explicit padding value for the height dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO CULong
paddingBottom mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor paddingBottomSelector

-- | Defines the explicit padding value for the height dimension to add after the data.
--
-- Default value: 0.
--
-- ObjC selector: @- setPaddingBottom:@
setPaddingBottom :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> CULong -> IO ()
setPaddingBottom mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setPaddingBottomSelector value

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor paddingStyleSelector

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setPaddingStyleSelector value

-- | Defines the data layout of the input data in the forward pass. See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSGraphTensorNamedDataLayout
dataLayout mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor dataLayoutSelector

-- | Defines the data layout of the input data in the forward pass. See: ``MPSGraphTensorNamedDataLayout``.
--
-- ObjC selector: @- setDataLayout:@
setDataLayout :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSGraphTensorNamedDataLayout -> IO ()
setDataLayout mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setDataLayoutSelector value

-- | Defines the mode for returned indices of maximum values within each pooling window. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- returnIndicesMode@
returnIndicesMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSGraphPoolingReturnIndicesMode
returnIndicesMode mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor returnIndicesModeSelector

-- | Defines the mode for returned indices of maximum values within each pooling window. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- setReturnIndicesMode:@
setReturnIndicesMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSGraphPoolingReturnIndicesMode -> IO ()
setReturnIndicesMode mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setReturnIndicesModeSelector value

-- | Defines the data type for returned indices. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- returnIndicesDataType@
returnIndicesDataType :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO MPSDataType
returnIndicesDataType mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor returnIndicesDataTypeSelector

-- | Defines the data type for returned indices. Use this in conjunction with ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- setReturnIndicesDataType:@
setReturnIndicesDataType :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> MPSDataType -> IO ()
setReturnIndicesDataType mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setReturnIndicesDataTypeSelector value

-- | Affects how the graph computes the output size.
--
-- if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride. Default value: @NO@.
--
-- ObjC selector: @- ceilMode@
ceilMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO Bool
ceilMode mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor ceilModeSelector

-- | Affects how the graph computes the output size.
--
-- if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride. Default value: @NO@.
--
-- ObjC selector: @- setCeilMode:@
setCeilMode :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> Bool -> IO ()
setCeilMode mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setCeilModeSelector value

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- includeZeroPadToAverage@
includeZeroPadToAverage :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> IO Bool
includeZeroPadToAverage mpsGraphPooling2DOpDescriptor =
  sendMessage mpsGraphPooling2DOpDescriptor includeZeroPadToAverageSelector

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- setIncludeZeroPadToAverage:@
setIncludeZeroPadToAverage :: IsMPSGraphPooling2DOpDescriptor mpsGraphPooling2DOpDescriptor => mpsGraphPooling2DOpDescriptor -> Bool -> IO ()
setIncludeZeroPadToAverage mpsGraphPooling2DOpDescriptor value =
  sendMessage mpsGraphPooling2DOpDescriptor setIncludeZeroPadToAverageSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, CULong, MPSGraphPaddingStyle, MPSGraphTensorNamedDataLayout] (Id MPSGraphPooling2DOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_dilationRateInX_dilationRateInY_paddingLeft_paddingRight_paddingTop_paddingBottom_paddingStyle_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:dilationRateInX:dilationRateInY:paddingLeft:paddingRight:paddingTop:paddingBottom:paddingStyle:dataLayout:"

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:@
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector :: Selector '[CULong, CULong, CULong, CULong, MPSGraphPaddingStyle, MPSGraphTensorNamedDataLayout] (Id MPSGraphPooling2DOpDescriptor)
descriptorWithKernelWidth_kernelHeight_strideInX_strideInY_paddingStyle_dataLayoutSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:strideInX:strideInY:paddingStyle:dataLayout:"

-- | @Selector@ for @setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:@
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector :: Selector '[CULong, CULong, CULong, CULong] ()
setExplicitPaddingWithPaddingLeft_paddingRight_paddingTop_paddingBottomSelector = mkSelector "setExplicitPaddingWithPaddingLeft:paddingRight:paddingTop:paddingBottom:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector '[CULong] ()
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector '[CULong] ()
setKernelHeightSelector = mkSelector "setKernelHeight:"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector '[] CULong
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @setStrideInX:@
setStrideInXSelector :: Selector '[CULong] ()
setStrideInXSelector = mkSelector "setStrideInX:"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector '[] CULong
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @setStrideInY:@
setStrideInYSelector :: Selector '[CULong] ()
setStrideInYSelector = mkSelector "setStrideInY:"

-- | @Selector@ for @dilationRateInX@
dilationRateInXSelector :: Selector '[] CULong
dilationRateInXSelector = mkSelector "dilationRateInX"

-- | @Selector@ for @setDilationRateInX:@
setDilationRateInXSelector :: Selector '[CULong] ()
setDilationRateInXSelector = mkSelector "setDilationRateInX:"

-- | @Selector@ for @dilationRateInY@
dilationRateInYSelector :: Selector '[] CULong
dilationRateInYSelector = mkSelector "dilationRateInY"

-- | @Selector@ for @setDilationRateInY:@
setDilationRateInYSelector :: Selector '[CULong] ()
setDilationRateInYSelector = mkSelector "setDilationRateInY:"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector '[] CULong
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @setPaddingLeft:@
setPaddingLeftSelector :: Selector '[CULong] ()
setPaddingLeftSelector = mkSelector "setPaddingLeft:"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector '[] CULong
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @setPaddingRight:@
setPaddingRightSelector :: Selector '[CULong] ()
setPaddingRightSelector = mkSelector "setPaddingRight:"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector '[] CULong
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @setPaddingTop:@
setPaddingTopSelector :: Selector '[CULong] ()
setPaddingTopSelector = mkSelector "setPaddingTop:"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector '[] CULong
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @setPaddingBottom:@
setPaddingBottomSelector :: Selector '[CULong] ()
setPaddingBottomSelector = mkSelector "setPaddingBottom:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector '[] MPSGraphPaddingStyle
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector '[MPSGraphPaddingStyle] ()
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector '[] MPSGraphTensorNamedDataLayout
dataLayoutSelector = mkSelector "dataLayout"

-- | @Selector@ for @setDataLayout:@
setDataLayoutSelector :: Selector '[MPSGraphTensorNamedDataLayout] ()
setDataLayoutSelector = mkSelector "setDataLayout:"

-- | @Selector@ for @returnIndicesMode@
returnIndicesModeSelector :: Selector '[] MPSGraphPoolingReturnIndicesMode
returnIndicesModeSelector = mkSelector "returnIndicesMode"

-- | @Selector@ for @setReturnIndicesMode:@
setReturnIndicesModeSelector :: Selector '[MPSGraphPoolingReturnIndicesMode] ()
setReturnIndicesModeSelector = mkSelector "setReturnIndicesMode:"

-- | @Selector@ for @returnIndicesDataType@
returnIndicesDataTypeSelector :: Selector '[] MPSDataType
returnIndicesDataTypeSelector = mkSelector "returnIndicesDataType"

-- | @Selector@ for @setReturnIndicesDataType:@
setReturnIndicesDataTypeSelector :: Selector '[MPSDataType] ()
setReturnIndicesDataTypeSelector = mkSelector "setReturnIndicesDataType:"

-- | @Selector@ for @ceilMode@
ceilModeSelector :: Selector '[] Bool
ceilModeSelector = mkSelector "ceilMode"

-- | @Selector@ for @setCeilMode:@
setCeilModeSelector :: Selector '[Bool] ()
setCeilModeSelector = mkSelector "setCeilMode:"

-- | @Selector@ for @includeZeroPadToAverage@
includeZeroPadToAverageSelector :: Selector '[] Bool
includeZeroPadToAverageSelector = mkSelector "includeZeroPadToAverage"

-- | @Selector@ for @setIncludeZeroPadToAverage:@
setIncludeZeroPadToAverageSelector :: Selector '[Bool] ()
setIncludeZeroPadToAverageSelector = mkSelector "setIncludeZeroPadToAverage:"

