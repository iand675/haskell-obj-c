{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a 4D pooling operation.
--
-- Use this descriptor with the following methods: - ``MPSGraph/maxPooling4DWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling4DGradientWithGradientTensor:sourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling4DGradientWithGradientTensor:indicesTensor:outputShape:descriptor:name:`` - ``MPSGraph/maxPooling4DGradientWithGradientTensor:indicesTensor:outputShapeTensor:descriptor:name:`` - ``MPSGraph/avgPooling4DWithSourceTensor:descriptor:name:`` - ``MPSGraph/avgPooling4DGradientWithGradientTensor:sourceTensor:descriptor:name:`` - ``MPSGraph/L2NormPooling4DWithSourceTensor:descriptor:name:`` - ``MPSGraph/L2NormPooling4DGradientWithGradientTensor:sourceTensor:descriptor:name:``
--
-- Generated bindings for @MPSGraphPooling4DOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphPooling4DOpDescriptor
  ( MPSGraphPooling4DOpDescriptor
  , IsMPSGraphPooling4DOpDescriptor(..)
  , descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyle
  , descriptorWithKernelSizes_paddingStyle
  , kernelSizes
  , setKernelSizes
  , strides
  , setStrides
  , dilationRates
  , setDilationRates
  , paddingValues
  , setPaddingValues
  , paddingStyle
  , setPaddingStyle
  , ceilMode
  , setCeilMode
  , includeZeroPadToAverage
  , setIncludeZeroPadToAverage
  , returnIndicesMode
  , setReturnIndicesMode
  , returnIndicesDataType
  , setReturnIndicesDataType
  , ceilModeSelector
  , descriptorWithKernelSizes_paddingStyleSelector
  , descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector
  , dilationRatesSelector
  , includeZeroPadToAverageSelector
  , kernelSizesSelector
  , paddingStyleSelector
  , paddingValuesSelector
  , returnIndicesDataTypeSelector
  , returnIndicesModeSelector
  , setCeilModeSelector
  , setDilationRatesSelector
  , setIncludeZeroPadToAverageSelector
  , setKernelSizesSelector
  , setPaddingStyleSelector
  , setPaddingValuesSelector
  , setReturnIndicesDataTypeSelector
  , setReturnIndicesModeSelector
  , setStridesSelector
  , stridesSelector

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

-- | Creates a 4D pooling descriptor with given values.
--
-- - Parameters:   - kernelSizes: See @kernelSizes@ property.   - strides: See @strides@ property.   - dilationRates: See @dilationRates@ property.   - paddingValues: See @paddingValues@ property.   - paddingStyle: See @paddingStyle@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:@
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyle :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingValues) => kernelSizes -> strides -> dilationRates -> paddingValues -> MPSGraphPaddingStyle -> IO (Id MPSGraphPooling4DOpDescriptor)
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyle kernelSizes strides dilationRates paddingValues paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphPooling4DOpDescriptor"
    sendClassMessage cls' descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector (toNSArray kernelSizes) (toNSArray strides) (toNSArray dilationRates) (toNSArray paddingValues) paddingStyle

-- | Creates a 4D pooling descriptor with default values.
--
-- - Parameters:   - kernelSizes: See @kernelSizes@ property.   - paddingStyle: See @paddingStyle@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelSizes:paddingStyle:@
descriptorWithKernelSizes_paddingStyle :: IsNSArray kernelSizes => kernelSizes -> MPSGraphPaddingStyle -> IO (Id MPSGraphPooling4DOpDescriptor)
descriptorWithKernelSizes_paddingStyle kernelSizes paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphPooling4DOpDescriptor"
    sendClassMessage cls' descriptorWithKernelSizes_paddingStyleSelector (toNSArray kernelSizes) paddingStyle

-- | Defines the pooling window size.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- ObjC selector: @- kernelSizes@
kernelSizes :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
kernelSizes mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor kernelSizesSelector

-- | Defines the pooling window size.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- ObjC selector: @- setKernelSizes:@
setKernelSizes :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setKernelSizes mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setKernelSizesSelector (toNSArray value)

-- | Defines strides for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- strides@
strides :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
strides mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor stridesSelector

-- | Defines strides for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setStrides:@
setStrides :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setStrides mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setStridesSelector (toNSArray value)

-- | Defines dilation rates for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- dilationRates@
dilationRates :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
dilationRates mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor dilationRatesSelector

-- | Defines dilation rates for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setDilationRates:@
setDilationRates :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setDilationRates mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setDilationRatesSelector (toNSArray value)

-- | Defines padding values for spatial dimensions which must be eight numbers, two for each spatial dimension.
--
-- For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- paddingValues@
paddingValues :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
paddingValues mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor paddingValuesSelector

-- | Defines padding values for spatial dimensions which must be eight numbers, two for each spatial dimension.
--
-- For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setPaddingValues:@
setPaddingValues :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setPaddingValues mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setPaddingValuesSelector (toNSArray value)

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor paddingStyleSelector

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setPaddingStyleSelector value

-- | Affects how MPSGraph computes the output size: if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride.
--
-- Default value: @NO@.
--
-- ObjC selector: @- ceilMode@
ceilMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO Bool
ceilMode mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor ceilModeSelector

-- | Affects how MPSGraph computes the output size: if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride.
--
-- Default value: @NO@.
--
-- ObjC selector: @- setCeilMode:@
setCeilMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> Bool -> IO ()
setCeilMode mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setCeilModeSelector value

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- includeZeroPadToAverage@
includeZeroPadToAverage :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO Bool
includeZeroPadToAverage mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor includeZeroPadToAverageSelector

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- setIncludeZeroPadToAverage:@
setIncludeZeroPadToAverage :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> Bool -> IO ()
setIncludeZeroPadToAverage mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setIncludeZeroPadToAverageSelector value

-- | Defines the mode for returned indices of maximum values within each pooling window.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- returnIndicesMode@
returnIndicesMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO MPSGraphPoolingReturnIndicesMode
returnIndicesMode mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor returnIndicesModeSelector

-- | Defines the mode for returned indices of maximum values within each pooling window.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- setReturnIndicesMode:@
setReturnIndicesMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> MPSGraphPoolingReturnIndicesMode -> IO ()
setReturnIndicesMode mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setReturnIndicesModeSelector value

-- | Defines the data type for returned indices.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- returnIndicesDataType@
returnIndicesDataType :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO MPSDataType
returnIndicesDataType mpsGraphPooling4DOpDescriptor =
  sendMessage mpsGraphPooling4DOpDescriptor returnIndicesDataTypeSelector

-- | Defines the data type for returned indices.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- setReturnIndicesDataType:@
setReturnIndicesDataType :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> MPSDataType -> IO ()
setReturnIndicesDataType mpsGraphPooling4DOpDescriptor value =
  sendMessage mpsGraphPooling4DOpDescriptor setReturnIndicesDataTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:@
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, Id NSArray, MPSGraphPaddingStyle] (Id MPSGraphPooling4DOpDescriptor)
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector = mkSelector "descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:"

-- | @Selector@ for @descriptorWithKernelSizes:paddingStyle:@
descriptorWithKernelSizes_paddingStyleSelector :: Selector '[Id NSArray, MPSGraphPaddingStyle] (Id MPSGraphPooling4DOpDescriptor)
descriptorWithKernelSizes_paddingStyleSelector = mkSelector "descriptorWithKernelSizes:paddingStyle:"

-- | @Selector@ for @kernelSizes@
kernelSizesSelector :: Selector '[] (Id NSArray)
kernelSizesSelector = mkSelector "kernelSizes"

-- | @Selector@ for @setKernelSizes:@
setKernelSizesSelector :: Selector '[Id NSArray] ()
setKernelSizesSelector = mkSelector "setKernelSizes:"

-- | @Selector@ for @strides@
stridesSelector :: Selector '[] (Id NSArray)
stridesSelector = mkSelector "strides"

-- | @Selector@ for @setStrides:@
setStridesSelector :: Selector '[Id NSArray] ()
setStridesSelector = mkSelector "setStrides:"

-- | @Selector@ for @dilationRates@
dilationRatesSelector :: Selector '[] (Id NSArray)
dilationRatesSelector = mkSelector "dilationRates"

-- | @Selector@ for @setDilationRates:@
setDilationRatesSelector :: Selector '[Id NSArray] ()
setDilationRatesSelector = mkSelector "setDilationRates:"

-- | @Selector@ for @paddingValues@
paddingValuesSelector :: Selector '[] (Id NSArray)
paddingValuesSelector = mkSelector "paddingValues"

-- | @Selector@ for @setPaddingValues:@
setPaddingValuesSelector :: Selector '[Id NSArray] ()
setPaddingValuesSelector = mkSelector "setPaddingValues:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector '[] MPSGraphPaddingStyle
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector '[MPSGraphPaddingStyle] ()
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

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

