{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector
  , descriptorWithKernelSizes_paddingStyleSelector
  , kernelSizesSelector
  , setKernelSizesSelector
  , stridesSelector
  , setStridesSelector
  , dilationRatesSelector
  , setDilationRatesSelector
  , paddingValuesSelector
  , setPaddingValuesSelector
  , paddingStyleSelector
  , setPaddingStyleSelector
  , ceilModeSelector
  , setCeilModeSelector
  , includeZeroPadToAverageSelector
  , setIncludeZeroPadToAverageSelector
  , returnIndicesModeSelector
  , setReturnIndicesModeSelector
  , returnIndicesDataTypeSelector
  , setReturnIndicesDataTypeSelector

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

-- | Creates a 4D pooling descriptor with given values.
--
-- - Parameters:   - kernelSizes: See @kernelSizes@ property.   - strides: See @strides@ property.   - dilationRates: See @dilationRates@ property.   - paddingValues: See @paddingValues@ property.   - paddingStyle: See @paddingStyle@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:@
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyle :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingValues) => kernelSizes -> strides -> dilationRates -> paddingValues -> MPSGraphPaddingStyle -> IO (Id MPSGraphPooling4DOpDescriptor)
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyle kernelSizes strides dilationRates paddingValues paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphPooling4DOpDescriptor"
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr dilationRates $ \raw_dilationRates ->
          withObjCPtr paddingValues $ \raw_paddingValues ->
            sendClassMsg cls' (mkSelector "descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argPtr (castPtr raw_dilationRates :: Ptr ()), argPtr (castPtr raw_paddingValues :: Ptr ()), argCULong (coerce paddingStyle)] >>= retainedObject . castPtr

-- | Creates a 4D pooling descriptor with default values.
--
-- - Parameters:   - kernelSizes: See @kernelSizes@ property.   - paddingStyle: See @paddingStyle@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithKernelSizes:paddingStyle:@
descriptorWithKernelSizes_paddingStyle :: IsNSArray kernelSizes => kernelSizes -> MPSGraphPaddingStyle -> IO (Id MPSGraphPooling4DOpDescriptor)
descriptorWithKernelSizes_paddingStyle kernelSizes paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphPooling4DOpDescriptor"
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      sendClassMsg cls' (mkSelector "descriptorWithKernelSizes:paddingStyle:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argCULong (coerce paddingStyle)] >>= retainedObject . castPtr

-- | Defines the pooling window size.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- ObjC selector: @- kernelSizes@
kernelSizes :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
kernelSizes mpsGraphPooling4DOpDescriptor  =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "kernelSizes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defines the pooling window size.
--
-- Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- ObjC selector: @- setKernelSizes:@
setKernelSizes :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setKernelSizes mpsGraphPooling4DOpDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setKernelSizes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defines strides for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- strides@
strides :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
strides mpsGraphPooling4DOpDescriptor  =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "strides") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defines strides for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setStrides:@
setStrides :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setStrides mpsGraphPooling4DOpDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setStrides:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defines dilation rates for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- dilationRates@
dilationRates :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
dilationRates mpsGraphPooling4DOpDescriptor  =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "dilationRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defines dilation rates for spatial dimensions. Must be four numbers, one for each spatial dimension, fastest running index last.
--
-- Default value: `\@[ \@1, \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setDilationRates:@
setDilationRates :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setDilationRates mpsGraphPooling4DOpDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setDilationRates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defines padding values for spatial dimensions which must be eight numbers, two for each spatial dimension.
--
-- For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- paddingValues@
paddingValues :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO (Id NSArray)
paddingValues mpsGraphPooling4DOpDescriptor  =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "paddingValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defines padding values for spatial dimensions which must be eight numbers, two for each spatial dimension.
--
-- For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions), @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Used only when @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setPaddingValues:@
setPaddingValues :: (IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor, IsNSArray value) => mpsGraphPooling4DOpDescriptor -> value -> IO ()
setPaddingValues mpsGraphPooling4DOpDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setPaddingValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphPooling4DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPaddingStyle) $ sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "paddingStyle") retCULong []

-- | Defines what kind of padding graph applies to the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphPooling4DOpDescriptor  value =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setPaddingStyle:") retVoid [argCULong (coerce value)]

-- | Affects how MPSGraph computes the output size: if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride.
--
-- Default value: @NO@.
--
-- ObjC selector: @- ceilMode@
ceilMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO Bool
ceilMode mpsGraphPooling4DOpDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "ceilMode") retCULong []

-- | Affects how MPSGraph computes the output size: if set to @YES@ then output size is computed by rounding up instead of down when dividing input size by stride.
--
-- Default value: @NO@.
--
-- ObjC selector: @- setCeilMode:@
setCeilMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> Bool -> IO ()
setCeilMode mpsGraphPooling4DOpDescriptor  value =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setCeilMode:") retVoid [argCULong (if value then 1 else 0)]

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- includeZeroPadToAverage@
includeZeroPadToAverage :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO Bool
includeZeroPadToAverage mpsGraphPooling4DOpDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "includeZeroPadToAverage") retCULong []

-- | Defines a mode for average pooling, where samples outside the input tensor count as zeroes in the average computation.
--
-- Otherwise the result is sum over samples divided by number of samples that didn't come from padding. Default value: @NO@.
--
-- ObjC selector: @- setIncludeZeroPadToAverage:@
setIncludeZeroPadToAverage :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> Bool -> IO ()
setIncludeZeroPadToAverage mpsGraphPooling4DOpDescriptor  value =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setIncludeZeroPadToAverage:") retVoid [argCULong (if value then 1 else 0)]

-- | Defines the mode for returned indices of maximum values within each pooling window.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- returnIndicesMode@
returnIndicesMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO MPSGraphPoolingReturnIndicesMode
returnIndicesMode mpsGraphPooling4DOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphPoolingReturnIndicesMode) $ sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "returnIndicesMode") retCULong []

-- | Defines the mode for returned indices of maximum values within each pooling window.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. If @returnIndicesMode = MPSGraphPoolingReturnIndicesNone@ then only the first result MPSGraph returns from ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` will be valid and using the second result will assert. Default value: @MPSGraphPoolingReturnIndicesNone@.
--
-- ObjC selector: @- setReturnIndicesMode:@
setReturnIndicesMode :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> MPSGraphPoolingReturnIndicesMode -> IO ()
setReturnIndicesMode mpsGraphPooling4DOpDescriptor  value =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setReturnIndicesMode:") retVoid [argCULong (coerce value)]

-- | Defines the data type for returned indices.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- returnIndicesDataType@
returnIndicesDataType :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> IO MPSDataType
returnIndicesDataType mpsGraphPooling4DOpDescriptor  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "returnIndicesDataType") retCUInt []

-- | Defines the data type for returned indices.
--
-- Use this in conjunction with ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` API. Currently MPSGraph supports the following datatypes: @MPSDataTypeInt32@. Default value: @MPSDataTypeInt32@.
--
-- ObjC selector: @- setReturnIndicesDataType:@
setReturnIndicesDataType :: IsMPSGraphPooling4DOpDescriptor mpsGraphPooling4DOpDescriptor => mpsGraphPooling4DOpDescriptor -> MPSDataType -> IO ()
setReturnIndicesDataType mpsGraphPooling4DOpDescriptor  value =
  sendMsg mpsGraphPooling4DOpDescriptor (mkSelector "setReturnIndicesDataType:") retVoid [argCUInt (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:@
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector :: Selector
descriptorWithKernelSizes_strides_dilationRates_paddingValues_paddingStyleSelector = mkSelector "descriptorWithKernelSizes:strides:dilationRates:paddingValues:paddingStyle:"

-- | @Selector@ for @descriptorWithKernelSizes:paddingStyle:@
descriptorWithKernelSizes_paddingStyleSelector :: Selector
descriptorWithKernelSizes_paddingStyleSelector = mkSelector "descriptorWithKernelSizes:paddingStyle:"

-- | @Selector@ for @kernelSizes@
kernelSizesSelector :: Selector
kernelSizesSelector = mkSelector "kernelSizes"

-- | @Selector@ for @setKernelSizes:@
setKernelSizesSelector :: Selector
setKernelSizesSelector = mkSelector "setKernelSizes:"

-- | @Selector@ for @strides@
stridesSelector :: Selector
stridesSelector = mkSelector "strides"

-- | @Selector@ for @setStrides:@
setStridesSelector :: Selector
setStridesSelector = mkSelector "setStrides:"

-- | @Selector@ for @dilationRates@
dilationRatesSelector :: Selector
dilationRatesSelector = mkSelector "dilationRates"

-- | @Selector@ for @setDilationRates:@
setDilationRatesSelector :: Selector
setDilationRatesSelector = mkSelector "setDilationRates:"

-- | @Selector@ for @paddingValues@
paddingValuesSelector :: Selector
paddingValuesSelector = mkSelector "paddingValues"

-- | @Selector@ for @setPaddingValues:@
setPaddingValuesSelector :: Selector
setPaddingValuesSelector = mkSelector "setPaddingValues:"

-- | @Selector@ for @paddingStyle@
paddingStyleSelector :: Selector
paddingStyleSelector = mkSelector "paddingStyle"

-- | @Selector@ for @setPaddingStyle:@
setPaddingStyleSelector :: Selector
setPaddingStyleSelector = mkSelector "setPaddingStyle:"

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

