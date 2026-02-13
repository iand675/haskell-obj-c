{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a 3D-depthwise convolution operation.
--
-- A @MPSGraphDepthwiseConvolution3DOpDescriptor@ defines constant parameters for 3D depthwise convolutions. Use this class with ``MPSGraph/depthwiseConvolution3DWithSourceTensor:weightsTensor:descriptor:name:``, ``MPSGraph/depthwiseConvolution3DDataGradientWithIncomingGradientTensor:weightsTensor:outputShape:descriptor:name:`` and ``MPSGraph/depthwiseConvolution3DWeightsGradientWithIncomingGradientTensor:sourceTensor:outputShape:descriptor:name:`` methods.
--
-- Generated bindings for @MPSGraphDepthwiseConvolution3DOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphDepthwiseConvolution3DOpDescriptor
  ( MPSGraphDepthwiseConvolution3DOpDescriptor
  , IsMPSGraphDepthwiseConvolution3DOpDescriptor(..)
  , descriptorWithStrides_dilationRates_paddingValues_paddingStyle
  , descriptorWithPaddingStyle
  , strides
  , setStrides
  , dilationRates
  , setDilationRates
  , paddingValues
  , setPaddingValues
  , paddingStyle
  , setPaddingStyle
  , channelDimensionIndex
  , setChannelDimensionIndex
  , channelDimensionIndexSelector
  , descriptorWithPaddingStyleSelector
  , descriptorWithStrides_dilationRates_paddingValues_paddingStyleSelector
  , dilationRatesSelector
  , paddingStyleSelector
  , paddingValuesSelector
  , setChannelDimensionIndexSelector
  , setDilationRatesSelector
  , setPaddingStyleSelector
  , setPaddingValuesSelector
  , setStridesSelector
  , stridesSelector

  -- * Enum types
  , MPSGraphPaddingStyle(MPSGraphPaddingStyle)
  , pattern MPSGraphPaddingStyleExplicit
  , pattern MPSGraphPaddingStyleTF_VALID
  , pattern MPSGraphPaddingStyleTF_SAME
  , pattern MPSGraphPaddingStyleExplicitOffset
  , pattern MPSGraphPaddingStyleONNX_SAME_LOWER

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a 3D depthwise convolution descriptor with given values.
--
-- - Parameters:   - strides: See @strides@ property.   - dilationRates: See @dilationRates@ property.   - paddingValues: See @paddingValues@ property.   - paddingStyle: See @paddingStyle@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithStrides:dilationRates:paddingValues:paddingStyle:@
descriptorWithStrides_dilationRates_paddingValues_paddingStyle :: (IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingValues) => strides -> dilationRates -> paddingValues -> MPSGraphPaddingStyle -> IO (Id MPSGraphDepthwiseConvolution3DOpDescriptor)
descriptorWithStrides_dilationRates_paddingValues_paddingStyle strides dilationRates paddingValues paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphDepthwiseConvolution3DOpDescriptor"
    sendClassMessage cls' descriptorWithStrides_dilationRates_paddingValues_paddingStyleSelector (toNSArray strides) (toNSArray dilationRates) (toNSArray paddingValues) paddingStyle

-- | Creates a 3D depthwise convolution descriptor with default values.
--
-- - Parameters:   - paddingStyle: See @paddingStyle@ property. - Returns: The descriptor on autoreleasepool.
--
-- ObjC selector: @+ descriptorWithPaddingStyle:@
descriptorWithPaddingStyle :: MPSGraphPaddingStyle -> IO (Id MPSGraphDepthwiseConvolution3DOpDescriptor)
descriptorWithPaddingStyle paddingStyle =
  do
    cls' <- getRequiredClass "MPSGraphDepthwiseConvolution3DOpDescriptor"
    sendClassMessage cls' descriptorWithPaddingStyleSelector paddingStyle

-- | The strides for spatial dimensions.
--
-- Must be three numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1 ]`
--
-- ObjC selector: @- strides@
strides :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> IO (Id NSArray)
strides mpsGraphDepthwiseConvolution3DOpDescriptor =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor stridesSelector

-- | The strides for spatial dimensions.
--
-- Must be three numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setStrides:@
setStrides :: (IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor, IsNSArray value) => mpsGraphDepthwiseConvolution3DOpDescriptor -> value -> IO ()
setStrides mpsGraphDepthwiseConvolution3DOpDescriptor value =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor setStridesSelector (toNSArray value)

-- | The dilation rates for spatial dimensions.
--
-- Must be three numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1 ]`
--
-- ObjC selector: @- dilationRates@
dilationRates :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> IO (Id NSArray)
dilationRates mpsGraphDepthwiseConvolution3DOpDescriptor =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor dilationRatesSelector

-- | The dilation rates for spatial dimensions.
--
-- Must be three numbers, one for each spatial dimension, fastest running index last. Default value: `\@[ \@1, \@1, \@1 ]`
--
-- ObjC selector: @- setDilationRates:@
setDilationRates :: (IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor, IsNSArray value) => mpsGraphDepthwiseConvolution3DOpDescriptor -> value -> IO ()
setDilationRates mpsGraphDepthwiseConvolution3DOpDescriptor value =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor setDilationRatesSelector (toNSArray value)

-- | The padding values for spatial dimensions.
--
-- Must be six numbers, two for each spatial dimension. For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions),  @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Use only with @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- paddingValues@
paddingValues :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> IO (Id NSArray)
paddingValues mpsGraphDepthwiseConvolution3DOpDescriptor =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor paddingValuesSelector

-- | The padding values for spatial dimensions.
--
-- Must be six numbers, two for each spatial dimension. For example @paddingValues[0]@ defines the explicit padding amount before the first spatial dimension (slowest running index of spatial dimensions),  @paddingValues[1]@ defines the padding amount after the first spatial dimension etc. Use only with @paddingStyle = MPSGraphPaddingStyleExplicit@. Default value: `\@[ \@0, \@0, \@0, \@0, \@0, \@0 ]`
--
-- ObjC selector: @- setPaddingValues:@
setPaddingValues :: (IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor, IsNSArray value) => mpsGraphDepthwiseConvolution3DOpDescriptor -> value -> IO ()
setPaddingValues mpsGraphDepthwiseConvolution3DOpDescriptor value =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor setPaddingValuesSelector (toNSArray value)

-- | The padding style for the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- paddingStyle@
paddingStyle :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> IO MPSGraphPaddingStyle
paddingStyle mpsGraphDepthwiseConvolution3DOpDescriptor =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor paddingStyleSelector

-- | The padding style for the operation.
--
-- Default value: @MPSGraphPaddingStyleExplicit@.
--
-- ObjC selector: @- setPaddingStyle:@
setPaddingStyle :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> MPSGraphPaddingStyle -> IO ()
setPaddingStyle mpsGraphDepthwiseConvolution3DOpDescriptor value =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor setPaddingStyleSelector value

-- | The axis that contains the channels in the input and the weights, within the 4D tile of the last dimensions.
--
-- For example the value of @-1@ corresponds to @NDHWC@, @NHWC@ layouts. This allows the placement of the channel index anywhere within the last 4 dimensions of the tensor. In case your weights are in a different layout you can bring them to the same layout as inputs using transposes or permutations. Default value: @-4@, corresponds to @NCDHW@ and @CDHW@ layouts.
--
-- ObjC selector: @- channelDimensionIndex@
channelDimensionIndex :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> IO CLong
channelDimensionIndex mpsGraphDepthwiseConvolution3DOpDescriptor =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor channelDimensionIndexSelector

-- | The axis that contains the channels in the input and the weights, within the 4D tile of the last dimensions.
--
-- For example the value of @-1@ corresponds to @NDHWC@, @NHWC@ layouts. This allows the placement of the channel index anywhere within the last 4 dimensions of the tensor. In case your weights are in a different layout you can bring them to the same layout as inputs using transposes or permutations. Default value: @-4@, corresponds to @NCDHW@ and @CDHW@ layouts.
--
-- ObjC selector: @- setChannelDimensionIndex:@
setChannelDimensionIndex :: IsMPSGraphDepthwiseConvolution3DOpDescriptor mpsGraphDepthwiseConvolution3DOpDescriptor => mpsGraphDepthwiseConvolution3DOpDescriptor -> CLong -> IO ()
setChannelDimensionIndex mpsGraphDepthwiseConvolution3DOpDescriptor value =
  sendMessage mpsGraphDepthwiseConvolution3DOpDescriptor setChannelDimensionIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStrides:dilationRates:paddingValues:paddingStyle:@
descriptorWithStrides_dilationRates_paddingValues_paddingStyleSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, MPSGraphPaddingStyle] (Id MPSGraphDepthwiseConvolution3DOpDescriptor)
descriptorWithStrides_dilationRates_paddingValues_paddingStyleSelector = mkSelector "descriptorWithStrides:dilationRates:paddingValues:paddingStyle:"

-- | @Selector@ for @descriptorWithPaddingStyle:@
descriptorWithPaddingStyleSelector :: Selector '[MPSGraphPaddingStyle] (Id MPSGraphDepthwiseConvolution3DOpDescriptor)
descriptorWithPaddingStyleSelector = mkSelector "descriptorWithPaddingStyle:"

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

-- | @Selector@ for @channelDimensionIndex@
channelDimensionIndexSelector :: Selector '[] CLong
channelDimensionIndexSelector = mkSelector "channelDimensionIndex"

-- | @Selector@ for @setChannelDimensionIndex:@
setChannelDimensionIndexSelector :: Selector '[CLong] ()
setChannelDimensionIndexSelector = mkSelector "setChannelDimensionIndex:"

