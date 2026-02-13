{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCConvolutionDescriptor
--
-- The MLCConvolutionDescriptor specifies a convolution descriptor
--
-- Generated bindings for @MLCConvolutionDescriptor@.
module ObjC.MLCompute.MLCConvolutionDescriptor
  ( MLCConvolutionDescriptor
  , IsMLCConvolutionDescriptor(..)
  , descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes
  , descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCount
  , descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizes
  , descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes
  , convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCount
  , convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizes
  , convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes
  , depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplier
  , depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizes
  , depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizes
  , convolutionType
  , kernelWidth
  , kernelHeight
  , inputFeatureChannelCount
  , outputFeatureChannelCount
  , strideInX
  , strideInY
  , dilationRateInX
  , dilationRateInY
  , groupCount
  , paddingPolicy
  , paddingSizeInX
  , paddingSizeInY
  , isConvolutionTranspose
  , usesDepthwiseConvolution
  , convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector
  , convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector
  , convolutionTypeSelector
  , depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizesSelector
  , depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplierSelector
  , descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector
  , descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector
  , descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , groupCountSelector
  , inputFeatureChannelCountSelector
  , isConvolutionTransposeSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , outputFeatureChannelCountSelector
  , paddingPolicySelector
  , paddingSizeInXSelector
  , paddingSizeInYSelector
  , strideInXSelector
  , strideInYSelector
  , usesDepthwiseConvolutionSelector

  -- * Enum types
  , MLCConvolutionType(MLCConvolutionType)
  , pattern MLCConvolutionTypeStandard
  , pattern MLCConvolutionTypeTransposed
  , pattern MLCConvolutionTypeDepthwise
  , MLCPaddingPolicy(MLCPaddingPolicy)
  , pattern MLCPaddingPolicySame
  , pattern MLCPaddingPolicyValid
  , pattern MLCPaddingPolicyUsePaddingSize

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a convolution descriptor with the specified convolution type.
--
-- @convolutionType@ — The type of convolution.
--
-- @kernelSizes@ — The kernel sizes in x and y.
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor.
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor. When the convolution type is @MLCConvolutionTypeDepthwise@ , this value must be a multiple of @inputFeatureChannelCount@ .
--
-- @groupCount@ — The number of groups.
--
-- @strides@ — The kernel strides in x and y.
--
-- @dilationRates@ — The dilation rates in x and y.
--
-- @paddingPolicy@ — The padding policy.
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is @MLCPaddingPolicyUsePaddingSize@ .
--
-- Returns: A new convolution descriptor.
--
-- ObjC selector: @+ descriptorWithType:kernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:@
descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => MLCConvolutionType -> kernelSizes -> CULong -> CULong -> CULong -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes convolutionType kernelSizes inputFeatureChannelCount outputFeatureChannelCount groupCount strides dilationRates paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector convolutionType (toNSArray kernelSizes) inputFeatureChannelCount outputFeatureChannelCount groupCount (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCConvolutionDescriptor object
--
-- @kernelWidth@ — The kernel size in x
--
-- @kernelHeight@ — The kernel size in x
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ descriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:outputFeatureChannelCount:@
descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCount :: CULong -> CULong -> CULong -> CULong -> IO (Id MLCConvolutionDescriptor)
descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCount kernelWidth kernelHeight inputFeatureChannelCount outputFeatureChannelCount =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector kernelWidth kernelHeight inputFeatureChannelCount outputFeatureChannelCount

-- | Create a MLCConvolutionDescriptor object
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor
--
-- @strides@ — The kernel strides in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ descriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:strides:paddingPolicy:paddingSizes:@
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray paddingSizes) => kernelSizes -> CULong -> CULong -> strides -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizes kernelSizes inputFeatureChannelCount outputFeatureChannelCount strides paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) inputFeatureChannelCount outputFeatureChannelCount (toNSArray strides) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCConvolutionDescriptor object
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor
--
-- @groupCount@ — Number of groups
--
-- @strides@ — The kernel strides in x and y
--
-- @dilationRates@ — The dilation rates in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ descriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:@
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => kernelSizes -> CULong -> CULong -> CULong -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes kernelSizes inputFeatureChannelCount outputFeatureChannelCount groupCount strides dilationRates paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) inputFeatureChannelCount outputFeatureChannelCount groupCount (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCConvolutionDescriptor object for convolution transpose
--
-- @kernelWidth@ — The kernel size in x
--
-- @kernelHeight@ — The kernel size in x
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ convolutionTransposeDescriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:outputFeatureChannelCount:@
convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCount :: CULong -> CULong -> CULong -> CULong -> IO (Id MLCConvolutionDescriptor)
convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCount kernelWidth kernelHeight inputFeatureChannelCount outputFeatureChannelCount =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector kernelWidth kernelHeight inputFeatureChannelCount outputFeatureChannelCount

-- | Create a MLCConvolutionDescriptor object for convolution transpose
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor
--
-- @strides@ — The kernel strides in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ convolutionTransposeDescriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:strides:paddingPolicy:paddingSizes:@
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray paddingSizes) => kernelSizes -> CULong -> CULong -> strides -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizes kernelSizes inputFeatureChannelCount outputFeatureChannelCount strides paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) inputFeatureChannelCount outputFeatureChannelCount (toNSArray strides) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCConvolutionDescriptor object for convolution transpose
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @outputFeatureChannelCount@ — The number of feature channels in the output tensor
--
-- @groupCount@ — Number of groups
--
-- @strides@ — The kernel strides in x and y
--
-- @dilationRates@ — The dilation rates in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ convolutionTransposeDescriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:@
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => kernelSizes -> CULong -> CULong -> CULong -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizes kernelSizes inputFeatureChannelCount outputFeatureChannelCount groupCount strides dilationRates paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) inputFeatureChannelCount outputFeatureChannelCount groupCount (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCConvolutionDescriptor object for depthwise convolution
--
-- @kernelWidth@ — The kernel size in x
--
-- @kernelHeight@ — The kernel size in x
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @channelMultiplier@ — The channel multiplier
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ depthwiseConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:channelMultiplier:@
depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplier :: CULong -> CULong -> CULong -> CULong -> IO (Id MLCConvolutionDescriptor)
depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplier kernelWidth kernelHeight inputFeatureChannelCount channelMultiplier =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplierSelector kernelWidth kernelHeight inputFeatureChannelCount channelMultiplier

-- | Create a MLCConvolutionDescriptor object for depthwise convolution
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @channelMultiplier@ — The channel multiplier
--
-- @strides@ — The kernel strides in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ depthwiseConvolutionDescriptorWithKernelSizes:inputFeatureChannelCount:channelMultiplier:strides:paddingPolicy:paddingSizes:@
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray paddingSizes) => kernelSizes -> CULong -> CULong -> strides -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizes kernelSizes inputFeatureChannelCount channelMultiplier strides paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) inputFeatureChannelCount channelMultiplier (toNSArray strides) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCConvolutionDescriptor object for depthwise convolution
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @inputFeatureChannelCount@ — The number of feature channels in the input tensor
--
-- @channelMultiplier@ — The channel multiplier
--
-- @strides@ — The kernel strides in x and y
--
-- @dilationRates@ — The dilation rates in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCConvolutionDescriptor object.
--
-- ObjC selector: @+ depthwiseConvolutionDescriptorWithKernelSizes:inputFeatureChannelCount:channelMultiplier:strides:dilationRates:paddingPolicy:paddingSizes:@
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => kernelSizes -> CULong -> CULong -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCConvolutionDescriptor)
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizes kernelSizes inputFeatureChannelCount channelMultiplier strides dilationRates paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCConvolutionDescriptor"
    sendClassMessage cls' depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) inputFeatureChannelCount channelMultiplier (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes)

-- | convolutionType
--
-- The type of convolution.
--
-- ObjC selector: @- convolutionType@
convolutionType :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO MLCConvolutionType
convolutionType mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor convolutionTypeSelector

-- | kernelWidth
--
-- The convolution kernel size in x.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
kernelWidth mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor kernelWidthSelector

-- | kernelHeight
--
-- The convolution kernel size in y.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
kernelHeight mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor kernelHeightSelector

-- | inputFeatureChannelCount
--
-- Number of channels in the input tensor
--
-- ObjC selector: @- inputFeatureChannelCount@
inputFeatureChannelCount :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
inputFeatureChannelCount mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor inputFeatureChannelCountSelector

-- | outputFeatureChannelCount
--
-- Number of channels in the output tensor
--
-- ObjC selector: @- outputFeatureChannelCount@
outputFeatureChannelCount :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
outputFeatureChannelCount mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor outputFeatureChannelCountSelector

-- | strideInX
--
-- The stride of the kernel in x.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
strideInX mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor strideInXSelector

-- | strideInY
--
-- The stride of the kernel in y.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
strideInY mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor strideInYSelector

-- | dilationRateInX
--
-- The dilation rate i.e. stride of elements in the kernel in x.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
dilationRateInX mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor dilationRateInXSelector

-- | dilationRateInY
--
-- The dilation rate i.e. stride of elements in the kernel in y.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
dilationRateInY mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor dilationRateInYSelector

-- | groupCount
--
-- Number of blocked connections from input channels to output channels
--
-- ObjC selector: @- groupCount@
groupCount :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
groupCount mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor groupCountSelector

-- | paddingPolicy
--
-- The padding policy to use.
--
-- ObjC selector: @- paddingPolicy@
paddingPolicy :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO MLCPaddingPolicy
paddingPolicy mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor paddingPolicySelector

-- | paddingSizeInX
--
-- The pooling size in x (left and right) to use if paddingPolicy is MLCPaddingPolicyUsePaddingSize
--
-- ObjC selector: @- paddingSizeInX@
paddingSizeInX :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
paddingSizeInX mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor paddingSizeInXSelector

-- | paddingSizeInY
--
-- The pooling size in y (top and bottom) to use if paddingPolicy is MLCPaddingPolicyUsePaddingSize
--
-- ObjC selector: @- paddingSizeInY@
paddingSizeInY :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO CULong
paddingSizeInY mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor paddingSizeInYSelector

-- | isConvolutionTranspose
--
-- A flag to indicate if this is a convolution transpose
--
-- ObjC selector: @- isConvolutionTranspose@
isConvolutionTranspose :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO Bool
isConvolutionTranspose mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor isConvolutionTransposeSelector

-- | usesDepthwiseConvolution
--
-- A flag to indicate depthwise convolution
--
-- ObjC selector: @- usesDepthwiseConvolution@
usesDepthwiseConvolution :: IsMLCConvolutionDescriptor mlcConvolutionDescriptor => mlcConvolutionDescriptor -> IO Bool
usesDepthwiseConvolution mlcConvolutionDescriptor =
  sendMessage mlcConvolutionDescriptor usesDepthwiseConvolutionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithType:kernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:@
descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector '[MLCConvolutionType, Id NSArray, CULong, CULong, CULong, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
descriptorWithType_kernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "descriptorWithType:kernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @descriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:outputFeatureChannelCount:@
descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector :: Selector '[CULong, CULong, CULong, CULong] (Id MLCConvolutionDescriptor)
descriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector = mkSelector "descriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:outputFeatureChannelCount:"

-- | @Selector@ for @descriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:strides:paddingPolicy:paddingSizes:@
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, CULong, CULong, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector = mkSelector "descriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @descriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:@
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, CULong, CULong, CULong, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
descriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "descriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @convolutionTransposeDescriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:outputFeatureChannelCount:@
convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector :: Selector '[CULong, CULong, CULong, CULong] (Id MLCConvolutionDescriptor)
convolutionTransposeDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_outputFeatureChannelCountSelector = mkSelector "convolutionTransposeDescriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:outputFeatureChannelCount:"

-- | @Selector@ for @convolutionTransposeDescriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:strides:paddingPolicy:paddingSizes:@
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, CULong, CULong, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_strides_paddingPolicy_paddingSizesSelector = mkSelector "convolutionTransposeDescriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @convolutionTransposeDescriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:@
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, CULong, CULong, CULong, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
convolutionTransposeDescriptorWithKernelSizes_inputFeatureChannelCount_outputFeatureChannelCount_groupCount_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "convolutionTransposeDescriptorWithKernelSizes:inputFeatureChannelCount:outputFeatureChannelCount:groupCount:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @depthwiseConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:channelMultiplier:@
depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplierSelector :: Selector '[CULong, CULong, CULong, CULong] (Id MLCConvolutionDescriptor)
depthwiseConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannelCount_channelMultiplierSelector = mkSelector "depthwiseConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannelCount:channelMultiplier:"

-- | @Selector@ for @depthwiseConvolutionDescriptorWithKernelSizes:inputFeatureChannelCount:channelMultiplier:strides:paddingPolicy:paddingSizes:@
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, CULong, CULong, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_paddingPolicy_paddingSizesSelector = mkSelector "depthwiseConvolutionDescriptorWithKernelSizes:inputFeatureChannelCount:channelMultiplier:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @depthwiseConvolutionDescriptorWithKernelSizes:inputFeatureChannelCount:channelMultiplier:strides:dilationRates:paddingPolicy:paddingSizes:@
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, CULong, CULong, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCConvolutionDescriptor)
depthwiseConvolutionDescriptorWithKernelSizes_inputFeatureChannelCount_channelMultiplier_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "depthwiseConvolutionDescriptorWithKernelSizes:inputFeatureChannelCount:channelMultiplier:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @convolutionType@
convolutionTypeSelector :: Selector '[] MLCConvolutionType
convolutionTypeSelector = mkSelector "convolutionType"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @inputFeatureChannelCount@
inputFeatureChannelCountSelector :: Selector '[] CULong
inputFeatureChannelCountSelector = mkSelector "inputFeatureChannelCount"

-- | @Selector@ for @outputFeatureChannelCount@
outputFeatureChannelCountSelector :: Selector '[] CULong
outputFeatureChannelCountSelector = mkSelector "outputFeatureChannelCount"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector '[] CULong
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector '[] CULong
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @dilationRateInX@
dilationRateInXSelector :: Selector '[] CULong
dilationRateInXSelector = mkSelector "dilationRateInX"

-- | @Selector@ for @dilationRateInY@
dilationRateInYSelector :: Selector '[] CULong
dilationRateInYSelector = mkSelector "dilationRateInY"

-- | @Selector@ for @groupCount@
groupCountSelector :: Selector '[] CULong
groupCountSelector = mkSelector "groupCount"

-- | @Selector@ for @paddingPolicy@
paddingPolicySelector :: Selector '[] MLCPaddingPolicy
paddingPolicySelector = mkSelector "paddingPolicy"

-- | @Selector@ for @paddingSizeInX@
paddingSizeInXSelector :: Selector '[] CULong
paddingSizeInXSelector = mkSelector "paddingSizeInX"

-- | @Selector@ for @paddingSizeInY@
paddingSizeInYSelector :: Selector '[] CULong
paddingSizeInYSelector = mkSelector "paddingSizeInY"

-- | @Selector@ for @isConvolutionTranspose@
isConvolutionTransposeSelector :: Selector '[] Bool
isConvolutionTransposeSelector = mkSelector "isConvolutionTranspose"

-- | @Selector@ for @usesDepthwiseConvolution@
usesDepthwiseConvolutionSelector :: Selector '[] Bool
usesDepthwiseConvolutionSelector = mkSelector "usesDepthwiseConvolution"

