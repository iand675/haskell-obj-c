{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCPoolingDescriptor
--
-- The MLCPoolingDescriptor specifies a pooling descriptor.
--
-- Generated bindings for @MLCPoolingDescriptor@.
module ObjC.MLCompute.MLCPoolingDescriptor
  ( MLCPoolingDescriptor
  , IsMLCPoolingDescriptor(..)
  , new
  , init_
  , poolingDescriptorWithType_kernelSize_stride
  , maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes
  , maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes
  , averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPadding
  , averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPadding
  , l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes
  , l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes
  , poolingType
  , kernelWidth
  , kernelHeight
  , strideInX
  , strideInY
  , dilationRateInX
  , dilationRateInY
  , paddingPolicy
  , paddingSizeInX
  , paddingSizeInY
  , countIncludesPadding
  , averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector
  , averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector
  , countIncludesPaddingSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , initSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector
  , maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector
  , newSelector
  , paddingPolicySelector
  , paddingSizeInXSelector
  , paddingSizeInYSelector
  , poolingDescriptorWithType_kernelSize_strideSelector
  , poolingTypeSelector
  , strideInXSelector
  , strideInYSelector

  -- * Enum types
  , MLCPaddingPolicy(MLCPaddingPolicy)
  , pattern MLCPaddingPolicySame
  , pattern MLCPaddingPolicyValid
  , pattern MLCPaddingPolicyUsePaddingSize
  , MLCPoolingType(MLCPoolingType)
  , pattern MLCPoolingTypeMax
  , pattern MLCPoolingTypeAverage
  , pattern MLCPoolingTypeL2Norm
  , pattern MLCPoolingTypeCount

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

-- | @+ new@
new :: IO (Id MLCPoolingDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO (Id MLCPoolingDescriptor)
init_ mlcPoolingDescriptor =
  sendOwnedMessage mlcPoolingDescriptor initSelector

-- | Create a MLCPoolingDescriptor object
--
-- @poolingType@ — The pooling function
--
-- @kernelSize@ — The kernel sizes in x and y
--
-- @stride@ — The kernel strides in x and y
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ poolingDescriptorWithType:kernelSize:stride:@
poolingDescriptorWithType_kernelSize_stride :: MLCPoolingType -> CULong -> CULong -> IO (Id MLCPoolingDescriptor)
poolingDescriptorWithType_kernelSize_stride poolingType kernelSize stride =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' poolingDescriptorWithType_kernelSize_strideSelector poolingType kernelSize stride

-- | Create a MLCPoolingDescriptor object for a max pooling function
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @strides@ — The kernel strides in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ maxPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:@
maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray paddingSizes) => kernelSizes -> strides -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCPoolingDescriptor)
maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes kernelSizes strides paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) (toNSArray strides) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCPoolingDescriptor object for a max pooling function
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @strides@ — The kernel strides in x and y
--
-- @dilationRates@ — The kernel dilation rates in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ maxPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:@
maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => kernelSizes -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCPoolingDescriptor)
maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes kernelSizes strides dilationRates paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCPoolingDescriptor object for an average pooling function
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @strides@ — The kernel strides in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- @countIncludesPadding@ — Whether to include zero padding in the averaging calculation
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ averagePoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:countIncludesPadding:@
averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPadding :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray paddingSizes) => kernelSizes -> strides -> MLCPaddingPolicy -> paddingSizes -> Bool -> IO (Id MLCPoolingDescriptor)
averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPadding kernelSizes strides paddingPolicy paddingSizes countIncludesPadding =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector (toNSArray kernelSizes) (toNSArray strides) paddingPolicy (toNSArray paddingSizes) countIncludesPadding

-- | Create a MLCPoolingDescriptor object for an average pooling function
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @strides@ — The kernel strides in x and y
--
-- @dilationRates@ — The kernel dilation rates in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- @countIncludesPadding@ — Whether to include zero padding in the averaging calculation
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ averagePoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:countIncludesPadding:@
averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPadding :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => kernelSizes -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> Bool -> IO (Id MLCPoolingDescriptor)
averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPadding kernelSizes strides dilationRates paddingPolicy paddingSizes countIncludesPadding =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector (toNSArray kernelSizes) (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes) countIncludesPadding

-- | Create a MLCPoolingDescriptor object for a L2 norm pooling function
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @strides@ — The kernel strides in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ l2NormPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:@
l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray paddingSizes) => kernelSizes -> strides -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCPoolingDescriptor)
l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes kernelSizes strides paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) (toNSArray strides) paddingPolicy (toNSArray paddingSizes)

-- | Create a MLCPoolingDescriptor object for a L2 norm pooling function
--
-- @kernelSizes@ — The kernel sizes in x and y
--
-- @strides@ — The kernel strides in x and y
--
-- @dilationRates@ — The kernel dilation rates in x and y
--
-- @paddingPolicy@ — The padding policy
--
-- @paddingSizes@ — The padding sizes in x and y if padding policy is MLCPaddingPolicyUsePaddingSIze
--
-- Returns: A new MLCPoolingDescriptor object.
--
-- ObjC selector: @+ l2NormPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:@
l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes :: (IsNSArray kernelSizes, IsNSArray strides, IsNSArray dilationRates, IsNSArray paddingSizes) => kernelSizes -> strides -> dilationRates -> MLCPaddingPolicy -> paddingSizes -> IO (Id MLCPoolingDescriptor)
l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes kernelSizes strides dilationRates paddingPolicy paddingSizes =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMessage cls' l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector (toNSArray kernelSizes) (toNSArray strides) (toNSArray dilationRates) paddingPolicy (toNSArray paddingSizes)

-- | poolingType
--
-- The pooling operation
--
-- ObjC selector: @- poolingType@
poolingType :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO MLCPoolingType
poolingType mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor poolingTypeSelector

-- | kernelWidth
--
-- The pooling kernel size in x.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
kernelWidth mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor kernelWidthSelector

-- | kernelHeight
--
-- The pooling kernel size in y.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
kernelHeight mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor kernelHeightSelector

-- | strideInX
--
-- The stride of the kernel in x.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
strideInX mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor strideInXSelector

-- | strideInY
--
-- The stride of the kernel in y.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
strideInY mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor strideInYSelector

-- | dilationRateInX
--
-- The dilation rate i.e. stride of elements in the kernel in x.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
dilationRateInX mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor dilationRateInXSelector

-- | dilationRateInY
--
-- The dilation rate i.e. stride of elements in the kernel in y.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
dilationRateInY mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor dilationRateInYSelector

-- | paddingPolicy
--
-- The padding policy to use.
--
-- ObjC selector: @- paddingPolicy@
paddingPolicy :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO MLCPaddingPolicy
paddingPolicy mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor paddingPolicySelector

-- | paddingSizeInX
--
-- The padding size in x (left and right) to use if paddingPolicy is MLCPaddingPolicyUsePaddingSize
--
-- ObjC selector: @- paddingSizeInX@
paddingSizeInX :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
paddingSizeInX mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor paddingSizeInXSelector

-- | paddingSizeInY
--
-- The padding size in y (top and bottom) to use if paddingPolicy is MLCPaddingPolicyUsePaddingSize
--
-- ObjC selector: @- paddingSizeInY@
paddingSizeInY :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
paddingSizeInY mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor paddingSizeInYSelector

-- | countIncludesPadding
--
-- Include the zero-padding in the averaging calculation if true.  Used only with average pooling.
--
-- ObjC selector: @- countIncludesPadding@
countIncludesPadding :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO Bool
countIncludesPadding mlcPoolingDescriptor =
  sendMessage mlcPoolingDescriptor countIncludesPaddingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCPoolingDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCPoolingDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @poolingDescriptorWithType:kernelSize:stride:@
poolingDescriptorWithType_kernelSize_strideSelector :: Selector '[MLCPoolingType, CULong, CULong] (Id MLCPoolingDescriptor)
poolingDescriptorWithType_kernelSize_strideSelector = mkSelector "poolingDescriptorWithType:kernelSize:stride:"

-- | @Selector@ for @maxPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:@
maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCPoolingDescriptor)
maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector = mkSelector "maxPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @maxPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:@
maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCPoolingDescriptor)
maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "maxPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @averagePoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:countIncludesPadding:@
averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector :: Selector '[Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray, Bool] (Id MLCPoolingDescriptor)
averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector = mkSelector "averagePoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:countIncludesPadding:"

-- | @Selector@ for @averagePoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:countIncludesPadding:@
averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray, Bool] (Id MLCPoolingDescriptor)
averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector = mkSelector "averagePoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:countIncludesPadding:"

-- | @Selector@ for @l2NormPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:@
l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCPoolingDescriptor)
l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector = mkSelector "l2NormPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @l2NormPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:@
l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, MLCPaddingPolicy, Id NSArray] (Id MLCPoolingDescriptor)
l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "l2NormPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @poolingType@
poolingTypeSelector :: Selector '[] MLCPoolingType
poolingTypeSelector = mkSelector "poolingType"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

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

-- | @Selector@ for @paddingPolicy@
paddingPolicySelector :: Selector '[] MLCPaddingPolicy
paddingPolicySelector = mkSelector "paddingPolicy"

-- | @Selector@ for @paddingSizeInX@
paddingSizeInXSelector :: Selector '[] CULong
paddingSizeInXSelector = mkSelector "paddingSizeInX"

-- | @Selector@ for @paddingSizeInY@
paddingSizeInYSelector :: Selector '[] CULong
paddingSizeInYSelector = mkSelector "paddingSizeInY"

-- | @Selector@ for @countIncludesPadding@
countIncludesPaddingSelector :: Selector '[] Bool
countIncludesPaddingSelector = mkSelector "countIncludesPadding"

