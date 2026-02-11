{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , poolingDescriptorWithType_kernelSize_strideSelector
  , maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector
  , maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector
  , averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector
  , l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector
  , l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector
  , poolingTypeSelector
  , kernelWidthSelector
  , kernelHeightSelector
  , strideInXSelector
  , strideInYSelector
  , dilationRateInXSelector
  , dilationRateInYSelector
  , paddingPolicySelector
  , paddingSizeInXSelector
  , paddingSizeInYSelector
  , countIncludesPaddingSelector

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

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCPoolingDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCPoolingDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO (Id MLCPoolingDescriptor)
init_ mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "poolingDescriptorWithType:kernelSize:stride:") (retPtr retVoid) [argCInt (coerce poolingType), argCULong (fromIntegral kernelSize), argCULong (fromIntegral stride)] >>= retainedObject . castPtr

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
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr paddingSizes $ \raw_paddingSizes ->
          sendClassMsg cls' (mkSelector "maxPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argCInt (coerce paddingPolicy), argPtr (castPtr raw_paddingSizes :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr dilationRates $ \raw_dilationRates ->
          withObjCPtr paddingSizes $ \raw_paddingSizes ->
            sendClassMsg cls' (mkSelector "maxPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argPtr (castPtr raw_dilationRates :: Ptr ()), argCInt (coerce paddingPolicy), argPtr (castPtr raw_paddingSizes :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr paddingSizes $ \raw_paddingSizes ->
          sendClassMsg cls' (mkSelector "averagePoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:countIncludesPadding:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argCInt (coerce paddingPolicy), argPtr (castPtr raw_paddingSizes :: Ptr ()), argCULong (if countIncludesPadding then 1 else 0)] >>= retainedObject . castPtr

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
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr dilationRates $ \raw_dilationRates ->
          withObjCPtr paddingSizes $ \raw_paddingSizes ->
            sendClassMsg cls' (mkSelector "averagePoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:countIncludesPadding:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argPtr (castPtr raw_dilationRates :: Ptr ()), argCInt (coerce paddingPolicy), argPtr (castPtr raw_paddingSizes :: Ptr ()), argCULong (if countIncludesPadding then 1 else 0)] >>= retainedObject . castPtr

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
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr paddingSizes $ \raw_paddingSizes ->
          sendClassMsg cls' (mkSelector "l2NormPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argCInt (coerce paddingPolicy), argPtr (castPtr raw_paddingSizes :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr kernelSizes $ \raw_kernelSizes ->
      withObjCPtr strides $ \raw_strides ->
        withObjCPtr dilationRates $ \raw_dilationRates ->
          withObjCPtr paddingSizes $ \raw_paddingSizes ->
            sendClassMsg cls' (mkSelector "l2NormPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:") (retPtr retVoid) [argPtr (castPtr raw_kernelSizes :: Ptr ()), argPtr (castPtr raw_strides :: Ptr ()), argPtr (castPtr raw_dilationRates :: Ptr ()), argCInt (coerce paddingPolicy), argPtr (castPtr raw_paddingSizes :: Ptr ())] >>= retainedObject . castPtr

-- | poolingType
--
-- The pooling operation
--
-- ObjC selector: @- poolingType@
poolingType :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO MLCPoolingType
poolingType mlcPoolingDescriptor  =
  fmap (coerce :: CInt -> MLCPoolingType) $ sendMsg mlcPoolingDescriptor (mkSelector "poolingType") retCInt []

-- | kernelWidth
--
-- The pooling kernel size in x.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
kernelWidth mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "kernelWidth") retCULong []

-- | kernelHeight
--
-- The pooling kernel size in y.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
kernelHeight mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "kernelHeight") retCULong []

-- | strideInX
--
-- The stride of the kernel in x.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
strideInX mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "strideInX") retCULong []

-- | strideInY
--
-- The stride of the kernel in y.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
strideInY mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "strideInY") retCULong []

-- | dilationRateInX
--
-- The dilation rate i.e. stride of elements in the kernel in x.
--
-- ObjC selector: @- dilationRateInX@
dilationRateInX :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
dilationRateInX mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "dilationRateInX") retCULong []

-- | dilationRateInY
--
-- The dilation rate i.e. stride of elements in the kernel in y.
--
-- ObjC selector: @- dilationRateInY@
dilationRateInY :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
dilationRateInY mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "dilationRateInY") retCULong []

-- | paddingPolicy
--
-- The padding policy to use.
--
-- ObjC selector: @- paddingPolicy@
paddingPolicy :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO MLCPaddingPolicy
paddingPolicy mlcPoolingDescriptor  =
  fmap (coerce :: CInt -> MLCPaddingPolicy) $ sendMsg mlcPoolingDescriptor (mkSelector "paddingPolicy") retCInt []

-- | paddingSizeInX
--
-- The padding size in x (left and right) to use if paddingPolicy is MLCPaddingPolicyUsePaddingSize
--
-- ObjC selector: @- paddingSizeInX@
paddingSizeInX :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
paddingSizeInX mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "paddingSizeInX") retCULong []

-- | paddingSizeInY
--
-- The padding size in y (top and bottom) to use if paddingPolicy is MLCPaddingPolicyUsePaddingSize
--
-- ObjC selector: @- paddingSizeInY@
paddingSizeInY :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO CULong
paddingSizeInY mlcPoolingDescriptor  =
  sendMsg mlcPoolingDescriptor (mkSelector "paddingSizeInY") retCULong []

-- | countIncludesPadding
--
-- Include the zero-padding in the averaging calculation if true.  Used only with average pooling.
--
-- ObjC selector: @- countIncludesPadding@
countIncludesPadding :: IsMLCPoolingDescriptor mlcPoolingDescriptor => mlcPoolingDescriptor -> IO Bool
countIncludesPadding mlcPoolingDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcPoolingDescriptor (mkSelector "countIncludesPadding") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @poolingDescriptorWithType:kernelSize:stride:@
poolingDescriptorWithType_kernelSize_strideSelector :: Selector
poolingDescriptorWithType_kernelSize_strideSelector = mkSelector "poolingDescriptorWithType:kernelSize:stride:"

-- | @Selector@ for @maxPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:@
maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector :: Selector
maxPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector = mkSelector "maxPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @maxPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:@
maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector
maxPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "maxPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @averagePoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:countIncludesPadding:@
averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector :: Selector
averagePoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizes_countIncludesPaddingSelector = mkSelector "averagePoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:countIncludesPadding:"

-- | @Selector@ for @averagePoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:countIncludesPadding:@
averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector :: Selector
averagePoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizes_countIncludesPaddingSelector = mkSelector "averagePoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:countIncludesPadding:"

-- | @Selector@ for @l2NormPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:@
l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector :: Selector
l2NormPoolingDescriptorWithKernelSizes_strides_paddingPolicy_paddingSizesSelector = mkSelector "l2NormPoolingDescriptorWithKernelSizes:strides:paddingPolicy:paddingSizes:"

-- | @Selector@ for @l2NormPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:@
l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector :: Selector
l2NormPoolingDescriptorWithKernelSizes_strides_dilationRates_paddingPolicy_paddingSizesSelector = mkSelector "l2NormPoolingDescriptorWithKernelSizes:strides:dilationRates:paddingPolicy:paddingSizes:"

-- | @Selector@ for @poolingType@
poolingTypeSelector :: Selector
poolingTypeSelector = mkSelector "poolingType"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @dilationRateInX@
dilationRateInXSelector :: Selector
dilationRateInXSelector = mkSelector "dilationRateInX"

-- | @Selector@ for @dilationRateInY@
dilationRateInYSelector :: Selector
dilationRateInYSelector = mkSelector "dilationRateInY"

-- | @Selector@ for @paddingPolicy@
paddingPolicySelector :: Selector
paddingPolicySelector = mkSelector "paddingPolicy"

-- | @Selector@ for @paddingSizeInX@
paddingSizeInXSelector :: Selector
paddingSizeInXSelector = mkSelector "paddingSizeInX"

-- | @Selector@ for @paddingSizeInY@
paddingSizeInYSelector :: Selector
paddingSizeInYSelector = mkSelector "paddingSizeInY"

-- | @Selector@ for @countIncludesPadding@
countIncludesPaddingSelector :: Selector
countIncludesPaddingSelector = mkSelector "countIncludesPadding"

