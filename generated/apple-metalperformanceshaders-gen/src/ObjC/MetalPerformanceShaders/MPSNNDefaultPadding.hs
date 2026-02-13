{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class provides some pre-rolled padding policies for common tasks
--
-- You are, of course, welcome to write your own class that conforms to              The MPSNNPadding protocol and use that instead.
--
-- Generated bindings for @MPSNNDefaultPadding@.
module ObjC.MetalPerformanceShaders.MPSNNDefaultPadding
  ( MPSNNDefaultPadding
  , IsMPSNNDefaultPadding(..)
  , paddingWithMethod
  , paddingForTensorflowAveragePooling
  , paddingForTensorflowAveragePoolingValidOnly
  , label
  , labelSelector
  , paddingForTensorflowAveragePoolingSelector
  , paddingForTensorflowAveragePoolingValidOnlySelector
  , paddingWithMethodSelector

  -- * Enum types
  , MPSNNPaddingMethod(MPSNNPaddingMethod)
  , pattern MPSNNPaddingMethodAlignCentered
  , pattern MPSNNPaddingMethodAlignTopLeft
  , pattern MPSNNPaddingMethodAlignBottomRight
  , pattern MPSNNPaddingMethodAlign_reserved
  , pattern MPSNNPaddingMethodAlignMask
  , pattern MPSNNPaddingMethodAddRemainderToTopLeft
  , pattern MPSNNPaddingMethodAddRemainderToTopRight
  , pattern MPSNNPaddingMethodAddRemainderToBottomLeft
  , pattern MPSNNPaddingMethodAddRemainderToBottomRight
  , pattern MPSNNPaddingMethodAddRemainderToMask
  , pattern MPSNNPaddingMethodSizeValidOnly
  , pattern MPSNNPaddingMethodSizeSame
  , pattern MPSNNPaddingMethodSizeFull
  , pattern MPSNNPaddingMethodSize_reserved
  , pattern MPSNNPaddingMethodCustomWhitelistForNodeFusion
  , pattern MPSNNPaddingMethodCustomAllowForNodeFusion
  , pattern MPSNNPaddingMethodCustom
  , pattern MPSNNPaddingMethodSizeMask
  , pattern MPSNNPaddingMethodExcludeEdges

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Fetch a well known object that implements a non-custom padding method
--
-- For custom padding methods, you will need to implement an object that conforms              to the full MPSNNPadding protocol, including NSSecureCoding.
--
-- @method@ — A MPSNNPaddingMethod
--
-- Returns: An object that implements <MPSNNPadding> for use with MPSNNGraphNodes.
--
-- ObjC selector: @+ paddingWithMethod:@
paddingWithMethod :: MPSNNPaddingMethod -> IO (Id MPSNNDefaultPadding)
paddingWithMethod method =
  do
    cls' <- getRequiredClass "MPSNNDefaultPadding"
    sendClassMessage cls' paddingWithMethodSelector method

-- | A padding policy that attempts to reproduce TensorFlow behavior for average pooling
--
-- Most TensorFlow padding is covered by the standard MPSNNPaddingMethod encodings.                  You can use +paddingWithMethod to get quick access to MPSNNPadding objects, when                  default filter behavior isn't enough. (It often is.)  However, the edging for                  max pooling in TensorFlow is a bit unusual.
--
-- This padding method attempts to reproduce TensorFlow padding for average pooling.                  In addition to setting MPSNNPaddingMethodSizeSame | MPSNNPaddingMethodAlignCentered |                  MPSNNPaddingMethodAddRemainderToBottomRight, it also configures the filter to run with                  MPSImageEdgeModeClamp, which (as a special case for average pooling only), normalizes the                  sum of contributing samples to the area of valid contributing pixels only.
--
-- // Sample implementation for the tensorflowPoolingPaddingPolicy returned
-- -(MPSNNPaddingMethod) paddingMethod{ return MPSNNPaddingMethodCustom | MPSNNPaddingMethodSizeSame; }
--
-- -(MPSImageDescriptor * __nonnull) destinationImageDescriptorForSourceImages: (NSArray <MPSImage *> *__nonnull) sourceImages
-- sourceStates: (NSArray <MPSState *> * __nullable) sourceStates
-- forKernel: (MPSKernel * __nonnull) kernel
-- suggestedDescriptor: (MPSImageDescriptor * __nonnull) inDescriptor
-- {
--
-- ((MPSCNNKernel *)kernel).edgeMode = MPSImageEdgeModeClamp;
--
-- return inDescriptor;
-- }
--
-- ObjC selector: @+ paddingForTensorflowAveragePooling@
paddingForTensorflowAveragePooling :: IO (Id MPSNNDefaultPadding)
paddingForTensorflowAveragePooling  =
  do
    cls' <- getRequiredClass "MPSNNDefaultPadding"
    sendClassMessage cls' paddingForTensorflowAveragePoolingSelector

-- | Typical pooling padding policy for valid only mode
--
-- ObjC selector: @+ paddingForTensorflowAveragePoolingValidOnly@
paddingForTensorflowAveragePoolingValidOnly :: IO (Id MPSNNDefaultPadding)
paddingForTensorflowAveragePoolingValidOnly  =
  do
    cls' <- getRequiredClass "MPSNNDefaultPadding"
    sendClassMessage cls' paddingForTensorflowAveragePoolingValidOnlySelector

-- | Human readable description of what the padding policy does
--
-- ObjC selector: @- label@
label :: IsMPSNNDefaultPadding mpsnnDefaultPadding => mpsnnDefaultPadding -> IO (Id NSString)
label mpsnnDefaultPadding =
  sendMessage mpsnnDefaultPadding labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paddingWithMethod:@
paddingWithMethodSelector :: Selector '[MPSNNPaddingMethod] (Id MPSNNDefaultPadding)
paddingWithMethodSelector = mkSelector "paddingWithMethod:"

-- | @Selector@ for @paddingForTensorflowAveragePooling@
paddingForTensorflowAveragePoolingSelector :: Selector '[] (Id MPSNNDefaultPadding)
paddingForTensorflowAveragePoolingSelector = mkSelector "paddingForTensorflowAveragePooling"

-- | @Selector@ for @paddingForTensorflowAveragePoolingValidOnly@
paddingForTensorflowAveragePoolingValidOnlySelector :: Selector '[] (Id MPSNNDefaultPadding)
paddingForTensorflowAveragePoolingValidOnlySelector = mkSelector "paddingForTensorflowAveragePoolingValidOnly"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

