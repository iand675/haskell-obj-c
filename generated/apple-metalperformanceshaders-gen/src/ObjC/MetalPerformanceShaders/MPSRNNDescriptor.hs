{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNDescriptor
--
-- This depends on Metal.framework
--
-- The MPSRNNDescriptor specifies a Recursive neural network block/layer descriptor.
--
-- Generated bindings for @MPSRNNDescriptor@.
module ObjC.MetalPerformanceShaders.MPSRNNDescriptor
  ( MPSRNNDescriptor
  , IsMPSRNNDescriptor(..)
  , inputFeatureChannels
  , setInputFeatureChannels
  , outputFeatureChannels
  , setOutputFeatureChannels
  , useLayerInputUnitTransformMode
  , setUseLayerInputUnitTransformMode
  , useFloat32Weights
  , setUseFloat32Weights
  , layerSequenceDirection
  , setLayerSequenceDirection
  , inputFeatureChannelsSelector
  , layerSequenceDirectionSelector
  , outputFeatureChannelsSelector
  , setInputFeatureChannelsSelector
  , setLayerSequenceDirectionSelector
  , setOutputFeatureChannelsSelector
  , setUseFloat32WeightsSelector
  , setUseLayerInputUnitTransformModeSelector
  , useFloat32WeightsSelector
  , useLayerInputUnitTransformModeSelector

  -- * Enum types
  , MPSRNNSequenceDirection(MPSRNNSequenceDirection)
  , pattern MPSRNNSequenceDirectionForward
  , pattern MPSRNNSequenceDirectionBackward

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

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image or number of rows in the input matrix.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO CULong
inputFeatureChannels mpsrnnDescriptor =
  sendMessage mpsrnnDescriptor inputFeatureChannelsSelector

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image or number of rows in the input matrix.
--
-- ObjC selector: @- setInputFeatureChannels:@
setInputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> CULong -> IO ()
setInputFeatureChannels mpsrnnDescriptor value =
  sendMessage mpsrnnDescriptor setInputFeatureChannelsSelector value

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the destination image or number of rows in the destination matrix.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO CULong
outputFeatureChannels mpsrnnDescriptor =
  sendMessage mpsrnnDescriptor outputFeatureChannelsSelector

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the destination image or number of rows in the destination matrix.
--
-- ObjC selector: @- setOutputFeatureChannels:@
setOutputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> CULong -> IO ()
setOutputFeatureChannels mpsrnnDescriptor value =
  sendMessage mpsrnnDescriptor setOutputFeatureChannelsSelector value

-- | useLayerInputUnitTransformMode
--
-- if YES then use identity transformation for all weights (W, Wr, Wi, Wf, Wo, Wc) affecting input x_j in this layer,              even if said weights are specified as nil.              For example 'W_ij * x_j' is replaced by 'x_j' in formulae defined in MPSRNNSingleGateDescriptor. Defaults to NO.
--
-- ObjC selector: @- useLayerInputUnitTransformMode@
useLayerInputUnitTransformMode :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO Bool
useLayerInputUnitTransformMode mpsrnnDescriptor =
  sendMessage mpsrnnDescriptor useLayerInputUnitTransformModeSelector

-- | useLayerInputUnitTransformMode
--
-- if YES then use identity transformation for all weights (W, Wr, Wi, Wf, Wo, Wc) affecting input x_j in this layer,              even if said weights are specified as nil.              For example 'W_ij * x_j' is replaced by 'x_j' in formulae defined in MPSRNNSingleGateDescriptor. Defaults to NO.
--
-- ObjC selector: @- setUseLayerInputUnitTransformMode:@
setUseLayerInputUnitTransformMode :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> Bool -> IO ()
setUseLayerInputUnitTransformMode mpsrnnDescriptor value =
  sendMessage mpsrnnDescriptor setUseLayerInputUnitTransformModeSelector value

-- | useFloat32Weights
--
-- If YES, then MPSRNNMatrixInferenceLayer uses 32-bit floating point numbers internally for weights when              computing matrix transformations. If NO, then 16-bit, half precision floating point numbers are used.              Currently MPSRNNImageInferenceLayer ignores this property and the convolution operations always              convert FP32 weights into FP16 for better performance.              Defaults to NO.
--
-- ObjC selector: @- useFloat32Weights@
useFloat32Weights :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO Bool
useFloat32Weights mpsrnnDescriptor =
  sendMessage mpsrnnDescriptor useFloat32WeightsSelector

-- | useFloat32Weights
--
-- If YES, then MPSRNNMatrixInferenceLayer uses 32-bit floating point numbers internally for weights when              computing matrix transformations. If NO, then 16-bit, half precision floating point numbers are used.              Currently MPSRNNImageInferenceLayer ignores this property and the convolution operations always              convert FP32 weights into FP16 for better performance.              Defaults to NO.
--
-- ObjC selector: @- setUseFloat32Weights:@
setUseFloat32Weights :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> Bool -> IO ()
setUseFloat32Weights mpsrnnDescriptor value =
  sendMessage mpsrnnDescriptor setUseFloat32WeightsSelector value

-- | layerSequenceDirection
--
-- When the layer specified with this descriptor is used to process a sequence of inputs              by calling
--
-- See: encodeBidirectionalSequenceToCommandBuffer then this parameter defines              in which direction the sequence is processed. The operation of the layer is:                  (yt, ht, ct) = f(xt,ht-1,ct-1) for MPSRNNSequenceDirectionForward              and                  (yt, ht, ct) = f(xt,ht+1,ct+1) for MPSRNNSequenceDirectionBackward, where              xt is the output of the previous layer that encodes in the same direction as this layer,              (or the input image or matrix if this is the first layer in stack with this direction).
--
-- See: MPSRNNImageInferenceLayer and
--
-- See: MPSRNNMatrixInferenceLayer.
--
-- ObjC selector: @- layerSequenceDirection@
layerSequenceDirection :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO MPSRNNSequenceDirection
layerSequenceDirection mpsrnnDescriptor =
  sendMessage mpsrnnDescriptor layerSequenceDirectionSelector

-- | layerSequenceDirection
--
-- When the layer specified with this descriptor is used to process a sequence of inputs              by calling
--
-- See: encodeBidirectionalSequenceToCommandBuffer then this parameter defines              in which direction the sequence is processed. The operation of the layer is:                  (yt, ht, ct) = f(xt,ht-1,ct-1) for MPSRNNSequenceDirectionForward              and                  (yt, ht, ct) = f(xt,ht+1,ct+1) for MPSRNNSequenceDirectionBackward, where              xt is the output of the previous layer that encodes in the same direction as this layer,              (or the input image or matrix if this is the first layer in stack with this direction).
--
-- See: MPSRNNImageInferenceLayer and
--
-- See: MPSRNNMatrixInferenceLayer.
--
-- ObjC selector: @- setLayerSequenceDirection:@
setLayerSequenceDirection :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> MPSRNNSequenceDirection -> IO ()
setLayerSequenceDirection mpsrnnDescriptor value =
  sendMessage mpsrnnDescriptor setLayerSequenceDirectionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector '[] CULong
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @setInputFeatureChannels:@
setInputFeatureChannelsSelector :: Selector '[CULong] ()
setInputFeatureChannelsSelector = mkSelector "setInputFeatureChannels:"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector '[] CULong
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @setOutputFeatureChannels:@
setOutputFeatureChannelsSelector :: Selector '[CULong] ()
setOutputFeatureChannelsSelector = mkSelector "setOutputFeatureChannels:"

-- | @Selector@ for @useLayerInputUnitTransformMode@
useLayerInputUnitTransformModeSelector :: Selector '[] Bool
useLayerInputUnitTransformModeSelector = mkSelector "useLayerInputUnitTransformMode"

-- | @Selector@ for @setUseLayerInputUnitTransformMode:@
setUseLayerInputUnitTransformModeSelector :: Selector '[Bool] ()
setUseLayerInputUnitTransformModeSelector = mkSelector "setUseLayerInputUnitTransformMode:"

-- | @Selector@ for @useFloat32Weights@
useFloat32WeightsSelector :: Selector '[] Bool
useFloat32WeightsSelector = mkSelector "useFloat32Weights"

-- | @Selector@ for @setUseFloat32Weights:@
setUseFloat32WeightsSelector :: Selector '[Bool] ()
setUseFloat32WeightsSelector = mkSelector "setUseFloat32Weights:"

-- | @Selector@ for @layerSequenceDirection@
layerSequenceDirectionSelector :: Selector '[] MPSRNNSequenceDirection
layerSequenceDirectionSelector = mkSelector "layerSequenceDirection"

-- | @Selector@ for @setLayerSequenceDirection:@
setLayerSequenceDirectionSelector :: Selector '[MPSRNNSequenceDirection] ()
setLayerSequenceDirectionSelector = mkSelector "setLayerSequenceDirection:"

