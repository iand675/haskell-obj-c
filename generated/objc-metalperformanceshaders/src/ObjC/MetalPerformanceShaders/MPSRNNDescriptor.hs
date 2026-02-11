{-# LANGUAGE PatternSynonyms #-}
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
  , setInputFeatureChannelsSelector
  , outputFeatureChannelsSelector
  , setOutputFeatureChannelsSelector
  , useLayerInputUnitTransformModeSelector
  , setUseLayerInputUnitTransformModeSelector
  , useFloat32WeightsSelector
  , setUseFloat32WeightsSelector
  , layerSequenceDirectionSelector
  , setLayerSequenceDirectionSelector

  -- * Enum types
  , MPSRNNSequenceDirection(MPSRNNSequenceDirection)
  , pattern MPSRNNSequenceDirectionForward
  , pattern MPSRNNSequenceDirectionBackward

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image or number of rows in the input matrix.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO CULong
inputFeatureChannels mpsrnnDescriptor  =
  sendMsg mpsrnnDescriptor (mkSelector "inputFeatureChannels") retCULong []

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image or number of rows in the input matrix.
--
-- ObjC selector: @- setInputFeatureChannels:@
setInputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> CULong -> IO ()
setInputFeatureChannels mpsrnnDescriptor  value =
  sendMsg mpsrnnDescriptor (mkSelector "setInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the destination image or number of rows in the destination matrix.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO CULong
outputFeatureChannels mpsrnnDescriptor  =
  sendMsg mpsrnnDescriptor (mkSelector "outputFeatureChannels") retCULong []

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the destination image or number of rows in the destination matrix.
--
-- ObjC selector: @- setOutputFeatureChannels:@
setOutputFeatureChannels :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> CULong -> IO ()
setOutputFeatureChannels mpsrnnDescriptor  value =
  sendMsg mpsrnnDescriptor (mkSelector "setOutputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | useLayerInputUnitTransformMode
--
-- if YES then use identity transformation for all weights (W, Wr, Wi, Wf, Wo, Wc) affecting input x_j in this layer,              even if said weights are specified as nil.              For example 'W_ij * x_j' is replaced by 'x_j' in formulae defined in MPSRNNSingleGateDescriptor. Defaults to NO.
--
-- ObjC selector: @- useLayerInputUnitTransformMode@
useLayerInputUnitTransformMode :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO Bool
useLayerInputUnitTransformMode mpsrnnDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsrnnDescriptor (mkSelector "useLayerInputUnitTransformMode") retCULong []

-- | useLayerInputUnitTransformMode
--
-- if YES then use identity transformation for all weights (W, Wr, Wi, Wf, Wo, Wc) affecting input x_j in this layer,              even if said weights are specified as nil.              For example 'W_ij * x_j' is replaced by 'x_j' in formulae defined in MPSRNNSingleGateDescriptor. Defaults to NO.
--
-- ObjC selector: @- setUseLayerInputUnitTransformMode:@
setUseLayerInputUnitTransformMode :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> Bool -> IO ()
setUseLayerInputUnitTransformMode mpsrnnDescriptor  value =
  sendMsg mpsrnnDescriptor (mkSelector "setUseLayerInputUnitTransformMode:") retVoid [argCULong (if value then 1 else 0)]

-- | useFloat32Weights
--
-- If YES, then MPSRNNMatrixInferenceLayer uses 32-bit floating point numbers internally for weights when              computing matrix transformations. If NO, then 16-bit, half precision floating point numbers are used.              Currently MPSRNNImageInferenceLayer ignores this property and the convolution operations always              convert FP32 weights into FP16 for better performance.              Defaults to NO.
--
-- ObjC selector: @- useFloat32Weights@
useFloat32Weights :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> IO Bool
useFloat32Weights mpsrnnDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsrnnDescriptor (mkSelector "useFloat32Weights") retCULong []

-- | useFloat32Weights
--
-- If YES, then MPSRNNMatrixInferenceLayer uses 32-bit floating point numbers internally for weights when              computing matrix transformations. If NO, then 16-bit, half precision floating point numbers are used.              Currently MPSRNNImageInferenceLayer ignores this property and the convolution operations always              convert FP32 weights into FP16 for better performance.              Defaults to NO.
--
-- ObjC selector: @- setUseFloat32Weights:@
setUseFloat32Weights :: IsMPSRNNDescriptor mpsrnnDescriptor => mpsrnnDescriptor -> Bool -> IO ()
setUseFloat32Weights mpsrnnDescriptor  value =
  sendMsg mpsrnnDescriptor (mkSelector "setUseFloat32Weights:") retVoid [argCULong (if value then 1 else 0)]

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
layerSequenceDirection mpsrnnDescriptor  =
  fmap (coerce :: CULong -> MPSRNNSequenceDirection) $ sendMsg mpsrnnDescriptor (mkSelector "layerSequenceDirection") retCULong []

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
setLayerSequenceDirection mpsrnnDescriptor  value =
  sendMsg mpsrnnDescriptor (mkSelector "setLayerSequenceDirection:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @setInputFeatureChannels:@
setInputFeatureChannelsSelector :: Selector
setInputFeatureChannelsSelector = mkSelector "setInputFeatureChannels:"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @setOutputFeatureChannels:@
setOutputFeatureChannelsSelector :: Selector
setOutputFeatureChannelsSelector = mkSelector "setOutputFeatureChannels:"

-- | @Selector@ for @useLayerInputUnitTransformMode@
useLayerInputUnitTransformModeSelector :: Selector
useLayerInputUnitTransformModeSelector = mkSelector "useLayerInputUnitTransformMode"

-- | @Selector@ for @setUseLayerInputUnitTransformMode:@
setUseLayerInputUnitTransformModeSelector :: Selector
setUseLayerInputUnitTransformModeSelector = mkSelector "setUseLayerInputUnitTransformMode:"

-- | @Selector@ for @useFloat32Weights@
useFloat32WeightsSelector :: Selector
useFloat32WeightsSelector = mkSelector "useFloat32Weights"

-- | @Selector@ for @setUseFloat32Weights:@
setUseFloat32WeightsSelector :: Selector
setUseFloat32WeightsSelector = mkSelector "setUseFloat32Weights:"

-- | @Selector@ for @layerSequenceDirection@
layerSequenceDirectionSelector :: Selector
layerSequenceDirectionSelector = mkSelector "layerSequenceDirection"

-- | @Selector@ for @setLayerSequenceDirection:@
setLayerSequenceDirectionSelector :: Selector
setLayerSequenceDirectionSelector = mkSelector "setLayerSequenceDirection:"

