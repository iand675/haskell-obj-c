{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionGradientState
--
-- The MPSCNNConvolutionGradientState is returned by resultStateForSourceImage:sourceStates method on MPSCNNConvolution object.              Note that resultStateForSourceImage:sourceStates:destinationImage creates the object on autoreleasepool.              It will be consumed by MPSCNNConvolutionGradient. This is also used by MPSCNNConvolutionTranspose encode call              that returns MPSImage on left hand side to correctly size the destination.              Note that state objects are not usable across batches i.e. when batch is done you should nuke the state object and create              new one for next batch.
--
-- This state exposes the gradient with respect to weights and biases, as computed by the MPSCNNConvolutionGradient kernel, as a metal buffer to be used              during weights and biases update. The standard weights and biases update formula is:
--
-- weights(t+1) = f(weights(t), gradientForWeights(t)) and                        biases(t+1) = f(biases(t), gradientForBiases(t)),
--
-- where the weights(t)/biases(t) are the wegihts and the biases at step t that are provided by data source provider used to create MPSCNNConvolution and              MPSCNNConvoltuionGradient objects. There are multiple ways user can update weights and biases as described below:
--
-- 1) For check pointing, i.e. updating weights/biases and storing:                   once the command buffer on which MPSCNNConvolutionGradient is enqueued is done (e.g. in command                 buffer completion callback), the application can simply use                                    float* delta_w = (float*)((char*)[gradientForWeights contents]);                                    float* delta_b = (float*)((char*)[gradientForBiases contents]);                  to update the weights and biases in the data provider directly.                  The application can instead provide a metal kernel that reads from gradientForWeights and gradientForBiases buffer and the buffer created using data provided by the data source                  to do any kind of update it will like to do, then read back the updated weights/biases and store to the data source. Note that lifetime of the                  gradientForWeights and gradientForBiases buffer is the same as the MPSCNNConvolutionGradientState. So it's the applications's responsibility to make sure the buffer is alive                  (retained) when the update kernel is running if the command buffer doesn't retain the buffer. Also, in order to gaurantee that the buffer is correctly                  synchronized for CPU side access, it is the application's responsibility to call                                     [gradientState synchronizeOnCommandBuffer:]                  before accessing data from the buffer.
--
-- 2) For a CPU side update, once the weights and biases in the data source provider are updated as above, the original MPSCNNConvolution and                 MPSCNNConvolutionGradient objects need to be updated with the new weigths and biases by calling the                       -(void) reloadWeightsAndBiasesFromDataSource                 method. Again application needs to call [gradientState synchronizeOnCommandBuffer:] before touching data on CPU side.
--
-- 3) The above CPU side update requires command buffer to be done. If the application doesn't want to update its data source provider object and would prefer to directly                 enqueue an update of the internal MPSCNNConvolution and MPSCNNConvolutionGradient weights/biases buffers on the GPU without CPU side involvement, it needs to do                 following:                     i) get gradientForWeights and gradientForBiases buffers from this gradient state object and set it as source of update kernel                    ii) create a temporary buffer, dest, of same size and set it as destination of update kernel                   iii) enqueue update kernel on command buffer                    iv) call reloadWeightsAndBiasesWithCommandBuffer:dest:weightsOffset:biasesOffset on MPSCNNConvolution and MPSCNNConvolutionGradient objects. This                        will reload the weights from application's update kernel in dest on GPU without CPU side involvement.
--
-- Generated bindings for @MPSCNNConvolutionGradientState@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionGradientState
  ( MPSCNNConvolutionGradientState
  , IsMPSCNNConvolutionGradientState(..)
  , convolution
  , gradientForWeightsLayout
  , convolutionSelector
  , gradientForWeightsLayoutSelector

  -- * Enum types
  , MPSCNNConvolutionWeightsLayout(MPSCNNConvolutionWeightsLayout)
  , pattern MPSCNNConvolutionWeightsLayoutOHWI

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

-- | convolution
--
-- The convolution filter that produced the state.              For child MPSCNNConvolutionTrasposeGradientState object, convolution              below refers to MPSCNNConvolution object that produced MPSCNNConvolutionGradientState object              which was used to create MPSCNNConvolutionTransposeGradientState object. See resultStateForSourceImage:sourceStates              method of MPSCNNConvolutionTranspose below.
--
-- ObjC selector: @- convolution@
convolution :: IsMPSCNNConvolutionGradientState mpscnnConvolutionGradientState => mpscnnConvolutionGradientState -> IO (Id MPSCNNConvolution)
convolution mpscnnConvolutionGradientState  =
  sendMsg mpscnnConvolutionGradientState (mkSelector "convolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gradientForWeightsLayout
--
-- Layout of gradient with respect to weights in gradientForWeights buffer.              Currently only MPSCNNConvolutionWeightsLayoutOHWI is supported.
--
-- ObjC selector: @- gradientForWeightsLayout@
gradientForWeightsLayout :: IsMPSCNNConvolutionGradientState mpscnnConvolutionGradientState => mpscnnConvolutionGradientState -> IO MPSCNNConvolutionWeightsLayout
gradientForWeightsLayout mpscnnConvolutionGradientState  =
  fmap (coerce :: CUInt -> MPSCNNConvolutionWeightsLayout) $ sendMsg mpscnnConvolutionGradientState (mkSelector "gradientForWeightsLayout") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @convolution@
convolutionSelector :: Selector
convolutionSelector = mkSelector "convolution"

-- | @Selector@ for @gradientForWeightsLayout@
gradientForWeightsLayoutSelector :: Selector
gradientForWeightsLayoutSelector = mkSelector "gradientForWeightsLayout"

