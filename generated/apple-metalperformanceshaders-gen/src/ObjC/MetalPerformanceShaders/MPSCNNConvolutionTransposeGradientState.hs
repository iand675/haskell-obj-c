{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionTransposeGradientState
--
-- The MPSCNNConvolutionTransposeGradientState is returned by resultStateForSourceImage:sourceStates method on MPSCNNConvolutionTranspose object.              Note that resultStateForSourceImage:sourceStates:destinationImage creates the object on autoreleasepool.              It will be consumed by MPSCNNConvolutionTransposeGradient. It contains reference to MPSCNNConvolutionGradientState object that connects              MPSCNNConvolution and its corresponding MPSCNNConvolutionTranspose in forward pass of autoencoder. In an autoencoder forward pass, MPSCNNConvolutionGradientState is produced              by MPSCNNConvolution object and is used by corresponding MPSCNNConvolutionTraspose of forward pass that "undo" the corresponding MPSCNNConvolution. It is used to correctly size              destination image that is returned on left hand side by encode call MPSCNNConvolutionTranspose as well as automatically set kernelOffsetX/Y on MPSCNNConvolutionTranspose using              the offset and other properties of corresponding MPSCNNConvolution object. During training, same MPSCNNConvolutionGradientState object will be consumed by MPSCNNConvolutionGradient              object and the MPSCNNConvolutionTransposeGradientState produced by MPSCNNConvolutionTranspose's resultStateForSourceImage:sourceStates:destinationImage will be consumed by              MPSCNNConvolutionTransposeGradient object
--
-- Note that state objects are not usable across batches i.e. when batch is done you should nuke the state object and create              new one for next batch.              Weights update process for MPSCNNConvolutionTranspose is same as explained above for MPSCNNConvolution. See comments for MPSCNNConvolutionGradientState.
--
-- Generated bindings for @MPSCNNConvolutionTransposeGradientState@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionTransposeGradientState
  ( MPSCNNConvolutionTransposeGradientState
  , IsMPSCNNConvolutionTransposeGradientState(..)
  , convolutionTranspose
  , convolutionTransposeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | convolutionTranspose
--
-- The convolutionTranspose filter that produced the state.
--
-- ObjC selector: @- convolutionTranspose@
convolutionTranspose :: IsMPSCNNConvolutionTransposeGradientState mpscnnConvolutionTransposeGradientState => mpscnnConvolutionTransposeGradientState -> IO (Id MPSCNNConvolutionTranspose)
convolutionTranspose mpscnnConvolutionTransposeGradientState =
  sendMessage mpscnnConvolutionTransposeGradientState convolutionTransposeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @convolutionTranspose@
convolutionTransposeSelector :: Selector '[] (Id MPSCNNConvolutionTranspose)
convolutionTransposeSelector = mkSelector "convolutionTranspose"

