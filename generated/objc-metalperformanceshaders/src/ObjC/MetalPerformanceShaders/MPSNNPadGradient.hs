{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNPadGradient
--
-- This depends on Metal.framework
--
-- Computes the gradient for the MPSNNPad layer.              Since the padding forward operation typically increases the size of the image, the gradient operation              decreases it. In case of zero or constant padding forward operation the gradient operation slices the              input gradient and in other edge modes the padded values copied in the forward operation are              summed together in the gradient operation.              For Example for the MPSImageEdgeModeClamp the forward operation with offset = -2, destSize = 8              or paddingSizeBefore = 2, paddingSizeAfter = 3, sourceSize = 3:
--
-- Source Image:
-- |--------------|
-- | x0 | x1 | x2 |
-- |--------------|
-- Destination Image:
-- |---------------------------------------|
-- | x0 | x0 | x0 | x1 | x2 | x2 | x2 | x2 |
-- |---------------------------------------|
--
-- Then the gradient operation becomes:
--
-- Source Gradient Image:
-- |---------------------------------------|
-- | d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7 |
-- |---------------------------------------|
-- Destination Gradient Image:
-- |-----------------------------|
-- | d0+d1+d2 | d3 | d4+d5+d6+d7 |
-- |-----------------------------|
--
-- Another example with MPSImageEdgeModeMirror, the forward operation with offset = -4, destSize = 8              or paddingSizeBefore = 4, paddingSizeAfter = 1, sourceSize = 3:
--
-- Source Image:
-- |--------------|
-- | x0 | x1 | x2 |
-- |--------------|
-- Destination Image:
-- |---------------------------------------|
-- | x0 | x1 | x2 | x1 | x0 | x1 | x2 | x1 |
-- |---------------------------------------|
--
-- Then the gradient operation becomes:
--
-- Source Gradient Image:
-- |---------------------------------------|
-- | d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7 |
-- |---------------------------------------|
-- Destination Gradient Image:
-- |-----------------------------|
-- | d0+d4 | d1+d3+d5+d7 | d2+d6 |
-- |-----------------------------|
--
-- NOTE: There are no channel fill-values to use with MPSImageEdgeModeConstant              since the gradient values are independent of the constant of the forward pass.              NOTE: In case the forward pass defined a slice operation in feature channels then              the channels not read in the forward pass will be filled with zeros in the gradient pass.
--
-- Generated bindings for @MPSNNPadGradient@.
module ObjC.MetalPerformanceShaders.MPSNNPadGradient
  ( MPSNNPadGradient
  , IsMPSNNPadGradient(..)
  , initWithDevice
  , initWithCoder_device
  , initWithDeviceSelector
  , initWithCoder_deviceSelector


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
import ObjC.Foundation.Internal.Classes

-- | Initializes a MPSNNPadGradient filter
--
-- @device@ — The MTLDevice on which this filter will be used
--
-- Returns: A valid MPSNNPadGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNPadGradient mpsnnPadGradient => mpsnnPadGradient -> RawId -> IO (Id MPSNNPadGradient)
initWithDevice mpsnnPadGradient  device =
  sendMsg mpsnnPadGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSNNPadGradient.
--
-- @device@ — The MTLDevice on which to make the MPSNNPadGradient.
--
-- Returns: A new MPSNNPadGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNPadGradient mpsnnPadGradient, IsNSCoder aDecoder) => mpsnnPadGradient -> aDecoder -> RawId -> IO (Id MPSNNPadGradient)
initWithCoder_device mpsnnPadGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnPadGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

