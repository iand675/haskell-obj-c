{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNFullyConnectedGradient
--
-- This depends on Metal.framework
--
-- Compute the gradient for fully connected layer.
--
-- Generated bindings for @MPSCNNFullyConnectedGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNFullyConnectedGradient
  ( MPSCNNFullyConnectedGradient
  , IsMPSCNNFullyConnectedGradient(..)
  , initWithDevice_weights
  , initWithCoder_device
  , initWithDevice
  , initWithDevice_weightsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


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

-- | Initializes a convolution gradient (with respect to weights and bias) object.
--
-- @device@ — The MTLDevice on which this MPSCNNConvolutionGradient filter will be used
--
-- @weights@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource                                              protocol. Note that same data source as provided to forward convolution should be used.
--
-- Returns: A valid MPSCNNConvolutionGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:weights:@
initWithDevice_weights :: IsMPSCNNFullyConnectedGradient mpscnnFullyConnectedGradient => mpscnnFullyConnectedGradient -> RawId -> RawId -> IO (Id MPSCNNFullyConnectedGradient)
initWithDevice_weights mpscnnFullyConnectedGradient  device weights =
  sendMsg mpscnnFullyConnectedGradient (mkSelector "initWithDevice:weights:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNFullyConnectedGradient mpscnnFullyConnectedGradient, IsNSCoder aDecoder) => mpscnnFullyConnectedGradient -> aDecoder -> RawId -> IO (Id MPSCNNFullyConnectedGradient)
initWithCoder_device mpscnnFullyConnectedGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnFullyConnectedGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNFullyConnectedGradient mpscnnFullyConnectedGradient => mpscnnFullyConnectedGradient -> RawId -> IO (Id MPSCNNFullyConnectedGradient)
initWithDevice mpscnnFullyConnectedGradient  device =
  sendMsg mpscnnFullyConnectedGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

