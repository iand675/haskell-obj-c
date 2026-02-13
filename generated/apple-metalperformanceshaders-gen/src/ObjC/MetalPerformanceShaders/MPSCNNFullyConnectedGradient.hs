{-# LANGUAGE DataKinds #-}
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
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDevice_weights mpscnnFullyConnectedGradient device weights =
  sendOwnedMessage mpscnnFullyConnectedGradient initWithDevice_weightsSelector device weights

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
initWithCoder_device mpscnnFullyConnectedGradient aDecoder device =
  sendOwnedMessage mpscnnFullyConnectedGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNFullyConnectedGradient mpscnnFullyConnectedGradient => mpscnnFullyConnectedGradient -> RawId -> IO (Id MPSCNNFullyConnectedGradient)
initWithDevice mpscnnFullyConnectedGradient device =
  sendOwnedMessage mpscnnFullyConnectedGradient initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector '[RawId, RawId] (Id MPSCNNFullyConnectedGradient)
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNFullyConnectedGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNFullyConnectedGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

