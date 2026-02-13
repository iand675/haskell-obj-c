{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReshapeGradient
--
-- This depends on Metal.framework
--
-- The reshape gradient filter reshapes the incoming gradient into the dimensions              of the forward reshape kernel's source image.
--
-- Generated bindings for @MPSNNReshapeGradient@.
module ObjC.MetalPerformanceShaders.MPSNNReshapeGradient
  ( MPSNNReshapeGradient
  , IsMPSNNReshapeGradient(..)
  , initWithDevice
  , initWithCoder_device
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a MPSNNReshapeGradient function
--
-- @device@ — The MTLDevice on which this filter will be used
--
-- Returns: A valid MPSNNReshapeGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReshapeGradient mpsnnReshapeGradient => mpsnnReshapeGradient -> RawId -> IO (Id MPSNNReshapeGradient)
initWithDevice mpsnnReshapeGradient device =
  sendOwnedMessage mpsnnReshapeGradient initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSNNReshapeGradient mpsnnReshapeGradient, IsNSCoder aDecoder) => mpsnnReshapeGradient -> aDecoder -> RawId -> IO (Id MPSNNReshapeGradient)
initWithCoder_device mpsnnReshapeGradient aDecoder device =
  sendOwnedMessage mpsnnReshapeGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReshapeGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReshapeGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

