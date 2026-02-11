{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNGramMatrixCalculation
--
-- This depends on Metal.framework
--
-- The MPSNNGramMatrixCalculation filter specifies a layer which computes the uncentered cross-correlation              values between the image planes of each feature channel of an image. If the input image batch is              x = x[b, y, x, c], where 'b' is batch index, 'y' and 'x' are the image coordinate and              'c' is the feature channel index then this filter computes the values:
--
-- y = y[b, 1, f, c] = alpha * sum_{x,y} x[b,y,x,f] * x[b,y,x,c], where
--
-- 'alpha' is a scaling factor. This operation can be interpreted to be computing all combinations              of fully connected layers between the different image planes of the input image. The results              are stored in the feature channel and 'x'-coordinate indices of the output batch.              The operation is performed independently on different images in the batch.
--
-- NOTE: Due to the nature of the operation this filter specifies a special padding policy              and hence does not support non-default offset or cliprect properties.
--
-- Generated bindings for @MPSNNGramMatrixCalculation@.
module ObjC.MetalPerformanceShaders.MPSNNGramMatrixCalculation
  ( MPSNNGramMatrixCalculation
  , IsMPSNNGramMatrixCalculation(..)
  , initWithCoder_device
  , initWithDevice_alpha
  , initWithDevice
  , alpha
  , setAlpha
  , initWithCoder_deviceSelector
  , initWithDevice_alphaSelector
  , initWithDeviceSelector
  , alphaSelector
  , setAlphaSelector


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
initWithCoder_device :: (IsMPSNNGramMatrixCalculation mpsnnGramMatrixCalculation, IsNSCoder aDecoder) => mpsnnGramMatrixCalculation -> aDecoder -> RawId -> IO (Id MPSNNGramMatrixCalculation)
initWithCoder_device mpsnnGramMatrixCalculation  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnGramMatrixCalculation (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a MPSNNGramMatrixCalculation kernel.
--
-- @device@ — The MTLDevice on which this MPSNNGramMatrixCalculation filter will be used.
--
-- @alpha@ — Scaling factor for the output.
--
-- Returns: A valid MPSNNGramMatrixCalculation object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:alpha:@
initWithDevice_alpha :: IsMPSNNGramMatrixCalculation mpsnnGramMatrixCalculation => mpsnnGramMatrixCalculation -> RawId -> CFloat -> IO (Id MPSNNGramMatrixCalculation)
initWithDevice_alpha mpsnnGramMatrixCalculation  device alpha =
  sendMsg mpsnnGramMatrixCalculation (mkSelector "initWithDevice:alpha:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral alpha)] >>= ownedObject . castPtr

-- | Initializes a MPSNNGramMatrixCalculation kernel with scaling factor alpha = 1.0f.
--
-- @device@ — The MTLDevice on which this MPSNNGramMatrixCalculation filter will be used.
--
-- Returns: A valid MPSNNGramMatrixCalculation object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNGramMatrixCalculation mpsnnGramMatrixCalculation => mpsnnGramMatrixCalculation -> RawId -> IO (Id MPSNNGramMatrixCalculation)
initWithDevice mpsnnGramMatrixCalculation  device =
  sendMsg mpsnnGramMatrixCalculation (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNNGramMatrixCalculation mpsnnGramMatrixCalculation => mpsnnGramMatrixCalculation -> IO CFloat
alpha mpsnnGramMatrixCalculation  =
  sendMsg mpsnnGramMatrixCalculation (mkSelector "alpha") retCFloat []

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSNNGramMatrixCalculation mpsnnGramMatrixCalculation => mpsnnGramMatrixCalculation -> CFloat -> IO ()
setAlpha mpsnnGramMatrixCalculation  value =
  sendMsg mpsnnGramMatrixCalculation (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:alpha:@
initWithDevice_alphaSelector :: Selector
initWithDevice_alphaSelector = mkSelector "initWithDevice:alpha:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

