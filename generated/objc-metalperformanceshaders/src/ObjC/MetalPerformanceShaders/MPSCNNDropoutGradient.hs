{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDropoutGradient
--
-- This depends on Metal.framework
--
-- This filter is the backward filter for the MPSCNNDropout forward filter.              It requires the mask data, along with all the associated parameters used              to generate the mask, from the forward pass. The mask is associated with              a MPSCNNDropoutGradientState object.
--
-- In this kernel, use the secondaryOffset to apply an offset to the mask data.
--
-- Generated bindings for @MPSCNNDropoutGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNDropoutGradient
  ( MPSCNNDropoutGradient
  , IsMPSCNNDropoutGradient(..)
  , initWithDevice
  , initWithCoder_device
  , keepProbability
  , seed
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , keepProbabilitySelector
  , seedSelector


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

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNDropoutGradient mpscnnDropoutGradient => mpscnnDropoutGradient -> RawId -> IO (Id MPSCNNDropoutGradient)
initWithDevice mpscnnDropoutGradient  device =
  sendMsg mpscnnDropoutGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDropoutGradient mpscnnDropoutGradient, IsNSCoder aDecoder) => mpscnnDropoutGradient -> aDecoder -> RawId -> IO (Id MPSCNNDropoutGradient)
initWithCoder_device mpscnnDropoutGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnDropoutGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | keepProbability
--
-- The probability that each element in the input is kept.              The valid range is (0.0f, 1.0f).
--
-- ObjC selector: @- keepProbability@
keepProbability :: IsMPSCNNDropoutGradient mpscnnDropoutGradient => mpscnnDropoutGradient -> IO CFloat
keepProbability mpscnnDropoutGradient  =
  sendMsg mpscnnDropoutGradient (mkSelector "keepProbability") retCFloat []

-- | seed
--
-- The seed used to generate random numbers.
--
-- ObjC selector: @- seed@
seed :: IsMPSCNNDropoutGradient mpscnnDropoutGradient => mpscnnDropoutGradient -> IO CULong
seed mpscnnDropoutGradient  =
  sendMsg mpscnnDropoutGradient (mkSelector "seed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

