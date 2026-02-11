{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsamplingGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNUpsamplingGradient filter is used for training. It is the backward              filter for the MPSCNNUpsampling filter. It operates on the gradient input,              specifically, it reduces the size of the gradient input in the x and y dimensions.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- The scaleFactor must be an integer value >= 1. The default value is 1.              If scaleFactor == 1, the filter acts as a copy kernel.
--
-- Nearest and bilinear variants are supported.
--
-- For example, for the nearest variant with scaleFactorX = scaleFactorY = 2, the              forward pass produced the following output:
--
-- Input:	    Output:                          a a b b              a b         a a b b              c d         c c d d                          c c d d
--
-- To upsample the image, the input data is replicated.
--
-- And, the backward pass for the above froward pass is computed in the following              way:
--
-- Input:		    Output:              a1 a2 b1 b2              a2 a3 b3 b4	    x y              c1 c2 d1 d2	    z w              c3 c4 d3 d4
--
-- where	x = a1 + a2 + a3 + a4                      y = b1 + b2 + b3 + b4                      z = c1 + c2 + c3 + c4                      w = d1 + d2 + d3 + d4
--
-- Generated bindings for @MPSCNNUpsamplingGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingGradient
  ( MPSCNNUpsamplingGradient
  , IsMPSCNNUpsamplingGradient(..)
  , initWithDevice
  , scaleFactorX
  , scaleFactorY
  , initWithDeviceSelector
  , scaleFactorXSelector
  , scaleFactorYSelector


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
initWithDevice :: IsMPSCNNUpsamplingGradient mpscnnUpsamplingGradient => mpscnnUpsamplingGradient -> RawId -> IO (Id MPSCNNUpsamplingGradient)
initWithDevice mpscnnUpsamplingGradient  device =
  sendMsg mpscnnUpsamplingGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | scaleFactorX
--
-- The downsampling scale factor for the x dimension. The default value is 1.
--
-- ObjC selector: @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingGradient mpscnnUpsamplingGradient => mpscnnUpsamplingGradient -> IO CDouble
scaleFactorX mpscnnUpsamplingGradient  =
  sendMsg mpscnnUpsamplingGradient (mkSelector "scaleFactorX") retCDouble []

-- | scaleFactorY
--
-- The downsampling scale factor for the y dimension. The default value is 1.
--
-- ObjC selector: @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingGradient mpscnnUpsamplingGradient => mpscnnUpsamplingGradient -> IO CDouble
scaleFactorY mpscnnUpsamplingGradient  =
  sendMsg mpscnnUpsamplingGradient (mkSelector "scaleFactorY") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector
scaleFactorYSelector = mkSelector "scaleFactorY"

