{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsampling
--
-- This depends on Metal.framework
--
-- The MPSCNNUpsampling filter can be used to resample an existing MPSImage              using a different sampling frequency for the x and y dimensions with the purpose of              enlarging the size of an image.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- The scaleFactor must be an integer value >= 1. The default value is 1.              If scaleFactor == 1, the filter acts as a copy kernel.
--
-- Nearest and bilinear variants are supported.
--
-- Generated bindings for @MPSCNNUpsampling@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsampling
  ( MPSCNNUpsampling
  , IsMPSCNNUpsampling(..)
  , initWithDevice
  , scaleFactorX
  , scaleFactorY
  , alignCorners
  , initWithDeviceSelector
  , scaleFactorXSelector
  , scaleFactorYSelector
  , alignCornersSelector


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
initWithDevice :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> RawId -> IO (Id MPSCNNUpsampling)
initWithDevice mpscnnUpsampling  device =
  sendMsg mpscnnUpsampling (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | scaleFactorX
--
-- The upsampling scale factor for the x dimension. The default value is 1.
--
-- ObjC selector: @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> IO CDouble
scaleFactorX mpscnnUpsampling  =
  sendMsg mpscnnUpsampling (mkSelector "scaleFactorX") retCDouble []

-- | scaleFactorY
--
-- The upsampling scale factor for the y dimension. The default value is 1.
--
-- ObjC selector: @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> IO CDouble
scaleFactorY mpscnnUpsampling  =
  sendMsg mpscnnUpsampling (mkSelector "scaleFactorY") retCDouble []

-- | alignCorners
--
-- If YES, the centers of the 4 corner pixels of the input and output regions are aligned,              preserving the values at the corner pixels.              The default is NO.
--
-- ObjC selector: @- alignCorners@
alignCorners :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> IO Bool
alignCorners mpscnnUpsampling  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnUpsampling (mkSelector "alignCorners") retCULong []

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

-- | @Selector@ for @alignCorners@
alignCornersSelector :: Selector
alignCornersSelector = mkSelector "alignCorners"

