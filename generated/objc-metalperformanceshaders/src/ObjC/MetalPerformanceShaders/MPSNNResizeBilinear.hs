{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNResizeBilinear
--
-- This depends on Metal.framework
--
-- The MPSNNResizeBilinear filter resizes the source image  using bilinear interpolation to              a destination whose dimensions are given by resizeWidth and resizeHeight
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- Generated bindings for @MPSNNResizeBilinear@.
module ObjC.MetalPerformanceShaders.MPSNNResizeBilinear
  ( MPSNNResizeBilinear
  , IsMPSNNResizeBilinear(..)
  , initWithDevice
  , initWithDevice_resizeWidth_resizeHeight_alignCorners
  , initWithCoder_device
  , resizeWidth
  , resizeHeight
  , alignCorners
  , initWithDeviceSelector
  , initWithDevice_resizeWidth_resizeHeight_alignCornersSelector
  , initWithCoder_deviceSelector
  , resizeWidthSelector
  , resizeHeightSelector
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
initWithDevice :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> RawId -> IO (Id MPSNNResizeBilinear)
initWithDevice mpsnnResizeBilinear  device =
  sendMsg mpsnnResizeBilinear (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the resize bilinear filter.
--
-- @device@ — The device the filter will run on.
--
-- @resizeWidth@ — The destination resize width in pixels
--
-- @resizeHeight@ — The destination resize height in pixels
--
-- @alignCorners@ — Specifier whether the centers of the 4 corner pixels of the input and output regions are aligned,                                      preserving the values at the corner pixels.
--
-- Returns: A valid MPSNNResizeBilinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:resizeWidth:resizeHeight:alignCorners:@
initWithDevice_resizeWidth_resizeHeight_alignCorners :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> RawId -> CULong -> CULong -> Bool -> IO (Id MPSNNResizeBilinear)
initWithDevice_resizeWidth_resizeHeight_alignCorners mpsnnResizeBilinear  device resizeWidth resizeHeight alignCorners =
  sendMsg mpsnnResizeBilinear (mkSelector "initWithDevice:resizeWidth:resizeHeight:alignCorners:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral resizeWidth), argCULong (fromIntegral resizeHeight), argCULong (if alignCorners then 1 else 0)] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSNNResizeBilinear
--
-- @device@ — The MTLDevice on which to make the MPSNNResizeBilinear
--
-- Returns: A new MPSNNResizeBilinear object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNResizeBilinear mpsnnResizeBilinear, IsNSCoder aDecoder) => mpsnnResizeBilinear -> aDecoder -> RawId -> IO (Id MPSNNResizeBilinear)
initWithCoder_device mpsnnResizeBilinear  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnResizeBilinear (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | resizeWidth
--
-- The resize width.
--
-- ObjC selector: @- resizeWidth@
resizeWidth :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> IO CULong
resizeWidth mpsnnResizeBilinear  =
  sendMsg mpsnnResizeBilinear (mkSelector "resizeWidth") retCULong []

-- | resizeHeight
--
-- The resize height.
--
-- ObjC selector: @- resizeHeight@
resizeHeight :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> IO CULong
resizeHeight mpsnnResizeBilinear  =
  sendMsg mpsnnResizeBilinear (mkSelector "resizeHeight") retCULong []

-- | alignCorners
--
-- If YES, the centers of the 4 corner pixels of the input and output regions are aligned,              preserving the values at the corner pixels.              The default is NO.
--
-- ObjC selector: @- alignCorners@
alignCorners :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> IO Bool
alignCorners mpsnnResizeBilinear  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnResizeBilinear (mkSelector "alignCorners") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:resizeWidth:resizeHeight:alignCorners:@
initWithDevice_resizeWidth_resizeHeight_alignCornersSelector :: Selector
initWithDevice_resizeWidth_resizeHeight_alignCornersSelector = mkSelector "initWithDevice:resizeWidth:resizeHeight:alignCorners:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @resizeWidth@
resizeWidthSelector :: Selector
resizeWidthSelector = mkSelector "resizeWidth"

-- | @Selector@ for @resizeHeight@
resizeHeightSelector :: Selector
resizeHeightSelector = mkSelector "resizeHeight"

-- | @Selector@ for @alignCorners@
alignCornersSelector :: Selector
alignCornersSelector = mkSelector "alignCorners"

