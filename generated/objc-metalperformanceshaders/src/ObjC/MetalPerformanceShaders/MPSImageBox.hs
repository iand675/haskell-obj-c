{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageBox
--
-- The MPSImageBox convolves an image with given filter of odd width and height. The kernel elements              all have equal weight, achieving a blur effect. (Each result is the unweighted average of the              surrounding pixels.) This allows for much faster algorithms, espcially for larger blur radii.              The box height and width must be odd numbers. The box blur is a separable filter. The implementation               is aware of this and will act accordingly to give best performance for multi-dimensional blurs.
--
-- Generated bindings for @MPSImageBox@.
module ObjC.MetalPerformanceShaders.MPSImageBox
  ( MPSImageBox
  , IsMPSImageBox(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithCoder_device
  , initWithDevice
  , kernelHeight
  , kernelWidth
  , initWithDevice_kernelWidth_kernelHeightSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , kernelHeightSelector
  , kernelWidthSelector


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

-- | Initialize a filter for a particular kernel size and device
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — the width of the kernel.  Must be an odd number.
--
-- @kernelHeight@ — the height of the kernel. Must be an odd number.
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeight :: IsMPSImageBox mpsImageBox => mpsImageBox -> RawId -> CULong -> CULong -> IO (Id MPSImageBox)
initWithDevice_kernelWidth_kernelHeight mpsImageBox  device kernelWidth kernelHeight =
  sendMsg mpsImageBox (mkSelector "initWithDevice:kernelWidth:kernelHeight:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageBox mpsImageBox, IsNSCoder aDecoder) => mpsImageBox -> aDecoder -> RawId -> IO (Id MPSImageBox)
initWithCoder_device mpsImageBox  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageBox (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageBox mpsImageBox => mpsImageBox -> RawId -> IO (Id MPSImageBox)
initWithDevice mpsImageBox  device =
  sendMsg mpsImageBox (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | kernelHeight
--
-- The height of the filter window.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSImageBox mpsImageBox => mpsImageBox -> IO CULong
kernelHeight mpsImageBox  =
  sendMsg mpsImageBox (mkSelector "kernelHeight") retCULong []

-- | kernelWidth
--
-- The width of the filter window.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSImageBox mpsImageBox => mpsImageBox -> IO CULong
kernelWidth mpsImageBox  =
  sendMsg mpsImageBox (mkSelector "kernelWidth") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

