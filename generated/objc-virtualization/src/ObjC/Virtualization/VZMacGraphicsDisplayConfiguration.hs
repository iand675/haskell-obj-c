{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a display attached to a Mac graphics device.
--
-- This display can be shown in a VZVirtualMachineView.
--
-- Generated bindings for @VZMacGraphicsDisplayConfiguration@.
module ObjC.Virtualization.VZMacGraphicsDisplayConfiguration
  ( VZMacGraphicsDisplayConfiguration
  , IsVZMacGraphicsDisplayConfiguration(..)
  , initWithWidthInPixels_heightInPixels_pixelsPerInch
  , initForScreen_sizeInPoints
  , widthInPixels
  , setWidthInPixels
  , heightInPixels
  , setHeightInPixels
  , pixelsPerInch
  , setPixelsPerInch
  , initWithWidthInPixels_heightInPixels_pixelsPerInchSelector
  , initForScreen_sizeInPointsSelector
  , widthInPixelsSelector
  , setWidthInPixelsSelector
  , heightInPixelsSelector
  , setHeightInPixelsSelector
  , pixelsPerInchSelector
  , setPixelsPerInchSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a display configuration with the specified pixel dimensions and pixel density.
--
-- @widthInPixels@ — The width of the display, in pixels.
--
-- @heightInPixels@ — The height of the display, in pixels.
--
-- @pixelsPerInch@ — The pixel density as a number of pixels per inch.
--
-- ObjC selector: @- initWithWidthInPixels:heightInPixels:pixelsPerInch:@
initWithWidthInPixels_heightInPixels_pixelsPerInch :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> CLong -> CLong -> IO (Id VZMacGraphicsDisplayConfiguration)
initWithWidthInPixels_heightInPixels_pixelsPerInch vzMacGraphicsDisplayConfiguration  widthInPixels heightInPixels pixelsPerInch =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "initWithWidthInPixels:heightInPixels:pixelsPerInch:") (retPtr retVoid) [argCLong (fromIntegral widthInPixels), argCLong (fromIntegral heightInPixels), argCLong (fromIntegral pixelsPerInch)] >>= ownedObject . castPtr

-- | Create a display configuration suitable for showing on the specified screen.
--
-- @screen@ — The screen on which you intend to present the VZVirtualMachineView for the display.
--
-- @sizeInPoints@ — The intended logical size of the display.
--
-- The pixel dimensions and pixel density will be initialized based on the specified screen and    size. Note: an instance of macOS running in the virtual machine may not necessarily provide    a display mode with a backing scale factor matching the specified screen.
--
-- ObjC selector: @- initForScreen:sizeInPoints:@
initForScreen_sizeInPoints :: (IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration, IsNSScreen screen) => vzMacGraphicsDisplayConfiguration -> screen -> NSSize -> IO (Id VZMacGraphicsDisplayConfiguration)
initForScreen_sizeInPoints vzMacGraphicsDisplayConfiguration  screen sizeInPoints =
withObjCPtr screen $ \raw_screen ->
    sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "initForScreen:sizeInPoints:") (retPtr retVoid) [argPtr (castPtr raw_screen :: Ptr ()), argNSSize sizeInPoints] >>= ownedObject . castPtr

-- | The width of the display, in pixels.
--
-- ObjC selector: @- widthInPixels@
widthInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> IO CLong
widthInPixels vzMacGraphicsDisplayConfiguration  =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "widthInPixels") retCLong []

-- | The width of the display, in pixels.
--
-- ObjC selector: @- setWidthInPixels:@
setWidthInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> IO ()
setWidthInPixels vzMacGraphicsDisplayConfiguration  value =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "setWidthInPixels:") retVoid [argCLong (fromIntegral value)]

-- | The height of the display, in pixels.
--
-- ObjC selector: @- heightInPixels@
heightInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> IO CLong
heightInPixels vzMacGraphicsDisplayConfiguration  =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "heightInPixels") retCLong []

-- | The height of the display, in pixels.
--
-- ObjC selector: @- setHeightInPixels:@
setHeightInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> IO ()
setHeightInPixels vzMacGraphicsDisplayConfiguration  value =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "setHeightInPixels:") retVoid [argCLong (fromIntegral value)]

-- | The pixel density as a number of pixels per inch.
--
-- ObjC selector: @- pixelsPerInch@
pixelsPerInch :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> IO CLong
pixelsPerInch vzMacGraphicsDisplayConfiguration  =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "pixelsPerInch") retCLong []

-- | The pixel density as a number of pixels per inch.
--
-- ObjC selector: @- setPixelsPerInch:@
setPixelsPerInch :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> IO ()
setPixelsPerInch vzMacGraphicsDisplayConfiguration  value =
  sendMsg vzMacGraphicsDisplayConfiguration (mkSelector "setPixelsPerInch:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWidthInPixels:heightInPixels:pixelsPerInch:@
initWithWidthInPixels_heightInPixels_pixelsPerInchSelector :: Selector
initWithWidthInPixels_heightInPixels_pixelsPerInchSelector = mkSelector "initWithWidthInPixels:heightInPixels:pixelsPerInch:"

-- | @Selector@ for @initForScreen:sizeInPoints:@
initForScreen_sizeInPointsSelector :: Selector
initForScreen_sizeInPointsSelector = mkSelector "initForScreen:sizeInPoints:"

-- | @Selector@ for @widthInPixels@
widthInPixelsSelector :: Selector
widthInPixelsSelector = mkSelector "widthInPixels"

-- | @Selector@ for @setWidthInPixels:@
setWidthInPixelsSelector :: Selector
setWidthInPixelsSelector = mkSelector "setWidthInPixels:"

-- | @Selector@ for @heightInPixels@
heightInPixelsSelector :: Selector
heightInPixelsSelector = mkSelector "heightInPixels"

-- | @Selector@ for @setHeightInPixels:@
setHeightInPixelsSelector :: Selector
setHeightInPixelsSelector = mkSelector "setHeightInPixels:"

-- | @Selector@ for @pixelsPerInch@
pixelsPerInchSelector :: Selector
pixelsPerInchSelector = mkSelector "pixelsPerInch"

-- | @Selector@ for @setPixelsPerInch:@
setPixelsPerInchSelector :: Selector
setPixelsPerInchSelector = mkSelector "setPixelsPerInch:"

