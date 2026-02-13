{-# LANGUAGE DataKinds #-}
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
  , heightInPixelsSelector
  , initForScreen_sizeInPointsSelector
  , initWithWidthInPixels_heightInPixels_pixelsPerInchSelector
  , pixelsPerInchSelector
  , setHeightInPixelsSelector
  , setPixelsPerInchSelector
  , setWidthInPixelsSelector
  , widthInPixelsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithWidthInPixels_heightInPixels_pixelsPerInch vzMacGraphicsDisplayConfiguration widthInPixels heightInPixels pixelsPerInch =
  sendOwnedMessage vzMacGraphicsDisplayConfiguration initWithWidthInPixels_heightInPixels_pixelsPerInchSelector widthInPixels heightInPixels pixelsPerInch

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
initForScreen_sizeInPoints vzMacGraphicsDisplayConfiguration screen sizeInPoints =
  sendOwnedMessage vzMacGraphicsDisplayConfiguration initForScreen_sizeInPointsSelector (toNSScreen screen) sizeInPoints

-- | The width of the display, in pixels.
--
-- ObjC selector: @- widthInPixels@
widthInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> IO CLong
widthInPixels vzMacGraphicsDisplayConfiguration =
  sendMessage vzMacGraphicsDisplayConfiguration widthInPixelsSelector

-- | The width of the display, in pixels.
--
-- ObjC selector: @- setWidthInPixels:@
setWidthInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> IO ()
setWidthInPixels vzMacGraphicsDisplayConfiguration value =
  sendMessage vzMacGraphicsDisplayConfiguration setWidthInPixelsSelector value

-- | The height of the display, in pixels.
--
-- ObjC selector: @- heightInPixels@
heightInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> IO CLong
heightInPixels vzMacGraphicsDisplayConfiguration =
  sendMessage vzMacGraphicsDisplayConfiguration heightInPixelsSelector

-- | The height of the display, in pixels.
--
-- ObjC selector: @- setHeightInPixels:@
setHeightInPixels :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> IO ()
setHeightInPixels vzMacGraphicsDisplayConfiguration value =
  sendMessage vzMacGraphicsDisplayConfiguration setHeightInPixelsSelector value

-- | The pixel density as a number of pixels per inch.
--
-- ObjC selector: @- pixelsPerInch@
pixelsPerInch :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> IO CLong
pixelsPerInch vzMacGraphicsDisplayConfiguration =
  sendMessage vzMacGraphicsDisplayConfiguration pixelsPerInchSelector

-- | The pixel density as a number of pixels per inch.
--
-- ObjC selector: @- setPixelsPerInch:@
setPixelsPerInch :: IsVZMacGraphicsDisplayConfiguration vzMacGraphicsDisplayConfiguration => vzMacGraphicsDisplayConfiguration -> CLong -> IO ()
setPixelsPerInch vzMacGraphicsDisplayConfiguration value =
  sendMessage vzMacGraphicsDisplayConfiguration setPixelsPerInchSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWidthInPixels:heightInPixels:pixelsPerInch:@
initWithWidthInPixels_heightInPixels_pixelsPerInchSelector :: Selector '[CLong, CLong, CLong] (Id VZMacGraphicsDisplayConfiguration)
initWithWidthInPixels_heightInPixels_pixelsPerInchSelector = mkSelector "initWithWidthInPixels:heightInPixels:pixelsPerInch:"

-- | @Selector@ for @initForScreen:sizeInPoints:@
initForScreen_sizeInPointsSelector :: Selector '[Id NSScreen, NSSize] (Id VZMacGraphicsDisplayConfiguration)
initForScreen_sizeInPointsSelector = mkSelector "initForScreen:sizeInPoints:"

-- | @Selector@ for @widthInPixels@
widthInPixelsSelector :: Selector '[] CLong
widthInPixelsSelector = mkSelector "widthInPixels"

-- | @Selector@ for @setWidthInPixels:@
setWidthInPixelsSelector :: Selector '[CLong] ()
setWidthInPixelsSelector = mkSelector "setWidthInPixels:"

-- | @Selector@ for @heightInPixels@
heightInPixelsSelector :: Selector '[] CLong
heightInPixelsSelector = mkSelector "heightInPixels"

-- | @Selector@ for @setHeightInPixels:@
setHeightInPixelsSelector :: Selector '[CLong] ()
setHeightInPixelsSelector = mkSelector "setHeightInPixels:"

-- | @Selector@ for @pixelsPerInch@
pixelsPerInchSelector :: Selector '[] CLong
pixelsPerInchSelector = mkSelector "pixelsPerInch"

-- | @Selector@ for @setPixelsPerInch:@
setPixelsPerInchSelector :: Selector '[CLong] ()
setPixelsPerInchSelector = mkSelector "setPixelsPerInch:"

