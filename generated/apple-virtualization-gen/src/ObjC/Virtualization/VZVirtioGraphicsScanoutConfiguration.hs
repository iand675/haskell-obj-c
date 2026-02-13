{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a scanout attached to a Virtio graphics device.
--
-- This scanout can be shown in a VZVirtualMachineView.
--
-- VZVirtioGraphicsDeviceConfiguration
--
-- Generated bindings for @VZVirtioGraphicsScanoutConfiguration@.
module ObjC.Virtualization.VZVirtioGraphicsScanoutConfiguration
  ( VZVirtioGraphicsScanoutConfiguration
  , IsVZVirtioGraphicsScanoutConfiguration(..)
  , initWithWidthInPixels_heightInPixels
  , widthInPixels
  , setWidthInPixels
  , heightInPixels
  , setHeightInPixels
  , heightInPixelsSelector
  , initWithWidthInPixels_heightInPixelsSelector
  , setHeightInPixelsSelector
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
import ObjC.Foundation.Internal.Classes

-- | Create a scanout configuration with the specified pixel dimensions.
--
-- @widthInPixels@ — The width of the scanout, in pixels.
--
-- @heightInPixels@ — The height of the scanout, in pixels.
--
-- ObjC selector: @- initWithWidthInPixels:heightInPixels:@
initWithWidthInPixels_heightInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> CLong -> CLong -> IO (Id VZVirtioGraphicsScanoutConfiguration)
initWithWidthInPixels_heightInPixels vzVirtioGraphicsScanoutConfiguration widthInPixels heightInPixels =
  sendOwnedMessage vzVirtioGraphicsScanoutConfiguration initWithWidthInPixels_heightInPixelsSelector widthInPixels heightInPixels

-- | The width of the scanout, in pixels.
--
-- ObjC selector: @- widthInPixels@
widthInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> IO CLong
widthInPixels vzVirtioGraphicsScanoutConfiguration =
  sendMessage vzVirtioGraphicsScanoutConfiguration widthInPixelsSelector

-- | The width of the scanout, in pixels.
--
-- ObjC selector: @- setWidthInPixels:@
setWidthInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> CLong -> IO ()
setWidthInPixels vzVirtioGraphicsScanoutConfiguration value =
  sendMessage vzVirtioGraphicsScanoutConfiguration setWidthInPixelsSelector value

-- | The height of the scanout, in pixels.
--
-- ObjC selector: @- heightInPixels@
heightInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> IO CLong
heightInPixels vzVirtioGraphicsScanoutConfiguration =
  sendMessage vzVirtioGraphicsScanoutConfiguration heightInPixelsSelector

-- | The height of the scanout, in pixels.
--
-- ObjC selector: @- setHeightInPixels:@
setHeightInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> CLong -> IO ()
setHeightInPixels vzVirtioGraphicsScanoutConfiguration value =
  sendMessage vzVirtioGraphicsScanoutConfiguration setHeightInPixelsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWidthInPixels:heightInPixels:@
initWithWidthInPixels_heightInPixelsSelector :: Selector '[CLong, CLong] (Id VZVirtioGraphicsScanoutConfiguration)
initWithWidthInPixels_heightInPixelsSelector = mkSelector "initWithWidthInPixels:heightInPixels:"

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

