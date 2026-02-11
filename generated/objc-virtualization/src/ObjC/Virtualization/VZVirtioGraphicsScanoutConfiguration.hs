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
  , initWithWidthInPixels_heightInPixelsSelector
  , widthInPixelsSelector
  , setWidthInPixelsSelector
  , heightInPixelsSelector
  , setHeightInPixelsSelector


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
import ObjC.Foundation.Internal.Classes

-- | Create a scanout configuration with the specified pixel dimensions.
--
-- @widthInPixels@ — The width of the scanout, in pixels.
--
-- @heightInPixels@ — The height of the scanout, in pixels.
--
-- ObjC selector: @- initWithWidthInPixels:heightInPixels:@
initWithWidthInPixels_heightInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> CLong -> CLong -> IO (Id VZVirtioGraphicsScanoutConfiguration)
initWithWidthInPixels_heightInPixels vzVirtioGraphicsScanoutConfiguration  widthInPixels heightInPixels =
  sendMsg vzVirtioGraphicsScanoutConfiguration (mkSelector "initWithWidthInPixels:heightInPixels:") (retPtr retVoid) [argCLong (fromIntegral widthInPixels), argCLong (fromIntegral heightInPixels)] >>= ownedObject . castPtr

-- | The width of the scanout, in pixels.
--
-- ObjC selector: @- widthInPixels@
widthInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> IO CLong
widthInPixels vzVirtioGraphicsScanoutConfiguration  =
  sendMsg vzVirtioGraphicsScanoutConfiguration (mkSelector "widthInPixels") retCLong []

-- | The width of the scanout, in pixels.
--
-- ObjC selector: @- setWidthInPixels:@
setWidthInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> CLong -> IO ()
setWidthInPixels vzVirtioGraphicsScanoutConfiguration  value =
  sendMsg vzVirtioGraphicsScanoutConfiguration (mkSelector "setWidthInPixels:") retVoid [argCLong (fromIntegral value)]

-- | The height of the scanout, in pixels.
--
-- ObjC selector: @- heightInPixels@
heightInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> IO CLong
heightInPixels vzVirtioGraphicsScanoutConfiguration  =
  sendMsg vzVirtioGraphicsScanoutConfiguration (mkSelector "heightInPixels") retCLong []

-- | The height of the scanout, in pixels.
--
-- ObjC selector: @- setHeightInPixels:@
setHeightInPixels :: IsVZVirtioGraphicsScanoutConfiguration vzVirtioGraphicsScanoutConfiguration => vzVirtioGraphicsScanoutConfiguration -> CLong -> IO ()
setHeightInPixels vzVirtioGraphicsScanoutConfiguration  value =
  sendMsg vzVirtioGraphicsScanoutConfiguration (mkSelector "setHeightInPixels:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWidthInPixels:heightInPixels:@
initWithWidthInPixels_heightInPixelsSelector :: Selector
initWithWidthInPixels_heightInPixelsSelector = mkSelector "initWithWidthInPixels:heightInPixels:"

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

