{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOpenGLLayer@.
module ObjC.AppKit.NSOpenGLLayer
  ( NSOpenGLLayer
  , IsNSOpenGLLayer(..)
  , openGLPixelFormatForDisplayMask
  , openGLContextForPixelFormat
  , canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTime
  , drawInOpenGLContext_pixelFormat_forLayerTime_displayTime
  , view
  , setView
  , openGLPixelFormat
  , setOpenGLPixelFormat
  , openGLContext
  , setOpenGLContext
  , canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector
  , drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector
  , openGLContextForPixelFormatSelector
  , openGLContextSelector
  , openGLPixelFormatForDisplayMaskSelector
  , openGLPixelFormatSelector
  , setOpenGLContextSelector
  , setOpenGLPixelFormatSelector
  , setViewSelector
  , viewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- openGLPixelFormatForDisplayMask:@
openGLPixelFormatForDisplayMask :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> CUInt -> IO (Id NSOpenGLPixelFormat)
openGLPixelFormatForDisplayMask nsOpenGLLayer mask =
  sendMessage nsOpenGLLayer openGLPixelFormatForDisplayMaskSelector mask

-- | @- openGLContextForPixelFormat:@
openGLContextForPixelFormat :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLPixelFormat pixelFormat) => nsOpenGLLayer -> pixelFormat -> IO (Id NSOpenGLContext)
openGLContextForPixelFormat nsOpenGLLayer pixelFormat =
  sendMessage nsOpenGLLayer openGLContextForPixelFormatSelector (toNSOpenGLPixelFormat pixelFormat)

-- | @- canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTime :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLContext context, IsNSOpenGLPixelFormat pixelFormat) => nsOpenGLLayer -> context -> pixelFormat -> CDouble -> Const RawId -> IO Bool
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTime nsOpenGLLayer context pixelFormat t ts =
  sendMessage nsOpenGLLayer canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector (toNSOpenGLContext context) (toNSOpenGLPixelFormat pixelFormat) t ts

-- | @- drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
drawInOpenGLContext_pixelFormat_forLayerTime_displayTime :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLContext context, IsNSOpenGLPixelFormat pixelFormat) => nsOpenGLLayer -> context -> pixelFormat -> CDouble -> Const RawId -> IO ()
drawInOpenGLContext_pixelFormat_forLayerTime_displayTime nsOpenGLLayer context pixelFormat t ts =
  sendMessage nsOpenGLLayer drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector (toNSOpenGLContext context) (toNSOpenGLPixelFormat pixelFormat) t ts

-- | @- view@
view :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> IO (Id NSView)
view nsOpenGLLayer =
  sendMessage nsOpenGLLayer viewSelector

-- | @- setView:@
setView :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSView value) => nsOpenGLLayer -> value -> IO ()
setView nsOpenGLLayer value =
  sendMessage nsOpenGLLayer setViewSelector (toNSView value)

-- | @- openGLPixelFormat@
openGLPixelFormat :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> IO (Id NSOpenGLPixelFormat)
openGLPixelFormat nsOpenGLLayer =
  sendMessage nsOpenGLLayer openGLPixelFormatSelector

-- | @- setOpenGLPixelFormat:@
setOpenGLPixelFormat :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLPixelFormat value) => nsOpenGLLayer -> value -> IO ()
setOpenGLPixelFormat nsOpenGLLayer value =
  sendMessage nsOpenGLLayer setOpenGLPixelFormatSelector (toNSOpenGLPixelFormat value)

-- | @- openGLContext@
openGLContext :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> IO (Id NSOpenGLContext)
openGLContext nsOpenGLLayer =
  sendMessage nsOpenGLLayer openGLContextSelector

-- | @- setOpenGLContext:@
setOpenGLContext :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLContext value) => nsOpenGLLayer -> value -> IO ()
setOpenGLContext nsOpenGLLayer value =
  sendMessage nsOpenGLLayer setOpenGLContextSelector (toNSOpenGLContext value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openGLPixelFormatForDisplayMask:@
openGLPixelFormatForDisplayMaskSelector :: Selector '[CUInt] (Id NSOpenGLPixelFormat)
openGLPixelFormatForDisplayMaskSelector = mkSelector "openGLPixelFormatForDisplayMask:"

-- | @Selector@ for @openGLContextForPixelFormat:@
openGLContextForPixelFormatSelector :: Selector '[Id NSOpenGLPixelFormat] (Id NSOpenGLContext)
openGLContextForPixelFormatSelector = mkSelector "openGLContextForPixelFormat:"

-- | @Selector@ for @canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector :: Selector '[Id NSOpenGLContext, Id NSOpenGLPixelFormat, CDouble, Const RawId] Bool
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector = mkSelector "canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:"

-- | @Selector@ for @drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector :: Selector '[Id NSOpenGLContext, Id NSOpenGLPixelFormat, CDouble, Const RawId] ()
drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector = mkSelector "drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @openGLPixelFormat@
openGLPixelFormatSelector :: Selector '[] (Id NSOpenGLPixelFormat)
openGLPixelFormatSelector = mkSelector "openGLPixelFormat"

-- | @Selector@ for @setOpenGLPixelFormat:@
setOpenGLPixelFormatSelector :: Selector '[Id NSOpenGLPixelFormat] ()
setOpenGLPixelFormatSelector = mkSelector "setOpenGLPixelFormat:"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector '[] (Id NSOpenGLContext)
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @setOpenGLContext:@
setOpenGLContextSelector :: Selector '[Id NSOpenGLContext] ()
setOpenGLContextSelector = mkSelector "setOpenGLContext:"

