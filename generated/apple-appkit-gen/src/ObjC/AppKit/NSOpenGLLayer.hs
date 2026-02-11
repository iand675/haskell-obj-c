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
  , openGLPixelFormatForDisplayMaskSelector
  , openGLContextForPixelFormatSelector
  , canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector
  , drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector
  , viewSelector
  , setViewSelector
  , openGLPixelFormatSelector
  , setOpenGLPixelFormatSelector
  , openGLContextSelector
  , setOpenGLContextSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- openGLPixelFormatForDisplayMask:@
openGLPixelFormatForDisplayMask :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> CUInt -> IO (Id NSOpenGLPixelFormat)
openGLPixelFormatForDisplayMask nsOpenGLLayer  mask =
    sendMsg nsOpenGLLayer (mkSelector "openGLPixelFormatForDisplayMask:") (retPtr retVoid) [argCUInt mask] >>= retainedObject . castPtr

-- | @- openGLContextForPixelFormat:@
openGLContextForPixelFormat :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLPixelFormat pixelFormat) => nsOpenGLLayer -> pixelFormat -> IO (Id NSOpenGLContext)
openGLContextForPixelFormat nsOpenGLLayer  pixelFormat =
  withObjCPtr pixelFormat $ \raw_pixelFormat ->
      sendMsg nsOpenGLLayer (mkSelector "openGLContextForPixelFormat:") (retPtr retVoid) [argPtr (castPtr raw_pixelFormat :: Ptr ())] >>= retainedObject . castPtr

-- | @- canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTime :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLContext context, IsNSOpenGLPixelFormat pixelFormat) => nsOpenGLLayer -> context -> pixelFormat -> CDouble -> Const RawId -> IO Bool
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTime nsOpenGLLayer  context pixelFormat t ts =
  withObjCPtr context $ \raw_context ->
    withObjCPtr pixelFormat $ \raw_pixelFormat ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenGLLayer (mkSelector "canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:") retCULong [argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_pixelFormat :: Ptr ()), argCDouble t, argPtr (castPtr (unRawId (unConst ts)) :: Ptr ())]

-- | @- drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
drawInOpenGLContext_pixelFormat_forLayerTime_displayTime :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLContext context, IsNSOpenGLPixelFormat pixelFormat) => nsOpenGLLayer -> context -> pixelFormat -> CDouble -> Const RawId -> IO ()
drawInOpenGLContext_pixelFormat_forLayerTime_displayTime nsOpenGLLayer  context pixelFormat t ts =
  withObjCPtr context $ \raw_context ->
    withObjCPtr pixelFormat $ \raw_pixelFormat ->
        sendMsg nsOpenGLLayer (mkSelector "drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:") retVoid [argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_pixelFormat :: Ptr ()), argCDouble t, argPtr (castPtr (unRawId (unConst ts)) :: Ptr ())]

-- | @- view@
view :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> IO (Id NSView)
view nsOpenGLLayer  =
    sendMsg nsOpenGLLayer (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSView value) => nsOpenGLLayer -> value -> IO ()
setView nsOpenGLLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsOpenGLLayer (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- openGLPixelFormat@
openGLPixelFormat :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> IO (Id NSOpenGLPixelFormat)
openGLPixelFormat nsOpenGLLayer  =
    sendMsg nsOpenGLLayer (mkSelector "openGLPixelFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOpenGLPixelFormat:@
setOpenGLPixelFormat :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLPixelFormat value) => nsOpenGLLayer -> value -> IO ()
setOpenGLPixelFormat nsOpenGLLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsOpenGLLayer (mkSelector "setOpenGLPixelFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- openGLContext@
openGLContext :: IsNSOpenGLLayer nsOpenGLLayer => nsOpenGLLayer -> IO (Id NSOpenGLContext)
openGLContext nsOpenGLLayer  =
    sendMsg nsOpenGLLayer (mkSelector "openGLContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOpenGLContext:@
setOpenGLContext :: (IsNSOpenGLLayer nsOpenGLLayer, IsNSOpenGLContext value) => nsOpenGLLayer -> value -> IO ()
setOpenGLContext nsOpenGLLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsOpenGLLayer (mkSelector "setOpenGLContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openGLPixelFormatForDisplayMask:@
openGLPixelFormatForDisplayMaskSelector :: Selector
openGLPixelFormatForDisplayMaskSelector = mkSelector "openGLPixelFormatForDisplayMask:"

-- | @Selector@ for @openGLContextForPixelFormat:@
openGLContextForPixelFormatSelector :: Selector
openGLContextForPixelFormatSelector = mkSelector "openGLContextForPixelFormat:"

-- | @Selector@ for @canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector :: Selector
canDrawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector = mkSelector "canDrawInOpenGLContext:pixelFormat:forLayerTime:displayTime:"

-- | @Selector@ for @drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:@
drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector :: Selector
drawInOpenGLContext_pixelFormat_forLayerTime_displayTimeSelector = mkSelector "drawInOpenGLContext:pixelFormat:forLayerTime:displayTime:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @openGLPixelFormat@
openGLPixelFormatSelector :: Selector
openGLPixelFormatSelector = mkSelector "openGLPixelFormat"

-- | @Selector@ for @setOpenGLPixelFormat:@
setOpenGLPixelFormatSelector :: Selector
setOpenGLPixelFormatSelector = mkSelector "setOpenGLPixelFormat:"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @setOpenGLContext:@
setOpenGLContextSelector :: Selector
setOpenGLContextSelector = mkSelector "setOpenGLContext:"

