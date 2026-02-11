{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOpenGLView@.
module ObjC.AppKit.NSOpenGLView
  ( NSOpenGLView
  , IsNSOpenGLView(..)
  , defaultPixelFormat
  , initWithFrame_pixelFormat
  , clearGLContext
  , update
  , reshape
  , prepareOpenGL
  , openGLContext
  , setOpenGLContext
  , pixelFormat
  , setPixelFormat
  , wantsBestResolutionOpenGLSurface
  , setWantsBestResolutionOpenGLSurface
  , wantsExtendedDynamicRangeOpenGLSurface
  , setWantsExtendedDynamicRangeOpenGLSurface
  , defaultPixelFormatSelector
  , initWithFrame_pixelFormatSelector
  , clearGLContextSelector
  , updateSelector
  , reshapeSelector
  , prepareOpenGLSelector
  , openGLContextSelector
  , setOpenGLContextSelector
  , pixelFormatSelector
  , setPixelFormatSelector
  , wantsBestResolutionOpenGLSurfaceSelector
  , setWantsBestResolutionOpenGLSurfaceSelector
  , wantsExtendedDynamicRangeOpenGLSurfaceSelector
  , setWantsExtendedDynamicRangeOpenGLSurfaceSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ defaultPixelFormat@
defaultPixelFormat :: IO (Id NSOpenGLPixelFormat)
defaultPixelFormat  =
  do
    cls' <- getRequiredClass "NSOpenGLView"
    sendClassMsg cls' (mkSelector "defaultPixelFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithFrame:pixelFormat:@
initWithFrame_pixelFormat :: (IsNSOpenGLView nsOpenGLView, IsNSOpenGLPixelFormat format) => nsOpenGLView -> NSRect -> format -> IO (Id NSOpenGLView)
initWithFrame_pixelFormat nsOpenGLView  frameRect format =
withObjCPtr format $ \raw_format ->
    sendMsg nsOpenGLView (mkSelector "initWithFrame:pixelFormat:") (retPtr retVoid) [argNSRect frameRect, argPtr (castPtr raw_format :: Ptr ())] >>= ownedObject . castPtr

-- | @- clearGLContext@
clearGLContext :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
clearGLContext nsOpenGLView  =
  sendMsg nsOpenGLView (mkSelector "clearGLContext") retVoid []

-- | @- update@
update :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
update nsOpenGLView  =
  sendMsg nsOpenGLView (mkSelector "update") retVoid []

-- | @- reshape@
reshape :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
reshape nsOpenGLView  =
  sendMsg nsOpenGLView (mkSelector "reshape") retVoid []

-- | @- prepareOpenGL@
prepareOpenGL :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
prepareOpenGL nsOpenGLView  =
  sendMsg nsOpenGLView (mkSelector "prepareOpenGL") retVoid []

-- | @- openGLContext@
openGLContext :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO (Id NSOpenGLContext)
openGLContext nsOpenGLView  =
  sendMsg nsOpenGLView (mkSelector "openGLContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOpenGLContext:@
setOpenGLContext :: (IsNSOpenGLView nsOpenGLView, IsNSOpenGLContext value) => nsOpenGLView -> value -> IO ()
setOpenGLContext nsOpenGLView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsOpenGLView (mkSelector "setOpenGLContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pixelFormat@
pixelFormat :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO (Id NSOpenGLPixelFormat)
pixelFormat nsOpenGLView  =
  sendMsg nsOpenGLView (mkSelector "pixelFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPixelFormat:@
setPixelFormat :: (IsNSOpenGLView nsOpenGLView, IsNSOpenGLPixelFormat value) => nsOpenGLView -> value -> IO ()
setPixelFormat nsOpenGLView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsOpenGLView (mkSelector "setPixelFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO Bool
wantsBestResolutionOpenGLSurface nsOpenGLView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenGLView (mkSelector "wantsBestResolutionOpenGLSurface") retCULong []

-- | @- setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> Bool -> IO ()
setWantsBestResolutionOpenGLSurface nsOpenGLView  value =
  sendMsg nsOpenGLView (mkSelector "setWantsBestResolutionOpenGLSurface:") retVoid [argCULong (if value then 1 else 0)]

-- | @- wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO Bool
wantsExtendedDynamicRangeOpenGLSurface nsOpenGLView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOpenGLView (mkSelector "wantsExtendedDynamicRangeOpenGLSurface") retCULong []

-- | @- setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> Bool -> IO ()
setWantsExtendedDynamicRangeOpenGLSurface nsOpenGLView  value =
  sendMsg nsOpenGLView (mkSelector "setWantsExtendedDynamicRangeOpenGLSurface:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultPixelFormat@
defaultPixelFormatSelector :: Selector
defaultPixelFormatSelector = mkSelector "defaultPixelFormat"

-- | @Selector@ for @initWithFrame:pixelFormat:@
initWithFrame_pixelFormatSelector :: Selector
initWithFrame_pixelFormatSelector = mkSelector "initWithFrame:pixelFormat:"

-- | @Selector@ for @clearGLContext@
clearGLContextSelector :: Selector
clearGLContextSelector = mkSelector "clearGLContext"

-- | @Selector@ for @update@
updateSelector :: Selector
updateSelector = mkSelector "update"

-- | @Selector@ for @reshape@
reshapeSelector :: Selector
reshapeSelector = mkSelector "reshape"

-- | @Selector@ for @prepareOpenGL@
prepareOpenGLSelector :: Selector
prepareOpenGLSelector = mkSelector "prepareOpenGL"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @setOpenGLContext:@
setOpenGLContextSelector :: Selector
setOpenGLContextSelector = mkSelector "setOpenGLContext:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurfaceSelector :: Selector
wantsBestResolutionOpenGLSurfaceSelector = mkSelector "wantsBestResolutionOpenGLSurface"

-- | @Selector@ for @setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurfaceSelector :: Selector
setWantsBestResolutionOpenGLSurfaceSelector = mkSelector "setWantsBestResolutionOpenGLSurface:"

-- | @Selector@ for @wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector
wantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "wantsExtendedDynamicRangeOpenGLSurface"

-- | @Selector@ for @setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector
setWantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "setWantsExtendedDynamicRangeOpenGLSurface:"

