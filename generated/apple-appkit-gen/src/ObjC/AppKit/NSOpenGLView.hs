{-# LANGUAGE DataKinds #-}
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
  , clearGLContextSelector
  , defaultPixelFormatSelector
  , initWithFrame_pixelFormatSelector
  , openGLContextSelector
  , pixelFormatSelector
  , prepareOpenGLSelector
  , reshapeSelector
  , setOpenGLContextSelector
  , setPixelFormatSelector
  , setWantsBestResolutionOpenGLSurfaceSelector
  , setWantsExtendedDynamicRangeOpenGLSurfaceSelector
  , updateSelector
  , wantsBestResolutionOpenGLSurfaceSelector
  , wantsExtendedDynamicRangeOpenGLSurfaceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultPixelFormatSelector

-- | @- initWithFrame:pixelFormat:@
initWithFrame_pixelFormat :: (IsNSOpenGLView nsOpenGLView, IsNSOpenGLPixelFormat format) => nsOpenGLView -> NSRect -> format -> IO (Id NSOpenGLView)
initWithFrame_pixelFormat nsOpenGLView frameRect format =
  sendOwnedMessage nsOpenGLView initWithFrame_pixelFormatSelector frameRect (toNSOpenGLPixelFormat format)

-- | @- clearGLContext@
clearGLContext :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
clearGLContext nsOpenGLView =
  sendMessage nsOpenGLView clearGLContextSelector

-- | @- update@
update :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
update nsOpenGLView =
  sendMessage nsOpenGLView updateSelector

-- | @- reshape@
reshape :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
reshape nsOpenGLView =
  sendMessage nsOpenGLView reshapeSelector

-- | @- prepareOpenGL@
prepareOpenGL :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO ()
prepareOpenGL nsOpenGLView =
  sendMessage nsOpenGLView prepareOpenGLSelector

-- | @- openGLContext@
openGLContext :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO (Id NSOpenGLContext)
openGLContext nsOpenGLView =
  sendMessage nsOpenGLView openGLContextSelector

-- | @- setOpenGLContext:@
setOpenGLContext :: (IsNSOpenGLView nsOpenGLView, IsNSOpenGLContext value) => nsOpenGLView -> value -> IO ()
setOpenGLContext nsOpenGLView value =
  sendMessage nsOpenGLView setOpenGLContextSelector (toNSOpenGLContext value)

-- | @- pixelFormat@
pixelFormat :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO (Id NSOpenGLPixelFormat)
pixelFormat nsOpenGLView =
  sendMessage nsOpenGLView pixelFormatSelector

-- | @- setPixelFormat:@
setPixelFormat :: (IsNSOpenGLView nsOpenGLView, IsNSOpenGLPixelFormat value) => nsOpenGLView -> value -> IO ()
setPixelFormat nsOpenGLView value =
  sendMessage nsOpenGLView setPixelFormatSelector (toNSOpenGLPixelFormat value)

-- | @- wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO Bool
wantsBestResolutionOpenGLSurface nsOpenGLView =
  sendMessage nsOpenGLView wantsBestResolutionOpenGLSurfaceSelector

-- | @- setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> Bool -> IO ()
setWantsBestResolutionOpenGLSurface nsOpenGLView value =
  sendMessage nsOpenGLView setWantsBestResolutionOpenGLSurfaceSelector value

-- | @- wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> IO Bool
wantsExtendedDynamicRangeOpenGLSurface nsOpenGLView =
  sendMessage nsOpenGLView wantsExtendedDynamicRangeOpenGLSurfaceSelector

-- | @- setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurface :: IsNSOpenGLView nsOpenGLView => nsOpenGLView -> Bool -> IO ()
setWantsExtendedDynamicRangeOpenGLSurface nsOpenGLView value =
  sendMessage nsOpenGLView setWantsExtendedDynamicRangeOpenGLSurfaceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultPixelFormat@
defaultPixelFormatSelector :: Selector '[] (Id NSOpenGLPixelFormat)
defaultPixelFormatSelector = mkSelector "defaultPixelFormat"

-- | @Selector@ for @initWithFrame:pixelFormat:@
initWithFrame_pixelFormatSelector :: Selector '[NSRect, Id NSOpenGLPixelFormat] (Id NSOpenGLView)
initWithFrame_pixelFormatSelector = mkSelector "initWithFrame:pixelFormat:"

-- | @Selector@ for @clearGLContext@
clearGLContextSelector :: Selector '[] ()
clearGLContextSelector = mkSelector "clearGLContext"

-- | @Selector@ for @update@
updateSelector :: Selector '[] ()
updateSelector = mkSelector "update"

-- | @Selector@ for @reshape@
reshapeSelector :: Selector '[] ()
reshapeSelector = mkSelector "reshape"

-- | @Selector@ for @prepareOpenGL@
prepareOpenGLSelector :: Selector '[] ()
prepareOpenGLSelector = mkSelector "prepareOpenGL"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector '[] (Id NSOpenGLContext)
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @setOpenGLContext:@
setOpenGLContextSelector :: Selector '[Id NSOpenGLContext] ()
setOpenGLContextSelector = mkSelector "setOpenGLContext:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] (Id NSOpenGLPixelFormat)
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector '[Id NSOpenGLPixelFormat] ()
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @wantsBestResolutionOpenGLSurface@
wantsBestResolutionOpenGLSurfaceSelector :: Selector '[] Bool
wantsBestResolutionOpenGLSurfaceSelector = mkSelector "wantsBestResolutionOpenGLSurface"

-- | @Selector@ for @setWantsBestResolutionOpenGLSurface:@
setWantsBestResolutionOpenGLSurfaceSelector :: Selector '[Bool] ()
setWantsBestResolutionOpenGLSurfaceSelector = mkSelector "setWantsBestResolutionOpenGLSurface:"

-- | @Selector@ for @wantsExtendedDynamicRangeOpenGLSurface@
wantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector '[] Bool
wantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "wantsExtendedDynamicRangeOpenGLSurface"

-- | @Selector@ for @setWantsExtendedDynamicRangeOpenGLSurface:@
setWantsExtendedDynamicRangeOpenGLSurfaceSelector :: Selector '[Bool] ()
setWantsExtendedDynamicRangeOpenGLSurfaceSelector = mkSelector "setWantsExtendedDynamicRangeOpenGLSurface:"

