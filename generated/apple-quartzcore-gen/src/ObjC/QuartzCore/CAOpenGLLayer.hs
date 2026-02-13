{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAOpenGLLayer@.
module ObjC.QuartzCore.CAOpenGLLayer
  ( CAOpenGLLayer
  , IsCAOpenGLLayer(..)
  , canDrawInCGLContext_pixelFormat_forLayerTime_displayTime
  , drawInCGLContext_pixelFormat_forLayerTime_displayTime
  , copyCGLPixelFormatForDisplayMask
  , releaseCGLPixelFormat
  , copyCGLContextForPixelFormat
  , releaseCGLContext
  , asynchronous
  , setAsynchronous
  , colorspace
  , setColorspace
  , wantsExtendedDynamicRangeContent
  , setWantsExtendedDynamicRangeContent
  , asynchronousSelector
  , canDrawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector
  , colorspaceSelector
  , copyCGLContextForPixelFormatSelector
  , copyCGLPixelFormatForDisplayMaskSelector
  , drawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector
  , releaseCGLContextSelector
  , releaseCGLPixelFormatSelector
  , setAsynchronousSelector
  , setColorspaceSelector
  , setWantsExtendedDynamicRangeContentSelector
  , wantsExtendedDynamicRangeContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- canDrawInCGLContext:pixelFormat:forLayerTime:displayTime:@
canDrawInCGLContext_pixelFormat_forLayerTime_displayTime :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> Ptr () -> CDouble -> Const RawId -> IO Bool
canDrawInCGLContext_pixelFormat_forLayerTime_displayTime caOpenGLLayer ctx pf t ts =
  sendMessage caOpenGLLayer canDrawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector ctx pf t ts

-- | @- drawInCGLContext:pixelFormat:forLayerTime:displayTime:@
drawInCGLContext_pixelFormat_forLayerTime_displayTime :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> Ptr () -> CDouble -> Const RawId -> IO ()
drawInCGLContext_pixelFormat_forLayerTime_displayTime caOpenGLLayer ctx pf t ts =
  sendMessage caOpenGLLayer drawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector ctx pf t ts

-- | @- copyCGLPixelFormatForDisplayMask:@
copyCGLPixelFormatForDisplayMask :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> CUInt -> IO (Ptr ())
copyCGLPixelFormatForDisplayMask caOpenGLLayer mask =
  sendOwnedMessage caOpenGLLayer copyCGLPixelFormatForDisplayMaskSelector mask

-- | @- releaseCGLPixelFormat:@
releaseCGLPixelFormat :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO ()
releaseCGLPixelFormat caOpenGLLayer pf =
  sendMessage caOpenGLLayer releaseCGLPixelFormatSelector pf

-- | @- copyCGLContextForPixelFormat:@
copyCGLContextForPixelFormat :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO (Ptr ())
copyCGLContextForPixelFormat caOpenGLLayer pf =
  sendOwnedMessage caOpenGLLayer copyCGLContextForPixelFormatSelector pf

-- | @- releaseCGLContext:@
releaseCGLContext :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO ()
releaseCGLContext caOpenGLLayer ctx =
  sendMessage caOpenGLLayer releaseCGLContextSelector ctx

-- | @- asynchronous@
asynchronous :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> IO Bool
asynchronous caOpenGLLayer =
  sendMessage caOpenGLLayer asynchronousSelector

-- | @- setAsynchronous:@
setAsynchronous :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Bool -> IO ()
setAsynchronous caOpenGLLayer value =
  sendMessage caOpenGLLayer setAsynchronousSelector value

-- | @- colorspace@
colorspace :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> IO (Ptr ())
colorspace caOpenGLLayer =
  sendMessage caOpenGLLayer colorspaceSelector

-- | @- setColorspace:@
setColorspace :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO ()
setColorspace caOpenGLLayer value =
  sendMessage caOpenGLLayer setColorspaceSelector value

-- | @- wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContent :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> IO Bool
wantsExtendedDynamicRangeContent caOpenGLLayer =
  sendMessage caOpenGLLayer wantsExtendedDynamicRangeContentSelector

-- | @- setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContent :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Bool -> IO ()
setWantsExtendedDynamicRangeContent caOpenGLLayer value =
  sendMessage caOpenGLLayer setWantsExtendedDynamicRangeContentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canDrawInCGLContext:pixelFormat:forLayerTime:displayTime:@
canDrawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector :: Selector '[Ptr (), Ptr (), CDouble, Const RawId] Bool
canDrawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector = mkSelector "canDrawInCGLContext:pixelFormat:forLayerTime:displayTime:"

-- | @Selector@ for @drawInCGLContext:pixelFormat:forLayerTime:displayTime:@
drawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector :: Selector '[Ptr (), Ptr (), CDouble, Const RawId] ()
drawInCGLContext_pixelFormat_forLayerTime_displayTimeSelector = mkSelector "drawInCGLContext:pixelFormat:forLayerTime:displayTime:"

-- | @Selector@ for @copyCGLPixelFormatForDisplayMask:@
copyCGLPixelFormatForDisplayMaskSelector :: Selector '[CUInt] (Ptr ())
copyCGLPixelFormatForDisplayMaskSelector = mkSelector "copyCGLPixelFormatForDisplayMask:"

-- | @Selector@ for @releaseCGLPixelFormat:@
releaseCGLPixelFormatSelector :: Selector '[Ptr ()] ()
releaseCGLPixelFormatSelector = mkSelector "releaseCGLPixelFormat:"

-- | @Selector@ for @copyCGLContextForPixelFormat:@
copyCGLContextForPixelFormatSelector :: Selector '[Ptr ()] (Ptr ())
copyCGLContextForPixelFormatSelector = mkSelector "copyCGLContextForPixelFormat:"

-- | @Selector@ for @releaseCGLContext:@
releaseCGLContextSelector :: Selector '[Ptr ()] ()
releaseCGLContextSelector = mkSelector "releaseCGLContext:"

-- | @Selector@ for @asynchronous@
asynchronousSelector :: Selector '[] Bool
asynchronousSelector = mkSelector "asynchronous"

-- | @Selector@ for @setAsynchronous:@
setAsynchronousSelector :: Selector '[Bool] ()
setAsynchronousSelector = mkSelector "setAsynchronous:"

-- | @Selector@ for @colorspace@
colorspaceSelector :: Selector '[] (Ptr ())
colorspaceSelector = mkSelector "colorspace"

-- | @Selector@ for @setColorspace:@
setColorspaceSelector :: Selector '[Ptr ()] ()
setColorspaceSelector = mkSelector "setColorspace:"

-- | @Selector@ for @wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContentSelector :: Selector '[] Bool
wantsExtendedDynamicRangeContentSelector = mkSelector "wantsExtendedDynamicRangeContent"

-- | @Selector@ for @setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContentSelector :: Selector '[Bool] ()
setWantsExtendedDynamicRangeContentSelector = mkSelector "setWantsExtendedDynamicRangeContent:"

