{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAOpenGLLayer@.
module ObjC.QuartzCore.CAOpenGLLayer
  ( CAOpenGLLayer
  , IsCAOpenGLLayer(..)
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
  , copyCGLPixelFormatForDisplayMaskSelector
  , releaseCGLPixelFormatSelector
  , copyCGLContextForPixelFormatSelector
  , releaseCGLContextSelector
  , asynchronousSelector
  , setAsynchronousSelector
  , colorspaceSelector
  , setColorspaceSelector
  , wantsExtendedDynamicRangeContentSelector
  , setWantsExtendedDynamicRangeContentSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- copyCGLPixelFormatForDisplayMask:@
copyCGLPixelFormatForDisplayMask :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> CUInt -> IO (Ptr ())
copyCGLPixelFormatForDisplayMask caOpenGLLayer  mask =
  fmap castPtr $ sendMsg caOpenGLLayer (mkSelector "copyCGLPixelFormatForDisplayMask:") (retPtr retVoid) [argCUInt (fromIntegral mask)]

-- | @- releaseCGLPixelFormat:@
releaseCGLPixelFormat :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO ()
releaseCGLPixelFormat caOpenGLLayer  pf =
  sendMsg caOpenGLLayer (mkSelector "releaseCGLPixelFormat:") retVoid [argPtr pf]

-- | @- copyCGLContextForPixelFormat:@
copyCGLContextForPixelFormat :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO (Ptr ())
copyCGLContextForPixelFormat caOpenGLLayer  pf =
  fmap castPtr $ sendMsg caOpenGLLayer (mkSelector "copyCGLContextForPixelFormat:") (retPtr retVoid) [argPtr pf]

-- | @- releaseCGLContext:@
releaseCGLContext :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO ()
releaseCGLContext caOpenGLLayer  ctx =
  sendMsg caOpenGLLayer (mkSelector "releaseCGLContext:") retVoid [argPtr ctx]

-- | @- asynchronous@
asynchronous :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> IO Bool
asynchronous caOpenGLLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caOpenGLLayer (mkSelector "asynchronous") retCULong []

-- | @- setAsynchronous:@
setAsynchronous :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Bool -> IO ()
setAsynchronous caOpenGLLayer  value =
  sendMsg caOpenGLLayer (mkSelector "setAsynchronous:") retVoid [argCULong (if value then 1 else 0)]

-- | @- colorspace@
colorspace :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> IO (Ptr ())
colorspace caOpenGLLayer  =
  fmap castPtr $ sendMsg caOpenGLLayer (mkSelector "colorspace") (retPtr retVoid) []

-- | @- setColorspace:@
setColorspace :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Ptr () -> IO ()
setColorspace caOpenGLLayer  value =
  sendMsg caOpenGLLayer (mkSelector "setColorspace:") retVoid [argPtr value]

-- | @- wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContent :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> IO Bool
wantsExtendedDynamicRangeContent caOpenGLLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caOpenGLLayer (mkSelector "wantsExtendedDynamicRangeContent") retCULong []

-- | @- setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContent :: IsCAOpenGLLayer caOpenGLLayer => caOpenGLLayer -> Bool -> IO ()
setWantsExtendedDynamicRangeContent caOpenGLLayer  value =
  sendMsg caOpenGLLayer (mkSelector "setWantsExtendedDynamicRangeContent:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @copyCGLPixelFormatForDisplayMask:@
copyCGLPixelFormatForDisplayMaskSelector :: Selector
copyCGLPixelFormatForDisplayMaskSelector = mkSelector "copyCGLPixelFormatForDisplayMask:"

-- | @Selector@ for @releaseCGLPixelFormat:@
releaseCGLPixelFormatSelector :: Selector
releaseCGLPixelFormatSelector = mkSelector "releaseCGLPixelFormat:"

-- | @Selector@ for @copyCGLContextForPixelFormat:@
copyCGLContextForPixelFormatSelector :: Selector
copyCGLContextForPixelFormatSelector = mkSelector "copyCGLContextForPixelFormat:"

-- | @Selector@ for @releaseCGLContext:@
releaseCGLContextSelector :: Selector
releaseCGLContextSelector = mkSelector "releaseCGLContext:"

-- | @Selector@ for @asynchronous@
asynchronousSelector :: Selector
asynchronousSelector = mkSelector "asynchronous"

-- | @Selector@ for @setAsynchronous:@
setAsynchronousSelector :: Selector
setAsynchronousSelector = mkSelector "setAsynchronous:"

-- | @Selector@ for @colorspace@
colorspaceSelector :: Selector
colorspaceSelector = mkSelector "colorspace"

-- | @Selector@ for @setColorspace:@
setColorspaceSelector :: Selector
setColorspaceSelector = mkSelector "setColorspace:"

-- | @Selector@ for @wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContentSelector :: Selector
wantsExtendedDynamicRangeContentSelector = mkSelector "wantsExtendedDynamicRangeContent"

-- | @Selector@ for @setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContentSelector :: Selector
setWantsExtendedDynamicRangeContentSelector = mkSelector "setWantsExtendedDynamicRangeContent:"

