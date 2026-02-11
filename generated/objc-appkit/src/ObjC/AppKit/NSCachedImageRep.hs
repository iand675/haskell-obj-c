{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCachedImageRep@.
module ObjC.AppKit.NSCachedImageRep
  ( NSCachedImageRep
  , IsNSCachedImageRep(..)
  , initWithWindow_rect
  , initWithSize_depth_separate_alpha
  , window
  , rect
  , initWithWindow_rectSelector
  , initWithSize_depth_separate_alphaSelector
  , windowSelector
  , rectSelector

  -- * Enum types
  , NSWindowDepth(NSWindowDepth)
  , pattern NSWindowDepthTwentyfourBitRGB
  , pattern NSWindowDepthSixtyfourBitRGB
  , pattern NSWindowDepthOnehundredtwentyeightBitRGB

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithWindow:rect:@
initWithWindow_rect :: (IsNSCachedImageRep nsCachedImageRep, IsNSWindow win) => nsCachedImageRep -> win -> NSRect -> IO RawId
initWithWindow_rect nsCachedImageRep  win rect =
withObjCPtr win $ \raw_win ->
    fmap (RawId . castPtr) $ sendMsg nsCachedImageRep (mkSelector "initWithWindow:rect:") (retPtr retVoid) [argPtr (castPtr raw_win :: Ptr ()), argNSRect rect]

-- | @- initWithSize:depth:separate:alpha:@
initWithSize_depth_separate_alpha :: IsNSCachedImageRep nsCachedImageRep => nsCachedImageRep -> NSSize -> NSWindowDepth -> Bool -> Bool -> IO RawId
initWithSize_depth_separate_alpha nsCachedImageRep  size depth flag alpha =
  fmap (RawId . castPtr) $ sendMsg nsCachedImageRep (mkSelector "initWithSize:depth:separate:alpha:") (retPtr retVoid) [argNSSize size, argCInt (coerce depth), argCULong (if flag then 1 else 0), argCULong (if alpha then 1 else 0)]

-- | @- window@
window :: IsNSCachedImageRep nsCachedImageRep => nsCachedImageRep -> IO (Id NSWindow)
window nsCachedImageRep  =
  sendMsg nsCachedImageRep (mkSelector "window") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rect@
rect :: IsNSCachedImageRep nsCachedImageRep => nsCachedImageRep -> IO NSRect
rect nsCachedImageRep  =
  sendMsgStret nsCachedImageRep (mkSelector "rect") retNSRect []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWindow:rect:@
initWithWindow_rectSelector :: Selector
initWithWindow_rectSelector = mkSelector "initWithWindow:rect:"

-- | @Selector@ for @initWithSize:depth:separate:alpha:@
initWithSize_depth_separate_alphaSelector :: Selector
initWithSize_depth_separate_alphaSelector = mkSelector "initWithSize:depth:separate:alpha:"

-- | @Selector@ for @window@
windowSelector :: Selector
windowSelector = mkSelector "window"

-- | @Selector@ for @rect@
rectSelector :: Selector
rectSelector = mkSelector "rect"

