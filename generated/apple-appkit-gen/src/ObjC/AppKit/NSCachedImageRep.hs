{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initWithSize_depth_separate_alphaSelector
  , initWithWindow_rectSelector
  , rectSelector
  , windowSelector

  -- * Enum types
  , NSWindowDepth(NSWindowDepth)
  , pattern NSWindowDepthTwentyfourBitRGB
  , pattern NSWindowDepthSixtyfourBitRGB
  , pattern NSWindowDepthOnehundredtwentyeightBitRGB

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithWindow:rect:@
initWithWindow_rect :: (IsNSCachedImageRep nsCachedImageRep, IsNSWindow win) => nsCachedImageRep -> win -> NSRect -> IO RawId
initWithWindow_rect nsCachedImageRep win rect =
  sendOwnedMessage nsCachedImageRep initWithWindow_rectSelector (toNSWindow win) rect

-- | @- initWithSize:depth:separate:alpha:@
initWithSize_depth_separate_alpha :: IsNSCachedImageRep nsCachedImageRep => nsCachedImageRep -> NSSize -> NSWindowDepth -> Bool -> Bool -> IO RawId
initWithSize_depth_separate_alpha nsCachedImageRep size depth flag alpha =
  sendOwnedMessage nsCachedImageRep initWithSize_depth_separate_alphaSelector size depth flag alpha

-- | @- window@
window :: IsNSCachedImageRep nsCachedImageRep => nsCachedImageRep -> IO (Id NSWindow)
window nsCachedImageRep =
  sendMessage nsCachedImageRep windowSelector

-- | @- rect@
rect :: IsNSCachedImageRep nsCachedImageRep => nsCachedImageRep -> IO NSRect
rect nsCachedImageRep =
  sendMessage nsCachedImageRep rectSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWindow:rect:@
initWithWindow_rectSelector :: Selector '[Id NSWindow, NSRect] RawId
initWithWindow_rectSelector = mkSelector "initWithWindow:rect:"

-- | @Selector@ for @initWithSize:depth:separate:alpha:@
initWithSize_depth_separate_alphaSelector :: Selector '[NSSize, NSWindowDepth, Bool, Bool] RawId
initWithSize_depth_separate_alphaSelector = mkSelector "initWithSize:depth:separate:alpha:"

-- | @Selector@ for @window@
windowSelector :: Selector '[] (Id NSWindow)
windowSelector = mkSelector "window"

-- | @Selector@ for @rect@
rectSelector :: Selector '[] NSRect
rectSelector = mkSelector "rect"

