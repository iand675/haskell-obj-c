{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSImageCell@.
module ObjC.AppKit.NSImageCell
  ( NSImageCell
  , IsNSImageCell(..)
  , imageAlignment
  , setImageAlignment
  , imageScaling
  , setImageScaling
  , imageFrameStyle
  , setImageFrameStyle
  , imageAlignmentSelector
  , setImageAlignmentSelector
  , imageScalingSelector
  , setImageScalingSelector
  , imageFrameStyleSelector
  , setImageFrameStyleSelector

  -- * Enum types
  , NSImageAlignment(NSImageAlignment)
  , pattern NSImageAlignCenter
  , pattern NSImageAlignTop
  , pattern NSImageAlignTopLeft
  , pattern NSImageAlignTopRight
  , pattern NSImageAlignLeft
  , pattern NSImageAlignBottom
  , pattern NSImageAlignBottomLeft
  , pattern NSImageAlignBottomRight
  , pattern NSImageAlignRight
  , NSImageFrameStyle(NSImageFrameStyle)
  , pattern NSImageFrameNone
  , pattern NSImageFramePhoto
  , pattern NSImageFrameGrayBezel
  , pattern NSImageFrameGroove
  , pattern NSImageFrameButton
  , NSImageScaling(NSImageScaling)
  , pattern NSImageScaleProportionallyDown
  , pattern NSImageScaleAxesIndependently
  , pattern NSImageScaleNone
  , pattern NSImageScaleProportionallyUpOrDown
  , pattern NSScaleProportionally
  , pattern NSScaleToFit
  , pattern NSScaleNone

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- imageAlignment@
imageAlignment :: IsNSImageCell nsImageCell => nsImageCell -> IO NSImageAlignment
imageAlignment nsImageCell  =
  fmap (coerce :: CULong -> NSImageAlignment) $ sendMsg nsImageCell (mkSelector "imageAlignment") retCULong []

-- | @- setImageAlignment:@
setImageAlignment :: IsNSImageCell nsImageCell => nsImageCell -> NSImageAlignment -> IO ()
setImageAlignment nsImageCell  value =
  sendMsg nsImageCell (mkSelector "setImageAlignment:") retVoid [argCULong (coerce value)]

-- | @- imageScaling@
imageScaling :: IsNSImageCell nsImageCell => nsImageCell -> IO NSImageScaling
imageScaling nsImageCell  =
  fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsImageCell (mkSelector "imageScaling") retCULong []

-- | @- setImageScaling:@
setImageScaling :: IsNSImageCell nsImageCell => nsImageCell -> NSImageScaling -> IO ()
setImageScaling nsImageCell  value =
  sendMsg nsImageCell (mkSelector "setImageScaling:") retVoid [argCULong (coerce value)]

-- | @- imageFrameStyle@
imageFrameStyle :: IsNSImageCell nsImageCell => nsImageCell -> IO NSImageFrameStyle
imageFrameStyle nsImageCell  =
  fmap (coerce :: CULong -> NSImageFrameStyle) $ sendMsg nsImageCell (mkSelector "imageFrameStyle") retCULong []

-- | @- setImageFrameStyle:@
setImageFrameStyle :: IsNSImageCell nsImageCell => nsImageCell -> NSImageFrameStyle -> IO ()
setImageFrameStyle nsImageCell  value =
  sendMsg nsImageCell (mkSelector "setImageFrameStyle:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @setImageAlignment:@
setImageAlignmentSelector :: Selector
setImageAlignmentSelector = mkSelector "setImageAlignment:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @imageFrameStyle@
imageFrameStyleSelector :: Selector
imageFrameStyleSelector = mkSelector "imageFrameStyle"

-- | @Selector@ for @setImageFrameStyle:@
setImageFrameStyleSelector :: Selector
setImageFrameStyleSelector = mkSelector "setImageFrameStyle:"

