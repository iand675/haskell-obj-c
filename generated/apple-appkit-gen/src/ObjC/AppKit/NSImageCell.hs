{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , imageFrameStyleSelector
  , imageScalingSelector
  , setImageAlignmentSelector
  , setImageFrameStyleSelector
  , setImageScalingSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- imageAlignment@
imageAlignment :: IsNSImageCell nsImageCell => nsImageCell -> IO NSImageAlignment
imageAlignment nsImageCell =
  sendMessage nsImageCell imageAlignmentSelector

-- | @- setImageAlignment:@
setImageAlignment :: IsNSImageCell nsImageCell => nsImageCell -> NSImageAlignment -> IO ()
setImageAlignment nsImageCell value =
  sendMessage nsImageCell setImageAlignmentSelector value

-- | @- imageScaling@
imageScaling :: IsNSImageCell nsImageCell => nsImageCell -> IO NSImageScaling
imageScaling nsImageCell =
  sendMessage nsImageCell imageScalingSelector

-- | @- setImageScaling:@
setImageScaling :: IsNSImageCell nsImageCell => nsImageCell -> NSImageScaling -> IO ()
setImageScaling nsImageCell value =
  sendMessage nsImageCell setImageScalingSelector value

-- | @- imageFrameStyle@
imageFrameStyle :: IsNSImageCell nsImageCell => nsImageCell -> IO NSImageFrameStyle
imageFrameStyle nsImageCell =
  sendMessage nsImageCell imageFrameStyleSelector

-- | @- setImageFrameStyle:@
setImageFrameStyle :: IsNSImageCell nsImageCell => nsImageCell -> NSImageFrameStyle -> IO ()
setImageFrameStyle nsImageCell value =
  sendMessage nsImageCell setImageFrameStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector '[] NSImageAlignment
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @setImageAlignment:@
setImageAlignmentSelector :: Selector '[NSImageAlignment] ()
setImageAlignmentSelector = mkSelector "setImageAlignment:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector '[] NSImageScaling
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector '[NSImageScaling] ()
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @imageFrameStyle@
imageFrameStyleSelector :: Selector '[] NSImageFrameStyle
imageFrameStyleSelector = mkSelector "imageFrameStyle"

-- | @Selector@ for @setImageFrameStyle:@
setImageFrameStyleSelector :: Selector '[NSImageFrameStyle] ()
setImageFrameStyleSelector = mkSelector "setImageFrameStyle:"

