{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberTextItemView
--
-- A simple @NSScrubberItemView@ for displaying an image.
--
-- If the provided image is larger than the view's frame, it is scaled proportionally to fill the entire frame. The cropped portion of the image is determined by the @imageAlignment@ property.
--
-- Generated bindings for @NSScrubberImageItemView@.
module ObjC.AppKit.NSScrubberImageItemView
  ( NSScrubberImageItemView
  , IsNSScrubberImageItemView(..)
  , imageView
  , image
  , setImage
  , imageAlignment
  , setImageAlignment
  , imageViewSelector
  , imageSelector
  , setImageSelector
  , imageAlignmentSelector
  , setImageAlignmentSelector

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

-- | @- imageView@
imageView :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> IO (Id NSImageView)
imageView nsScrubberImageItemView  =
  sendMsg nsScrubberImageItemView (mkSelector "imageView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- image@
image :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> IO (Id NSImage)
image nsScrubberImageItemView  =
  sendMsg nsScrubberImageItemView (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSScrubberImageItemView nsScrubberImageItemView, IsNSImage value) => nsScrubberImageItemView -> value -> IO ()
setImage nsScrubberImageItemView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubberImageItemView (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageAlignment@
imageAlignment :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> IO NSImageAlignment
imageAlignment nsScrubberImageItemView  =
  fmap (coerce :: CULong -> NSImageAlignment) $ sendMsg nsScrubberImageItemView (mkSelector "imageAlignment") retCULong []

-- | @- setImageAlignment:@
setImageAlignment :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> NSImageAlignment -> IO ()
setImageAlignment nsScrubberImageItemView  value =
  sendMsg nsScrubberImageItemView (mkSelector "setImageAlignment:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageView@
imageViewSelector :: Selector
imageViewSelector = mkSelector "imageView"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @setImageAlignment:@
setImageAlignmentSelector :: Selector
setImageAlignmentSelector = mkSelector "setImageAlignment:"

