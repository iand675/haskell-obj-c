{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , imageAlignmentSelector
  , imageSelector
  , imageViewSelector
  , setImageAlignmentSelector
  , setImageSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- imageView@
imageView :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> IO (Id NSImageView)
imageView nsScrubberImageItemView =
  sendMessage nsScrubberImageItemView imageViewSelector

-- | @- image@
image :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> IO (Id NSImage)
image nsScrubberImageItemView =
  sendMessage nsScrubberImageItemView imageSelector

-- | @- setImage:@
setImage :: (IsNSScrubberImageItemView nsScrubberImageItemView, IsNSImage value) => nsScrubberImageItemView -> value -> IO ()
setImage nsScrubberImageItemView value =
  sendMessage nsScrubberImageItemView setImageSelector (toNSImage value)

-- | @- imageAlignment@
imageAlignment :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> IO NSImageAlignment
imageAlignment nsScrubberImageItemView =
  sendMessage nsScrubberImageItemView imageAlignmentSelector

-- | @- setImageAlignment:@
setImageAlignment :: IsNSScrubberImageItemView nsScrubberImageItemView => nsScrubberImageItemView -> NSImageAlignment -> IO ()
setImageAlignment nsScrubberImageItemView value =
  sendMessage nsScrubberImageItemView setImageAlignmentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageView@
imageViewSelector :: Selector '[] (Id NSImageView)
imageViewSelector = mkSelector "imageView"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector '[] NSImageAlignment
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @setImageAlignment:@
setImageAlignmentSelector :: Selector '[NSImageAlignment] ()
setImageAlignmentSelector = mkSelector "setImageAlignment:"

