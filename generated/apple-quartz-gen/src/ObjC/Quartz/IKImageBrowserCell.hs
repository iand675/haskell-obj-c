{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IKImageBrowserCell@.
module ObjC.Quartz.IKImageBrowserCell
  ( IKImageBrowserCell
  , IsIKImageBrowserCell(..)
  , imageBrowserView
  , representedItem
  , indexOfRepresentedItem
  , frame
  , imageContainerFrame
  , imageFrame
  , selectionFrame
  , titleFrame
  , subtitleFrame
  , imageAlignment
  , isSelected
  , opacity
  , layerForType
  , frameSelector
  , imageAlignmentSelector
  , imageBrowserViewSelector
  , imageContainerFrameSelector
  , imageFrameSelector
  , indexOfRepresentedItemSelector
  , isSelectedSelector
  , layerForTypeSelector
  , opacitySelector
  , representedItemSelector
  , selectionFrameSelector
  , subtitleFrameSelector
  , titleFrameSelector

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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | imageBrowserView
--
-- Returns the view the receiver uses to display its represented object.
--
-- Subclasses should not override this method.
--
-- ObjC selector: @- imageBrowserView@
imageBrowserView :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO (Id IKImageBrowserView)
imageBrowserView ikImageBrowserCell =
  sendMessage ikImageBrowserCell imageBrowserViewSelector

-- | representedItem
--
-- Returns the receiver�s represented object.
--
-- Subclasses should not override this method.
--
-- ObjC selector: @- representedItem@
representedItem :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO RawId
representedItem ikImageBrowserCell =
  sendMessage ikImageBrowserCell representedItemSelector

-- | indexOfRepresentedItem
--
-- Returns the index of the receiver�s represented object in the datasource.
--
-- Subclasses should not override this method.
--
-- ObjC selector: @- indexOfRepresentedItem@
indexOfRepresentedItem :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO CULong
indexOfRepresentedItem ikImageBrowserCell =
  sendMessage ikImageBrowserCell indexOfRepresentedItemSelector

-- | frame
--
-- Returns the receiver�s frame rectangle, which defines its position in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses should not override this method.
--
-- ObjC selector: @- frame@
frame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
frame ikImageBrowserCell =
  sendMessage ikImageBrowserCell frameSelector

-- | imageContainerFrame
--
-- Returns the receiver�s image container frame rectangle, which defines the position of the container of the thumbnail in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the thumbnail container. The image frame is computed automatically from the image container frame by taking in account the image alignment and the image aspect ratio.
--
-- ObjC selector: @- imageContainerFrame@
imageContainerFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
imageContainerFrame ikImageBrowserCell =
  sendMessage ikImageBrowserCell imageContainerFrameSelector

-- | imageFrame
--
-- Returns the receiver�s image frame rectangle, which defines the position of the thumbnail in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the thumbnail.
--
-- ObjC selector: @- imageFrame@
imageFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
imageFrame ikImageBrowserCell =
  sendMessage ikImageBrowserCell imageFrameSelector

-- | selectionFrame
--
-- Returns the receiver�s selection frame rectangle, which defines the position of the selection rectangle in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the selection frame.
--
-- ObjC selector: @- selectionFrame@
selectionFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
selectionFrame ikImageBrowserCell =
  sendMessage ikImageBrowserCell selectionFrameSelector

-- | titleFrame
--
-- Returns the receiver�s title frame rectangle, which defines the position of the title in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the title.
--
-- ObjC selector: @- titleFrame@
titleFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
titleFrame ikImageBrowserCell =
  sendMessage ikImageBrowserCell titleFrameSelector

-- | subtitleFrame
--
-- Returns the receiver�s subtitle frame rectangle, which defines the position of the subtitle in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the subtitle.
--
-- ObjC selector: @- subtitleFrame@
subtitleFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
subtitleFrame ikImageBrowserCell =
  sendMessage ikImageBrowserCell subtitleFrameSelector

-- | imageAlignment
--
-- Returns the position of the cell�s image in the frame. The default is NSImageAlignCenter.
--
-- Subclasses can override this method to customize the image alignment. For the list of possible alignments, see [NSImageView setImageAlignment:]. The image frame will be computed automatically from the image container frame by taking in account the image alignment and the image aspect ratio.
--
-- ObjC selector: @- imageAlignment@
imageAlignment :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSImageAlignment
imageAlignment ikImageBrowserCell =
  sendMessage ikImageBrowserCell imageAlignmentSelector

-- | isSelected
--
-- Returns the selection state of the receiver.
--
-- Returns YES if the receiver is selected, otherwise NO. Subclasses should not override this method.
--
-- ObjC selector: @- isSelected@
isSelected :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO Bool
isSelected ikImageBrowserCell =
  sendMessage ikImageBrowserCell isSelectedSelector

-- | opacity
--
-- Determines the opacity of the receiver.
--
-- Possible values are between 0.0 (transparent) and 1.0 (opaque). Subclasses can override this method to customize the opacity of the cell.
--
-- ObjC selector: @- opacity@
opacity :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO CDouble
opacity ikImageBrowserCell =
  sendMessage ikImageBrowserCell opacitySelector

-- | layerForType:
--
-- Provides the receiver�s layer for the given type. The default is nil.
--
-- Subclasses can override this method to add a layer in the background, foreground... of the cell (see possible types above).
--
-- ObjC selector: @- layerForType:@
layerForType :: (IsIKImageBrowserCell ikImageBrowserCell, IsNSString type_) => ikImageBrowserCell -> type_ -> IO (Id CALayer)
layerForType ikImageBrowserCell type_ =
  sendMessage ikImageBrowserCell layerForTypeSelector (toNSString type_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageBrowserView@
imageBrowserViewSelector :: Selector '[] (Id IKImageBrowserView)
imageBrowserViewSelector = mkSelector "imageBrowserView"

-- | @Selector@ for @representedItem@
representedItemSelector :: Selector '[] RawId
representedItemSelector = mkSelector "representedItem"

-- | @Selector@ for @indexOfRepresentedItem@
indexOfRepresentedItemSelector :: Selector '[] CULong
indexOfRepresentedItemSelector = mkSelector "indexOfRepresentedItem"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @imageContainerFrame@
imageContainerFrameSelector :: Selector '[] NSRect
imageContainerFrameSelector = mkSelector "imageContainerFrame"

-- | @Selector@ for @imageFrame@
imageFrameSelector :: Selector '[] NSRect
imageFrameSelector = mkSelector "imageFrame"

-- | @Selector@ for @selectionFrame@
selectionFrameSelector :: Selector '[] NSRect
selectionFrameSelector = mkSelector "selectionFrame"

-- | @Selector@ for @titleFrame@
titleFrameSelector :: Selector '[] NSRect
titleFrameSelector = mkSelector "titleFrame"

-- | @Selector@ for @subtitleFrame@
subtitleFrameSelector :: Selector '[] NSRect
subtitleFrameSelector = mkSelector "subtitleFrame"

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector '[] NSImageAlignment
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @isSelected@
isSelectedSelector :: Selector '[] Bool
isSelectedSelector = mkSelector "isSelected"

-- | @Selector@ for @opacity@
opacitySelector :: Selector '[] CDouble
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @layerForType:@
layerForTypeSelector :: Selector '[Id NSString] (Id CALayer)
layerForTypeSelector = mkSelector "layerForType:"

