{-# LANGUAGE PatternSynonyms #-}
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
  , imageBrowserViewSelector
  , representedItemSelector
  , indexOfRepresentedItemSelector
  , frameSelector
  , imageContainerFrameSelector
  , imageFrameSelector
  , selectionFrameSelector
  , titleFrameSelector
  , subtitleFrameSelector
  , imageAlignmentSelector
  , isSelectedSelector
  , opacitySelector
  , layerForTypeSelector

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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
imageBrowserView ikImageBrowserCell  =
  sendMsg ikImageBrowserCell (mkSelector "imageBrowserView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | representedItem
--
-- Returns the receiver�s represented object.
--
-- Subclasses should not override this method.
--
-- ObjC selector: @- representedItem@
representedItem :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO RawId
representedItem ikImageBrowserCell  =
  fmap (RawId . castPtr) $ sendMsg ikImageBrowserCell (mkSelector "representedItem") (retPtr retVoid) []

-- | indexOfRepresentedItem
--
-- Returns the index of the receiver�s represented object in the datasource.
--
-- Subclasses should not override this method.
--
-- ObjC selector: @- indexOfRepresentedItem@
indexOfRepresentedItem :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO CULong
indexOfRepresentedItem ikImageBrowserCell  =
  sendMsg ikImageBrowserCell (mkSelector "indexOfRepresentedItem") retCULong []

-- | frame
--
-- Returns the receiver�s frame rectangle, which defines its position in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses should not override this method.
--
-- ObjC selector: @- frame@
frame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
frame ikImageBrowserCell  =
  sendMsgStret ikImageBrowserCell (mkSelector "frame") retNSRect []

-- | imageContainerFrame
--
-- Returns the receiver�s image container frame rectangle, which defines the position of the container of the thumbnail in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the thumbnail container. The image frame is computed automatically from the image container frame by taking in account the image alignment and the image aspect ratio.
--
-- ObjC selector: @- imageContainerFrame@
imageContainerFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
imageContainerFrame ikImageBrowserCell  =
  sendMsgStret ikImageBrowserCell (mkSelector "imageContainerFrame") retNSRect []

-- | imageFrame
--
-- Returns the receiver�s image frame rectangle, which defines the position of the thumbnail in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the thumbnail.
--
-- ObjC selector: @- imageFrame@
imageFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
imageFrame ikImageBrowserCell  =
  sendMsgStret ikImageBrowserCell (mkSelector "imageFrame") retNSRect []

-- | selectionFrame
--
-- Returns the receiver�s selection frame rectangle, which defines the position of the selection rectangle in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the selection frame.
--
-- ObjC selector: @- selectionFrame@
selectionFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
selectionFrame ikImageBrowserCell  =
  sendMsgStret ikImageBrowserCell (mkSelector "selectionFrame") retNSRect []

-- | titleFrame
--
-- Returns the receiver�s title frame rectangle, which defines the position of the title in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the title.
--
-- ObjC selector: @- titleFrame@
titleFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
titleFrame ikImageBrowserCell  =
  sendMsgStret ikImageBrowserCell (mkSelector "titleFrame") retNSRect []

-- | subtitleFrame
--
-- Returns the receiver�s subtitle frame rectangle, which defines the position of the subtitle in its IKImageBrowserView.
--
-- The coordinates of this frame are in view's coordinate space. Subclasses can override this method to customize the position of the subtitle.
--
-- ObjC selector: @- subtitleFrame@
subtitleFrame :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSRect
subtitleFrame ikImageBrowserCell  =
  sendMsgStret ikImageBrowserCell (mkSelector "subtitleFrame") retNSRect []

-- | imageAlignment
--
-- Returns the position of the cell�s image in the frame. The default is NSImageAlignCenter.
--
-- Subclasses can override this method to customize the image alignment. For the list of possible alignments, see [NSImageView setImageAlignment:]. The image frame will be computed automatically from the image container frame by taking in account the image alignment and the image aspect ratio.
--
-- ObjC selector: @- imageAlignment@
imageAlignment :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO NSImageAlignment
imageAlignment ikImageBrowserCell  =
  fmap (coerce :: CULong -> NSImageAlignment) $ sendMsg ikImageBrowserCell (mkSelector "imageAlignment") retCULong []

-- | isSelected
--
-- Returns the selection state of the receiver.
--
-- Returns YES if the receiver is selected, otherwise NO. Subclasses should not override this method.
--
-- ObjC selector: @- isSelected@
isSelected :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO Bool
isSelected ikImageBrowserCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageBrowserCell (mkSelector "isSelected") retCULong []

-- | opacity
--
-- Determines the opacity of the receiver.
--
-- Possible values are between 0.0 (transparent) and 1.0 (opaque). Subclasses can override this method to customize the opacity of the cell.
--
-- ObjC selector: @- opacity@
opacity :: IsIKImageBrowserCell ikImageBrowserCell => ikImageBrowserCell -> IO CDouble
opacity ikImageBrowserCell  =
  sendMsg ikImageBrowserCell (mkSelector "opacity") retCDouble []

-- | layerForType:
--
-- Provides the receiver�s layer for the given type. The default is nil.
--
-- Subclasses can override this method to add a layer in the background, foreground... of the cell (see possible types above).
--
-- ObjC selector: @- layerForType:@
layerForType :: (IsIKImageBrowserCell ikImageBrowserCell, IsNSString type_) => ikImageBrowserCell -> type_ -> IO (Id CALayer)
layerForType ikImageBrowserCell  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg ikImageBrowserCell (mkSelector "layerForType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageBrowserView@
imageBrowserViewSelector :: Selector
imageBrowserViewSelector = mkSelector "imageBrowserView"

-- | @Selector@ for @representedItem@
representedItemSelector :: Selector
representedItemSelector = mkSelector "representedItem"

-- | @Selector@ for @indexOfRepresentedItem@
indexOfRepresentedItemSelector :: Selector
indexOfRepresentedItemSelector = mkSelector "indexOfRepresentedItem"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @imageContainerFrame@
imageContainerFrameSelector :: Selector
imageContainerFrameSelector = mkSelector "imageContainerFrame"

-- | @Selector@ for @imageFrame@
imageFrameSelector :: Selector
imageFrameSelector = mkSelector "imageFrame"

-- | @Selector@ for @selectionFrame@
selectionFrameSelector :: Selector
selectionFrameSelector = mkSelector "selectionFrame"

-- | @Selector@ for @titleFrame@
titleFrameSelector :: Selector
titleFrameSelector = mkSelector "titleFrame"

-- | @Selector@ for @subtitleFrame@
subtitleFrameSelector :: Selector
subtitleFrameSelector = mkSelector "subtitleFrame"

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @isSelected@
isSelectedSelector :: Selector
isSelectedSelector = mkSelector "isSelected"

-- | @Selector@ for @opacity@
opacitySelector :: Selector
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @layerForType:@
layerForTypeSelector :: Selector
layerForTypeSelector = mkSelector "layerForType:"

