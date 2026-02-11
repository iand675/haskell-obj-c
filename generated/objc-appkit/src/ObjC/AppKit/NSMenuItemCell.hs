{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenuItemCell@.
module ObjC.AppKit.NSMenuItemCell
  ( NSMenuItemCell
  , IsNSMenuItemCell(..)
  , initTextCell
  , initWithCoder
  , calcSize
  , stateImageRectForBounds
  , titleRectForBounds
  , keyEquivalentRectForBounds
  , drawSeparatorItemWithFrame_inView
  , drawStateImageWithFrame_inView
  , drawImageWithFrame_inView
  , drawTitleWithFrame_inView
  , drawKeyEquivalentWithFrame_inView
  , drawBorderAndBackgroundWithFrame_inView
  , menuItem
  , setMenuItem
  , needsSizing
  , setNeedsSizing
  , needsDisplay
  , setNeedsDisplay
  , stateImageWidth
  , imageWidth
  , titleWidth
  , keyEquivalentWidth
  , tag
  , setTag
  , initTextCellSelector
  , initWithCoderSelector
  , calcSizeSelector
  , stateImageRectForBoundsSelector
  , titleRectForBoundsSelector
  , keyEquivalentRectForBoundsSelector
  , drawSeparatorItemWithFrame_inViewSelector
  , drawStateImageWithFrame_inViewSelector
  , drawImageWithFrame_inViewSelector
  , drawTitleWithFrame_inViewSelector
  , drawKeyEquivalentWithFrame_inViewSelector
  , drawBorderAndBackgroundWithFrame_inViewSelector
  , menuItemSelector
  , setMenuItemSelector
  , needsSizingSelector
  , setNeedsSizingSelector
  , needsDisplaySelector
  , setNeedsDisplaySelector
  , stateImageWidthSelector
  , imageWidthSelector
  , titleWidthSelector
  , keyEquivalentWidthSelector
  , tagSelector
  , setTagSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSMenuItemCell nsMenuItemCell, IsNSString string) => nsMenuItemCell -> string -> IO (Id NSMenuItemCell)
initTextCell nsMenuItemCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsMenuItemCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSMenuItemCell nsMenuItemCell, IsNSCoder coder) => nsMenuItemCell -> coder -> IO (Id NSMenuItemCell)
initWithCoder nsMenuItemCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsMenuItemCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- calcSize@
calcSize :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO ()
calcSize nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "calcSize") retVoid []

-- | @- stateImageRectForBounds:@
stateImageRectForBounds :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> NSRect -> IO NSRect
stateImageRectForBounds nsMenuItemCell  cellFrame =
  sendMsgStret nsMenuItemCell (mkSelector "stateImageRectForBounds:") retNSRect [argNSRect cellFrame]

-- | @- titleRectForBounds:@
titleRectForBounds :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> NSRect -> IO NSRect
titleRectForBounds nsMenuItemCell  cellFrame =
  sendMsgStret nsMenuItemCell (mkSelector "titleRectForBounds:") retNSRect [argNSRect cellFrame]

-- | @- keyEquivalentRectForBounds:@
keyEquivalentRectForBounds :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> NSRect -> IO NSRect
keyEquivalentRectForBounds nsMenuItemCell  cellFrame =
  sendMsgStret nsMenuItemCell (mkSelector "keyEquivalentRectForBounds:") retNSRect [argNSRect cellFrame]

-- | @- drawSeparatorItemWithFrame:inView:@
drawSeparatorItemWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawSeparatorItemWithFrame_inView nsMenuItemCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsMenuItemCell (mkSelector "drawSeparatorItemWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawStateImageWithFrame:inView:@
drawStateImageWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawStateImageWithFrame_inView nsMenuItemCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsMenuItemCell (mkSelector "drawStateImageWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawImageWithFrame:inView:@
drawImageWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawImageWithFrame_inView nsMenuItemCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsMenuItemCell (mkSelector "drawImageWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawTitleWithFrame:inView:@
drawTitleWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawTitleWithFrame_inView nsMenuItemCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsMenuItemCell (mkSelector "drawTitleWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawKeyEquivalentWithFrame:inView:@
drawKeyEquivalentWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawKeyEquivalentWithFrame_inView nsMenuItemCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsMenuItemCell (mkSelector "drawKeyEquivalentWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawBorderAndBackgroundWithFrame:inView:@
drawBorderAndBackgroundWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawBorderAndBackgroundWithFrame_inView nsMenuItemCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsMenuItemCell (mkSelector "drawBorderAndBackgroundWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- menuItem@
menuItem :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO (Id NSMenuItem)
menuItem nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "menuItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMenuItem:@
setMenuItem :: (IsNSMenuItemCell nsMenuItemCell, IsNSMenuItem value) => nsMenuItemCell -> value -> IO ()
setMenuItem nsMenuItemCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMenuItemCell (mkSelector "setMenuItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- needsSizing@
needsSizing :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO Bool
needsSizing nsMenuItemCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItemCell (mkSelector "needsSizing") retCULong []

-- | @- setNeedsSizing:@
setNeedsSizing :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> Bool -> IO ()
setNeedsSizing nsMenuItemCell  value =
  sendMsg nsMenuItemCell (mkSelector "setNeedsSizing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- needsDisplay@
needsDisplay :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO Bool
needsDisplay nsMenuItemCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItemCell (mkSelector "needsDisplay") retCULong []

-- | @- setNeedsDisplay:@
setNeedsDisplay :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> Bool -> IO ()
setNeedsDisplay nsMenuItemCell  value =
  sendMsg nsMenuItemCell (mkSelector "setNeedsDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | @- stateImageWidth@
stateImageWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
stateImageWidth nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "stateImageWidth") retCDouble []

-- | @- imageWidth@
imageWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
imageWidth nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "imageWidth") retCDouble []

-- | @- titleWidth@
titleWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
titleWidth nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "titleWidth") retCDouble []

-- | @- keyEquivalentWidth@
keyEquivalentWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
keyEquivalentWidth nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "keyEquivalentWidth") retCDouble []

-- | @- tag@
tag :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CLong
tag nsMenuItemCell  =
  sendMsg nsMenuItemCell (mkSelector "tag") retCLong []

-- | @- setTag:@
setTag :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> CLong -> IO ()
setTag nsMenuItemCell  value =
  sendMsg nsMenuItemCell (mkSelector "setTag:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @calcSize@
calcSizeSelector :: Selector
calcSizeSelector = mkSelector "calcSize"

-- | @Selector@ for @stateImageRectForBounds:@
stateImageRectForBoundsSelector :: Selector
stateImageRectForBoundsSelector = mkSelector "stateImageRectForBounds:"

-- | @Selector@ for @titleRectForBounds:@
titleRectForBoundsSelector :: Selector
titleRectForBoundsSelector = mkSelector "titleRectForBounds:"

-- | @Selector@ for @keyEquivalentRectForBounds:@
keyEquivalentRectForBoundsSelector :: Selector
keyEquivalentRectForBoundsSelector = mkSelector "keyEquivalentRectForBounds:"

-- | @Selector@ for @drawSeparatorItemWithFrame:inView:@
drawSeparatorItemWithFrame_inViewSelector :: Selector
drawSeparatorItemWithFrame_inViewSelector = mkSelector "drawSeparatorItemWithFrame:inView:"

-- | @Selector@ for @drawStateImageWithFrame:inView:@
drawStateImageWithFrame_inViewSelector :: Selector
drawStateImageWithFrame_inViewSelector = mkSelector "drawStateImageWithFrame:inView:"

-- | @Selector@ for @drawImageWithFrame:inView:@
drawImageWithFrame_inViewSelector :: Selector
drawImageWithFrame_inViewSelector = mkSelector "drawImageWithFrame:inView:"

-- | @Selector@ for @drawTitleWithFrame:inView:@
drawTitleWithFrame_inViewSelector :: Selector
drawTitleWithFrame_inViewSelector = mkSelector "drawTitleWithFrame:inView:"

-- | @Selector@ for @drawKeyEquivalentWithFrame:inView:@
drawKeyEquivalentWithFrame_inViewSelector :: Selector
drawKeyEquivalentWithFrame_inViewSelector = mkSelector "drawKeyEquivalentWithFrame:inView:"

-- | @Selector@ for @drawBorderAndBackgroundWithFrame:inView:@
drawBorderAndBackgroundWithFrame_inViewSelector :: Selector
drawBorderAndBackgroundWithFrame_inViewSelector = mkSelector "drawBorderAndBackgroundWithFrame:inView:"

-- | @Selector@ for @menuItem@
menuItemSelector :: Selector
menuItemSelector = mkSelector "menuItem"

-- | @Selector@ for @setMenuItem:@
setMenuItemSelector :: Selector
setMenuItemSelector = mkSelector "setMenuItem:"

-- | @Selector@ for @needsSizing@
needsSizingSelector :: Selector
needsSizingSelector = mkSelector "needsSizing"

-- | @Selector@ for @setNeedsSizing:@
setNeedsSizingSelector :: Selector
setNeedsSizingSelector = mkSelector "setNeedsSizing:"

-- | @Selector@ for @needsDisplay@
needsDisplaySelector :: Selector
needsDisplaySelector = mkSelector "needsDisplay"

-- | @Selector@ for @setNeedsDisplay:@
setNeedsDisplaySelector :: Selector
setNeedsDisplaySelector = mkSelector "setNeedsDisplay:"

-- | @Selector@ for @stateImageWidth@
stateImageWidthSelector :: Selector
stateImageWidthSelector = mkSelector "stateImageWidth"

-- | @Selector@ for @imageWidth@
imageWidthSelector :: Selector
imageWidthSelector = mkSelector "imageWidth"

-- | @Selector@ for @titleWidth@
titleWidthSelector :: Selector
titleWidthSelector = mkSelector "titleWidth"

-- | @Selector@ for @keyEquivalentWidth@
keyEquivalentWidthSelector :: Selector
keyEquivalentWidthSelector = mkSelector "keyEquivalentWidth"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

