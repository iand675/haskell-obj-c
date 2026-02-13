{-# LANGUAGE DataKinds #-}
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
  , calcSizeSelector
  , drawBorderAndBackgroundWithFrame_inViewSelector
  , drawImageWithFrame_inViewSelector
  , drawKeyEquivalentWithFrame_inViewSelector
  , drawSeparatorItemWithFrame_inViewSelector
  , drawStateImageWithFrame_inViewSelector
  , drawTitleWithFrame_inViewSelector
  , imageWidthSelector
  , initTextCellSelector
  , initWithCoderSelector
  , keyEquivalentRectForBoundsSelector
  , keyEquivalentWidthSelector
  , menuItemSelector
  , needsDisplaySelector
  , needsSizingSelector
  , setMenuItemSelector
  , setNeedsDisplaySelector
  , setNeedsSizingSelector
  , setTagSelector
  , stateImageRectForBoundsSelector
  , stateImageWidthSelector
  , tagSelector
  , titleRectForBoundsSelector
  , titleWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSMenuItemCell nsMenuItemCell, IsNSString string) => nsMenuItemCell -> string -> IO (Id NSMenuItemCell)
initTextCell nsMenuItemCell string =
  sendOwnedMessage nsMenuItemCell initTextCellSelector (toNSString string)

-- | @- initWithCoder:@
initWithCoder :: (IsNSMenuItemCell nsMenuItemCell, IsNSCoder coder) => nsMenuItemCell -> coder -> IO (Id NSMenuItemCell)
initWithCoder nsMenuItemCell coder =
  sendOwnedMessage nsMenuItemCell initWithCoderSelector (toNSCoder coder)

-- | @- calcSize@
calcSize :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO ()
calcSize nsMenuItemCell =
  sendMessage nsMenuItemCell calcSizeSelector

-- | @- stateImageRectForBounds:@
stateImageRectForBounds :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> NSRect -> IO NSRect
stateImageRectForBounds nsMenuItemCell cellFrame =
  sendMessage nsMenuItemCell stateImageRectForBoundsSelector cellFrame

-- | @- titleRectForBounds:@
titleRectForBounds :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> NSRect -> IO NSRect
titleRectForBounds nsMenuItemCell cellFrame =
  sendMessage nsMenuItemCell titleRectForBoundsSelector cellFrame

-- | @- keyEquivalentRectForBounds:@
keyEquivalentRectForBounds :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> NSRect -> IO NSRect
keyEquivalentRectForBounds nsMenuItemCell cellFrame =
  sendMessage nsMenuItemCell keyEquivalentRectForBoundsSelector cellFrame

-- | @- drawSeparatorItemWithFrame:inView:@
drawSeparatorItemWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawSeparatorItemWithFrame_inView nsMenuItemCell cellFrame controlView =
  sendMessage nsMenuItemCell drawSeparatorItemWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- drawStateImageWithFrame:inView:@
drawStateImageWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawStateImageWithFrame_inView nsMenuItemCell cellFrame controlView =
  sendMessage nsMenuItemCell drawStateImageWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- drawImageWithFrame:inView:@
drawImageWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawImageWithFrame_inView nsMenuItemCell cellFrame controlView =
  sendMessage nsMenuItemCell drawImageWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- drawTitleWithFrame:inView:@
drawTitleWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawTitleWithFrame_inView nsMenuItemCell cellFrame controlView =
  sendMessage nsMenuItemCell drawTitleWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- drawKeyEquivalentWithFrame:inView:@
drawKeyEquivalentWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawKeyEquivalentWithFrame_inView nsMenuItemCell cellFrame controlView =
  sendMessage nsMenuItemCell drawKeyEquivalentWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- drawBorderAndBackgroundWithFrame:inView:@
drawBorderAndBackgroundWithFrame_inView :: (IsNSMenuItemCell nsMenuItemCell, IsNSView controlView) => nsMenuItemCell -> NSRect -> controlView -> IO ()
drawBorderAndBackgroundWithFrame_inView nsMenuItemCell cellFrame controlView =
  sendMessage nsMenuItemCell drawBorderAndBackgroundWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- menuItem@
menuItem :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO (Id NSMenuItem)
menuItem nsMenuItemCell =
  sendMessage nsMenuItemCell menuItemSelector

-- | @- setMenuItem:@
setMenuItem :: (IsNSMenuItemCell nsMenuItemCell, IsNSMenuItem value) => nsMenuItemCell -> value -> IO ()
setMenuItem nsMenuItemCell value =
  sendMessage nsMenuItemCell setMenuItemSelector (toNSMenuItem value)

-- | @- needsSizing@
needsSizing :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO Bool
needsSizing nsMenuItemCell =
  sendMessage nsMenuItemCell needsSizingSelector

-- | @- setNeedsSizing:@
setNeedsSizing :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> Bool -> IO ()
setNeedsSizing nsMenuItemCell value =
  sendMessage nsMenuItemCell setNeedsSizingSelector value

-- | @- needsDisplay@
needsDisplay :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO Bool
needsDisplay nsMenuItemCell =
  sendMessage nsMenuItemCell needsDisplaySelector

-- | @- setNeedsDisplay:@
setNeedsDisplay :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> Bool -> IO ()
setNeedsDisplay nsMenuItemCell value =
  sendMessage nsMenuItemCell setNeedsDisplaySelector value

-- | @- stateImageWidth@
stateImageWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
stateImageWidth nsMenuItemCell =
  sendMessage nsMenuItemCell stateImageWidthSelector

-- | @- imageWidth@
imageWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
imageWidth nsMenuItemCell =
  sendMessage nsMenuItemCell imageWidthSelector

-- | @- titleWidth@
titleWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
titleWidth nsMenuItemCell =
  sendMessage nsMenuItemCell titleWidthSelector

-- | @- keyEquivalentWidth@
keyEquivalentWidth :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CDouble
keyEquivalentWidth nsMenuItemCell =
  sendMessage nsMenuItemCell keyEquivalentWidthSelector

-- | @- tag@
tag :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> IO CLong
tag nsMenuItemCell =
  sendMessage nsMenuItemCell tagSelector

-- | @- setTag:@
setTag :: IsNSMenuItemCell nsMenuItemCell => nsMenuItemCell -> CLong -> IO ()
setTag nsMenuItemCell value =
  sendMessage nsMenuItemCell setTagSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSMenuItemCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMenuItemCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @calcSize@
calcSizeSelector :: Selector '[] ()
calcSizeSelector = mkSelector "calcSize"

-- | @Selector@ for @stateImageRectForBounds:@
stateImageRectForBoundsSelector :: Selector '[NSRect] NSRect
stateImageRectForBoundsSelector = mkSelector "stateImageRectForBounds:"

-- | @Selector@ for @titleRectForBounds:@
titleRectForBoundsSelector :: Selector '[NSRect] NSRect
titleRectForBoundsSelector = mkSelector "titleRectForBounds:"

-- | @Selector@ for @keyEquivalentRectForBounds:@
keyEquivalentRectForBoundsSelector :: Selector '[NSRect] NSRect
keyEquivalentRectForBoundsSelector = mkSelector "keyEquivalentRectForBounds:"

-- | @Selector@ for @drawSeparatorItemWithFrame:inView:@
drawSeparatorItemWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawSeparatorItemWithFrame_inViewSelector = mkSelector "drawSeparatorItemWithFrame:inView:"

-- | @Selector@ for @drawStateImageWithFrame:inView:@
drawStateImageWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawStateImageWithFrame_inViewSelector = mkSelector "drawStateImageWithFrame:inView:"

-- | @Selector@ for @drawImageWithFrame:inView:@
drawImageWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawImageWithFrame_inViewSelector = mkSelector "drawImageWithFrame:inView:"

-- | @Selector@ for @drawTitleWithFrame:inView:@
drawTitleWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawTitleWithFrame_inViewSelector = mkSelector "drawTitleWithFrame:inView:"

-- | @Selector@ for @drawKeyEquivalentWithFrame:inView:@
drawKeyEquivalentWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawKeyEquivalentWithFrame_inViewSelector = mkSelector "drawKeyEquivalentWithFrame:inView:"

-- | @Selector@ for @drawBorderAndBackgroundWithFrame:inView:@
drawBorderAndBackgroundWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawBorderAndBackgroundWithFrame_inViewSelector = mkSelector "drawBorderAndBackgroundWithFrame:inView:"

-- | @Selector@ for @menuItem@
menuItemSelector :: Selector '[] (Id NSMenuItem)
menuItemSelector = mkSelector "menuItem"

-- | @Selector@ for @setMenuItem:@
setMenuItemSelector :: Selector '[Id NSMenuItem] ()
setMenuItemSelector = mkSelector "setMenuItem:"

-- | @Selector@ for @needsSizing@
needsSizingSelector :: Selector '[] Bool
needsSizingSelector = mkSelector "needsSizing"

-- | @Selector@ for @setNeedsSizing:@
setNeedsSizingSelector :: Selector '[Bool] ()
setNeedsSizingSelector = mkSelector "setNeedsSizing:"

-- | @Selector@ for @needsDisplay@
needsDisplaySelector :: Selector '[] Bool
needsDisplaySelector = mkSelector "needsDisplay"

-- | @Selector@ for @setNeedsDisplay:@
setNeedsDisplaySelector :: Selector '[Bool] ()
setNeedsDisplaySelector = mkSelector "setNeedsDisplay:"

-- | @Selector@ for @stateImageWidth@
stateImageWidthSelector :: Selector '[] CDouble
stateImageWidthSelector = mkSelector "stateImageWidth"

-- | @Selector@ for @imageWidth@
imageWidthSelector :: Selector '[] CDouble
imageWidthSelector = mkSelector "imageWidth"

-- | @Selector@ for @titleWidth@
titleWidthSelector :: Selector '[] CDouble
titleWidthSelector = mkSelector "titleWidth"

-- | @Selector@ for @keyEquivalentWidth@
keyEquivalentWidthSelector :: Selector '[] CDouble
keyEquivalentWidthSelector = mkSelector "keyEquivalentWidth"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CLong
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[CLong] ()
setTagSelector = mkSelector "setTag:"

