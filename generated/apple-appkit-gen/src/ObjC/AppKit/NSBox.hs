{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBox@.
module ObjC.AppKit.NSBox
  ( NSBox
  , IsNSBox(..)
  , sizeToFit
  , setFrameFromContentFrame
  , setTitleWithMnemonic
  , boxType
  , setBoxType
  , titlePosition
  , setTitlePosition
  , title
  , setTitle
  , titleFont
  , setTitleFont
  , borderRect
  , titleRect
  , titleCell
  , contentViewMargins
  , setContentViewMargins
  , contentView
  , setContentView
  , transparent
  , setTransparent
  , borderWidth
  , setBorderWidth
  , cornerRadius
  , setCornerRadius
  , borderColor
  , setBorderColor
  , fillColor
  , setFillColor
  , borderType
  , setBorderType
  , borderColorSelector
  , borderRectSelector
  , borderTypeSelector
  , borderWidthSelector
  , boxTypeSelector
  , contentViewMarginsSelector
  , contentViewSelector
  , cornerRadiusSelector
  , fillColorSelector
  , setBorderColorSelector
  , setBorderTypeSelector
  , setBorderWidthSelector
  , setBoxTypeSelector
  , setContentViewMarginsSelector
  , setContentViewSelector
  , setCornerRadiusSelector
  , setFillColorSelector
  , setFrameFromContentFrameSelector
  , setTitleFontSelector
  , setTitlePositionSelector
  , setTitleSelector
  , setTitleWithMnemonicSelector
  , setTransparentSelector
  , sizeToFitSelector
  , titleCellSelector
  , titleFontSelector
  , titlePositionSelector
  , titleRectSelector
  , titleSelector
  , transparentSelector

  -- * Enum types
  , NSBorderType(NSBorderType)
  , pattern NSNoBorder
  , pattern NSLineBorder
  , pattern NSBezelBorder
  , pattern NSGrooveBorder
  , NSBoxType(NSBoxType)
  , pattern NSBoxPrimary
  , pattern NSBoxSeparator
  , pattern NSBoxCustom
  , NSTitlePosition(NSTitlePosition)
  , pattern NSNoTitle
  , pattern NSAboveTop
  , pattern NSAtTop
  , pattern NSBelowTop
  , pattern NSAboveBottom
  , pattern NSAtBottom
  , pattern NSBelowBottom

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

-- | @- sizeToFit@
sizeToFit :: IsNSBox nsBox => nsBox -> IO ()
sizeToFit nsBox =
  sendMessage nsBox sizeToFitSelector

-- | @- setFrameFromContentFrame:@
setFrameFromContentFrame :: IsNSBox nsBox => nsBox -> NSRect -> IO ()
setFrameFromContentFrame nsBox contentFrame =
  sendMessage nsBox setFrameFromContentFrameSelector contentFrame

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSBox nsBox, IsNSString stringWithAmpersand) => nsBox -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsBox stringWithAmpersand =
  sendMessage nsBox setTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | @- boxType@
boxType :: IsNSBox nsBox => nsBox -> IO NSBoxType
boxType nsBox =
  sendMessage nsBox boxTypeSelector

-- | @- setBoxType:@
setBoxType :: IsNSBox nsBox => nsBox -> NSBoxType -> IO ()
setBoxType nsBox value =
  sendMessage nsBox setBoxTypeSelector value

-- | @- titlePosition@
titlePosition :: IsNSBox nsBox => nsBox -> IO NSTitlePosition
titlePosition nsBox =
  sendMessage nsBox titlePositionSelector

-- | @- setTitlePosition:@
setTitlePosition :: IsNSBox nsBox => nsBox -> NSTitlePosition -> IO ()
setTitlePosition nsBox value =
  sendMessage nsBox setTitlePositionSelector value

-- | @- title@
title :: IsNSBox nsBox => nsBox -> IO (Id NSString)
title nsBox =
  sendMessage nsBox titleSelector

-- | @- setTitle:@
setTitle :: (IsNSBox nsBox, IsNSString value) => nsBox -> value -> IO ()
setTitle nsBox value =
  sendMessage nsBox setTitleSelector (toNSString value)

-- | @- titleFont@
titleFont :: IsNSBox nsBox => nsBox -> IO (Id NSFont)
titleFont nsBox =
  sendMessage nsBox titleFontSelector

-- | @- setTitleFont:@
setTitleFont :: (IsNSBox nsBox, IsNSFont value) => nsBox -> value -> IO ()
setTitleFont nsBox value =
  sendMessage nsBox setTitleFontSelector (toNSFont value)

-- | @- borderRect@
borderRect :: IsNSBox nsBox => nsBox -> IO NSRect
borderRect nsBox =
  sendMessage nsBox borderRectSelector

-- | @- titleRect@
titleRect :: IsNSBox nsBox => nsBox -> IO NSRect
titleRect nsBox =
  sendMessage nsBox titleRectSelector

-- | @- titleCell@
titleCell :: IsNSBox nsBox => nsBox -> IO RawId
titleCell nsBox =
  sendMessage nsBox titleCellSelector

-- | @- contentViewMargins@
contentViewMargins :: IsNSBox nsBox => nsBox -> IO NSSize
contentViewMargins nsBox =
  sendMessage nsBox contentViewMarginsSelector

-- | @- setContentViewMargins:@
setContentViewMargins :: IsNSBox nsBox => nsBox -> NSSize -> IO ()
setContentViewMargins nsBox value =
  sendMessage nsBox setContentViewMarginsSelector value

-- | @- contentView@
contentView :: IsNSBox nsBox => nsBox -> IO (Id NSView)
contentView nsBox =
  sendMessage nsBox contentViewSelector

-- | @- setContentView:@
setContentView :: (IsNSBox nsBox, IsNSView value) => nsBox -> value -> IO ()
setContentView nsBox value =
  sendMessage nsBox setContentViewSelector (toNSView value)

-- | @- transparent@
transparent :: IsNSBox nsBox => nsBox -> IO Bool
transparent nsBox =
  sendMessage nsBox transparentSelector

-- | @- setTransparent:@
setTransparent :: IsNSBox nsBox => nsBox -> Bool -> IO ()
setTransparent nsBox value =
  sendMessage nsBox setTransparentSelector value

-- | @- borderWidth@
borderWidth :: IsNSBox nsBox => nsBox -> IO CDouble
borderWidth nsBox =
  sendMessage nsBox borderWidthSelector

-- | @- setBorderWidth:@
setBorderWidth :: IsNSBox nsBox => nsBox -> CDouble -> IO ()
setBorderWidth nsBox value =
  sendMessage nsBox setBorderWidthSelector value

-- | @- cornerRadius@
cornerRadius :: IsNSBox nsBox => nsBox -> IO CDouble
cornerRadius nsBox =
  sendMessage nsBox cornerRadiusSelector

-- | @- setCornerRadius:@
setCornerRadius :: IsNSBox nsBox => nsBox -> CDouble -> IO ()
setCornerRadius nsBox value =
  sendMessage nsBox setCornerRadiusSelector value

-- | @- borderColor@
borderColor :: IsNSBox nsBox => nsBox -> IO (Id NSColor)
borderColor nsBox =
  sendMessage nsBox borderColorSelector

-- | @- setBorderColor:@
setBorderColor :: (IsNSBox nsBox, IsNSColor value) => nsBox -> value -> IO ()
setBorderColor nsBox value =
  sendMessage nsBox setBorderColorSelector (toNSColor value)

-- | @- fillColor@
fillColor :: IsNSBox nsBox => nsBox -> IO (Id NSColor)
fillColor nsBox =
  sendMessage nsBox fillColorSelector

-- | @- setFillColor:@
setFillColor :: (IsNSBox nsBox, IsNSColor value) => nsBox -> value -> IO ()
setFillColor nsBox value =
  sendMessage nsBox setFillColorSelector (toNSColor value)

-- | @- borderType@
borderType :: IsNSBox nsBox => nsBox -> IO NSBorderType
borderType nsBox =
  sendMessage nsBox borderTypeSelector

-- | @- setBorderType:@
setBorderType :: IsNSBox nsBox => nsBox -> NSBorderType -> IO ()
setBorderType nsBox value =
  sendMessage nsBox setBorderTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @setFrameFromContentFrame:@
setFrameFromContentFrameSelector :: Selector '[NSRect] ()
setFrameFromContentFrameSelector = mkSelector "setFrameFromContentFrame:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @boxType@
boxTypeSelector :: Selector '[] NSBoxType
boxTypeSelector = mkSelector "boxType"

-- | @Selector@ for @setBoxType:@
setBoxTypeSelector :: Selector '[NSBoxType] ()
setBoxTypeSelector = mkSelector "setBoxType:"

-- | @Selector@ for @titlePosition@
titlePositionSelector :: Selector '[] NSTitlePosition
titlePositionSelector = mkSelector "titlePosition"

-- | @Selector@ for @setTitlePosition:@
setTitlePositionSelector :: Selector '[NSTitlePosition] ()
setTitlePositionSelector = mkSelector "setTitlePosition:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @titleFont@
titleFontSelector :: Selector '[] (Id NSFont)
titleFontSelector = mkSelector "titleFont"

-- | @Selector@ for @setTitleFont:@
setTitleFontSelector :: Selector '[Id NSFont] ()
setTitleFontSelector = mkSelector "setTitleFont:"

-- | @Selector@ for @borderRect@
borderRectSelector :: Selector '[] NSRect
borderRectSelector = mkSelector "borderRect"

-- | @Selector@ for @titleRect@
titleRectSelector :: Selector '[] NSRect
titleRectSelector = mkSelector "titleRect"

-- | @Selector@ for @titleCell@
titleCellSelector :: Selector '[] RawId
titleCellSelector = mkSelector "titleCell"

-- | @Selector@ for @contentViewMargins@
contentViewMarginsSelector :: Selector '[] NSSize
contentViewMarginsSelector = mkSelector "contentViewMargins"

-- | @Selector@ for @setContentViewMargins:@
setContentViewMarginsSelector :: Selector '[NSSize] ()
setContentViewMarginsSelector = mkSelector "setContentViewMargins:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @transparent@
transparentSelector :: Selector '[] Bool
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @setTransparent:@
setTransparentSelector :: Selector '[Bool] ()
setTransparentSelector = mkSelector "setTransparent:"

-- | @Selector@ for @borderWidth@
borderWidthSelector :: Selector '[] CDouble
borderWidthSelector = mkSelector "borderWidth"

-- | @Selector@ for @setBorderWidth:@
setBorderWidthSelector :: Selector '[CDouble] ()
setBorderWidthSelector = mkSelector "setBorderWidth:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector '[] (Id NSColor)
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[Id NSColor] ()
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector '[] (Id NSColor)
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector '[Id NSColor] ()
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @borderType@
borderTypeSelector :: Selector '[] NSBorderType
borderTypeSelector = mkSelector "borderType"

-- | @Selector@ for @setBorderType:@
setBorderTypeSelector :: Selector '[NSBorderType] ()
setBorderTypeSelector = mkSelector "setBorderType:"

