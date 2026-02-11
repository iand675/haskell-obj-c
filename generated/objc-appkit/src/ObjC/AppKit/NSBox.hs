{-# LANGUAGE PatternSynonyms #-}
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
  , borderType
  , setBorderType
  , sizeToFitSelector
  , setFrameFromContentFrameSelector
  , setTitleWithMnemonicSelector
  , boxTypeSelector
  , setBoxTypeSelector
  , titlePositionSelector
  , setTitlePositionSelector
  , titleSelector
  , setTitleSelector
  , titleFontSelector
  , setTitleFontSelector
  , borderRectSelector
  , titleRectSelector
  , titleCellSelector
  , contentViewMarginsSelector
  , setContentViewMarginsSelector
  , contentViewSelector
  , setContentViewSelector
  , transparentSelector
  , setTransparentSelector
  , borderWidthSelector
  , setBorderWidthSelector
  , cornerRadiusSelector
  , setCornerRadiusSelector
  , borderTypeSelector
  , setBorderTypeSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- sizeToFit@
sizeToFit :: IsNSBox nsBox => nsBox -> IO ()
sizeToFit nsBox  =
  sendMsg nsBox (mkSelector "sizeToFit") retVoid []

-- | @- setFrameFromContentFrame:@
setFrameFromContentFrame :: IsNSBox nsBox => nsBox -> NSRect -> IO ()
setFrameFromContentFrame nsBox  contentFrame =
  sendMsg nsBox (mkSelector "setFrameFromContentFrame:") retVoid [argNSRect contentFrame]

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSBox nsBox, IsNSString stringWithAmpersand) => nsBox -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsBox  stringWithAmpersand =
withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
    sendMsg nsBox (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | @- boxType@
boxType :: IsNSBox nsBox => nsBox -> IO NSBoxType
boxType nsBox  =
  fmap (coerce :: CULong -> NSBoxType) $ sendMsg nsBox (mkSelector "boxType") retCULong []

-- | @- setBoxType:@
setBoxType :: IsNSBox nsBox => nsBox -> NSBoxType -> IO ()
setBoxType nsBox  value =
  sendMsg nsBox (mkSelector "setBoxType:") retVoid [argCULong (coerce value)]

-- | @- titlePosition@
titlePosition :: IsNSBox nsBox => nsBox -> IO NSTitlePosition
titlePosition nsBox  =
  fmap (coerce :: CULong -> NSTitlePosition) $ sendMsg nsBox (mkSelector "titlePosition") retCULong []

-- | @- setTitlePosition:@
setTitlePosition :: IsNSBox nsBox => nsBox -> NSTitlePosition -> IO ()
setTitlePosition nsBox  value =
  sendMsg nsBox (mkSelector "setTitlePosition:") retVoid [argCULong (coerce value)]

-- | @- title@
title :: IsNSBox nsBox => nsBox -> IO (Id NSString)
title nsBox  =
  sendMsg nsBox (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSBox nsBox, IsNSString value) => nsBox -> value -> IO ()
setTitle nsBox  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBox (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- titleFont@
titleFont :: IsNSBox nsBox => nsBox -> IO (Id NSFont)
titleFont nsBox  =
  sendMsg nsBox (mkSelector "titleFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitleFont:@
setTitleFont :: (IsNSBox nsBox, IsNSFont value) => nsBox -> value -> IO ()
setTitleFont nsBox  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBox (mkSelector "setTitleFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- borderRect@
borderRect :: IsNSBox nsBox => nsBox -> IO NSRect
borderRect nsBox  =
  sendMsgStret nsBox (mkSelector "borderRect") retNSRect []

-- | @- titleRect@
titleRect :: IsNSBox nsBox => nsBox -> IO NSRect
titleRect nsBox  =
  sendMsgStret nsBox (mkSelector "titleRect") retNSRect []

-- | @- titleCell@
titleCell :: IsNSBox nsBox => nsBox -> IO RawId
titleCell nsBox  =
  fmap (RawId . castPtr) $ sendMsg nsBox (mkSelector "titleCell") (retPtr retVoid) []

-- | @- contentViewMargins@
contentViewMargins :: IsNSBox nsBox => nsBox -> IO NSSize
contentViewMargins nsBox  =
  sendMsgStret nsBox (mkSelector "contentViewMargins") retNSSize []

-- | @- setContentViewMargins:@
setContentViewMargins :: IsNSBox nsBox => nsBox -> NSSize -> IO ()
setContentViewMargins nsBox  value =
  sendMsg nsBox (mkSelector "setContentViewMargins:") retVoid [argNSSize value]

-- | @- contentView@
contentView :: IsNSBox nsBox => nsBox -> IO (Id NSView)
contentView nsBox  =
  sendMsg nsBox (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentView:@
setContentView :: (IsNSBox nsBox, IsNSView value) => nsBox -> value -> IO ()
setContentView nsBox  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBox (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transparent@
transparent :: IsNSBox nsBox => nsBox -> IO Bool
transparent nsBox  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBox (mkSelector "transparent") retCULong []

-- | @- setTransparent:@
setTransparent :: IsNSBox nsBox => nsBox -> Bool -> IO ()
setTransparent nsBox  value =
  sendMsg nsBox (mkSelector "setTransparent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- borderWidth@
borderWidth :: IsNSBox nsBox => nsBox -> IO CDouble
borderWidth nsBox  =
  sendMsg nsBox (mkSelector "borderWidth") retCDouble []

-- | @- setBorderWidth:@
setBorderWidth :: IsNSBox nsBox => nsBox -> CDouble -> IO ()
setBorderWidth nsBox  value =
  sendMsg nsBox (mkSelector "setBorderWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- cornerRadius@
cornerRadius :: IsNSBox nsBox => nsBox -> IO CDouble
cornerRadius nsBox  =
  sendMsg nsBox (mkSelector "cornerRadius") retCDouble []

-- | @- setCornerRadius:@
setCornerRadius :: IsNSBox nsBox => nsBox -> CDouble -> IO ()
setCornerRadius nsBox  value =
  sendMsg nsBox (mkSelector "setCornerRadius:") retVoid [argCDouble (fromIntegral value)]

-- | @- borderType@
borderType :: IsNSBox nsBox => nsBox -> IO NSBorderType
borderType nsBox  =
  fmap (coerce :: CULong -> NSBorderType) $ sendMsg nsBox (mkSelector "borderType") retCULong []

-- | @- setBorderType:@
setBorderType :: IsNSBox nsBox => nsBox -> NSBorderType -> IO ()
setBorderType nsBox  value =
  sendMsg nsBox (mkSelector "setBorderType:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @setFrameFromContentFrame:@
setFrameFromContentFrameSelector :: Selector
setFrameFromContentFrameSelector = mkSelector "setFrameFromContentFrame:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @boxType@
boxTypeSelector :: Selector
boxTypeSelector = mkSelector "boxType"

-- | @Selector@ for @setBoxType:@
setBoxTypeSelector :: Selector
setBoxTypeSelector = mkSelector "setBoxType:"

-- | @Selector@ for @titlePosition@
titlePositionSelector :: Selector
titlePositionSelector = mkSelector "titlePosition"

-- | @Selector@ for @setTitlePosition:@
setTitlePositionSelector :: Selector
setTitlePositionSelector = mkSelector "setTitlePosition:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @titleFont@
titleFontSelector :: Selector
titleFontSelector = mkSelector "titleFont"

-- | @Selector@ for @setTitleFont:@
setTitleFontSelector :: Selector
setTitleFontSelector = mkSelector "setTitleFont:"

-- | @Selector@ for @borderRect@
borderRectSelector :: Selector
borderRectSelector = mkSelector "borderRect"

-- | @Selector@ for @titleRect@
titleRectSelector :: Selector
titleRectSelector = mkSelector "titleRect"

-- | @Selector@ for @titleCell@
titleCellSelector :: Selector
titleCellSelector = mkSelector "titleCell"

-- | @Selector@ for @contentViewMargins@
contentViewMarginsSelector :: Selector
contentViewMarginsSelector = mkSelector "contentViewMargins"

-- | @Selector@ for @setContentViewMargins:@
setContentViewMarginsSelector :: Selector
setContentViewMarginsSelector = mkSelector "setContentViewMargins:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @transparent@
transparentSelector :: Selector
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @setTransparent:@
setTransparentSelector :: Selector
setTransparentSelector = mkSelector "setTransparent:"

-- | @Selector@ for @borderWidth@
borderWidthSelector :: Selector
borderWidthSelector = mkSelector "borderWidth"

-- | @Selector@ for @setBorderWidth:@
setBorderWidthSelector :: Selector
setBorderWidthSelector = mkSelector "setBorderWidth:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @borderType@
borderTypeSelector :: Selector
borderTypeSelector = mkSelector "borderType"

-- | @Selector@ for @setBorderType:@
setBorderTypeSelector :: Selector
setBorderTypeSelector = mkSelector "setBorderType:"

