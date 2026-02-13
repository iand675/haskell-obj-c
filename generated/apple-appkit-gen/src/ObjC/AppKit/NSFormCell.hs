{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFormCell@.
module ObjC.AppKit.NSFormCell
  ( NSFormCell
  , IsNSFormCell(..)
  , initTextCell
  , initWithCoder
  , initImageCell
  , titleWidth
  , setTitleWithMnemonic
  , setTitleWidth
  , title
  , setTitle
  , titleFont
  , setTitleFont
  , opaque
  , placeholderString
  , setPlaceholderString
  , placeholderAttributedString
  , setPlaceholderAttributedString
  , titleAlignment
  , setTitleAlignment
  , titleBaseWritingDirection
  , setTitleBaseWritingDirection
  , preferredTextFieldWidth
  , setPreferredTextFieldWidth
  , attributedTitle
  , setAttributedTitle
  , attributedTitleSelector
  , initImageCellSelector
  , initTextCellSelector
  , initWithCoderSelector
  , opaqueSelector
  , placeholderAttributedStringSelector
  , placeholderStringSelector
  , preferredTextFieldWidthSelector
  , setAttributedTitleSelector
  , setPlaceholderAttributedStringSelector
  , setPlaceholderStringSelector
  , setPreferredTextFieldWidthSelector
  , setTitleAlignmentSelector
  , setTitleBaseWritingDirectionSelector
  , setTitleFontSelector
  , setTitleSelector
  , setTitleWidthSelector
  , setTitleWithMnemonicSelector
  , titleAlignmentSelector
  , titleBaseWritingDirectionSelector
  , titleFontSelector
  , titleSelector
  , titleWidthSelector

  -- * Enum types
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
  , NSWritingDirection(NSWritingDirection)
  , pattern NSWritingDirectionNatural
  , pattern NSWritingDirectionLeftToRight
  , pattern NSWritingDirectionRightToLeft

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

-- | @- initTextCell:@
initTextCell :: (IsNSFormCell nsFormCell, IsNSString string) => nsFormCell -> string -> IO (Id NSFormCell)
initTextCell nsFormCell string =
  sendOwnedMessage nsFormCell initTextCellSelector (toNSString string)

-- | @- initWithCoder:@
initWithCoder :: (IsNSFormCell nsFormCell, IsNSCoder coder) => nsFormCell -> coder -> IO (Id NSFormCell)
initWithCoder nsFormCell coder =
  sendOwnedMessage nsFormCell initWithCoderSelector (toNSCoder coder)

-- | @- initImageCell:@
initImageCell :: (IsNSFormCell nsFormCell, IsNSImage image) => nsFormCell -> image -> IO (Id NSFormCell)
initImageCell nsFormCell image =
  sendOwnedMessage nsFormCell initImageCellSelector (toNSImage image)

-- | @- titleWidth:@
titleWidth :: IsNSFormCell nsFormCell => nsFormCell -> NSSize -> IO CDouble
titleWidth nsFormCell size =
  sendMessage nsFormCell titleWidthSelector size

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSFormCell nsFormCell, IsNSString stringWithAmpersand) => nsFormCell -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsFormCell stringWithAmpersand =
  sendMessage nsFormCell setTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | @- setTitleWidth:@
setTitleWidth :: IsNSFormCell nsFormCell => nsFormCell -> CDouble -> IO ()
setTitleWidth nsFormCell value =
  sendMessage nsFormCell setTitleWidthSelector value

-- | @- title@
title :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSString)
title nsFormCell =
  sendMessage nsFormCell titleSelector

-- | @- setTitle:@
setTitle :: (IsNSFormCell nsFormCell, IsNSString value) => nsFormCell -> value -> IO ()
setTitle nsFormCell value =
  sendMessage nsFormCell setTitleSelector (toNSString value)

-- | @- titleFont@
titleFont :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSFont)
titleFont nsFormCell =
  sendMessage nsFormCell titleFontSelector

-- | @- setTitleFont:@
setTitleFont :: (IsNSFormCell nsFormCell, IsNSFont value) => nsFormCell -> value -> IO ()
setTitleFont nsFormCell value =
  sendMessage nsFormCell setTitleFontSelector (toNSFont value)

-- | @- opaque@
opaque :: IsNSFormCell nsFormCell => nsFormCell -> IO Bool
opaque nsFormCell =
  sendMessage nsFormCell opaqueSelector

-- | @- placeholderString@
placeholderString :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSString)
placeholderString nsFormCell =
  sendMessage nsFormCell placeholderStringSelector

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSFormCell nsFormCell, IsNSString value) => nsFormCell -> value -> IO ()
setPlaceholderString nsFormCell value =
  sendMessage nsFormCell setPlaceholderStringSelector (toNSString value)

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSAttributedString)
placeholderAttributedString nsFormCell =
  sendMessage nsFormCell placeholderAttributedStringSelector

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSFormCell nsFormCell, IsNSAttributedString value) => nsFormCell -> value -> IO ()
setPlaceholderAttributedString nsFormCell value =
  sendMessage nsFormCell setPlaceholderAttributedStringSelector (toNSAttributedString value)

-- | @- titleAlignment@
titleAlignment :: IsNSFormCell nsFormCell => nsFormCell -> IO NSTextAlignment
titleAlignment nsFormCell =
  sendMessage nsFormCell titleAlignmentSelector

-- | @- setTitleAlignment:@
setTitleAlignment :: IsNSFormCell nsFormCell => nsFormCell -> NSTextAlignment -> IO ()
setTitleAlignment nsFormCell value =
  sendMessage nsFormCell setTitleAlignmentSelector value

-- | @- titleBaseWritingDirection@
titleBaseWritingDirection :: IsNSFormCell nsFormCell => nsFormCell -> IO NSWritingDirection
titleBaseWritingDirection nsFormCell =
  sendMessage nsFormCell titleBaseWritingDirectionSelector

-- | @- setTitleBaseWritingDirection:@
setTitleBaseWritingDirection :: IsNSFormCell nsFormCell => nsFormCell -> NSWritingDirection -> IO ()
setTitleBaseWritingDirection nsFormCell value =
  sendMessage nsFormCell setTitleBaseWritingDirectionSelector value

-- | @- preferredTextFieldWidth@
preferredTextFieldWidth :: IsNSFormCell nsFormCell => nsFormCell -> IO CDouble
preferredTextFieldWidth nsFormCell =
  sendMessage nsFormCell preferredTextFieldWidthSelector

-- | @- setPreferredTextFieldWidth:@
setPreferredTextFieldWidth :: IsNSFormCell nsFormCell => nsFormCell -> CDouble -> IO ()
setPreferredTextFieldWidth nsFormCell value =
  sendMessage nsFormCell setPreferredTextFieldWidthSelector value

-- | @- attributedTitle@
attributedTitle :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSAttributedString)
attributedTitle nsFormCell =
  sendMessage nsFormCell attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSFormCell nsFormCell, IsNSAttributedString value) => nsFormCell -> value -> IO ()
setAttributedTitle nsFormCell value =
  sendMessage nsFormCell setAttributedTitleSelector (toNSAttributedString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSFormCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSFormCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSFormCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @titleWidth:@
titleWidthSelector :: Selector '[NSSize] CDouble
titleWidthSelector = mkSelector "titleWidth:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @setTitleWidth:@
setTitleWidthSelector :: Selector '[CDouble] ()
setTitleWidthSelector = mkSelector "setTitleWidth:"

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

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @placeholderString@
placeholderStringSelector :: Selector '[] (Id NSString)
placeholderStringSelector = mkSelector "placeholderString"

-- | @Selector@ for @setPlaceholderString:@
setPlaceholderStringSelector :: Selector '[Id NSString] ()
setPlaceholderStringSelector = mkSelector "setPlaceholderString:"

-- | @Selector@ for @placeholderAttributedString@
placeholderAttributedStringSelector :: Selector '[] (Id NSAttributedString)
placeholderAttributedStringSelector = mkSelector "placeholderAttributedString"

-- | @Selector@ for @setPlaceholderAttributedString:@
setPlaceholderAttributedStringSelector :: Selector '[Id NSAttributedString] ()
setPlaceholderAttributedStringSelector = mkSelector "setPlaceholderAttributedString:"

-- | @Selector@ for @titleAlignment@
titleAlignmentSelector :: Selector '[] NSTextAlignment
titleAlignmentSelector = mkSelector "titleAlignment"

-- | @Selector@ for @setTitleAlignment:@
setTitleAlignmentSelector :: Selector '[NSTextAlignment] ()
setTitleAlignmentSelector = mkSelector "setTitleAlignment:"

-- | @Selector@ for @titleBaseWritingDirection@
titleBaseWritingDirectionSelector :: Selector '[] NSWritingDirection
titleBaseWritingDirectionSelector = mkSelector "titleBaseWritingDirection"

-- | @Selector@ for @setTitleBaseWritingDirection:@
setTitleBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setTitleBaseWritingDirectionSelector = mkSelector "setTitleBaseWritingDirection:"

-- | @Selector@ for @preferredTextFieldWidth@
preferredTextFieldWidthSelector :: Selector '[] CDouble
preferredTextFieldWidthSelector = mkSelector "preferredTextFieldWidth"

-- | @Selector@ for @setPreferredTextFieldWidth:@
setPreferredTextFieldWidthSelector :: Selector '[CDouble] ()
setPreferredTextFieldWidthSelector = mkSelector "setPreferredTextFieldWidth:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector '[] (Id NSAttributedString)
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

