{-# LANGUAGE PatternSynonyms #-}
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
  , initTextCellSelector
  , initWithCoderSelector
  , initImageCellSelector
  , titleWidthSelector
  , setTitleWithMnemonicSelector
  , setTitleWidthSelector
  , titleSelector
  , setTitleSelector
  , titleFontSelector
  , setTitleFontSelector
  , opaqueSelector
  , placeholderStringSelector
  , setPlaceholderStringSelector
  , placeholderAttributedStringSelector
  , setPlaceholderAttributedStringSelector
  , titleAlignmentSelector
  , setTitleAlignmentSelector
  , titleBaseWritingDirectionSelector
  , setTitleBaseWritingDirectionSelector
  , preferredTextFieldWidthSelector
  , setPreferredTextFieldWidthSelector
  , attributedTitleSelector
  , setAttributedTitleSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSFormCell nsFormCell, IsNSString string) => nsFormCell -> string -> IO (Id NSFormCell)
initTextCell nsFormCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsFormCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSFormCell nsFormCell, IsNSCoder coder) => nsFormCell -> coder -> IO (Id NSFormCell)
initWithCoder nsFormCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsFormCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSFormCell nsFormCell, IsNSImage image) => nsFormCell -> image -> IO (Id NSFormCell)
initImageCell nsFormCell  image =
withObjCPtr image $ \raw_image ->
    sendMsg nsFormCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- titleWidth:@
titleWidth :: IsNSFormCell nsFormCell => nsFormCell -> NSSize -> IO CDouble
titleWidth nsFormCell  size =
  sendMsg nsFormCell (mkSelector "titleWidth:") retCDouble [argNSSize size]

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSFormCell nsFormCell, IsNSString stringWithAmpersand) => nsFormCell -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsFormCell  stringWithAmpersand =
withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
    sendMsg nsFormCell (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | @- setTitleWidth:@
setTitleWidth :: IsNSFormCell nsFormCell => nsFormCell -> CDouble -> IO ()
setTitleWidth nsFormCell  value =
  sendMsg nsFormCell (mkSelector "setTitleWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- title@
title :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSString)
title nsFormCell  =
  sendMsg nsFormCell (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSFormCell nsFormCell, IsNSString value) => nsFormCell -> value -> IO ()
setTitle nsFormCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFormCell (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- titleFont@
titleFont :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSFont)
titleFont nsFormCell  =
  sendMsg nsFormCell (mkSelector "titleFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitleFont:@
setTitleFont :: (IsNSFormCell nsFormCell, IsNSFont value) => nsFormCell -> value -> IO ()
setTitleFont nsFormCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFormCell (mkSelector "setTitleFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- opaque@
opaque :: IsNSFormCell nsFormCell => nsFormCell -> IO Bool
opaque nsFormCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFormCell (mkSelector "opaque") retCULong []

-- | @- placeholderString@
placeholderString :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSString)
placeholderString nsFormCell  =
  sendMsg nsFormCell (mkSelector "placeholderString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSFormCell nsFormCell, IsNSString value) => nsFormCell -> value -> IO ()
setPlaceholderString nsFormCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFormCell (mkSelector "setPlaceholderString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSAttributedString)
placeholderAttributedString nsFormCell  =
  sendMsg nsFormCell (mkSelector "placeholderAttributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSFormCell nsFormCell, IsNSAttributedString value) => nsFormCell -> value -> IO ()
setPlaceholderAttributedString nsFormCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFormCell (mkSelector "setPlaceholderAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- titleAlignment@
titleAlignment :: IsNSFormCell nsFormCell => nsFormCell -> IO NSTextAlignment
titleAlignment nsFormCell  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsFormCell (mkSelector "titleAlignment") retCLong []

-- | @- setTitleAlignment:@
setTitleAlignment :: IsNSFormCell nsFormCell => nsFormCell -> NSTextAlignment -> IO ()
setTitleAlignment nsFormCell  value =
  sendMsg nsFormCell (mkSelector "setTitleAlignment:") retVoid [argCLong (coerce value)]

-- | @- titleBaseWritingDirection@
titleBaseWritingDirection :: IsNSFormCell nsFormCell => nsFormCell -> IO NSWritingDirection
titleBaseWritingDirection nsFormCell  =
  fmap (coerce :: CLong -> NSWritingDirection) $ sendMsg nsFormCell (mkSelector "titleBaseWritingDirection") retCLong []

-- | @- setTitleBaseWritingDirection:@
setTitleBaseWritingDirection :: IsNSFormCell nsFormCell => nsFormCell -> NSWritingDirection -> IO ()
setTitleBaseWritingDirection nsFormCell  value =
  sendMsg nsFormCell (mkSelector "setTitleBaseWritingDirection:") retVoid [argCLong (coerce value)]

-- | @- preferredTextFieldWidth@
preferredTextFieldWidth :: IsNSFormCell nsFormCell => nsFormCell -> IO CDouble
preferredTextFieldWidth nsFormCell  =
  sendMsg nsFormCell (mkSelector "preferredTextFieldWidth") retCDouble []

-- | @- setPreferredTextFieldWidth:@
setPreferredTextFieldWidth :: IsNSFormCell nsFormCell => nsFormCell -> CDouble -> IO ()
setPreferredTextFieldWidth nsFormCell  value =
  sendMsg nsFormCell (mkSelector "setPreferredTextFieldWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- attributedTitle@
attributedTitle :: IsNSFormCell nsFormCell => nsFormCell -> IO (Id NSAttributedString)
attributedTitle nsFormCell  =
  sendMsg nsFormCell (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSFormCell nsFormCell, IsNSAttributedString value) => nsFormCell -> value -> IO ()
setAttributedTitle nsFormCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFormCell (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @titleWidth:@
titleWidthSelector :: Selector
titleWidthSelector = mkSelector "titleWidth:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @setTitleWidth:@
setTitleWidthSelector :: Selector
setTitleWidthSelector = mkSelector "setTitleWidth:"

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

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @placeholderString@
placeholderStringSelector :: Selector
placeholderStringSelector = mkSelector "placeholderString"

-- | @Selector@ for @setPlaceholderString:@
setPlaceholderStringSelector :: Selector
setPlaceholderStringSelector = mkSelector "setPlaceholderString:"

-- | @Selector@ for @placeholderAttributedString@
placeholderAttributedStringSelector :: Selector
placeholderAttributedStringSelector = mkSelector "placeholderAttributedString"

-- | @Selector@ for @setPlaceholderAttributedString:@
setPlaceholderAttributedStringSelector :: Selector
setPlaceholderAttributedStringSelector = mkSelector "setPlaceholderAttributedString:"

-- | @Selector@ for @titleAlignment@
titleAlignmentSelector :: Selector
titleAlignmentSelector = mkSelector "titleAlignment"

-- | @Selector@ for @setTitleAlignment:@
setTitleAlignmentSelector :: Selector
setTitleAlignmentSelector = mkSelector "setTitleAlignment:"

-- | @Selector@ for @titleBaseWritingDirection@
titleBaseWritingDirectionSelector :: Selector
titleBaseWritingDirectionSelector = mkSelector "titleBaseWritingDirection"

-- | @Selector@ for @setTitleBaseWritingDirection:@
setTitleBaseWritingDirectionSelector :: Selector
setTitleBaseWritingDirectionSelector = mkSelector "setTitleBaseWritingDirection:"

-- | @Selector@ for @preferredTextFieldWidth@
preferredTextFieldWidthSelector :: Selector
preferredTextFieldWidthSelector = mkSelector "preferredTextFieldWidth"

-- | @Selector@ for @setPreferredTextFieldWidth:@
setPreferredTextFieldWidthSelector :: Selector
setPreferredTextFieldWidthSelector = mkSelector "setPreferredTextFieldWidth:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

