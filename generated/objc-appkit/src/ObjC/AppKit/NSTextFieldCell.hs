{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextFieldCell@.
module ObjC.AppKit.NSTextFieldCell
  ( NSTextFieldCell
  , IsNSTextFieldCell(..)
  , initTextCell
  , initWithCoder
  , initImageCell
  , setUpFieldEditorAttributes
  , setWantsNotificationForMarkedText
  , backgroundColor
  , setBackgroundColor
  , drawsBackground
  , setDrawsBackground
  , textColor
  , setTextColor
  , bezelStyle
  , setBezelStyle
  , placeholderString
  , setPlaceholderString
  , placeholderAttributedString
  , setPlaceholderAttributedString
  , initTextCellSelector
  , initWithCoderSelector
  , initImageCellSelector
  , setUpFieldEditorAttributesSelector
  , setWantsNotificationForMarkedTextSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , textColorSelector
  , setTextColorSelector
  , bezelStyleSelector
  , setBezelStyleSelector
  , placeholderStringSelector
  , setPlaceholderStringSelector
  , placeholderAttributedStringSelector
  , setPlaceholderAttributedStringSelector

  -- * Enum types
  , NSTextFieldBezelStyle(NSTextFieldBezelStyle)
  , pattern NSTextFieldSquareBezel
  , pattern NSTextFieldRoundedBezel

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

-- | @- initTextCell:@
initTextCell :: (IsNSTextFieldCell nsTextFieldCell, IsNSString string) => nsTextFieldCell -> string -> IO (Id NSTextFieldCell)
initTextCell nsTextFieldCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsTextFieldCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextFieldCell nsTextFieldCell, IsNSCoder coder) => nsTextFieldCell -> coder -> IO (Id NSTextFieldCell)
initWithCoder nsTextFieldCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTextFieldCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSTextFieldCell nsTextFieldCell, IsNSImage image) => nsTextFieldCell -> image -> IO (Id NSTextFieldCell)
initImageCell nsTextFieldCell  image =
withObjCPtr image $ \raw_image ->
    sendMsg nsTextFieldCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- setUpFieldEditorAttributes:@
setUpFieldEditorAttributes :: (IsNSTextFieldCell nsTextFieldCell, IsNSText textObj) => nsTextFieldCell -> textObj -> IO (Id NSText)
setUpFieldEditorAttributes nsTextFieldCell  textObj =
withObjCPtr textObj $ \raw_textObj ->
    sendMsg nsTextFieldCell (mkSelector "setUpFieldEditorAttributes:") (retPtr retVoid) [argPtr (castPtr raw_textObj :: Ptr ())] >>= retainedObject . castPtr

-- | @- setWantsNotificationForMarkedText:@
setWantsNotificationForMarkedText :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> Bool -> IO ()
setWantsNotificationForMarkedText nsTextFieldCell  flag =
  sendMsg nsTextFieldCell (mkSelector "setWantsNotificationForMarkedText:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSColor)
backgroundColor nsTextFieldCell  =
  sendMsg nsTextFieldCell (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextFieldCell nsTextFieldCell, IsNSColor value) => nsTextFieldCell -> value -> IO ()
setBackgroundColor nsTextFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextFieldCell (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawsBackground@
drawsBackground :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO Bool
drawsBackground nsTextFieldCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextFieldCell (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> Bool -> IO ()
setDrawsBackground nsTextFieldCell  value =
  sendMsg nsTextFieldCell (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- textColor@
textColor :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSColor)
textColor nsTextFieldCell  =
  sendMsg nsTextFieldCell (mkSelector "textColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextColor:@
setTextColor :: (IsNSTextFieldCell nsTextFieldCell, IsNSColor value) => nsTextFieldCell -> value -> IO ()
setTextColor nsTextFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextFieldCell (mkSelector "setTextColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bezelStyle@
bezelStyle :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO NSTextFieldBezelStyle
bezelStyle nsTextFieldCell  =
  fmap (coerce :: CULong -> NSTextFieldBezelStyle) $ sendMsg nsTextFieldCell (mkSelector "bezelStyle") retCULong []

-- | @- setBezelStyle:@
setBezelStyle :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> NSTextFieldBezelStyle -> IO ()
setBezelStyle nsTextFieldCell  value =
  sendMsg nsTextFieldCell (mkSelector "setBezelStyle:") retVoid [argCULong (coerce value)]

-- | @- placeholderString@
placeholderString :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSString)
placeholderString nsTextFieldCell  =
  sendMsg nsTextFieldCell (mkSelector "placeholderString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSTextFieldCell nsTextFieldCell, IsNSString value) => nsTextFieldCell -> value -> IO ()
setPlaceholderString nsTextFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextFieldCell (mkSelector "setPlaceholderString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSAttributedString)
placeholderAttributedString nsTextFieldCell  =
  sendMsg nsTextFieldCell (mkSelector "placeholderAttributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSTextFieldCell nsTextFieldCell, IsNSAttributedString value) => nsTextFieldCell -> value -> IO ()
setPlaceholderAttributedString nsTextFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextFieldCell (mkSelector "setPlaceholderAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @setUpFieldEditorAttributes:@
setUpFieldEditorAttributesSelector :: Selector
setUpFieldEditorAttributesSelector = mkSelector "setUpFieldEditorAttributes:"

-- | @Selector@ for @setWantsNotificationForMarkedText:@
setWantsNotificationForMarkedTextSelector :: Selector
setWantsNotificationForMarkedTextSelector = mkSelector "setWantsNotificationForMarkedText:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @textColor@
textColorSelector :: Selector
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @setTextColor:@
setTextColorSelector :: Selector
setTextColorSelector = mkSelector "setTextColor:"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector
setBezelStyleSelector = mkSelector "setBezelStyle:"

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

