{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowedInputSourceLocales
  , setAllowedInputSourceLocales
  , allowedInputSourceLocalesSelector
  , backgroundColorSelector
  , bezelStyleSelector
  , drawsBackgroundSelector
  , initImageCellSelector
  , initTextCellSelector
  , initWithCoderSelector
  , placeholderAttributedStringSelector
  , placeholderStringSelector
  , setAllowedInputSourceLocalesSelector
  , setBackgroundColorSelector
  , setBezelStyleSelector
  , setDrawsBackgroundSelector
  , setPlaceholderAttributedStringSelector
  , setPlaceholderStringSelector
  , setTextColorSelector
  , setUpFieldEditorAttributesSelector
  , setWantsNotificationForMarkedTextSelector
  , textColorSelector

  -- * Enum types
  , NSTextFieldBezelStyle(NSTextFieldBezelStyle)
  , pattern NSTextFieldSquareBezel
  , pattern NSTextFieldRoundedBezel

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

-- | @- initTextCell:@
initTextCell :: (IsNSTextFieldCell nsTextFieldCell, IsNSString string) => nsTextFieldCell -> string -> IO (Id NSTextFieldCell)
initTextCell nsTextFieldCell string =
  sendOwnedMessage nsTextFieldCell initTextCellSelector (toNSString string)

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextFieldCell nsTextFieldCell, IsNSCoder coder) => nsTextFieldCell -> coder -> IO (Id NSTextFieldCell)
initWithCoder nsTextFieldCell coder =
  sendOwnedMessage nsTextFieldCell initWithCoderSelector (toNSCoder coder)

-- | @- initImageCell:@
initImageCell :: (IsNSTextFieldCell nsTextFieldCell, IsNSImage image) => nsTextFieldCell -> image -> IO (Id NSTextFieldCell)
initImageCell nsTextFieldCell image =
  sendOwnedMessage nsTextFieldCell initImageCellSelector (toNSImage image)

-- | @- setUpFieldEditorAttributes:@
setUpFieldEditorAttributes :: (IsNSTextFieldCell nsTextFieldCell, IsNSText textObj) => nsTextFieldCell -> textObj -> IO (Id NSText)
setUpFieldEditorAttributes nsTextFieldCell textObj =
  sendMessage nsTextFieldCell setUpFieldEditorAttributesSelector (toNSText textObj)

-- | @- setWantsNotificationForMarkedText:@
setWantsNotificationForMarkedText :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> Bool -> IO ()
setWantsNotificationForMarkedText nsTextFieldCell flag =
  sendMessage nsTextFieldCell setWantsNotificationForMarkedTextSelector flag

-- | @- backgroundColor@
backgroundColor :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSColor)
backgroundColor nsTextFieldCell =
  sendMessage nsTextFieldCell backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextFieldCell nsTextFieldCell, IsNSColor value) => nsTextFieldCell -> value -> IO ()
setBackgroundColor nsTextFieldCell value =
  sendMessage nsTextFieldCell setBackgroundColorSelector (toNSColor value)

-- | @- drawsBackground@
drawsBackground :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO Bool
drawsBackground nsTextFieldCell =
  sendMessage nsTextFieldCell drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> Bool -> IO ()
setDrawsBackground nsTextFieldCell value =
  sendMessage nsTextFieldCell setDrawsBackgroundSelector value

-- | @- textColor@
textColor :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSColor)
textColor nsTextFieldCell =
  sendMessage nsTextFieldCell textColorSelector

-- | @- setTextColor:@
setTextColor :: (IsNSTextFieldCell nsTextFieldCell, IsNSColor value) => nsTextFieldCell -> value -> IO ()
setTextColor nsTextFieldCell value =
  sendMessage nsTextFieldCell setTextColorSelector (toNSColor value)

-- | @- bezelStyle@
bezelStyle :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO NSTextFieldBezelStyle
bezelStyle nsTextFieldCell =
  sendMessage nsTextFieldCell bezelStyleSelector

-- | @- setBezelStyle:@
setBezelStyle :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> NSTextFieldBezelStyle -> IO ()
setBezelStyle nsTextFieldCell value =
  sendMessage nsTextFieldCell setBezelStyleSelector value

-- | @- placeholderString@
placeholderString :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSString)
placeholderString nsTextFieldCell =
  sendMessage nsTextFieldCell placeholderStringSelector

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSTextFieldCell nsTextFieldCell, IsNSString value) => nsTextFieldCell -> value -> IO ()
setPlaceholderString nsTextFieldCell value =
  sendMessage nsTextFieldCell setPlaceholderStringSelector (toNSString value)

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSAttributedString)
placeholderAttributedString nsTextFieldCell =
  sendMessage nsTextFieldCell placeholderAttributedStringSelector

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSTextFieldCell nsTextFieldCell, IsNSAttributedString value) => nsTextFieldCell -> value -> IO ()
setPlaceholderAttributedString nsTextFieldCell value =
  sendMessage nsTextFieldCell setPlaceholderAttributedStringSelector (toNSAttributedString value)

-- | @- allowedInputSourceLocales@
allowedInputSourceLocales :: IsNSTextFieldCell nsTextFieldCell => nsTextFieldCell -> IO (Id NSArray)
allowedInputSourceLocales nsTextFieldCell =
  sendMessage nsTextFieldCell allowedInputSourceLocalesSelector

-- | @- setAllowedInputSourceLocales:@
setAllowedInputSourceLocales :: (IsNSTextFieldCell nsTextFieldCell, IsNSArray value) => nsTextFieldCell -> value -> IO ()
setAllowedInputSourceLocales nsTextFieldCell value =
  sendMessage nsTextFieldCell setAllowedInputSourceLocalesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSTextFieldCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextFieldCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSTextFieldCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @setUpFieldEditorAttributes:@
setUpFieldEditorAttributesSelector :: Selector '[Id NSText] (Id NSText)
setUpFieldEditorAttributesSelector = mkSelector "setUpFieldEditorAttributes:"

-- | @Selector@ for @setWantsNotificationForMarkedText:@
setWantsNotificationForMarkedTextSelector :: Selector '[Bool] ()
setWantsNotificationForMarkedTextSelector = mkSelector "setWantsNotificationForMarkedText:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @textColor@
textColorSelector :: Selector '[] (Id NSColor)
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @setTextColor:@
setTextColorSelector :: Selector '[Id NSColor] ()
setTextColorSelector = mkSelector "setTextColor:"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector '[] NSTextFieldBezelStyle
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector '[NSTextFieldBezelStyle] ()
setBezelStyleSelector = mkSelector "setBezelStyle:"

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

-- | @Selector@ for @allowedInputSourceLocales@
allowedInputSourceLocalesSelector :: Selector '[] (Id NSArray)
allowedInputSourceLocalesSelector = mkSelector "allowedInputSourceLocales"

-- | @Selector@ for @setAllowedInputSourceLocales:@
setAllowedInputSourceLocalesSelector :: Selector '[Id NSArray] ()
setAllowedInputSourceLocalesSelector = mkSelector "setAllowedInputSourceLocales:"

