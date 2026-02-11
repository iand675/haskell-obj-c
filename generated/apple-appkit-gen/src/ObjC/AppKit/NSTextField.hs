{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextField@.
module ObjC.AppKit.NSTextField
  ( NSTextField
  , IsNSTextField(..)
  , selectText
  , textShouldBeginEditing
  , textShouldEndEditing
  , textDidBeginEditing
  , textDidEndEditing
  , textDidChange
  , setTitleWithMnemonic
  , labelWithString
  , wrappingLabelWithString
  , labelWithAttributedString
  , textFieldWithString
  , placeholderString
  , setPlaceholderString
  , placeholderAttributedString
  , setPlaceholderAttributedString
  , backgroundColor
  , setBackgroundColor
  , drawsBackground
  , setDrawsBackground
  , textColor
  , setTextColor
  , bordered
  , setBordered
  , bezeled
  , setBezeled
  , editable
  , setEditable
  , selectable
  , setSelectable
  , delegate
  , setDelegate
  , acceptsFirstResponder
  , bezelStyle
  , setBezelStyle
  , preferredMaxLayoutWidth
  , setPreferredMaxLayoutWidth
  , maximumNumberOfLines
  , setMaximumNumberOfLines
  , allowsDefaultTighteningForTruncation
  , setAllowsDefaultTighteningForTruncation
  , lineBreakStrategy
  , setLineBreakStrategy
  , allowsWritingTools
  , setAllowsWritingTools
  , allowsWritingToolsAffordance
  , setAllowsWritingToolsAffordance
  , placeholderStrings
  , setPlaceholderStrings
  , placeholderAttributedStrings
  , setPlaceholderAttributedStrings
  , resolvesNaturalAlignmentWithBaseWritingDirection
  , setResolvesNaturalAlignmentWithBaseWritingDirection
  , allowsEditingTextAttributes
  , setAllowsEditingTextAttributes
  , importsGraphics
  , setImportsGraphics
  , automaticTextCompletionEnabled
  , setAutomaticTextCompletionEnabled
  , allowsCharacterPickerTouchBarItem
  , setAllowsCharacterPickerTouchBarItem
  , selectTextSelector
  , textShouldBeginEditingSelector
  , textShouldEndEditingSelector
  , textDidBeginEditingSelector
  , textDidEndEditingSelector
  , textDidChangeSelector
  , setTitleWithMnemonicSelector
  , labelWithStringSelector
  , wrappingLabelWithStringSelector
  , labelWithAttributedStringSelector
  , textFieldWithStringSelector
  , placeholderStringSelector
  , setPlaceholderStringSelector
  , placeholderAttributedStringSelector
  , setPlaceholderAttributedStringSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , textColorSelector
  , setTextColorSelector
  , borderedSelector
  , setBorderedSelector
  , bezeledSelector
  , setBezeledSelector
  , editableSelector
  , setEditableSelector
  , selectableSelector
  , setSelectableSelector
  , delegateSelector
  , setDelegateSelector
  , acceptsFirstResponderSelector
  , bezelStyleSelector
  , setBezelStyleSelector
  , preferredMaxLayoutWidthSelector
  , setPreferredMaxLayoutWidthSelector
  , maximumNumberOfLinesSelector
  , setMaximumNumberOfLinesSelector
  , allowsDefaultTighteningForTruncationSelector
  , setAllowsDefaultTighteningForTruncationSelector
  , lineBreakStrategySelector
  , setLineBreakStrategySelector
  , allowsWritingToolsSelector
  , setAllowsWritingToolsSelector
  , allowsWritingToolsAffordanceSelector
  , setAllowsWritingToolsAffordanceSelector
  , placeholderStringsSelector
  , setPlaceholderStringsSelector
  , placeholderAttributedStringsSelector
  , setPlaceholderAttributedStringsSelector
  , resolvesNaturalAlignmentWithBaseWritingDirectionSelector
  , setResolvesNaturalAlignmentWithBaseWritingDirectionSelector
  , allowsEditingTextAttributesSelector
  , setAllowsEditingTextAttributesSelector
  , importsGraphicsSelector
  , setImportsGraphicsSelector
  , automaticTextCompletionEnabledSelector
  , setAutomaticTextCompletionEnabledSelector
  , allowsCharacterPickerTouchBarItemSelector
  , setAllowsCharacterPickerTouchBarItemSelector

  -- * Enum types
  , NSLineBreakStrategy(NSLineBreakStrategy)
  , pattern NSLineBreakStrategyNone
  , pattern NSLineBreakStrategyPushOut
  , pattern NSLineBreakStrategyHangulWordPriority
  , pattern NSLineBreakStrategyStandard
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

-- | @- selectText:@
selectText :: IsNSTextField nsTextField => nsTextField -> RawId -> IO ()
selectText nsTextField  sender =
    sendMsg nsTextField (mkSelector "selectText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- textShouldBeginEditing:@
textShouldBeginEditing :: (IsNSTextField nsTextField, IsNSText textObject) => nsTextField -> textObject -> IO Bool
textShouldBeginEditing nsTextField  textObject =
  withObjCPtr textObject $ \raw_textObject ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "textShouldBeginEditing:") retCULong [argPtr (castPtr raw_textObject :: Ptr ())]

-- | @- textShouldEndEditing:@
textShouldEndEditing :: (IsNSTextField nsTextField, IsNSText textObject) => nsTextField -> textObject -> IO Bool
textShouldEndEditing nsTextField  textObject =
  withObjCPtr textObject $ \raw_textObject ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "textShouldEndEditing:") retCULong [argPtr (castPtr raw_textObject :: Ptr ())]

-- | @- textDidBeginEditing:@
textDidBeginEditing :: (IsNSTextField nsTextField, IsNSNotification notification) => nsTextField -> notification -> IO ()
textDidBeginEditing nsTextField  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsTextField (mkSelector "textDidBeginEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textDidEndEditing:@
textDidEndEditing :: (IsNSTextField nsTextField, IsNSNotification notification) => nsTextField -> notification -> IO ()
textDidEndEditing nsTextField  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsTextField (mkSelector "textDidEndEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textDidChange:@
textDidChange :: (IsNSTextField nsTextField, IsNSNotification notification) => nsTextField -> notification -> IO ()
textDidChange nsTextField  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsTextField (mkSelector "textDidChange:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSTextField nsTextField, IsNSString stringWithAmpersand) => nsTextField -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsTextField  stringWithAmpersand =
  withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
      sendMsg nsTextField (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | Creates a non-wrapping, non-editable, non-selectable text field that displays text in the default system font.
--
-- @stringValue@ — The title text to display in the field.
--
-- Returns: An initialized text field object.
--
-- ObjC selector: @+ labelWithString:@
labelWithString :: IsNSString stringValue => stringValue -> IO (Id NSTextField)
labelWithString stringValue =
  do
    cls' <- getRequiredClass "NSTextField"
    withObjCPtr stringValue $ \raw_stringValue ->
      sendClassMsg cls' (mkSelector "labelWithString:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a wrapping, non-editable, selectable text field that displays text in the default system font.
--
-- @stringValue@ — The title text to display in the field.
--
-- Returns: An initialized text field object.
--
-- ObjC selector: @+ wrappingLabelWithString:@
wrappingLabelWithString :: IsNSString stringValue => stringValue -> IO (Id NSTextField)
wrappingLabelWithString stringValue =
  do
    cls' <- getRequiredClass "NSTextField"
    withObjCPtr stringValue $ \raw_stringValue ->
      sendClassMsg cls' (mkSelector "wrappingLabelWithString:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a non-editable, non-selectable text field that displays attributed text. The line break mode of this field is determined by the attributed string's NSParagraphStyle attribute.
--
-- @attributedStringValue@ — The attributed string to display in the field.
--
-- Returns: An initialized text field object.
--
-- ObjC selector: @+ labelWithAttributedString:@
labelWithAttributedString :: IsNSAttributedString attributedStringValue => attributedStringValue -> IO (Id NSTextField)
labelWithAttributedString attributedStringValue =
  do
    cls' <- getRequiredClass "NSTextField"
    withObjCPtr attributedStringValue $ \raw_attributedStringValue ->
      sendClassMsg cls' (mkSelector "labelWithAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_attributedStringValue :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a non-wrapping editable text field.
--
-- @stringValue@ — The initial contents of the text field, or empty string for an initially empty text field.
--
-- Returns: An initialized text field object.
--
-- ObjC selector: @+ textFieldWithString:@
textFieldWithString :: IsNSString stringValue => stringValue -> IO (Id NSTextField)
textFieldWithString stringValue =
  do
    cls' <- getRequiredClass "NSTextField"
    withObjCPtr stringValue $ \raw_stringValue ->
      sendClassMsg cls' (mkSelector "textFieldWithString:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())] >>= retainedObject . castPtr

-- | @- placeholderString@
placeholderString :: IsNSTextField nsTextField => nsTextField -> IO (Id NSString)
placeholderString nsTextField  =
    sendMsg nsTextField (mkSelector "placeholderString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSTextField nsTextField, IsNSString value) => nsTextField -> value -> IO ()
setPlaceholderString nsTextField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextField (mkSelector "setPlaceholderString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSTextField nsTextField => nsTextField -> IO (Id NSAttributedString)
placeholderAttributedString nsTextField  =
    sendMsg nsTextField (mkSelector "placeholderAttributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSTextField nsTextField, IsNSAttributedString value) => nsTextField -> value -> IO ()
setPlaceholderAttributedString nsTextField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextField (mkSelector "setPlaceholderAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsNSTextField nsTextField => nsTextField -> IO (Id NSColor)
backgroundColor nsTextField  =
    sendMsg nsTextField (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextField nsTextField, IsNSColor value) => nsTextField -> value -> IO ()
setBackgroundColor nsTextField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextField (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawsBackground@
drawsBackground :: IsNSTextField nsTextField => nsTextField -> IO Bool
drawsBackground nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setDrawsBackground nsTextField  value =
    sendMsg nsTextField (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- textColor@
textColor :: IsNSTextField nsTextField => nsTextField -> IO (Id NSColor)
textColor nsTextField  =
    sendMsg nsTextField (mkSelector "textColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextColor:@
setTextColor :: (IsNSTextField nsTextField, IsNSColor value) => nsTextField -> value -> IO ()
setTextColor nsTextField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextField (mkSelector "setTextColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bordered@
bordered :: IsNSTextField nsTextField => nsTextField -> IO Bool
bordered nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "bordered") retCULong []

-- | @- setBordered:@
setBordered :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setBordered nsTextField  value =
    sendMsg nsTextField (mkSelector "setBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- bezeled@
bezeled :: IsNSTextField nsTextField => nsTextField -> IO Bool
bezeled nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "bezeled") retCULong []

-- | @- setBezeled:@
setBezeled :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setBezeled nsTextField  value =
    sendMsg nsTextField (mkSelector "setBezeled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- editable@
editable :: IsNSTextField nsTextField => nsTextField -> IO Bool
editable nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setEditable nsTextField  value =
    sendMsg nsTextField (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectable@
selectable :: IsNSTextField nsTextField => nsTextField -> IO Bool
selectable nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "selectable") retCULong []

-- | @- setSelectable:@
setSelectable :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setSelectable nsTextField  value =
    sendMsg nsTextField (mkSelector "setSelectable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delegate@
delegate :: IsNSTextField nsTextField => nsTextField -> IO RawId
delegate nsTextField  =
    fmap (RawId . castPtr) $ sendMsg nsTextField (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSTextField nsTextField => nsTextField -> RawId -> IO ()
setDelegate nsTextField  value =
    sendMsg nsTextField (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- acceptsFirstResponder@
acceptsFirstResponder :: IsNSTextField nsTextField => nsTextField -> IO Bool
acceptsFirstResponder nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "acceptsFirstResponder") retCULong []

-- | @- bezelStyle@
bezelStyle :: IsNSTextField nsTextField => nsTextField -> IO NSTextFieldBezelStyle
bezelStyle nsTextField  =
    fmap (coerce :: CULong -> NSTextFieldBezelStyle) $ sendMsg nsTextField (mkSelector "bezelStyle") retCULong []

-- | @- setBezelStyle:@
setBezelStyle :: IsNSTextField nsTextField => nsTextField -> NSTextFieldBezelStyle -> IO ()
setBezelStyle nsTextField  value =
    sendMsg nsTextField (mkSelector "setBezelStyle:") retVoid [argCULong (coerce value)]

-- | @- preferredMaxLayoutWidth@
preferredMaxLayoutWidth :: IsNSTextField nsTextField => nsTextField -> IO CDouble
preferredMaxLayoutWidth nsTextField  =
    sendMsg nsTextField (mkSelector "preferredMaxLayoutWidth") retCDouble []

-- | @- setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidth :: IsNSTextField nsTextField => nsTextField -> CDouble -> IO ()
setPreferredMaxLayoutWidth nsTextField  value =
    sendMsg nsTextField (mkSelector "setPreferredMaxLayoutWidth:") retVoid [argCDouble value]

-- | @- maximumNumberOfLines@
maximumNumberOfLines :: IsNSTextField nsTextField => nsTextField -> IO CLong
maximumNumberOfLines nsTextField  =
    sendMsg nsTextField (mkSelector "maximumNumberOfLines") retCLong []

-- | @- setMaximumNumberOfLines:@
setMaximumNumberOfLines :: IsNSTextField nsTextField => nsTextField -> CLong -> IO ()
setMaximumNumberOfLines nsTextField  value =
    sendMsg nsTextField (mkSelector "setMaximumNumberOfLines:") retVoid [argCLong value]

-- | @- allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncation :: IsNSTextField nsTextField => nsTextField -> IO Bool
allowsDefaultTighteningForTruncation nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "allowsDefaultTighteningForTruncation") retCULong []

-- | @- setAllowsDefaultTighteningForTruncation:@
setAllowsDefaultTighteningForTruncation :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setAllowsDefaultTighteningForTruncation nsTextField  value =
    sendMsg nsTextField (mkSelector "setAllowsDefaultTighteningForTruncation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- lineBreakStrategy@
lineBreakStrategy :: IsNSTextField nsTextField => nsTextField -> IO NSLineBreakStrategy
lineBreakStrategy nsTextField  =
    fmap (coerce :: CULong -> NSLineBreakStrategy) $ sendMsg nsTextField (mkSelector "lineBreakStrategy") retCULong []

-- | @- setLineBreakStrategy:@
setLineBreakStrategy :: IsNSTextField nsTextField => nsTextField -> NSLineBreakStrategy -> IO ()
setLineBreakStrategy nsTextField  value =
    sendMsg nsTextField (mkSelector "setLineBreakStrategy:") retVoid [argCULong (coerce value)]

-- | @- allowsWritingTools@
allowsWritingTools :: IsNSTextField nsTextField => nsTextField -> IO Bool
allowsWritingTools nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "allowsWritingTools") retCULong []

-- | @- setAllowsWritingTools:@
setAllowsWritingTools :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setAllowsWritingTools nsTextField  value =
    sendMsg nsTextField (mkSelector "setAllowsWritingTools:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsWritingToolsAffordance@
allowsWritingToolsAffordance :: IsNSTextField nsTextField => nsTextField -> IO Bool
allowsWritingToolsAffordance nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "allowsWritingToolsAffordance") retCULong []

-- | @- setAllowsWritingToolsAffordance:@
setAllowsWritingToolsAffordance :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setAllowsWritingToolsAffordance nsTextField  value =
    sendMsg nsTextField (mkSelector "setAllowsWritingToolsAffordance:") retVoid [argCULong (if value then 1 else 0)]

-- | @- placeholderStrings@
placeholderStrings :: IsNSTextField nsTextField => nsTextField -> IO (Id NSArray)
placeholderStrings nsTextField  =
    sendMsg nsTextField (mkSelector "placeholderStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderStrings:@
setPlaceholderStrings :: (IsNSTextField nsTextField, IsNSArray value) => nsTextField -> value -> IO ()
setPlaceholderStrings nsTextField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextField (mkSelector "setPlaceholderStrings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- placeholderAttributedStrings@
placeholderAttributedStrings :: IsNSTextField nsTextField => nsTextField -> IO (Id NSArray)
placeholderAttributedStrings nsTextField  =
    sendMsg nsTextField (mkSelector "placeholderAttributedStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderAttributedStrings:@
setPlaceholderAttributedStrings :: (IsNSTextField nsTextField, IsNSArray value) => nsTextField -> value -> IO ()
setPlaceholderAttributedStrings nsTextField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextField (mkSelector "setPlaceholderAttributedStrings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies the behavior for resolving ``NSTextAlignment.natural`` to the visual alignment.
--
-- When set to ``true``, the resolved visual alignment is determined by the resolved base writing direction; otherwise, it is using the user’s preferred language. The default value is ``false``.
--
-- ObjC selector: @- resolvesNaturalAlignmentWithBaseWritingDirection@
resolvesNaturalAlignmentWithBaseWritingDirection :: IsNSTextField nsTextField => nsTextField -> IO Bool
resolvesNaturalAlignmentWithBaseWritingDirection nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "resolvesNaturalAlignmentWithBaseWritingDirection") retCULong []

-- | Specifies the behavior for resolving ``NSTextAlignment.natural`` to the visual alignment.
--
-- When set to ``true``, the resolved visual alignment is determined by the resolved base writing direction; otherwise, it is using the user’s preferred language. The default value is ``false``.
--
-- ObjC selector: @- setResolvesNaturalAlignmentWithBaseWritingDirection:@
setResolvesNaturalAlignmentWithBaseWritingDirection :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setResolvesNaturalAlignmentWithBaseWritingDirection nsTextField  value =
    sendMsg nsTextField (mkSelector "setResolvesNaturalAlignmentWithBaseWritingDirection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsEditingTextAttributes@
allowsEditingTextAttributes :: IsNSTextField nsTextField => nsTextField -> IO Bool
allowsEditingTextAttributes nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "allowsEditingTextAttributes") retCULong []

-- | @- setAllowsEditingTextAttributes:@
setAllowsEditingTextAttributes :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setAllowsEditingTextAttributes nsTextField  value =
    sendMsg nsTextField (mkSelector "setAllowsEditingTextAttributes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- importsGraphics@
importsGraphics :: IsNSTextField nsTextField => nsTextField -> IO Bool
importsGraphics nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "importsGraphics") retCULong []

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setImportsGraphics nsTextField  value =
    sendMsg nsTextField (mkSelector "setImportsGraphics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticTextCompletionEnabled@
automaticTextCompletionEnabled :: IsNSTextField nsTextField => nsTextField -> IO Bool
automaticTextCompletionEnabled nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "automaticTextCompletionEnabled") retCULong []

-- | @- setAutomaticTextCompletionEnabled:@
setAutomaticTextCompletionEnabled :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setAutomaticTextCompletionEnabled nsTextField  value =
    sendMsg nsTextField (mkSelector "setAutomaticTextCompletionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsCharacterPickerTouchBarItem@
allowsCharacterPickerTouchBarItem :: IsNSTextField nsTextField => nsTextField -> IO Bool
allowsCharacterPickerTouchBarItem nsTextField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextField (mkSelector "allowsCharacterPickerTouchBarItem") retCULong []

-- | @- setAllowsCharacterPickerTouchBarItem:@
setAllowsCharacterPickerTouchBarItem :: IsNSTextField nsTextField => nsTextField -> Bool -> IO ()
setAllowsCharacterPickerTouchBarItem nsTextField  value =
    sendMsg nsTextField (mkSelector "setAllowsCharacterPickerTouchBarItem:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectText:@
selectTextSelector :: Selector
selectTextSelector = mkSelector "selectText:"

-- | @Selector@ for @textShouldBeginEditing:@
textShouldBeginEditingSelector :: Selector
textShouldBeginEditingSelector = mkSelector "textShouldBeginEditing:"

-- | @Selector@ for @textShouldEndEditing:@
textShouldEndEditingSelector :: Selector
textShouldEndEditingSelector = mkSelector "textShouldEndEditing:"

-- | @Selector@ for @textDidBeginEditing:@
textDidBeginEditingSelector :: Selector
textDidBeginEditingSelector = mkSelector "textDidBeginEditing:"

-- | @Selector@ for @textDidEndEditing:@
textDidEndEditingSelector :: Selector
textDidEndEditingSelector = mkSelector "textDidEndEditing:"

-- | @Selector@ for @textDidChange:@
textDidChangeSelector :: Selector
textDidChangeSelector = mkSelector "textDidChange:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @labelWithString:@
labelWithStringSelector :: Selector
labelWithStringSelector = mkSelector "labelWithString:"

-- | @Selector@ for @wrappingLabelWithString:@
wrappingLabelWithStringSelector :: Selector
wrappingLabelWithStringSelector = mkSelector "wrappingLabelWithString:"

-- | @Selector@ for @labelWithAttributedString:@
labelWithAttributedStringSelector :: Selector
labelWithAttributedStringSelector = mkSelector "labelWithAttributedString:"

-- | @Selector@ for @textFieldWithString:@
textFieldWithStringSelector :: Selector
textFieldWithStringSelector = mkSelector "textFieldWithString:"

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

-- | @Selector@ for @bordered@
borderedSelector :: Selector
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @acceptsFirstResponder@
acceptsFirstResponderSelector :: Selector
acceptsFirstResponderSelector = mkSelector "acceptsFirstResponder"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector
setBezelStyleSelector = mkSelector "setBezelStyle:"

-- | @Selector@ for @preferredMaxLayoutWidth@
preferredMaxLayoutWidthSelector :: Selector
preferredMaxLayoutWidthSelector = mkSelector "preferredMaxLayoutWidth"

-- | @Selector@ for @setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidthSelector :: Selector
setPreferredMaxLayoutWidthSelector = mkSelector "setPreferredMaxLayoutWidth:"

-- | @Selector@ for @maximumNumberOfLines@
maximumNumberOfLinesSelector :: Selector
maximumNumberOfLinesSelector = mkSelector "maximumNumberOfLines"

-- | @Selector@ for @setMaximumNumberOfLines:@
setMaximumNumberOfLinesSelector :: Selector
setMaximumNumberOfLinesSelector = mkSelector "setMaximumNumberOfLines:"

-- | @Selector@ for @allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncationSelector :: Selector
allowsDefaultTighteningForTruncationSelector = mkSelector "allowsDefaultTighteningForTruncation"

-- | @Selector@ for @setAllowsDefaultTighteningForTruncation:@
setAllowsDefaultTighteningForTruncationSelector :: Selector
setAllowsDefaultTighteningForTruncationSelector = mkSelector "setAllowsDefaultTighteningForTruncation:"

-- | @Selector@ for @lineBreakStrategy@
lineBreakStrategySelector :: Selector
lineBreakStrategySelector = mkSelector "lineBreakStrategy"

-- | @Selector@ for @setLineBreakStrategy:@
setLineBreakStrategySelector :: Selector
setLineBreakStrategySelector = mkSelector "setLineBreakStrategy:"

-- | @Selector@ for @allowsWritingTools@
allowsWritingToolsSelector :: Selector
allowsWritingToolsSelector = mkSelector "allowsWritingTools"

-- | @Selector@ for @setAllowsWritingTools:@
setAllowsWritingToolsSelector :: Selector
setAllowsWritingToolsSelector = mkSelector "setAllowsWritingTools:"

-- | @Selector@ for @allowsWritingToolsAffordance@
allowsWritingToolsAffordanceSelector :: Selector
allowsWritingToolsAffordanceSelector = mkSelector "allowsWritingToolsAffordance"

-- | @Selector@ for @setAllowsWritingToolsAffordance:@
setAllowsWritingToolsAffordanceSelector :: Selector
setAllowsWritingToolsAffordanceSelector = mkSelector "setAllowsWritingToolsAffordance:"

-- | @Selector@ for @placeholderStrings@
placeholderStringsSelector :: Selector
placeholderStringsSelector = mkSelector "placeholderStrings"

-- | @Selector@ for @setPlaceholderStrings:@
setPlaceholderStringsSelector :: Selector
setPlaceholderStringsSelector = mkSelector "setPlaceholderStrings:"

-- | @Selector@ for @placeholderAttributedStrings@
placeholderAttributedStringsSelector :: Selector
placeholderAttributedStringsSelector = mkSelector "placeholderAttributedStrings"

-- | @Selector@ for @setPlaceholderAttributedStrings:@
setPlaceholderAttributedStringsSelector :: Selector
setPlaceholderAttributedStringsSelector = mkSelector "setPlaceholderAttributedStrings:"

-- | @Selector@ for @resolvesNaturalAlignmentWithBaseWritingDirection@
resolvesNaturalAlignmentWithBaseWritingDirectionSelector :: Selector
resolvesNaturalAlignmentWithBaseWritingDirectionSelector = mkSelector "resolvesNaturalAlignmentWithBaseWritingDirection"

-- | @Selector@ for @setResolvesNaturalAlignmentWithBaseWritingDirection:@
setResolvesNaturalAlignmentWithBaseWritingDirectionSelector :: Selector
setResolvesNaturalAlignmentWithBaseWritingDirectionSelector = mkSelector "setResolvesNaturalAlignmentWithBaseWritingDirection:"

-- | @Selector@ for @allowsEditingTextAttributes@
allowsEditingTextAttributesSelector :: Selector
allowsEditingTextAttributesSelector = mkSelector "allowsEditingTextAttributes"

-- | @Selector@ for @setAllowsEditingTextAttributes:@
setAllowsEditingTextAttributesSelector :: Selector
setAllowsEditingTextAttributesSelector = mkSelector "setAllowsEditingTextAttributes:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @automaticTextCompletionEnabled@
automaticTextCompletionEnabledSelector :: Selector
automaticTextCompletionEnabledSelector = mkSelector "automaticTextCompletionEnabled"

-- | @Selector@ for @setAutomaticTextCompletionEnabled:@
setAutomaticTextCompletionEnabledSelector :: Selector
setAutomaticTextCompletionEnabledSelector = mkSelector "setAutomaticTextCompletionEnabled:"

-- | @Selector@ for @allowsCharacterPickerTouchBarItem@
allowsCharacterPickerTouchBarItemSelector :: Selector
allowsCharacterPickerTouchBarItemSelector = mkSelector "allowsCharacterPickerTouchBarItem"

-- | @Selector@ for @setAllowsCharacterPickerTouchBarItem:@
setAllowsCharacterPickerTouchBarItemSelector :: Selector
setAllowsCharacterPickerTouchBarItemSelector = mkSelector "setAllowsCharacterPickerTouchBarItem:"

