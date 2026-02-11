{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSText@.
module ObjC.AppKit.NSText
  ( NSText
  , IsNSText(..)
  , initWithFrame
  , initWithCoder
  , replaceCharactersInRange_withString
  , replaceCharactersInRange_withRTF
  , replaceCharactersInRange_withRTFD
  , rtfFromRange
  , rtfdFromRange
  , writeRTFDToFile_atomically
  , readRTFDFromFile
  , scrollRangeToVisible
  , setTextColor_range
  , setFont_range
  , sizeToFit
  , copy
  , copyFont
  , copyRuler
  , cut
  , delete
  , paste
  , pasteFont
  , pasteRuler
  , selectAll
  , changeFont
  , alignLeft
  , alignRight
  , alignCenter
  , subscript
  , superscript
  , underline
  , unscript
  , showGuessPanel
  , checkSpelling
  , toggleRuler
  , string
  , setString
  , delegate
  , setDelegate
  , editable
  , setEditable
  , selectable
  , setSelectable
  , richText
  , setRichText
  , importsGraphics
  , setImportsGraphics
  , fieldEditor
  , setFieldEditor
  , usesFontPanel
  , setUsesFontPanel
  , drawsBackground
  , setDrawsBackground
  , backgroundColor
  , setBackgroundColor
  , rulerVisible
  , selectedRange
  , setSelectedRange
  , font
  , setFont
  , textColor
  , setTextColor
  , alignment
  , setAlignment
  , baseWritingDirection
  , setBaseWritingDirection
  , maxSize
  , setMaxSize
  , minSize
  , setMinSize
  , horizontallyResizable
  , setHorizontallyResizable
  , verticallyResizable
  , setVerticallyResizable
  , initWithFrameSelector
  , initWithCoderSelector
  , replaceCharactersInRange_withStringSelector
  , replaceCharactersInRange_withRTFSelector
  , replaceCharactersInRange_withRTFDSelector
  , rtfFromRangeSelector
  , rtfdFromRangeSelector
  , writeRTFDToFile_atomicallySelector
  , readRTFDFromFileSelector
  , scrollRangeToVisibleSelector
  , setTextColor_rangeSelector
  , setFont_rangeSelector
  , sizeToFitSelector
  , copySelector
  , copyFontSelector
  , copyRulerSelector
  , cutSelector
  , deleteSelector
  , pasteSelector
  , pasteFontSelector
  , pasteRulerSelector
  , selectAllSelector
  , changeFontSelector
  , alignLeftSelector
  , alignRightSelector
  , alignCenterSelector
  , subscriptSelector
  , superscriptSelector
  , underlineSelector
  , unscriptSelector
  , showGuessPanelSelector
  , checkSpellingSelector
  , toggleRulerSelector
  , stringSelector
  , setStringSelector
  , delegateSelector
  , setDelegateSelector
  , editableSelector
  , setEditableSelector
  , selectableSelector
  , setSelectableSelector
  , richTextSelector
  , setRichTextSelector
  , importsGraphicsSelector
  , setImportsGraphicsSelector
  , fieldEditorSelector
  , setFieldEditorSelector
  , usesFontPanelSelector
  , setUsesFontPanelSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , rulerVisibleSelector
  , selectedRangeSelector
  , setSelectedRangeSelector
  , fontSelector
  , setFontSelector
  , textColorSelector
  , setTextColorSelector
  , alignmentSelector
  , setAlignmentSelector
  , baseWritingDirectionSelector
  , setBaseWritingDirectionSelector
  , maxSizeSelector
  , setMaxSizeSelector
  , minSizeSelector
  , setMinSizeSelector
  , horizontallyResizableSelector
  , setHorizontallyResizableSelector
  , verticallyResizableSelector
  , setVerticallyResizableSelector

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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsNSText nsText => nsText -> NSRect -> IO (Id NSText)
initWithFrame nsText  frameRect =
    sendMsg nsText (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSText nsText, IsNSCoder coder) => nsText -> coder -> IO (Id NSText)
initWithCoder nsText  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsText (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- replaceCharactersInRange:withString:@
replaceCharactersInRange_withString :: (IsNSText nsText, IsNSString string) => nsText -> NSRange -> string -> IO ()
replaceCharactersInRange_withString nsText  range string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsText (mkSelector "replaceCharactersInRange:withString:") retVoid [argNSRange range, argPtr (castPtr raw_string :: Ptr ())]

-- | @- replaceCharactersInRange:withRTF:@
replaceCharactersInRange_withRTF :: (IsNSText nsText, IsNSData rtfData) => nsText -> NSRange -> rtfData -> IO ()
replaceCharactersInRange_withRTF nsText  range rtfData =
  withObjCPtr rtfData $ \raw_rtfData ->
      sendMsg nsText (mkSelector "replaceCharactersInRange:withRTF:") retVoid [argNSRange range, argPtr (castPtr raw_rtfData :: Ptr ())]

-- | @- replaceCharactersInRange:withRTFD:@
replaceCharactersInRange_withRTFD :: (IsNSText nsText, IsNSData rtfdData) => nsText -> NSRange -> rtfdData -> IO ()
replaceCharactersInRange_withRTFD nsText  range rtfdData =
  withObjCPtr rtfdData $ \raw_rtfdData ->
      sendMsg nsText (mkSelector "replaceCharactersInRange:withRTFD:") retVoid [argNSRange range, argPtr (castPtr raw_rtfdData :: Ptr ())]

-- | @- RTFFromRange:@
rtfFromRange :: IsNSText nsText => nsText -> NSRange -> IO (Id NSData)
rtfFromRange nsText  range =
    sendMsg nsText (mkSelector "RTFFromRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- RTFDFromRange:@
rtfdFromRange :: IsNSText nsText => nsText -> NSRange -> IO (Id NSData)
rtfdFromRange nsText  range =
    sendMsg nsText (mkSelector "RTFDFromRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- writeRTFDToFile:atomically:@
writeRTFDToFile_atomically :: (IsNSText nsText, IsNSString path) => nsText -> path -> Bool -> IO Bool
writeRTFDToFile_atomically nsText  path flag =
  withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "writeRTFDToFile:atomically:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- readRTFDFromFile:@
readRTFDFromFile :: (IsNSText nsText, IsNSString path) => nsText -> path -> IO Bool
readRTFDFromFile nsText  path =
  withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "readRTFDFromFile:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- scrollRangeToVisible:@
scrollRangeToVisible :: IsNSText nsText => nsText -> NSRange -> IO ()
scrollRangeToVisible nsText  range =
    sendMsg nsText (mkSelector "scrollRangeToVisible:") retVoid [argNSRange range]

-- | @- setTextColor:range:@
setTextColor_range :: (IsNSText nsText, IsNSColor color) => nsText -> color -> NSRange -> IO ()
setTextColor_range nsText  color range =
  withObjCPtr color $ \raw_color ->
      sendMsg nsText (mkSelector "setTextColor:range:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argNSRange range]

-- | @- setFont:range:@
setFont_range :: (IsNSText nsText, IsNSFont font) => nsText -> font -> NSRange -> IO ()
setFont_range nsText  font range =
  withObjCPtr font $ \raw_font ->
      sendMsg nsText (mkSelector "setFont:range:") retVoid [argPtr (castPtr raw_font :: Ptr ()), argNSRange range]

-- | @- sizeToFit@
sizeToFit :: IsNSText nsText => nsText -> IO ()
sizeToFit nsText  =
    sendMsg nsText (mkSelector "sizeToFit") retVoid []

-- | @- copy:@
copy :: IsNSText nsText => nsText -> RawId -> IO ()
copy nsText  sender =
    sendMsg nsText (mkSelector "copy:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- copyFont:@
copyFont :: IsNSText nsText => nsText -> RawId -> IO ()
copyFont nsText  sender =
    sendMsg nsText (mkSelector "copyFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- copyRuler:@
copyRuler :: IsNSText nsText => nsText -> RawId -> IO ()
copyRuler nsText  sender =
    sendMsg nsText (mkSelector "copyRuler:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- cut:@
cut :: IsNSText nsText => nsText -> RawId -> IO ()
cut nsText  sender =
    sendMsg nsText (mkSelector "cut:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- delete:@
delete :: IsNSText nsText => nsText -> RawId -> IO ()
delete nsText  sender =
    sendMsg nsText (mkSelector "delete:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- paste:@
paste :: IsNSText nsText => nsText -> RawId -> IO ()
paste nsText  sender =
    sendMsg nsText (mkSelector "paste:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pasteFont:@
pasteFont :: IsNSText nsText => nsText -> RawId -> IO ()
pasteFont nsText  sender =
    sendMsg nsText (mkSelector "pasteFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pasteRuler:@
pasteRuler :: IsNSText nsText => nsText -> RawId -> IO ()
pasteRuler nsText  sender =
    sendMsg nsText (mkSelector "pasteRuler:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectAll:@
selectAll :: IsNSText nsText => nsText -> RawId -> IO ()
selectAll nsText  sender =
    sendMsg nsText (mkSelector "selectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeFont:@
changeFont :: IsNSText nsText => nsText -> RawId -> IO ()
changeFont nsText  sender =
    sendMsg nsText (mkSelector "changeFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignLeft:@
alignLeft :: IsNSText nsText => nsText -> RawId -> IO ()
alignLeft nsText  sender =
    sendMsg nsText (mkSelector "alignLeft:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignRight:@
alignRight :: IsNSText nsText => nsText -> RawId -> IO ()
alignRight nsText  sender =
    sendMsg nsText (mkSelector "alignRight:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignCenter:@
alignCenter :: IsNSText nsText => nsText -> RawId -> IO ()
alignCenter nsText  sender =
    sendMsg nsText (mkSelector "alignCenter:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- subscript:@
subscript :: IsNSText nsText => nsText -> RawId -> IO ()
subscript nsText  sender =
    sendMsg nsText (mkSelector "subscript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- superscript:@
superscript :: IsNSText nsText => nsText -> RawId -> IO ()
superscript nsText  sender =
    sendMsg nsText (mkSelector "superscript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- underline:@
underline :: IsNSText nsText => nsText -> RawId -> IO ()
underline nsText  sender =
    sendMsg nsText (mkSelector "underline:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- unscript:@
unscript :: IsNSText nsText => nsText -> RawId -> IO ()
unscript nsText  sender =
    sendMsg nsText (mkSelector "unscript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- showGuessPanel:@
showGuessPanel :: IsNSText nsText => nsText -> RawId -> IO ()
showGuessPanel nsText  sender =
    sendMsg nsText (mkSelector "showGuessPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkSpelling:@
checkSpelling :: IsNSText nsText => nsText -> RawId -> IO ()
checkSpelling nsText  sender =
    sendMsg nsText (mkSelector "checkSpelling:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleRuler:@
toggleRuler :: IsNSText nsText => nsText -> RawId -> IO ()
toggleRuler nsText  sender =
    sendMsg nsText (mkSelector "toggleRuler:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- string@
string :: IsNSText nsText => nsText -> IO (Id NSString)
string nsText  =
    sendMsg nsText (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setString:@
setString :: (IsNSText nsText, IsNSString value) => nsText -> value -> IO ()
setString nsText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsText (mkSelector "setString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSText nsText => nsText -> IO RawId
delegate nsText  =
    fmap (RawId . castPtr) $ sendMsg nsText (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSText nsText => nsText -> RawId -> IO ()
setDelegate nsText  value =
    sendMsg nsText (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- editable@
editable :: IsNSText nsText => nsText -> IO Bool
editable nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSText nsText => nsText -> Bool -> IO ()
setEditable nsText  value =
    sendMsg nsText (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectable@
selectable :: IsNSText nsText => nsText -> IO Bool
selectable nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "selectable") retCULong []

-- | @- setSelectable:@
setSelectable :: IsNSText nsText => nsText -> Bool -> IO ()
setSelectable nsText  value =
    sendMsg nsText (mkSelector "setSelectable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- richText@
richText :: IsNSText nsText => nsText -> IO Bool
richText nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "richText") retCULong []

-- | @- setRichText:@
setRichText :: IsNSText nsText => nsText -> Bool -> IO ()
setRichText nsText  value =
    sendMsg nsText (mkSelector "setRichText:") retVoid [argCULong (if value then 1 else 0)]

-- | @- importsGraphics@
importsGraphics :: IsNSText nsText => nsText -> IO Bool
importsGraphics nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "importsGraphics") retCULong []

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSText nsText => nsText -> Bool -> IO ()
setImportsGraphics nsText  value =
    sendMsg nsText (mkSelector "setImportsGraphics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- fieldEditor@
fieldEditor :: IsNSText nsText => nsText -> IO Bool
fieldEditor nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "fieldEditor") retCULong []

-- | @- setFieldEditor:@
setFieldEditor :: IsNSText nsText => nsText -> Bool -> IO ()
setFieldEditor nsText  value =
    sendMsg nsText (mkSelector "setFieldEditor:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesFontPanel@
usesFontPanel :: IsNSText nsText => nsText -> IO Bool
usesFontPanel nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "usesFontPanel") retCULong []

-- | @- setUsesFontPanel:@
setUsesFontPanel :: IsNSText nsText => nsText -> Bool -> IO ()
setUsesFontPanel nsText  value =
    sendMsg nsText (mkSelector "setUsesFontPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | @- drawsBackground@
drawsBackground :: IsNSText nsText => nsText -> IO Bool
drawsBackground nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSText nsText => nsText -> Bool -> IO ()
setDrawsBackground nsText  value =
    sendMsg nsText (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSText nsText => nsText -> IO (Id NSColor)
backgroundColor nsText  =
    sendMsg nsText (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSText nsText, IsNSColor value) => nsText -> value -> IO ()
setBackgroundColor nsText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsText (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rulerVisible@
rulerVisible :: IsNSText nsText => nsText -> IO Bool
rulerVisible nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "rulerVisible") retCULong []

-- | @- selectedRange@
selectedRange :: IsNSText nsText => nsText -> IO NSRange
selectedRange nsText  =
    sendMsgStret nsText (mkSelector "selectedRange") retNSRange []

-- | @- setSelectedRange:@
setSelectedRange :: IsNSText nsText => nsText -> NSRange -> IO ()
setSelectedRange nsText  value =
    sendMsg nsText (mkSelector "setSelectedRange:") retVoid [argNSRange value]

-- | @- font@
font :: IsNSText nsText => nsText -> IO (Id NSFont)
font nsText  =
    sendMsg nsText (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsNSText nsText, IsNSFont value) => nsText -> value -> IO ()
setFont nsText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsText (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textColor@
textColor :: IsNSText nsText => nsText -> IO (Id NSColor)
textColor nsText  =
    sendMsg nsText (mkSelector "textColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextColor:@
setTextColor :: (IsNSText nsText, IsNSColor value) => nsText -> value -> IO ()
setTextColor nsText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsText (mkSelector "setTextColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alignment@
alignment :: IsNSText nsText => nsText -> IO NSTextAlignment
alignment nsText  =
    fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsText (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsNSText nsText => nsText -> NSTextAlignment -> IO ()
setAlignment nsText  value =
    sendMsg nsText (mkSelector "setAlignment:") retVoid [argCLong (coerce value)]

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSText nsText => nsText -> IO NSWritingDirection
baseWritingDirection nsText  =
    fmap (coerce :: CLong -> NSWritingDirection) $ sendMsg nsText (mkSelector "baseWritingDirection") retCLong []

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSText nsText => nsText -> NSWritingDirection -> IO ()
setBaseWritingDirection nsText  value =
    sendMsg nsText (mkSelector "setBaseWritingDirection:") retVoid [argCLong (coerce value)]

-- | @- maxSize@
maxSize :: IsNSText nsText => nsText -> IO NSSize
maxSize nsText  =
    sendMsgStret nsText (mkSelector "maxSize") retNSSize []

-- | @- setMaxSize:@
setMaxSize :: IsNSText nsText => nsText -> NSSize -> IO ()
setMaxSize nsText  value =
    sendMsg nsText (mkSelector "setMaxSize:") retVoid [argNSSize value]

-- | @- minSize@
minSize :: IsNSText nsText => nsText -> IO NSSize
minSize nsText  =
    sendMsgStret nsText (mkSelector "minSize") retNSSize []

-- | @- setMinSize:@
setMinSize :: IsNSText nsText => nsText -> NSSize -> IO ()
setMinSize nsText  value =
    sendMsg nsText (mkSelector "setMinSize:") retVoid [argNSSize value]

-- | @- horizontallyResizable@
horizontallyResizable :: IsNSText nsText => nsText -> IO Bool
horizontallyResizable nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "horizontallyResizable") retCULong []

-- | @- setHorizontallyResizable:@
setHorizontallyResizable :: IsNSText nsText => nsText -> Bool -> IO ()
setHorizontallyResizable nsText  value =
    sendMsg nsText (mkSelector "setHorizontallyResizable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- verticallyResizable@
verticallyResizable :: IsNSText nsText => nsText -> IO Bool
verticallyResizable nsText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsText (mkSelector "verticallyResizable") retCULong []

-- | @- setVerticallyResizable:@
setVerticallyResizable :: IsNSText nsText => nsText -> Bool -> IO ()
setVerticallyResizable nsText  value =
    sendMsg nsText (mkSelector "setVerticallyResizable:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @replaceCharactersInRange:withString:@
replaceCharactersInRange_withStringSelector :: Selector
replaceCharactersInRange_withStringSelector = mkSelector "replaceCharactersInRange:withString:"

-- | @Selector@ for @replaceCharactersInRange:withRTF:@
replaceCharactersInRange_withRTFSelector :: Selector
replaceCharactersInRange_withRTFSelector = mkSelector "replaceCharactersInRange:withRTF:"

-- | @Selector@ for @replaceCharactersInRange:withRTFD:@
replaceCharactersInRange_withRTFDSelector :: Selector
replaceCharactersInRange_withRTFDSelector = mkSelector "replaceCharactersInRange:withRTFD:"

-- | @Selector@ for @RTFFromRange:@
rtfFromRangeSelector :: Selector
rtfFromRangeSelector = mkSelector "RTFFromRange:"

-- | @Selector@ for @RTFDFromRange:@
rtfdFromRangeSelector :: Selector
rtfdFromRangeSelector = mkSelector "RTFDFromRange:"

-- | @Selector@ for @writeRTFDToFile:atomically:@
writeRTFDToFile_atomicallySelector :: Selector
writeRTFDToFile_atomicallySelector = mkSelector "writeRTFDToFile:atomically:"

-- | @Selector@ for @readRTFDFromFile:@
readRTFDFromFileSelector :: Selector
readRTFDFromFileSelector = mkSelector "readRTFDFromFile:"

-- | @Selector@ for @scrollRangeToVisible:@
scrollRangeToVisibleSelector :: Selector
scrollRangeToVisibleSelector = mkSelector "scrollRangeToVisible:"

-- | @Selector@ for @setTextColor:range:@
setTextColor_rangeSelector :: Selector
setTextColor_rangeSelector = mkSelector "setTextColor:range:"

-- | @Selector@ for @setFont:range:@
setFont_rangeSelector :: Selector
setFont_rangeSelector = mkSelector "setFont:range:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @copy:@
copySelector :: Selector
copySelector = mkSelector "copy:"

-- | @Selector@ for @copyFont:@
copyFontSelector :: Selector
copyFontSelector = mkSelector "copyFont:"

-- | @Selector@ for @copyRuler:@
copyRulerSelector :: Selector
copyRulerSelector = mkSelector "copyRuler:"

-- | @Selector@ for @cut:@
cutSelector :: Selector
cutSelector = mkSelector "cut:"

-- | @Selector@ for @delete:@
deleteSelector :: Selector
deleteSelector = mkSelector "delete:"

-- | @Selector@ for @paste:@
pasteSelector :: Selector
pasteSelector = mkSelector "paste:"

-- | @Selector@ for @pasteFont:@
pasteFontSelector :: Selector
pasteFontSelector = mkSelector "pasteFont:"

-- | @Selector@ for @pasteRuler:@
pasteRulerSelector :: Selector
pasteRulerSelector = mkSelector "pasteRuler:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @changeFont:@
changeFontSelector :: Selector
changeFontSelector = mkSelector "changeFont:"

-- | @Selector@ for @alignLeft:@
alignLeftSelector :: Selector
alignLeftSelector = mkSelector "alignLeft:"

-- | @Selector@ for @alignRight:@
alignRightSelector :: Selector
alignRightSelector = mkSelector "alignRight:"

-- | @Selector@ for @alignCenter:@
alignCenterSelector :: Selector
alignCenterSelector = mkSelector "alignCenter:"

-- | @Selector@ for @subscript:@
subscriptSelector :: Selector
subscriptSelector = mkSelector "subscript:"

-- | @Selector@ for @superscript:@
superscriptSelector :: Selector
superscriptSelector = mkSelector "superscript:"

-- | @Selector@ for @underline:@
underlineSelector :: Selector
underlineSelector = mkSelector "underline:"

-- | @Selector@ for @unscript:@
unscriptSelector :: Selector
unscriptSelector = mkSelector "unscript:"

-- | @Selector@ for @showGuessPanel:@
showGuessPanelSelector :: Selector
showGuessPanelSelector = mkSelector "showGuessPanel:"

-- | @Selector@ for @checkSpelling:@
checkSpellingSelector :: Selector
checkSpellingSelector = mkSelector "checkSpelling:"

-- | @Selector@ for @toggleRuler:@
toggleRulerSelector :: Selector
toggleRulerSelector = mkSelector "toggleRuler:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

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

-- | @Selector@ for @richText@
richTextSelector :: Selector
richTextSelector = mkSelector "richText"

-- | @Selector@ for @setRichText:@
setRichTextSelector :: Selector
setRichTextSelector = mkSelector "setRichText:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @fieldEditor@
fieldEditorSelector :: Selector
fieldEditorSelector = mkSelector "fieldEditor"

-- | @Selector@ for @setFieldEditor:@
setFieldEditorSelector :: Selector
setFieldEditorSelector = mkSelector "setFieldEditor:"

-- | @Selector@ for @usesFontPanel@
usesFontPanelSelector :: Selector
usesFontPanelSelector = mkSelector "usesFontPanel"

-- | @Selector@ for @setUsesFontPanel:@
setUsesFontPanelSelector :: Selector
setUsesFontPanelSelector = mkSelector "setUsesFontPanel:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @rulerVisible@
rulerVisibleSelector :: Selector
rulerVisibleSelector = mkSelector "rulerVisible"

-- | @Selector@ for @selectedRange@
selectedRangeSelector :: Selector
selectedRangeSelector = mkSelector "selectedRange"

-- | @Selector@ for @setSelectedRange:@
setSelectedRangeSelector :: Selector
setSelectedRangeSelector = mkSelector "setSelectedRange:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @textColor@
textColorSelector :: Selector
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @setTextColor:@
setTextColorSelector :: Selector
setTextColorSelector = mkSelector "setTextColor:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @maxSize@
maxSizeSelector :: Selector
maxSizeSelector = mkSelector "maxSize"

-- | @Selector@ for @setMaxSize:@
setMaxSizeSelector :: Selector
setMaxSizeSelector = mkSelector "setMaxSize:"

-- | @Selector@ for @minSize@
minSizeSelector :: Selector
minSizeSelector = mkSelector "minSize"

-- | @Selector@ for @setMinSize:@
setMinSizeSelector :: Selector
setMinSizeSelector = mkSelector "setMinSize:"

-- | @Selector@ for @horizontallyResizable@
horizontallyResizableSelector :: Selector
horizontallyResizableSelector = mkSelector "horizontallyResizable"

-- | @Selector@ for @setHorizontallyResizable:@
setHorizontallyResizableSelector :: Selector
setHorizontallyResizableSelector = mkSelector "setHorizontallyResizable:"

-- | @Selector@ for @verticallyResizable@
verticallyResizableSelector :: Selector
verticallyResizableSelector = mkSelector "verticallyResizable"

-- | @Selector@ for @setVerticallyResizable:@
setVerticallyResizableSelector :: Selector
setVerticallyResizableSelector = mkSelector "setVerticallyResizable:"

