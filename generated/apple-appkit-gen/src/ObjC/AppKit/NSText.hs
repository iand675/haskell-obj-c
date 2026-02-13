{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , alignCenterSelector
  , alignLeftSelector
  , alignRightSelector
  , alignmentSelector
  , backgroundColorSelector
  , baseWritingDirectionSelector
  , changeFontSelector
  , checkSpellingSelector
  , copyFontSelector
  , copyRulerSelector
  , copySelector
  , cutSelector
  , delegateSelector
  , deleteSelector
  , drawsBackgroundSelector
  , editableSelector
  , fieldEditorSelector
  , fontSelector
  , horizontallyResizableSelector
  , importsGraphicsSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , maxSizeSelector
  , minSizeSelector
  , pasteFontSelector
  , pasteRulerSelector
  , pasteSelector
  , readRTFDFromFileSelector
  , replaceCharactersInRange_withRTFDSelector
  , replaceCharactersInRange_withRTFSelector
  , replaceCharactersInRange_withStringSelector
  , richTextSelector
  , rtfFromRangeSelector
  , rtfdFromRangeSelector
  , rulerVisibleSelector
  , scrollRangeToVisibleSelector
  , selectAllSelector
  , selectableSelector
  , selectedRangeSelector
  , setAlignmentSelector
  , setBackgroundColorSelector
  , setBaseWritingDirectionSelector
  , setDelegateSelector
  , setDrawsBackgroundSelector
  , setEditableSelector
  , setFieldEditorSelector
  , setFontSelector
  , setFont_rangeSelector
  , setHorizontallyResizableSelector
  , setImportsGraphicsSelector
  , setMaxSizeSelector
  , setMinSizeSelector
  , setRichTextSelector
  , setSelectableSelector
  , setSelectedRangeSelector
  , setStringSelector
  , setTextColorSelector
  , setTextColor_rangeSelector
  , setUsesFontPanelSelector
  , setVerticallyResizableSelector
  , showGuessPanelSelector
  , sizeToFitSelector
  , stringSelector
  , subscriptSelector
  , superscriptSelector
  , textColorSelector
  , toggleRulerSelector
  , underlineSelector
  , unscriptSelector
  , usesFontPanelSelector
  , verticallyResizableSelector
  , writeRTFDToFile_atomicallySelector

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

-- | @- initWithFrame:@
initWithFrame :: IsNSText nsText => nsText -> NSRect -> IO (Id NSText)
initWithFrame nsText frameRect =
  sendOwnedMessage nsText initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSText nsText, IsNSCoder coder) => nsText -> coder -> IO (Id NSText)
initWithCoder nsText coder =
  sendOwnedMessage nsText initWithCoderSelector (toNSCoder coder)

-- | @- replaceCharactersInRange:withString:@
replaceCharactersInRange_withString :: (IsNSText nsText, IsNSString string) => nsText -> NSRange -> string -> IO ()
replaceCharactersInRange_withString nsText range string =
  sendMessage nsText replaceCharactersInRange_withStringSelector range (toNSString string)

-- | @- replaceCharactersInRange:withRTF:@
replaceCharactersInRange_withRTF :: (IsNSText nsText, IsNSData rtfData) => nsText -> NSRange -> rtfData -> IO ()
replaceCharactersInRange_withRTF nsText range rtfData =
  sendMessage nsText replaceCharactersInRange_withRTFSelector range (toNSData rtfData)

-- | @- replaceCharactersInRange:withRTFD:@
replaceCharactersInRange_withRTFD :: (IsNSText nsText, IsNSData rtfdData) => nsText -> NSRange -> rtfdData -> IO ()
replaceCharactersInRange_withRTFD nsText range rtfdData =
  sendMessage nsText replaceCharactersInRange_withRTFDSelector range (toNSData rtfdData)

-- | @- RTFFromRange:@
rtfFromRange :: IsNSText nsText => nsText -> NSRange -> IO (Id NSData)
rtfFromRange nsText range =
  sendMessage nsText rtfFromRangeSelector range

-- | @- RTFDFromRange:@
rtfdFromRange :: IsNSText nsText => nsText -> NSRange -> IO (Id NSData)
rtfdFromRange nsText range =
  sendMessage nsText rtfdFromRangeSelector range

-- | @- writeRTFDToFile:atomically:@
writeRTFDToFile_atomically :: (IsNSText nsText, IsNSString path) => nsText -> path -> Bool -> IO Bool
writeRTFDToFile_atomically nsText path flag =
  sendMessage nsText writeRTFDToFile_atomicallySelector (toNSString path) flag

-- | @- readRTFDFromFile:@
readRTFDFromFile :: (IsNSText nsText, IsNSString path) => nsText -> path -> IO Bool
readRTFDFromFile nsText path =
  sendMessage nsText readRTFDFromFileSelector (toNSString path)

-- | @- scrollRangeToVisible:@
scrollRangeToVisible :: IsNSText nsText => nsText -> NSRange -> IO ()
scrollRangeToVisible nsText range =
  sendMessage nsText scrollRangeToVisibleSelector range

-- | @- setTextColor:range:@
setTextColor_range :: (IsNSText nsText, IsNSColor color) => nsText -> color -> NSRange -> IO ()
setTextColor_range nsText color range =
  sendMessage nsText setTextColor_rangeSelector (toNSColor color) range

-- | @- setFont:range:@
setFont_range :: (IsNSText nsText, IsNSFont font) => nsText -> font -> NSRange -> IO ()
setFont_range nsText font range =
  sendMessage nsText setFont_rangeSelector (toNSFont font) range

-- | @- sizeToFit@
sizeToFit :: IsNSText nsText => nsText -> IO ()
sizeToFit nsText =
  sendMessage nsText sizeToFitSelector

-- | @- copy:@
copy :: IsNSText nsText => nsText -> RawId -> IO ()
copy nsText sender =
  sendOwnedMessage nsText copySelector sender

-- | @- copyFont:@
copyFont :: IsNSText nsText => nsText -> RawId -> IO ()
copyFont nsText sender =
  sendOwnedMessage nsText copyFontSelector sender

-- | @- copyRuler:@
copyRuler :: IsNSText nsText => nsText -> RawId -> IO ()
copyRuler nsText sender =
  sendOwnedMessage nsText copyRulerSelector sender

-- | @- cut:@
cut :: IsNSText nsText => nsText -> RawId -> IO ()
cut nsText sender =
  sendMessage nsText cutSelector sender

-- | @- delete:@
delete :: IsNSText nsText => nsText -> RawId -> IO ()
delete nsText sender =
  sendMessage nsText deleteSelector sender

-- | @- paste:@
paste :: IsNSText nsText => nsText -> RawId -> IO ()
paste nsText sender =
  sendMessage nsText pasteSelector sender

-- | @- pasteFont:@
pasteFont :: IsNSText nsText => nsText -> RawId -> IO ()
pasteFont nsText sender =
  sendMessage nsText pasteFontSelector sender

-- | @- pasteRuler:@
pasteRuler :: IsNSText nsText => nsText -> RawId -> IO ()
pasteRuler nsText sender =
  sendMessage nsText pasteRulerSelector sender

-- | @- selectAll:@
selectAll :: IsNSText nsText => nsText -> RawId -> IO ()
selectAll nsText sender =
  sendMessage nsText selectAllSelector sender

-- | @- changeFont:@
changeFont :: IsNSText nsText => nsText -> RawId -> IO ()
changeFont nsText sender =
  sendMessage nsText changeFontSelector sender

-- | @- alignLeft:@
alignLeft :: IsNSText nsText => nsText -> RawId -> IO ()
alignLeft nsText sender =
  sendMessage nsText alignLeftSelector sender

-- | @- alignRight:@
alignRight :: IsNSText nsText => nsText -> RawId -> IO ()
alignRight nsText sender =
  sendMessage nsText alignRightSelector sender

-- | @- alignCenter:@
alignCenter :: IsNSText nsText => nsText -> RawId -> IO ()
alignCenter nsText sender =
  sendMessage nsText alignCenterSelector sender

-- | @- subscript:@
subscript :: IsNSText nsText => nsText -> RawId -> IO ()
subscript nsText sender =
  sendMessage nsText subscriptSelector sender

-- | @- superscript:@
superscript :: IsNSText nsText => nsText -> RawId -> IO ()
superscript nsText sender =
  sendMessage nsText superscriptSelector sender

-- | @- underline:@
underline :: IsNSText nsText => nsText -> RawId -> IO ()
underline nsText sender =
  sendMessage nsText underlineSelector sender

-- | @- unscript:@
unscript :: IsNSText nsText => nsText -> RawId -> IO ()
unscript nsText sender =
  sendMessage nsText unscriptSelector sender

-- | @- showGuessPanel:@
showGuessPanel :: IsNSText nsText => nsText -> RawId -> IO ()
showGuessPanel nsText sender =
  sendMessage nsText showGuessPanelSelector sender

-- | @- checkSpelling:@
checkSpelling :: IsNSText nsText => nsText -> RawId -> IO ()
checkSpelling nsText sender =
  sendMessage nsText checkSpellingSelector sender

-- | @- toggleRuler:@
toggleRuler :: IsNSText nsText => nsText -> RawId -> IO ()
toggleRuler nsText sender =
  sendMessage nsText toggleRulerSelector sender

-- | @- string@
string :: IsNSText nsText => nsText -> IO (Id NSString)
string nsText =
  sendMessage nsText stringSelector

-- | @- setString:@
setString :: (IsNSText nsText, IsNSString value) => nsText -> value -> IO ()
setString nsText value =
  sendMessage nsText setStringSelector (toNSString value)

-- | @- delegate@
delegate :: IsNSText nsText => nsText -> IO RawId
delegate nsText =
  sendMessage nsText delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSText nsText => nsText -> RawId -> IO ()
setDelegate nsText value =
  sendMessage nsText setDelegateSelector value

-- | @- editable@
editable :: IsNSText nsText => nsText -> IO Bool
editable nsText =
  sendMessage nsText editableSelector

-- | @- setEditable:@
setEditable :: IsNSText nsText => nsText -> Bool -> IO ()
setEditable nsText value =
  sendMessage nsText setEditableSelector value

-- | @- selectable@
selectable :: IsNSText nsText => nsText -> IO Bool
selectable nsText =
  sendMessage nsText selectableSelector

-- | @- setSelectable:@
setSelectable :: IsNSText nsText => nsText -> Bool -> IO ()
setSelectable nsText value =
  sendMessage nsText setSelectableSelector value

-- | @- richText@
richText :: IsNSText nsText => nsText -> IO Bool
richText nsText =
  sendMessage nsText richTextSelector

-- | @- setRichText:@
setRichText :: IsNSText nsText => nsText -> Bool -> IO ()
setRichText nsText value =
  sendMessage nsText setRichTextSelector value

-- | @- importsGraphics@
importsGraphics :: IsNSText nsText => nsText -> IO Bool
importsGraphics nsText =
  sendMessage nsText importsGraphicsSelector

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSText nsText => nsText -> Bool -> IO ()
setImportsGraphics nsText value =
  sendMessage nsText setImportsGraphicsSelector value

-- | @- fieldEditor@
fieldEditor :: IsNSText nsText => nsText -> IO Bool
fieldEditor nsText =
  sendMessage nsText fieldEditorSelector

-- | @- setFieldEditor:@
setFieldEditor :: IsNSText nsText => nsText -> Bool -> IO ()
setFieldEditor nsText value =
  sendMessage nsText setFieldEditorSelector value

-- | @- usesFontPanel@
usesFontPanel :: IsNSText nsText => nsText -> IO Bool
usesFontPanel nsText =
  sendMessage nsText usesFontPanelSelector

-- | @- setUsesFontPanel:@
setUsesFontPanel :: IsNSText nsText => nsText -> Bool -> IO ()
setUsesFontPanel nsText value =
  sendMessage nsText setUsesFontPanelSelector value

-- | @- drawsBackground@
drawsBackground :: IsNSText nsText => nsText -> IO Bool
drawsBackground nsText =
  sendMessage nsText drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSText nsText => nsText -> Bool -> IO ()
setDrawsBackground nsText value =
  sendMessage nsText setDrawsBackgroundSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSText nsText => nsText -> IO (Id NSColor)
backgroundColor nsText =
  sendMessage nsText backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSText nsText, IsNSColor value) => nsText -> value -> IO ()
setBackgroundColor nsText value =
  sendMessage nsText setBackgroundColorSelector (toNSColor value)

-- | @- rulerVisible@
rulerVisible :: IsNSText nsText => nsText -> IO Bool
rulerVisible nsText =
  sendMessage nsText rulerVisibleSelector

-- | @- selectedRange@
selectedRange :: IsNSText nsText => nsText -> IO NSRange
selectedRange nsText =
  sendMessage nsText selectedRangeSelector

-- | @- setSelectedRange:@
setSelectedRange :: IsNSText nsText => nsText -> NSRange -> IO ()
setSelectedRange nsText value =
  sendMessage nsText setSelectedRangeSelector value

-- | @- font@
font :: IsNSText nsText => nsText -> IO (Id NSFont)
font nsText =
  sendMessage nsText fontSelector

-- | @- setFont:@
setFont :: (IsNSText nsText, IsNSFont value) => nsText -> value -> IO ()
setFont nsText value =
  sendMessage nsText setFontSelector (toNSFont value)

-- | @- textColor@
textColor :: IsNSText nsText => nsText -> IO (Id NSColor)
textColor nsText =
  sendMessage nsText textColorSelector

-- | @- setTextColor:@
setTextColor :: (IsNSText nsText, IsNSColor value) => nsText -> value -> IO ()
setTextColor nsText value =
  sendMessage nsText setTextColorSelector (toNSColor value)

-- | @- alignment@
alignment :: IsNSText nsText => nsText -> IO NSTextAlignment
alignment nsText =
  sendMessage nsText alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsNSText nsText => nsText -> NSTextAlignment -> IO ()
setAlignment nsText value =
  sendMessage nsText setAlignmentSelector value

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSText nsText => nsText -> IO NSWritingDirection
baseWritingDirection nsText =
  sendMessage nsText baseWritingDirectionSelector

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSText nsText => nsText -> NSWritingDirection -> IO ()
setBaseWritingDirection nsText value =
  sendMessage nsText setBaseWritingDirectionSelector value

-- | @- maxSize@
maxSize :: IsNSText nsText => nsText -> IO NSSize
maxSize nsText =
  sendMessage nsText maxSizeSelector

-- | @- setMaxSize:@
setMaxSize :: IsNSText nsText => nsText -> NSSize -> IO ()
setMaxSize nsText value =
  sendMessage nsText setMaxSizeSelector value

-- | @- minSize@
minSize :: IsNSText nsText => nsText -> IO NSSize
minSize nsText =
  sendMessage nsText minSizeSelector

-- | @- setMinSize:@
setMinSize :: IsNSText nsText => nsText -> NSSize -> IO ()
setMinSize nsText value =
  sendMessage nsText setMinSizeSelector value

-- | @- horizontallyResizable@
horizontallyResizable :: IsNSText nsText => nsText -> IO Bool
horizontallyResizable nsText =
  sendMessage nsText horizontallyResizableSelector

-- | @- setHorizontallyResizable:@
setHorizontallyResizable :: IsNSText nsText => nsText -> Bool -> IO ()
setHorizontallyResizable nsText value =
  sendMessage nsText setHorizontallyResizableSelector value

-- | @- verticallyResizable@
verticallyResizable :: IsNSText nsText => nsText -> IO Bool
verticallyResizable nsText =
  sendMessage nsText verticallyResizableSelector

-- | @- setVerticallyResizable:@
setVerticallyResizable :: IsNSText nsText => nsText -> Bool -> IO ()
setVerticallyResizable nsText value =
  sendMessage nsText setVerticallyResizableSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSText)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSText)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @replaceCharactersInRange:withString:@
replaceCharactersInRange_withStringSelector :: Selector '[NSRange, Id NSString] ()
replaceCharactersInRange_withStringSelector = mkSelector "replaceCharactersInRange:withString:"

-- | @Selector@ for @replaceCharactersInRange:withRTF:@
replaceCharactersInRange_withRTFSelector :: Selector '[NSRange, Id NSData] ()
replaceCharactersInRange_withRTFSelector = mkSelector "replaceCharactersInRange:withRTF:"

-- | @Selector@ for @replaceCharactersInRange:withRTFD:@
replaceCharactersInRange_withRTFDSelector :: Selector '[NSRange, Id NSData] ()
replaceCharactersInRange_withRTFDSelector = mkSelector "replaceCharactersInRange:withRTFD:"

-- | @Selector@ for @RTFFromRange:@
rtfFromRangeSelector :: Selector '[NSRange] (Id NSData)
rtfFromRangeSelector = mkSelector "RTFFromRange:"

-- | @Selector@ for @RTFDFromRange:@
rtfdFromRangeSelector :: Selector '[NSRange] (Id NSData)
rtfdFromRangeSelector = mkSelector "RTFDFromRange:"

-- | @Selector@ for @writeRTFDToFile:atomically:@
writeRTFDToFile_atomicallySelector :: Selector '[Id NSString, Bool] Bool
writeRTFDToFile_atomicallySelector = mkSelector "writeRTFDToFile:atomically:"

-- | @Selector@ for @readRTFDFromFile:@
readRTFDFromFileSelector :: Selector '[Id NSString] Bool
readRTFDFromFileSelector = mkSelector "readRTFDFromFile:"

-- | @Selector@ for @scrollRangeToVisible:@
scrollRangeToVisibleSelector :: Selector '[NSRange] ()
scrollRangeToVisibleSelector = mkSelector "scrollRangeToVisible:"

-- | @Selector@ for @setTextColor:range:@
setTextColor_rangeSelector :: Selector '[Id NSColor, NSRange] ()
setTextColor_rangeSelector = mkSelector "setTextColor:range:"

-- | @Selector@ for @setFont:range:@
setFont_rangeSelector :: Selector '[Id NSFont, NSRange] ()
setFont_rangeSelector = mkSelector "setFont:range:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @copy:@
copySelector :: Selector '[RawId] ()
copySelector = mkSelector "copy:"

-- | @Selector@ for @copyFont:@
copyFontSelector :: Selector '[RawId] ()
copyFontSelector = mkSelector "copyFont:"

-- | @Selector@ for @copyRuler:@
copyRulerSelector :: Selector '[RawId] ()
copyRulerSelector = mkSelector "copyRuler:"

-- | @Selector@ for @cut:@
cutSelector :: Selector '[RawId] ()
cutSelector = mkSelector "cut:"

-- | @Selector@ for @delete:@
deleteSelector :: Selector '[RawId] ()
deleteSelector = mkSelector "delete:"

-- | @Selector@ for @paste:@
pasteSelector :: Selector '[RawId] ()
pasteSelector = mkSelector "paste:"

-- | @Selector@ for @pasteFont:@
pasteFontSelector :: Selector '[RawId] ()
pasteFontSelector = mkSelector "pasteFont:"

-- | @Selector@ for @pasteRuler:@
pasteRulerSelector :: Selector '[RawId] ()
pasteRulerSelector = mkSelector "pasteRuler:"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector '[RawId] ()
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @changeFont:@
changeFontSelector :: Selector '[RawId] ()
changeFontSelector = mkSelector "changeFont:"

-- | @Selector@ for @alignLeft:@
alignLeftSelector :: Selector '[RawId] ()
alignLeftSelector = mkSelector "alignLeft:"

-- | @Selector@ for @alignRight:@
alignRightSelector :: Selector '[RawId] ()
alignRightSelector = mkSelector "alignRight:"

-- | @Selector@ for @alignCenter:@
alignCenterSelector :: Selector '[RawId] ()
alignCenterSelector = mkSelector "alignCenter:"

-- | @Selector@ for @subscript:@
subscriptSelector :: Selector '[RawId] ()
subscriptSelector = mkSelector "subscript:"

-- | @Selector@ for @superscript:@
superscriptSelector :: Selector '[RawId] ()
superscriptSelector = mkSelector "superscript:"

-- | @Selector@ for @underline:@
underlineSelector :: Selector '[RawId] ()
underlineSelector = mkSelector "underline:"

-- | @Selector@ for @unscript:@
unscriptSelector :: Selector '[RawId] ()
unscriptSelector = mkSelector "unscript:"

-- | @Selector@ for @showGuessPanel:@
showGuessPanelSelector :: Selector '[RawId] ()
showGuessPanelSelector = mkSelector "showGuessPanel:"

-- | @Selector@ for @checkSpelling:@
checkSpellingSelector :: Selector '[RawId] ()
checkSpellingSelector = mkSelector "checkSpelling:"

-- | @Selector@ for @toggleRuler:@
toggleRulerSelector :: Selector '[RawId] ()
toggleRulerSelector = mkSelector "toggleRuler:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[Id NSString] ()
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector '[] Bool
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector '[Bool] ()
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @richText@
richTextSelector :: Selector '[] Bool
richTextSelector = mkSelector "richText"

-- | @Selector@ for @setRichText:@
setRichTextSelector :: Selector '[Bool] ()
setRichTextSelector = mkSelector "setRichText:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector '[] Bool
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector '[Bool] ()
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @fieldEditor@
fieldEditorSelector :: Selector '[] Bool
fieldEditorSelector = mkSelector "fieldEditor"

-- | @Selector@ for @setFieldEditor:@
setFieldEditorSelector :: Selector '[Bool] ()
setFieldEditorSelector = mkSelector "setFieldEditor:"

-- | @Selector@ for @usesFontPanel@
usesFontPanelSelector :: Selector '[] Bool
usesFontPanelSelector = mkSelector "usesFontPanel"

-- | @Selector@ for @setUsesFontPanel:@
setUsesFontPanelSelector :: Selector '[Bool] ()
setUsesFontPanelSelector = mkSelector "setUsesFontPanel:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @rulerVisible@
rulerVisibleSelector :: Selector '[] Bool
rulerVisibleSelector = mkSelector "rulerVisible"

-- | @Selector@ for @selectedRange@
selectedRangeSelector :: Selector '[] NSRange
selectedRangeSelector = mkSelector "selectedRange"

-- | @Selector@ for @setSelectedRange:@
setSelectedRangeSelector :: Selector '[NSRange] ()
setSelectedRangeSelector = mkSelector "setSelectedRange:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @textColor@
textColorSelector :: Selector '[] (Id NSColor)
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @setTextColor:@
setTextColorSelector :: Selector '[Id NSColor] ()
setTextColorSelector = mkSelector "setTextColor:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSTextAlignment] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector '[] NSWritingDirection
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @maxSize@
maxSizeSelector :: Selector '[] NSSize
maxSizeSelector = mkSelector "maxSize"

-- | @Selector@ for @setMaxSize:@
setMaxSizeSelector :: Selector '[NSSize] ()
setMaxSizeSelector = mkSelector "setMaxSize:"

-- | @Selector@ for @minSize@
minSizeSelector :: Selector '[] NSSize
minSizeSelector = mkSelector "minSize"

-- | @Selector@ for @setMinSize:@
setMinSizeSelector :: Selector '[NSSize] ()
setMinSizeSelector = mkSelector "setMinSize:"

-- | @Selector@ for @horizontallyResizable@
horizontallyResizableSelector :: Selector '[] Bool
horizontallyResizableSelector = mkSelector "horizontallyResizable"

-- | @Selector@ for @setHorizontallyResizable:@
setHorizontallyResizableSelector :: Selector '[Bool] ()
setHorizontallyResizableSelector = mkSelector "setHorizontallyResizable:"

-- | @Selector@ for @verticallyResizable@
verticallyResizableSelector :: Selector '[] Bool
verticallyResizableSelector = mkSelector "verticallyResizable"

-- | @Selector@ for @setVerticallyResizable:@
setVerticallyResizableSelector :: Selector '[Bool] ()
setVerticallyResizableSelector = mkSelector "setVerticallyResizable:"

