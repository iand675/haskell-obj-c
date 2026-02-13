{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotation@.
module ObjC.PDFKit.PDFAnnotation
  ( PDFAnnotation
  , IsPDFAnnotation(..)
  , drawWithBox_inContext
  , lineStyleFromName
  , nameForLineStyle
  , addBezierPath
  , removeBezierPath
  , initWithDictionary_forPage
  , initWithBounds
  , removeAllAppearanceStreams
  , drawWithBox
  , page
  , setPage
  , type_
  , setType
  , bounds
  , setBounds
  , shouldDisplay
  , setShouldDisplay
  , shouldPrint
  , setShouldPrint
  , modificationDate
  , setModificationDate
  , userName
  , setUserName
  , popup
  , setPopup
  , border
  , setBorder
  , color
  , setColor
  , contents
  , setContents
  , action
  , setAction
  , hasAppearanceStream
  , highlighted
  , setHighlighted
  , annotationKeyValues
  , font
  , setFont
  , fontColor
  , setFontColor
  , interiorColor
  , setInteriorColor
  , alignment
  , setAlignment
  , startPoint
  , setStartPoint
  , endPoint
  , setEndPoint
  , startLineStyle
  , setStartLineStyle
  , endLineStyle
  , setEndLineStyle
  , iconType
  , setIconType
  , quadrilateralPoints
  , setQuadrilateralPoints
  , markupType
  , setMarkupType
  , widgetControlType
  , setWidgetControlType
  , multiline
  , setMultiline
  , activatableTextField
  , isPasswordField
  , comb
  , setComb
  , maximumLength
  , setMaximumLength
  , widgetStringValue
  , setWidgetStringValue
  , widgetDefaultStringValue
  , setWidgetDefaultStringValue
  , allowsToggleToOff
  , setAllowsToggleToOff
  , radiosInUnison
  , setRadiosInUnison
  , readOnly
  , setReadOnly
  , listChoice
  , setListChoice
  , choices
  , setChoices
  , values
  , setValues
  , buttonWidgetState
  , setButtonWidgetState
  , buttonWidgetStateString
  , setButtonWidgetStateString
  , open
  , setOpen
  , paths
  , destination
  , setDestination
  , url
  , setURL
  , fieldName
  , setFieldName
  , caption
  , setCaption
  , backgroundColor
  , setBackgroundColor
  , stampName
  , setStampName
  , toolTip
  , mouseUpAction
  , setMouseUpAction
  , actionSelector
  , activatableTextFieldSelector
  , addBezierPathSelector
  , alignmentSelector
  , allowsToggleToOffSelector
  , annotationKeyValuesSelector
  , backgroundColorSelector
  , borderSelector
  , boundsSelector
  , buttonWidgetStateSelector
  , buttonWidgetStateStringSelector
  , captionSelector
  , choicesSelector
  , colorSelector
  , combSelector
  , contentsSelector
  , destinationSelector
  , drawWithBoxSelector
  , drawWithBox_inContextSelector
  , endLineStyleSelector
  , endPointSelector
  , fieldNameSelector
  , fontColorSelector
  , fontSelector
  , hasAppearanceStreamSelector
  , highlightedSelector
  , iconTypeSelector
  , initWithBoundsSelector
  , initWithDictionary_forPageSelector
  , interiorColorSelector
  , isPasswordFieldSelector
  , lineStyleFromNameSelector
  , listChoiceSelector
  , markupTypeSelector
  , maximumLengthSelector
  , modificationDateSelector
  , mouseUpActionSelector
  , multilineSelector
  , nameForLineStyleSelector
  , openSelector
  , pageSelector
  , pathsSelector
  , popupSelector
  , quadrilateralPointsSelector
  , radiosInUnisonSelector
  , readOnlySelector
  , removeAllAppearanceStreamsSelector
  , removeBezierPathSelector
  , setActionSelector
  , setAlignmentSelector
  , setAllowsToggleToOffSelector
  , setBackgroundColorSelector
  , setBorderSelector
  , setBoundsSelector
  , setButtonWidgetStateSelector
  , setButtonWidgetStateStringSelector
  , setCaptionSelector
  , setChoicesSelector
  , setColorSelector
  , setCombSelector
  , setContentsSelector
  , setDestinationSelector
  , setEndLineStyleSelector
  , setEndPointSelector
  , setFieldNameSelector
  , setFontColorSelector
  , setFontSelector
  , setHighlightedSelector
  , setIconTypeSelector
  , setInteriorColorSelector
  , setListChoiceSelector
  , setMarkupTypeSelector
  , setMaximumLengthSelector
  , setModificationDateSelector
  , setMouseUpActionSelector
  , setMultilineSelector
  , setOpenSelector
  , setPageSelector
  , setPopupSelector
  , setQuadrilateralPointsSelector
  , setRadiosInUnisonSelector
  , setReadOnlySelector
  , setShouldDisplaySelector
  , setShouldPrintSelector
  , setStampNameSelector
  , setStartLineStyleSelector
  , setStartPointSelector
  , setTypeSelector
  , setURLSelector
  , setUserNameSelector
  , setValuesSelector
  , setWidgetControlTypeSelector
  , setWidgetDefaultStringValueSelector
  , setWidgetStringValueSelector
  , shouldDisplaySelector
  , shouldPrintSelector
  , stampNameSelector
  , startLineStyleSelector
  , startPointSelector
  , toolTipSelector
  , typeSelector
  , urlSelector
  , userNameSelector
  , valuesSelector
  , widgetControlTypeSelector
  , widgetDefaultStringValueSelector
  , widgetStringValueSelector

  -- * Enum types
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
  , PDFDisplayBox(PDFDisplayBox)
  , pattern KPDFDisplayBoxMediaBox
  , pattern KPDFDisplayBoxCropBox
  , pattern KPDFDisplayBoxBleedBox
  , pattern KPDFDisplayBoxTrimBox
  , pattern KPDFDisplayBoxArtBox
  , PDFLineStyle(PDFLineStyle)
  , pattern KPDFLineStyleNone
  , pattern KPDFLineStyleSquare
  , pattern KPDFLineStyleCircle
  , pattern KPDFLineStyleDiamond
  , pattern KPDFLineStyleOpenArrow
  , pattern KPDFLineStyleClosedArrow
  , PDFMarkupType(PDFMarkupType)
  , pattern KPDFMarkupTypeHighlight
  , pattern KPDFMarkupTypeStrikeOut
  , pattern KPDFMarkupTypeUnderline
  , pattern KPDFMarkupTypeRedact
  , PDFTextAnnotationIconType(PDFTextAnnotationIconType)
  , pattern KPDFTextAnnotationIconComment
  , pattern KPDFTextAnnotationIconKey
  , pattern KPDFTextAnnotationIconNote
  , pattern KPDFTextAnnotationIconHelp
  , pattern KPDFTextAnnotationIconNewParagraph
  , pattern KPDFTextAnnotationIconParagraph
  , pattern KPDFTextAnnotationIconInsert
  , PDFWidgetCellState(PDFWidgetCellState)
  , pattern KPDFWidgetMixedState
  , pattern KPDFWidgetOffState
  , pattern KPDFWidgetOnState
  , PDFWidgetControlType(PDFWidgetControlType)
  , pattern KPDFWidgetUnknownControl
  , pattern KPDFWidgetPushButtonControl
  , pattern KPDFWidgetRadioButtonControl
  , pattern KPDFWidgetCheckBoxControl

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- drawWithBox:inContext:@
drawWithBox_inContext :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFDisplayBox -> Ptr () -> IO ()
drawWithBox_inContext pdfAnnotation box context =
  sendMessage pdfAnnotation drawWithBox_inContextSelector box context

-- | @+ lineStyleFromName:@
lineStyleFromName :: IsNSString name => name -> IO PDFLineStyle
lineStyleFromName name =
  do
    cls' <- getRequiredClass "PDFAnnotation"
    sendClassMessage cls' lineStyleFromNameSelector (toNSString name)

-- | @+ nameForLineStyle:@
nameForLineStyle :: PDFLineStyle -> IO (Id NSString)
nameForLineStyle style =
  do
    cls' <- getRequiredClass "PDFAnnotation"
    sendClassMessage cls' nameForLineStyleSelector style

-- | @- addBezierPath:@
addBezierPath :: (IsPDFAnnotation pdfAnnotation, IsNSBezierPath path) => pdfAnnotation -> path -> IO ()
addBezierPath pdfAnnotation path =
  sendMessage pdfAnnotation addBezierPathSelector (toNSBezierPath path)

-- | @- removeBezierPath:@
removeBezierPath :: (IsPDFAnnotation pdfAnnotation, IsNSBezierPath path) => pdfAnnotation -> path -> IO ()
removeBezierPath pdfAnnotation path =
  sendMessage pdfAnnotation removeBezierPathSelector (toNSBezierPath path)

-- | @- initWithDictionary:forPage:@
initWithDictionary_forPage :: (IsPDFAnnotation pdfAnnotation, IsNSDictionary dictionary, IsPDFPage page) => pdfAnnotation -> dictionary -> page -> IO (Id PDFAnnotation)
initWithDictionary_forPage pdfAnnotation dictionary page =
  sendOwnedMessage pdfAnnotation initWithDictionary_forPageSelector (toNSDictionary dictionary) (toPDFPage page)

-- | @- initWithBounds:@
initWithBounds :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSRect -> IO (Id PDFAnnotation)
initWithBounds pdfAnnotation bounds =
  sendOwnedMessage pdfAnnotation initWithBoundsSelector bounds

-- | @- removeAllAppearanceStreams@
removeAllAppearanceStreams :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO ()
removeAllAppearanceStreams pdfAnnotation =
  sendMessage pdfAnnotation removeAllAppearanceStreamsSelector

-- | @- drawWithBox:@
drawWithBox :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFDisplayBox -> IO ()
drawWithBox pdfAnnotation box =
  sendMessage pdfAnnotation drawWithBoxSelector box

-- | @- page@
page :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id PDFPage)
page pdfAnnotation =
  sendMessage pdfAnnotation pageSelector

-- | @- setPage:@
setPage :: (IsPDFAnnotation pdfAnnotation, IsPDFPage value) => pdfAnnotation -> value -> IO ()
setPage pdfAnnotation value =
  sendMessage pdfAnnotation setPageSelector (toPDFPage value)

-- | @- type@
type_ :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
type_ pdfAnnotation =
  sendMessage pdfAnnotation typeSelector

-- | @- setType:@
setType :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setType pdfAnnotation value =
  sendMessage pdfAnnotation setTypeSelector (toNSString value)

-- | @- bounds@
bounds :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSRect
bounds pdfAnnotation =
  sendMessage pdfAnnotation boundsSelector

-- | @- setBounds:@
setBounds :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSRect -> IO ()
setBounds pdfAnnotation value =
  sendMessage pdfAnnotation setBoundsSelector value

-- | @- shouldDisplay@
shouldDisplay :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
shouldDisplay pdfAnnotation =
  sendMessage pdfAnnotation shouldDisplaySelector

-- | @- setShouldDisplay:@
setShouldDisplay :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setShouldDisplay pdfAnnotation value =
  sendMessage pdfAnnotation setShouldDisplaySelector value

-- | @- shouldPrint@
shouldPrint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
shouldPrint pdfAnnotation =
  sendMessage pdfAnnotation shouldPrintSelector

-- | @- setShouldPrint:@
setShouldPrint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setShouldPrint pdfAnnotation value =
  sendMessage pdfAnnotation setShouldPrintSelector value

-- | @- modificationDate@
modificationDate :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
modificationDate pdfAnnotation =
  sendMessage pdfAnnotation modificationDateSelector

-- | @- setModificationDate:@
setModificationDate :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setModificationDate pdfAnnotation value =
  sendMessage pdfAnnotation setModificationDateSelector value

-- | @- userName@
userName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
userName pdfAnnotation =
  sendMessage pdfAnnotation userNameSelector

-- | @- setUserName:@
setUserName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setUserName pdfAnnotation value =
  sendMessage pdfAnnotation setUserNameSelector value

-- | @- popup@
popup :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
popup pdfAnnotation =
  sendMessage pdfAnnotation popupSelector

-- | @- setPopup:@
setPopup :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setPopup pdfAnnotation value =
  sendMessage pdfAnnotation setPopupSelector value

-- | @- border@
border :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
border pdfAnnotation =
  sendMessage pdfAnnotation borderSelector

-- | @- setBorder:@
setBorder :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setBorder pdfAnnotation value =
  sendMessage pdfAnnotation setBorderSelector value

-- | @- color@
color :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
color pdfAnnotation =
  sendMessage pdfAnnotation colorSelector

-- | @- setColor:@
setColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setColor pdfAnnotation value =
  sendMessage pdfAnnotation setColorSelector value

-- | @- contents@
contents :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
contents pdfAnnotation =
  sendMessage pdfAnnotation contentsSelector

-- | @- setContents:@
setContents :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setContents pdfAnnotation value =
  sendMessage pdfAnnotation setContentsSelector value

-- | @- action@
action :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
action pdfAnnotation =
  sendMessage pdfAnnotation actionSelector

-- | @- setAction:@
setAction :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setAction pdfAnnotation value =
  sendMessage pdfAnnotation setActionSelector value

-- | @- hasAppearanceStream@
hasAppearanceStream :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
hasAppearanceStream pdfAnnotation =
  sendMessage pdfAnnotation hasAppearanceStreamSelector

-- | @- highlighted@
highlighted :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
highlighted pdfAnnotation =
  sendMessage pdfAnnotation highlightedSelector

-- | @- setHighlighted:@
setHighlighted :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setHighlighted pdfAnnotation value =
  sendMessage pdfAnnotation setHighlightedSelector value

-- | @- annotationKeyValues@
annotationKeyValues :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
annotationKeyValues pdfAnnotation =
  sendMessage pdfAnnotation annotationKeyValuesSelector

-- | @- font@
font :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSFont)
font pdfAnnotation =
  sendMessage pdfAnnotation fontSelector

-- | @- setFont:@
setFont :: (IsPDFAnnotation pdfAnnotation, IsNSFont value) => pdfAnnotation -> value -> IO ()
setFont pdfAnnotation value =
  sendMessage pdfAnnotation setFontSelector (toNSFont value)

-- | @- fontColor@
fontColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSColor)
fontColor pdfAnnotation =
  sendMessage pdfAnnotation fontColorSelector

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotation pdfAnnotation, IsNSColor value) => pdfAnnotation -> value -> IO ()
setFontColor pdfAnnotation value =
  sendMessage pdfAnnotation setFontColorSelector (toNSColor value)

-- | @- interiorColor@
interiorColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSColor)
interiorColor pdfAnnotation =
  sendMessage pdfAnnotation interiorColorSelector

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotation pdfAnnotation, IsNSColor value) => pdfAnnotation -> value -> IO ()
setInteriorColor pdfAnnotation value =
  sendMessage pdfAnnotation setInteriorColorSelector (toNSColor value)

-- | @- alignment@
alignment :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSTextAlignment
alignment pdfAnnotation =
  sendMessage pdfAnnotation alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSTextAlignment -> IO ()
setAlignment pdfAnnotation value =
  sendMessage pdfAnnotation setAlignmentSelector value

-- | @- startPoint@
startPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSPoint
startPoint pdfAnnotation =
  sendMessage pdfAnnotation startPointSelector

-- | @- setStartPoint:@
setStartPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSPoint -> IO ()
setStartPoint pdfAnnotation value =
  sendMessage pdfAnnotation setStartPointSelector value

-- | @- endPoint@
endPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSPoint
endPoint pdfAnnotation =
  sendMessage pdfAnnotation endPointSelector

-- | @- setEndPoint:@
setEndPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSPoint -> IO ()
setEndPoint pdfAnnotation value =
  sendMessage pdfAnnotation setEndPointSelector value

-- | @- startLineStyle@
startLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFLineStyle
startLineStyle pdfAnnotation =
  sendMessage pdfAnnotation startLineStyleSelector

-- | @- setStartLineStyle:@
setStartLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFLineStyle -> IO ()
setStartLineStyle pdfAnnotation value =
  sendMessage pdfAnnotation setStartLineStyleSelector value

-- | @- endLineStyle@
endLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFLineStyle
endLineStyle pdfAnnotation =
  sendMessage pdfAnnotation endLineStyleSelector

-- | @- setEndLineStyle:@
setEndLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFLineStyle -> IO ()
setEndLineStyle pdfAnnotation value =
  sendMessage pdfAnnotation setEndLineStyleSelector value

-- | @- iconType@
iconType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFTextAnnotationIconType
iconType pdfAnnotation =
  sendMessage pdfAnnotation iconTypeSelector

-- | @- setIconType:@
setIconType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFTextAnnotationIconType -> IO ()
setIconType pdfAnnotation value =
  sendMessage pdfAnnotation setIconTypeSelector value

-- | @- quadrilateralPoints@
quadrilateralPoints :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
quadrilateralPoints pdfAnnotation =
  sendMessage pdfAnnotation quadrilateralPointsSelector

-- | @- setQuadrilateralPoints:@
setQuadrilateralPoints :: (IsPDFAnnotation pdfAnnotation, IsNSArray value) => pdfAnnotation -> value -> IO ()
setQuadrilateralPoints pdfAnnotation value =
  sendMessage pdfAnnotation setQuadrilateralPointsSelector (toNSArray value)

-- | @- markupType@
markupType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFMarkupType
markupType pdfAnnotation =
  sendMessage pdfAnnotation markupTypeSelector

-- | @- setMarkupType:@
setMarkupType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFMarkupType -> IO ()
setMarkupType pdfAnnotation value =
  sendMessage pdfAnnotation setMarkupTypeSelector value

-- | @- widgetControlType@
widgetControlType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFWidgetControlType
widgetControlType pdfAnnotation =
  sendMessage pdfAnnotation widgetControlTypeSelector

-- | @- setWidgetControlType:@
setWidgetControlType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFWidgetControlType -> IO ()
setWidgetControlType pdfAnnotation value =
  sendMessage pdfAnnotation setWidgetControlTypeSelector value

-- | @- multiline@
multiline :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
multiline pdfAnnotation =
  sendMessage pdfAnnotation multilineSelector

-- | @- setMultiline:@
setMultiline :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setMultiline pdfAnnotation value =
  sendMessage pdfAnnotation setMultilineSelector value

-- | @- activatableTextField@
activatableTextField :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
activatableTextField pdfAnnotation =
  sendMessage pdfAnnotation activatableTextFieldSelector

-- | @- isPasswordField@
isPasswordField :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
isPasswordField pdfAnnotation =
  sendMessage pdfAnnotation isPasswordFieldSelector

-- | @- comb@
comb :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
comb pdfAnnotation =
  sendMessage pdfAnnotation combSelector

-- | @- setComb:@
setComb :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setComb pdfAnnotation value =
  sendMessage pdfAnnotation setCombSelector value

-- | @- maximumLength@
maximumLength :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO CLong
maximumLength pdfAnnotation =
  sendMessage pdfAnnotation maximumLengthSelector

-- | @- setMaximumLength:@
setMaximumLength :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> CLong -> IO ()
setMaximumLength pdfAnnotation value =
  sendMessage pdfAnnotation setMaximumLengthSelector value

-- | @- widgetStringValue@
widgetStringValue :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
widgetStringValue pdfAnnotation =
  sendMessage pdfAnnotation widgetStringValueSelector

-- | @- setWidgetStringValue:@
setWidgetStringValue :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setWidgetStringValue pdfAnnotation value =
  sendMessage pdfAnnotation setWidgetStringValueSelector (toNSString value)

-- | @- widgetDefaultStringValue@
widgetDefaultStringValue :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
widgetDefaultStringValue pdfAnnotation =
  sendMessage pdfAnnotation widgetDefaultStringValueSelector

-- | @- setWidgetDefaultStringValue:@
setWidgetDefaultStringValue :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setWidgetDefaultStringValue pdfAnnotation value =
  sendMessage pdfAnnotation setWidgetDefaultStringValueSelector (toNSString value)

-- | @- allowsToggleToOff@
allowsToggleToOff :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
allowsToggleToOff pdfAnnotation =
  sendMessage pdfAnnotation allowsToggleToOffSelector

-- | @- setAllowsToggleToOff:@
setAllowsToggleToOff :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setAllowsToggleToOff pdfAnnotation value =
  sendMessage pdfAnnotation setAllowsToggleToOffSelector value

-- | @- radiosInUnison@
radiosInUnison :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
radiosInUnison pdfAnnotation =
  sendMessage pdfAnnotation radiosInUnisonSelector

-- | @- setRadiosInUnison:@
setRadiosInUnison :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setRadiosInUnison pdfAnnotation value =
  sendMessage pdfAnnotation setRadiosInUnisonSelector value

-- | @- readOnly@
readOnly :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
readOnly pdfAnnotation =
  sendMessage pdfAnnotation readOnlySelector

-- | @- setReadOnly:@
setReadOnly :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setReadOnly pdfAnnotation value =
  sendMessage pdfAnnotation setReadOnlySelector value

-- | @- listChoice@
listChoice :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
listChoice pdfAnnotation =
  sendMessage pdfAnnotation listChoiceSelector

-- | @- setListChoice:@
setListChoice :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setListChoice pdfAnnotation value =
  sendMessage pdfAnnotation setListChoiceSelector value

-- | @- choices@
choices :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
choices pdfAnnotation =
  sendMessage pdfAnnotation choicesSelector

-- | @- setChoices:@
setChoices :: (IsPDFAnnotation pdfAnnotation, IsNSArray value) => pdfAnnotation -> value -> IO ()
setChoices pdfAnnotation value =
  sendMessage pdfAnnotation setChoicesSelector (toNSArray value)

-- | @- values@
values :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
values pdfAnnotation =
  sendMessage pdfAnnotation valuesSelector

-- | @- setValues:@
setValues :: (IsPDFAnnotation pdfAnnotation, IsNSArray value) => pdfAnnotation -> value -> IO ()
setValues pdfAnnotation value =
  sendMessage pdfAnnotation setValuesSelector (toNSArray value)

-- | @- buttonWidgetState@
buttonWidgetState :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFWidgetCellState
buttonWidgetState pdfAnnotation =
  sendMessage pdfAnnotation buttonWidgetStateSelector

-- | @- setButtonWidgetState:@
setButtonWidgetState :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFWidgetCellState -> IO ()
setButtonWidgetState pdfAnnotation value =
  sendMessage pdfAnnotation setButtonWidgetStateSelector value

-- | @- buttonWidgetStateString@
buttonWidgetStateString :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
buttonWidgetStateString pdfAnnotation =
  sendMessage pdfAnnotation buttonWidgetStateStringSelector

-- | @- setButtonWidgetStateString:@
setButtonWidgetStateString :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setButtonWidgetStateString pdfAnnotation value =
  sendMessage pdfAnnotation setButtonWidgetStateStringSelector (toNSString value)

-- | @- open@
open :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
open pdfAnnotation =
  sendMessage pdfAnnotation openSelector

-- | @- setOpen:@
setOpen :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setOpen pdfAnnotation value =
  sendMessage pdfAnnotation setOpenSelector value

-- | @- paths@
paths :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
paths pdfAnnotation =
  sendMessage pdfAnnotation pathsSelector

-- | @- destination@
destination :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id PDFDestination)
destination pdfAnnotation =
  sendMessage pdfAnnotation destinationSelector

-- | @- setDestination:@
setDestination :: (IsPDFAnnotation pdfAnnotation, IsPDFDestination value) => pdfAnnotation -> value -> IO ()
setDestination pdfAnnotation value =
  sendMessage pdfAnnotation setDestinationSelector (toPDFDestination value)

-- | @- URL@
url :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSURL)
url pdfAnnotation =
  sendMessage pdfAnnotation urlSelector

-- | @- setURL:@
setURL :: (IsPDFAnnotation pdfAnnotation, IsNSURL value) => pdfAnnotation -> value -> IO ()
setURL pdfAnnotation value =
  sendMessage pdfAnnotation setURLSelector (toNSURL value)

-- | @- fieldName@
fieldName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
fieldName pdfAnnotation =
  sendMessage pdfAnnotation fieldNameSelector

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setFieldName pdfAnnotation value =
  sendMessage pdfAnnotation setFieldNameSelector (toNSString value)

-- | @- caption@
caption :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
caption pdfAnnotation =
  sendMessage pdfAnnotation captionSelector

-- | @- setCaption:@
setCaption :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setCaption pdfAnnotation value =
  sendMessage pdfAnnotation setCaptionSelector (toNSString value)

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSColor)
backgroundColor pdfAnnotation =
  sendMessage pdfAnnotation backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotation pdfAnnotation, IsNSColor value) => pdfAnnotation -> value -> IO ()
setBackgroundColor pdfAnnotation value =
  sendMessage pdfAnnotation setBackgroundColorSelector (toNSColor value)

-- | @- stampName@
stampName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
stampName pdfAnnotation =
  sendMessage pdfAnnotation stampNameSelector

-- | @- setStampName:@
setStampName :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setStampName pdfAnnotation value =
  sendMessage pdfAnnotation setStampNameSelector (toNSString value)

-- | @- toolTip@
toolTip :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
toolTip pdfAnnotation =
  sendMessage pdfAnnotation toolTipSelector

-- | @- mouseUpAction@
mouseUpAction :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
mouseUpAction pdfAnnotation =
  sendMessage pdfAnnotation mouseUpActionSelector

-- | @- setMouseUpAction:@
setMouseUpAction :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setMouseUpAction pdfAnnotation value =
  sendMessage pdfAnnotation setMouseUpActionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawWithBox:inContext:@
drawWithBox_inContextSelector :: Selector '[PDFDisplayBox, Ptr ()] ()
drawWithBox_inContextSelector = mkSelector "drawWithBox:inContext:"

-- | @Selector@ for @lineStyleFromName:@
lineStyleFromNameSelector :: Selector '[Id NSString] PDFLineStyle
lineStyleFromNameSelector = mkSelector "lineStyleFromName:"

-- | @Selector@ for @nameForLineStyle:@
nameForLineStyleSelector :: Selector '[PDFLineStyle] (Id NSString)
nameForLineStyleSelector = mkSelector "nameForLineStyle:"

-- | @Selector@ for @addBezierPath:@
addBezierPathSelector :: Selector '[Id NSBezierPath] ()
addBezierPathSelector = mkSelector "addBezierPath:"

-- | @Selector@ for @removeBezierPath:@
removeBezierPathSelector :: Selector '[Id NSBezierPath] ()
removeBezierPathSelector = mkSelector "removeBezierPath:"

-- | @Selector@ for @initWithDictionary:forPage:@
initWithDictionary_forPageSelector :: Selector '[Id NSDictionary, Id PDFPage] (Id PDFAnnotation)
initWithDictionary_forPageSelector = mkSelector "initWithDictionary:forPage:"

-- | @Selector@ for @initWithBounds:@
initWithBoundsSelector :: Selector '[NSRect] (Id PDFAnnotation)
initWithBoundsSelector = mkSelector "initWithBounds:"

-- | @Selector@ for @removeAllAppearanceStreams@
removeAllAppearanceStreamsSelector :: Selector '[] ()
removeAllAppearanceStreamsSelector = mkSelector "removeAllAppearanceStreams"

-- | @Selector@ for @drawWithBox:@
drawWithBoxSelector :: Selector '[PDFDisplayBox] ()
drawWithBoxSelector = mkSelector "drawWithBox:"

-- | @Selector@ for @page@
pageSelector :: Selector '[] (Id PDFPage)
pageSelector = mkSelector "page"

-- | @Selector@ for @setPage:@
setPageSelector :: Selector '[Id PDFPage] ()
setPageSelector = mkSelector "setPage:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @bounds@
boundsSelector :: Selector '[] NSRect
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @setBounds:@
setBoundsSelector :: Selector '[NSRect] ()
setBoundsSelector = mkSelector "setBounds:"

-- | @Selector@ for @shouldDisplay@
shouldDisplaySelector :: Selector '[] Bool
shouldDisplaySelector = mkSelector "shouldDisplay"

-- | @Selector@ for @setShouldDisplay:@
setShouldDisplaySelector :: Selector '[Bool] ()
setShouldDisplaySelector = mkSelector "setShouldDisplay:"

-- | @Selector@ for @shouldPrint@
shouldPrintSelector :: Selector '[] Bool
shouldPrintSelector = mkSelector "shouldPrint"

-- | @Selector@ for @setShouldPrint:@
setShouldPrintSelector :: Selector '[Bool] ()
setShouldPrintSelector = mkSelector "setShouldPrint:"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] RawId
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @setModificationDate:@
setModificationDateSelector :: Selector '[RawId] ()
setModificationDateSelector = mkSelector "setModificationDate:"

-- | @Selector@ for @userName@
userNameSelector :: Selector '[] RawId
userNameSelector = mkSelector "userName"

-- | @Selector@ for @setUserName:@
setUserNameSelector :: Selector '[RawId] ()
setUserNameSelector = mkSelector "setUserName:"

-- | @Selector@ for @popup@
popupSelector :: Selector '[] RawId
popupSelector = mkSelector "popup"

-- | @Selector@ for @setPopup:@
setPopupSelector :: Selector '[RawId] ()
setPopupSelector = mkSelector "setPopup:"

-- | @Selector@ for @border@
borderSelector :: Selector '[] RawId
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector '[RawId] ()
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] RawId
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[RawId] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] RawId
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[RawId] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] RawId
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[RawId] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @hasAppearanceStream@
hasAppearanceStreamSelector :: Selector '[] Bool
hasAppearanceStreamSelector = mkSelector "hasAppearanceStream"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector '[] Bool
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector '[Bool] ()
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @annotationKeyValues@
annotationKeyValuesSelector :: Selector '[] RawId
annotationKeyValuesSelector = mkSelector "annotationKeyValues"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector '[] (Id NSColor)
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector '[Id NSColor] ()
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector '[] (Id NSColor)
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector '[Id NSColor] ()
setInteriorColorSelector = mkSelector "setInteriorColor:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSTextAlignment] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @startPoint@
startPointSelector :: Selector '[] NSPoint
startPointSelector = mkSelector "startPoint"

-- | @Selector@ for @setStartPoint:@
setStartPointSelector :: Selector '[NSPoint] ()
setStartPointSelector = mkSelector "setStartPoint:"

-- | @Selector@ for @endPoint@
endPointSelector :: Selector '[] NSPoint
endPointSelector = mkSelector "endPoint"

-- | @Selector@ for @setEndPoint:@
setEndPointSelector :: Selector '[NSPoint] ()
setEndPointSelector = mkSelector "setEndPoint:"

-- | @Selector@ for @startLineStyle@
startLineStyleSelector :: Selector '[] PDFLineStyle
startLineStyleSelector = mkSelector "startLineStyle"

-- | @Selector@ for @setStartLineStyle:@
setStartLineStyleSelector :: Selector '[PDFLineStyle] ()
setStartLineStyleSelector = mkSelector "setStartLineStyle:"

-- | @Selector@ for @endLineStyle@
endLineStyleSelector :: Selector '[] PDFLineStyle
endLineStyleSelector = mkSelector "endLineStyle"

-- | @Selector@ for @setEndLineStyle:@
setEndLineStyleSelector :: Selector '[PDFLineStyle] ()
setEndLineStyleSelector = mkSelector "setEndLineStyle:"

-- | @Selector@ for @iconType@
iconTypeSelector :: Selector '[] PDFTextAnnotationIconType
iconTypeSelector = mkSelector "iconType"

-- | @Selector@ for @setIconType:@
setIconTypeSelector :: Selector '[PDFTextAnnotationIconType] ()
setIconTypeSelector = mkSelector "setIconType:"

-- | @Selector@ for @quadrilateralPoints@
quadrilateralPointsSelector :: Selector '[] (Id NSArray)
quadrilateralPointsSelector = mkSelector "quadrilateralPoints"

-- | @Selector@ for @setQuadrilateralPoints:@
setQuadrilateralPointsSelector :: Selector '[Id NSArray] ()
setQuadrilateralPointsSelector = mkSelector "setQuadrilateralPoints:"

-- | @Selector@ for @markupType@
markupTypeSelector :: Selector '[] PDFMarkupType
markupTypeSelector = mkSelector "markupType"

-- | @Selector@ for @setMarkupType:@
setMarkupTypeSelector :: Selector '[PDFMarkupType] ()
setMarkupTypeSelector = mkSelector "setMarkupType:"

-- | @Selector@ for @widgetControlType@
widgetControlTypeSelector :: Selector '[] PDFWidgetControlType
widgetControlTypeSelector = mkSelector "widgetControlType"

-- | @Selector@ for @setWidgetControlType:@
setWidgetControlTypeSelector :: Selector '[PDFWidgetControlType] ()
setWidgetControlTypeSelector = mkSelector "setWidgetControlType:"

-- | @Selector@ for @multiline@
multilineSelector :: Selector '[] Bool
multilineSelector = mkSelector "multiline"

-- | @Selector@ for @setMultiline:@
setMultilineSelector :: Selector '[Bool] ()
setMultilineSelector = mkSelector "setMultiline:"

-- | @Selector@ for @activatableTextField@
activatableTextFieldSelector :: Selector '[] Bool
activatableTextFieldSelector = mkSelector "activatableTextField"

-- | @Selector@ for @isPasswordField@
isPasswordFieldSelector :: Selector '[] Bool
isPasswordFieldSelector = mkSelector "isPasswordField"

-- | @Selector@ for @comb@
combSelector :: Selector '[] Bool
combSelector = mkSelector "comb"

-- | @Selector@ for @setComb:@
setCombSelector :: Selector '[Bool] ()
setCombSelector = mkSelector "setComb:"

-- | @Selector@ for @maximumLength@
maximumLengthSelector :: Selector '[] CLong
maximumLengthSelector = mkSelector "maximumLength"

-- | @Selector@ for @setMaximumLength:@
setMaximumLengthSelector :: Selector '[CLong] ()
setMaximumLengthSelector = mkSelector "setMaximumLength:"

-- | @Selector@ for @widgetStringValue@
widgetStringValueSelector :: Selector '[] (Id NSString)
widgetStringValueSelector = mkSelector "widgetStringValue"

-- | @Selector@ for @setWidgetStringValue:@
setWidgetStringValueSelector :: Selector '[Id NSString] ()
setWidgetStringValueSelector = mkSelector "setWidgetStringValue:"

-- | @Selector@ for @widgetDefaultStringValue@
widgetDefaultStringValueSelector :: Selector '[] (Id NSString)
widgetDefaultStringValueSelector = mkSelector "widgetDefaultStringValue"

-- | @Selector@ for @setWidgetDefaultStringValue:@
setWidgetDefaultStringValueSelector :: Selector '[Id NSString] ()
setWidgetDefaultStringValueSelector = mkSelector "setWidgetDefaultStringValue:"

-- | @Selector@ for @allowsToggleToOff@
allowsToggleToOffSelector :: Selector '[] Bool
allowsToggleToOffSelector = mkSelector "allowsToggleToOff"

-- | @Selector@ for @setAllowsToggleToOff:@
setAllowsToggleToOffSelector :: Selector '[Bool] ()
setAllowsToggleToOffSelector = mkSelector "setAllowsToggleToOff:"

-- | @Selector@ for @radiosInUnison@
radiosInUnisonSelector :: Selector '[] Bool
radiosInUnisonSelector = mkSelector "radiosInUnison"

-- | @Selector@ for @setRadiosInUnison:@
setRadiosInUnisonSelector :: Selector '[Bool] ()
setRadiosInUnisonSelector = mkSelector "setRadiosInUnison:"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector '[Bool] ()
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @listChoice@
listChoiceSelector :: Selector '[] Bool
listChoiceSelector = mkSelector "listChoice"

-- | @Selector@ for @setListChoice:@
setListChoiceSelector :: Selector '[Bool] ()
setListChoiceSelector = mkSelector "setListChoice:"

-- | @Selector@ for @choices@
choicesSelector :: Selector '[] (Id NSArray)
choicesSelector = mkSelector "choices"

-- | @Selector@ for @setChoices:@
setChoicesSelector :: Selector '[Id NSArray] ()
setChoicesSelector = mkSelector "setChoices:"

-- | @Selector@ for @values@
valuesSelector :: Selector '[] (Id NSArray)
valuesSelector = mkSelector "values"

-- | @Selector@ for @setValues:@
setValuesSelector :: Selector '[Id NSArray] ()
setValuesSelector = mkSelector "setValues:"

-- | @Selector@ for @buttonWidgetState@
buttonWidgetStateSelector :: Selector '[] PDFWidgetCellState
buttonWidgetStateSelector = mkSelector "buttonWidgetState"

-- | @Selector@ for @setButtonWidgetState:@
setButtonWidgetStateSelector :: Selector '[PDFWidgetCellState] ()
setButtonWidgetStateSelector = mkSelector "setButtonWidgetState:"

-- | @Selector@ for @buttonWidgetStateString@
buttonWidgetStateStringSelector :: Selector '[] (Id NSString)
buttonWidgetStateStringSelector = mkSelector "buttonWidgetStateString"

-- | @Selector@ for @setButtonWidgetStateString:@
setButtonWidgetStateStringSelector :: Selector '[Id NSString] ()
setButtonWidgetStateStringSelector = mkSelector "setButtonWidgetStateString:"

-- | @Selector@ for @open@
openSelector :: Selector '[] Bool
openSelector = mkSelector "open"

-- | @Selector@ for @setOpen:@
setOpenSelector :: Selector '[Bool] ()
setOpenSelector = mkSelector "setOpen:"

-- | @Selector@ for @paths@
pathsSelector :: Selector '[] (Id NSArray)
pathsSelector = mkSelector "paths"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id PDFDestination)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[Id PDFDestination] ()
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector '[] (Id NSString)
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector '[Id NSString] ()
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @caption@
captionSelector :: Selector '[] (Id NSString)
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector '[Id NSString] ()
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @stampName@
stampNameSelector :: Selector '[] (Id NSString)
stampNameSelector = mkSelector "stampName"

-- | @Selector@ for @setStampName:@
setStampNameSelector :: Selector '[Id NSString] ()
setStampNameSelector = mkSelector "setStampName:"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] RawId
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @mouseUpAction@
mouseUpActionSelector :: Selector '[] RawId
mouseUpActionSelector = mkSelector "mouseUpAction"

-- | @Selector@ for @setMouseUpAction:@
setMouseUpActionSelector :: Selector '[RawId] ()
setMouseUpActionSelector = mkSelector "setMouseUpAction:"

