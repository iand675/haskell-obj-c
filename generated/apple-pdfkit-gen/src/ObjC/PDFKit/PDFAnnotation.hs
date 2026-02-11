{-# LANGUAGE PatternSynonyms #-}
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
  , drawWithBox_inContextSelector
  , lineStyleFromNameSelector
  , nameForLineStyleSelector
  , addBezierPathSelector
  , removeBezierPathSelector
  , initWithDictionary_forPageSelector
  , initWithBoundsSelector
  , removeAllAppearanceStreamsSelector
  , drawWithBoxSelector
  , pageSelector
  , setPageSelector
  , typeSelector
  , setTypeSelector
  , boundsSelector
  , setBoundsSelector
  , shouldDisplaySelector
  , setShouldDisplaySelector
  , shouldPrintSelector
  , setShouldPrintSelector
  , modificationDateSelector
  , setModificationDateSelector
  , userNameSelector
  , setUserNameSelector
  , popupSelector
  , setPopupSelector
  , borderSelector
  , setBorderSelector
  , colorSelector
  , setColorSelector
  , contentsSelector
  , setContentsSelector
  , actionSelector
  , setActionSelector
  , hasAppearanceStreamSelector
  , highlightedSelector
  , setHighlightedSelector
  , annotationKeyValuesSelector
  , fontSelector
  , setFontSelector
  , fontColorSelector
  , setFontColorSelector
  , interiorColorSelector
  , setInteriorColorSelector
  , alignmentSelector
  , setAlignmentSelector
  , startPointSelector
  , setStartPointSelector
  , endPointSelector
  , setEndPointSelector
  , startLineStyleSelector
  , setStartLineStyleSelector
  , endLineStyleSelector
  , setEndLineStyleSelector
  , iconTypeSelector
  , setIconTypeSelector
  , quadrilateralPointsSelector
  , setQuadrilateralPointsSelector
  , markupTypeSelector
  , setMarkupTypeSelector
  , widgetControlTypeSelector
  , setWidgetControlTypeSelector
  , multilineSelector
  , setMultilineSelector
  , activatableTextFieldSelector
  , isPasswordFieldSelector
  , combSelector
  , setCombSelector
  , maximumLengthSelector
  , setMaximumLengthSelector
  , widgetStringValueSelector
  , setWidgetStringValueSelector
  , widgetDefaultStringValueSelector
  , setWidgetDefaultStringValueSelector
  , allowsToggleToOffSelector
  , setAllowsToggleToOffSelector
  , radiosInUnisonSelector
  , setRadiosInUnisonSelector
  , readOnlySelector
  , setReadOnlySelector
  , listChoiceSelector
  , setListChoiceSelector
  , choicesSelector
  , setChoicesSelector
  , valuesSelector
  , setValuesSelector
  , buttonWidgetStateSelector
  , setButtonWidgetStateSelector
  , buttonWidgetStateStringSelector
  , setButtonWidgetStateStringSelector
  , openSelector
  , setOpenSelector
  , pathsSelector
  , destinationSelector
  , setDestinationSelector
  , urlSelector
  , setURLSelector
  , fieldNameSelector
  , setFieldNameSelector
  , captionSelector
  , setCaptionSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , stampNameSelector
  , setStampNameSelector
  , toolTipSelector
  , mouseUpActionSelector
  , setMouseUpActionSelector

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

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- drawWithBox:inContext:@
drawWithBox_inContext :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFDisplayBox -> Ptr () -> IO ()
drawWithBox_inContext pdfAnnotation  box context =
    sendMsg pdfAnnotation (mkSelector "drawWithBox:inContext:") retVoid [argCLong (coerce box), argPtr context]

-- | @+ lineStyleFromName:@
lineStyleFromName :: IsNSString name => name -> IO PDFLineStyle
lineStyleFromName name =
  do
    cls' <- getRequiredClass "PDFAnnotation"
    withObjCPtr name $ \raw_name ->
      fmap (coerce :: CLong -> PDFLineStyle) $ sendClassMsg cls' (mkSelector "lineStyleFromName:") retCLong [argPtr (castPtr raw_name :: Ptr ())]

-- | @+ nameForLineStyle:@
nameForLineStyle :: PDFLineStyle -> IO (Id NSString)
nameForLineStyle style =
  do
    cls' <- getRequiredClass "PDFAnnotation"
    sendClassMsg cls' (mkSelector "nameForLineStyle:") (retPtr retVoid) [argCLong (coerce style)] >>= retainedObject . castPtr

-- | @- addBezierPath:@
addBezierPath :: (IsPDFAnnotation pdfAnnotation, IsNSBezierPath path) => pdfAnnotation -> path -> IO ()
addBezierPath pdfAnnotation  path =
  withObjCPtr path $ \raw_path ->
      sendMsg pdfAnnotation (mkSelector "addBezierPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- removeBezierPath:@
removeBezierPath :: (IsPDFAnnotation pdfAnnotation, IsNSBezierPath path) => pdfAnnotation -> path -> IO ()
removeBezierPath pdfAnnotation  path =
  withObjCPtr path $ \raw_path ->
      sendMsg pdfAnnotation (mkSelector "removeBezierPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initWithDictionary:forPage:@
initWithDictionary_forPage :: (IsPDFAnnotation pdfAnnotation, IsNSDictionary dictionary, IsPDFPage page) => pdfAnnotation -> dictionary -> page -> IO (Id PDFAnnotation)
initWithDictionary_forPage pdfAnnotation  dictionary page =
  withObjCPtr dictionary $ \raw_dictionary ->
    withObjCPtr page $ \raw_page ->
        sendMsg pdfAnnotation (mkSelector "initWithDictionary:forPage:") (retPtr retVoid) [argPtr (castPtr raw_dictionary :: Ptr ()), argPtr (castPtr raw_page :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithBounds:@
initWithBounds :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSRect -> IO (Id PDFAnnotation)
initWithBounds pdfAnnotation  bounds =
    sendMsg pdfAnnotation (mkSelector "initWithBounds:") (retPtr retVoid) [argNSRect bounds] >>= ownedObject . castPtr

-- | @- removeAllAppearanceStreams@
removeAllAppearanceStreams :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO ()
removeAllAppearanceStreams pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "removeAllAppearanceStreams") retVoid []

-- | @- drawWithBox:@
drawWithBox :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFDisplayBox -> IO ()
drawWithBox pdfAnnotation  box =
    sendMsg pdfAnnotation (mkSelector "drawWithBox:") retVoid [argCLong (coerce box)]

-- | @- page@
page :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id PDFPage)
page pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "page") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPage:@
setPage :: (IsPDFAnnotation pdfAnnotation, IsPDFPage value) => pdfAnnotation -> value -> IO ()
setPage pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setPage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
type_ pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setType pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bounds@
bounds :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSRect
bounds pdfAnnotation  =
    sendMsgStret pdfAnnotation (mkSelector "bounds") retNSRect []

-- | @- setBounds:@
setBounds :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSRect -> IO ()
setBounds pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setBounds:") retVoid [argNSRect value]

-- | @- shouldDisplay@
shouldDisplay :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
shouldDisplay pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "shouldDisplay") retCULong []

-- | @- setShouldDisplay:@
setShouldDisplay :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setShouldDisplay pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setShouldDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldPrint@
shouldPrint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
shouldPrint pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "shouldPrint") retCULong []

-- | @- setShouldPrint:@
setShouldPrint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setShouldPrint pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setShouldPrint:") retVoid [argCULong (if value then 1 else 0)]

-- | @- modificationDate@
modificationDate :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
modificationDate pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "modificationDate") (retPtr retVoid) []

-- | @- setModificationDate:@
setModificationDate :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setModificationDate pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setModificationDate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- userName@
userName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
userName pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "userName") (retPtr retVoid) []

-- | @- setUserName:@
setUserName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setUserName pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setUserName:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- popup@
popup :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
popup pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "popup") (retPtr retVoid) []

-- | @- setPopup:@
setPopup :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setPopup pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setPopup:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- border@
border :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
border pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "border") (retPtr retVoid) []

-- | @- setBorder:@
setBorder :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setBorder pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setBorder:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- color@
color :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
color pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "color") (retPtr retVoid) []

-- | @- setColor:@
setColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setColor pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setColor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- contents@
contents :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
contents pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "contents") (retPtr retVoid) []

-- | @- setContents:@
setContents :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setContents pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setContents:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
action pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setAction pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setAction:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- hasAppearanceStream@
hasAppearanceStream :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
hasAppearanceStream pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "hasAppearanceStream") retCULong []

-- | @- highlighted@
highlighted :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
highlighted pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "highlighted") retCULong []

-- | @- setHighlighted:@
setHighlighted :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setHighlighted pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setHighlighted:") retVoid [argCULong (if value then 1 else 0)]

-- | @- annotationKeyValues@
annotationKeyValues :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
annotationKeyValues pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "annotationKeyValues") (retPtr retVoid) []

-- | @- font@
font :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSFont)
font pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsPDFAnnotation pdfAnnotation, IsNSFont value) => pdfAnnotation -> value -> IO ()
setFont pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fontColor@
fontColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSColor)
fontColor pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "fontColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotation pdfAnnotation, IsNSColor value) => pdfAnnotation -> value -> IO ()
setFontColor pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setFontColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- interiorColor@
interiorColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSColor)
interiorColor pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "interiorColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotation pdfAnnotation, IsNSColor value) => pdfAnnotation -> value -> IO ()
setInteriorColor pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setInteriorColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alignment@
alignment :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSTextAlignment
alignment pdfAnnotation  =
    fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg pdfAnnotation (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSTextAlignment -> IO ()
setAlignment pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setAlignment:") retVoid [argCLong (coerce value)]

-- | @- startPoint@
startPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSPoint
startPoint pdfAnnotation  =
    sendMsgStret pdfAnnotation (mkSelector "startPoint") retNSPoint []

-- | @- setStartPoint:@
setStartPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSPoint -> IO ()
setStartPoint pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setStartPoint:") retVoid [argNSPoint value]

-- | @- endPoint@
endPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO NSPoint
endPoint pdfAnnotation  =
    sendMsgStret pdfAnnotation (mkSelector "endPoint") retNSPoint []

-- | @- setEndPoint:@
setEndPoint :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> NSPoint -> IO ()
setEndPoint pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setEndPoint:") retVoid [argNSPoint value]

-- | @- startLineStyle@
startLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFLineStyle
startLineStyle pdfAnnotation  =
    fmap (coerce :: CLong -> PDFLineStyle) $ sendMsg pdfAnnotation (mkSelector "startLineStyle") retCLong []

-- | @- setStartLineStyle:@
setStartLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFLineStyle -> IO ()
setStartLineStyle pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setStartLineStyle:") retVoid [argCLong (coerce value)]

-- | @- endLineStyle@
endLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFLineStyle
endLineStyle pdfAnnotation  =
    fmap (coerce :: CLong -> PDFLineStyle) $ sendMsg pdfAnnotation (mkSelector "endLineStyle") retCLong []

-- | @- setEndLineStyle:@
setEndLineStyle :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFLineStyle -> IO ()
setEndLineStyle pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setEndLineStyle:") retVoid [argCLong (coerce value)]

-- | @- iconType@
iconType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFTextAnnotationIconType
iconType pdfAnnotation  =
    fmap (coerce :: CLong -> PDFTextAnnotationIconType) $ sendMsg pdfAnnotation (mkSelector "iconType") retCLong []

-- | @- setIconType:@
setIconType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFTextAnnotationIconType -> IO ()
setIconType pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setIconType:") retVoid [argCLong (coerce value)]

-- | @- quadrilateralPoints@
quadrilateralPoints :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
quadrilateralPoints pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "quadrilateralPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuadrilateralPoints:@
setQuadrilateralPoints :: (IsPDFAnnotation pdfAnnotation, IsNSArray value) => pdfAnnotation -> value -> IO ()
setQuadrilateralPoints pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setQuadrilateralPoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- markupType@
markupType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFMarkupType
markupType pdfAnnotation  =
    fmap (coerce :: CLong -> PDFMarkupType) $ sendMsg pdfAnnotation (mkSelector "markupType") retCLong []

-- | @- setMarkupType:@
setMarkupType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFMarkupType -> IO ()
setMarkupType pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setMarkupType:") retVoid [argCLong (coerce value)]

-- | @- widgetControlType@
widgetControlType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFWidgetControlType
widgetControlType pdfAnnotation  =
    fmap (coerce :: CLong -> PDFWidgetControlType) $ sendMsg pdfAnnotation (mkSelector "widgetControlType") retCLong []

-- | @- setWidgetControlType:@
setWidgetControlType :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFWidgetControlType -> IO ()
setWidgetControlType pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setWidgetControlType:") retVoid [argCLong (coerce value)]

-- | @- multiline@
multiline :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
multiline pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "multiline") retCULong []

-- | @- setMultiline:@
setMultiline :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setMultiline pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setMultiline:") retVoid [argCULong (if value then 1 else 0)]

-- | @- activatableTextField@
activatableTextField :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
activatableTextField pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "activatableTextField") retCULong []

-- | @- isPasswordField@
isPasswordField :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
isPasswordField pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "isPasswordField") retCULong []

-- | @- comb@
comb :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
comb pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "comb") retCULong []

-- | @- setComb:@
setComb :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setComb pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setComb:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maximumLength@
maximumLength :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO CLong
maximumLength pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "maximumLength") retCLong []

-- | @- setMaximumLength:@
setMaximumLength :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> CLong -> IO ()
setMaximumLength pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setMaximumLength:") retVoid [argCLong value]

-- | @- widgetStringValue@
widgetStringValue :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
widgetStringValue pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "widgetStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidgetStringValue:@
setWidgetStringValue :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setWidgetStringValue pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setWidgetStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- widgetDefaultStringValue@
widgetDefaultStringValue :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
widgetDefaultStringValue pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "widgetDefaultStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidgetDefaultStringValue:@
setWidgetDefaultStringValue :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setWidgetDefaultStringValue pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setWidgetDefaultStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsToggleToOff@
allowsToggleToOff :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
allowsToggleToOff pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "allowsToggleToOff") retCULong []

-- | @- setAllowsToggleToOff:@
setAllowsToggleToOff :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setAllowsToggleToOff pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setAllowsToggleToOff:") retVoid [argCULong (if value then 1 else 0)]

-- | @- radiosInUnison@
radiosInUnison :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
radiosInUnison pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "radiosInUnison") retCULong []

-- | @- setRadiosInUnison:@
setRadiosInUnison :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setRadiosInUnison pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setRadiosInUnison:") retVoid [argCULong (if value then 1 else 0)]

-- | @- readOnly@
readOnly :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
readOnly pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "readOnly") retCULong []

-- | @- setReadOnly:@
setReadOnly :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setReadOnly pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setReadOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- listChoice@
listChoice :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
listChoice pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "listChoice") retCULong []

-- | @- setListChoice:@
setListChoice :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setListChoice pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setListChoice:") retVoid [argCULong (if value then 1 else 0)]

-- | @- choices@
choices :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
choices pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "choices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChoices:@
setChoices :: (IsPDFAnnotation pdfAnnotation, IsNSArray value) => pdfAnnotation -> value -> IO ()
setChoices pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setChoices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- values@
values :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
values pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "values") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValues:@
setValues :: (IsPDFAnnotation pdfAnnotation, IsNSArray value) => pdfAnnotation -> value -> IO ()
setValues pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- buttonWidgetState@
buttonWidgetState :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO PDFWidgetCellState
buttonWidgetState pdfAnnotation  =
    fmap (coerce :: CLong -> PDFWidgetCellState) $ sendMsg pdfAnnotation (mkSelector "buttonWidgetState") retCLong []

-- | @- setButtonWidgetState:@
setButtonWidgetState :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> PDFWidgetCellState -> IO ()
setButtonWidgetState pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setButtonWidgetState:") retVoid [argCLong (coerce value)]

-- | @- buttonWidgetStateString@
buttonWidgetStateString :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
buttonWidgetStateString pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "buttonWidgetStateString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setButtonWidgetStateString:@
setButtonWidgetStateString :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setButtonWidgetStateString pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setButtonWidgetStateString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- open@
open :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO Bool
open pdfAnnotation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotation (mkSelector "open") retCULong []

-- | @- setOpen:@
setOpen :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> Bool -> IO ()
setOpen pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setOpen:") retVoid [argCULong (if value then 1 else 0)]

-- | @- paths@
paths :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSArray)
paths pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "paths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destination@
destination :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id PDFDestination)
destination pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestination:@
setDestination :: (IsPDFAnnotation pdfAnnotation, IsPDFDestination value) => pdfAnnotation -> value -> IO ()
setDestination pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setDestination:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URL@
url :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSURL)
url pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsPDFAnnotation pdfAnnotation, IsNSURL value) => pdfAnnotation -> value -> IO ()
setURL pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fieldName@
fieldName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
fieldName pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "fieldName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setFieldName pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setFieldName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caption@
caption :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
caption pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "caption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaption:@
setCaption :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setCaption pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setCaption:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSColor)
backgroundColor pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotation pdfAnnotation, IsNSColor value) => pdfAnnotation -> value -> IO ()
setBackgroundColor pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stampName@
stampName :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO (Id NSString)
stampName pdfAnnotation  =
    sendMsg pdfAnnotation (mkSelector "stampName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStampName:@
setStampName :: (IsPDFAnnotation pdfAnnotation, IsNSString value) => pdfAnnotation -> value -> IO ()
setStampName pdfAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pdfAnnotation (mkSelector "setStampName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- toolTip@
toolTip :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
toolTip pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "toolTip") (retPtr retVoid) []

-- | @- mouseUpAction@
mouseUpAction :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> IO RawId
mouseUpAction pdfAnnotation  =
    fmap (RawId . castPtr) $ sendMsg pdfAnnotation (mkSelector "mouseUpAction") (retPtr retVoid) []

-- | @- setMouseUpAction:@
setMouseUpAction :: IsPDFAnnotation pdfAnnotation => pdfAnnotation -> RawId -> IO ()
setMouseUpAction pdfAnnotation  value =
    sendMsg pdfAnnotation (mkSelector "setMouseUpAction:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawWithBox:inContext:@
drawWithBox_inContextSelector :: Selector
drawWithBox_inContextSelector = mkSelector "drawWithBox:inContext:"

-- | @Selector@ for @lineStyleFromName:@
lineStyleFromNameSelector :: Selector
lineStyleFromNameSelector = mkSelector "lineStyleFromName:"

-- | @Selector@ for @nameForLineStyle:@
nameForLineStyleSelector :: Selector
nameForLineStyleSelector = mkSelector "nameForLineStyle:"

-- | @Selector@ for @addBezierPath:@
addBezierPathSelector :: Selector
addBezierPathSelector = mkSelector "addBezierPath:"

-- | @Selector@ for @removeBezierPath:@
removeBezierPathSelector :: Selector
removeBezierPathSelector = mkSelector "removeBezierPath:"

-- | @Selector@ for @initWithDictionary:forPage:@
initWithDictionary_forPageSelector :: Selector
initWithDictionary_forPageSelector = mkSelector "initWithDictionary:forPage:"

-- | @Selector@ for @initWithBounds:@
initWithBoundsSelector :: Selector
initWithBoundsSelector = mkSelector "initWithBounds:"

-- | @Selector@ for @removeAllAppearanceStreams@
removeAllAppearanceStreamsSelector :: Selector
removeAllAppearanceStreamsSelector = mkSelector "removeAllAppearanceStreams"

-- | @Selector@ for @drawWithBox:@
drawWithBoxSelector :: Selector
drawWithBoxSelector = mkSelector "drawWithBox:"

-- | @Selector@ for @page@
pageSelector :: Selector
pageSelector = mkSelector "page"

-- | @Selector@ for @setPage:@
setPageSelector :: Selector
setPageSelector = mkSelector "setPage:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @bounds@
boundsSelector :: Selector
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @setBounds:@
setBoundsSelector :: Selector
setBoundsSelector = mkSelector "setBounds:"

-- | @Selector@ for @shouldDisplay@
shouldDisplaySelector :: Selector
shouldDisplaySelector = mkSelector "shouldDisplay"

-- | @Selector@ for @setShouldDisplay:@
setShouldDisplaySelector :: Selector
setShouldDisplaySelector = mkSelector "setShouldDisplay:"

-- | @Selector@ for @shouldPrint@
shouldPrintSelector :: Selector
shouldPrintSelector = mkSelector "shouldPrint"

-- | @Selector@ for @setShouldPrint:@
setShouldPrintSelector :: Selector
setShouldPrintSelector = mkSelector "setShouldPrint:"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @setModificationDate:@
setModificationDateSelector :: Selector
setModificationDateSelector = mkSelector "setModificationDate:"

-- | @Selector@ for @userName@
userNameSelector :: Selector
userNameSelector = mkSelector "userName"

-- | @Selector@ for @setUserName:@
setUserNameSelector :: Selector
setUserNameSelector = mkSelector "setUserName:"

-- | @Selector@ for @popup@
popupSelector :: Selector
popupSelector = mkSelector "popup"

-- | @Selector@ for @setPopup:@
setPopupSelector :: Selector
setPopupSelector = mkSelector "setPopup:"

-- | @Selector@ for @border@
borderSelector :: Selector
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @hasAppearanceStream@
hasAppearanceStreamSelector :: Selector
hasAppearanceStreamSelector = mkSelector "hasAppearanceStream"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @annotationKeyValues@
annotationKeyValuesSelector :: Selector
annotationKeyValuesSelector = mkSelector "annotationKeyValues"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector
setInteriorColorSelector = mkSelector "setInteriorColor:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @startPoint@
startPointSelector :: Selector
startPointSelector = mkSelector "startPoint"

-- | @Selector@ for @setStartPoint:@
setStartPointSelector :: Selector
setStartPointSelector = mkSelector "setStartPoint:"

-- | @Selector@ for @endPoint@
endPointSelector :: Selector
endPointSelector = mkSelector "endPoint"

-- | @Selector@ for @setEndPoint:@
setEndPointSelector :: Selector
setEndPointSelector = mkSelector "setEndPoint:"

-- | @Selector@ for @startLineStyle@
startLineStyleSelector :: Selector
startLineStyleSelector = mkSelector "startLineStyle"

-- | @Selector@ for @setStartLineStyle:@
setStartLineStyleSelector :: Selector
setStartLineStyleSelector = mkSelector "setStartLineStyle:"

-- | @Selector@ for @endLineStyle@
endLineStyleSelector :: Selector
endLineStyleSelector = mkSelector "endLineStyle"

-- | @Selector@ for @setEndLineStyle:@
setEndLineStyleSelector :: Selector
setEndLineStyleSelector = mkSelector "setEndLineStyle:"

-- | @Selector@ for @iconType@
iconTypeSelector :: Selector
iconTypeSelector = mkSelector "iconType"

-- | @Selector@ for @setIconType:@
setIconTypeSelector :: Selector
setIconTypeSelector = mkSelector "setIconType:"

-- | @Selector@ for @quadrilateralPoints@
quadrilateralPointsSelector :: Selector
quadrilateralPointsSelector = mkSelector "quadrilateralPoints"

-- | @Selector@ for @setQuadrilateralPoints:@
setQuadrilateralPointsSelector :: Selector
setQuadrilateralPointsSelector = mkSelector "setQuadrilateralPoints:"

-- | @Selector@ for @markupType@
markupTypeSelector :: Selector
markupTypeSelector = mkSelector "markupType"

-- | @Selector@ for @setMarkupType:@
setMarkupTypeSelector :: Selector
setMarkupTypeSelector = mkSelector "setMarkupType:"

-- | @Selector@ for @widgetControlType@
widgetControlTypeSelector :: Selector
widgetControlTypeSelector = mkSelector "widgetControlType"

-- | @Selector@ for @setWidgetControlType:@
setWidgetControlTypeSelector :: Selector
setWidgetControlTypeSelector = mkSelector "setWidgetControlType:"

-- | @Selector@ for @multiline@
multilineSelector :: Selector
multilineSelector = mkSelector "multiline"

-- | @Selector@ for @setMultiline:@
setMultilineSelector :: Selector
setMultilineSelector = mkSelector "setMultiline:"

-- | @Selector@ for @activatableTextField@
activatableTextFieldSelector :: Selector
activatableTextFieldSelector = mkSelector "activatableTextField"

-- | @Selector@ for @isPasswordField@
isPasswordFieldSelector :: Selector
isPasswordFieldSelector = mkSelector "isPasswordField"

-- | @Selector@ for @comb@
combSelector :: Selector
combSelector = mkSelector "comb"

-- | @Selector@ for @setComb:@
setCombSelector :: Selector
setCombSelector = mkSelector "setComb:"

-- | @Selector@ for @maximumLength@
maximumLengthSelector :: Selector
maximumLengthSelector = mkSelector "maximumLength"

-- | @Selector@ for @setMaximumLength:@
setMaximumLengthSelector :: Selector
setMaximumLengthSelector = mkSelector "setMaximumLength:"

-- | @Selector@ for @widgetStringValue@
widgetStringValueSelector :: Selector
widgetStringValueSelector = mkSelector "widgetStringValue"

-- | @Selector@ for @setWidgetStringValue:@
setWidgetStringValueSelector :: Selector
setWidgetStringValueSelector = mkSelector "setWidgetStringValue:"

-- | @Selector@ for @widgetDefaultStringValue@
widgetDefaultStringValueSelector :: Selector
widgetDefaultStringValueSelector = mkSelector "widgetDefaultStringValue"

-- | @Selector@ for @setWidgetDefaultStringValue:@
setWidgetDefaultStringValueSelector :: Selector
setWidgetDefaultStringValueSelector = mkSelector "setWidgetDefaultStringValue:"

-- | @Selector@ for @allowsToggleToOff@
allowsToggleToOffSelector :: Selector
allowsToggleToOffSelector = mkSelector "allowsToggleToOff"

-- | @Selector@ for @setAllowsToggleToOff:@
setAllowsToggleToOffSelector :: Selector
setAllowsToggleToOffSelector = mkSelector "setAllowsToggleToOff:"

-- | @Selector@ for @radiosInUnison@
radiosInUnisonSelector :: Selector
radiosInUnisonSelector = mkSelector "radiosInUnison"

-- | @Selector@ for @setRadiosInUnison:@
setRadiosInUnisonSelector :: Selector
setRadiosInUnisonSelector = mkSelector "setRadiosInUnison:"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @listChoice@
listChoiceSelector :: Selector
listChoiceSelector = mkSelector "listChoice"

-- | @Selector@ for @setListChoice:@
setListChoiceSelector :: Selector
setListChoiceSelector = mkSelector "setListChoice:"

-- | @Selector@ for @choices@
choicesSelector :: Selector
choicesSelector = mkSelector "choices"

-- | @Selector@ for @setChoices:@
setChoicesSelector :: Selector
setChoicesSelector = mkSelector "setChoices:"

-- | @Selector@ for @values@
valuesSelector :: Selector
valuesSelector = mkSelector "values"

-- | @Selector@ for @setValues:@
setValuesSelector :: Selector
setValuesSelector = mkSelector "setValues:"

-- | @Selector@ for @buttonWidgetState@
buttonWidgetStateSelector :: Selector
buttonWidgetStateSelector = mkSelector "buttonWidgetState"

-- | @Selector@ for @setButtonWidgetState:@
setButtonWidgetStateSelector :: Selector
setButtonWidgetStateSelector = mkSelector "setButtonWidgetState:"

-- | @Selector@ for @buttonWidgetStateString@
buttonWidgetStateStringSelector :: Selector
buttonWidgetStateStringSelector = mkSelector "buttonWidgetStateString"

-- | @Selector@ for @setButtonWidgetStateString:@
setButtonWidgetStateStringSelector :: Selector
setButtonWidgetStateStringSelector = mkSelector "setButtonWidgetStateString:"

-- | @Selector@ for @open@
openSelector :: Selector
openSelector = mkSelector "open"

-- | @Selector@ for @setOpen:@
setOpenSelector :: Selector
setOpenSelector = mkSelector "setOpen:"

-- | @Selector@ for @paths@
pathsSelector :: Selector
pathsSelector = mkSelector "paths"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @caption@
captionSelector :: Selector
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @stampName@
stampNameSelector :: Selector
stampNameSelector = mkSelector "stampName"

-- | @Selector@ for @setStampName:@
setStampNameSelector :: Selector
setStampNameSelector = mkSelector "setStampName:"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @mouseUpAction@
mouseUpActionSelector :: Selector
mouseUpActionSelector = mkSelector "mouseUpAction"

-- | @Selector@ for @setMouseUpAction:@
setMouseUpActionSelector :: Selector
setMouseUpActionSelector = mkSelector "setMouseUpAction:"

