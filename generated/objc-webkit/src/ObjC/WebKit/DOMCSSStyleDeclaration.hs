{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSStyleDeclaration@.
module ObjC.WebKit.DOMCSSStyleDeclaration
  ( DOMCSSStyleDeclaration
  , IsDOMCSSStyleDeclaration(..)
  , getPropertyValue
  , getPropertyCSSValue
  , removeProperty
  , getPropertyPriority
  , setProperty_value_priority
  , item
  , getPropertyShorthand
  , isPropertyImplicit
  , azimuth
  , setAzimuth
  , background
  , setBackground
  , backgroundAttachment
  , setBackgroundAttachment
  , backgroundColor
  , setBackgroundColor
  , backgroundImage
  , setBackgroundImage
  , backgroundPosition
  , setBackgroundPosition
  , backgroundRepeat
  , setBackgroundRepeat
  , border
  , setBorder
  , borderCollapse
  , setBorderCollapse
  , borderColor
  , setBorderColor
  , borderSpacing
  , setBorderSpacing
  , borderStyle
  , setBorderStyle
  , borderTop
  , setBorderTop
  , borderRight
  , setBorderRight
  , borderBottom
  , setBorderBottom
  , borderLeft
  , setBorderLeft
  , borderTopColor
  , setBorderTopColor
  , borderRightColor
  , setBorderRightColor
  , borderBottomColor
  , setBorderBottomColor
  , borderLeftColor
  , setBorderLeftColor
  , borderTopStyle
  , setBorderTopStyle
  , borderRightStyle
  , setBorderRightStyle
  , borderBottomStyle
  , setBorderBottomStyle
  , borderLeftStyle
  , setBorderLeftStyle
  , borderTopWidth
  , setBorderTopWidth
  , borderRightWidth
  , setBorderRightWidth
  , borderBottomWidth
  , setBorderBottomWidth
  , borderLeftWidth
  , setBorderLeftWidth
  , borderWidth
  , setBorderWidth
  , bottom
  , setBottom
  , captionSide
  , setCaptionSide
  , clear
  , setClear
  , clip
  , setClip
  , color
  , setColor
  , content
  , setContent
  , counterIncrement
  , setCounterIncrement
  , counterReset
  , setCounterReset
  , cue
  , setCue
  , cueAfter
  , setCueAfter
  , cueBefore
  , setCueBefore
  , cursor
  , setCursor
  , direction
  , setDirection
  , display
  , setDisplay
  , elevation
  , setElevation
  , emptyCells
  , setEmptyCells
  , cssFloat
  , setCssFloat
  , font
  , setFont
  , fontFamily
  , setFontFamily
  , fontSize
  , setFontSize
  , fontSizeAdjust
  , setFontSizeAdjust
  , fontStretch
  , setFontStretch
  , fontStyle
  , setFontStyle
  , fontVariant
  , setFontVariant
  , fontWeight
  , setFontWeight
  , height
  , setHeight
  , left
  , setLeft
  , letterSpacing
  , setLetterSpacing
  , lineHeight
  , setLineHeight
  , listStyle
  , setListStyle
  , listStyleImage
  , setListStyleImage
  , listStylePosition
  , setListStylePosition
  , listStyleType
  , setListStyleType
  , margin
  , setMargin
  , marginTop
  , setMarginTop
  , marginRight
  , setMarginRight
  , marginBottom
  , setMarginBottom
  , marginLeft
  , setMarginLeft
  , markerOffset
  , setMarkerOffset
  , marks
  , setMarks
  , maxHeight
  , setMaxHeight
  , maxWidth
  , setMaxWidth
  , minHeight
  , setMinHeight
  , minWidth
  , setMinWidth
  , orphans
  , setOrphans
  , outline
  , setOutline
  , outlineColor
  , setOutlineColor
  , outlineStyle
  , setOutlineStyle
  , outlineWidth
  , setOutlineWidth
  , overflow
  , setOverflow
  , padding
  , setPadding
  , paddingTop
  , setPaddingTop
  , paddingRight
  , setPaddingRight
  , paddingBottom
  , setPaddingBottom
  , paddingLeft
  , setPaddingLeft
  , page
  , setPage
  , pageBreakAfter
  , setPageBreakAfter
  , pageBreakBefore
  , setPageBreakBefore
  , pageBreakInside
  , setPageBreakInside
  , pause
  , setPause
  , pauseAfter
  , setPauseAfter
  , pauseBefore
  , setPauseBefore
  , pitch
  , setPitch
  , pitchRange
  , setPitchRange
  , playDuring
  , setPlayDuring
  , position
  , setPosition
  , quotes
  , setQuotes
  , richness
  , setRichness
  , right
  , setRight
  , size
  , setSize
  , speak
  , setSpeak
  , speakHeader
  , setSpeakHeader
  , speakNumeral
  , setSpeakNumeral
  , speakPunctuation
  , setSpeakPunctuation
  , speechRate
  , setSpeechRate
  , stress
  , setStress
  , tableLayout
  , setTableLayout
  , textAlign
  , setTextAlign
  , textDecoration
  , setTextDecoration
  , textIndent
  , setTextIndent
  , textShadow
  , setTextShadow
  , textTransform
  , setTextTransform
  , top
  , setTop
  , unicodeBidi
  , setUnicodeBidi
  , verticalAlign
  , setVerticalAlign
  , visibility
  , setVisibility
  , voiceFamily
  , setVoiceFamily
  , volume
  , setVolume
  , whiteSpace
  , setWhiteSpace
  , widows
  , setWidows
  , width
  , setWidth
  , wordSpacing
  , setWordSpacing
  , zIndex
  , setZIndex
  , setProperty
  , cssText
  , setCssText
  , length_
  , parentRule
  , getPropertyValueSelector
  , getPropertyCSSValueSelector
  , removePropertySelector
  , getPropertyPrioritySelector
  , setProperty_value_prioritySelector
  , itemSelector
  , getPropertyShorthandSelector
  , isPropertyImplicitSelector
  , azimuthSelector
  , setAzimuthSelector
  , backgroundSelector
  , setBackgroundSelector
  , backgroundAttachmentSelector
  , setBackgroundAttachmentSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , backgroundImageSelector
  , setBackgroundImageSelector
  , backgroundPositionSelector
  , setBackgroundPositionSelector
  , backgroundRepeatSelector
  , setBackgroundRepeatSelector
  , borderSelector
  , setBorderSelector
  , borderCollapseSelector
  , setBorderCollapseSelector
  , borderColorSelector
  , setBorderColorSelector
  , borderSpacingSelector
  , setBorderSpacingSelector
  , borderStyleSelector
  , setBorderStyleSelector
  , borderTopSelector
  , setBorderTopSelector
  , borderRightSelector
  , setBorderRightSelector
  , borderBottomSelector
  , setBorderBottomSelector
  , borderLeftSelector
  , setBorderLeftSelector
  , borderTopColorSelector
  , setBorderTopColorSelector
  , borderRightColorSelector
  , setBorderRightColorSelector
  , borderBottomColorSelector
  , setBorderBottomColorSelector
  , borderLeftColorSelector
  , setBorderLeftColorSelector
  , borderTopStyleSelector
  , setBorderTopStyleSelector
  , borderRightStyleSelector
  , setBorderRightStyleSelector
  , borderBottomStyleSelector
  , setBorderBottomStyleSelector
  , borderLeftStyleSelector
  , setBorderLeftStyleSelector
  , borderTopWidthSelector
  , setBorderTopWidthSelector
  , borderRightWidthSelector
  , setBorderRightWidthSelector
  , borderBottomWidthSelector
  , setBorderBottomWidthSelector
  , borderLeftWidthSelector
  , setBorderLeftWidthSelector
  , borderWidthSelector
  , setBorderWidthSelector
  , bottomSelector
  , setBottomSelector
  , captionSideSelector
  , setCaptionSideSelector
  , clearSelector
  , setClearSelector
  , clipSelector
  , setClipSelector
  , colorSelector
  , setColorSelector
  , contentSelector
  , setContentSelector
  , counterIncrementSelector
  , setCounterIncrementSelector
  , counterResetSelector
  , setCounterResetSelector
  , cueSelector
  , setCueSelector
  , cueAfterSelector
  , setCueAfterSelector
  , cueBeforeSelector
  , setCueBeforeSelector
  , cursorSelector
  , setCursorSelector
  , directionSelector
  , setDirectionSelector
  , displaySelector
  , setDisplaySelector
  , elevationSelector
  , setElevationSelector
  , emptyCellsSelector
  , setEmptyCellsSelector
  , cssFloatSelector
  , setCssFloatSelector
  , fontSelector
  , setFontSelector
  , fontFamilySelector
  , setFontFamilySelector
  , fontSizeSelector
  , setFontSizeSelector
  , fontSizeAdjustSelector
  , setFontSizeAdjustSelector
  , fontStretchSelector
  , setFontStretchSelector
  , fontStyleSelector
  , setFontStyleSelector
  , fontVariantSelector
  , setFontVariantSelector
  , fontWeightSelector
  , setFontWeightSelector
  , heightSelector
  , setHeightSelector
  , leftSelector
  , setLeftSelector
  , letterSpacingSelector
  , setLetterSpacingSelector
  , lineHeightSelector
  , setLineHeightSelector
  , listStyleSelector
  , setListStyleSelector
  , listStyleImageSelector
  , setListStyleImageSelector
  , listStylePositionSelector
  , setListStylePositionSelector
  , listStyleTypeSelector
  , setListStyleTypeSelector
  , marginSelector
  , setMarginSelector
  , marginTopSelector
  , setMarginTopSelector
  , marginRightSelector
  , setMarginRightSelector
  , marginBottomSelector
  , setMarginBottomSelector
  , marginLeftSelector
  , setMarginLeftSelector
  , markerOffsetSelector
  , setMarkerOffsetSelector
  , marksSelector
  , setMarksSelector
  , maxHeightSelector
  , setMaxHeightSelector
  , maxWidthSelector
  , setMaxWidthSelector
  , minHeightSelector
  , setMinHeightSelector
  , minWidthSelector
  , setMinWidthSelector
  , orphansSelector
  , setOrphansSelector
  , outlineSelector
  , setOutlineSelector
  , outlineColorSelector
  , setOutlineColorSelector
  , outlineStyleSelector
  , setOutlineStyleSelector
  , outlineWidthSelector
  , setOutlineWidthSelector
  , overflowSelector
  , setOverflowSelector
  , paddingSelector
  , setPaddingSelector
  , paddingTopSelector
  , setPaddingTopSelector
  , paddingRightSelector
  , setPaddingRightSelector
  , paddingBottomSelector
  , setPaddingBottomSelector
  , paddingLeftSelector
  , setPaddingLeftSelector
  , pageSelector
  , setPageSelector
  , pageBreakAfterSelector
  , setPageBreakAfterSelector
  , pageBreakBeforeSelector
  , setPageBreakBeforeSelector
  , pageBreakInsideSelector
  , setPageBreakInsideSelector
  , pauseSelector
  , setPauseSelector
  , pauseAfterSelector
  , setPauseAfterSelector
  , pauseBeforeSelector
  , setPauseBeforeSelector
  , pitchSelector
  , setPitchSelector
  , pitchRangeSelector
  , setPitchRangeSelector
  , playDuringSelector
  , setPlayDuringSelector
  , positionSelector
  , setPositionSelector
  , quotesSelector
  , setQuotesSelector
  , richnessSelector
  , setRichnessSelector
  , rightSelector
  , setRightSelector
  , sizeSelector
  , setSizeSelector
  , speakSelector
  , setSpeakSelector
  , speakHeaderSelector
  , setSpeakHeaderSelector
  , speakNumeralSelector
  , setSpeakNumeralSelector
  , speakPunctuationSelector
  , setSpeakPunctuationSelector
  , speechRateSelector
  , setSpeechRateSelector
  , stressSelector
  , setStressSelector
  , tableLayoutSelector
  , setTableLayoutSelector
  , textAlignSelector
  , setTextAlignSelector
  , textDecorationSelector
  , setTextDecorationSelector
  , textIndentSelector
  , setTextIndentSelector
  , textShadowSelector
  , setTextShadowSelector
  , textTransformSelector
  , setTextTransformSelector
  , topSelector
  , setTopSelector
  , unicodeBidiSelector
  , setUnicodeBidiSelector
  , verticalAlignSelector
  , setVerticalAlignSelector
  , visibilitySelector
  , setVisibilitySelector
  , voiceFamilySelector
  , setVoiceFamilySelector
  , volumeSelector
  , setVolumeSelector
  , whiteSpaceSelector
  , setWhiteSpaceSelector
  , widowsSelector
  , setWidowsSelector
  , widthSelector
  , setWidthSelector
  , wordSpacingSelector
  , setWordSpacingSelector
  , zIndexSelector
  , setZIndexSelector
  , setPropertySelector
  , cssTextSelector
  , setCssTextSelector
  , lengthSelector
  , parentRuleSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getPropertyValue:@
getPropertyValue :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
getPropertyValue domcssStyleDeclaration  propertyName =
withObjCPtr propertyName $ \raw_propertyName ->
    sendMsg domcssStyleDeclaration (mkSelector "getPropertyValue:") (retPtr retVoid) [argPtr (castPtr raw_propertyName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getPropertyCSSValue:@
getPropertyCSSValue :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id DOMCSSValue)
getPropertyCSSValue domcssStyleDeclaration  propertyName =
withObjCPtr propertyName $ \raw_propertyName ->
    sendMsg domcssStyleDeclaration (mkSelector "getPropertyCSSValue:") (retPtr retVoid) [argPtr (castPtr raw_propertyName :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeProperty:@
removeProperty :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
removeProperty domcssStyleDeclaration  propertyName =
withObjCPtr propertyName $ \raw_propertyName ->
    sendMsg domcssStyleDeclaration (mkSelector "removeProperty:") (retPtr retVoid) [argPtr (castPtr raw_propertyName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getPropertyPriority:@
getPropertyPriority :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
getPropertyPriority domcssStyleDeclaration  propertyName =
withObjCPtr propertyName $ \raw_propertyName ->
    sendMsg domcssStyleDeclaration (mkSelector "getPropertyPriority:") (retPtr retVoid) [argPtr (castPtr raw_propertyName :: Ptr ())] >>= retainedObject . castPtr

-- | @- setProperty:value:priority:@
setProperty_value_priority :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName, IsNSString value, IsNSString priority) => domcssStyleDeclaration -> propertyName -> value -> priority -> IO ()
setProperty_value_priority domcssStyleDeclaration  propertyName value priority =
withObjCPtr propertyName $ \raw_propertyName ->
  withObjCPtr value $ \raw_value ->
    withObjCPtr priority $ \raw_priority ->
        sendMsg domcssStyleDeclaration (mkSelector "setProperty:value:priority:") retVoid [argPtr (castPtr raw_propertyName :: Ptr ()), argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_priority :: Ptr ())]

-- | @- item:@
item :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> CUInt -> IO (Id NSString)
item domcssStyleDeclaration  index =
  sendMsg domcssStyleDeclaration (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- getPropertyShorthand:@
getPropertyShorthand :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
getPropertyShorthand domcssStyleDeclaration  propertyName =
withObjCPtr propertyName $ \raw_propertyName ->
    sendMsg domcssStyleDeclaration (mkSelector "getPropertyShorthand:") (retPtr retVoid) [argPtr (castPtr raw_propertyName :: Ptr ())] >>= retainedObject . castPtr

-- | @- isPropertyImplicit:@
isPropertyImplicit :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO Bool
isPropertyImplicit domcssStyleDeclaration  propertyName =
withObjCPtr propertyName $ \raw_propertyName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domcssStyleDeclaration (mkSelector "isPropertyImplicit:") retCULong [argPtr (castPtr raw_propertyName :: Ptr ())]

-- | @- azimuth@
azimuth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
azimuth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "azimuth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAzimuth:@
setAzimuth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString azimuth) => domcssStyleDeclaration -> azimuth -> IO ()
setAzimuth domcssStyleDeclaration  azimuth =
withObjCPtr azimuth $ \raw_azimuth ->
    sendMsg domcssStyleDeclaration (mkSelector "setAzimuth:") retVoid [argPtr (castPtr raw_azimuth :: Ptr ())]

-- | @- background@
background :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
background domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "background") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackground:@
setBackground :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString background) => domcssStyleDeclaration -> background -> IO ()
setBackground domcssStyleDeclaration  background =
withObjCPtr background $ \raw_background ->
    sendMsg domcssStyleDeclaration (mkSelector "setBackground:") retVoid [argPtr (castPtr raw_background :: Ptr ())]

-- | @- backgroundAttachment@
backgroundAttachment :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundAttachment domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "backgroundAttachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundAttachment:@
setBackgroundAttachment :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundAttachment) => domcssStyleDeclaration -> backgroundAttachment -> IO ()
setBackgroundAttachment domcssStyleDeclaration  backgroundAttachment =
withObjCPtr backgroundAttachment $ \raw_backgroundAttachment ->
    sendMsg domcssStyleDeclaration (mkSelector "setBackgroundAttachment:") retVoid [argPtr (castPtr raw_backgroundAttachment :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundColor) => domcssStyleDeclaration -> backgroundColor -> IO ()
setBackgroundColor domcssStyleDeclaration  backgroundColor =
withObjCPtr backgroundColor $ \raw_backgroundColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_backgroundColor :: Ptr ())]

-- | @- backgroundImage@
backgroundImage :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundImage domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "backgroundImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundImage:@
setBackgroundImage :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundImage) => domcssStyleDeclaration -> backgroundImage -> IO ()
setBackgroundImage domcssStyleDeclaration  backgroundImage =
withObjCPtr backgroundImage $ \raw_backgroundImage ->
    sendMsg domcssStyleDeclaration (mkSelector "setBackgroundImage:") retVoid [argPtr (castPtr raw_backgroundImage :: Ptr ())]

-- | @- backgroundPosition@
backgroundPosition :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundPosition domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "backgroundPosition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundPosition:@
setBackgroundPosition :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundPosition) => domcssStyleDeclaration -> backgroundPosition -> IO ()
setBackgroundPosition domcssStyleDeclaration  backgroundPosition =
withObjCPtr backgroundPosition $ \raw_backgroundPosition ->
    sendMsg domcssStyleDeclaration (mkSelector "setBackgroundPosition:") retVoid [argPtr (castPtr raw_backgroundPosition :: Ptr ())]

-- | @- backgroundRepeat@
backgroundRepeat :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundRepeat domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "backgroundRepeat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundRepeat:@
setBackgroundRepeat :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundRepeat) => domcssStyleDeclaration -> backgroundRepeat -> IO ()
setBackgroundRepeat domcssStyleDeclaration  backgroundRepeat =
withObjCPtr backgroundRepeat $ \raw_backgroundRepeat ->
    sendMsg domcssStyleDeclaration (mkSelector "setBackgroundRepeat:") retVoid [argPtr (castPtr raw_backgroundRepeat :: Ptr ())]

-- | @- border@
border :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
border domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "border") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorder:@
setBorder :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString border) => domcssStyleDeclaration -> border -> IO ()
setBorder domcssStyleDeclaration  border =
withObjCPtr border $ \raw_border ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorder:") retVoid [argPtr (castPtr raw_border :: Ptr ())]

-- | @- borderCollapse@
borderCollapse :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderCollapse domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderCollapse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderCollapse:@
setBorderCollapse :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderCollapse) => domcssStyleDeclaration -> borderCollapse -> IO ()
setBorderCollapse domcssStyleDeclaration  borderCollapse =
withObjCPtr borderCollapse $ \raw_borderCollapse ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderCollapse:") retVoid [argPtr (castPtr raw_borderCollapse :: Ptr ())]

-- | @- borderColor@
borderColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderColor:@
setBorderColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderColor) => domcssStyleDeclaration -> borderColor -> IO ()
setBorderColor domcssStyleDeclaration  borderColor =
withObjCPtr borderColor $ \raw_borderColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderColor:") retVoid [argPtr (castPtr raw_borderColor :: Ptr ())]

-- | @- borderSpacing@
borderSpacing :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderSpacing domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderSpacing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderSpacing:@
setBorderSpacing :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderSpacing) => domcssStyleDeclaration -> borderSpacing -> IO ()
setBorderSpacing domcssStyleDeclaration  borderSpacing =
withObjCPtr borderSpacing $ \raw_borderSpacing ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderSpacing:") retVoid [argPtr (castPtr raw_borderSpacing :: Ptr ())]

-- | @- borderStyle@
borderStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderStyle:@
setBorderStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderStyle) => domcssStyleDeclaration -> borderStyle -> IO ()
setBorderStyle domcssStyleDeclaration  borderStyle =
withObjCPtr borderStyle $ \raw_borderStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderStyle:") retVoid [argPtr (castPtr raw_borderStyle :: Ptr ())]

-- | @- borderTop@
borderTop :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTop domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderTop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderTop:@
setBorderTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTop) => domcssStyleDeclaration -> borderTop -> IO ()
setBorderTop domcssStyleDeclaration  borderTop =
withObjCPtr borderTop $ \raw_borderTop ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderTop:") retVoid [argPtr (castPtr raw_borderTop :: Ptr ())]

-- | @- borderRight@
borderRight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderRight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderRight:@
setBorderRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRight) => domcssStyleDeclaration -> borderRight -> IO ()
setBorderRight domcssStyleDeclaration  borderRight =
withObjCPtr borderRight $ \raw_borderRight ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderRight:") retVoid [argPtr (castPtr raw_borderRight :: Ptr ())]

-- | @- borderBottom@
borderBottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottom domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderBottom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderBottom:@
setBorderBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottom) => domcssStyleDeclaration -> borderBottom -> IO ()
setBorderBottom domcssStyleDeclaration  borderBottom =
withObjCPtr borderBottom $ \raw_borderBottom ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderBottom:") retVoid [argPtr (castPtr raw_borderBottom :: Ptr ())]

-- | @- borderLeft@
borderLeft :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeft domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderLeft") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderLeft:@
setBorderLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeft) => domcssStyleDeclaration -> borderLeft -> IO ()
setBorderLeft domcssStyleDeclaration  borderLeft =
withObjCPtr borderLeft $ \raw_borderLeft ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderLeft:") retVoid [argPtr (castPtr raw_borderLeft :: Ptr ())]

-- | @- borderTopColor@
borderTopColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTopColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderTopColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderTopColor:@
setBorderTopColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTopColor) => domcssStyleDeclaration -> borderTopColor -> IO ()
setBorderTopColor domcssStyleDeclaration  borderTopColor =
withObjCPtr borderTopColor $ \raw_borderTopColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderTopColor:") retVoid [argPtr (castPtr raw_borderTopColor :: Ptr ())]

-- | @- borderRightColor@
borderRightColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRightColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderRightColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderRightColor:@
setBorderRightColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRightColor) => domcssStyleDeclaration -> borderRightColor -> IO ()
setBorderRightColor domcssStyleDeclaration  borderRightColor =
withObjCPtr borderRightColor $ \raw_borderRightColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderRightColor:") retVoid [argPtr (castPtr raw_borderRightColor :: Ptr ())]

-- | @- borderBottomColor@
borderBottomColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottomColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderBottomColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderBottomColor:@
setBorderBottomColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottomColor) => domcssStyleDeclaration -> borderBottomColor -> IO ()
setBorderBottomColor domcssStyleDeclaration  borderBottomColor =
withObjCPtr borderBottomColor $ \raw_borderBottomColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderBottomColor:") retVoid [argPtr (castPtr raw_borderBottomColor :: Ptr ())]

-- | @- borderLeftColor@
borderLeftColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeftColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderLeftColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderLeftColor:@
setBorderLeftColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeftColor) => domcssStyleDeclaration -> borderLeftColor -> IO ()
setBorderLeftColor domcssStyleDeclaration  borderLeftColor =
withObjCPtr borderLeftColor $ \raw_borderLeftColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderLeftColor:") retVoid [argPtr (castPtr raw_borderLeftColor :: Ptr ())]

-- | @- borderTopStyle@
borderTopStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTopStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderTopStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderTopStyle:@
setBorderTopStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTopStyle) => domcssStyleDeclaration -> borderTopStyle -> IO ()
setBorderTopStyle domcssStyleDeclaration  borderTopStyle =
withObjCPtr borderTopStyle $ \raw_borderTopStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderTopStyle:") retVoid [argPtr (castPtr raw_borderTopStyle :: Ptr ())]

-- | @- borderRightStyle@
borderRightStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRightStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderRightStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderRightStyle:@
setBorderRightStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRightStyle) => domcssStyleDeclaration -> borderRightStyle -> IO ()
setBorderRightStyle domcssStyleDeclaration  borderRightStyle =
withObjCPtr borderRightStyle $ \raw_borderRightStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderRightStyle:") retVoid [argPtr (castPtr raw_borderRightStyle :: Ptr ())]

-- | @- borderBottomStyle@
borderBottomStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottomStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderBottomStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderBottomStyle:@
setBorderBottomStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottomStyle) => domcssStyleDeclaration -> borderBottomStyle -> IO ()
setBorderBottomStyle domcssStyleDeclaration  borderBottomStyle =
withObjCPtr borderBottomStyle $ \raw_borderBottomStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderBottomStyle:") retVoid [argPtr (castPtr raw_borderBottomStyle :: Ptr ())]

-- | @- borderLeftStyle@
borderLeftStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeftStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderLeftStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderLeftStyle:@
setBorderLeftStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeftStyle) => domcssStyleDeclaration -> borderLeftStyle -> IO ()
setBorderLeftStyle domcssStyleDeclaration  borderLeftStyle =
withObjCPtr borderLeftStyle $ \raw_borderLeftStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderLeftStyle:") retVoid [argPtr (castPtr raw_borderLeftStyle :: Ptr ())]

-- | @- borderTopWidth@
borderTopWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTopWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderTopWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderTopWidth:@
setBorderTopWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTopWidth) => domcssStyleDeclaration -> borderTopWidth -> IO ()
setBorderTopWidth domcssStyleDeclaration  borderTopWidth =
withObjCPtr borderTopWidth $ \raw_borderTopWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderTopWidth:") retVoid [argPtr (castPtr raw_borderTopWidth :: Ptr ())]

-- | @- borderRightWidth@
borderRightWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRightWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderRightWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderRightWidth:@
setBorderRightWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRightWidth) => domcssStyleDeclaration -> borderRightWidth -> IO ()
setBorderRightWidth domcssStyleDeclaration  borderRightWidth =
withObjCPtr borderRightWidth $ \raw_borderRightWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderRightWidth:") retVoid [argPtr (castPtr raw_borderRightWidth :: Ptr ())]

-- | @- borderBottomWidth@
borderBottomWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottomWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderBottomWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderBottomWidth:@
setBorderBottomWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottomWidth) => domcssStyleDeclaration -> borderBottomWidth -> IO ()
setBorderBottomWidth domcssStyleDeclaration  borderBottomWidth =
withObjCPtr borderBottomWidth $ \raw_borderBottomWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderBottomWidth:") retVoid [argPtr (castPtr raw_borderBottomWidth :: Ptr ())]

-- | @- borderLeftWidth@
borderLeftWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeftWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderLeftWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderLeftWidth:@
setBorderLeftWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeftWidth) => domcssStyleDeclaration -> borderLeftWidth -> IO ()
setBorderLeftWidth domcssStyleDeclaration  borderLeftWidth =
withObjCPtr borderLeftWidth $ \raw_borderLeftWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderLeftWidth:") retVoid [argPtr (castPtr raw_borderLeftWidth :: Ptr ())]

-- | @- borderWidth@
borderWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "borderWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderWidth:@
setBorderWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderWidth) => domcssStyleDeclaration -> borderWidth -> IO ()
setBorderWidth domcssStyleDeclaration  borderWidth =
withObjCPtr borderWidth $ \raw_borderWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setBorderWidth:") retVoid [argPtr (castPtr raw_borderWidth :: Ptr ())]

-- | @- bottom@
bottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
bottom domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "bottom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBottom:@
setBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString bottom) => domcssStyleDeclaration -> bottom -> IO ()
setBottom domcssStyleDeclaration  bottom =
withObjCPtr bottom $ \raw_bottom ->
    sendMsg domcssStyleDeclaration (mkSelector "setBottom:") retVoid [argPtr (castPtr raw_bottom :: Ptr ())]

-- | @- captionSide@
captionSide :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
captionSide domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "captionSide") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaptionSide:@
setCaptionSide :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString captionSide) => domcssStyleDeclaration -> captionSide -> IO ()
setCaptionSide domcssStyleDeclaration  captionSide =
withObjCPtr captionSide $ \raw_captionSide ->
    sendMsg domcssStyleDeclaration (mkSelector "setCaptionSide:") retVoid [argPtr (castPtr raw_captionSide :: Ptr ())]

-- | @- clear@
clear :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
clear domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "clear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClear:@
setClear :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString clear) => domcssStyleDeclaration -> clear -> IO ()
setClear domcssStyleDeclaration  clear =
withObjCPtr clear $ \raw_clear ->
    sendMsg domcssStyleDeclaration (mkSelector "setClear:") retVoid [argPtr (castPtr raw_clear :: Ptr ())]

-- | @- clip@
clip :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
clip domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "clip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClip:@
setClip :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString clip) => domcssStyleDeclaration -> clip -> IO ()
setClip domcssStyleDeclaration  clip =
withObjCPtr clip $ \raw_clip ->
    sendMsg domcssStyleDeclaration (mkSelector "setClip:") retVoid [argPtr (castPtr raw_clip :: Ptr ())]

-- | @- color@
color :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
color domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString color) => domcssStyleDeclaration -> color -> IO ()
setColor domcssStyleDeclaration  color =
withObjCPtr color $ \raw_color ->
    sendMsg domcssStyleDeclaration (mkSelector "setColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- content@
content :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
content domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContent:@
setContent :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString content) => domcssStyleDeclaration -> content -> IO ()
setContent domcssStyleDeclaration  content =
withObjCPtr content $ \raw_content ->
    sendMsg domcssStyleDeclaration (mkSelector "setContent:") retVoid [argPtr (castPtr raw_content :: Ptr ())]

-- | @- counterIncrement@
counterIncrement :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
counterIncrement domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "counterIncrement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCounterIncrement:@
setCounterIncrement :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString counterIncrement) => domcssStyleDeclaration -> counterIncrement -> IO ()
setCounterIncrement domcssStyleDeclaration  counterIncrement =
withObjCPtr counterIncrement $ \raw_counterIncrement ->
    sendMsg domcssStyleDeclaration (mkSelector "setCounterIncrement:") retVoid [argPtr (castPtr raw_counterIncrement :: Ptr ())]

-- | @- counterReset@
counterReset :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
counterReset domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "counterReset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCounterReset:@
setCounterReset :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString counterReset) => domcssStyleDeclaration -> counterReset -> IO ()
setCounterReset domcssStyleDeclaration  counterReset =
withObjCPtr counterReset $ \raw_counterReset ->
    sendMsg domcssStyleDeclaration (mkSelector "setCounterReset:") retVoid [argPtr (castPtr raw_counterReset :: Ptr ())]

-- | @- cue@
cue :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cue domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "cue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCue:@
setCue :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cue) => domcssStyleDeclaration -> cue -> IO ()
setCue domcssStyleDeclaration  cue =
withObjCPtr cue $ \raw_cue ->
    sendMsg domcssStyleDeclaration (mkSelector "setCue:") retVoid [argPtr (castPtr raw_cue :: Ptr ())]

-- | @- cueAfter@
cueAfter :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cueAfter domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "cueAfter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCueAfter:@
setCueAfter :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cueAfter) => domcssStyleDeclaration -> cueAfter -> IO ()
setCueAfter domcssStyleDeclaration  cueAfter =
withObjCPtr cueAfter $ \raw_cueAfter ->
    sendMsg domcssStyleDeclaration (mkSelector "setCueAfter:") retVoid [argPtr (castPtr raw_cueAfter :: Ptr ())]

-- | @- cueBefore@
cueBefore :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cueBefore domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "cueBefore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCueBefore:@
setCueBefore :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cueBefore) => domcssStyleDeclaration -> cueBefore -> IO ()
setCueBefore domcssStyleDeclaration  cueBefore =
withObjCPtr cueBefore $ \raw_cueBefore ->
    sendMsg domcssStyleDeclaration (mkSelector "setCueBefore:") retVoid [argPtr (castPtr raw_cueBefore :: Ptr ())]

-- | @- cursor@
cursor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cursor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "cursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCursor:@
setCursor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cursor) => domcssStyleDeclaration -> cursor -> IO ()
setCursor domcssStyleDeclaration  cursor =
withObjCPtr cursor $ \raw_cursor ->
    sendMsg domcssStyleDeclaration (mkSelector "setCursor:") retVoid [argPtr (castPtr raw_cursor :: Ptr ())]

-- | @- direction@
direction :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
direction domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "direction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirection:@
setDirection :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString direction) => domcssStyleDeclaration -> direction -> IO ()
setDirection domcssStyleDeclaration  direction =
withObjCPtr direction $ \raw_direction ->
    sendMsg domcssStyleDeclaration (mkSelector "setDirection:") retVoid [argPtr (castPtr raw_direction :: Ptr ())]

-- | @- display@
display :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
display domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "display") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplay:@
setDisplay :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString display) => domcssStyleDeclaration -> display -> IO ()
setDisplay domcssStyleDeclaration  display =
withObjCPtr display $ \raw_display ->
    sendMsg domcssStyleDeclaration (mkSelector "setDisplay:") retVoid [argPtr (castPtr raw_display :: Ptr ())]

-- | @- elevation@
elevation :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
elevation domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "elevation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElevation:@
setElevation :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString elevation) => domcssStyleDeclaration -> elevation -> IO ()
setElevation domcssStyleDeclaration  elevation =
withObjCPtr elevation $ \raw_elevation ->
    sendMsg domcssStyleDeclaration (mkSelector "setElevation:") retVoid [argPtr (castPtr raw_elevation :: Ptr ())]

-- | @- emptyCells@
emptyCells :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
emptyCells domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "emptyCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmptyCells:@
setEmptyCells :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString emptyCells) => domcssStyleDeclaration -> emptyCells -> IO ()
setEmptyCells domcssStyleDeclaration  emptyCells =
withObjCPtr emptyCells $ \raw_emptyCells ->
    sendMsg domcssStyleDeclaration (mkSelector "setEmptyCells:") retVoid [argPtr (castPtr raw_emptyCells :: Ptr ())]

-- | @- cssFloat@
cssFloat :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cssFloat domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "cssFloat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCssFloat:@
setCssFloat :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cssFloat) => domcssStyleDeclaration -> cssFloat -> IO ()
setCssFloat domcssStyleDeclaration  cssFloat =
withObjCPtr cssFloat $ \raw_cssFloat ->
    sendMsg domcssStyleDeclaration (mkSelector "setCssFloat:") retVoid [argPtr (castPtr raw_cssFloat :: Ptr ())]

-- | @- font@
font :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
font domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString font) => domcssStyleDeclaration -> font -> IO ()
setFont domcssStyleDeclaration  font =
withObjCPtr font $ \raw_font ->
    sendMsg domcssStyleDeclaration (mkSelector "setFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @- fontFamily@
fontFamily :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontFamily domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontFamily:@
setFontFamily :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontFamily) => domcssStyleDeclaration -> fontFamily -> IO ()
setFontFamily domcssStyleDeclaration  fontFamily =
withObjCPtr fontFamily $ \raw_fontFamily ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontFamily:") retVoid [argPtr (castPtr raw_fontFamily :: Ptr ())]

-- | @- fontSize@
fontSize :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontSize domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontSize:@
setFontSize :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontSize) => domcssStyleDeclaration -> fontSize -> IO ()
setFontSize domcssStyleDeclaration  fontSize =
withObjCPtr fontSize $ \raw_fontSize ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontSize:") retVoid [argPtr (castPtr raw_fontSize :: Ptr ())]

-- | @- fontSizeAdjust@
fontSizeAdjust :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontSizeAdjust domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontSizeAdjust") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontSizeAdjust:@
setFontSizeAdjust :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontSizeAdjust) => domcssStyleDeclaration -> fontSizeAdjust -> IO ()
setFontSizeAdjust domcssStyleDeclaration  fontSizeAdjust =
withObjCPtr fontSizeAdjust $ \raw_fontSizeAdjust ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontSizeAdjust:") retVoid [argPtr (castPtr raw_fontSizeAdjust :: Ptr ())]

-- | @- fontStretch@
fontStretch :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontStretch domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontStretch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontStretch:@
setFontStretch :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontStretch) => domcssStyleDeclaration -> fontStretch -> IO ()
setFontStretch domcssStyleDeclaration  fontStretch =
withObjCPtr fontStretch $ \raw_fontStretch ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontStretch:") retVoid [argPtr (castPtr raw_fontStretch :: Ptr ())]

-- | @- fontStyle@
fontStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontStyle:@
setFontStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontStyle) => domcssStyleDeclaration -> fontStyle -> IO ()
setFontStyle domcssStyleDeclaration  fontStyle =
withObjCPtr fontStyle $ \raw_fontStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontStyle:") retVoid [argPtr (castPtr raw_fontStyle :: Ptr ())]

-- | @- fontVariant@
fontVariant :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontVariant domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontVariant:@
setFontVariant :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontVariant) => domcssStyleDeclaration -> fontVariant -> IO ()
setFontVariant domcssStyleDeclaration  fontVariant =
withObjCPtr fontVariant $ \raw_fontVariant ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontVariant:") retVoid [argPtr (castPtr raw_fontVariant :: Ptr ())]

-- | @- fontWeight@
fontWeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontWeight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "fontWeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontWeight:@
setFontWeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontWeight) => domcssStyleDeclaration -> fontWeight -> IO ()
setFontWeight domcssStyleDeclaration  fontWeight =
withObjCPtr fontWeight $ \raw_fontWeight ->
    sendMsg domcssStyleDeclaration (mkSelector "setFontWeight:") retVoid [argPtr (castPtr raw_fontWeight :: Ptr ())]

-- | @- height@
height :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
height domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString height) => domcssStyleDeclaration -> height -> IO ()
setHeight domcssStyleDeclaration  height =
withObjCPtr height $ \raw_height ->
    sendMsg domcssStyleDeclaration (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_height :: Ptr ())]

-- | @- left@
left :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
left domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "left") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLeft:@
setLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString left) => domcssStyleDeclaration -> left -> IO ()
setLeft domcssStyleDeclaration  left =
withObjCPtr left $ \raw_left ->
    sendMsg domcssStyleDeclaration (mkSelector "setLeft:") retVoid [argPtr (castPtr raw_left :: Ptr ())]

-- | @- letterSpacing@
letterSpacing :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
letterSpacing domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "letterSpacing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLetterSpacing:@
setLetterSpacing :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString letterSpacing) => domcssStyleDeclaration -> letterSpacing -> IO ()
setLetterSpacing domcssStyleDeclaration  letterSpacing =
withObjCPtr letterSpacing $ \raw_letterSpacing ->
    sendMsg domcssStyleDeclaration (mkSelector "setLetterSpacing:") retVoid [argPtr (castPtr raw_letterSpacing :: Ptr ())]

-- | @- lineHeight@
lineHeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
lineHeight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "lineHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineHeight:@
setLineHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString lineHeight) => domcssStyleDeclaration -> lineHeight -> IO ()
setLineHeight domcssStyleDeclaration  lineHeight =
withObjCPtr lineHeight $ \raw_lineHeight ->
    sendMsg domcssStyleDeclaration (mkSelector "setLineHeight:") retVoid [argPtr (castPtr raw_lineHeight :: Ptr ())]

-- | @- listStyle@
listStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "listStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListStyle:@
setListStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStyle) => domcssStyleDeclaration -> listStyle -> IO ()
setListStyle domcssStyleDeclaration  listStyle =
withObjCPtr listStyle $ \raw_listStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setListStyle:") retVoid [argPtr (castPtr raw_listStyle :: Ptr ())]

-- | @- listStyleImage@
listStyleImage :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStyleImage domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "listStyleImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListStyleImage:@
setListStyleImage :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStyleImage) => domcssStyleDeclaration -> listStyleImage -> IO ()
setListStyleImage domcssStyleDeclaration  listStyleImage =
withObjCPtr listStyleImage $ \raw_listStyleImage ->
    sendMsg domcssStyleDeclaration (mkSelector "setListStyleImage:") retVoid [argPtr (castPtr raw_listStyleImage :: Ptr ())]

-- | @- listStylePosition@
listStylePosition :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStylePosition domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "listStylePosition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListStylePosition:@
setListStylePosition :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStylePosition) => domcssStyleDeclaration -> listStylePosition -> IO ()
setListStylePosition domcssStyleDeclaration  listStylePosition =
withObjCPtr listStylePosition $ \raw_listStylePosition ->
    sendMsg domcssStyleDeclaration (mkSelector "setListStylePosition:") retVoid [argPtr (castPtr raw_listStylePosition :: Ptr ())]

-- | @- listStyleType@
listStyleType :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStyleType domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "listStyleType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListStyleType:@
setListStyleType :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStyleType) => domcssStyleDeclaration -> listStyleType -> IO ()
setListStyleType domcssStyleDeclaration  listStyleType =
withObjCPtr listStyleType $ \raw_listStyleType ->
    sendMsg domcssStyleDeclaration (mkSelector "setListStyleType:") retVoid [argPtr (castPtr raw_listStyleType :: Ptr ())]

-- | @- margin@
margin :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
margin domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "margin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMargin:@
setMargin :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString margin) => domcssStyleDeclaration -> margin -> IO ()
setMargin domcssStyleDeclaration  margin =
withObjCPtr margin $ \raw_margin ->
    sendMsg domcssStyleDeclaration (mkSelector "setMargin:") retVoid [argPtr (castPtr raw_margin :: Ptr ())]

-- | @- marginTop@
marginTop :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginTop domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "marginTop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginTop:@
setMarginTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginTop) => domcssStyleDeclaration -> marginTop -> IO ()
setMarginTop domcssStyleDeclaration  marginTop =
withObjCPtr marginTop $ \raw_marginTop ->
    sendMsg domcssStyleDeclaration (mkSelector "setMarginTop:") retVoid [argPtr (castPtr raw_marginTop :: Ptr ())]

-- | @- marginRight@
marginRight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginRight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "marginRight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginRight:@
setMarginRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginRight) => domcssStyleDeclaration -> marginRight -> IO ()
setMarginRight domcssStyleDeclaration  marginRight =
withObjCPtr marginRight $ \raw_marginRight ->
    sendMsg domcssStyleDeclaration (mkSelector "setMarginRight:") retVoid [argPtr (castPtr raw_marginRight :: Ptr ())]

-- | @- marginBottom@
marginBottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginBottom domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "marginBottom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginBottom:@
setMarginBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginBottom) => domcssStyleDeclaration -> marginBottom -> IO ()
setMarginBottom domcssStyleDeclaration  marginBottom =
withObjCPtr marginBottom $ \raw_marginBottom ->
    sendMsg domcssStyleDeclaration (mkSelector "setMarginBottom:") retVoid [argPtr (castPtr raw_marginBottom :: Ptr ())]

-- | @- marginLeft@
marginLeft :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginLeft domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "marginLeft") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginLeft:@
setMarginLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginLeft) => domcssStyleDeclaration -> marginLeft -> IO ()
setMarginLeft domcssStyleDeclaration  marginLeft =
withObjCPtr marginLeft $ \raw_marginLeft ->
    sendMsg domcssStyleDeclaration (mkSelector "setMarginLeft:") retVoid [argPtr (castPtr raw_marginLeft :: Ptr ())]

-- | @- markerOffset@
markerOffset :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
markerOffset domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "markerOffset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarkerOffset:@
setMarkerOffset :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString markerOffset) => domcssStyleDeclaration -> markerOffset -> IO ()
setMarkerOffset domcssStyleDeclaration  markerOffset =
withObjCPtr markerOffset $ \raw_markerOffset ->
    sendMsg domcssStyleDeclaration (mkSelector "setMarkerOffset:") retVoid [argPtr (castPtr raw_markerOffset :: Ptr ())]

-- | @- marks@
marks :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marks domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "marks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarks:@
setMarks :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marks) => domcssStyleDeclaration -> marks -> IO ()
setMarks domcssStyleDeclaration  marks =
withObjCPtr marks $ \raw_marks ->
    sendMsg domcssStyleDeclaration (mkSelector "setMarks:") retVoid [argPtr (castPtr raw_marks :: Ptr ())]

-- | @- maxHeight@
maxHeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
maxHeight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "maxHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxHeight:@
setMaxHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString maxHeight) => domcssStyleDeclaration -> maxHeight -> IO ()
setMaxHeight domcssStyleDeclaration  maxHeight =
withObjCPtr maxHeight $ \raw_maxHeight ->
    sendMsg domcssStyleDeclaration (mkSelector "setMaxHeight:") retVoid [argPtr (castPtr raw_maxHeight :: Ptr ())]

-- | @- maxWidth@
maxWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
maxWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "maxWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxWidth:@
setMaxWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString maxWidth) => domcssStyleDeclaration -> maxWidth -> IO ()
setMaxWidth domcssStyleDeclaration  maxWidth =
withObjCPtr maxWidth $ \raw_maxWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setMaxWidth:") retVoid [argPtr (castPtr raw_maxWidth :: Ptr ())]

-- | @- minHeight@
minHeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
minHeight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "minHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinHeight:@
setMinHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString minHeight) => domcssStyleDeclaration -> minHeight -> IO ()
setMinHeight domcssStyleDeclaration  minHeight =
withObjCPtr minHeight $ \raw_minHeight ->
    sendMsg domcssStyleDeclaration (mkSelector "setMinHeight:") retVoid [argPtr (castPtr raw_minHeight :: Ptr ())]

-- | @- minWidth@
minWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
minWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "minWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinWidth:@
setMinWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString minWidth) => domcssStyleDeclaration -> minWidth -> IO ()
setMinWidth domcssStyleDeclaration  minWidth =
withObjCPtr minWidth $ \raw_minWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setMinWidth:") retVoid [argPtr (castPtr raw_minWidth :: Ptr ())]

-- | @- orphans@
orphans :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
orphans domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "orphans") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrphans:@
setOrphans :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString orphans) => domcssStyleDeclaration -> orphans -> IO ()
setOrphans domcssStyleDeclaration  orphans =
withObjCPtr orphans $ \raw_orphans ->
    sendMsg domcssStyleDeclaration (mkSelector "setOrphans:") retVoid [argPtr (castPtr raw_orphans :: Ptr ())]

-- | @- outline@
outline :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outline domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "outline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutline:@
setOutline :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outline) => domcssStyleDeclaration -> outline -> IO ()
setOutline domcssStyleDeclaration  outline =
withObjCPtr outline $ \raw_outline ->
    sendMsg domcssStyleDeclaration (mkSelector "setOutline:") retVoid [argPtr (castPtr raw_outline :: Ptr ())]

-- | @- outlineColor@
outlineColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outlineColor domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "outlineColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutlineColor:@
setOutlineColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outlineColor) => domcssStyleDeclaration -> outlineColor -> IO ()
setOutlineColor domcssStyleDeclaration  outlineColor =
withObjCPtr outlineColor $ \raw_outlineColor ->
    sendMsg domcssStyleDeclaration (mkSelector "setOutlineColor:") retVoid [argPtr (castPtr raw_outlineColor :: Ptr ())]

-- | @- outlineStyle@
outlineStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outlineStyle domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "outlineStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutlineStyle:@
setOutlineStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outlineStyle) => domcssStyleDeclaration -> outlineStyle -> IO ()
setOutlineStyle domcssStyleDeclaration  outlineStyle =
withObjCPtr outlineStyle $ \raw_outlineStyle ->
    sendMsg domcssStyleDeclaration (mkSelector "setOutlineStyle:") retVoid [argPtr (castPtr raw_outlineStyle :: Ptr ())]

-- | @- outlineWidth@
outlineWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outlineWidth domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "outlineWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutlineWidth:@
setOutlineWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outlineWidth) => domcssStyleDeclaration -> outlineWidth -> IO ()
setOutlineWidth domcssStyleDeclaration  outlineWidth =
withObjCPtr outlineWidth $ \raw_outlineWidth ->
    sendMsg domcssStyleDeclaration (mkSelector "setOutlineWidth:") retVoid [argPtr (castPtr raw_outlineWidth :: Ptr ())]

-- | @- overflow@
overflow :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
overflow domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "overflow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOverflow:@
setOverflow :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString overflow) => domcssStyleDeclaration -> overflow -> IO ()
setOverflow domcssStyleDeclaration  overflow =
withObjCPtr overflow $ \raw_overflow ->
    sendMsg domcssStyleDeclaration (mkSelector "setOverflow:") retVoid [argPtr (castPtr raw_overflow :: Ptr ())]

-- | @- padding@
padding :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
padding domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "padding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPadding:@
setPadding :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString padding) => domcssStyleDeclaration -> padding -> IO ()
setPadding domcssStyleDeclaration  padding =
withObjCPtr padding $ \raw_padding ->
    sendMsg domcssStyleDeclaration (mkSelector "setPadding:") retVoid [argPtr (castPtr raw_padding :: Ptr ())]

-- | @- paddingTop@
paddingTop :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingTop domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "paddingTop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaddingTop:@
setPaddingTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingTop) => domcssStyleDeclaration -> paddingTop -> IO ()
setPaddingTop domcssStyleDeclaration  paddingTop =
withObjCPtr paddingTop $ \raw_paddingTop ->
    sendMsg domcssStyleDeclaration (mkSelector "setPaddingTop:") retVoid [argPtr (castPtr raw_paddingTop :: Ptr ())]

-- | @- paddingRight@
paddingRight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingRight domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "paddingRight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaddingRight:@
setPaddingRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingRight) => domcssStyleDeclaration -> paddingRight -> IO ()
setPaddingRight domcssStyleDeclaration  paddingRight =
withObjCPtr paddingRight $ \raw_paddingRight ->
    sendMsg domcssStyleDeclaration (mkSelector "setPaddingRight:") retVoid [argPtr (castPtr raw_paddingRight :: Ptr ())]

-- | @- paddingBottom@
paddingBottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingBottom domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "paddingBottom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaddingBottom:@
setPaddingBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingBottom) => domcssStyleDeclaration -> paddingBottom -> IO ()
setPaddingBottom domcssStyleDeclaration  paddingBottom =
withObjCPtr paddingBottom $ \raw_paddingBottom ->
    sendMsg domcssStyleDeclaration (mkSelector "setPaddingBottom:") retVoid [argPtr (castPtr raw_paddingBottom :: Ptr ())]

-- | @- paddingLeft@
paddingLeft :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingLeft domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "paddingLeft") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaddingLeft:@
setPaddingLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingLeft) => domcssStyleDeclaration -> paddingLeft -> IO ()
setPaddingLeft domcssStyleDeclaration  paddingLeft =
withObjCPtr paddingLeft $ \raw_paddingLeft ->
    sendMsg domcssStyleDeclaration (mkSelector "setPaddingLeft:") retVoid [argPtr (castPtr raw_paddingLeft :: Ptr ())]

-- | @- page@
page :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
page domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "page") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPage:@
setPage :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString page) => domcssStyleDeclaration -> page -> IO ()
setPage domcssStyleDeclaration  page =
withObjCPtr page $ \raw_page ->
    sendMsg domcssStyleDeclaration (mkSelector "setPage:") retVoid [argPtr (castPtr raw_page :: Ptr ())]

-- | @- pageBreakAfter@
pageBreakAfter :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pageBreakAfter domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pageBreakAfter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageBreakAfter:@
setPageBreakAfter :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pageBreakAfter) => domcssStyleDeclaration -> pageBreakAfter -> IO ()
setPageBreakAfter domcssStyleDeclaration  pageBreakAfter =
withObjCPtr pageBreakAfter $ \raw_pageBreakAfter ->
    sendMsg domcssStyleDeclaration (mkSelector "setPageBreakAfter:") retVoid [argPtr (castPtr raw_pageBreakAfter :: Ptr ())]

-- | @- pageBreakBefore@
pageBreakBefore :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pageBreakBefore domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pageBreakBefore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageBreakBefore:@
setPageBreakBefore :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pageBreakBefore) => domcssStyleDeclaration -> pageBreakBefore -> IO ()
setPageBreakBefore domcssStyleDeclaration  pageBreakBefore =
withObjCPtr pageBreakBefore $ \raw_pageBreakBefore ->
    sendMsg domcssStyleDeclaration (mkSelector "setPageBreakBefore:") retVoid [argPtr (castPtr raw_pageBreakBefore :: Ptr ())]

-- | @- pageBreakInside@
pageBreakInside :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pageBreakInside domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pageBreakInside") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageBreakInside:@
setPageBreakInside :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pageBreakInside) => domcssStyleDeclaration -> pageBreakInside -> IO ()
setPageBreakInside domcssStyleDeclaration  pageBreakInside =
withObjCPtr pageBreakInside $ \raw_pageBreakInside ->
    sendMsg domcssStyleDeclaration (mkSelector "setPageBreakInside:") retVoid [argPtr (castPtr raw_pageBreakInside :: Ptr ())]

-- | @- pause@
pause :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pause domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPause:@
setPause :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pause) => domcssStyleDeclaration -> pause -> IO ()
setPause domcssStyleDeclaration  pause =
withObjCPtr pause $ \raw_pause ->
    sendMsg domcssStyleDeclaration (mkSelector "setPause:") retVoid [argPtr (castPtr raw_pause :: Ptr ())]

-- | @- pauseAfter@
pauseAfter :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pauseAfter domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pauseAfter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPauseAfter:@
setPauseAfter :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pauseAfter) => domcssStyleDeclaration -> pauseAfter -> IO ()
setPauseAfter domcssStyleDeclaration  pauseAfter =
withObjCPtr pauseAfter $ \raw_pauseAfter ->
    sendMsg domcssStyleDeclaration (mkSelector "setPauseAfter:") retVoid [argPtr (castPtr raw_pauseAfter :: Ptr ())]

-- | @- pauseBefore@
pauseBefore :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pauseBefore domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pauseBefore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPauseBefore:@
setPauseBefore :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pauseBefore) => domcssStyleDeclaration -> pauseBefore -> IO ()
setPauseBefore domcssStyleDeclaration  pauseBefore =
withObjCPtr pauseBefore $ \raw_pauseBefore ->
    sendMsg domcssStyleDeclaration (mkSelector "setPauseBefore:") retVoid [argPtr (castPtr raw_pauseBefore :: Ptr ())]

-- | @- pitch@
pitch :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pitch domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pitch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPitch:@
setPitch :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pitch) => domcssStyleDeclaration -> pitch -> IO ()
setPitch domcssStyleDeclaration  pitch =
withObjCPtr pitch $ \raw_pitch ->
    sendMsg domcssStyleDeclaration (mkSelector "setPitch:") retVoid [argPtr (castPtr raw_pitch :: Ptr ())]

-- | @- pitchRange@
pitchRange :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pitchRange domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "pitchRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPitchRange:@
setPitchRange :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pitchRange) => domcssStyleDeclaration -> pitchRange -> IO ()
setPitchRange domcssStyleDeclaration  pitchRange =
withObjCPtr pitchRange $ \raw_pitchRange ->
    sendMsg domcssStyleDeclaration (mkSelector "setPitchRange:") retVoid [argPtr (castPtr raw_pitchRange :: Ptr ())]

-- | @- playDuring@
playDuring :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
playDuring domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "playDuring") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlayDuring:@
setPlayDuring :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString playDuring) => domcssStyleDeclaration -> playDuring -> IO ()
setPlayDuring domcssStyleDeclaration  playDuring =
withObjCPtr playDuring $ \raw_playDuring ->
    sendMsg domcssStyleDeclaration (mkSelector "setPlayDuring:") retVoid [argPtr (castPtr raw_playDuring :: Ptr ())]

-- | @- position@
position :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
position domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString position) => domcssStyleDeclaration -> position -> IO ()
setPosition domcssStyleDeclaration  position =
withObjCPtr position $ \raw_position ->
    sendMsg domcssStyleDeclaration (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_position :: Ptr ())]

-- | @- quotes@
quotes :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
quotes domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "quotes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuotes:@
setQuotes :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString quotes) => domcssStyleDeclaration -> quotes -> IO ()
setQuotes domcssStyleDeclaration  quotes =
withObjCPtr quotes $ \raw_quotes ->
    sendMsg domcssStyleDeclaration (mkSelector "setQuotes:") retVoid [argPtr (castPtr raw_quotes :: Ptr ())]

-- | @- richness@
richness :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
richness domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "richness") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRichness:@
setRichness :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString richness) => domcssStyleDeclaration -> richness -> IO ()
setRichness domcssStyleDeclaration  richness =
withObjCPtr richness $ \raw_richness ->
    sendMsg domcssStyleDeclaration (mkSelector "setRichness:") retVoid [argPtr (castPtr raw_richness :: Ptr ())]

-- | @- right@
right :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
right domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "right") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRight:@
setRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString right) => domcssStyleDeclaration -> right -> IO ()
setRight domcssStyleDeclaration  right =
withObjCPtr right $ \raw_right ->
    sendMsg domcssStyleDeclaration (mkSelector "setRight:") retVoid [argPtr (castPtr raw_right :: Ptr ())]

-- | @- size@
size :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
size domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString size) => domcssStyleDeclaration -> size -> IO ()
setSize domcssStyleDeclaration  size =
withObjCPtr size $ \raw_size ->
    sendMsg domcssStyleDeclaration (mkSelector "setSize:") retVoid [argPtr (castPtr raw_size :: Ptr ())]

-- | @- speak@
speak :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speak domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "speak") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeak:@
setSpeak :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speak) => domcssStyleDeclaration -> speak -> IO ()
setSpeak domcssStyleDeclaration  speak =
withObjCPtr speak $ \raw_speak ->
    sendMsg domcssStyleDeclaration (mkSelector "setSpeak:") retVoid [argPtr (castPtr raw_speak :: Ptr ())]

-- | @- speakHeader@
speakHeader :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speakHeader domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "speakHeader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeakHeader:@
setSpeakHeader :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speakHeader) => domcssStyleDeclaration -> speakHeader -> IO ()
setSpeakHeader domcssStyleDeclaration  speakHeader =
withObjCPtr speakHeader $ \raw_speakHeader ->
    sendMsg domcssStyleDeclaration (mkSelector "setSpeakHeader:") retVoid [argPtr (castPtr raw_speakHeader :: Ptr ())]

-- | @- speakNumeral@
speakNumeral :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speakNumeral domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "speakNumeral") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeakNumeral:@
setSpeakNumeral :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speakNumeral) => domcssStyleDeclaration -> speakNumeral -> IO ()
setSpeakNumeral domcssStyleDeclaration  speakNumeral =
withObjCPtr speakNumeral $ \raw_speakNumeral ->
    sendMsg domcssStyleDeclaration (mkSelector "setSpeakNumeral:") retVoid [argPtr (castPtr raw_speakNumeral :: Ptr ())]

-- | @- speakPunctuation@
speakPunctuation :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speakPunctuation domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "speakPunctuation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeakPunctuation:@
setSpeakPunctuation :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speakPunctuation) => domcssStyleDeclaration -> speakPunctuation -> IO ()
setSpeakPunctuation domcssStyleDeclaration  speakPunctuation =
withObjCPtr speakPunctuation $ \raw_speakPunctuation ->
    sendMsg domcssStyleDeclaration (mkSelector "setSpeakPunctuation:") retVoid [argPtr (castPtr raw_speakPunctuation :: Ptr ())]

-- | @- speechRate@
speechRate :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speechRate domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "speechRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeechRate:@
setSpeechRate :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speechRate) => domcssStyleDeclaration -> speechRate -> IO ()
setSpeechRate domcssStyleDeclaration  speechRate =
withObjCPtr speechRate $ \raw_speechRate ->
    sendMsg domcssStyleDeclaration (mkSelector "setSpeechRate:") retVoid [argPtr (castPtr raw_speechRate :: Ptr ())]

-- | @- stress@
stress :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
stress domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "stress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStress:@
setStress :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString stress) => domcssStyleDeclaration -> stress -> IO ()
setStress domcssStyleDeclaration  stress =
withObjCPtr stress $ \raw_stress ->
    sendMsg domcssStyleDeclaration (mkSelector "setStress:") retVoid [argPtr (castPtr raw_stress :: Ptr ())]

-- | @- tableLayout@
tableLayout :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
tableLayout domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "tableLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTableLayout:@
setTableLayout :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString tableLayout) => domcssStyleDeclaration -> tableLayout -> IO ()
setTableLayout domcssStyleDeclaration  tableLayout =
withObjCPtr tableLayout $ \raw_tableLayout ->
    sendMsg domcssStyleDeclaration (mkSelector "setTableLayout:") retVoid [argPtr (castPtr raw_tableLayout :: Ptr ())]

-- | @- textAlign@
textAlign :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textAlign domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "textAlign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAlign:@
setTextAlign :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textAlign) => domcssStyleDeclaration -> textAlign -> IO ()
setTextAlign domcssStyleDeclaration  textAlign =
withObjCPtr textAlign $ \raw_textAlign ->
    sendMsg domcssStyleDeclaration (mkSelector "setTextAlign:") retVoid [argPtr (castPtr raw_textAlign :: Ptr ())]

-- | @- textDecoration@
textDecoration :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textDecoration domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "textDecoration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextDecoration:@
setTextDecoration :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textDecoration) => domcssStyleDeclaration -> textDecoration -> IO ()
setTextDecoration domcssStyleDeclaration  textDecoration =
withObjCPtr textDecoration $ \raw_textDecoration ->
    sendMsg domcssStyleDeclaration (mkSelector "setTextDecoration:") retVoid [argPtr (castPtr raw_textDecoration :: Ptr ())]

-- | @- textIndent@
textIndent :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textIndent domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "textIndent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextIndent:@
setTextIndent :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textIndent) => domcssStyleDeclaration -> textIndent -> IO ()
setTextIndent domcssStyleDeclaration  textIndent =
withObjCPtr textIndent $ \raw_textIndent ->
    sendMsg domcssStyleDeclaration (mkSelector "setTextIndent:") retVoid [argPtr (castPtr raw_textIndent :: Ptr ())]

-- | @- textShadow@
textShadow :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textShadow domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "textShadow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextShadow:@
setTextShadow :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textShadow) => domcssStyleDeclaration -> textShadow -> IO ()
setTextShadow domcssStyleDeclaration  textShadow =
withObjCPtr textShadow $ \raw_textShadow ->
    sendMsg domcssStyleDeclaration (mkSelector "setTextShadow:") retVoid [argPtr (castPtr raw_textShadow :: Ptr ())]

-- | @- textTransform@
textTransform :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textTransform domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "textTransform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextTransform:@
setTextTransform :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textTransform) => domcssStyleDeclaration -> textTransform -> IO ()
setTextTransform domcssStyleDeclaration  textTransform =
withObjCPtr textTransform $ \raw_textTransform ->
    sendMsg domcssStyleDeclaration (mkSelector "setTextTransform:") retVoid [argPtr (castPtr raw_textTransform :: Ptr ())]

-- | @- top@
top :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
top domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "top") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTop:@
setTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString top) => domcssStyleDeclaration -> top -> IO ()
setTop domcssStyleDeclaration  top =
withObjCPtr top $ \raw_top ->
    sendMsg domcssStyleDeclaration (mkSelector "setTop:") retVoid [argPtr (castPtr raw_top :: Ptr ())]

-- | @- unicodeBidi@
unicodeBidi :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
unicodeBidi domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "unicodeBidi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUnicodeBidi:@
setUnicodeBidi :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString unicodeBidi) => domcssStyleDeclaration -> unicodeBidi -> IO ()
setUnicodeBidi domcssStyleDeclaration  unicodeBidi =
withObjCPtr unicodeBidi $ \raw_unicodeBidi ->
    sendMsg domcssStyleDeclaration (mkSelector "setUnicodeBidi:") retVoid [argPtr (castPtr raw_unicodeBidi :: Ptr ())]

-- | @- verticalAlign@
verticalAlign :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
verticalAlign domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "verticalAlign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVerticalAlign:@
setVerticalAlign :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString verticalAlign) => domcssStyleDeclaration -> verticalAlign -> IO ()
setVerticalAlign domcssStyleDeclaration  verticalAlign =
withObjCPtr verticalAlign $ \raw_verticalAlign ->
    sendMsg domcssStyleDeclaration (mkSelector "setVerticalAlign:") retVoid [argPtr (castPtr raw_verticalAlign :: Ptr ())]

-- | @- visibility@
visibility :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
visibility domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "visibility") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVisibility:@
setVisibility :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString visibility) => domcssStyleDeclaration -> visibility -> IO ()
setVisibility domcssStyleDeclaration  visibility =
withObjCPtr visibility $ \raw_visibility ->
    sendMsg domcssStyleDeclaration (mkSelector "setVisibility:") retVoid [argPtr (castPtr raw_visibility :: Ptr ())]

-- | @- voiceFamily@
voiceFamily :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
voiceFamily domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "voiceFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVoiceFamily:@
setVoiceFamily :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString voiceFamily) => domcssStyleDeclaration -> voiceFamily -> IO ()
setVoiceFamily domcssStyleDeclaration  voiceFamily =
withObjCPtr voiceFamily $ \raw_voiceFamily ->
    sendMsg domcssStyleDeclaration (mkSelector "setVoiceFamily:") retVoid [argPtr (castPtr raw_voiceFamily :: Ptr ())]

-- | @- volume@
volume :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
volume domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "volume") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVolume:@
setVolume :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString volume) => domcssStyleDeclaration -> volume -> IO ()
setVolume domcssStyleDeclaration  volume =
withObjCPtr volume $ \raw_volume ->
    sendMsg domcssStyleDeclaration (mkSelector "setVolume:") retVoid [argPtr (castPtr raw_volume :: Ptr ())]

-- | @- whiteSpace@
whiteSpace :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
whiteSpace domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "whiteSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWhiteSpace:@
setWhiteSpace :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString whiteSpace) => domcssStyleDeclaration -> whiteSpace -> IO ()
setWhiteSpace domcssStyleDeclaration  whiteSpace =
withObjCPtr whiteSpace $ \raw_whiteSpace ->
    sendMsg domcssStyleDeclaration (mkSelector "setWhiteSpace:") retVoid [argPtr (castPtr raw_whiteSpace :: Ptr ())]

-- | @- widows@
widows :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
widows domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "widows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidows:@
setWidows :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString widows) => domcssStyleDeclaration -> widows -> IO ()
setWidows domcssStyleDeclaration  widows =
withObjCPtr widows $ \raw_widows ->
    sendMsg domcssStyleDeclaration (mkSelector "setWidows:") retVoid [argPtr (castPtr raw_widows :: Ptr ())]

-- | @- width@
width :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
width domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString width) => domcssStyleDeclaration -> width -> IO ()
setWidth domcssStyleDeclaration  width =
withObjCPtr width $ \raw_width ->
    sendMsg domcssStyleDeclaration (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_width :: Ptr ())]

-- | @- wordSpacing@
wordSpacing :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
wordSpacing domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "wordSpacing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWordSpacing:@
setWordSpacing :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString wordSpacing) => domcssStyleDeclaration -> wordSpacing -> IO ()
setWordSpacing domcssStyleDeclaration  wordSpacing =
withObjCPtr wordSpacing $ \raw_wordSpacing ->
    sendMsg domcssStyleDeclaration (mkSelector "setWordSpacing:") retVoid [argPtr (castPtr raw_wordSpacing :: Ptr ())]

-- | @- zIndex@
zIndex :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
zIndex domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "zIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZIndex:@
setZIndex :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString zIndex) => domcssStyleDeclaration -> zIndex -> IO ()
setZIndex domcssStyleDeclaration  zIndex =
withObjCPtr zIndex $ \raw_zIndex ->
    sendMsg domcssStyleDeclaration (mkSelector "setZIndex:") retVoid [argPtr (castPtr raw_zIndex :: Ptr ())]

-- | @- setProperty:::@
setProperty :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName, IsNSString value, IsNSString priority) => domcssStyleDeclaration -> propertyName -> value -> priority -> IO ()
setProperty domcssStyleDeclaration  propertyName value priority =
withObjCPtr propertyName $ \raw_propertyName ->
  withObjCPtr value $ \raw_value ->
    withObjCPtr priority $ \raw_priority ->
        sendMsg domcssStyleDeclaration (mkSelector "setProperty:::") retVoid [argPtr (castPtr raw_propertyName :: Ptr ()), argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_priority :: Ptr ())]

-- | @- cssText@
cssText :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cssText domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "cssText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCssText:@
setCssText :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString value) => domcssStyleDeclaration -> value -> IO ()
setCssText domcssStyleDeclaration  value =
withObjCPtr value $ \raw_value ->
    sendMsg domcssStyleDeclaration (mkSelector "setCssText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- length@
length_ :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO CUInt
length_ domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "length") retCUInt []

-- | @- parentRule@
parentRule :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id DOMCSSRule)
parentRule domcssStyleDeclaration  =
  sendMsg domcssStyleDeclaration (mkSelector "parentRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getPropertyValue:@
getPropertyValueSelector :: Selector
getPropertyValueSelector = mkSelector "getPropertyValue:"

-- | @Selector@ for @getPropertyCSSValue:@
getPropertyCSSValueSelector :: Selector
getPropertyCSSValueSelector = mkSelector "getPropertyCSSValue:"

-- | @Selector@ for @removeProperty:@
removePropertySelector :: Selector
removePropertySelector = mkSelector "removeProperty:"

-- | @Selector@ for @getPropertyPriority:@
getPropertyPrioritySelector :: Selector
getPropertyPrioritySelector = mkSelector "getPropertyPriority:"

-- | @Selector@ for @setProperty:value:priority:@
setProperty_value_prioritySelector :: Selector
setProperty_value_prioritySelector = mkSelector "setProperty:value:priority:"

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @getPropertyShorthand:@
getPropertyShorthandSelector :: Selector
getPropertyShorthandSelector = mkSelector "getPropertyShorthand:"

-- | @Selector@ for @isPropertyImplicit:@
isPropertyImplicitSelector :: Selector
isPropertyImplicitSelector = mkSelector "isPropertyImplicit:"

-- | @Selector@ for @azimuth@
azimuthSelector :: Selector
azimuthSelector = mkSelector "azimuth"

-- | @Selector@ for @setAzimuth:@
setAzimuthSelector :: Selector
setAzimuthSelector = mkSelector "setAzimuth:"

-- | @Selector@ for @background@
backgroundSelector :: Selector
backgroundSelector = mkSelector "background"

-- | @Selector@ for @setBackground:@
setBackgroundSelector :: Selector
setBackgroundSelector = mkSelector "setBackground:"

-- | @Selector@ for @backgroundAttachment@
backgroundAttachmentSelector :: Selector
backgroundAttachmentSelector = mkSelector "backgroundAttachment"

-- | @Selector@ for @setBackgroundAttachment:@
setBackgroundAttachmentSelector :: Selector
setBackgroundAttachmentSelector = mkSelector "setBackgroundAttachment:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @backgroundImage@
backgroundImageSelector :: Selector
backgroundImageSelector = mkSelector "backgroundImage"

-- | @Selector@ for @setBackgroundImage:@
setBackgroundImageSelector :: Selector
setBackgroundImageSelector = mkSelector "setBackgroundImage:"

-- | @Selector@ for @backgroundPosition@
backgroundPositionSelector :: Selector
backgroundPositionSelector = mkSelector "backgroundPosition"

-- | @Selector@ for @setBackgroundPosition:@
setBackgroundPositionSelector :: Selector
setBackgroundPositionSelector = mkSelector "setBackgroundPosition:"

-- | @Selector@ for @backgroundRepeat@
backgroundRepeatSelector :: Selector
backgroundRepeatSelector = mkSelector "backgroundRepeat"

-- | @Selector@ for @setBackgroundRepeat:@
setBackgroundRepeatSelector :: Selector
setBackgroundRepeatSelector = mkSelector "setBackgroundRepeat:"

-- | @Selector@ for @border@
borderSelector :: Selector
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @borderCollapse@
borderCollapseSelector :: Selector
borderCollapseSelector = mkSelector "borderCollapse"

-- | @Selector@ for @setBorderCollapse:@
setBorderCollapseSelector :: Selector
setBorderCollapseSelector = mkSelector "setBorderCollapse:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @borderSpacing@
borderSpacingSelector :: Selector
borderSpacingSelector = mkSelector "borderSpacing"

-- | @Selector@ for @setBorderSpacing:@
setBorderSpacingSelector :: Selector
setBorderSpacingSelector = mkSelector "setBorderSpacing:"

-- | @Selector@ for @borderStyle@
borderStyleSelector :: Selector
borderStyleSelector = mkSelector "borderStyle"

-- | @Selector@ for @setBorderStyle:@
setBorderStyleSelector :: Selector
setBorderStyleSelector = mkSelector "setBorderStyle:"

-- | @Selector@ for @borderTop@
borderTopSelector :: Selector
borderTopSelector = mkSelector "borderTop"

-- | @Selector@ for @setBorderTop:@
setBorderTopSelector :: Selector
setBorderTopSelector = mkSelector "setBorderTop:"

-- | @Selector@ for @borderRight@
borderRightSelector :: Selector
borderRightSelector = mkSelector "borderRight"

-- | @Selector@ for @setBorderRight:@
setBorderRightSelector :: Selector
setBorderRightSelector = mkSelector "setBorderRight:"

-- | @Selector@ for @borderBottom@
borderBottomSelector :: Selector
borderBottomSelector = mkSelector "borderBottom"

-- | @Selector@ for @setBorderBottom:@
setBorderBottomSelector :: Selector
setBorderBottomSelector = mkSelector "setBorderBottom:"

-- | @Selector@ for @borderLeft@
borderLeftSelector :: Selector
borderLeftSelector = mkSelector "borderLeft"

-- | @Selector@ for @setBorderLeft:@
setBorderLeftSelector :: Selector
setBorderLeftSelector = mkSelector "setBorderLeft:"

-- | @Selector@ for @borderTopColor@
borderTopColorSelector :: Selector
borderTopColorSelector = mkSelector "borderTopColor"

-- | @Selector@ for @setBorderTopColor:@
setBorderTopColorSelector :: Selector
setBorderTopColorSelector = mkSelector "setBorderTopColor:"

-- | @Selector@ for @borderRightColor@
borderRightColorSelector :: Selector
borderRightColorSelector = mkSelector "borderRightColor"

-- | @Selector@ for @setBorderRightColor:@
setBorderRightColorSelector :: Selector
setBorderRightColorSelector = mkSelector "setBorderRightColor:"

-- | @Selector@ for @borderBottomColor@
borderBottomColorSelector :: Selector
borderBottomColorSelector = mkSelector "borderBottomColor"

-- | @Selector@ for @setBorderBottomColor:@
setBorderBottomColorSelector :: Selector
setBorderBottomColorSelector = mkSelector "setBorderBottomColor:"

-- | @Selector@ for @borderLeftColor@
borderLeftColorSelector :: Selector
borderLeftColorSelector = mkSelector "borderLeftColor"

-- | @Selector@ for @setBorderLeftColor:@
setBorderLeftColorSelector :: Selector
setBorderLeftColorSelector = mkSelector "setBorderLeftColor:"

-- | @Selector@ for @borderTopStyle@
borderTopStyleSelector :: Selector
borderTopStyleSelector = mkSelector "borderTopStyle"

-- | @Selector@ for @setBorderTopStyle:@
setBorderTopStyleSelector :: Selector
setBorderTopStyleSelector = mkSelector "setBorderTopStyle:"

-- | @Selector@ for @borderRightStyle@
borderRightStyleSelector :: Selector
borderRightStyleSelector = mkSelector "borderRightStyle"

-- | @Selector@ for @setBorderRightStyle:@
setBorderRightStyleSelector :: Selector
setBorderRightStyleSelector = mkSelector "setBorderRightStyle:"

-- | @Selector@ for @borderBottomStyle@
borderBottomStyleSelector :: Selector
borderBottomStyleSelector = mkSelector "borderBottomStyle"

-- | @Selector@ for @setBorderBottomStyle:@
setBorderBottomStyleSelector :: Selector
setBorderBottomStyleSelector = mkSelector "setBorderBottomStyle:"

-- | @Selector@ for @borderLeftStyle@
borderLeftStyleSelector :: Selector
borderLeftStyleSelector = mkSelector "borderLeftStyle"

-- | @Selector@ for @setBorderLeftStyle:@
setBorderLeftStyleSelector :: Selector
setBorderLeftStyleSelector = mkSelector "setBorderLeftStyle:"

-- | @Selector@ for @borderTopWidth@
borderTopWidthSelector :: Selector
borderTopWidthSelector = mkSelector "borderTopWidth"

-- | @Selector@ for @setBorderTopWidth:@
setBorderTopWidthSelector :: Selector
setBorderTopWidthSelector = mkSelector "setBorderTopWidth:"

-- | @Selector@ for @borderRightWidth@
borderRightWidthSelector :: Selector
borderRightWidthSelector = mkSelector "borderRightWidth"

-- | @Selector@ for @setBorderRightWidth:@
setBorderRightWidthSelector :: Selector
setBorderRightWidthSelector = mkSelector "setBorderRightWidth:"

-- | @Selector@ for @borderBottomWidth@
borderBottomWidthSelector :: Selector
borderBottomWidthSelector = mkSelector "borderBottomWidth"

-- | @Selector@ for @setBorderBottomWidth:@
setBorderBottomWidthSelector :: Selector
setBorderBottomWidthSelector = mkSelector "setBorderBottomWidth:"

-- | @Selector@ for @borderLeftWidth@
borderLeftWidthSelector :: Selector
borderLeftWidthSelector = mkSelector "borderLeftWidth"

-- | @Selector@ for @setBorderLeftWidth:@
setBorderLeftWidthSelector :: Selector
setBorderLeftWidthSelector = mkSelector "setBorderLeftWidth:"

-- | @Selector@ for @borderWidth@
borderWidthSelector :: Selector
borderWidthSelector = mkSelector "borderWidth"

-- | @Selector@ for @setBorderWidth:@
setBorderWidthSelector :: Selector
setBorderWidthSelector = mkSelector "setBorderWidth:"

-- | @Selector@ for @bottom@
bottomSelector :: Selector
bottomSelector = mkSelector "bottom"

-- | @Selector@ for @setBottom:@
setBottomSelector :: Selector
setBottomSelector = mkSelector "setBottom:"

-- | @Selector@ for @captionSide@
captionSideSelector :: Selector
captionSideSelector = mkSelector "captionSide"

-- | @Selector@ for @setCaptionSide:@
setCaptionSideSelector :: Selector
setCaptionSideSelector = mkSelector "setCaptionSide:"

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @setClear:@
setClearSelector :: Selector
setClearSelector = mkSelector "setClear:"

-- | @Selector@ for @clip@
clipSelector :: Selector
clipSelector = mkSelector "clip"

-- | @Selector@ for @setClip:@
setClipSelector :: Selector
setClipSelector = mkSelector "setClip:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @counterIncrement@
counterIncrementSelector :: Selector
counterIncrementSelector = mkSelector "counterIncrement"

-- | @Selector@ for @setCounterIncrement:@
setCounterIncrementSelector :: Selector
setCounterIncrementSelector = mkSelector "setCounterIncrement:"

-- | @Selector@ for @counterReset@
counterResetSelector :: Selector
counterResetSelector = mkSelector "counterReset"

-- | @Selector@ for @setCounterReset:@
setCounterResetSelector :: Selector
setCounterResetSelector = mkSelector "setCounterReset:"

-- | @Selector@ for @cue@
cueSelector :: Selector
cueSelector = mkSelector "cue"

-- | @Selector@ for @setCue:@
setCueSelector :: Selector
setCueSelector = mkSelector "setCue:"

-- | @Selector@ for @cueAfter@
cueAfterSelector :: Selector
cueAfterSelector = mkSelector "cueAfter"

-- | @Selector@ for @setCueAfter:@
setCueAfterSelector :: Selector
setCueAfterSelector = mkSelector "setCueAfter:"

-- | @Selector@ for @cueBefore@
cueBeforeSelector :: Selector
cueBeforeSelector = mkSelector "cueBefore"

-- | @Selector@ for @setCueBefore:@
setCueBeforeSelector :: Selector
setCueBeforeSelector = mkSelector "setCueBefore:"

-- | @Selector@ for @cursor@
cursorSelector :: Selector
cursorSelector = mkSelector "cursor"

-- | @Selector@ for @setCursor:@
setCursorSelector :: Selector
setCursorSelector = mkSelector "setCursor:"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @display@
displaySelector :: Selector
displaySelector = mkSelector "display"

-- | @Selector@ for @setDisplay:@
setDisplaySelector :: Selector
setDisplaySelector = mkSelector "setDisplay:"

-- | @Selector@ for @elevation@
elevationSelector :: Selector
elevationSelector = mkSelector "elevation"

-- | @Selector@ for @setElevation:@
setElevationSelector :: Selector
setElevationSelector = mkSelector "setElevation:"

-- | @Selector@ for @emptyCells@
emptyCellsSelector :: Selector
emptyCellsSelector = mkSelector "emptyCells"

-- | @Selector@ for @setEmptyCells:@
setEmptyCellsSelector :: Selector
setEmptyCellsSelector = mkSelector "setEmptyCells:"

-- | @Selector@ for @cssFloat@
cssFloatSelector :: Selector
cssFloatSelector = mkSelector "cssFloat"

-- | @Selector@ for @setCssFloat:@
setCssFloatSelector :: Selector
setCssFloatSelector = mkSelector "setCssFloat:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontFamily@
fontFamilySelector :: Selector
fontFamilySelector = mkSelector "fontFamily"

-- | @Selector@ for @setFontFamily:@
setFontFamilySelector :: Selector
setFontFamilySelector = mkSelector "setFontFamily:"

-- | @Selector@ for @fontSize@
fontSizeSelector :: Selector
fontSizeSelector = mkSelector "fontSize"

-- | @Selector@ for @setFontSize:@
setFontSizeSelector :: Selector
setFontSizeSelector = mkSelector "setFontSize:"

-- | @Selector@ for @fontSizeAdjust@
fontSizeAdjustSelector :: Selector
fontSizeAdjustSelector = mkSelector "fontSizeAdjust"

-- | @Selector@ for @setFontSizeAdjust:@
setFontSizeAdjustSelector :: Selector
setFontSizeAdjustSelector = mkSelector "setFontSizeAdjust:"

-- | @Selector@ for @fontStretch@
fontStretchSelector :: Selector
fontStretchSelector = mkSelector "fontStretch"

-- | @Selector@ for @setFontStretch:@
setFontStretchSelector :: Selector
setFontStretchSelector = mkSelector "setFontStretch:"

-- | @Selector@ for @fontStyle@
fontStyleSelector :: Selector
fontStyleSelector = mkSelector "fontStyle"

-- | @Selector@ for @setFontStyle:@
setFontStyleSelector :: Selector
setFontStyleSelector = mkSelector "setFontStyle:"

-- | @Selector@ for @fontVariant@
fontVariantSelector :: Selector
fontVariantSelector = mkSelector "fontVariant"

-- | @Selector@ for @setFontVariant:@
setFontVariantSelector :: Selector
setFontVariantSelector = mkSelector "setFontVariant:"

-- | @Selector@ for @fontWeight@
fontWeightSelector :: Selector
fontWeightSelector = mkSelector "fontWeight"

-- | @Selector@ for @setFontWeight:@
setFontWeightSelector :: Selector
setFontWeightSelector = mkSelector "setFontWeight:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @left@
leftSelector :: Selector
leftSelector = mkSelector "left"

-- | @Selector@ for @setLeft:@
setLeftSelector :: Selector
setLeftSelector = mkSelector "setLeft:"

-- | @Selector@ for @letterSpacing@
letterSpacingSelector :: Selector
letterSpacingSelector = mkSelector "letterSpacing"

-- | @Selector@ for @setLetterSpacing:@
setLetterSpacingSelector :: Selector
setLetterSpacingSelector = mkSelector "setLetterSpacing:"

-- | @Selector@ for @lineHeight@
lineHeightSelector :: Selector
lineHeightSelector = mkSelector "lineHeight"

-- | @Selector@ for @setLineHeight:@
setLineHeightSelector :: Selector
setLineHeightSelector = mkSelector "setLineHeight:"

-- | @Selector@ for @listStyle@
listStyleSelector :: Selector
listStyleSelector = mkSelector "listStyle"

-- | @Selector@ for @setListStyle:@
setListStyleSelector :: Selector
setListStyleSelector = mkSelector "setListStyle:"

-- | @Selector@ for @listStyleImage@
listStyleImageSelector :: Selector
listStyleImageSelector = mkSelector "listStyleImage"

-- | @Selector@ for @setListStyleImage:@
setListStyleImageSelector :: Selector
setListStyleImageSelector = mkSelector "setListStyleImage:"

-- | @Selector@ for @listStylePosition@
listStylePositionSelector :: Selector
listStylePositionSelector = mkSelector "listStylePosition"

-- | @Selector@ for @setListStylePosition:@
setListStylePositionSelector :: Selector
setListStylePositionSelector = mkSelector "setListStylePosition:"

-- | @Selector@ for @listStyleType@
listStyleTypeSelector :: Selector
listStyleTypeSelector = mkSelector "listStyleType"

-- | @Selector@ for @setListStyleType:@
setListStyleTypeSelector :: Selector
setListStyleTypeSelector = mkSelector "setListStyleType:"

-- | @Selector@ for @margin@
marginSelector :: Selector
marginSelector = mkSelector "margin"

-- | @Selector@ for @setMargin:@
setMarginSelector :: Selector
setMarginSelector = mkSelector "setMargin:"

-- | @Selector@ for @marginTop@
marginTopSelector :: Selector
marginTopSelector = mkSelector "marginTop"

-- | @Selector@ for @setMarginTop:@
setMarginTopSelector :: Selector
setMarginTopSelector = mkSelector "setMarginTop:"

-- | @Selector@ for @marginRight@
marginRightSelector :: Selector
marginRightSelector = mkSelector "marginRight"

-- | @Selector@ for @setMarginRight:@
setMarginRightSelector :: Selector
setMarginRightSelector = mkSelector "setMarginRight:"

-- | @Selector@ for @marginBottom@
marginBottomSelector :: Selector
marginBottomSelector = mkSelector "marginBottom"

-- | @Selector@ for @setMarginBottom:@
setMarginBottomSelector :: Selector
setMarginBottomSelector = mkSelector "setMarginBottom:"

-- | @Selector@ for @marginLeft@
marginLeftSelector :: Selector
marginLeftSelector = mkSelector "marginLeft"

-- | @Selector@ for @setMarginLeft:@
setMarginLeftSelector :: Selector
setMarginLeftSelector = mkSelector "setMarginLeft:"

-- | @Selector@ for @markerOffset@
markerOffsetSelector :: Selector
markerOffsetSelector = mkSelector "markerOffset"

-- | @Selector@ for @setMarkerOffset:@
setMarkerOffsetSelector :: Selector
setMarkerOffsetSelector = mkSelector "setMarkerOffset:"

-- | @Selector@ for @marks@
marksSelector :: Selector
marksSelector = mkSelector "marks"

-- | @Selector@ for @setMarks:@
setMarksSelector :: Selector
setMarksSelector = mkSelector "setMarks:"

-- | @Selector@ for @maxHeight@
maxHeightSelector :: Selector
maxHeightSelector = mkSelector "maxHeight"

-- | @Selector@ for @setMaxHeight:@
setMaxHeightSelector :: Selector
setMaxHeightSelector = mkSelector "setMaxHeight:"

-- | @Selector@ for @maxWidth@
maxWidthSelector :: Selector
maxWidthSelector = mkSelector "maxWidth"

-- | @Selector@ for @setMaxWidth:@
setMaxWidthSelector :: Selector
setMaxWidthSelector = mkSelector "setMaxWidth:"

-- | @Selector@ for @minHeight@
minHeightSelector :: Selector
minHeightSelector = mkSelector "minHeight"

-- | @Selector@ for @setMinHeight:@
setMinHeightSelector :: Selector
setMinHeightSelector = mkSelector "setMinHeight:"

-- | @Selector@ for @minWidth@
minWidthSelector :: Selector
minWidthSelector = mkSelector "minWidth"

-- | @Selector@ for @setMinWidth:@
setMinWidthSelector :: Selector
setMinWidthSelector = mkSelector "setMinWidth:"

-- | @Selector@ for @orphans@
orphansSelector :: Selector
orphansSelector = mkSelector "orphans"

-- | @Selector@ for @setOrphans:@
setOrphansSelector :: Selector
setOrphansSelector = mkSelector "setOrphans:"

-- | @Selector@ for @outline@
outlineSelector :: Selector
outlineSelector = mkSelector "outline"

-- | @Selector@ for @setOutline:@
setOutlineSelector :: Selector
setOutlineSelector = mkSelector "setOutline:"

-- | @Selector@ for @outlineColor@
outlineColorSelector :: Selector
outlineColorSelector = mkSelector "outlineColor"

-- | @Selector@ for @setOutlineColor:@
setOutlineColorSelector :: Selector
setOutlineColorSelector = mkSelector "setOutlineColor:"

-- | @Selector@ for @outlineStyle@
outlineStyleSelector :: Selector
outlineStyleSelector = mkSelector "outlineStyle"

-- | @Selector@ for @setOutlineStyle:@
setOutlineStyleSelector :: Selector
setOutlineStyleSelector = mkSelector "setOutlineStyle:"

-- | @Selector@ for @outlineWidth@
outlineWidthSelector :: Selector
outlineWidthSelector = mkSelector "outlineWidth"

-- | @Selector@ for @setOutlineWidth:@
setOutlineWidthSelector :: Selector
setOutlineWidthSelector = mkSelector "setOutlineWidth:"

-- | @Selector@ for @overflow@
overflowSelector :: Selector
overflowSelector = mkSelector "overflow"

-- | @Selector@ for @setOverflow:@
setOverflowSelector :: Selector
setOverflowSelector = mkSelector "setOverflow:"

-- | @Selector@ for @padding@
paddingSelector :: Selector
paddingSelector = mkSelector "padding"

-- | @Selector@ for @setPadding:@
setPaddingSelector :: Selector
setPaddingSelector = mkSelector "setPadding:"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @setPaddingTop:@
setPaddingTopSelector :: Selector
setPaddingTopSelector = mkSelector "setPaddingTop:"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @setPaddingRight:@
setPaddingRightSelector :: Selector
setPaddingRightSelector = mkSelector "setPaddingRight:"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @setPaddingBottom:@
setPaddingBottomSelector :: Selector
setPaddingBottomSelector = mkSelector "setPaddingBottom:"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @setPaddingLeft:@
setPaddingLeftSelector :: Selector
setPaddingLeftSelector = mkSelector "setPaddingLeft:"

-- | @Selector@ for @page@
pageSelector :: Selector
pageSelector = mkSelector "page"

-- | @Selector@ for @setPage:@
setPageSelector :: Selector
setPageSelector = mkSelector "setPage:"

-- | @Selector@ for @pageBreakAfter@
pageBreakAfterSelector :: Selector
pageBreakAfterSelector = mkSelector "pageBreakAfter"

-- | @Selector@ for @setPageBreakAfter:@
setPageBreakAfterSelector :: Selector
setPageBreakAfterSelector = mkSelector "setPageBreakAfter:"

-- | @Selector@ for @pageBreakBefore@
pageBreakBeforeSelector :: Selector
pageBreakBeforeSelector = mkSelector "pageBreakBefore"

-- | @Selector@ for @setPageBreakBefore:@
setPageBreakBeforeSelector :: Selector
setPageBreakBeforeSelector = mkSelector "setPageBreakBefore:"

-- | @Selector@ for @pageBreakInside@
pageBreakInsideSelector :: Selector
pageBreakInsideSelector = mkSelector "pageBreakInside"

-- | @Selector@ for @setPageBreakInside:@
setPageBreakInsideSelector :: Selector
setPageBreakInsideSelector = mkSelector "setPageBreakInside:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @setPause:@
setPauseSelector :: Selector
setPauseSelector = mkSelector "setPause:"

-- | @Selector@ for @pauseAfter@
pauseAfterSelector :: Selector
pauseAfterSelector = mkSelector "pauseAfter"

-- | @Selector@ for @setPauseAfter:@
setPauseAfterSelector :: Selector
setPauseAfterSelector = mkSelector "setPauseAfter:"

-- | @Selector@ for @pauseBefore@
pauseBeforeSelector :: Selector
pauseBeforeSelector = mkSelector "pauseBefore"

-- | @Selector@ for @setPauseBefore:@
setPauseBeforeSelector :: Selector
setPauseBeforeSelector = mkSelector "setPauseBefore:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @pitchRange@
pitchRangeSelector :: Selector
pitchRangeSelector = mkSelector "pitchRange"

-- | @Selector@ for @setPitchRange:@
setPitchRangeSelector :: Selector
setPitchRangeSelector = mkSelector "setPitchRange:"

-- | @Selector@ for @playDuring@
playDuringSelector :: Selector
playDuringSelector = mkSelector "playDuring"

-- | @Selector@ for @setPlayDuring:@
setPlayDuringSelector :: Selector
setPlayDuringSelector = mkSelector "setPlayDuring:"

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @quotes@
quotesSelector :: Selector
quotesSelector = mkSelector "quotes"

-- | @Selector@ for @setQuotes:@
setQuotesSelector :: Selector
setQuotesSelector = mkSelector "setQuotes:"

-- | @Selector@ for @richness@
richnessSelector :: Selector
richnessSelector = mkSelector "richness"

-- | @Selector@ for @setRichness:@
setRichnessSelector :: Selector
setRichnessSelector = mkSelector "setRichness:"

-- | @Selector@ for @right@
rightSelector :: Selector
rightSelector = mkSelector "right"

-- | @Selector@ for @setRight:@
setRightSelector :: Selector
setRightSelector = mkSelector "setRight:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @speak@
speakSelector :: Selector
speakSelector = mkSelector "speak"

-- | @Selector@ for @setSpeak:@
setSpeakSelector :: Selector
setSpeakSelector = mkSelector "setSpeak:"

-- | @Selector@ for @speakHeader@
speakHeaderSelector :: Selector
speakHeaderSelector = mkSelector "speakHeader"

-- | @Selector@ for @setSpeakHeader:@
setSpeakHeaderSelector :: Selector
setSpeakHeaderSelector = mkSelector "setSpeakHeader:"

-- | @Selector@ for @speakNumeral@
speakNumeralSelector :: Selector
speakNumeralSelector = mkSelector "speakNumeral"

-- | @Selector@ for @setSpeakNumeral:@
setSpeakNumeralSelector :: Selector
setSpeakNumeralSelector = mkSelector "setSpeakNumeral:"

-- | @Selector@ for @speakPunctuation@
speakPunctuationSelector :: Selector
speakPunctuationSelector = mkSelector "speakPunctuation"

-- | @Selector@ for @setSpeakPunctuation:@
setSpeakPunctuationSelector :: Selector
setSpeakPunctuationSelector = mkSelector "setSpeakPunctuation:"

-- | @Selector@ for @speechRate@
speechRateSelector :: Selector
speechRateSelector = mkSelector "speechRate"

-- | @Selector@ for @setSpeechRate:@
setSpeechRateSelector :: Selector
setSpeechRateSelector = mkSelector "setSpeechRate:"

-- | @Selector@ for @stress@
stressSelector :: Selector
stressSelector = mkSelector "stress"

-- | @Selector@ for @setStress:@
setStressSelector :: Selector
setStressSelector = mkSelector "setStress:"

-- | @Selector@ for @tableLayout@
tableLayoutSelector :: Selector
tableLayoutSelector = mkSelector "tableLayout"

-- | @Selector@ for @setTableLayout:@
setTableLayoutSelector :: Selector
setTableLayoutSelector = mkSelector "setTableLayout:"

-- | @Selector@ for @textAlign@
textAlignSelector :: Selector
textAlignSelector = mkSelector "textAlign"

-- | @Selector@ for @setTextAlign:@
setTextAlignSelector :: Selector
setTextAlignSelector = mkSelector "setTextAlign:"

-- | @Selector@ for @textDecoration@
textDecorationSelector :: Selector
textDecorationSelector = mkSelector "textDecoration"

-- | @Selector@ for @setTextDecoration:@
setTextDecorationSelector :: Selector
setTextDecorationSelector = mkSelector "setTextDecoration:"

-- | @Selector@ for @textIndent@
textIndentSelector :: Selector
textIndentSelector = mkSelector "textIndent"

-- | @Selector@ for @setTextIndent:@
setTextIndentSelector :: Selector
setTextIndentSelector = mkSelector "setTextIndent:"

-- | @Selector@ for @textShadow@
textShadowSelector :: Selector
textShadowSelector = mkSelector "textShadow"

-- | @Selector@ for @setTextShadow:@
setTextShadowSelector :: Selector
setTextShadowSelector = mkSelector "setTextShadow:"

-- | @Selector@ for @textTransform@
textTransformSelector :: Selector
textTransformSelector = mkSelector "textTransform"

-- | @Selector@ for @setTextTransform:@
setTextTransformSelector :: Selector
setTextTransformSelector = mkSelector "setTextTransform:"

-- | @Selector@ for @top@
topSelector :: Selector
topSelector = mkSelector "top"

-- | @Selector@ for @setTop:@
setTopSelector :: Selector
setTopSelector = mkSelector "setTop:"

-- | @Selector@ for @unicodeBidi@
unicodeBidiSelector :: Selector
unicodeBidiSelector = mkSelector "unicodeBidi"

-- | @Selector@ for @setUnicodeBidi:@
setUnicodeBidiSelector :: Selector
setUnicodeBidiSelector = mkSelector "setUnicodeBidi:"

-- | @Selector@ for @verticalAlign@
verticalAlignSelector :: Selector
verticalAlignSelector = mkSelector "verticalAlign"

-- | @Selector@ for @setVerticalAlign:@
setVerticalAlignSelector :: Selector
setVerticalAlignSelector = mkSelector "setVerticalAlign:"

-- | @Selector@ for @visibility@
visibilitySelector :: Selector
visibilitySelector = mkSelector "visibility"

-- | @Selector@ for @setVisibility:@
setVisibilitySelector :: Selector
setVisibilitySelector = mkSelector "setVisibility:"

-- | @Selector@ for @voiceFamily@
voiceFamilySelector :: Selector
voiceFamilySelector = mkSelector "voiceFamily"

-- | @Selector@ for @setVoiceFamily:@
setVoiceFamilySelector :: Selector
setVoiceFamilySelector = mkSelector "setVoiceFamily:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @whiteSpace@
whiteSpaceSelector :: Selector
whiteSpaceSelector = mkSelector "whiteSpace"

-- | @Selector@ for @setWhiteSpace:@
setWhiteSpaceSelector :: Selector
setWhiteSpaceSelector = mkSelector "setWhiteSpace:"

-- | @Selector@ for @widows@
widowsSelector :: Selector
widowsSelector = mkSelector "widows"

-- | @Selector@ for @setWidows:@
setWidowsSelector :: Selector
setWidowsSelector = mkSelector "setWidows:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @wordSpacing@
wordSpacingSelector :: Selector
wordSpacingSelector = mkSelector "wordSpacing"

-- | @Selector@ for @setWordSpacing:@
setWordSpacingSelector :: Selector
setWordSpacingSelector = mkSelector "setWordSpacing:"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector
zIndexSelector = mkSelector "zIndex"

-- | @Selector@ for @setZIndex:@
setZIndexSelector :: Selector
setZIndexSelector = mkSelector "setZIndex:"

-- | @Selector@ for @setProperty:::@
setPropertySelector :: Selector
setPropertySelector = mkSelector "setProperty:::"

-- | @Selector@ for @cssText@
cssTextSelector :: Selector
cssTextSelector = mkSelector "cssText"

-- | @Selector@ for @setCssText:@
setCssTextSelector :: Selector
setCssTextSelector = mkSelector "setCssText:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @parentRule@
parentRuleSelector :: Selector
parentRuleSelector = mkSelector "parentRule"

