{-# LANGUAGE DataKinds #-}
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
  , azimuthSelector
  , backgroundAttachmentSelector
  , backgroundColorSelector
  , backgroundImageSelector
  , backgroundPositionSelector
  , backgroundRepeatSelector
  , backgroundSelector
  , borderBottomColorSelector
  , borderBottomSelector
  , borderBottomStyleSelector
  , borderBottomWidthSelector
  , borderCollapseSelector
  , borderColorSelector
  , borderLeftColorSelector
  , borderLeftSelector
  , borderLeftStyleSelector
  , borderLeftWidthSelector
  , borderRightColorSelector
  , borderRightSelector
  , borderRightStyleSelector
  , borderRightWidthSelector
  , borderSelector
  , borderSpacingSelector
  , borderStyleSelector
  , borderTopColorSelector
  , borderTopSelector
  , borderTopStyleSelector
  , borderTopWidthSelector
  , borderWidthSelector
  , bottomSelector
  , captionSideSelector
  , clearSelector
  , clipSelector
  , colorSelector
  , contentSelector
  , counterIncrementSelector
  , counterResetSelector
  , cssFloatSelector
  , cssTextSelector
  , cueAfterSelector
  , cueBeforeSelector
  , cueSelector
  , cursorSelector
  , directionSelector
  , displaySelector
  , elevationSelector
  , emptyCellsSelector
  , fontFamilySelector
  , fontSelector
  , fontSizeAdjustSelector
  , fontSizeSelector
  , fontStretchSelector
  , fontStyleSelector
  , fontVariantSelector
  , fontWeightSelector
  , getPropertyCSSValueSelector
  , getPropertyPrioritySelector
  , getPropertyShorthandSelector
  , getPropertyValueSelector
  , heightSelector
  , isPropertyImplicitSelector
  , itemSelector
  , leftSelector
  , lengthSelector
  , letterSpacingSelector
  , lineHeightSelector
  , listStyleImageSelector
  , listStylePositionSelector
  , listStyleSelector
  , listStyleTypeSelector
  , marginBottomSelector
  , marginLeftSelector
  , marginRightSelector
  , marginSelector
  , marginTopSelector
  , markerOffsetSelector
  , marksSelector
  , maxHeightSelector
  , maxWidthSelector
  , minHeightSelector
  , minWidthSelector
  , orphansSelector
  , outlineColorSelector
  , outlineSelector
  , outlineStyleSelector
  , outlineWidthSelector
  , overflowSelector
  , paddingBottomSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingSelector
  , paddingTopSelector
  , pageBreakAfterSelector
  , pageBreakBeforeSelector
  , pageBreakInsideSelector
  , pageSelector
  , parentRuleSelector
  , pauseAfterSelector
  , pauseBeforeSelector
  , pauseSelector
  , pitchRangeSelector
  , pitchSelector
  , playDuringSelector
  , positionSelector
  , quotesSelector
  , removePropertySelector
  , richnessSelector
  , rightSelector
  , setAzimuthSelector
  , setBackgroundAttachmentSelector
  , setBackgroundColorSelector
  , setBackgroundImageSelector
  , setBackgroundPositionSelector
  , setBackgroundRepeatSelector
  , setBackgroundSelector
  , setBorderBottomColorSelector
  , setBorderBottomSelector
  , setBorderBottomStyleSelector
  , setBorderBottomWidthSelector
  , setBorderCollapseSelector
  , setBorderColorSelector
  , setBorderLeftColorSelector
  , setBorderLeftSelector
  , setBorderLeftStyleSelector
  , setBorderLeftWidthSelector
  , setBorderRightColorSelector
  , setBorderRightSelector
  , setBorderRightStyleSelector
  , setBorderRightWidthSelector
  , setBorderSelector
  , setBorderSpacingSelector
  , setBorderStyleSelector
  , setBorderTopColorSelector
  , setBorderTopSelector
  , setBorderTopStyleSelector
  , setBorderTopWidthSelector
  , setBorderWidthSelector
  , setBottomSelector
  , setCaptionSideSelector
  , setClearSelector
  , setClipSelector
  , setColorSelector
  , setContentSelector
  , setCounterIncrementSelector
  , setCounterResetSelector
  , setCssFloatSelector
  , setCssTextSelector
  , setCueAfterSelector
  , setCueBeforeSelector
  , setCueSelector
  , setCursorSelector
  , setDirectionSelector
  , setDisplaySelector
  , setElevationSelector
  , setEmptyCellsSelector
  , setFontFamilySelector
  , setFontSelector
  , setFontSizeAdjustSelector
  , setFontSizeSelector
  , setFontStretchSelector
  , setFontStyleSelector
  , setFontVariantSelector
  , setFontWeightSelector
  , setHeightSelector
  , setLeftSelector
  , setLetterSpacingSelector
  , setLineHeightSelector
  , setListStyleImageSelector
  , setListStylePositionSelector
  , setListStyleSelector
  , setListStyleTypeSelector
  , setMarginBottomSelector
  , setMarginLeftSelector
  , setMarginRightSelector
  , setMarginSelector
  , setMarginTopSelector
  , setMarkerOffsetSelector
  , setMarksSelector
  , setMaxHeightSelector
  , setMaxWidthSelector
  , setMinHeightSelector
  , setMinWidthSelector
  , setOrphansSelector
  , setOutlineColorSelector
  , setOutlineSelector
  , setOutlineStyleSelector
  , setOutlineWidthSelector
  , setOverflowSelector
  , setPaddingBottomSelector
  , setPaddingLeftSelector
  , setPaddingRightSelector
  , setPaddingSelector
  , setPaddingTopSelector
  , setPageBreakAfterSelector
  , setPageBreakBeforeSelector
  , setPageBreakInsideSelector
  , setPageSelector
  , setPauseAfterSelector
  , setPauseBeforeSelector
  , setPauseSelector
  , setPitchRangeSelector
  , setPitchSelector
  , setPlayDuringSelector
  , setPositionSelector
  , setPropertySelector
  , setProperty_value_prioritySelector
  , setQuotesSelector
  , setRichnessSelector
  , setRightSelector
  , setSizeSelector
  , setSpeakHeaderSelector
  , setSpeakNumeralSelector
  , setSpeakPunctuationSelector
  , setSpeakSelector
  , setSpeechRateSelector
  , setStressSelector
  , setTableLayoutSelector
  , setTextAlignSelector
  , setTextDecorationSelector
  , setTextIndentSelector
  , setTextShadowSelector
  , setTextTransformSelector
  , setTopSelector
  , setUnicodeBidiSelector
  , setVerticalAlignSelector
  , setVisibilitySelector
  , setVoiceFamilySelector
  , setVolumeSelector
  , setWhiteSpaceSelector
  , setWidowsSelector
  , setWidthSelector
  , setWordSpacingSelector
  , setZIndexSelector
  , sizeSelector
  , speakHeaderSelector
  , speakNumeralSelector
  , speakPunctuationSelector
  , speakSelector
  , speechRateSelector
  , stressSelector
  , tableLayoutSelector
  , textAlignSelector
  , textDecorationSelector
  , textIndentSelector
  , textShadowSelector
  , textTransformSelector
  , topSelector
  , unicodeBidiSelector
  , verticalAlignSelector
  , visibilitySelector
  , voiceFamilySelector
  , volumeSelector
  , whiteSpaceSelector
  , widowsSelector
  , widthSelector
  , wordSpacingSelector
  , zIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getPropertyValue:@
getPropertyValue :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
getPropertyValue domcssStyleDeclaration propertyName =
  sendMessage domcssStyleDeclaration getPropertyValueSelector (toNSString propertyName)

-- | @- getPropertyCSSValue:@
getPropertyCSSValue :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id DOMCSSValue)
getPropertyCSSValue domcssStyleDeclaration propertyName =
  sendMessage domcssStyleDeclaration getPropertyCSSValueSelector (toNSString propertyName)

-- | @- removeProperty:@
removeProperty :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
removeProperty domcssStyleDeclaration propertyName =
  sendMessage domcssStyleDeclaration removePropertySelector (toNSString propertyName)

-- | @- getPropertyPriority:@
getPropertyPriority :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
getPropertyPriority domcssStyleDeclaration propertyName =
  sendMessage domcssStyleDeclaration getPropertyPrioritySelector (toNSString propertyName)

-- | @- setProperty:value:priority:@
setProperty_value_priority :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName, IsNSString value, IsNSString priority) => domcssStyleDeclaration -> propertyName -> value -> priority -> IO ()
setProperty_value_priority domcssStyleDeclaration propertyName value priority =
  sendMessage domcssStyleDeclaration setProperty_value_prioritySelector (toNSString propertyName) (toNSString value) (toNSString priority)

-- | @- item:@
item :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> CUInt -> IO (Id NSString)
item domcssStyleDeclaration index =
  sendMessage domcssStyleDeclaration itemSelector index

-- | @- getPropertyShorthand:@
getPropertyShorthand :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO (Id NSString)
getPropertyShorthand domcssStyleDeclaration propertyName =
  sendMessage domcssStyleDeclaration getPropertyShorthandSelector (toNSString propertyName)

-- | @- isPropertyImplicit:@
isPropertyImplicit :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName) => domcssStyleDeclaration -> propertyName -> IO Bool
isPropertyImplicit domcssStyleDeclaration propertyName =
  sendMessage domcssStyleDeclaration isPropertyImplicitSelector (toNSString propertyName)

-- | @- azimuth@
azimuth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
azimuth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration azimuthSelector

-- | @- setAzimuth:@
setAzimuth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString azimuth) => domcssStyleDeclaration -> azimuth -> IO ()
setAzimuth domcssStyleDeclaration azimuth =
  sendMessage domcssStyleDeclaration setAzimuthSelector (toNSString azimuth)

-- | @- background@
background :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
background domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration backgroundSelector

-- | @- setBackground:@
setBackground :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString background) => domcssStyleDeclaration -> background -> IO ()
setBackground domcssStyleDeclaration background =
  sendMessage domcssStyleDeclaration setBackgroundSelector (toNSString background)

-- | @- backgroundAttachment@
backgroundAttachment :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundAttachment domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration backgroundAttachmentSelector

-- | @- setBackgroundAttachment:@
setBackgroundAttachment :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundAttachment) => domcssStyleDeclaration -> backgroundAttachment -> IO ()
setBackgroundAttachment domcssStyleDeclaration backgroundAttachment =
  sendMessage domcssStyleDeclaration setBackgroundAttachmentSelector (toNSString backgroundAttachment)

-- | @- backgroundColor@
backgroundColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundColor) => domcssStyleDeclaration -> backgroundColor -> IO ()
setBackgroundColor domcssStyleDeclaration backgroundColor =
  sendMessage domcssStyleDeclaration setBackgroundColorSelector (toNSString backgroundColor)

-- | @- backgroundImage@
backgroundImage :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundImage domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration backgroundImageSelector

-- | @- setBackgroundImage:@
setBackgroundImage :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundImage) => domcssStyleDeclaration -> backgroundImage -> IO ()
setBackgroundImage domcssStyleDeclaration backgroundImage =
  sendMessage domcssStyleDeclaration setBackgroundImageSelector (toNSString backgroundImage)

-- | @- backgroundPosition@
backgroundPosition :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundPosition domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration backgroundPositionSelector

-- | @- setBackgroundPosition:@
setBackgroundPosition :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundPosition) => domcssStyleDeclaration -> backgroundPosition -> IO ()
setBackgroundPosition domcssStyleDeclaration backgroundPosition =
  sendMessage domcssStyleDeclaration setBackgroundPositionSelector (toNSString backgroundPosition)

-- | @- backgroundRepeat@
backgroundRepeat :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
backgroundRepeat domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration backgroundRepeatSelector

-- | @- setBackgroundRepeat:@
setBackgroundRepeat :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString backgroundRepeat) => domcssStyleDeclaration -> backgroundRepeat -> IO ()
setBackgroundRepeat domcssStyleDeclaration backgroundRepeat =
  sendMessage domcssStyleDeclaration setBackgroundRepeatSelector (toNSString backgroundRepeat)

-- | @- border@
border :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
border domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderSelector

-- | @- setBorder:@
setBorder :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString border) => domcssStyleDeclaration -> border -> IO ()
setBorder domcssStyleDeclaration border =
  sendMessage domcssStyleDeclaration setBorderSelector (toNSString border)

-- | @- borderCollapse@
borderCollapse :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderCollapse domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderCollapseSelector

-- | @- setBorderCollapse:@
setBorderCollapse :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderCollapse) => domcssStyleDeclaration -> borderCollapse -> IO ()
setBorderCollapse domcssStyleDeclaration borderCollapse =
  sendMessage domcssStyleDeclaration setBorderCollapseSelector (toNSString borderCollapse)

-- | @- borderColor@
borderColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderColorSelector

-- | @- setBorderColor:@
setBorderColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderColor) => domcssStyleDeclaration -> borderColor -> IO ()
setBorderColor domcssStyleDeclaration borderColor =
  sendMessage domcssStyleDeclaration setBorderColorSelector (toNSString borderColor)

-- | @- borderSpacing@
borderSpacing :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderSpacing domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderSpacingSelector

-- | @- setBorderSpacing:@
setBorderSpacing :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderSpacing) => domcssStyleDeclaration -> borderSpacing -> IO ()
setBorderSpacing domcssStyleDeclaration borderSpacing =
  sendMessage domcssStyleDeclaration setBorderSpacingSelector (toNSString borderSpacing)

-- | @- borderStyle@
borderStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderStyleSelector

-- | @- setBorderStyle:@
setBorderStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderStyle) => domcssStyleDeclaration -> borderStyle -> IO ()
setBorderStyle domcssStyleDeclaration borderStyle =
  sendMessage domcssStyleDeclaration setBorderStyleSelector (toNSString borderStyle)

-- | @- borderTop@
borderTop :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTop domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderTopSelector

-- | @- setBorderTop:@
setBorderTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTop) => domcssStyleDeclaration -> borderTop -> IO ()
setBorderTop domcssStyleDeclaration borderTop =
  sendMessage domcssStyleDeclaration setBorderTopSelector (toNSString borderTop)

-- | @- borderRight@
borderRight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderRightSelector

-- | @- setBorderRight:@
setBorderRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRight) => domcssStyleDeclaration -> borderRight -> IO ()
setBorderRight domcssStyleDeclaration borderRight =
  sendMessage domcssStyleDeclaration setBorderRightSelector (toNSString borderRight)

-- | @- borderBottom@
borderBottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottom domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderBottomSelector

-- | @- setBorderBottom:@
setBorderBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottom) => domcssStyleDeclaration -> borderBottom -> IO ()
setBorderBottom domcssStyleDeclaration borderBottom =
  sendMessage domcssStyleDeclaration setBorderBottomSelector (toNSString borderBottom)

-- | @- borderLeft@
borderLeft :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeft domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderLeftSelector

-- | @- setBorderLeft:@
setBorderLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeft) => domcssStyleDeclaration -> borderLeft -> IO ()
setBorderLeft domcssStyleDeclaration borderLeft =
  sendMessage domcssStyleDeclaration setBorderLeftSelector (toNSString borderLeft)

-- | @- borderTopColor@
borderTopColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTopColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderTopColorSelector

-- | @- setBorderTopColor:@
setBorderTopColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTopColor) => domcssStyleDeclaration -> borderTopColor -> IO ()
setBorderTopColor domcssStyleDeclaration borderTopColor =
  sendMessage domcssStyleDeclaration setBorderTopColorSelector (toNSString borderTopColor)

-- | @- borderRightColor@
borderRightColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRightColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderRightColorSelector

-- | @- setBorderRightColor:@
setBorderRightColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRightColor) => domcssStyleDeclaration -> borderRightColor -> IO ()
setBorderRightColor domcssStyleDeclaration borderRightColor =
  sendMessage domcssStyleDeclaration setBorderRightColorSelector (toNSString borderRightColor)

-- | @- borderBottomColor@
borderBottomColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottomColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderBottomColorSelector

-- | @- setBorderBottomColor:@
setBorderBottomColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottomColor) => domcssStyleDeclaration -> borderBottomColor -> IO ()
setBorderBottomColor domcssStyleDeclaration borderBottomColor =
  sendMessage domcssStyleDeclaration setBorderBottomColorSelector (toNSString borderBottomColor)

-- | @- borderLeftColor@
borderLeftColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeftColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderLeftColorSelector

-- | @- setBorderLeftColor:@
setBorderLeftColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeftColor) => domcssStyleDeclaration -> borderLeftColor -> IO ()
setBorderLeftColor domcssStyleDeclaration borderLeftColor =
  sendMessage domcssStyleDeclaration setBorderLeftColorSelector (toNSString borderLeftColor)

-- | @- borderTopStyle@
borderTopStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTopStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderTopStyleSelector

-- | @- setBorderTopStyle:@
setBorderTopStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTopStyle) => domcssStyleDeclaration -> borderTopStyle -> IO ()
setBorderTopStyle domcssStyleDeclaration borderTopStyle =
  sendMessage domcssStyleDeclaration setBorderTopStyleSelector (toNSString borderTopStyle)

-- | @- borderRightStyle@
borderRightStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRightStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderRightStyleSelector

-- | @- setBorderRightStyle:@
setBorderRightStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRightStyle) => domcssStyleDeclaration -> borderRightStyle -> IO ()
setBorderRightStyle domcssStyleDeclaration borderRightStyle =
  sendMessage domcssStyleDeclaration setBorderRightStyleSelector (toNSString borderRightStyle)

-- | @- borderBottomStyle@
borderBottomStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottomStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderBottomStyleSelector

-- | @- setBorderBottomStyle:@
setBorderBottomStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottomStyle) => domcssStyleDeclaration -> borderBottomStyle -> IO ()
setBorderBottomStyle domcssStyleDeclaration borderBottomStyle =
  sendMessage domcssStyleDeclaration setBorderBottomStyleSelector (toNSString borderBottomStyle)

-- | @- borderLeftStyle@
borderLeftStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeftStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderLeftStyleSelector

-- | @- setBorderLeftStyle:@
setBorderLeftStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeftStyle) => domcssStyleDeclaration -> borderLeftStyle -> IO ()
setBorderLeftStyle domcssStyleDeclaration borderLeftStyle =
  sendMessage domcssStyleDeclaration setBorderLeftStyleSelector (toNSString borderLeftStyle)

-- | @- borderTopWidth@
borderTopWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderTopWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderTopWidthSelector

-- | @- setBorderTopWidth:@
setBorderTopWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderTopWidth) => domcssStyleDeclaration -> borderTopWidth -> IO ()
setBorderTopWidth domcssStyleDeclaration borderTopWidth =
  sendMessage domcssStyleDeclaration setBorderTopWidthSelector (toNSString borderTopWidth)

-- | @- borderRightWidth@
borderRightWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderRightWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderRightWidthSelector

-- | @- setBorderRightWidth:@
setBorderRightWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderRightWidth) => domcssStyleDeclaration -> borderRightWidth -> IO ()
setBorderRightWidth domcssStyleDeclaration borderRightWidth =
  sendMessage domcssStyleDeclaration setBorderRightWidthSelector (toNSString borderRightWidth)

-- | @- borderBottomWidth@
borderBottomWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderBottomWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderBottomWidthSelector

-- | @- setBorderBottomWidth:@
setBorderBottomWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderBottomWidth) => domcssStyleDeclaration -> borderBottomWidth -> IO ()
setBorderBottomWidth domcssStyleDeclaration borderBottomWidth =
  sendMessage domcssStyleDeclaration setBorderBottomWidthSelector (toNSString borderBottomWidth)

-- | @- borderLeftWidth@
borderLeftWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderLeftWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderLeftWidthSelector

-- | @- setBorderLeftWidth:@
setBorderLeftWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderLeftWidth) => domcssStyleDeclaration -> borderLeftWidth -> IO ()
setBorderLeftWidth domcssStyleDeclaration borderLeftWidth =
  sendMessage domcssStyleDeclaration setBorderLeftWidthSelector (toNSString borderLeftWidth)

-- | @- borderWidth@
borderWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
borderWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration borderWidthSelector

-- | @- setBorderWidth:@
setBorderWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString borderWidth) => domcssStyleDeclaration -> borderWidth -> IO ()
setBorderWidth domcssStyleDeclaration borderWidth =
  sendMessage domcssStyleDeclaration setBorderWidthSelector (toNSString borderWidth)

-- | @- bottom@
bottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
bottom domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration bottomSelector

-- | @- setBottom:@
setBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString bottom) => domcssStyleDeclaration -> bottom -> IO ()
setBottom domcssStyleDeclaration bottom =
  sendMessage domcssStyleDeclaration setBottomSelector (toNSString bottom)

-- | @- captionSide@
captionSide :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
captionSide domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration captionSideSelector

-- | @- setCaptionSide:@
setCaptionSide :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString captionSide) => domcssStyleDeclaration -> captionSide -> IO ()
setCaptionSide domcssStyleDeclaration captionSide =
  sendMessage domcssStyleDeclaration setCaptionSideSelector (toNSString captionSide)

-- | @- clear@
clear :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
clear domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration clearSelector

-- | @- setClear:@
setClear :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString clear) => domcssStyleDeclaration -> clear -> IO ()
setClear domcssStyleDeclaration clear =
  sendMessage domcssStyleDeclaration setClearSelector (toNSString clear)

-- | @- clip@
clip :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
clip domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration clipSelector

-- | @- setClip:@
setClip :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString clip) => domcssStyleDeclaration -> clip -> IO ()
setClip domcssStyleDeclaration clip =
  sendMessage domcssStyleDeclaration setClipSelector (toNSString clip)

-- | @- color@
color :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
color domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration colorSelector

-- | @- setColor:@
setColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString color) => domcssStyleDeclaration -> color -> IO ()
setColor domcssStyleDeclaration color =
  sendMessage domcssStyleDeclaration setColorSelector (toNSString color)

-- | @- content@
content :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
content domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration contentSelector

-- | @- setContent:@
setContent :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString content) => domcssStyleDeclaration -> content -> IO ()
setContent domcssStyleDeclaration content =
  sendMessage domcssStyleDeclaration setContentSelector (toNSString content)

-- | @- counterIncrement@
counterIncrement :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
counterIncrement domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration counterIncrementSelector

-- | @- setCounterIncrement:@
setCounterIncrement :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString counterIncrement) => domcssStyleDeclaration -> counterIncrement -> IO ()
setCounterIncrement domcssStyleDeclaration counterIncrement =
  sendMessage domcssStyleDeclaration setCounterIncrementSelector (toNSString counterIncrement)

-- | @- counterReset@
counterReset :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
counterReset domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration counterResetSelector

-- | @- setCounterReset:@
setCounterReset :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString counterReset) => domcssStyleDeclaration -> counterReset -> IO ()
setCounterReset domcssStyleDeclaration counterReset =
  sendMessage domcssStyleDeclaration setCounterResetSelector (toNSString counterReset)

-- | @- cue@
cue :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cue domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration cueSelector

-- | @- setCue:@
setCue :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cue) => domcssStyleDeclaration -> cue -> IO ()
setCue domcssStyleDeclaration cue =
  sendMessage domcssStyleDeclaration setCueSelector (toNSString cue)

-- | @- cueAfter@
cueAfter :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cueAfter domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration cueAfterSelector

-- | @- setCueAfter:@
setCueAfter :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cueAfter) => domcssStyleDeclaration -> cueAfter -> IO ()
setCueAfter domcssStyleDeclaration cueAfter =
  sendMessage domcssStyleDeclaration setCueAfterSelector (toNSString cueAfter)

-- | @- cueBefore@
cueBefore :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cueBefore domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration cueBeforeSelector

-- | @- setCueBefore:@
setCueBefore :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cueBefore) => domcssStyleDeclaration -> cueBefore -> IO ()
setCueBefore domcssStyleDeclaration cueBefore =
  sendMessage domcssStyleDeclaration setCueBeforeSelector (toNSString cueBefore)

-- | @- cursor@
cursor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cursor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration cursorSelector

-- | @- setCursor:@
setCursor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cursor) => domcssStyleDeclaration -> cursor -> IO ()
setCursor domcssStyleDeclaration cursor =
  sendMessage domcssStyleDeclaration setCursorSelector (toNSString cursor)

-- | @- direction@
direction :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
direction domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration directionSelector

-- | @- setDirection:@
setDirection :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString direction) => domcssStyleDeclaration -> direction -> IO ()
setDirection domcssStyleDeclaration direction =
  sendMessage domcssStyleDeclaration setDirectionSelector (toNSString direction)

-- | @- display@
display :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
display domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration displaySelector

-- | @- setDisplay:@
setDisplay :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString display) => domcssStyleDeclaration -> display -> IO ()
setDisplay domcssStyleDeclaration display =
  sendMessage domcssStyleDeclaration setDisplaySelector (toNSString display)

-- | @- elevation@
elevation :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
elevation domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration elevationSelector

-- | @- setElevation:@
setElevation :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString elevation) => domcssStyleDeclaration -> elevation -> IO ()
setElevation domcssStyleDeclaration elevation =
  sendMessage domcssStyleDeclaration setElevationSelector (toNSString elevation)

-- | @- emptyCells@
emptyCells :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
emptyCells domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration emptyCellsSelector

-- | @- setEmptyCells:@
setEmptyCells :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString emptyCells) => domcssStyleDeclaration -> emptyCells -> IO ()
setEmptyCells domcssStyleDeclaration emptyCells =
  sendMessage domcssStyleDeclaration setEmptyCellsSelector (toNSString emptyCells)

-- | @- cssFloat@
cssFloat :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cssFloat domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration cssFloatSelector

-- | @- setCssFloat:@
setCssFloat :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString cssFloat) => domcssStyleDeclaration -> cssFloat -> IO ()
setCssFloat domcssStyleDeclaration cssFloat =
  sendMessage domcssStyleDeclaration setCssFloatSelector (toNSString cssFloat)

-- | @- font@
font :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
font domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontSelector

-- | @- setFont:@
setFont :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString font) => domcssStyleDeclaration -> font -> IO ()
setFont domcssStyleDeclaration font =
  sendMessage domcssStyleDeclaration setFontSelector (toNSString font)

-- | @- fontFamily@
fontFamily :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontFamily domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontFamilySelector

-- | @- setFontFamily:@
setFontFamily :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontFamily) => domcssStyleDeclaration -> fontFamily -> IO ()
setFontFamily domcssStyleDeclaration fontFamily =
  sendMessage domcssStyleDeclaration setFontFamilySelector (toNSString fontFamily)

-- | @- fontSize@
fontSize :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontSize domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontSizeSelector

-- | @- setFontSize:@
setFontSize :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontSize) => domcssStyleDeclaration -> fontSize -> IO ()
setFontSize domcssStyleDeclaration fontSize =
  sendMessage domcssStyleDeclaration setFontSizeSelector (toNSString fontSize)

-- | @- fontSizeAdjust@
fontSizeAdjust :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontSizeAdjust domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontSizeAdjustSelector

-- | @- setFontSizeAdjust:@
setFontSizeAdjust :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontSizeAdjust) => domcssStyleDeclaration -> fontSizeAdjust -> IO ()
setFontSizeAdjust domcssStyleDeclaration fontSizeAdjust =
  sendMessage domcssStyleDeclaration setFontSizeAdjustSelector (toNSString fontSizeAdjust)

-- | @- fontStretch@
fontStretch :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontStretch domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontStretchSelector

-- | @- setFontStretch:@
setFontStretch :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontStretch) => domcssStyleDeclaration -> fontStretch -> IO ()
setFontStretch domcssStyleDeclaration fontStretch =
  sendMessage domcssStyleDeclaration setFontStretchSelector (toNSString fontStretch)

-- | @- fontStyle@
fontStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontStyleSelector

-- | @- setFontStyle:@
setFontStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontStyle) => domcssStyleDeclaration -> fontStyle -> IO ()
setFontStyle domcssStyleDeclaration fontStyle =
  sendMessage domcssStyleDeclaration setFontStyleSelector (toNSString fontStyle)

-- | @- fontVariant@
fontVariant :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontVariant domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontVariantSelector

-- | @- setFontVariant:@
setFontVariant :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontVariant) => domcssStyleDeclaration -> fontVariant -> IO ()
setFontVariant domcssStyleDeclaration fontVariant =
  sendMessage domcssStyleDeclaration setFontVariantSelector (toNSString fontVariant)

-- | @- fontWeight@
fontWeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
fontWeight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration fontWeightSelector

-- | @- setFontWeight:@
setFontWeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString fontWeight) => domcssStyleDeclaration -> fontWeight -> IO ()
setFontWeight domcssStyleDeclaration fontWeight =
  sendMessage domcssStyleDeclaration setFontWeightSelector (toNSString fontWeight)

-- | @- height@
height :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
height domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration heightSelector

-- | @- setHeight:@
setHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString height) => domcssStyleDeclaration -> height -> IO ()
setHeight domcssStyleDeclaration height =
  sendMessage domcssStyleDeclaration setHeightSelector (toNSString height)

-- | @- left@
left :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
left domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration leftSelector

-- | @- setLeft:@
setLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString left) => domcssStyleDeclaration -> left -> IO ()
setLeft domcssStyleDeclaration left =
  sendMessage domcssStyleDeclaration setLeftSelector (toNSString left)

-- | @- letterSpacing@
letterSpacing :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
letterSpacing domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration letterSpacingSelector

-- | @- setLetterSpacing:@
setLetterSpacing :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString letterSpacing) => domcssStyleDeclaration -> letterSpacing -> IO ()
setLetterSpacing domcssStyleDeclaration letterSpacing =
  sendMessage domcssStyleDeclaration setLetterSpacingSelector (toNSString letterSpacing)

-- | @- lineHeight@
lineHeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
lineHeight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration lineHeightSelector

-- | @- setLineHeight:@
setLineHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString lineHeight) => domcssStyleDeclaration -> lineHeight -> IO ()
setLineHeight domcssStyleDeclaration lineHeight =
  sendMessage domcssStyleDeclaration setLineHeightSelector (toNSString lineHeight)

-- | @- listStyle@
listStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration listStyleSelector

-- | @- setListStyle:@
setListStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStyle) => domcssStyleDeclaration -> listStyle -> IO ()
setListStyle domcssStyleDeclaration listStyle =
  sendMessage domcssStyleDeclaration setListStyleSelector (toNSString listStyle)

-- | @- listStyleImage@
listStyleImage :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStyleImage domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration listStyleImageSelector

-- | @- setListStyleImage:@
setListStyleImage :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStyleImage) => domcssStyleDeclaration -> listStyleImage -> IO ()
setListStyleImage domcssStyleDeclaration listStyleImage =
  sendMessage domcssStyleDeclaration setListStyleImageSelector (toNSString listStyleImage)

-- | @- listStylePosition@
listStylePosition :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStylePosition domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration listStylePositionSelector

-- | @- setListStylePosition:@
setListStylePosition :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStylePosition) => domcssStyleDeclaration -> listStylePosition -> IO ()
setListStylePosition domcssStyleDeclaration listStylePosition =
  sendMessage domcssStyleDeclaration setListStylePositionSelector (toNSString listStylePosition)

-- | @- listStyleType@
listStyleType :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
listStyleType domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration listStyleTypeSelector

-- | @- setListStyleType:@
setListStyleType :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString listStyleType) => domcssStyleDeclaration -> listStyleType -> IO ()
setListStyleType domcssStyleDeclaration listStyleType =
  sendMessage domcssStyleDeclaration setListStyleTypeSelector (toNSString listStyleType)

-- | @- margin@
margin :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
margin domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration marginSelector

-- | @- setMargin:@
setMargin :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString margin) => domcssStyleDeclaration -> margin -> IO ()
setMargin domcssStyleDeclaration margin =
  sendMessage domcssStyleDeclaration setMarginSelector (toNSString margin)

-- | @- marginTop@
marginTop :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginTop domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration marginTopSelector

-- | @- setMarginTop:@
setMarginTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginTop) => domcssStyleDeclaration -> marginTop -> IO ()
setMarginTop domcssStyleDeclaration marginTop =
  sendMessage domcssStyleDeclaration setMarginTopSelector (toNSString marginTop)

-- | @- marginRight@
marginRight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginRight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration marginRightSelector

-- | @- setMarginRight:@
setMarginRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginRight) => domcssStyleDeclaration -> marginRight -> IO ()
setMarginRight domcssStyleDeclaration marginRight =
  sendMessage domcssStyleDeclaration setMarginRightSelector (toNSString marginRight)

-- | @- marginBottom@
marginBottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginBottom domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration marginBottomSelector

-- | @- setMarginBottom:@
setMarginBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginBottom) => domcssStyleDeclaration -> marginBottom -> IO ()
setMarginBottom domcssStyleDeclaration marginBottom =
  sendMessage domcssStyleDeclaration setMarginBottomSelector (toNSString marginBottom)

-- | @- marginLeft@
marginLeft :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marginLeft domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration marginLeftSelector

-- | @- setMarginLeft:@
setMarginLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marginLeft) => domcssStyleDeclaration -> marginLeft -> IO ()
setMarginLeft domcssStyleDeclaration marginLeft =
  sendMessage domcssStyleDeclaration setMarginLeftSelector (toNSString marginLeft)

-- | @- markerOffset@
markerOffset :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
markerOffset domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration markerOffsetSelector

-- | @- setMarkerOffset:@
setMarkerOffset :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString markerOffset) => domcssStyleDeclaration -> markerOffset -> IO ()
setMarkerOffset domcssStyleDeclaration markerOffset =
  sendMessage domcssStyleDeclaration setMarkerOffsetSelector (toNSString markerOffset)

-- | @- marks@
marks :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
marks domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration marksSelector

-- | @- setMarks:@
setMarks :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString marks) => domcssStyleDeclaration -> marks -> IO ()
setMarks domcssStyleDeclaration marks =
  sendMessage domcssStyleDeclaration setMarksSelector (toNSString marks)

-- | @- maxHeight@
maxHeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
maxHeight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration maxHeightSelector

-- | @- setMaxHeight:@
setMaxHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString maxHeight) => domcssStyleDeclaration -> maxHeight -> IO ()
setMaxHeight domcssStyleDeclaration maxHeight =
  sendMessage domcssStyleDeclaration setMaxHeightSelector (toNSString maxHeight)

-- | @- maxWidth@
maxWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
maxWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration maxWidthSelector

-- | @- setMaxWidth:@
setMaxWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString maxWidth) => domcssStyleDeclaration -> maxWidth -> IO ()
setMaxWidth domcssStyleDeclaration maxWidth =
  sendMessage domcssStyleDeclaration setMaxWidthSelector (toNSString maxWidth)

-- | @- minHeight@
minHeight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
minHeight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration minHeightSelector

-- | @- setMinHeight:@
setMinHeight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString minHeight) => domcssStyleDeclaration -> minHeight -> IO ()
setMinHeight domcssStyleDeclaration minHeight =
  sendMessage domcssStyleDeclaration setMinHeightSelector (toNSString minHeight)

-- | @- minWidth@
minWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
minWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration minWidthSelector

-- | @- setMinWidth:@
setMinWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString minWidth) => domcssStyleDeclaration -> minWidth -> IO ()
setMinWidth domcssStyleDeclaration minWidth =
  sendMessage domcssStyleDeclaration setMinWidthSelector (toNSString minWidth)

-- | @- orphans@
orphans :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
orphans domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration orphansSelector

-- | @- setOrphans:@
setOrphans :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString orphans) => domcssStyleDeclaration -> orphans -> IO ()
setOrphans domcssStyleDeclaration orphans =
  sendMessage domcssStyleDeclaration setOrphansSelector (toNSString orphans)

-- | @- outline@
outline :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outline domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration outlineSelector

-- | @- setOutline:@
setOutline :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outline) => domcssStyleDeclaration -> outline -> IO ()
setOutline domcssStyleDeclaration outline =
  sendMessage domcssStyleDeclaration setOutlineSelector (toNSString outline)

-- | @- outlineColor@
outlineColor :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outlineColor domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration outlineColorSelector

-- | @- setOutlineColor:@
setOutlineColor :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outlineColor) => domcssStyleDeclaration -> outlineColor -> IO ()
setOutlineColor domcssStyleDeclaration outlineColor =
  sendMessage domcssStyleDeclaration setOutlineColorSelector (toNSString outlineColor)

-- | @- outlineStyle@
outlineStyle :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outlineStyle domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration outlineStyleSelector

-- | @- setOutlineStyle:@
setOutlineStyle :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outlineStyle) => domcssStyleDeclaration -> outlineStyle -> IO ()
setOutlineStyle domcssStyleDeclaration outlineStyle =
  sendMessage domcssStyleDeclaration setOutlineStyleSelector (toNSString outlineStyle)

-- | @- outlineWidth@
outlineWidth :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
outlineWidth domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration outlineWidthSelector

-- | @- setOutlineWidth:@
setOutlineWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString outlineWidth) => domcssStyleDeclaration -> outlineWidth -> IO ()
setOutlineWidth domcssStyleDeclaration outlineWidth =
  sendMessage domcssStyleDeclaration setOutlineWidthSelector (toNSString outlineWidth)

-- | @- overflow@
overflow :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
overflow domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration overflowSelector

-- | @- setOverflow:@
setOverflow :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString overflow) => domcssStyleDeclaration -> overflow -> IO ()
setOverflow domcssStyleDeclaration overflow =
  sendMessage domcssStyleDeclaration setOverflowSelector (toNSString overflow)

-- | @- padding@
padding :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
padding domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration paddingSelector

-- | @- setPadding:@
setPadding :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString padding) => domcssStyleDeclaration -> padding -> IO ()
setPadding domcssStyleDeclaration padding =
  sendMessage domcssStyleDeclaration setPaddingSelector (toNSString padding)

-- | @- paddingTop@
paddingTop :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingTop domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration paddingTopSelector

-- | @- setPaddingTop:@
setPaddingTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingTop) => domcssStyleDeclaration -> paddingTop -> IO ()
setPaddingTop domcssStyleDeclaration paddingTop =
  sendMessage domcssStyleDeclaration setPaddingTopSelector (toNSString paddingTop)

-- | @- paddingRight@
paddingRight :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingRight domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration paddingRightSelector

-- | @- setPaddingRight:@
setPaddingRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingRight) => domcssStyleDeclaration -> paddingRight -> IO ()
setPaddingRight domcssStyleDeclaration paddingRight =
  sendMessage domcssStyleDeclaration setPaddingRightSelector (toNSString paddingRight)

-- | @- paddingBottom@
paddingBottom :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingBottom domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration paddingBottomSelector

-- | @- setPaddingBottom:@
setPaddingBottom :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingBottom) => domcssStyleDeclaration -> paddingBottom -> IO ()
setPaddingBottom domcssStyleDeclaration paddingBottom =
  sendMessage domcssStyleDeclaration setPaddingBottomSelector (toNSString paddingBottom)

-- | @- paddingLeft@
paddingLeft :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
paddingLeft domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration paddingLeftSelector

-- | @- setPaddingLeft:@
setPaddingLeft :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString paddingLeft) => domcssStyleDeclaration -> paddingLeft -> IO ()
setPaddingLeft domcssStyleDeclaration paddingLeft =
  sendMessage domcssStyleDeclaration setPaddingLeftSelector (toNSString paddingLeft)

-- | @- page@
page :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
page domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pageSelector

-- | @- setPage:@
setPage :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString page) => domcssStyleDeclaration -> page -> IO ()
setPage domcssStyleDeclaration page =
  sendMessage domcssStyleDeclaration setPageSelector (toNSString page)

-- | @- pageBreakAfter@
pageBreakAfter :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pageBreakAfter domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pageBreakAfterSelector

-- | @- setPageBreakAfter:@
setPageBreakAfter :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pageBreakAfter) => domcssStyleDeclaration -> pageBreakAfter -> IO ()
setPageBreakAfter domcssStyleDeclaration pageBreakAfter =
  sendMessage domcssStyleDeclaration setPageBreakAfterSelector (toNSString pageBreakAfter)

-- | @- pageBreakBefore@
pageBreakBefore :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pageBreakBefore domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pageBreakBeforeSelector

-- | @- setPageBreakBefore:@
setPageBreakBefore :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pageBreakBefore) => domcssStyleDeclaration -> pageBreakBefore -> IO ()
setPageBreakBefore domcssStyleDeclaration pageBreakBefore =
  sendMessage domcssStyleDeclaration setPageBreakBeforeSelector (toNSString pageBreakBefore)

-- | @- pageBreakInside@
pageBreakInside :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pageBreakInside domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pageBreakInsideSelector

-- | @- setPageBreakInside:@
setPageBreakInside :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pageBreakInside) => domcssStyleDeclaration -> pageBreakInside -> IO ()
setPageBreakInside domcssStyleDeclaration pageBreakInside =
  sendMessage domcssStyleDeclaration setPageBreakInsideSelector (toNSString pageBreakInside)

-- | @- pause@
pause :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pause domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pauseSelector

-- | @- setPause:@
setPause :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pause) => domcssStyleDeclaration -> pause -> IO ()
setPause domcssStyleDeclaration pause =
  sendMessage domcssStyleDeclaration setPauseSelector (toNSString pause)

-- | @- pauseAfter@
pauseAfter :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pauseAfter domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pauseAfterSelector

-- | @- setPauseAfter:@
setPauseAfter :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pauseAfter) => domcssStyleDeclaration -> pauseAfter -> IO ()
setPauseAfter domcssStyleDeclaration pauseAfter =
  sendMessage domcssStyleDeclaration setPauseAfterSelector (toNSString pauseAfter)

-- | @- pauseBefore@
pauseBefore :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pauseBefore domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pauseBeforeSelector

-- | @- setPauseBefore:@
setPauseBefore :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pauseBefore) => domcssStyleDeclaration -> pauseBefore -> IO ()
setPauseBefore domcssStyleDeclaration pauseBefore =
  sendMessage domcssStyleDeclaration setPauseBeforeSelector (toNSString pauseBefore)

-- | @- pitch@
pitch :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pitch domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pitchSelector

-- | @- setPitch:@
setPitch :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pitch) => domcssStyleDeclaration -> pitch -> IO ()
setPitch domcssStyleDeclaration pitch =
  sendMessage domcssStyleDeclaration setPitchSelector (toNSString pitch)

-- | @- pitchRange@
pitchRange :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
pitchRange domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration pitchRangeSelector

-- | @- setPitchRange:@
setPitchRange :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString pitchRange) => domcssStyleDeclaration -> pitchRange -> IO ()
setPitchRange domcssStyleDeclaration pitchRange =
  sendMessage domcssStyleDeclaration setPitchRangeSelector (toNSString pitchRange)

-- | @- playDuring@
playDuring :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
playDuring domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration playDuringSelector

-- | @- setPlayDuring:@
setPlayDuring :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString playDuring) => domcssStyleDeclaration -> playDuring -> IO ()
setPlayDuring domcssStyleDeclaration playDuring =
  sendMessage domcssStyleDeclaration setPlayDuringSelector (toNSString playDuring)

-- | @- position@
position :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
position domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration positionSelector

-- | @- setPosition:@
setPosition :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString position) => domcssStyleDeclaration -> position -> IO ()
setPosition domcssStyleDeclaration position =
  sendMessage domcssStyleDeclaration setPositionSelector (toNSString position)

-- | @- quotes@
quotes :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
quotes domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration quotesSelector

-- | @- setQuotes:@
setQuotes :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString quotes) => domcssStyleDeclaration -> quotes -> IO ()
setQuotes domcssStyleDeclaration quotes =
  sendMessage domcssStyleDeclaration setQuotesSelector (toNSString quotes)

-- | @- richness@
richness :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
richness domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration richnessSelector

-- | @- setRichness:@
setRichness :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString richness) => domcssStyleDeclaration -> richness -> IO ()
setRichness domcssStyleDeclaration richness =
  sendMessage domcssStyleDeclaration setRichnessSelector (toNSString richness)

-- | @- right@
right :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
right domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration rightSelector

-- | @- setRight:@
setRight :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString right) => domcssStyleDeclaration -> right -> IO ()
setRight domcssStyleDeclaration right =
  sendMessage domcssStyleDeclaration setRightSelector (toNSString right)

-- | @- size@
size :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
size domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration sizeSelector

-- | @- setSize:@
setSize :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString size) => domcssStyleDeclaration -> size -> IO ()
setSize domcssStyleDeclaration size =
  sendMessage domcssStyleDeclaration setSizeSelector (toNSString size)

-- | @- speak@
speak :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speak domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration speakSelector

-- | @- setSpeak:@
setSpeak :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speak) => domcssStyleDeclaration -> speak -> IO ()
setSpeak domcssStyleDeclaration speak =
  sendMessage domcssStyleDeclaration setSpeakSelector (toNSString speak)

-- | @- speakHeader@
speakHeader :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speakHeader domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration speakHeaderSelector

-- | @- setSpeakHeader:@
setSpeakHeader :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speakHeader) => domcssStyleDeclaration -> speakHeader -> IO ()
setSpeakHeader domcssStyleDeclaration speakHeader =
  sendMessage domcssStyleDeclaration setSpeakHeaderSelector (toNSString speakHeader)

-- | @- speakNumeral@
speakNumeral :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speakNumeral domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration speakNumeralSelector

-- | @- setSpeakNumeral:@
setSpeakNumeral :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speakNumeral) => domcssStyleDeclaration -> speakNumeral -> IO ()
setSpeakNumeral domcssStyleDeclaration speakNumeral =
  sendMessage domcssStyleDeclaration setSpeakNumeralSelector (toNSString speakNumeral)

-- | @- speakPunctuation@
speakPunctuation :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speakPunctuation domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration speakPunctuationSelector

-- | @- setSpeakPunctuation:@
setSpeakPunctuation :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speakPunctuation) => domcssStyleDeclaration -> speakPunctuation -> IO ()
setSpeakPunctuation domcssStyleDeclaration speakPunctuation =
  sendMessage domcssStyleDeclaration setSpeakPunctuationSelector (toNSString speakPunctuation)

-- | @- speechRate@
speechRate :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
speechRate domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration speechRateSelector

-- | @- setSpeechRate:@
setSpeechRate :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString speechRate) => domcssStyleDeclaration -> speechRate -> IO ()
setSpeechRate domcssStyleDeclaration speechRate =
  sendMessage domcssStyleDeclaration setSpeechRateSelector (toNSString speechRate)

-- | @- stress@
stress :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
stress domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration stressSelector

-- | @- setStress:@
setStress :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString stress) => domcssStyleDeclaration -> stress -> IO ()
setStress domcssStyleDeclaration stress =
  sendMessage domcssStyleDeclaration setStressSelector (toNSString stress)

-- | @- tableLayout@
tableLayout :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
tableLayout domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration tableLayoutSelector

-- | @- setTableLayout:@
setTableLayout :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString tableLayout) => domcssStyleDeclaration -> tableLayout -> IO ()
setTableLayout domcssStyleDeclaration tableLayout =
  sendMessage domcssStyleDeclaration setTableLayoutSelector (toNSString tableLayout)

-- | @- textAlign@
textAlign :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textAlign domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration textAlignSelector

-- | @- setTextAlign:@
setTextAlign :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textAlign) => domcssStyleDeclaration -> textAlign -> IO ()
setTextAlign domcssStyleDeclaration textAlign =
  sendMessage domcssStyleDeclaration setTextAlignSelector (toNSString textAlign)

-- | @- textDecoration@
textDecoration :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textDecoration domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration textDecorationSelector

-- | @- setTextDecoration:@
setTextDecoration :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textDecoration) => domcssStyleDeclaration -> textDecoration -> IO ()
setTextDecoration domcssStyleDeclaration textDecoration =
  sendMessage domcssStyleDeclaration setTextDecorationSelector (toNSString textDecoration)

-- | @- textIndent@
textIndent :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textIndent domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration textIndentSelector

-- | @- setTextIndent:@
setTextIndent :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textIndent) => domcssStyleDeclaration -> textIndent -> IO ()
setTextIndent domcssStyleDeclaration textIndent =
  sendMessage domcssStyleDeclaration setTextIndentSelector (toNSString textIndent)

-- | @- textShadow@
textShadow :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textShadow domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration textShadowSelector

-- | @- setTextShadow:@
setTextShadow :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textShadow) => domcssStyleDeclaration -> textShadow -> IO ()
setTextShadow domcssStyleDeclaration textShadow =
  sendMessage domcssStyleDeclaration setTextShadowSelector (toNSString textShadow)

-- | @- textTransform@
textTransform :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
textTransform domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration textTransformSelector

-- | @- setTextTransform:@
setTextTransform :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString textTransform) => domcssStyleDeclaration -> textTransform -> IO ()
setTextTransform domcssStyleDeclaration textTransform =
  sendMessage domcssStyleDeclaration setTextTransformSelector (toNSString textTransform)

-- | @- top@
top :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
top domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration topSelector

-- | @- setTop:@
setTop :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString top) => domcssStyleDeclaration -> top -> IO ()
setTop domcssStyleDeclaration top =
  sendMessage domcssStyleDeclaration setTopSelector (toNSString top)

-- | @- unicodeBidi@
unicodeBidi :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
unicodeBidi domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration unicodeBidiSelector

-- | @- setUnicodeBidi:@
setUnicodeBidi :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString unicodeBidi) => domcssStyleDeclaration -> unicodeBidi -> IO ()
setUnicodeBidi domcssStyleDeclaration unicodeBidi =
  sendMessage domcssStyleDeclaration setUnicodeBidiSelector (toNSString unicodeBidi)

-- | @- verticalAlign@
verticalAlign :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
verticalAlign domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration verticalAlignSelector

-- | @- setVerticalAlign:@
setVerticalAlign :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString verticalAlign) => domcssStyleDeclaration -> verticalAlign -> IO ()
setVerticalAlign domcssStyleDeclaration verticalAlign =
  sendMessage domcssStyleDeclaration setVerticalAlignSelector (toNSString verticalAlign)

-- | @- visibility@
visibility :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
visibility domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration visibilitySelector

-- | @- setVisibility:@
setVisibility :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString visibility) => domcssStyleDeclaration -> visibility -> IO ()
setVisibility domcssStyleDeclaration visibility =
  sendMessage domcssStyleDeclaration setVisibilitySelector (toNSString visibility)

-- | @- voiceFamily@
voiceFamily :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
voiceFamily domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration voiceFamilySelector

-- | @- setVoiceFamily:@
setVoiceFamily :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString voiceFamily) => domcssStyleDeclaration -> voiceFamily -> IO ()
setVoiceFamily domcssStyleDeclaration voiceFamily =
  sendMessage domcssStyleDeclaration setVoiceFamilySelector (toNSString voiceFamily)

-- | @- volume@
volume :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
volume domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration volumeSelector

-- | @- setVolume:@
setVolume :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString volume) => domcssStyleDeclaration -> volume -> IO ()
setVolume domcssStyleDeclaration volume =
  sendMessage domcssStyleDeclaration setVolumeSelector (toNSString volume)

-- | @- whiteSpace@
whiteSpace :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
whiteSpace domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration whiteSpaceSelector

-- | @- setWhiteSpace:@
setWhiteSpace :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString whiteSpace) => domcssStyleDeclaration -> whiteSpace -> IO ()
setWhiteSpace domcssStyleDeclaration whiteSpace =
  sendMessage domcssStyleDeclaration setWhiteSpaceSelector (toNSString whiteSpace)

-- | @- widows@
widows :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
widows domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration widowsSelector

-- | @- setWidows:@
setWidows :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString widows) => domcssStyleDeclaration -> widows -> IO ()
setWidows domcssStyleDeclaration widows =
  sendMessage domcssStyleDeclaration setWidowsSelector (toNSString widows)

-- | @- width@
width :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
width domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString width) => domcssStyleDeclaration -> width -> IO ()
setWidth domcssStyleDeclaration width =
  sendMessage domcssStyleDeclaration setWidthSelector (toNSString width)

-- | @- wordSpacing@
wordSpacing :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
wordSpacing domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration wordSpacingSelector

-- | @- setWordSpacing:@
setWordSpacing :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString wordSpacing) => domcssStyleDeclaration -> wordSpacing -> IO ()
setWordSpacing domcssStyleDeclaration wordSpacing =
  sendMessage domcssStyleDeclaration setWordSpacingSelector (toNSString wordSpacing)

-- | @- zIndex@
zIndex :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
zIndex domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration zIndexSelector

-- | @- setZIndex:@
setZIndex :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString zIndex) => domcssStyleDeclaration -> zIndex -> IO ()
setZIndex domcssStyleDeclaration zIndex =
  sendMessage domcssStyleDeclaration setZIndexSelector (toNSString zIndex)

-- | @- setProperty:::@
setProperty :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString propertyName, IsNSString value, IsNSString priority) => domcssStyleDeclaration -> propertyName -> value -> priority -> IO ()
setProperty domcssStyleDeclaration propertyName value priority =
  sendMessage domcssStyleDeclaration setPropertySelector (toNSString propertyName) (toNSString value) (toNSString priority)

-- | @- cssText@
cssText :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id NSString)
cssText domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration cssTextSelector

-- | @- setCssText:@
setCssText :: (IsDOMCSSStyleDeclaration domcssStyleDeclaration, IsNSString value) => domcssStyleDeclaration -> value -> IO ()
setCssText domcssStyleDeclaration value =
  sendMessage domcssStyleDeclaration setCssTextSelector (toNSString value)

-- | @- length@
length_ :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO CUInt
length_ domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration lengthSelector

-- | @- parentRule@
parentRule :: IsDOMCSSStyleDeclaration domcssStyleDeclaration => domcssStyleDeclaration -> IO (Id DOMCSSRule)
parentRule domcssStyleDeclaration =
  sendMessage domcssStyleDeclaration parentRuleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getPropertyValue:@
getPropertyValueSelector :: Selector '[Id NSString] (Id NSString)
getPropertyValueSelector = mkSelector "getPropertyValue:"

-- | @Selector@ for @getPropertyCSSValue:@
getPropertyCSSValueSelector :: Selector '[Id NSString] (Id DOMCSSValue)
getPropertyCSSValueSelector = mkSelector "getPropertyCSSValue:"

-- | @Selector@ for @removeProperty:@
removePropertySelector :: Selector '[Id NSString] (Id NSString)
removePropertySelector = mkSelector "removeProperty:"

-- | @Selector@ for @getPropertyPriority:@
getPropertyPrioritySelector :: Selector '[Id NSString] (Id NSString)
getPropertyPrioritySelector = mkSelector "getPropertyPriority:"

-- | @Selector@ for @setProperty:value:priority:@
setProperty_value_prioritySelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
setProperty_value_prioritySelector = mkSelector "setProperty:value:priority:"

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id NSString)
itemSelector = mkSelector "item:"

-- | @Selector@ for @getPropertyShorthand:@
getPropertyShorthandSelector :: Selector '[Id NSString] (Id NSString)
getPropertyShorthandSelector = mkSelector "getPropertyShorthand:"

-- | @Selector@ for @isPropertyImplicit:@
isPropertyImplicitSelector :: Selector '[Id NSString] Bool
isPropertyImplicitSelector = mkSelector "isPropertyImplicit:"

-- | @Selector@ for @azimuth@
azimuthSelector :: Selector '[] (Id NSString)
azimuthSelector = mkSelector "azimuth"

-- | @Selector@ for @setAzimuth:@
setAzimuthSelector :: Selector '[Id NSString] ()
setAzimuthSelector = mkSelector "setAzimuth:"

-- | @Selector@ for @background@
backgroundSelector :: Selector '[] (Id NSString)
backgroundSelector = mkSelector "background"

-- | @Selector@ for @setBackground:@
setBackgroundSelector :: Selector '[Id NSString] ()
setBackgroundSelector = mkSelector "setBackground:"

-- | @Selector@ for @backgroundAttachment@
backgroundAttachmentSelector :: Selector '[] (Id NSString)
backgroundAttachmentSelector = mkSelector "backgroundAttachment"

-- | @Selector@ for @setBackgroundAttachment:@
setBackgroundAttachmentSelector :: Selector '[Id NSString] ()
setBackgroundAttachmentSelector = mkSelector "setBackgroundAttachment:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSString)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSString] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @backgroundImage@
backgroundImageSelector :: Selector '[] (Id NSString)
backgroundImageSelector = mkSelector "backgroundImage"

-- | @Selector@ for @setBackgroundImage:@
setBackgroundImageSelector :: Selector '[Id NSString] ()
setBackgroundImageSelector = mkSelector "setBackgroundImage:"

-- | @Selector@ for @backgroundPosition@
backgroundPositionSelector :: Selector '[] (Id NSString)
backgroundPositionSelector = mkSelector "backgroundPosition"

-- | @Selector@ for @setBackgroundPosition:@
setBackgroundPositionSelector :: Selector '[Id NSString] ()
setBackgroundPositionSelector = mkSelector "setBackgroundPosition:"

-- | @Selector@ for @backgroundRepeat@
backgroundRepeatSelector :: Selector '[] (Id NSString)
backgroundRepeatSelector = mkSelector "backgroundRepeat"

-- | @Selector@ for @setBackgroundRepeat:@
setBackgroundRepeatSelector :: Selector '[Id NSString] ()
setBackgroundRepeatSelector = mkSelector "setBackgroundRepeat:"

-- | @Selector@ for @border@
borderSelector :: Selector '[] (Id NSString)
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector '[Id NSString] ()
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @borderCollapse@
borderCollapseSelector :: Selector '[] (Id NSString)
borderCollapseSelector = mkSelector "borderCollapse"

-- | @Selector@ for @setBorderCollapse:@
setBorderCollapseSelector :: Selector '[Id NSString] ()
setBorderCollapseSelector = mkSelector "setBorderCollapse:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector '[] (Id NSString)
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[Id NSString] ()
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @borderSpacing@
borderSpacingSelector :: Selector '[] (Id NSString)
borderSpacingSelector = mkSelector "borderSpacing"

-- | @Selector@ for @setBorderSpacing:@
setBorderSpacingSelector :: Selector '[Id NSString] ()
setBorderSpacingSelector = mkSelector "setBorderSpacing:"

-- | @Selector@ for @borderStyle@
borderStyleSelector :: Selector '[] (Id NSString)
borderStyleSelector = mkSelector "borderStyle"

-- | @Selector@ for @setBorderStyle:@
setBorderStyleSelector :: Selector '[Id NSString] ()
setBorderStyleSelector = mkSelector "setBorderStyle:"

-- | @Selector@ for @borderTop@
borderTopSelector :: Selector '[] (Id NSString)
borderTopSelector = mkSelector "borderTop"

-- | @Selector@ for @setBorderTop:@
setBorderTopSelector :: Selector '[Id NSString] ()
setBorderTopSelector = mkSelector "setBorderTop:"

-- | @Selector@ for @borderRight@
borderRightSelector :: Selector '[] (Id NSString)
borderRightSelector = mkSelector "borderRight"

-- | @Selector@ for @setBorderRight:@
setBorderRightSelector :: Selector '[Id NSString] ()
setBorderRightSelector = mkSelector "setBorderRight:"

-- | @Selector@ for @borderBottom@
borderBottomSelector :: Selector '[] (Id NSString)
borderBottomSelector = mkSelector "borderBottom"

-- | @Selector@ for @setBorderBottom:@
setBorderBottomSelector :: Selector '[Id NSString] ()
setBorderBottomSelector = mkSelector "setBorderBottom:"

-- | @Selector@ for @borderLeft@
borderLeftSelector :: Selector '[] (Id NSString)
borderLeftSelector = mkSelector "borderLeft"

-- | @Selector@ for @setBorderLeft:@
setBorderLeftSelector :: Selector '[Id NSString] ()
setBorderLeftSelector = mkSelector "setBorderLeft:"

-- | @Selector@ for @borderTopColor@
borderTopColorSelector :: Selector '[] (Id NSString)
borderTopColorSelector = mkSelector "borderTopColor"

-- | @Selector@ for @setBorderTopColor:@
setBorderTopColorSelector :: Selector '[Id NSString] ()
setBorderTopColorSelector = mkSelector "setBorderTopColor:"

-- | @Selector@ for @borderRightColor@
borderRightColorSelector :: Selector '[] (Id NSString)
borderRightColorSelector = mkSelector "borderRightColor"

-- | @Selector@ for @setBorderRightColor:@
setBorderRightColorSelector :: Selector '[Id NSString] ()
setBorderRightColorSelector = mkSelector "setBorderRightColor:"

-- | @Selector@ for @borderBottomColor@
borderBottomColorSelector :: Selector '[] (Id NSString)
borderBottomColorSelector = mkSelector "borderBottomColor"

-- | @Selector@ for @setBorderBottomColor:@
setBorderBottomColorSelector :: Selector '[Id NSString] ()
setBorderBottomColorSelector = mkSelector "setBorderBottomColor:"

-- | @Selector@ for @borderLeftColor@
borderLeftColorSelector :: Selector '[] (Id NSString)
borderLeftColorSelector = mkSelector "borderLeftColor"

-- | @Selector@ for @setBorderLeftColor:@
setBorderLeftColorSelector :: Selector '[Id NSString] ()
setBorderLeftColorSelector = mkSelector "setBorderLeftColor:"

-- | @Selector@ for @borderTopStyle@
borderTopStyleSelector :: Selector '[] (Id NSString)
borderTopStyleSelector = mkSelector "borderTopStyle"

-- | @Selector@ for @setBorderTopStyle:@
setBorderTopStyleSelector :: Selector '[Id NSString] ()
setBorderTopStyleSelector = mkSelector "setBorderTopStyle:"

-- | @Selector@ for @borderRightStyle@
borderRightStyleSelector :: Selector '[] (Id NSString)
borderRightStyleSelector = mkSelector "borderRightStyle"

-- | @Selector@ for @setBorderRightStyle:@
setBorderRightStyleSelector :: Selector '[Id NSString] ()
setBorderRightStyleSelector = mkSelector "setBorderRightStyle:"

-- | @Selector@ for @borderBottomStyle@
borderBottomStyleSelector :: Selector '[] (Id NSString)
borderBottomStyleSelector = mkSelector "borderBottomStyle"

-- | @Selector@ for @setBorderBottomStyle:@
setBorderBottomStyleSelector :: Selector '[Id NSString] ()
setBorderBottomStyleSelector = mkSelector "setBorderBottomStyle:"

-- | @Selector@ for @borderLeftStyle@
borderLeftStyleSelector :: Selector '[] (Id NSString)
borderLeftStyleSelector = mkSelector "borderLeftStyle"

-- | @Selector@ for @setBorderLeftStyle:@
setBorderLeftStyleSelector :: Selector '[Id NSString] ()
setBorderLeftStyleSelector = mkSelector "setBorderLeftStyle:"

-- | @Selector@ for @borderTopWidth@
borderTopWidthSelector :: Selector '[] (Id NSString)
borderTopWidthSelector = mkSelector "borderTopWidth"

-- | @Selector@ for @setBorderTopWidth:@
setBorderTopWidthSelector :: Selector '[Id NSString] ()
setBorderTopWidthSelector = mkSelector "setBorderTopWidth:"

-- | @Selector@ for @borderRightWidth@
borderRightWidthSelector :: Selector '[] (Id NSString)
borderRightWidthSelector = mkSelector "borderRightWidth"

-- | @Selector@ for @setBorderRightWidth:@
setBorderRightWidthSelector :: Selector '[Id NSString] ()
setBorderRightWidthSelector = mkSelector "setBorderRightWidth:"

-- | @Selector@ for @borderBottomWidth@
borderBottomWidthSelector :: Selector '[] (Id NSString)
borderBottomWidthSelector = mkSelector "borderBottomWidth"

-- | @Selector@ for @setBorderBottomWidth:@
setBorderBottomWidthSelector :: Selector '[Id NSString] ()
setBorderBottomWidthSelector = mkSelector "setBorderBottomWidth:"

-- | @Selector@ for @borderLeftWidth@
borderLeftWidthSelector :: Selector '[] (Id NSString)
borderLeftWidthSelector = mkSelector "borderLeftWidth"

-- | @Selector@ for @setBorderLeftWidth:@
setBorderLeftWidthSelector :: Selector '[Id NSString] ()
setBorderLeftWidthSelector = mkSelector "setBorderLeftWidth:"

-- | @Selector@ for @borderWidth@
borderWidthSelector :: Selector '[] (Id NSString)
borderWidthSelector = mkSelector "borderWidth"

-- | @Selector@ for @setBorderWidth:@
setBorderWidthSelector :: Selector '[Id NSString] ()
setBorderWidthSelector = mkSelector "setBorderWidth:"

-- | @Selector@ for @bottom@
bottomSelector :: Selector '[] (Id NSString)
bottomSelector = mkSelector "bottom"

-- | @Selector@ for @setBottom:@
setBottomSelector :: Selector '[Id NSString] ()
setBottomSelector = mkSelector "setBottom:"

-- | @Selector@ for @captionSide@
captionSideSelector :: Selector '[] (Id NSString)
captionSideSelector = mkSelector "captionSide"

-- | @Selector@ for @setCaptionSide:@
setCaptionSideSelector :: Selector '[Id NSString] ()
setCaptionSideSelector = mkSelector "setCaptionSide:"

-- | @Selector@ for @clear@
clearSelector :: Selector '[] (Id NSString)
clearSelector = mkSelector "clear"

-- | @Selector@ for @setClear:@
setClearSelector :: Selector '[Id NSString] ()
setClearSelector = mkSelector "setClear:"

-- | @Selector@ for @clip@
clipSelector :: Selector '[] (Id NSString)
clipSelector = mkSelector "clip"

-- | @Selector@ for @setClip:@
setClipSelector :: Selector '[Id NSString] ()
setClipSelector = mkSelector "setClip:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSString)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSString] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector '[Id NSString] ()
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @counterIncrement@
counterIncrementSelector :: Selector '[] (Id NSString)
counterIncrementSelector = mkSelector "counterIncrement"

-- | @Selector@ for @setCounterIncrement:@
setCounterIncrementSelector :: Selector '[Id NSString] ()
setCounterIncrementSelector = mkSelector "setCounterIncrement:"

-- | @Selector@ for @counterReset@
counterResetSelector :: Selector '[] (Id NSString)
counterResetSelector = mkSelector "counterReset"

-- | @Selector@ for @setCounterReset:@
setCounterResetSelector :: Selector '[Id NSString] ()
setCounterResetSelector = mkSelector "setCounterReset:"

-- | @Selector@ for @cue@
cueSelector :: Selector '[] (Id NSString)
cueSelector = mkSelector "cue"

-- | @Selector@ for @setCue:@
setCueSelector :: Selector '[Id NSString] ()
setCueSelector = mkSelector "setCue:"

-- | @Selector@ for @cueAfter@
cueAfterSelector :: Selector '[] (Id NSString)
cueAfterSelector = mkSelector "cueAfter"

-- | @Selector@ for @setCueAfter:@
setCueAfterSelector :: Selector '[Id NSString] ()
setCueAfterSelector = mkSelector "setCueAfter:"

-- | @Selector@ for @cueBefore@
cueBeforeSelector :: Selector '[] (Id NSString)
cueBeforeSelector = mkSelector "cueBefore"

-- | @Selector@ for @setCueBefore:@
setCueBeforeSelector :: Selector '[Id NSString] ()
setCueBeforeSelector = mkSelector "setCueBefore:"

-- | @Selector@ for @cursor@
cursorSelector :: Selector '[] (Id NSString)
cursorSelector = mkSelector "cursor"

-- | @Selector@ for @setCursor:@
setCursorSelector :: Selector '[Id NSString] ()
setCursorSelector = mkSelector "setCursor:"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] (Id NSString)
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[Id NSString] ()
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @display@
displaySelector :: Selector '[] (Id NSString)
displaySelector = mkSelector "display"

-- | @Selector@ for @setDisplay:@
setDisplaySelector :: Selector '[Id NSString] ()
setDisplaySelector = mkSelector "setDisplay:"

-- | @Selector@ for @elevation@
elevationSelector :: Selector '[] (Id NSString)
elevationSelector = mkSelector "elevation"

-- | @Selector@ for @setElevation:@
setElevationSelector :: Selector '[Id NSString] ()
setElevationSelector = mkSelector "setElevation:"

-- | @Selector@ for @emptyCells@
emptyCellsSelector :: Selector '[] (Id NSString)
emptyCellsSelector = mkSelector "emptyCells"

-- | @Selector@ for @setEmptyCells:@
setEmptyCellsSelector :: Selector '[Id NSString] ()
setEmptyCellsSelector = mkSelector "setEmptyCells:"

-- | @Selector@ for @cssFloat@
cssFloatSelector :: Selector '[] (Id NSString)
cssFloatSelector = mkSelector "cssFloat"

-- | @Selector@ for @setCssFloat:@
setCssFloatSelector :: Selector '[Id NSString] ()
setCssFloatSelector = mkSelector "setCssFloat:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSString)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSString] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontFamily@
fontFamilySelector :: Selector '[] (Id NSString)
fontFamilySelector = mkSelector "fontFamily"

-- | @Selector@ for @setFontFamily:@
setFontFamilySelector :: Selector '[Id NSString] ()
setFontFamilySelector = mkSelector "setFontFamily:"

-- | @Selector@ for @fontSize@
fontSizeSelector :: Selector '[] (Id NSString)
fontSizeSelector = mkSelector "fontSize"

-- | @Selector@ for @setFontSize:@
setFontSizeSelector :: Selector '[Id NSString] ()
setFontSizeSelector = mkSelector "setFontSize:"

-- | @Selector@ for @fontSizeAdjust@
fontSizeAdjustSelector :: Selector '[] (Id NSString)
fontSizeAdjustSelector = mkSelector "fontSizeAdjust"

-- | @Selector@ for @setFontSizeAdjust:@
setFontSizeAdjustSelector :: Selector '[Id NSString] ()
setFontSizeAdjustSelector = mkSelector "setFontSizeAdjust:"

-- | @Selector@ for @fontStretch@
fontStretchSelector :: Selector '[] (Id NSString)
fontStretchSelector = mkSelector "fontStretch"

-- | @Selector@ for @setFontStretch:@
setFontStretchSelector :: Selector '[Id NSString] ()
setFontStretchSelector = mkSelector "setFontStretch:"

-- | @Selector@ for @fontStyle@
fontStyleSelector :: Selector '[] (Id NSString)
fontStyleSelector = mkSelector "fontStyle"

-- | @Selector@ for @setFontStyle:@
setFontStyleSelector :: Selector '[Id NSString] ()
setFontStyleSelector = mkSelector "setFontStyle:"

-- | @Selector@ for @fontVariant@
fontVariantSelector :: Selector '[] (Id NSString)
fontVariantSelector = mkSelector "fontVariant"

-- | @Selector@ for @setFontVariant:@
setFontVariantSelector :: Selector '[Id NSString] ()
setFontVariantSelector = mkSelector "setFontVariant:"

-- | @Selector@ for @fontWeight@
fontWeightSelector :: Selector '[] (Id NSString)
fontWeightSelector = mkSelector "fontWeight"

-- | @Selector@ for @setFontWeight:@
setFontWeightSelector :: Selector '[Id NSString] ()
setFontWeightSelector = mkSelector "setFontWeight:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSString)
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[Id NSString] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @left@
leftSelector :: Selector '[] (Id NSString)
leftSelector = mkSelector "left"

-- | @Selector@ for @setLeft:@
setLeftSelector :: Selector '[Id NSString] ()
setLeftSelector = mkSelector "setLeft:"

-- | @Selector@ for @letterSpacing@
letterSpacingSelector :: Selector '[] (Id NSString)
letterSpacingSelector = mkSelector "letterSpacing"

-- | @Selector@ for @setLetterSpacing:@
setLetterSpacingSelector :: Selector '[Id NSString] ()
setLetterSpacingSelector = mkSelector "setLetterSpacing:"

-- | @Selector@ for @lineHeight@
lineHeightSelector :: Selector '[] (Id NSString)
lineHeightSelector = mkSelector "lineHeight"

-- | @Selector@ for @setLineHeight:@
setLineHeightSelector :: Selector '[Id NSString] ()
setLineHeightSelector = mkSelector "setLineHeight:"

-- | @Selector@ for @listStyle@
listStyleSelector :: Selector '[] (Id NSString)
listStyleSelector = mkSelector "listStyle"

-- | @Selector@ for @setListStyle:@
setListStyleSelector :: Selector '[Id NSString] ()
setListStyleSelector = mkSelector "setListStyle:"

-- | @Selector@ for @listStyleImage@
listStyleImageSelector :: Selector '[] (Id NSString)
listStyleImageSelector = mkSelector "listStyleImage"

-- | @Selector@ for @setListStyleImage:@
setListStyleImageSelector :: Selector '[Id NSString] ()
setListStyleImageSelector = mkSelector "setListStyleImage:"

-- | @Selector@ for @listStylePosition@
listStylePositionSelector :: Selector '[] (Id NSString)
listStylePositionSelector = mkSelector "listStylePosition"

-- | @Selector@ for @setListStylePosition:@
setListStylePositionSelector :: Selector '[Id NSString] ()
setListStylePositionSelector = mkSelector "setListStylePosition:"

-- | @Selector@ for @listStyleType@
listStyleTypeSelector :: Selector '[] (Id NSString)
listStyleTypeSelector = mkSelector "listStyleType"

-- | @Selector@ for @setListStyleType:@
setListStyleTypeSelector :: Selector '[Id NSString] ()
setListStyleTypeSelector = mkSelector "setListStyleType:"

-- | @Selector@ for @margin@
marginSelector :: Selector '[] (Id NSString)
marginSelector = mkSelector "margin"

-- | @Selector@ for @setMargin:@
setMarginSelector :: Selector '[Id NSString] ()
setMarginSelector = mkSelector "setMargin:"

-- | @Selector@ for @marginTop@
marginTopSelector :: Selector '[] (Id NSString)
marginTopSelector = mkSelector "marginTop"

-- | @Selector@ for @setMarginTop:@
setMarginTopSelector :: Selector '[Id NSString] ()
setMarginTopSelector = mkSelector "setMarginTop:"

-- | @Selector@ for @marginRight@
marginRightSelector :: Selector '[] (Id NSString)
marginRightSelector = mkSelector "marginRight"

-- | @Selector@ for @setMarginRight:@
setMarginRightSelector :: Selector '[Id NSString] ()
setMarginRightSelector = mkSelector "setMarginRight:"

-- | @Selector@ for @marginBottom@
marginBottomSelector :: Selector '[] (Id NSString)
marginBottomSelector = mkSelector "marginBottom"

-- | @Selector@ for @setMarginBottom:@
setMarginBottomSelector :: Selector '[Id NSString] ()
setMarginBottomSelector = mkSelector "setMarginBottom:"

-- | @Selector@ for @marginLeft@
marginLeftSelector :: Selector '[] (Id NSString)
marginLeftSelector = mkSelector "marginLeft"

-- | @Selector@ for @setMarginLeft:@
setMarginLeftSelector :: Selector '[Id NSString] ()
setMarginLeftSelector = mkSelector "setMarginLeft:"

-- | @Selector@ for @markerOffset@
markerOffsetSelector :: Selector '[] (Id NSString)
markerOffsetSelector = mkSelector "markerOffset"

-- | @Selector@ for @setMarkerOffset:@
setMarkerOffsetSelector :: Selector '[Id NSString] ()
setMarkerOffsetSelector = mkSelector "setMarkerOffset:"

-- | @Selector@ for @marks@
marksSelector :: Selector '[] (Id NSString)
marksSelector = mkSelector "marks"

-- | @Selector@ for @setMarks:@
setMarksSelector :: Selector '[Id NSString] ()
setMarksSelector = mkSelector "setMarks:"

-- | @Selector@ for @maxHeight@
maxHeightSelector :: Selector '[] (Id NSString)
maxHeightSelector = mkSelector "maxHeight"

-- | @Selector@ for @setMaxHeight:@
setMaxHeightSelector :: Selector '[Id NSString] ()
setMaxHeightSelector = mkSelector "setMaxHeight:"

-- | @Selector@ for @maxWidth@
maxWidthSelector :: Selector '[] (Id NSString)
maxWidthSelector = mkSelector "maxWidth"

-- | @Selector@ for @setMaxWidth:@
setMaxWidthSelector :: Selector '[Id NSString] ()
setMaxWidthSelector = mkSelector "setMaxWidth:"

-- | @Selector@ for @minHeight@
minHeightSelector :: Selector '[] (Id NSString)
minHeightSelector = mkSelector "minHeight"

-- | @Selector@ for @setMinHeight:@
setMinHeightSelector :: Selector '[Id NSString] ()
setMinHeightSelector = mkSelector "setMinHeight:"

-- | @Selector@ for @minWidth@
minWidthSelector :: Selector '[] (Id NSString)
minWidthSelector = mkSelector "minWidth"

-- | @Selector@ for @setMinWidth:@
setMinWidthSelector :: Selector '[Id NSString] ()
setMinWidthSelector = mkSelector "setMinWidth:"

-- | @Selector@ for @orphans@
orphansSelector :: Selector '[] (Id NSString)
orphansSelector = mkSelector "orphans"

-- | @Selector@ for @setOrphans:@
setOrphansSelector :: Selector '[Id NSString] ()
setOrphansSelector = mkSelector "setOrphans:"

-- | @Selector@ for @outline@
outlineSelector :: Selector '[] (Id NSString)
outlineSelector = mkSelector "outline"

-- | @Selector@ for @setOutline:@
setOutlineSelector :: Selector '[Id NSString] ()
setOutlineSelector = mkSelector "setOutline:"

-- | @Selector@ for @outlineColor@
outlineColorSelector :: Selector '[] (Id NSString)
outlineColorSelector = mkSelector "outlineColor"

-- | @Selector@ for @setOutlineColor:@
setOutlineColorSelector :: Selector '[Id NSString] ()
setOutlineColorSelector = mkSelector "setOutlineColor:"

-- | @Selector@ for @outlineStyle@
outlineStyleSelector :: Selector '[] (Id NSString)
outlineStyleSelector = mkSelector "outlineStyle"

-- | @Selector@ for @setOutlineStyle:@
setOutlineStyleSelector :: Selector '[Id NSString] ()
setOutlineStyleSelector = mkSelector "setOutlineStyle:"

-- | @Selector@ for @outlineWidth@
outlineWidthSelector :: Selector '[] (Id NSString)
outlineWidthSelector = mkSelector "outlineWidth"

-- | @Selector@ for @setOutlineWidth:@
setOutlineWidthSelector :: Selector '[Id NSString] ()
setOutlineWidthSelector = mkSelector "setOutlineWidth:"

-- | @Selector@ for @overflow@
overflowSelector :: Selector '[] (Id NSString)
overflowSelector = mkSelector "overflow"

-- | @Selector@ for @setOverflow:@
setOverflowSelector :: Selector '[Id NSString] ()
setOverflowSelector = mkSelector "setOverflow:"

-- | @Selector@ for @padding@
paddingSelector :: Selector '[] (Id NSString)
paddingSelector = mkSelector "padding"

-- | @Selector@ for @setPadding:@
setPaddingSelector :: Selector '[Id NSString] ()
setPaddingSelector = mkSelector "setPadding:"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector '[] (Id NSString)
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @setPaddingTop:@
setPaddingTopSelector :: Selector '[Id NSString] ()
setPaddingTopSelector = mkSelector "setPaddingTop:"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector '[] (Id NSString)
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @setPaddingRight:@
setPaddingRightSelector :: Selector '[Id NSString] ()
setPaddingRightSelector = mkSelector "setPaddingRight:"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector '[] (Id NSString)
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @setPaddingBottom:@
setPaddingBottomSelector :: Selector '[Id NSString] ()
setPaddingBottomSelector = mkSelector "setPaddingBottom:"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector '[] (Id NSString)
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @setPaddingLeft:@
setPaddingLeftSelector :: Selector '[Id NSString] ()
setPaddingLeftSelector = mkSelector "setPaddingLeft:"

-- | @Selector@ for @page@
pageSelector :: Selector '[] (Id NSString)
pageSelector = mkSelector "page"

-- | @Selector@ for @setPage:@
setPageSelector :: Selector '[Id NSString] ()
setPageSelector = mkSelector "setPage:"

-- | @Selector@ for @pageBreakAfter@
pageBreakAfterSelector :: Selector '[] (Id NSString)
pageBreakAfterSelector = mkSelector "pageBreakAfter"

-- | @Selector@ for @setPageBreakAfter:@
setPageBreakAfterSelector :: Selector '[Id NSString] ()
setPageBreakAfterSelector = mkSelector "setPageBreakAfter:"

-- | @Selector@ for @pageBreakBefore@
pageBreakBeforeSelector :: Selector '[] (Id NSString)
pageBreakBeforeSelector = mkSelector "pageBreakBefore"

-- | @Selector@ for @setPageBreakBefore:@
setPageBreakBeforeSelector :: Selector '[Id NSString] ()
setPageBreakBeforeSelector = mkSelector "setPageBreakBefore:"

-- | @Selector@ for @pageBreakInside@
pageBreakInsideSelector :: Selector '[] (Id NSString)
pageBreakInsideSelector = mkSelector "pageBreakInside"

-- | @Selector@ for @setPageBreakInside:@
setPageBreakInsideSelector :: Selector '[Id NSString] ()
setPageBreakInsideSelector = mkSelector "setPageBreakInside:"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] (Id NSString)
pauseSelector = mkSelector "pause"

-- | @Selector@ for @setPause:@
setPauseSelector :: Selector '[Id NSString] ()
setPauseSelector = mkSelector "setPause:"

-- | @Selector@ for @pauseAfter@
pauseAfterSelector :: Selector '[] (Id NSString)
pauseAfterSelector = mkSelector "pauseAfter"

-- | @Selector@ for @setPauseAfter:@
setPauseAfterSelector :: Selector '[Id NSString] ()
setPauseAfterSelector = mkSelector "setPauseAfter:"

-- | @Selector@ for @pauseBefore@
pauseBeforeSelector :: Selector '[] (Id NSString)
pauseBeforeSelector = mkSelector "pauseBefore"

-- | @Selector@ for @setPauseBefore:@
setPauseBeforeSelector :: Selector '[Id NSString] ()
setPauseBeforeSelector = mkSelector "setPauseBefore:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] (Id NSString)
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector '[Id NSString] ()
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @pitchRange@
pitchRangeSelector :: Selector '[] (Id NSString)
pitchRangeSelector = mkSelector "pitchRange"

-- | @Selector@ for @setPitchRange:@
setPitchRangeSelector :: Selector '[Id NSString] ()
setPitchRangeSelector = mkSelector "setPitchRange:"

-- | @Selector@ for @playDuring@
playDuringSelector :: Selector '[] (Id NSString)
playDuringSelector = mkSelector "playDuring"

-- | @Selector@ for @setPlayDuring:@
setPlayDuringSelector :: Selector '[Id NSString] ()
setPlayDuringSelector = mkSelector "setPlayDuring:"

-- | @Selector@ for @position@
positionSelector :: Selector '[] (Id NSString)
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector '[Id NSString] ()
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @quotes@
quotesSelector :: Selector '[] (Id NSString)
quotesSelector = mkSelector "quotes"

-- | @Selector@ for @setQuotes:@
setQuotesSelector :: Selector '[Id NSString] ()
setQuotesSelector = mkSelector "setQuotes:"

-- | @Selector@ for @richness@
richnessSelector :: Selector '[] (Id NSString)
richnessSelector = mkSelector "richness"

-- | @Selector@ for @setRichness:@
setRichnessSelector :: Selector '[Id NSString] ()
setRichnessSelector = mkSelector "setRichness:"

-- | @Selector@ for @right@
rightSelector :: Selector '[] (Id NSString)
rightSelector = mkSelector "right"

-- | @Selector@ for @setRight:@
setRightSelector :: Selector '[Id NSString] ()
setRightSelector = mkSelector "setRight:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] (Id NSString)
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[Id NSString] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @speak@
speakSelector :: Selector '[] (Id NSString)
speakSelector = mkSelector "speak"

-- | @Selector@ for @setSpeak:@
setSpeakSelector :: Selector '[Id NSString] ()
setSpeakSelector = mkSelector "setSpeak:"

-- | @Selector@ for @speakHeader@
speakHeaderSelector :: Selector '[] (Id NSString)
speakHeaderSelector = mkSelector "speakHeader"

-- | @Selector@ for @setSpeakHeader:@
setSpeakHeaderSelector :: Selector '[Id NSString] ()
setSpeakHeaderSelector = mkSelector "setSpeakHeader:"

-- | @Selector@ for @speakNumeral@
speakNumeralSelector :: Selector '[] (Id NSString)
speakNumeralSelector = mkSelector "speakNumeral"

-- | @Selector@ for @setSpeakNumeral:@
setSpeakNumeralSelector :: Selector '[Id NSString] ()
setSpeakNumeralSelector = mkSelector "setSpeakNumeral:"

-- | @Selector@ for @speakPunctuation@
speakPunctuationSelector :: Selector '[] (Id NSString)
speakPunctuationSelector = mkSelector "speakPunctuation"

-- | @Selector@ for @setSpeakPunctuation:@
setSpeakPunctuationSelector :: Selector '[Id NSString] ()
setSpeakPunctuationSelector = mkSelector "setSpeakPunctuation:"

-- | @Selector@ for @speechRate@
speechRateSelector :: Selector '[] (Id NSString)
speechRateSelector = mkSelector "speechRate"

-- | @Selector@ for @setSpeechRate:@
setSpeechRateSelector :: Selector '[Id NSString] ()
setSpeechRateSelector = mkSelector "setSpeechRate:"

-- | @Selector@ for @stress@
stressSelector :: Selector '[] (Id NSString)
stressSelector = mkSelector "stress"

-- | @Selector@ for @setStress:@
setStressSelector :: Selector '[Id NSString] ()
setStressSelector = mkSelector "setStress:"

-- | @Selector@ for @tableLayout@
tableLayoutSelector :: Selector '[] (Id NSString)
tableLayoutSelector = mkSelector "tableLayout"

-- | @Selector@ for @setTableLayout:@
setTableLayoutSelector :: Selector '[Id NSString] ()
setTableLayoutSelector = mkSelector "setTableLayout:"

-- | @Selector@ for @textAlign@
textAlignSelector :: Selector '[] (Id NSString)
textAlignSelector = mkSelector "textAlign"

-- | @Selector@ for @setTextAlign:@
setTextAlignSelector :: Selector '[Id NSString] ()
setTextAlignSelector = mkSelector "setTextAlign:"

-- | @Selector@ for @textDecoration@
textDecorationSelector :: Selector '[] (Id NSString)
textDecorationSelector = mkSelector "textDecoration"

-- | @Selector@ for @setTextDecoration:@
setTextDecorationSelector :: Selector '[Id NSString] ()
setTextDecorationSelector = mkSelector "setTextDecoration:"

-- | @Selector@ for @textIndent@
textIndentSelector :: Selector '[] (Id NSString)
textIndentSelector = mkSelector "textIndent"

-- | @Selector@ for @setTextIndent:@
setTextIndentSelector :: Selector '[Id NSString] ()
setTextIndentSelector = mkSelector "setTextIndent:"

-- | @Selector@ for @textShadow@
textShadowSelector :: Selector '[] (Id NSString)
textShadowSelector = mkSelector "textShadow"

-- | @Selector@ for @setTextShadow:@
setTextShadowSelector :: Selector '[Id NSString] ()
setTextShadowSelector = mkSelector "setTextShadow:"

-- | @Selector@ for @textTransform@
textTransformSelector :: Selector '[] (Id NSString)
textTransformSelector = mkSelector "textTransform"

-- | @Selector@ for @setTextTransform:@
setTextTransformSelector :: Selector '[Id NSString] ()
setTextTransformSelector = mkSelector "setTextTransform:"

-- | @Selector@ for @top@
topSelector :: Selector '[] (Id NSString)
topSelector = mkSelector "top"

-- | @Selector@ for @setTop:@
setTopSelector :: Selector '[Id NSString] ()
setTopSelector = mkSelector "setTop:"

-- | @Selector@ for @unicodeBidi@
unicodeBidiSelector :: Selector '[] (Id NSString)
unicodeBidiSelector = mkSelector "unicodeBidi"

-- | @Selector@ for @setUnicodeBidi:@
setUnicodeBidiSelector :: Selector '[Id NSString] ()
setUnicodeBidiSelector = mkSelector "setUnicodeBidi:"

-- | @Selector@ for @verticalAlign@
verticalAlignSelector :: Selector '[] (Id NSString)
verticalAlignSelector = mkSelector "verticalAlign"

-- | @Selector@ for @setVerticalAlign:@
setVerticalAlignSelector :: Selector '[Id NSString] ()
setVerticalAlignSelector = mkSelector "setVerticalAlign:"

-- | @Selector@ for @visibility@
visibilitySelector :: Selector '[] (Id NSString)
visibilitySelector = mkSelector "visibility"

-- | @Selector@ for @setVisibility:@
setVisibilitySelector :: Selector '[Id NSString] ()
setVisibilitySelector = mkSelector "setVisibility:"

-- | @Selector@ for @voiceFamily@
voiceFamilySelector :: Selector '[] (Id NSString)
voiceFamilySelector = mkSelector "voiceFamily"

-- | @Selector@ for @setVoiceFamily:@
setVoiceFamilySelector :: Selector '[Id NSString] ()
setVoiceFamilySelector = mkSelector "setVoiceFamily:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] (Id NSString)
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[Id NSString] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @whiteSpace@
whiteSpaceSelector :: Selector '[] (Id NSString)
whiteSpaceSelector = mkSelector "whiteSpace"

-- | @Selector@ for @setWhiteSpace:@
setWhiteSpaceSelector :: Selector '[Id NSString] ()
setWhiteSpaceSelector = mkSelector "setWhiteSpace:"

-- | @Selector@ for @widows@
widowsSelector :: Selector '[] (Id NSString)
widowsSelector = mkSelector "widows"

-- | @Selector@ for @setWidows:@
setWidowsSelector :: Selector '[Id NSString] ()
setWidowsSelector = mkSelector "setWidows:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSString)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSString] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @wordSpacing@
wordSpacingSelector :: Selector '[] (Id NSString)
wordSpacingSelector = mkSelector "wordSpacing"

-- | @Selector@ for @setWordSpacing:@
setWordSpacingSelector :: Selector '[Id NSString] ()
setWordSpacingSelector = mkSelector "setWordSpacing:"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector '[] (Id NSString)
zIndexSelector = mkSelector "zIndex"

-- | @Selector@ for @setZIndex:@
setZIndexSelector :: Selector '[Id NSString] ()
setZIndexSelector = mkSelector "setZIndex:"

-- | @Selector@ for @setProperty:::@
setPropertySelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
setPropertySelector = mkSelector "setProperty:::"

-- | @Selector@ for @cssText@
cssTextSelector :: Selector '[] (Id NSString)
cssTextSelector = mkSelector "cssText"

-- | @Selector@ for @setCssText:@
setCssTextSelector :: Selector '[Id NSString] ()
setCssTextSelector = mkSelector "setCssText:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

-- | @Selector@ for @parentRule@
parentRuleSelector :: Selector '[] (Id DOMCSSRule)
parentRuleSelector = mkSelector "parentRule"

