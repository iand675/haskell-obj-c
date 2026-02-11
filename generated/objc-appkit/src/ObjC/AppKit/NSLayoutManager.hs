{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutManager@.
module ObjC.AppKit.NSLayoutManager
  ( NSLayoutManager
  , IsNSLayoutManager(..)
  , init_
  , initWithCoder
  , replaceTextStorage
  , addTextContainer
  , insertTextContainer_atIndex
  , removeTextContainerAtIndex
  , textContainerChangedGeometry
  , textContainerChangedTextView
  , invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange
  , invalidateLayoutForCharacterRange_actualCharacterRange
  , invalidateDisplayForCharacterRange
  , invalidateDisplayForGlyphRange
  , processEditingForTextStorage_edited_range_changeInLength_invalidatedRange
  , ensureGlyphsForCharacterRange
  , ensureGlyphsForGlyphRange
  , ensureLayoutForCharacterRange
  , ensureLayoutForGlyphRange
  , ensureLayoutForTextContainer
  , ensureLayoutForBoundingRect_inTextContainer
  , cgGlyphAtIndex_isValidIndex
  , cgGlyphAtIndex
  , isValidGlyphIndex
  , propertyForGlyphAtIndex
  , characterIndexForGlyphAtIndex
  , glyphIndexForCharacterAtIndex
  , setTextContainer_forGlyphRange
  , setLineFragmentRect_forGlyphRange_usedRect
  , setExtraLineFragmentRect_usedRect_textContainer
  , setLocation_forStartOfGlyphRange
  , setNotShownAttribute_forGlyphAtIndex
  , setDrawsOutsideLineFragment_forGlyphAtIndex
  , setAttachmentSize_forGlyphRange
  , getFirstUnlaidCharacterIndex_glyphIndex
  , firstUnlaidCharacterIndex
  , firstUnlaidGlyphIndex
  , textContainerForGlyphAtIndex_effectiveRange
  , textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout
  , usedRectForTextContainer
  , lineFragmentRectForGlyphAtIndex_effectiveRange
  , lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout
  , lineFragmentUsedRectForGlyphAtIndex_effectiveRange
  , lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout
  , locationForGlyphAtIndex
  , notShownAttributeForGlyphAtIndex
  , drawsOutsideLineFragmentForGlyphAtIndex
  , attachmentSizeForGlyphAtIndex
  , truncatedGlyphRangeInLineFragmentForGlyphAtIndex
  , glyphRangeForCharacterRange_actualCharacterRange
  , characterRangeForGlyphRange_actualGlyphRange
  , glyphRangeForTextContainer
  , rangeOfNominallySpacedGlyphsContainingIndex
  , boundingRectForGlyphRange_inTextContainer
  , glyphRangeForBoundingRect_inTextContainer
  , glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer
  , glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph
  , glyphIndexForPoint_inTextContainer
  , fractionOfDistanceThroughGlyphForPoint_inTextContainer
  , characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPoints
  , getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes
  , enumerateLineFragmentsForGlyphRange_usingBlock
  , enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlock
  , drawBackgroundForGlyphRange_atPoint
  , drawGlyphsForGlyphRange_atPoint
  , fillBackgroundRectArray_count_forCharacterRange_color
  , drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin
  , underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin
  , drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin
  , strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin
  , showAttachmentCell_inRect_characterIndex
  , setLayoutRect_forTextBlock_glyphRange
  , setBoundsRect_forTextBlock_glyphRange
  , layoutRectForTextBlock_glyphRange
  , boundsRectForTextBlock_glyphRange
  , layoutRectForTextBlock_atIndex_effectiveRange
  , boundsRectForTextBlock_atIndex_effectiveRange
  , temporaryAttributesAtCharacterIndex_effectiveRange
  , setTemporaryAttributes_forCharacterRange
  , addTemporaryAttributes_forCharacterRange
  , removeTemporaryAttribute_forCharacterRange
  , temporaryAttribute_atCharacterIndex_effectiveRange
  , temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange
  , temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange
  , addTemporaryAttribute_value_forCharacterRange
  , defaultLineHeightForFont
  , defaultBaselineOffsetForFont
  , glyphAtIndex_isValidIndex
  , glyphAtIndex
  , rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount
  , rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount
  , substituteFontForFont
  , insertGlyph_atGlyphIndex_characterIndex
  , replaceGlyphAtIndex_withGlyph
  , deleteGlyphsInRange
  , setCharacterIndex_forGlyphAtIndex
  , setIntAttribute_value_forGlyphAtIndex
  , invalidateGlyphsOnLayoutInvalidationForGlyphRange
  , intAttribute_forGlyphAtIndex
  , invalidateLayoutForCharacterRange_isSoft_actualCharacterRange
  , textStorage_edited_range_changeInLength_invalidatedRange
  , setLocations_startingGlyphIndexes_count_forGlyphRange
  , showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment
  , rulerMarkersForTextView_paragraphStyle_ruler
  , rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled
  , layoutManagerOwnsFirstResponderInWindow
  , textStorage
  , setTextStorage
  , textContainers
  , showsInvisibleCharacters
  , setShowsInvisibleCharacters
  , showsControlCharacters
  , setShowsControlCharacters
  , usesDefaultHyphenation
  , setUsesDefaultHyphenation
  , usesFontLeading
  , setUsesFontLeading
  , allowsNonContiguousLayout
  , setAllowsNonContiguousLayout
  , hasNonContiguousLayout
  , limitsLayoutForSuspiciousContents
  , setLimitsLayoutForSuspiciousContents
  , backgroundLayoutEnabled
  , setBackgroundLayoutEnabled
  , defaultAttachmentScaling
  , setDefaultAttachmentScaling
  , typesetter
  , setTypesetter
  , typesetterBehavior
  , setTypesetterBehavior
  , numberOfGlyphs
  , extraLineFragmentRect
  , extraLineFragmentUsedRect
  , extraLineFragmentTextContainer
  , glyphGenerator
  , setGlyphGenerator
  , usesScreenFonts
  , setUsesScreenFonts
  , hyphenationFactor
  , setHyphenationFactor
  , firstTextView
  , textViewForBeginningOfSelection
  , initSelector
  , initWithCoderSelector
  , replaceTextStorageSelector
  , addTextContainerSelector
  , insertTextContainer_atIndexSelector
  , removeTextContainerAtIndexSelector
  , textContainerChangedGeometrySelector
  , textContainerChangedTextViewSelector
  , invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector
  , invalidateLayoutForCharacterRange_actualCharacterRangeSelector
  , invalidateDisplayForCharacterRangeSelector
  , invalidateDisplayForGlyphRangeSelector
  , processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector
  , ensureGlyphsForCharacterRangeSelector
  , ensureGlyphsForGlyphRangeSelector
  , ensureLayoutForCharacterRangeSelector
  , ensureLayoutForGlyphRangeSelector
  , ensureLayoutForTextContainerSelector
  , ensureLayoutForBoundingRect_inTextContainerSelector
  , cgGlyphAtIndex_isValidIndexSelector
  , cgGlyphAtIndexSelector
  , isValidGlyphIndexSelector
  , propertyForGlyphAtIndexSelector
  , characterIndexForGlyphAtIndexSelector
  , glyphIndexForCharacterAtIndexSelector
  , setTextContainer_forGlyphRangeSelector
  , setLineFragmentRect_forGlyphRange_usedRectSelector
  , setExtraLineFragmentRect_usedRect_textContainerSelector
  , setLocation_forStartOfGlyphRangeSelector
  , setNotShownAttribute_forGlyphAtIndexSelector
  , setDrawsOutsideLineFragment_forGlyphAtIndexSelector
  , setAttachmentSize_forGlyphRangeSelector
  , getFirstUnlaidCharacterIndex_glyphIndexSelector
  , firstUnlaidCharacterIndexSelector
  , firstUnlaidGlyphIndexSelector
  , textContainerForGlyphAtIndex_effectiveRangeSelector
  , textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector
  , usedRectForTextContainerSelector
  , lineFragmentRectForGlyphAtIndex_effectiveRangeSelector
  , lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector
  , lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector
  , lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector
  , locationForGlyphAtIndexSelector
  , notShownAttributeForGlyphAtIndexSelector
  , drawsOutsideLineFragmentForGlyphAtIndexSelector
  , attachmentSizeForGlyphAtIndexSelector
  , truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector
  , glyphRangeForCharacterRange_actualCharacterRangeSelector
  , characterRangeForGlyphRange_actualGlyphRangeSelector
  , glyphRangeForTextContainerSelector
  , rangeOfNominallySpacedGlyphsContainingIndexSelector
  , boundingRectForGlyphRange_inTextContainerSelector
  , glyphRangeForBoundingRect_inTextContainerSelector
  , glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector
  , glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector
  , glyphIndexForPoint_inTextContainerSelector
  , fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector
  , characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector
  , getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector
  , enumerateLineFragmentsForGlyphRange_usingBlockSelector
  , enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector
  , drawBackgroundForGlyphRange_atPointSelector
  , drawGlyphsForGlyphRange_atPointSelector
  , fillBackgroundRectArray_count_forCharacterRange_colorSelector
  , drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , showAttachmentCell_inRect_characterIndexSelector
  , setLayoutRect_forTextBlock_glyphRangeSelector
  , setBoundsRect_forTextBlock_glyphRangeSelector
  , layoutRectForTextBlock_glyphRangeSelector
  , boundsRectForTextBlock_glyphRangeSelector
  , layoutRectForTextBlock_atIndex_effectiveRangeSelector
  , boundsRectForTextBlock_atIndex_effectiveRangeSelector
  , temporaryAttributesAtCharacterIndex_effectiveRangeSelector
  , setTemporaryAttributes_forCharacterRangeSelector
  , addTemporaryAttributes_forCharacterRangeSelector
  , removeTemporaryAttribute_forCharacterRangeSelector
  , temporaryAttribute_atCharacterIndex_effectiveRangeSelector
  , temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector
  , temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector
  , addTemporaryAttribute_value_forCharacterRangeSelector
  , defaultLineHeightForFontSelector
  , defaultBaselineOffsetForFontSelector
  , glyphAtIndex_isValidIndexSelector
  , glyphAtIndexSelector
  , rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector
  , rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector
  , substituteFontForFontSelector
  , insertGlyph_atGlyphIndex_characterIndexSelector
  , replaceGlyphAtIndex_withGlyphSelector
  , deleteGlyphsInRangeSelector
  , setCharacterIndex_forGlyphAtIndexSelector
  , setIntAttribute_value_forGlyphAtIndexSelector
  , invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector
  , intAttribute_forGlyphAtIndexSelector
  , invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector
  , textStorage_edited_range_changeInLength_invalidatedRangeSelector
  , setLocations_startingGlyphIndexes_count_forGlyphRangeSelector
  , showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector
  , rulerMarkersForTextView_paragraphStyle_rulerSelector
  , rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector
  , layoutManagerOwnsFirstResponderInWindowSelector
  , textStorageSelector
  , setTextStorageSelector
  , textContainersSelector
  , showsInvisibleCharactersSelector
  , setShowsInvisibleCharactersSelector
  , showsControlCharactersSelector
  , setShowsControlCharactersSelector
  , usesDefaultHyphenationSelector
  , setUsesDefaultHyphenationSelector
  , usesFontLeadingSelector
  , setUsesFontLeadingSelector
  , allowsNonContiguousLayoutSelector
  , setAllowsNonContiguousLayoutSelector
  , hasNonContiguousLayoutSelector
  , limitsLayoutForSuspiciousContentsSelector
  , setLimitsLayoutForSuspiciousContentsSelector
  , backgroundLayoutEnabledSelector
  , setBackgroundLayoutEnabledSelector
  , defaultAttachmentScalingSelector
  , setDefaultAttachmentScalingSelector
  , typesetterSelector
  , setTypesetterSelector
  , typesetterBehaviorSelector
  , setTypesetterBehaviorSelector
  , numberOfGlyphsSelector
  , extraLineFragmentRectSelector
  , extraLineFragmentUsedRectSelector
  , extraLineFragmentTextContainerSelector
  , glyphGeneratorSelector
  , setGlyphGeneratorSelector
  , usesScreenFontsSelector
  , setUsesScreenFontsSelector
  , hyphenationFactorSelector
  , setHyphenationFactorSelector
  , firstTextViewSelector
  , textViewForBeginningOfSelectionSelector

  -- * Enum types
  , NSGlyphProperty(NSGlyphProperty)
  , pattern NSGlyphPropertyNull
  , pattern NSGlyphPropertyControlCharacter
  , pattern NSGlyphPropertyElastic
  , pattern NSGlyphPropertyNonBaseCharacter
  , NSImageScaling(NSImageScaling)
  , pattern NSImageScaleProportionallyDown
  , pattern NSImageScaleAxesIndependently
  , pattern NSImageScaleNone
  , pattern NSImageScaleProportionallyUpOrDown
  , pattern NSScaleProportionally
  , pattern NSScaleToFit
  , pattern NSScaleNone
  , NSTextStorageEditActions(NSTextStorageEditActions)
  , pattern NSTextStorageEditedAttributes
  , pattern NSTextStorageEditedCharacters
  , NSTypesetterBehavior(NSTypesetterBehavior)
  , pattern NSTypesetterLatestBehavior
  , pattern NSTypesetterOriginalBehavior
  , pattern NSTypesetterBehavior_10_2_WithCompatibility
  , pattern NSTypesetterBehavior_10_2
  , pattern NSTypesetterBehavior_10_3
  , pattern NSTypesetterBehavior_10_4
  , NSUnderlineStyle(NSUnderlineStyle)
  , pattern NSUnderlineStyleNone
  , pattern NSUnderlineStyleSingle
  , pattern NSUnderlineStyleThick
  , pattern NSUnderlineStyleDouble
  , pattern NSUnderlineStylePatternSolid
  , pattern NSUnderlineStylePatternDot
  , pattern NSUnderlineStylePatternDash
  , pattern NSUnderlineStylePatternDashDot
  , pattern NSUnderlineStylePatternDashDotDot
  , pattern NSUnderlineStyleByWord

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

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- init@
init_ :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSLayoutManager)
init_ nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSLayoutManager nsLayoutManager, IsNSCoder coder) => nsLayoutManager -> coder -> IO (Id NSLayoutManager)
initWithCoder nsLayoutManager  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsLayoutManager (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- replaceTextStorage:@
replaceTextStorage :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage newTextStorage) => nsLayoutManager -> newTextStorage -> IO ()
replaceTextStorage nsLayoutManager  newTextStorage =
withObjCPtr newTextStorage $ \raw_newTextStorage ->
    sendMsg nsLayoutManager (mkSelector "replaceTextStorage:") retVoid [argPtr (castPtr raw_newTextStorage :: Ptr ())]

-- | @- addTextContainer:@
addTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
addTextContainer nsLayoutManager  container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "addTextContainer:") retVoid [argPtr (castPtr raw_container :: Ptr ())]

-- | @- insertTextContainer:atIndex:@
insertTextContainer_atIndex :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> CULong -> IO ()
insertTextContainer_atIndex nsLayoutManager  container index =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "insertTextContainer:atIndex:") retVoid [argPtr (castPtr raw_container :: Ptr ()), argCULong (fromIntegral index)]

-- | @- removeTextContainerAtIndex:@
removeTextContainerAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO ()
removeTextContainerAtIndex nsLayoutManager  index =
  sendMsg nsLayoutManager (mkSelector "removeTextContainerAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | @- textContainerChangedGeometry:@
textContainerChangedGeometry :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
textContainerChangedGeometry nsLayoutManager  container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "textContainerChangedGeometry:") retVoid [argPtr (castPtr raw_container :: Ptr ())]

-- | @- textContainerChangedTextView:@
textContainerChangedTextView :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
textContainerChangedTextView nsLayoutManager  container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "textContainerChangedTextView:") retVoid [argPtr (castPtr raw_container :: Ptr ())]

-- | ************************ Invalidation *************************
--
-- ObjC selector: @- invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:@
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> CLong -> Ptr NSRange -> IO ()
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange nsLayoutManager  charRange delta actualCharRange =
  sendMsg nsLayoutManager (mkSelector "invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:") retVoid [argNSRange charRange, argCLong (fromIntegral delta), argPtr actualCharRange]

-- | @- invalidateLayoutForCharacterRange:actualCharacterRange:@
invalidateLayoutForCharacterRange_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr NSRange -> IO ()
invalidateLayoutForCharacterRange_actualCharacterRange nsLayoutManager  charRange actualCharRange =
  sendMsg nsLayoutManager (mkSelector "invalidateLayoutForCharacterRange:actualCharacterRange:") retVoid [argNSRange charRange, argPtr actualCharRange]

-- | @- invalidateDisplayForCharacterRange:@
invalidateDisplayForCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
invalidateDisplayForCharacterRange nsLayoutManager  charRange =
  sendMsg nsLayoutManager (mkSelector "invalidateDisplayForCharacterRange:") retVoid [argNSRange charRange]

-- | @- invalidateDisplayForGlyphRange:@
invalidateDisplayForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
invalidateDisplayForGlyphRange nsLayoutManager  glyphRange =
  sendMsg nsLayoutManager (mkSelector "invalidateDisplayForGlyphRange:") retVoid [argNSRange glyphRange]

-- | @- processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:@
processEditingForTextStorage_edited_range_changeInLength_invalidatedRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage textStorage) => nsLayoutManager -> textStorage -> NSTextStorageEditActions -> NSRange -> CLong -> NSRange -> IO ()
processEditingForTextStorage_edited_range_changeInLength_invalidatedRange nsLayoutManager  textStorage editMask newCharRange delta invalidatedCharRange =
withObjCPtr textStorage $ \raw_textStorage ->
    sendMsg nsLayoutManager (mkSelector "processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:") retVoid [argPtr (castPtr raw_textStorage :: Ptr ()), argCULong (coerce editMask), argNSRange newCharRange, argCLong (fromIntegral delta), argNSRange invalidatedCharRange]

-- | ********************** Causing glyph generation and layout ***********************
--
-- ObjC selector: @- ensureGlyphsForCharacterRange:@
ensureGlyphsForCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureGlyphsForCharacterRange nsLayoutManager  charRange =
  sendMsg nsLayoutManager (mkSelector "ensureGlyphsForCharacterRange:") retVoid [argNSRange charRange]

-- | @- ensureGlyphsForGlyphRange:@
ensureGlyphsForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureGlyphsForGlyphRange nsLayoutManager  glyphRange =
  sendMsg nsLayoutManager (mkSelector "ensureGlyphsForGlyphRange:") retVoid [argNSRange glyphRange]

-- | @- ensureLayoutForCharacterRange:@
ensureLayoutForCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureLayoutForCharacterRange nsLayoutManager  charRange =
  sendMsg nsLayoutManager (mkSelector "ensureLayoutForCharacterRange:") retVoid [argNSRange charRange]

-- | @- ensureLayoutForGlyphRange:@
ensureLayoutForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureLayoutForGlyphRange nsLayoutManager  glyphRange =
  sendMsg nsLayoutManager (mkSelector "ensureLayoutForGlyphRange:") retVoid [argNSRange glyphRange]

-- | @- ensureLayoutForTextContainer:@
ensureLayoutForTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
ensureLayoutForTextContainer nsLayoutManager  container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "ensureLayoutForTextContainer:") retVoid [argPtr (castPtr raw_container :: Ptr ())]

-- | @- ensureLayoutForBoundingRect:inTextContainer:@
ensureLayoutForBoundingRect_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> container -> IO ()
ensureLayoutForBoundingRect_inTextContainer nsLayoutManager  bounds container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "ensureLayoutForBoundingRect:inTextContainer:") retVoid [argNSRect bounds, argPtr (castPtr raw_container :: Ptr ())]

-- | @- CGGlyphAtIndex:isValidIndex:@
cgGlyphAtIndex_isValidIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr Bool -> IO CUShort
cgGlyphAtIndex_isValidIndex nsLayoutManager  glyphIndex isValidIndex =
  fmap fromIntegral $ sendMsg nsLayoutManager (mkSelector "CGGlyphAtIndex:isValidIndex:") retCUInt [argCULong (fromIntegral glyphIndex), argPtr isValidIndex]

-- | @- CGGlyphAtIndex:@
cgGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CUShort
cgGlyphAtIndex nsLayoutManager  glyphIndex =
  fmap fromIntegral $ sendMsg nsLayoutManager (mkSelector "CGGlyphAtIndex:") retCUInt [argCULong (fromIntegral glyphIndex)]

-- | @- isValidGlyphIndex:@
isValidGlyphIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO Bool
isValidGlyphIndex nsLayoutManager  glyphIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "isValidGlyphIndex:") retCULong [argCULong (fromIntegral glyphIndex)]

-- | @- propertyForGlyphAtIndex:@
propertyForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSGlyphProperty
propertyForGlyphAtIndex nsLayoutManager  glyphIndex =
  fmap (coerce :: CLong -> NSGlyphProperty) $ sendMsg nsLayoutManager (mkSelector "propertyForGlyphAtIndex:") retCLong [argCULong (fromIntegral glyphIndex)]

-- | @- characterIndexForGlyphAtIndex:@
characterIndexForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CULong
characterIndexForGlyphAtIndex nsLayoutManager  glyphIndex =
  sendMsg nsLayoutManager (mkSelector "characterIndexForGlyphAtIndex:") retCULong [argCULong (fromIntegral glyphIndex)]

-- | @- glyphIndexForCharacterAtIndex:@
glyphIndexForCharacterAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CULong
glyphIndexForCharacterAtIndex nsLayoutManager  charIndex =
  sendMsg nsLayoutManager (mkSelector "glyphIndexForCharacterAtIndex:") retCULong [argCULong (fromIntegral charIndex)]

-- | @- setTextContainer:forGlyphRange:@
setTextContainer_forGlyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> NSRange -> IO ()
setTextContainer_forGlyphRange nsLayoutManager  container glyphRange =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "setTextContainer:forGlyphRange:") retVoid [argPtr (castPtr raw_container :: Ptr ()), argNSRange glyphRange]

-- | @- setLineFragmentRect:forGlyphRange:usedRect:@
setLineFragmentRect_forGlyphRange_usedRect :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRect -> NSRange -> NSRect -> IO ()
setLineFragmentRect_forGlyphRange_usedRect nsLayoutManager  fragmentRect glyphRange usedRect =
  sendMsg nsLayoutManager (mkSelector "setLineFragmentRect:forGlyphRange:usedRect:") retVoid [argNSRect fragmentRect, argNSRange glyphRange, argNSRect usedRect]

-- | @- setExtraLineFragmentRect:usedRect:textContainer:@
setExtraLineFragmentRect_usedRect_textContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> NSRect -> container -> IO ()
setExtraLineFragmentRect_usedRect_textContainer nsLayoutManager  fragmentRect usedRect container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "setExtraLineFragmentRect:usedRect:textContainer:") retVoid [argNSRect fragmentRect, argNSRect usedRect, argPtr (castPtr raw_container :: Ptr ())]

-- | @- setLocation:forStartOfGlyphRange:@
setLocation_forStartOfGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSPoint -> NSRange -> IO ()
setLocation_forStartOfGlyphRange nsLayoutManager  location glyphRange =
  sendMsg nsLayoutManager (mkSelector "setLocation:forStartOfGlyphRange:") retVoid [argNSPoint location, argNSRange glyphRange]

-- | @- setNotShownAttribute:forGlyphAtIndex:@
setNotShownAttribute_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> CULong -> IO ()
setNotShownAttribute_forGlyphAtIndex nsLayoutManager  flag glyphIndex =
  sendMsg nsLayoutManager (mkSelector "setNotShownAttribute:forGlyphAtIndex:") retVoid [argCULong (if flag then 1 else 0), argCULong (fromIntegral glyphIndex)]

-- | @- setDrawsOutsideLineFragment:forGlyphAtIndex:@
setDrawsOutsideLineFragment_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> CULong -> IO ()
setDrawsOutsideLineFragment_forGlyphAtIndex nsLayoutManager  flag glyphIndex =
  sendMsg nsLayoutManager (mkSelector "setDrawsOutsideLineFragment:forGlyphAtIndex:") retVoid [argCULong (if flag then 1 else 0), argCULong (fromIntegral glyphIndex)]

-- | @- setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSSize -> NSRange -> IO ()
setAttachmentSize_forGlyphRange nsLayoutManager  attachmentSize glyphRange =
  sendMsg nsLayoutManager (mkSelector "setAttachmentSize:forGlyphRange:") retVoid [argNSSize attachmentSize, argNSRange glyphRange]

-- | ********************** Get layout information ***********************
--
-- ObjC selector: @- getFirstUnlaidCharacterIndex:glyphIndex:@
getFirstUnlaidCharacterIndex_glyphIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Ptr CULong -> Ptr CULong -> IO ()
getFirstUnlaidCharacterIndex_glyphIndex nsLayoutManager  charIndex glyphIndex =
  sendMsg nsLayoutManager (mkSelector "getFirstUnlaidCharacterIndex:glyphIndex:") retVoid [argPtr charIndex, argPtr glyphIndex]

-- | @- firstUnlaidCharacterIndex@
firstUnlaidCharacterIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CULong
firstUnlaidCharacterIndex nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "firstUnlaidCharacterIndex") retCULong []

-- | @- firstUnlaidGlyphIndex@
firstUnlaidGlyphIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CULong
firstUnlaidGlyphIndex nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "firstUnlaidGlyphIndex") retCULong []

-- | @- textContainerForGlyphAtIndex:effectiveRange:@
textContainerForGlyphAtIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO (Id NSTextContainer)
textContainerForGlyphAtIndex_effectiveRange nsLayoutManager  glyphIndex effectiveGlyphRange =
  sendMsg nsLayoutManager (mkSelector "textContainerForGlyphAtIndex:effectiveRange:") (retPtr retVoid) [argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange] >>= retainedObject . castPtr

-- | @- textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> Bool -> IO (Id NSTextContainer)
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout nsLayoutManager  glyphIndex effectiveGlyphRange flag =
  sendMsg nsLayoutManager (mkSelector "textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:") (retPtr retVoid) [argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange, argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @- usedRectForTextContainer:@
usedRectForTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO NSRect
usedRectForTextContainer nsLayoutManager  container =
withObjCPtr container $ \raw_container ->
    sendMsgStret nsLayoutManager (mkSelector "usedRectForTextContainer:") retNSRect [argPtr (castPtr raw_container :: Ptr ())]

-- | @- lineFragmentRectForGlyphAtIndex:effectiveRange:@
lineFragmentRectForGlyphAtIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO NSRect
lineFragmentRectForGlyphAtIndex_effectiveRange nsLayoutManager  glyphIndex effectiveGlyphRange =
  sendMsgStret nsLayoutManager (mkSelector "lineFragmentRectForGlyphAtIndex:effectiveRange:") retNSRect [argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange]

-- | @- lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> Bool -> IO NSRect
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout nsLayoutManager  glyphIndex effectiveGlyphRange flag =
  sendMsgStret nsLayoutManager (mkSelector "lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:") retNSRect [argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange, argCULong (if flag then 1 else 0)]

-- | @- lineFragmentUsedRectForGlyphAtIndex:effectiveRange:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO NSRect
lineFragmentUsedRectForGlyphAtIndex_effectiveRange nsLayoutManager  glyphIndex effectiveGlyphRange =
  sendMsgStret nsLayoutManager (mkSelector "lineFragmentUsedRectForGlyphAtIndex:effectiveRange:") retNSRect [argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange]

-- | @- lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> Bool -> IO NSRect
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout nsLayoutManager  glyphIndex effectiveGlyphRange flag =
  sendMsgStret nsLayoutManager (mkSelector "lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:") retNSRect [argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange, argCULong (if flag then 1 else 0)]

-- | @- locationForGlyphAtIndex:@
locationForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSPoint
locationForGlyphAtIndex nsLayoutManager  glyphIndex =
  sendMsgStret nsLayoutManager (mkSelector "locationForGlyphAtIndex:") retNSPoint [argCULong (fromIntegral glyphIndex)]

-- | @- notShownAttributeForGlyphAtIndex:@
notShownAttributeForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO Bool
notShownAttributeForGlyphAtIndex nsLayoutManager  glyphIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "notShownAttributeForGlyphAtIndex:") retCULong [argCULong (fromIntegral glyphIndex)]

-- | @- drawsOutsideLineFragmentForGlyphAtIndex:@
drawsOutsideLineFragmentForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO Bool
drawsOutsideLineFragmentForGlyphAtIndex nsLayoutManager  glyphIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "drawsOutsideLineFragmentForGlyphAtIndex:") retCULong [argCULong (fromIntegral glyphIndex)]

-- | @- attachmentSizeForGlyphAtIndex:@
attachmentSizeForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSSize
attachmentSizeForGlyphAtIndex nsLayoutManager  glyphIndex =
  sendMsgStret nsLayoutManager (mkSelector "attachmentSizeForGlyphAtIndex:") retNSSize [argCULong (fromIntegral glyphIndex)]

-- | @- truncatedGlyphRangeInLineFragmentForGlyphAtIndex:@
truncatedGlyphRangeInLineFragmentForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSRange
truncatedGlyphRangeInLineFragmentForGlyphAtIndex nsLayoutManager  glyphIndex =
  sendMsgStret nsLayoutManager (mkSelector "truncatedGlyphRangeInLineFragmentForGlyphAtIndex:") retNSRange [argCULong (fromIntegral glyphIndex)]

-- | ********************** More sophisticated queries ***********************
--
-- ObjC selector: @- glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr NSRange -> IO NSRange
glyphRangeForCharacterRange_actualCharacterRange nsLayoutManager  charRange actualCharRange =
  sendMsgStret nsLayoutManager (mkSelector "glyphRangeForCharacterRange:actualCharacterRange:") retNSRange [argNSRange charRange, argPtr actualCharRange]

-- | @- characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr NSRange -> IO NSRange
characterRangeForGlyphRange_actualGlyphRange nsLayoutManager  glyphRange actualGlyphRange =
  sendMsgStret nsLayoutManager (mkSelector "characterRangeForGlyphRange:actualGlyphRange:") retNSRange [argNSRange glyphRange, argPtr actualGlyphRange]

-- | @- glyphRangeForTextContainer:@
glyphRangeForTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO NSRange
glyphRangeForTextContainer nsLayoutManager  container =
withObjCPtr container $ \raw_container ->
    sendMsgStret nsLayoutManager (mkSelector "glyphRangeForTextContainer:") retNSRange [argPtr (castPtr raw_container :: Ptr ())]

-- | @- rangeOfNominallySpacedGlyphsContainingIndex:@
rangeOfNominallySpacedGlyphsContainingIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSRange
rangeOfNominallySpacedGlyphsContainingIndex nsLayoutManager  glyphIndex =
  sendMsgStret nsLayoutManager (mkSelector "rangeOfNominallySpacedGlyphsContainingIndex:") retNSRange [argCULong (fromIntegral glyphIndex)]

-- | @- boundingRectForGlyphRange:inTextContainer:@
boundingRectForGlyphRange_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRange -> container -> IO NSRect
boundingRectForGlyphRange_inTextContainer nsLayoutManager  glyphRange container =
withObjCPtr container $ \raw_container ->
    sendMsgStret nsLayoutManager (mkSelector "boundingRectForGlyphRange:inTextContainer:") retNSRect [argNSRange glyphRange, argPtr (castPtr raw_container :: Ptr ())]

-- | @- glyphRangeForBoundingRect:inTextContainer:@
glyphRangeForBoundingRect_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> container -> IO NSRange
glyphRangeForBoundingRect_inTextContainer nsLayoutManager  bounds container =
withObjCPtr container $ \raw_container ->
    sendMsgStret nsLayoutManager (mkSelector "glyphRangeForBoundingRect:inTextContainer:") retNSRange [argNSRect bounds, argPtr (castPtr raw_container :: Ptr ())]

-- | @- glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:@
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> container -> IO NSRange
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer nsLayoutManager  bounds container =
withObjCPtr container $ \raw_container ->
    sendMsgStret nsLayoutManager (mkSelector "glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:") retNSRange [argNSRect bounds, argPtr (castPtr raw_container :: Ptr ())]

-- | @- glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:@
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> Ptr CDouble -> IO CULong
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph nsLayoutManager  point container partialFraction =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:") retCULong [argNSPoint point, argPtr (castPtr raw_container :: Ptr ()), argPtr partialFraction]

-- | @- glyphIndexForPoint:inTextContainer:@
glyphIndexForPoint_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> IO CULong
glyphIndexForPoint_inTextContainer nsLayoutManager  point container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "glyphIndexForPoint:inTextContainer:") retCULong [argNSPoint point, argPtr (castPtr raw_container :: Ptr ())]

-- | @- fractionOfDistanceThroughGlyphForPoint:inTextContainer:@
fractionOfDistanceThroughGlyphForPoint_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> IO CDouble
fractionOfDistanceThroughGlyphForPoint_inTextContainer nsLayoutManager  point container =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "fractionOfDistanceThroughGlyphForPoint:inTextContainer:") retCDouble [argNSPoint point, argPtr (castPtr raw_container :: Ptr ())]

-- | @- characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:@
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPoints :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> Ptr CDouble -> IO CULong
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPoints nsLayoutManager  point container partialFraction =
withObjCPtr container $ \raw_container ->
    sendMsg nsLayoutManager (mkSelector "characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:") retCULong [argNSPoint point, argPtr (castPtr raw_container :: Ptr ()), argPtr partialFraction]

-- | @- getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:@
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Bool -> Bool -> Ptr CDouble -> Ptr CULong -> IO CULong
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes nsLayoutManager  charIndex aFlag dFlag positions charIndexes =
  sendMsg nsLayoutManager (mkSelector "getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:") retCULong [argCULong (fromIntegral charIndex), argCULong (if aFlag then 1 else 0), argCULong (if dFlag then 1 else 0), argPtr positions, argPtr charIndexes]

-- | @- enumerateLineFragmentsForGlyphRange:usingBlock:@
enumerateLineFragmentsForGlyphRange_usingBlock :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr () -> IO ()
enumerateLineFragmentsForGlyphRange_usingBlock nsLayoutManager  glyphRange block =
  sendMsg nsLayoutManager (mkSelector "enumerateLineFragmentsForGlyphRange:usingBlock:") retVoid [argNSRange glyphRange, argPtr (castPtr block :: Ptr ())]

-- | @- enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:@
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlock :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer textContainer) => nsLayoutManager -> NSRange -> NSRange -> textContainer -> Ptr () -> IO ()
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlock nsLayoutManager  glyphRange selectedRange textContainer block =
withObjCPtr textContainer $ \raw_textContainer ->
    sendMsg nsLayoutManager (mkSelector "enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:") retVoid [argNSRange glyphRange, argNSRange selectedRange, argPtr (castPtr raw_textContainer :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | ********************** Drawing support ***********************
--
-- ObjC selector: @- drawBackgroundForGlyphRange:atPoint:@
drawBackgroundForGlyphRange_atPoint :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSPoint -> IO ()
drawBackgroundForGlyphRange_atPoint nsLayoutManager  glyphsToShow origin =
  sendMsg nsLayoutManager (mkSelector "drawBackgroundForGlyphRange:atPoint:") retVoid [argNSRange glyphsToShow, argNSPoint origin]

-- | @- drawGlyphsForGlyphRange:atPoint:@
drawGlyphsForGlyphRange_atPoint :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSPoint -> IO ()
drawGlyphsForGlyphRange_atPoint nsLayoutManager  glyphsToShow origin =
  sendMsg nsLayoutManager (mkSelector "drawGlyphsForGlyphRange:atPoint:") retVoid [argNSRange glyphsToShow, argNSPoint origin]

-- | @- fillBackgroundRectArray:count:forCharacterRange:color:@
fillBackgroundRectArray_count_forCharacterRange_color :: (IsNSLayoutManager nsLayoutManager, IsNSColor color) => nsLayoutManager -> Const (Ptr NSRect) -> CULong -> NSRange -> color -> IO ()
fillBackgroundRectArray_count_forCharacterRange_color nsLayoutManager  rectArray rectCount charRange color =
withObjCPtr color $ \raw_color ->
    sendMsg nsLayoutManager (mkSelector "fillBackgroundRectArray:count:forCharacterRange:color:") retVoid [argPtr (unConst rectArray), argCULong (fromIntegral rectCount), argNSRange charRange, argPtr (castPtr raw_color :: Ptr ())]

-- | @- drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> CDouble -> NSRect -> NSRange -> NSPoint -> IO ()
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager  glyphRange underlineVal baselineOffset lineRect lineGlyphRange containerOrigin =
  sendMsg nsLayoutManager (mkSelector "drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:") retVoid [argNSRange glyphRange, argCLong (coerce underlineVal), argCDouble (fromIntegral baselineOffset), argNSRect lineRect, argNSRange lineGlyphRange, argNSPoint containerOrigin]

-- | @- underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> NSRect -> NSRange -> NSPoint -> IO ()
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager  glyphRange underlineVal lineRect lineGlyphRange containerOrigin =
  sendMsg nsLayoutManager (mkSelector "underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:") retVoid [argNSRange glyphRange, argCLong (coerce underlineVal), argNSRect lineRect, argNSRange lineGlyphRange, argNSPoint containerOrigin]

-- | @- drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> CDouble -> NSRect -> NSRange -> NSPoint -> IO ()
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager  glyphRange strikethroughVal baselineOffset lineRect lineGlyphRange containerOrigin =
  sendMsg nsLayoutManager (mkSelector "drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:") retVoid [argNSRange glyphRange, argCLong (coerce strikethroughVal), argCDouble (fromIntegral baselineOffset), argNSRect lineRect, argNSRange lineGlyphRange, argNSPoint containerOrigin]

-- | @- strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> NSRect -> NSRange -> NSPoint -> IO ()
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager  glyphRange strikethroughVal lineRect lineGlyphRange containerOrigin =
  sendMsg nsLayoutManager (mkSelector "strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:") retVoid [argNSRange glyphRange, argCLong (coerce strikethroughVal), argNSRect lineRect, argNSRange lineGlyphRange, argNSPoint containerOrigin]

-- | @- showAttachmentCell:inRect:characterIndex:@
showAttachmentCell_inRect_characterIndex :: (IsNSLayoutManager nsLayoutManager, IsNSCell cell) => nsLayoutManager -> cell -> NSRect -> CULong -> IO ()
showAttachmentCell_inRect_characterIndex nsLayoutManager  cell rect attachmentIndex =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsLayoutManager (mkSelector "showAttachmentCell:inRect:characterIndex:") retVoid [argPtr (castPtr raw_cell :: Ptr ()), argNSRect rect, argCULong (fromIntegral attachmentIndex)]

-- | ************************ Block information *************************
--
-- ObjC selector: @- setLayoutRect:forTextBlock:glyphRange:@
setLayoutRect_forTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> NSRect -> block -> NSRange -> IO ()
setLayoutRect_forTextBlock_glyphRange nsLayoutManager  rect block glyphRange =
withObjCPtr block $ \raw_block ->
    sendMsg nsLayoutManager (mkSelector "setLayoutRect:forTextBlock:glyphRange:") retVoid [argNSRect rect, argPtr (castPtr raw_block :: Ptr ()), argNSRange glyphRange]

-- | @- setBoundsRect:forTextBlock:glyphRange:@
setBoundsRect_forTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> NSRect -> block -> NSRange -> IO ()
setBoundsRect_forTextBlock_glyphRange nsLayoutManager  rect block glyphRange =
withObjCPtr block $ \raw_block ->
    sendMsg nsLayoutManager (mkSelector "setBoundsRect:forTextBlock:glyphRange:") retVoid [argNSRect rect, argPtr (castPtr raw_block :: Ptr ()), argNSRange glyphRange]

-- | @- layoutRectForTextBlock:glyphRange:@
layoutRectForTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> NSRange -> IO NSRect
layoutRectForTextBlock_glyphRange nsLayoutManager  block glyphRange =
withObjCPtr block $ \raw_block ->
    sendMsgStret nsLayoutManager (mkSelector "layoutRectForTextBlock:glyphRange:") retNSRect [argPtr (castPtr raw_block :: Ptr ()), argNSRange glyphRange]

-- | @- boundsRectForTextBlock:glyphRange:@
boundsRectForTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> NSRange -> IO NSRect
boundsRectForTextBlock_glyphRange nsLayoutManager  block glyphRange =
withObjCPtr block $ \raw_block ->
    sendMsgStret nsLayoutManager (mkSelector "boundsRectForTextBlock:glyphRange:") retNSRect [argPtr (castPtr raw_block :: Ptr ()), argNSRange glyphRange]

-- | @- layoutRectForTextBlock:atIndex:effectiveRange:@
layoutRectForTextBlock_atIndex_effectiveRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> CULong -> Ptr NSRange -> IO NSRect
layoutRectForTextBlock_atIndex_effectiveRange nsLayoutManager  block glyphIndex effectiveGlyphRange =
withObjCPtr block $ \raw_block ->
    sendMsgStret nsLayoutManager (mkSelector "layoutRectForTextBlock:atIndex:effectiveRange:") retNSRect [argPtr (castPtr raw_block :: Ptr ()), argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange]

-- | @- boundsRectForTextBlock:atIndex:effectiveRange:@
boundsRectForTextBlock_atIndex_effectiveRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> CULong -> Ptr NSRange -> IO NSRect
boundsRectForTextBlock_atIndex_effectiveRange nsLayoutManager  block glyphIndex effectiveGlyphRange =
withObjCPtr block $ \raw_block ->
    sendMsgStret nsLayoutManager (mkSelector "boundsRectForTextBlock:atIndex:effectiveRange:") retNSRect [argPtr (castPtr raw_block :: Ptr ()), argCULong (fromIntegral glyphIndex), argPtr effectiveGlyphRange]

-- | ********************** Temporary attribute support ***********************
--
-- ObjC selector: @- temporaryAttributesAtCharacterIndex:effectiveRange:@
temporaryAttributesAtCharacterIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO (Id NSDictionary)
temporaryAttributesAtCharacterIndex_effectiveRange nsLayoutManager  charIndex effectiveCharRange =
  sendMsg nsLayoutManager (mkSelector "temporaryAttributesAtCharacterIndex:effectiveRange:") (retPtr retVoid) [argCULong (fromIntegral charIndex), argPtr effectiveCharRange] >>= retainedObject . castPtr

-- | @- setTemporaryAttributes:forCharacterRange:@
setTemporaryAttributes_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSDictionary attrs) => nsLayoutManager -> attrs -> NSRange -> IO ()
setTemporaryAttributes_forCharacterRange nsLayoutManager  attrs charRange =
withObjCPtr attrs $ \raw_attrs ->
    sendMsg nsLayoutManager (mkSelector "setTemporaryAttributes:forCharacterRange:") retVoid [argPtr (castPtr raw_attrs :: Ptr ()), argNSRange charRange]

-- | @- addTemporaryAttributes:forCharacterRange:@
addTemporaryAttributes_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSDictionary attrs) => nsLayoutManager -> attrs -> NSRange -> IO ()
addTemporaryAttributes_forCharacterRange nsLayoutManager  attrs charRange =
withObjCPtr attrs $ \raw_attrs ->
    sendMsg nsLayoutManager (mkSelector "addTemporaryAttributes:forCharacterRange:") retVoid [argPtr (castPtr raw_attrs :: Ptr ()), argNSRange charRange]

-- | @- removeTemporaryAttribute:forCharacterRange:@
removeTemporaryAttribute_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> NSRange -> IO ()
removeTemporaryAttribute_forCharacterRange nsLayoutManager  attrName charRange =
withObjCPtr attrName $ \raw_attrName ->
    sendMsg nsLayoutManager (mkSelector "removeTemporaryAttribute:forCharacterRange:") retVoid [argPtr (castPtr raw_attrName :: Ptr ()), argNSRange charRange]

-- | @- temporaryAttribute:atCharacterIndex:effectiveRange:@
temporaryAttribute_atCharacterIndex_effectiveRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> CULong -> Ptr NSRange -> IO RawId
temporaryAttribute_atCharacterIndex_effectiveRange nsLayoutManager  attrName location range =
withObjCPtr attrName $ \raw_attrName ->
    fmap (RawId . castPtr) $ sendMsg nsLayoutManager (mkSelector "temporaryAttribute:atCharacterIndex:effectiveRange:") (retPtr retVoid) [argPtr (castPtr raw_attrName :: Ptr ()), argCULong (fromIntegral location), argPtr range]

-- | @- temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> CULong -> Ptr NSRange -> NSRange -> IO RawId
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange nsLayoutManager  attrName location range rangeLimit =
withObjCPtr attrName $ \raw_attrName ->
    fmap (RawId . castPtr) $ sendMsg nsLayoutManager (mkSelector "temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:") (retPtr retVoid) [argPtr (castPtr raw_attrName :: Ptr ()), argCULong (fromIntegral location), argPtr range, argNSRange rangeLimit]

-- | @- temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> NSRange -> IO (Id NSDictionary)
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange nsLayoutManager  location range rangeLimit =
  sendMsg nsLayoutManager (mkSelector "temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:") (retPtr retVoid) [argCULong (fromIntegral location), argPtr range, argNSRange rangeLimit] >>= retainedObject . castPtr

-- | @- addTemporaryAttribute:value:forCharacterRange:@
addTemporaryAttribute_value_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> RawId -> NSRange -> IO ()
addTemporaryAttribute_value_forCharacterRange nsLayoutManager  attrName value charRange =
withObjCPtr attrName $ \raw_attrName ->
    sendMsg nsLayoutManager (mkSelector "addTemporaryAttribute:value:forCharacterRange:") retVoid [argPtr (castPtr raw_attrName :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argNSRange charRange]

-- | ***************************** Font metrics *****************************
--
-- ObjC selector: @- defaultLineHeightForFont:@
defaultLineHeightForFont :: (IsNSLayoutManager nsLayoutManager, IsNSFont theFont) => nsLayoutManager -> theFont -> IO CDouble
defaultLineHeightForFont nsLayoutManager  theFont =
withObjCPtr theFont $ \raw_theFont ->
    sendMsg nsLayoutManager (mkSelector "defaultLineHeightForFont:") retCDouble [argPtr (castPtr raw_theFont :: Ptr ())]

-- | @- defaultBaselineOffsetForFont:@
defaultBaselineOffsetForFont :: (IsNSLayoutManager nsLayoutManager, IsNSFont theFont) => nsLayoutManager -> theFont -> IO CDouble
defaultBaselineOffsetForFont nsLayoutManager  theFont =
withObjCPtr theFont $ \raw_theFont ->
    sendMsg nsLayoutManager (mkSelector "defaultBaselineOffsetForFont:") retCDouble [argPtr (castPtr raw_theFont :: Ptr ())]

-- | @- glyphAtIndex:isValidIndex:@
glyphAtIndex_isValidIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr Bool -> IO CUInt
glyphAtIndex_isValidIndex nsLayoutManager  glyphIndex isValidIndex =
  sendMsg nsLayoutManager (mkSelector "glyphAtIndex:isValidIndex:") retCUInt [argCULong (fromIntegral glyphIndex), argPtr isValidIndex]

-- | @- glyphAtIndex:@
glyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CUInt
glyphAtIndex nsLayoutManager  glyphIndex =
  sendMsg nsLayoutManager (mkSelector "glyphAtIndex:") retCUInt [argCULong (fromIntegral glyphIndex)]

-- | @- rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:@
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRange -> NSRange -> container -> Ptr CULong -> IO (Ptr NSRect)
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount nsLayoutManager  charRange selCharRange container rectCount =
withObjCPtr container $ \raw_container ->
    fmap castPtr $ sendMsg nsLayoutManager (mkSelector "rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:") (retPtr retVoid) [argNSRange charRange, argNSRange selCharRange, argPtr (castPtr raw_container :: Ptr ()), argPtr rectCount]

-- | @- rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:@
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRange -> NSRange -> container -> Ptr CULong -> IO (Ptr NSRect)
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount nsLayoutManager  glyphRange selGlyphRange container rectCount =
withObjCPtr container $ \raw_container ->
    fmap castPtr $ sendMsg nsLayoutManager (mkSelector "rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:") (retPtr retVoid) [argNSRange glyphRange, argNSRange selGlyphRange, argPtr (castPtr raw_container :: Ptr ()), argPtr rectCount]

-- | @- substituteFontForFont:@
substituteFontForFont :: (IsNSLayoutManager nsLayoutManager, IsNSFont originalFont) => nsLayoutManager -> originalFont -> IO (Id NSFont)
substituteFontForFont nsLayoutManager  originalFont =
withObjCPtr originalFont $ \raw_originalFont ->
    sendMsg nsLayoutManager (mkSelector "substituteFontForFont:") (retPtr retVoid) [argPtr (castPtr raw_originalFont :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CUInt -> CULong -> CULong -> IO ()
insertGlyph_atGlyphIndex_characterIndex nsLayoutManager  glyph glyphIndex charIndex =
  sendMsg nsLayoutManager (mkSelector "insertGlyph:atGlyphIndex:characterIndex:") retVoid [argCUInt (fromIntegral glyph), argCULong (fromIntegral glyphIndex), argCULong (fromIntegral charIndex)]

-- | @- replaceGlyphAtIndex:withGlyph:@
replaceGlyphAtIndex_withGlyph :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> CUInt -> IO ()
replaceGlyphAtIndex_withGlyph nsLayoutManager  glyphIndex newGlyph =
  sendMsg nsLayoutManager (mkSelector "replaceGlyphAtIndex:withGlyph:") retVoid [argCULong (fromIntegral glyphIndex), argCUInt (fromIntegral newGlyph)]

-- | @- deleteGlyphsInRange:@
deleteGlyphsInRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
deleteGlyphsInRange nsLayoutManager  glyphRange =
  sendMsg nsLayoutManager (mkSelector "deleteGlyphsInRange:") retVoid [argNSRange glyphRange]

-- | @- setCharacterIndex:forGlyphAtIndex:@
setCharacterIndex_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> CULong -> IO ()
setCharacterIndex_forGlyphAtIndex nsLayoutManager  charIndex glyphIndex =
  sendMsg nsLayoutManager (mkSelector "setCharacterIndex:forGlyphAtIndex:") retVoid [argCULong (fromIntegral charIndex), argCULong (fromIntegral glyphIndex)]

-- | @- setIntAttribute:value:forGlyphAtIndex:@
setIntAttribute_value_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CLong -> CLong -> CULong -> IO ()
setIntAttribute_value_forGlyphAtIndex nsLayoutManager  attributeTag val glyphIndex =
  sendMsg nsLayoutManager (mkSelector "setIntAttribute:value:forGlyphAtIndex:") retVoid [argCLong (fromIntegral attributeTag), argCLong (fromIntegral val), argCULong (fromIntegral glyphIndex)]

-- | @- invalidateGlyphsOnLayoutInvalidationForGlyphRange:@
invalidateGlyphsOnLayoutInvalidationForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
invalidateGlyphsOnLayoutInvalidationForGlyphRange nsLayoutManager  glyphRange =
  sendMsg nsLayoutManager (mkSelector "invalidateGlyphsOnLayoutInvalidationForGlyphRange:") retVoid [argNSRange glyphRange]

-- | @- intAttribute:forGlyphAtIndex:@
intAttribute_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CLong -> CULong -> IO CLong
intAttribute_forGlyphAtIndex nsLayoutManager  attributeTag glyphIndex =
  sendMsg nsLayoutManager (mkSelector "intAttribute:forGlyphAtIndex:") retCLong [argCLong (fromIntegral attributeTag), argCULong (fromIntegral glyphIndex)]

-- | @- invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:@
invalidateLayoutForCharacterRange_isSoft_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Bool -> Ptr NSRange -> IO ()
invalidateLayoutForCharacterRange_isSoft_actualCharacterRange nsLayoutManager  charRange flag actualCharRange =
  sendMsg nsLayoutManager (mkSelector "invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:") retVoid [argNSRange charRange, argCULong (if flag then 1 else 0), argPtr actualCharRange]

-- | @- textStorage:edited:range:changeInLength:invalidatedRange:@
textStorage_edited_range_changeInLength_invalidatedRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage str) => nsLayoutManager -> str -> CULong -> NSRange -> CLong -> NSRange -> IO ()
textStorage_edited_range_changeInLength_invalidatedRange nsLayoutManager  str editedMask newCharRange delta invalidatedCharRange =
withObjCPtr str $ \raw_str ->
    sendMsg nsLayoutManager (mkSelector "textStorage:edited:range:changeInLength:invalidatedRange:") retVoid [argPtr (castPtr raw_str :: Ptr ()), argCULong (fromIntegral editedMask), argNSRange newCharRange, argCLong (fromIntegral delta), argNSRange invalidatedCharRange]

-- | @- setLocations:startingGlyphIndexes:count:forGlyphRange:@
setLocations_startingGlyphIndexes_count_forGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Ptr NSPoint -> Ptr CULong -> CULong -> NSRange -> IO ()
setLocations_startingGlyphIndexes_count_forGlyphRange nsLayoutManager  locations glyphIndexes count glyphRange =
  sendMsg nsLayoutManager (mkSelector "setLocations:startingGlyphIndexes:count:forGlyphRange:") retVoid [argPtr locations, argPtr glyphIndexes, argCULong (fromIntegral count), argNSRange glyphRange]

-- | @- showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:@
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment :: (IsNSLayoutManager nsLayoutManager, IsNSFont font, IsNSColor color) => nsLayoutManager -> Ptr CChar -> CULong -> NSRange -> NSPoint -> font -> color -> NSSize -> IO ()
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment nsLayoutManager  glyphs glyphLen glyphRange point font color printingAdjustment =
withObjCPtr font $ \raw_font ->
  withObjCPtr color $ \raw_color ->
      sendMsg nsLayoutManager (mkSelector "showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:") retVoid [argPtr glyphs, argCULong (fromIntegral glyphLen), argNSRange glyphRange, argNSPoint point, argPtr (castPtr raw_font :: Ptr ()), argPtr (castPtr raw_color :: Ptr ()), argNSSize printingAdjustment]

-- | *************************** Ruler support ****************************
--
-- ObjC selector: @- rulerMarkersForTextView:paragraphStyle:ruler:@
rulerMarkersForTextView_paragraphStyle_ruler :: (IsNSLayoutManager nsLayoutManager, IsNSTextView view, IsNSParagraphStyle style, IsNSRulerView ruler) => nsLayoutManager -> view -> style -> ruler -> IO (Id NSArray)
rulerMarkersForTextView_paragraphStyle_ruler nsLayoutManager  view style ruler =
withObjCPtr view $ \raw_view ->
  withObjCPtr style $ \raw_style ->
    withObjCPtr ruler $ \raw_ruler ->
        sendMsg nsLayoutManager (mkSelector "rulerMarkersForTextView:paragraphStyle:ruler:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_style :: Ptr ()), argPtr (castPtr raw_ruler :: Ptr ())] >>= retainedObject . castPtr

-- | @- rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:@
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled :: (IsNSLayoutManager nsLayoutManager, IsNSTextView view, IsNSParagraphStyle style, IsNSRulerView ruler) => nsLayoutManager -> view -> style -> ruler -> Bool -> IO (Id NSView)
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled nsLayoutManager  view style ruler isEnabled =
withObjCPtr view $ \raw_view ->
  withObjCPtr style $ \raw_style ->
    withObjCPtr ruler $ \raw_ruler ->
        sendMsg nsLayoutManager (mkSelector "rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_style :: Ptr ()), argPtr (castPtr raw_ruler :: Ptr ()), argCULong (if isEnabled then 1 else 0)] >>= retainedObject . castPtr

-- | ********************** First responder support ***********************
--
-- ObjC selector: @- layoutManagerOwnsFirstResponderInWindow:@
layoutManagerOwnsFirstResponderInWindow :: (IsNSLayoutManager nsLayoutManager, IsNSWindow window) => nsLayoutManager -> window -> IO Bool
layoutManagerOwnsFirstResponderInWindow nsLayoutManager  window =
withObjCPtr window $ \raw_window ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "layoutManagerOwnsFirstResponderInWindow:") retCULong [argPtr (castPtr raw_window :: Ptr ())]

-- | ************************* Text storage **************************
--
-- ObjC selector: @- textStorage@
textStorage :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextStorage)
textStorage nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "textStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************* Text storage **************************
--
-- ObjC selector: @- setTextStorage:@
setTextStorage :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage value) => nsLayoutManager -> value -> IO ()
setTextStorage nsLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLayoutManager (mkSelector "setTextStorage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | ************************** Text containers ***************************
--
-- ObjC selector: @- textContainers@
textContainers :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSArray)
textContainers nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "textContainers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ********************* Global layout manager options **********************
--
-- ObjC selector: @- showsInvisibleCharacters@
showsInvisibleCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
showsInvisibleCharacters nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "showsInvisibleCharacters") retCULong []

-- | ********************* Global layout manager options **********************
--
-- ObjC selector: @- setShowsInvisibleCharacters:@
setShowsInvisibleCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setShowsInvisibleCharacters nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setShowsInvisibleCharacters:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsControlCharacters@
showsControlCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
showsControlCharacters nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "showsControlCharacters") retCULong []

-- | @- setShowsControlCharacters:@
setShowsControlCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setShowsControlCharacters nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setShowsControlCharacters:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesDefaultHyphenation@
usesDefaultHyphenation :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
usesDefaultHyphenation nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "usesDefaultHyphenation") retCULong []

-- | @- setUsesDefaultHyphenation:@
setUsesDefaultHyphenation :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setUsesDefaultHyphenation nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setUsesDefaultHyphenation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesFontLeading@
usesFontLeading :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
usesFontLeading nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "usesFontLeading") retCULong []

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setUsesFontLeading nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setUsesFontLeading:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsNonContiguousLayout@
allowsNonContiguousLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
allowsNonContiguousLayout nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "allowsNonContiguousLayout") retCULong []

-- | @- setAllowsNonContiguousLayout:@
setAllowsNonContiguousLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setAllowsNonContiguousLayout nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setAllowsNonContiguousLayout:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasNonContiguousLayout@
hasNonContiguousLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
hasNonContiguousLayout nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "hasNonContiguousLayout") retCULong []

-- | @- limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContents :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
limitsLayoutForSuspiciousContents nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "limitsLayoutForSuspiciousContents") retCULong []

-- | @- setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContents :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setLimitsLayoutForSuspiciousContents nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setLimitsLayoutForSuspiciousContents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundLayoutEnabled@
backgroundLayoutEnabled :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
backgroundLayoutEnabled nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "backgroundLayoutEnabled") retCULong []

-- | @- setBackgroundLayoutEnabled:@
setBackgroundLayoutEnabled :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setBackgroundLayoutEnabled nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setBackgroundLayoutEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- defaultAttachmentScaling@
defaultAttachmentScaling :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSImageScaling
defaultAttachmentScaling nsLayoutManager  =
  fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsLayoutManager (mkSelector "defaultAttachmentScaling") retCULong []

-- | @- setDefaultAttachmentScaling:@
setDefaultAttachmentScaling :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSImageScaling -> IO ()
setDefaultAttachmentScaling nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setDefaultAttachmentScaling:") retVoid [argCULong (coerce value)]

-- | ********************* Typesetter **********************
--
-- ObjC selector: @- typesetter@
typesetter :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTypesetter)
typesetter nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "typesetter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ********************* Typesetter **********************
--
-- ObjC selector: @- setTypesetter:@
setTypesetter :: (IsNSLayoutManager nsLayoutManager, IsNSTypesetter value) => nsLayoutManager -> value -> IO ()
setTypesetter nsLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLayoutManager (mkSelector "setTypesetter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- typesetterBehavior@
typesetterBehavior :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSTypesetterBehavior
typesetterBehavior nsLayoutManager  =
  fmap (coerce :: CLong -> NSTypesetterBehavior) $ sendMsg nsLayoutManager (mkSelector "typesetterBehavior") retCLong []

-- | @- setTypesetterBehavior:@
setTypesetterBehavior :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSTypesetterBehavior -> IO ()
setTypesetterBehavior nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setTypesetterBehavior:") retVoid [argCLong (coerce value)]

-- | ********************** Get glyphs and glyph properties ***********************
--
-- ObjC selector: @- numberOfGlyphs@
numberOfGlyphs :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CULong
numberOfGlyphs nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "numberOfGlyphs") retCULong []

-- | @- extraLineFragmentRect@
extraLineFragmentRect :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSRect
extraLineFragmentRect nsLayoutManager  =
  sendMsgStret nsLayoutManager (mkSelector "extraLineFragmentRect") retNSRect []

-- | @- extraLineFragmentUsedRect@
extraLineFragmentUsedRect :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSRect
extraLineFragmentUsedRect nsLayoutManager  =
  sendMsgStret nsLayoutManager (mkSelector "extraLineFragmentUsedRect") retNSRect []

-- | @- extraLineFragmentTextContainer@
extraLineFragmentTextContainer :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextContainer)
extraLineFragmentTextContainer nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "extraLineFragmentTextContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- glyphGenerator@
glyphGenerator :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSGlyphGenerator)
glyphGenerator nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "glyphGenerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGlyphGenerator:@
setGlyphGenerator :: (IsNSLayoutManager nsLayoutManager, IsNSGlyphGenerator value) => nsLayoutManager -> value -> IO ()
setGlyphGenerator nsLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLayoutManager (mkSelector "setGlyphGenerator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesScreenFonts@
usesScreenFonts :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
usesScreenFonts nsLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutManager (mkSelector "usesScreenFonts") retCULong []

-- | @- setUsesScreenFonts:@
setUsesScreenFonts :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setUsesScreenFonts nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setUsesScreenFonts:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CFloat
hyphenationFactor nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "hyphenationFactor") retCFloat []

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CFloat -> IO ()
setHyphenationFactor nsLayoutManager  value =
  sendMsg nsLayoutManager (mkSelector "setHyphenationFactor:") retVoid [argCFloat (fromIntegral value)]

-- | @- firstTextView@
firstTextView :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextView)
firstTextView nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "firstTextView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textViewForBeginningOfSelection@
textViewForBeginningOfSelection :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextView)
textViewForBeginningOfSelection nsLayoutManager  =
  sendMsg nsLayoutManager (mkSelector "textViewForBeginningOfSelection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @replaceTextStorage:@
replaceTextStorageSelector :: Selector
replaceTextStorageSelector = mkSelector "replaceTextStorage:"

-- | @Selector@ for @addTextContainer:@
addTextContainerSelector :: Selector
addTextContainerSelector = mkSelector "addTextContainer:"

-- | @Selector@ for @insertTextContainer:atIndex:@
insertTextContainer_atIndexSelector :: Selector
insertTextContainer_atIndexSelector = mkSelector "insertTextContainer:atIndex:"

-- | @Selector@ for @removeTextContainerAtIndex:@
removeTextContainerAtIndexSelector :: Selector
removeTextContainerAtIndexSelector = mkSelector "removeTextContainerAtIndex:"

-- | @Selector@ for @textContainerChangedGeometry:@
textContainerChangedGeometrySelector :: Selector
textContainerChangedGeometrySelector = mkSelector "textContainerChangedGeometry:"

-- | @Selector@ for @textContainerChangedTextView:@
textContainerChangedTextViewSelector :: Selector
textContainerChangedTextViewSelector = mkSelector "textContainerChangedTextView:"

-- | @Selector@ for @invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:@
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector :: Selector
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector = mkSelector "invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:"

-- | @Selector@ for @invalidateLayoutForCharacterRange:actualCharacterRange:@
invalidateLayoutForCharacterRange_actualCharacterRangeSelector :: Selector
invalidateLayoutForCharacterRange_actualCharacterRangeSelector = mkSelector "invalidateLayoutForCharacterRange:actualCharacterRange:"

-- | @Selector@ for @invalidateDisplayForCharacterRange:@
invalidateDisplayForCharacterRangeSelector :: Selector
invalidateDisplayForCharacterRangeSelector = mkSelector "invalidateDisplayForCharacterRange:"

-- | @Selector@ for @invalidateDisplayForGlyphRange:@
invalidateDisplayForGlyphRangeSelector :: Selector
invalidateDisplayForGlyphRangeSelector = mkSelector "invalidateDisplayForGlyphRange:"

-- | @Selector@ for @processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:@
processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector :: Selector
processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector = mkSelector "processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:"

-- | @Selector@ for @ensureGlyphsForCharacterRange:@
ensureGlyphsForCharacterRangeSelector :: Selector
ensureGlyphsForCharacterRangeSelector = mkSelector "ensureGlyphsForCharacterRange:"

-- | @Selector@ for @ensureGlyphsForGlyphRange:@
ensureGlyphsForGlyphRangeSelector :: Selector
ensureGlyphsForGlyphRangeSelector = mkSelector "ensureGlyphsForGlyphRange:"

-- | @Selector@ for @ensureLayoutForCharacterRange:@
ensureLayoutForCharacterRangeSelector :: Selector
ensureLayoutForCharacterRangeSelector = mkSelector "ensureLayoutForCharacterRange:"

-- | @Selector@ for @ensureLayoutForGlyphRange:@
ensureLayoutForGlyphRangeSelector :: Selector
ensureLayoutForGlyphRangeSelector = mkSelector "ensureLayoutForGlyphRange:"

-- | @Selector@ for @ensureLayoutForTextContainer:@
ensureLayoutForTextContainerSelector :: Selector
ensureLayoutForTextContainerSelector = mkSelector "ensureLayoutForTextContainer:"

-- | @Selector@ for @ensureLayoutForBoundingRect:inTextContainer:@
ensureLayoutForBoundingRect_inTextContainerSelector :: Selector
ensureLayoutForBoundingRect_inTextContainerSelector = mkSelector "ensureLayoutForBoundingRect:inTextContainer:"

-- | @Selector@ for @CGGlyphAtIndex:isValidIndex:@
cgGlyphAtIndex_isValidIndexSelector :: Selector
cgGlyphAtIndex_isValidIndexSelector = mkSelector "CGGlyphAtIndex:isValidIndex:"

-- | @Selector@ for @CGGlyphAtIndex:@
cgGlyphAtIndexSelector :: Selector
cgGlyphAtIndexSelector = mkSelector "CGGlyphAtIndex:"

-- | @Selector@ for @isValidGlyphIndex:@
isValidGlyphIndexSelector :: Selector
isValidGlyphIndexSelector = mkSelector "isValidGlyphIndex:"

-- | @Selector@ for @propertyForGlyphAtIndex:@
propertyForGlyphAtIndexSelector :: Selector
propertyForGlyphAtIndexSelector = mkSelector "propertyForGlyphAtIndex:"

-- | @Selector@ for @characterIndexForGlyphAtIndex:@
characterIndexForGlyphAtIndexSelector :: Selector
characterIndexForGlyphAtIndexSelector = mkSelector "characterIndexForGlyphAtIndex:"

-- | @Selector@ for @glyphIndexForCharacterAtIndex:@
glyphIndexForCharacterAtIndexSelector :: Selector
glyphIndexForCharacterAtIndexSelector = mkSelector "glyphIndexForCharacterAtIndex:"

-- | @Selector@ for @setTextContainer:forGlyphRange:@
setTextContainer_forGlyphRangeSelector :: Selector
setTextContainer_forGlyphRangeSelector = mkSelector "setTextContainer:forGlyphRange:"

-- | @Selector@ for @setLineFragmentRect:forGlyphRange:usedRect:@
setLineFragmentRect_forGlyphRange_usedRectSelector :: Selector
setLineFragmentRect_forGlyphRange_usedRectSelector = mkSelector "setLineFragmentRect:forGlyphRange:usedRect:"

-- | @Selector@ for @setExtraLineFragmentRect:usedRect:textContainer:@
setExtraLineFragmentRect_usedRect_textContainerSelector :: Selector
setExtraLineFragmentRect_usedRect_textContainerSelector = mkSelector "setExtraLineFragmentRect:usedRect:textContainer:"

-- | @Selector@ for @setLocation:forStartOfGlyphRange:@
setLocation_forStartOfGlyphRangeSelector :: Selector
setLocation_forStartOfGlyphRangeSelector = mkSelector "setLocation:forStartOfGlyphRange:"

-- | @Selector@ for @setNotShownAttribute:forGlyphAtIndex:@
setNotShownAttribute_forGlyphAtIndexSelector :: Selector
setNotShownAttribute_forGlyphAtIndexSelector = mkSelector "setNotShownAttribute:forGlyphAtIndex:"

-- | @Selector@ for @setDrawsOutsideLineFragment:forGlyphAtIndex:@
setDrawsOutsideLineFragment_forGlyphAtIndexSelector :: Selector
setDrawsOutsideLineFragment_forGlyphAtIndexSelector = mkSelector "setDrawsOutsideLineFragment:forGlyphAtIndex:"

-- | @Selector@ for @setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRangeSelector :: Selector
setAttachmentSize_forGlyphRangeSelector = mkSelector "setAttachmentSize:forGlyphRange:"

-- | @Selector@ for @getFirstUnlaidCharacterIndex:glyphIndex:@
getFirstUnlaidCharacterIndex_glyphIndexSelector :: Selector
getFirstUnlaidCharacterIndex_glyphIndexSelector = mkSelector "getFirstUnlaidCharacterIndex:glyphIndex:"

-- | @Selector@ for @firstUnlaidCharacterIndex@
firstUnlaidCharacterIndexSelector :: Selector
firstUnlaidCharacterIndexSelector = mkSelector "firstUnlaidCharacterIndex"

-- | @Selector@ for @firstUnlaidGlyphIndex@
firstUnlaidGlyphIndexSelector :: Selector
firstUnlaidGlyphIndexSelector = mkSelector "firstUnlaidGlyphIndex"

-- | @Selector@ for @textContainerForGlyphAtIndex:effectiveRange:@
textContainerForGlyphAtIndex_effectiveRangeSelector :: Selector
textContainerForGlyphAtIndex_effectiveRangeSelector = mkSelector "textContainerForGlyphAtIndex:effectiveRange:"

-- | @Selector@ for @textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector :: Selector
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector = mkSelector "textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:"

-- | @Selector@ for @usedRectForTextContainer:@
usedRectForTextContainerSelector :: Selector
usedRectForTextContainerSelector = mkSelector "usedRectForTextContainer:"

-- | @Selector@ for @lineFragmentRectForGlyphAtIndex:effectiveRange:@
lineFragmentRectForGlyphAtIndex_effectiveRangeSelector :: Selector
lineFragmentRectForGlyphAtIndex_effectiveRangeSelector = mkSelector "lineFragmentRectForGlyphAtIndex:effectiveRange:"

-- | @Selector@ for @lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector :: Selector
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector = mkSelector "lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:"

-- | @Selector@ for @lineFragmentUsedRectForGlyphAtIndex:effectiveRange:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector :: Selector
lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector = mkSelector "lineFragmentUsedRectForGlyphAtIndex:effectiveRange:"

-- | @Selector@ for @lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector :: Selector
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector = mkSelector "lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:"

-- | @Selector@ for @locationForGlyphAtIndex:@
locationForGlyphAtIndexSelector :: Selector
locationForGlyphAtIndexSelector = mkSelector "locationForGlyphAtIndex:"

-- | @Selector@ for @notShownAttributeForGlyphAtIndex:@
notShownAttributeForGlyphAtIndexSelector :: Selector
notShownAttributeForGlyphAtIndexSelector = mkSelector "notShownAttributeForGlyphAtIndex:"

-- | @Selector@ for @drawsOutsideLineFragmentForGlyphAtIndex:@
drawsOutsideLineFragmentForGlyphAtIndexSelector :: Selector
drawsOutsideLineFragmentForGlyphAtIndexSelector = mkSelector "drawsOutsideLineFragmentForGlyphAtIndex:"

-- | @Selector@ for @attachmentSizeForGlyphAtIndex:@
attachmentSizeForGlyphAtIndexSelector :: Selector
attachmentSizeForGlyphAtIndexSelector = mkSelector "attachmentSizeForGlyphAtIndex:"

-- | @Selector@ for @truncatedGlyphRangeInLineFragmentForGlyphAtIndex:@
truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector :: Selector
truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector = mkSelector "truncatedGlyphRangeInLineFragmentForGlyphAtIndex:"

-- | @Selector@ for @glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRangeSelector :: Selector
glyphRangeForCharacterRange_actualCharacterRangeSelector = mkSelector "glyphRangeForCharacterRange:actualCharacterRange:"

-- | @Selector@ for @characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRangeSelector :: Selector
characterRangeForGlyphRange_actualGlyphRangeSelector = mkSelector "characterRangeForGlyphRange:actualGlyphRange:"

-- | @Selector@ for @glyphRangeForTextContainer:@
glyphRangeForTextContainerSelector :: Selector
glyphRangeForTextContainerSelector = mkSelector "glyphRangeForTextContainer:"

-- | @Selector@ for @rangeOfNominallySpacedGlyphsContainingIndex:@
rangeOfNominallySpacedGlyphsContainingIndexSelector :: Selector
rangeOfNominallySpacedGlyphsContainingIndexSelector = mkSelector "rangeOfNominallySpacedGlyphsContainingIndex:"

-- | @Selector@ for @boundingRectForGlyphRange:inTextContainer:@
boundingRectForGlyphRange_inTextContainerSelector :: Selector
boundingRectForGlyphRange_inTextContainerSelector = mkSelector "boundingRectForGlyphRange:inTextContainer:"

-- | @Selector@ for @glyphRangeForBoundingRect:inTextContainer:@
glyphRangeForBoundingRect_inTextContainerSelector :: Selector
glyphRangeForBoundingRect_inTextContainerSelector = mkSelector "glyphRangeForBoundingRect:inTextContainer:"

-- | @Selector@ for @glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:@
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector :: Selector
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector = mkSelector "glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:"

-- | @Selector@ for @glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:@
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector :: Selector
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector = mkSelector "glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:"

-- | @Selector@ for @glyphIndexForPoint:inTextContainer:@
glyphIndexForPoint_inTextContainerSelector :: Selector
glyphIndexForPoint_inTextContainerSelector = mkSelector "glyphIndexForPoint:inTextContainer:"

-- | @Selector@ for @fractionOfDistanceThroughGlyphForPoint:inTextContainer:@
fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector :: Selector
fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector = mkSelector "fractionOfDistanceThroughGlyphForPoint:inTextContainer:"

-- | @Selector@ for @characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:@
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector :: Selector
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector = mkSelector "characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:"

-- | @Selector@ for @getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:@
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector :: Selector
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector = mkSelector "getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:"

-- | @Selector@ for @enumerateLineFragmentsForGlyphRange:usingBlock:@
enumerateLineFragmentsForGlyphRange_usingBlockSelector :: Selector
enumerateLineFragmentsForGlyphRange_usingBlockSelector = mkSelector "enumerateLineFragmentsForGlyphRange:usingBlock:"

-- | @Selector@ for @enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:@
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector :: Selector
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector = mkSelector "enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:"

-- | @Selector@ for @drawBackgroundForGlyphRange:atPoint:@
drawBackgroundForGlyphRange_atPointSelector :: Selector
drawBackgroundForGlyphRange_atPointSelector = mkSelector "drawBackgroundForGlyphRange:atPoint:"

-- | @Selector@ for @drawGlyphsForGlyphRange:atPoint:@
drawGlyphsForGlyphRange_atPointSelector :: Selector
drawGlyphsForGlyphRange_atPointSelector = mkSelector "drawGlyphsForGlyphRange:atPoint:"

-- | @Selector@ for @fillBackgroundRectArray:count:forCharacterRange:color:@
fillBackgroundRectArray_count_forCharacterRange_colorSelector :: Selector
fillBackgroundRectArray_count_forCharacterRange_colorSelector = mkSelector "fillBackgroundRectArray:count:forCharacterRange:color:"

-- | @Selector@ for @drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @showAttachmentCell:inRect:characterIndex:@
showAttachmentCell_inRect_characterIndexSelector :: Selector
showAttachmentCell_inRect_characterIndexSelector = mkSelector "showAttachmentCell:inRect:characterIndex:"

-- | @Selector@ for @setLayoutRect:forTextBlock:glyphRange:@
setLayoutRect_forTextBlock_glyphRangeSelector :: Selector
setLayoutRect_forTextBlock_glyphRangeSelector = mkSelector "setLayoutRect:forTextBlock:glyphRange:"

-- | @Selector@ for @setBoundsRect:forTextBlock:glyphRange:@
setBoundsRect_forTextBlock_glyphRangeSelector :: Selector
setBoundsRect_forTextBlock_glyphRangeSelector = mkSelector "setBoundsRect:forTextBlock:glyphRange:"

-- | @Selector@ for @layoutRectForTextBlock:glyphRange:@
layoutRectForTextBlock_glyphRangeSelector :: Selector
layoutRectForTextBlock_glyphRangeSelector = mkSelector "layoutRectForTextBlock:glyphRange:"

-- | @Selector@ for @boundsRectForTextBlock:glyphRange:@
boundsRectForTextBlock_glyphRangeSelector :: Selector
boundsRectForTextBlock_glyphRangeSelector = mkSelector "boundsRectForTextBlock:glyphRange:"

-- | @Selector@ for @layoutRectForTextBlock:atIndex:effectiveRange:@
layoutRectForTextBlock_atIndex_effectiveRangeSelector :: Selector
layoutRectForTextBlock_atIndex_effectiveRangeSelector = mkSelector "layoutRectForTextBlock:atIndex:effectiveRange:"

-- | @Selector@ for @boundsRectForTextBlock:atIndex:effectiveRange:@
boundsRectForTextBlock_atIndex_effectiveRangeSelector :: Selector
boundsRectForTextBlock_atIndex_effectiveRangeSelector = mkSelector "boundsRectForTextBlock:atIndex:effectiveRange:"

-- | @Selector@ for @temporaryAttributesAtCharacterIndex:effectiveRange:@
temporaryAttributesAtCharacterIndex_effectiveRangeSelector :: Selector
temporaryAttributesAtCharacterIndex_effectiveRangeSelector = mkSelector "temporaryAttributesAtCharacterIndex:effectiveRange:"

-- | @Selector@ for @setTemporaryAttributes:forCharacterRange:@
setTemporaryAttributes_forCharacterRangeSelector :: Selector
setTemporaryAttributes_forCharacterRangeSelector = mkSelector "setTemporaryAttributes:forCharacterRange:"

-- | @Selector@ for @addTemporaryAttributes:forCharacterRange:@
addTemporaryAttributes_forCharacterRangeSelector :: Selector
addTemporaryAttributes_forCharacterRangeSelector = mkSelector "addTemporaryAttributes:forCharacterRange:"

-- | @Selector@ for @removeTemporaryAttribute:forCharacterRange:@
removeTemporaryAttribute_forCharacterRangeSelector :: Selector
removeTemporaryAttribute_forCharacterRangeSelector = mkSelector "removeTemporaryAttribute:forCharacterRange:"

-- | @Selector@ for @temporaryAttribute:atCharacterIndex:effectiveRange:@
temporaryAttribute_atCharacterIndex_effectiveRangeSelector :: Selector
temporaryAttribute_atCharacterIndex_effectiveRangeSelector = mkSelector "temporaryAttribute:atCharacterIndex:effectiveRange:"

-- | @Selector@ for @temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector :: Selector
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector = mkSelector "temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector :: Selector
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector = mkSelector "temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @addTemporaryAttribute:value:forCharacterRange:@
addTemporaryAttribute_value_forCharacterRangeSelector :: Selector
addTemporaryAttribute_value_forCharacterRangeSelector = mkSelector "addTemporaryAttribute:value:forCharacterRange:"

-- | @Selector@ for @defaultLineHeightForFont:@
defaultLineHeightForFontSelector :: Selector
defaultLineHeightForFontSelector = mkSelector "defaultLineHeightForFont:"

-- | @Selector@ for @defaultBaselineOffsetForFont:@
defaultBaselineOffsetForFontSelector :: Selector
defaultBaselineOffsetForFontSelector = mkSelector "defaultBaselineOffsetForFont:"

-- | @Selector@ for @glyphAtIndex:isValidIndex:@
glyphAtIndex_isValidIndexSelector :: Selector
glyphAtIndex_isValidIndexSelector = mkSelector "glyphAtIndex:isValidIndex:"

-- | @Selector@ for @glyphAtIndex:@
glyphAtIndexSelector :: Selector
glyphAtIndexSelector = mkSelector "glyphAtIndex:"

-- | @Selector@ for @rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:@
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector :: Selector
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector = mkSelector "rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:"

-- | @Selector@ for @rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:@
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector :: Selector
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector = mkSelector "rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:"

-- | @Selector@ for @substituteFontForFont:@
substituteFontForFontSelector :: Selector
substituteFontForFontSelector = mkSelector "substituteFontForFont:"

-- | @Selector@ for @insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndexSelector :: Selector
insertGlyph_atGlyphIndex_characterIndexSelector = mkSelector "insertGlyph:atGlyphIndex:characterIndex:"

-- | @Selector@ for @replaceGlyphAtIndex:withGlyph:@
replaceGlyphAtIndex_withGlyphSelector :: Selector
replaceGlyphAtIndex_withGlyphSelector = mkSelector "replaceGlyphAtIndex:withGlyph:"

-- | @Selector@ for @deleteGlyphsInRange:@
deleteGlyphsInRangeSelector :: Selector
deleteGlyphsInRangeSelector = mkSelector "deleteGlyphsInRange:"

-- | @Selector@ for @setCharacterIndex:forGlyphAtIndex:@
setCharacterIndex_forGlyphAtIndexSelector :: Selector
setCharacterIndex_forGlyphAtIndexSelector = mkSelector "setCharacterIndex:forGlyphAtIndex:"

-- | @Selector@ for @setIntAttribute:value:forGlyphAtIndex:@
setIntAttribute_value_forGlyphAtIndexSelector :: Selector
setIntAttribute_value_forGlyphAtIndexSelector = mkSelector "setIntAttribute:value:forGlyphAtIndex:"

-- | @Selector@ for @invalidateGlyphsOnLayoutInvalidationForGlyphRange:@
invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector :: Selector
invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector = mkSelector "invalidateGlyphsOnLayoutInvalidationForGlyphRange:"

-- | @Selector@ for @intAttribute:forGlyphAtIndex:@
intAttribute_forGlyphAtIndexSelector :: Selector
intAttribute_forGlyphAtIndexSelector = mkSelector "intAttribute:forGlyphAtIndex:"

-- | @Selector@ for @invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:@
invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector :: Selector
invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector = mkSelector "invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:"

-- | @Selector@ for @textStorage:edited:range:changeInLength:invalidatedRange:@
textStorage_edited_range_changeInLength_invalidatedRangeSelector :: Selector
textStorage_edited_range_changeInLength_invalidatedRangeSelector = mkSelector "textStorage:edited:range:changeInLength:invalidatedRange:"

-- | @Selector@ for @setLocations:startingGlyphIndexes:count:forGlyphRange:@
setLocations_startingGlyphIndexes_count_forGlyphRangeSelector :: Selector
setLocations_startingGlyphIndexes_count_forGlyphRangeSelector = mkSelector "setLocations:startingGlyphIndexes:count:forGlyphRange:"

-- | @Selector@ for @showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:@
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector :: Selector
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector = mkSelector "showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:"

-- | @Selector@ for @rulerMarkersForTextView:paragraphStyle:ruler:@
rulerMarkersForTextView_paragraphStyle_rulerSelector :: Selector
rulerMarkersForTextView_paragraphStyle_rulerSelector = mkSelector "rulerMarkersForTextView:paragraphStyle:ruler:"

-- | @Selector@ for @rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:@
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector :: Selector
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector = mkSelector "rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:"

-- | @Selector@ for @layoutManagerOwnsFirstResponderInWindow:@
layoutManagerOwnsFirstResponderInWindowSelector :: Selector
layoutManagerOwnsFirstResponderInWindowSelector = mkSelector "layoutManagerOwnsFirstResponderInWindow:"

-- | @Selector@ for @textStorage@
textStorageSelector :: Selector
textStorageSelector = mkSelector "textStorage"

-- | @Selector@ for @setTextStorage:@
setTextStorageSelector :: Selector
setTextStorageSelector = mkSelector "setTextStorage:"

-- | @Selector@ for @textContainers@
textContainersSelector :: Selector
textContainersSelector = mkSelector "textContainers"

-- | @Selector@ for @showsInvisibleCharacters@
showsInvisibleCharactersSelector :: Selector
showsInvisibleCharactersSelector = mkSelector "showsInvisibleCharacters"

-- | @Selector@ for @setShowsInvisibleCharacters:@
setShowsInvisibleCharactersSelector :: Selector
setShowsInvisibleCharactersSelector = mkSelector "setShowsInvisibleCharacters:"

-- | @Selector@ for @showsControlCharacters@
showsControlCharactersSelector :: Selector
showsControlCharactersSelector = mkSelector "showsControlCharacters"

-- | @Selector@ for @setShowsControlCharacters:@
setShowsControlCharactersSelector :: Selector
setShowsControlCharactersSelector = mkSelector "setShowsControlCharacters:"

-- | @Selector@ for @usesDefaultHyphenation@
usesDefaultHyphenationSelector :: Selector
usesDefaultHyphenationSelector = mkSelector "usesDefaultHyphenation"

-- | @Selector@ for @setUsesDefaultHyphenation:@
setUsesDefaultHyphenationSelector :: Selector
setUsesDefaultHyphenationSelector = mkSelector "setUsesDefaultHyphenation:"

-- | @Selector@ for @usesFontLeading@
usesFontLeadingSelector :: Selector
usesFontLeadingSelector = mkSelector "usesFontLeading"

-- | @Selector@ for @setUsesFontLeading:@
setUsesFontLeadingSelector :: Selector
setUsesFontLeadingSelector = mkSelector "setUsesFontLeading:"

-- | @Selector@ for @allowsNonContiguousLayout@
allowsNonContiguousLayoutSelector :: Selector
allowsNonContiguousLayoutSelector = mkSelector "allowsNonContiguousLayout"

-- | @Selector@ for @setAllowsNonContiguousLayout:@
setAllowsNonContiguousLayoutSelector :: Selector
setAllowsNonContiguousLayoutSelector = mkSelector "setAllowsNonContiguousLayout:"

-- | @Selector@ for @hasNonContiguousLayout@
hasNonContiguousLayoutSelector :: Selector
hasNonContiguousLayoutSelector = mkSelector "hasNonContiguousLayout"

-- | @Selector@ for @limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContentsSelector :: Selector
limitsLayoutForSuspiciousContentsSelector = mkSelector "limitsLayoutForSuspiciousContents"

-- | @Selector@ for @setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContentsSelector :: Selector
setLimitsLayoutForSuspiciousContentsSelector = mkSelector "setLimitsLayoutForSuspiciousContents:"

-- | @Selector@ for @backgroundLayoutEnabled@
backgroundLayoutEnabledSelector :: Selector
backgroundLayoutEnabledSelector = mkSelector "backgroundLayoutEnabled"

-- | @Selector@ for @setBackgroundLayoutEnabled:@
setBackgroundLayoutEnabledSelector :: Selector
setBackgroundLayoutEnabledSelector = mkSelector "setBackgroundLayoutEnabled:"

-- | @Selector@ for @defaultAttachmentScaling@
defaultAttachmentScalingSelector :: Selector
defaultAttachmentScalingSelector = mkSelector "defaultAttachmentScaling"

-- | @Selector@ for @setDefaultAttachmentScaling:@
setDefaultAttachmentScalingSelector :: Selector
setDefaultAttachmentScalingSelector = mkSelector "setDefaultAttachmentScaling:"

-- | @Selector@ for @typesetter@
typesetterSelector :: Selector
typesetterSelector = mkSelector "typesetter"

-- | @Selector@ for @setTypesetter:@
setTypesetterSelector :: Selector
setTypesetterSelector = mkSelector "setTypesetter:"

-- | @Selector@ for @typesetterBehavior@
typesetterBehaviorSelector :: Selector
typesetterBehaviorSelector = mkSelector "typesetterBehavior"

-- | @Selector@ for @setTypesetterBehavior:@
setTypesetterBehaviorSelector :: Selector
setTypesetterBehaviorSelector = mkSelector "setTypesetterBehavior:"

-- | @Selector@ for @numberOfGlyphs@
numberOfGlyphsSelector :: Selector
numberOfGlyphsSelector = mkSelector "numberOfGlyphs"

-- | @Selector@ for @extraLineFragmentRect@
extraLineFragmentRectSelector :: Selector
extraLineFragmentRectSelector = mkSelector "extraLineFragmentRect"

-- | @Selector@ for @extraLineFragmentUsedRect@
extraLineFragmentUsedRectSelector :: Selector
extraLineFragmentUsedRectSelector = mkSelector "extraLineFragmentUsedRect"

-- | @Selector@ for @extraLineFragmentTextContainer@
extraLineFragmentTextContainerSelector :: Selector
extraLineFragmentTextContainerSelector = mkSelector "extraLineFragmentTextContainer"

-- | @Selector@ for @glyphGenerator@
glyphGeneratorSelector :: Selector
glyphGeneratorSelector = mkSelector "glyphGenerator"

-- | @Selector@ for @setGlyphGenerator:@
setGlyphGeneratorSelector :: Selector
setGlyphGeneratorSelector = mkSelector "setGlyphGenerator:"

-- | @Selector@ for @usesScreenFonts@
usesScreenFontsSelector :: Selector
usesScreenFontsSelector = mkSelector "usesScreenFonts"

-- | @Selector@ for @setUsesScreenFonts:@
setUsesScreenFontsSelector :: Selector
setUsesScreenFontsSelector = mkSelector "setUsesScreenFonts:"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @setHyphenationFactor:@
setHyphenationFactorSelector :: Selector
setHyphenationFactorSelector = mkSelector "setHyphenationFactor:"

-- | @Selector@ for @firstTextView@
firstTextViewSelector :: Selector
firstTextViewSelector = mkSelector "firstTextView"

-- | @Selector@ for @textViewForBeginningOfSelection@
textViewForBeginningOfSelectionSelector :: Selector
textViewForBeginningOfSelectionSelector = mkSelector "textViewForBeginningOfSelection"

