{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setGlyphs_properties_characterIndexes_font_forGlyphRange
  , cgGlyphAtIndex_isValidIndex
  , cgGlyphAtIndex
  , isValidGlyphIndex
  , propertyForGlyphAtIndex
  , characterIndexForGlyphAtIndex
  , glyphIndexForCharacterAtIndex
  , getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevels
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
  , insertGlyphs_length_forStartingGlyphAtIndex_characterIndex
  , insertGlyph_atGlyphIndex_characterIndex
  , replaceGlyphAtIndex_withGlyph
  , deleteGlyphsInRange
  , setCharacterIndex_forGlyphAtIndex
  , setIntAttribute_value_forGlyphAtIndex
  , invalidateGlyphsOnLayoutInvalidationForGlyphRange
  , intAttribute_forGlyphAtIndex
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels
  , getGlyphs_range
  , invalidateLayoutForCharacterRange_isSoft_actualCharacterRange
  , textStorage_edited_range_changeInLength_invalidatedRange
  , setLocations_startingGlyphIndexes_count_forGlyphRange
  , showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment
  , showCGGlyphs_positions_count_font_matrix_attributes_inContext
  , rulerMarkersForTextView_paragraphStyle_ruler
  , rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled
  , layoutManagerOwnsFirstResponderInWindow
  , textStorage
  , setTextStorage
  , textContainers
  , delegate
  , setDelegate
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
  , addTemporaryAttribute_value_forCharacterRangeSelector
  , addTemporaryAttributes_forCharacterRangeSelector
  , addTextContainerSelector
  , allowsNonContiguousLayoutSelector
  , attachmentSizeForGlyphAtIndexSelector
  , backgroundLayoutEnabledSelector
  , boundingRectForGlyphRange_inTextContainerSelector
  , boundsRectForTextBlock_atIndex_effectiveRangeSelector
  , boundsRectForTextBlock_glyphRangeSelector
  , cgGlyphAtIndexSelector
  , cgGlyphAtIndex_isValidIndexSelector
  , characterIndexForGlyphAtIndexSelector
  , characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector
  , characterRangeForGlyphRange_actualGlyphRangeSelector
  , defaultAttachmentScalingSelector
  , defaultBaselineOffsetForFontSelector
  , defaultLineHeightForFontSelector
  , delegateSelector
  , deleteGlyphsInRangeSelector
  , drawBackgroundForGlyphRange_atPointSelector
  , drawGlyphsForGlyphRange_atPointSelector
  , drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , drawsOutsideLineFragmentForGlyphAtIndexSelector
  , ensureGlyphsForCharacterRangeSelector
  , ensureGlyphsForGlyphRangeSelector
  , ensureLayoutForBoundingRect_inTextContainerSelector
  , ensureLayoutForCharacterRangeSelector
  , ensureLayoutForGlyphRangeSelector
  , ensureLayoutForTextContainerSelector
  , enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector
  , enumerateLineFragmentsForGlyphRange_usingBlockSelector
  , extraLineFragmentRectSelector
  , extraLineFragmentTextContainerSelector
  , extraLineFragmentUsedRectSelector
  , fillBackgroundRectArray_count_forCharacterRange_colorSelector
  , firstTextViewSelector
  , firstUnlaidCharacterIndexSelector
  , firstUnlaidGlyphIndexSelector
  , fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector
  , getFirstUnlaidCharacterIndex_glyphIndexSelector
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector
  , getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevelsSelector
  , getGlyphs_rangeSelector
  , getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector
  , glyphAtIndexSelector
  , glyphAtIndex_isValidIndexSelector
  , glyphGeneratorSelector
  , glyphIndexForCharacterAtIndexSelector
  , glyphIndexForPoint_inTextContainerSelector
  , glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector
  , glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector
  , glyphRangeForBoundingRect_inTextContainerSelector
  , glyphRangeForCharacterRange_actualCharacterRangeSelector
  , glyphRangeForTextContainerSelector
  , hasNonContiguousLayoutSelector
  , hyphenationFactorSelector
  , initSelector
  , initWithCoderSelector
  , insertGlyph_atGlyphIndex_characterIndexSelector
  , insertGlyphs_length_forStartingGlyphAtIndex_characterIndexSelector
  , insertTextContainer_atIndexSelector
  , intAttribute_forGlyphAtIndexSelector
  , invalidateDisplayForCharacterRangeSelector
  , invalidateDisplayForGlyphRangeSelector
  , invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector
  , invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector
  , invalidateLayoutForCharacterRange_actualCharacterRangeSelector
  , invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector
  , isValidGlyphIndexSelector
  , layoutManagerOwnsFirstResponderInWindowSelector
  , layoutRectForTextBlock_atIndex_effectiveRangeSelector
  , layoutRectForTextBlock_glyphRangeSelector
  , limitsLayoutForSuspiciousContentsSelector
  , lineFragmentRectForGlyphAtIndex_effectiveRangeSelector
  , lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector
  , lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector
  , lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector
  , locationForGlyphAtIndexSelector
  , notShownAttributeForGlyphAtIndexSelector
  , numberOfGlyphsSelector
  , processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector
  , propertyForGlyphAtIndexSelector
  , rangeOfNominallySpacedGlyphsContainingIndexSelector
  , rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector
  , rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector
  , removeTemporaryAttribute_forCharacterRangeSelector
  , removeTextContainerAtIndexSelector
  , replaceGlyphAtIndex_withGlyphSelector
  , replaceTextStorageSelector
  , rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector
  , rulerMarkersForTextView_paragraphStyle_rulerSelector
  , setAllowsNonContiguousLayoutSelector
  , setAttachmentSize_forGlyphRangeSelector
  , setBackgroundLayoutEnabledSelector
  , setBoundsRect_forTextBlock_glyphRangeSelector
  , setCharacterIndex_forGlyphAtIndexSelector
  , setDefaultAttachmentScalingSelector
  , setDelegateSelector
  , setDrawsOutsideLineFragment_forGlyphAtIndexSelector
  , setExtraLineFragmentRect_usedRect_textContainerSelector
  , setGlyphGeneratorSelector
  , setGlyphs_properties_characterIndexes_font_forGlyphRangeSelector
  , setHyphenationFactorSelector
  , setIntAttribute_value_forGlyphAtIndexSelector
  , setLayoutRect_forTextBlock_glyphRangeSelector
  , setLimitsLayoutForSuspiciousContentsSelector
  , setLineFragmentRect_forGlyphRange_usedRectSelector
  , setLocation_forStartOfGlyphRangeSelector
  , setLocations_startingGlyphIndexes_count_forGlyphRangeSelector
  , setNotShownAttribute_forGlyphAtIndexSelector
  , setShowsControlCharactersSelector
  , setShowsInvisibleCharactersSelector
  , setTemporaryAttributes_forCharacterRangeSelector
  , setTextContainer_forGlyphRangeSelector
  , setTextStorageSelector
  , setTypesetterBehaviorSelector
  , setTypesetterSelector
  , setUsesDefaultHyphenationSelector
  , setUsesFontLeadingSelector
  , setUsesScreenFontsSelector
  , showAttachmentCell_inRect_characterIndexSelector
  , showCGGlyphs_positions_count_font_matrix_attributes_inContextSelector
  , showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector
  , showsControlCharactersSelector
  , showsInvisibleCharactersSelector
  , strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , substituteFontForFontSelector
  , temporaryAttribute_atCharacterIndex_effectiveRangeSelector
  , temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector
  , temporaryAttributesAtCharacterIndex_effectiveRangeSelector
  , temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector
  , textContainerChangedGeometrySelector
  , textContainerChangedTextViewSelector
  , textContainerForGlyphAtIndex_effectiveRangeSelector
  , textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector
  , textContainersSelector
  , textStorageSelector
  , textStorage_edited_range_changeInLength_invalidatedRangeSelector
  , textViewForBeginningOfSelectionSelector
  , truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector
  , typesetterBehaviorSelector
  , typesetterSelector
  , underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector
  , usedRectForTextContainerSelector
  , usesDefaultHyphenationSelector
  , usesFontLeadingSelector
  , usesScreenFontsSelector

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

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- init@
init_ :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSLayoutManager)
init_ nsLayoutManager =
  sendOwnedMessage nsLayoutManager initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSLayoutManager nsLayoutManager, IsNSCoder coder) => nsLayoutManager -> coder -> IO (Id NSLayoutManager)
initWithCoder nsLayoutManager coder =
  sendOwnedMessage nsLayoutManager initWithCoderSelector (toNSCoder coder)

-- | @- replaceTextStorage:@
replaceTextStorage :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage newTextStorage) => nsLayoutManager -> newTextStorage -> IO ()
replaceTextStorage nsLayoutManager newTextStorage =
  sendMessage nsLayoutManager replaceTextStorageSelector (toNSTextStorage newTextStorage)

-- | @- addTextContainer:@
addTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
addTextContainer nsLayoutManager container =
  sendMessage nsLayoutManager addTextContainerSelector (toNSTextContainer container)

-- | @- insertTextContainer:atIndex:@
insertTextContainer_atIndex :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> CULong -> IO ()
insertTextContainer_atIndex nsLayoutManager container index =
  sendMessage nsLayoutManager insertTextContainer_atIndexSelector (toNSTextContainer container) index

-- | @- removeTextContainerAtIndex:@
removeTextContainerAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO ()
removeTextContainerAtIndex nsLayoutManager index =
  sendMessage nsLayoutManager removeTextContainerAtIndexSelector index

-- | @- textContainerChangedGeometry:@
textContainerChangedGeometry :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
textContainerChangedGeometry nsLayoutManager container =
  sendMessage nsLayoutManager textContainerChangedGeometrySelector (toNSTextContainer container)

-- | @- textContainerChangedTextView:@
textContainerChangedTextView :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
textContainerChangedTextView nsLayoutManager container =
  sendMessage nsLayoutManager textContainerChangedTextViewSelector (toNSTextContainer container)

-- | ************************ Invalidation *************************
--
-- ObjC selector: @- invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:@
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> CLong -> Ptr NSRange -> IO ()
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange nsLayoutManager charRange delta actualCharRange =
  sendMessage nsLayoutManager invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector charRange delta actualCharRange

-- | @- invalidateLayoutForCharacterRange:actualCharacterRange:@
invalidateLayoutForCharacterRange_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr NSRange -> IO ()
invalidateLayoutForCharacterRange_actualCharacterRange nsLayoutManager charRange actualCharRange =
  sendMessage nsLayoutManager invalidateLayoutForCharacterRange_actualCharacterRangeSelector charRange actualCharRange

-- | @- invalidateDisplayForCharacterRange:@
invalidateDisplayForCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
invalidateDisplayForCharacterRange nsLayoutManager charRange =
  sendMessage nsLayoutManager invalidateDisplayForCharacterRangeSelector charRange

-- | @- invalidateDisplayForGlyphRange:@
invalidateDisplayForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
invalidateDisplayForGlyphRange nsLayoutManager glyphRange =
  sendMessage nsLayoutManager invalidateDisplayForGlyphRangeSelector glyphRange

-- | @- processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:@
processEditingForTextStorage_edited_range_changeInLength_invalidatedRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage textStorage) => nsLayoutManager -> textStorage -> NSTextStorageEditActions -> NSRange -> CLong -> NSRange -> IO ()
processEditingForTextStorage_edited_range_changeInLength_invalidatedRange nsLayoutManager textStorage editMask newCharRange delta invalidatedCharRange =
  sendMessage nsLayoutManager processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector (toNSTextStorage textStorage) editMask newCharRange delta invalidatedCharRange

-- | ********************** Causing glyph generation and layout ***********************
--
-- ObjC selector: @- ensureGlyphsForCharacterRange:@
ensureGlyphsForCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureGlyphsForCharacterRange nsLayoutManager charRange =
  sendMessage nsLayoutManager ensureGlyphsForCharacterRangeSelector charRange

-- | @- ensureGlyphsForGlyphRange:@
ensureGlyphsForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureGlyphsForGlyphRange nsLayoutManager glyphRange =
  sendMessage nsLayoutManager ensureGlyphsForGlyphRangeSelector glyphRange

-- | @- ensureLayoutForCharacterRange:@
ensureLayoutForCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureLayoutForCharacterRange nsLayoutManager charRange =
  sendMessage nsLayoutManager ensureLayoutForCharacterRangeSelector charRange

-- | @- ensureLayoutForGlyphRange:@
ensureLayoutForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
ensureLayoutForGlyphRange nsLayoutManager glyphRange =
  sendMessage nsLayoutManager ensureLayoutForGlyphRangeSelector glyphRange

-- | @- ensureLayoutForTextContainer:@
ensureLayoutForTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO ()
ensureLayoutForTextContainer nsLayoutManager container =
  sendMessage nsLayoutManager ensureLayoutForTextContainerSelector (toNSTextContainer container)

-- | @- ensureLayoutForBoundingRect:inTextContainer:@
ensureLayoutForBoundingRect_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> container -> IO ()
ensureLayoutForBoundingRect_inTextContainer nsLayoutManager bounds container =
  sendMessage nsLayoutManager ensureLayoutForBoundingRect_inTextContainerSelector bounds (toNSTextContainer container)

-- | ********************** Set glyphs and glyph properties ***********************
--
-- ObjC selector: @- setGlyphs:properties:characterIndexes:font:forGlyphRange:@
setGlyphs_properties_characterIndexes_font_forGlyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSFont aFont) => nsLayoutManager -> Const RawId -> Const (Ptr NSGlyphProperty) -> Const (Ptr CULong) -> aFont -> NSRange -> IO ()
setGlyphs_properties_characterIndexes_font_forGlyphRange nsLayoutManager glyphs props charIndexes aFont glyphRange =
  sendMessage nsLayoutManager setGlyphs_properties_characterIndexes_font_forGlyphRangeSelector glyphs props charIndexes (toNSFont aFont) glyphRange

-- | @- CGGlyphAtIndex:isValidIndex:@
cgGlyphAtIndex_isValidIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr Bool -> IO CUShort
cgGlyphAtIndex_isValidIndex nsLayoutManager glyphIndex isValidIndex =
  sendMessage nsLayoutManager cgGlyphAtIndex_isValidIndexSelector glyphIndex isValidIndex

-- | @- CGGlyphAtIndex:@
cgGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CUShort
cgGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager cgGlyphAtIndexSelector glyphIndex

-- | @- isValidGlyphIndex:@
isValidGlyphIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO Bool
isValidGlyphIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager isValidGlyphIndexSelector glyphIndex

-- | @- propertyForGlyphAtIndex:@
propertyForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSGlyphProperty
propertyForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager propertyForGlyphAtIndexSelector glyphIndex

-- | @- characterIndexForGlyphAtIndex:@
characterIndexForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CULong
characterIndexForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager characterIndexForGlyphAtIndexSelector glyphIndex

-- | @- glyphIndexForCharacterAtIndex:@
glyphIndexForCharacterAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CULong
glyphIndexForCharacterAtIndex nsLayoutManager charIndex =
  sendMessage nsLayoutManager glyphIndexForCharacterAtIndexSelector charIndex

-- | @- getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels:@
getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevels :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> RawId -> Ptr NSGlyphProperty -> Ptr CULong -> Ptr CUChar -> IO CULong
getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevels nsLayoutManager glyphRange glyphBuffer props charIndexBuffer bidiLevelBuffer =
  sendMessage nsLayoutManager getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevelsSelector glyphRange glyphBuffer props charIndexBuffer bidiLevelBuffer

-- | @- setTextContainer:forGlyphRange:@
setTextContainer_forGlyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> NSRange -> IO ()
setTextContainer_forGlyphRange nsLayoutManager container glyphRange =
  sendMessage nsLayoutManager setTextContainer_forGlyphRangeSelector (toNSTextContainer container) glyphRange

-- | @- setLineFragmentRect:forGlyphRange:usedRect:@
setLineFragmentRect_forGlyphRange_usedRect :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRect -> NSRange -> NSRect -> IO ()
setLineFragmentRect_forGlyphRange_usedRect nsLayoutManager fragmentRect glyphRange usedRect =
  sendMessage nsLayoutManager setLineFragmentRect_forGlyphRange_usedRectSelector fragmentRect glyphRange usedRect

-- | @- setExtraLineFragmentRect:usedRect:textContainer:@
setExtraLineFragmentRect_usedRect_textContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> NSRect -> container -> IO ()
setExtraLineFragmentRect_usedRect_textContainer nsLayoutManager fragmentRect usedRect container =
  sendMessage nsLayoutManager setExtraLineFragmentRect_usedRect_textContainerSelector fragmentRect usedRect (toNSTextContainer container)

-- | @- setLocation:forStartOfGlyphRange:@
setLocation_forStartOfGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSPoint -> NSRange -> IO ()
setLocation_forStartOfGlyphRange nsLayoutManager location glyphRange =
  sendMessage nsLayoutManager setLocation_forStartOfGlyphRangeSelector location glyphRange

-- | @- setNotShownAttribute:forGlyphAtIndex:@
setNotShownAttribute_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> CULong -> IO ()
setNotShownAttribute_forGlyphAtIndex nsLayoutManager flag glyphIndex =
  sendMessage nsLayoutManager setNotShownAttribute_forGlyphAtIndexSelector flag glyphIndex

-- | @- setDrawsOutsideLineFragment:forGlyphAtIndex:@
setDrawsOutsideLineFragment_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> CULong -> IO ()
setDrawsOutsideLineFragment_forGlyphAtIndex nsLayoutManager flag glyphIndex =
  sendMessage nsLayoutManager setDrawsOutsideLineFragment_forGlyphAtIndexSelector flag glyphIndex

-- | @- setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSSize -> NSRange -> IO ()
setAttachmentSize_forGlyphRange nsLayoutManager attachmentSize glyphRange =
  sendMessage nsLayoutManager setAttachmentSize_forGlyphRangeSelector attachmentSize glyphRange

-- | ********************** Get layout information ***********************
--
-- ObjC selector: @- getFirstUnlaidCharacterIndex:glyphIndex:@
getFirstUnlaidCharacterIndex_glyphIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Ptr CULong -> Ptr CULong -> IO ()
getFirstUnlaidCharacterIndex_glyphIndex nsLayoutManager charIndex glyphIndex =
  sendMessage nsLayoutManager getFirstUnlaidCharacterIndex_glyphIndexSelector charIndex glyphIndex

-- | @- firstUnlaidCharacterIndex@
firstUnlaidCharacterIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CULong
firstUnlaidCharacterIndex nsLayoutManager =
  sendMessage nsLayoutManager firstUnlaidCharacterIndexSelector

-- | @- firstUnlaidGlyphIndex@
firstUnlaidGlyphIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CULong
firstUnlaidGlyphIndex nsLayoutManager =
  sendMessage nsLayoutManager firstUnlaidGlyphIndexSelector

-- | @- textContainerForGlyphAtIndex:effectiveRange:@
textContainerForGlyphAtIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO (Id NSTextContainer)
textContainerForGlyphAtIndex_effectiveRange nsLayoutManager glyphIndex effectiveGlyphRange =
  sendMessage nsLayoutManager textContainerForGlyphAtIndex_effectiveRangeSelector glyphIndex effectiveGlyphRange

-- | @- textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> Bool -> IO (Id NSTextContainer)
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout nsLayoutManager glyphIndex effectiveGlyphRange flag =
  sendMessage nsLayoutManager textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector glyphIndex effectiveGlyphRange flag

-- | @- usedRectForTextContainer:@
usedRectForTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO NSRect
usedRectForTextContainer nsLayoutManager container =
  sendMessage nsLayoutManager usedRectForTextContainerSelector (toNSTextContainer container)

-- | @- lineFragmentRectForGlyphAtIndex:effectiveRange:@
lineFragmentRectForGlyphAtIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO NSRect
lineFragmentRectForGlyphAtIndex_effectiveRange nsLayoutManager glyphIndex effectiveGlyphRange =
  sendMessage nsLayoutManager lineFragmentRectForGlyphAtIndex_effectiveRangeSelector glyphIndex effectiveGlyphRange

-- | @- lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> Bool -> IO NSRect
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout nsLayoutManager glyphIndex effectiveGlyphRange flag =
  sendMessage nsLayoutManager lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector glyphIndex effectiveGlyphRange flag

-- | @- lineFragmentUsedRectForGlyphAtIndex:effectiveRange:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO NSRect
lineFragmentUsedRectForGlyphAtIndex_effectiveRange nsLayoutManager glyphIndex effectiveGlyphRange =
  sendMessage nsLayoutManager lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector glyphIndex effectiveGlyphRange

-- | @- lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> Bool -> IO NSRect
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout nsLayoutManager glyphIndex effectiveGlyphRange flag =
  sendMessage nsLayoutManager lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector glyphIndex effectiveGlyphRange flag

-- | @- locationForGlyphAtIndex:@
locationForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSPoint
locationForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager locationForGlyphAtIndexSelector glyphIndex

-- | @- notShownAttributeForGlyphAtIndex:@
notShownAttributeForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO Bool
notShownAttributeForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager notShownAttributeForGlyphAtIndexSelector glyphIndex

-- | @- drawsOutsideLineFragmentForGlyphAtIndex:@
drawsOutsideLineFragmentForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO Bool
drawsOutsideLineFragmentForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager drawsOutsideLineFragmentForGlyphAtIndexSelector glyphIndex

-- | @- attachmentSizeForGlyphAtIndex:@
attachmentSizeForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSSize
attachmentSizeForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager attachmentSizeForGlyphAtIndexSelector glyphIndex

-- | @- truncatedGlyphRangeInLineFragmentForGlyphAtIndex:@
truncatedGlyphRangeInLineFragmentForGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSRange
truncatedGlyphRangeInLineFragmentForGlyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector glyphIndex

-- | ********************** More sophisticated queries ***********************
--
-- ObjC selector: @- glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr NSRange -> IO NSRange
glyphRangeForCharacterRange_actualCharacterRange nsLayoutManager charRange actualCharRange =
  sendMessage nsLayoutManager glyphRangeForCharacterRange_actualCharacterRangeSelector charRange actualCharRange

-- | @- characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr NSRange -> IO NSRange
characterRangeForGlyphRange_actualGlyphRange nsLayoutManager glyphRange actualGlyphRange =
  sendMessage nsLayoutManager characterRangeForGlyphRange_actualGlyphRangeSelector glyphRange actualGlyphRange

-- | @- glyphRangeForTextContainer:@
glyphRangeForTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> container -> IO NSRange
glyphRangeForTextContainer nsLayoutManager container =
  sendMessage nsLayoutManager glyphRangeForTextContainerSelector (toNSTextContainer container)

-- | @- rangeOfNominallySpacedGlyphsContainingIndex:@
rangeOfNominallySpacedGlyphsContainingIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO NSRange
rangeOfNominallySpacedGlyphsContainingIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager rangeOfNominallySpacedGlyphsContainingIndexSelector glyphIndex

-- | @- boundingRectForGlyphRange:inTextContainer:@
boundingRectForGlyphRange_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRange -> container -> IO NSRect
boundingRectForGlyphRange_inTextContainer nsLayoutManager glyphRange container =
  sendMessage nsLayoutManager boundingRectForGlyphRange_inTextContainerSelector glyphRange (toNSTextContainer container)

-- | @- glyphRangeForBoundingRect:inTextContainer:@
glyphRangeForBoundingRect_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> container -> IO NSRange
glyphRangeForBoundingRect_inTextContainer nsLayoutManager bounds container =
  sendMessage nsLayoutManager glyphRangeForBoundingRect_inTextContainerSelector bounds (toNSTextContainer container)

-- | @- glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:@
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRect -> container -> IO NSRange
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer nsLayoutManager bounds container =
  sendMessage nsLayoutManager glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector bounds (toNSTextContainer container)

-- | @- glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:@
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> Ptr CDouble -> IO CULong
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph nsLayoutManager point container partialFraction =
  sendMessage nsLayoutManager glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector point (toNSTextContainer container) partialFraction

-- | @- glyphIndexForPoint:inTextContainer:@
glyphIndexForPoint_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> IO CULong
glyphIndexForPoint_inTextContainer nsLayoutManager point container =
  sendMessage nsLayoutManager glyphIndexForPoint_inTextContainerSelector point (toNSTextContainer container)

-- | @- fractionOfDistanceThroughGlyphForPoint:inTextContainer:@
fractionOfDistanceThroughGlyphForPoint_inTextContainer :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> IO CDouble
fractionOfDistanceThroughGlyphForPoint_inTextContainer nsLayoutManager point container =
  sendMessage nsLayoutManager fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector point (toNSTextContainer container)

-- | @- characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:@
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPoints :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSPoint -> container -> Ptr CDouble -> IO CULong
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPoints nsLayoutManager point container partialFraction =
  sendMessage nsLayoutManager characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector point (toNSTextContainer container) partialFraction

-- | @- getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:@
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Bool -> Bool -> Ptr CDouble -> Ptr CULong -> IO CULong
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes nsLayoutManager charIndex aFlag dFlag positions charIndexes =
  sendMessage nsLayoutManager getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector charIndex aFlag dFlag positions charIndexes

-- | @- enumerateLineFragmentsForGlyphRange:usingBlock:@
enumerateLineFragmentsForGlyphRange_usingBlock :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Ptr () -> IO ()
enumerateLineFragmentsForGlyphRange_usingBlock nsLayoutManager glyphRange block =
  sendMessage nsLayoutManager enumerateLineFragmentsForGlyphRange_usingBlockSelector glyphRange block

-- | @- enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:@
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlock :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer textContainer) => nsLayoutManager -> NSRange -> NSRange -> textContainer -> Ptr () -> IO ()
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlock nsLayoutManager glyphRange selectedRange textContainer block =
  sendMessage nsLayoutManager enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector glyphRange selectedRange (toNSTextContainer textContainer) block

-- | ********************** Drawing support ***********************
--
-- ObjC selector: @- drawBackgroundForGlyphRange:atPoint:@
drawBackgroundForGlyphRange_atPoint :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSPoint -> IO ()
drawBackgroundForGlyphRange_atPoint nsLayoutManager glyphsToShow origin =
  sendMessage nsLayoutManager drawBackgroundForGlyphRange_atPointSelector glyphsToShow origin

-- | @- drawGlyphsForGlyphRange:atPoint:@
drawGlyphsForGlyphRange_atPoint :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSPoint -> IO ()
drawGlyphsForGlyphRange_atPoint nsLayoutManager glyphsToShow origin =
  sendMessage nsLayoutManager drawGlyphsForGlyphRange_atPointSelector glyphsToShow origin

-- | @- fillBackgroundRectArray:count:forCharacterRange:color:@
fillBackgroundRectArray_count_forCharacterRange_color :: (IsNSLayoutManager nsLayoutManager, IsNSColor color) => nsLayoutManager -> Const (Ptr NSRect) -> CULong -> NSRange -> color -> IO ()
fillBackgroundRectArray_count_forCharacterRange_color nsLayoutManager rectArray rectCount charRange color =
  sendMessage nsLayoutManager fillBackgroundRectArray_count_forCharacterRange_colorSelector rectArray rectCount charRange (toNSColor color)

-- | @- drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> CDouble -> NSRect -> NSRange -> NSPoint -> IO ()
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager glyphRange underlineVal baselineOffset lineRect lineGlyphRange containerOrigin =
  sendMessage nsLayoutManager drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector glyphRange underlineVal baselineOffset lineRect lineGlyphRange containerOrigin

-- | @- underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> NSRect -> NSRange -> NSPoint -> IO ()
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager glyphRange underlineVal lineRect lineGlyphRange containerOrigin =
  sendMessage nsLayoutManager underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector glyphRange underlineVal lineRect lineGlyphRange containerOrigin

-- | @- drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> CDouble -> NSRect -> NSRange -> NSPoint -> IO ()
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager glyphRange strikethroughVal baselineOffset lineRect lineGlyphRange containerOrigin =
  sendMessage nsLayoutManager drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector glyphRange strikethroughVal baselineOffset lineRect lineGlyphRange containerOrigin

-- | @- strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> NSUnderlineStyle -> NSRect -> NSRange -> NSPoint -> IO ()
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin nsLayoutManager glyphRange strikethroughVal lineRect lineGlyphRange containerOrigin =
  sendMessage nsLayoutManager strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector glyphRange strikethroughVal lineRect lineGlyphRange containerOrigin

-- | @- showAttachmentCell:inRect:characterIndex:@
showAttachmentCell_inRect_characterIndex :: (IsNSLayoutManager nsLayoutManager, IsNSCell cell) => nsLayoutManager -> cell -> NSRect -> CULong -> IO ()
showAttachmentCell_inRect_characterIndex nsLayoutManager cell rect attachmentIndex =
  sendMessage nsLayoutManager showAttachmentCell_inRect_characterIndexSelector (toNSCell cell) rect attachmentIndex

-- | ************************ Block information *************************
--
-- ObjC selector: @- setLayoutRect:forTextBlock:glyphRange:@
setLayoutRect_forTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> NSRect -> block -> NSRange -> IO ()
setLayoutRect_forTextBlock_glyphRange nsLayoutManager rect block glyphRange =
  sendMessage nsLayoutManager setLayoutRect_forTextBlock_glyphRangeSelector rect (toNSTextBlock block) glyphRange

-- | @- setBoundsRect:forTextBlock:glyphRange:@
setBoundsRect_forTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> NSRect -> block -> NSRange -> IO ()
setBoundsRect_forTextBlock_glyphRange nsLayoutManager rect block glyphRange =
  sendMessage nsLayoutManager setBoundsRect_forTextBlock_glyphRangeSelector rect (toNSTextBlock block) glyphRange

-- | @- layoutRectForTextBlock:glyphRange:@
layoutRectForTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> NSRange -> IO NSRect
layoutRectForTextBlock_glyphRange nsLayoutManager block glyphRange =
  sendMessage nsLayoutManager layoutRectForTextBlock_glyphRangeSelector (toNSTextBlock block) glyphRange

-- | @- boundsRectForTextBlock:glyphRange:@
boundsRectForTextBlock_glyphRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> NSRange -> IO NSRect
boundsRectForTextBlock_glyphRange nsLayoutManager block glyphRange =
  sendMessage nsLayoutManager boundsRectForTextBlock_glyphRangeSelector (toNSTextBlock block) glyphRange

-- | @- layoutRectForTextBlock:atIndex:effectiveRange:@
layoutRectForTextBlock_atIndex_effectiveRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> CULong -> Ptr NSRange -> IO NSRect
layoutRectForTextBlock_atIndex_effectiveRange nsLayoutManager block glyphIndex effectiveGlyphRange =
  sendMessage nsLayoutManager layoutRectForTextBlock_atIndex_effectiveRangeSelector (toNSTextBlock block) glyphIndex effectiveGlyphRange

-- | @- boundsRectForTextBlock:atIndex:effectiveRange:@
boundsRectForTextBlock_atIndex_effectiveRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextBlock block) => nsLayoutManager -> block -> CULong -> Ptr NSRange -> IO NSRect
boundsRectForTextBlock_atIndex_effectiveRange nsLayoutManager block glyphIndex effectiveGlyphRange =
  sendMessage nsLayoutManager boundsRectForTextBlock_atIndex_effectiveRangeSelector (toNSTextBlock block) glyphIndex effectiveGlyphRange

-- | ********************** Temporary attribute support ***********************
--
-- ObjC selector: @- temporaryAttributesAtCharacterIndex:effectiveRange:@
temporaryAttributesAtCharacterIndex_effectiveRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> IO (Id NSDictionary)
temporaryAttributesAtCharacterIndex_effectiveRange nsLayoutManager charIndex effectiveCharRange =
  sendMessage nsLayoutManager temporaryAttributesAtCharacterIndex_effectiveRangeSelector charIndex effectiveCharRange

-- | @- setTemporaryAttributes:forCharacterRange:@
setTemporaryAttributes_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSDictionary attrs) => nsLayoutManager -> attrs -> NSRange -> IO ()
setTemporaryAttributes_forCharacterRange nsLayoutManager attrs charRange =
  sendMessage nsLayoutManager setTemporaryAttributes_forCharacterRangeSelector (toNSDictionary attrs) charRange

-- | @- addTemporaryAttributes:forCharacterRange:@
addTemporaryAttributes_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSDictionary attrs) => nsLayoutManager -> attrs -> NSRange -> IO ()
addTemporaryAttributes_forCharacterRange nsLayoutManager attrs charRange =
  sendMessage nsLayoutManager addTemporaryAttributes_forCharacterRangeSelector (toNSDictionary attrs) charRange

-- | @- removeTemporaryAttribute:forCharacterRange:@
removeTemporaryAttribute_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> NSRange -> IO ()
removeTemporaryAttribute_forCharacterRange nsLayoutManager attrName charRange =
  sendMessage nsLayoutManager removeTemporaryAttribute_forCharacterRangeSelector (toNSString attrName) charRange

-- | @- temporaryAttribute:atCharacterIndex:effectiveRange:@
temporaryAttribute_atCharacterIndex_effectiveRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> CULong -> Ptr NSRange -> IO RawId
temporaryAttribute_atCharacterIndex_effectiveRange nsLayoutManager attrName location range =
  sendMessage nsLayoutManager temporaryAttribute_atCharacterIndex_effectiveRangeSelector (toNSString attrName) location range

-- | @- temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> CULong -> Ptr NSRange -> NSRange -> IO RawId
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange nsLayoutManager attrName location range rangeLimit =
  sendMessage nsLayoutManager temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector (toNSString attrName) location range rangeLimit

-- | @- temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr NSRange -> NSRange -> IO (Id NSDictionary)
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange nsLayoutManager location range rangeLimit =
  sendMessage nsLayoutManager temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector location range rangeLimit

-- | @- addTemporaryAttribute:value:forCharacterRange:@
addTemporaryAttribute_value_forCharacterRange :: (IsNSLayoutManager nsLayoutManager, IsNSString attrName) => nsLayoutManager -> attrName -> RawId -> NSRange -> IO ()
addTemporaryAttribute_value_forCharacterRange nsLayoutManager attrName value charRange =
  sendMessage nsLayoutManager addTemporaryAttribute_value_forCharacterRangeSelector (toNSString attrName) value charRange

-- | ***************************** Font metrics *****************************
--
-- ObjC selector: @- defaultLineHeightForFont:@
defaultLineHeightForFont :: (IsNSLayoutManager nsLayoutManager, IsNSFont theFont) => nsLayoutManager -> theFont -> IO CDouble
defaultLineHeightForFont nsLayoutManager theFont =
  sendMessage nsLayoutManager defaultLineHeightForFontSelector (toNSFont theFont)

-- | @- defaultBaselineOffsetForFont:@
defaultBaselineOffsetForFont :: (IsNSLayoutManager nsLayoutManager, IsNSFont theFont) => nsLayoutManager -> theFont -> IO CDouble
defaultBaselineOffsetForFont nsLayoutManager theFont =
  sendMessage nsLayoutManager defaultBaselineOffsetForFontSelector (toNSFont theFont)

-- | @- glyphAtIndex:isValidIndex:@
glyphAtIndex_isValidIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> Ptr Bool -> IO CUInt
glyphAtIndex_isValidIndex nsLayoutManager glyphIndex isValidIndex =
  sendMessage nsLayoutManager glyphAtIndex_isValidIndexSelector glyphIndex isValidIndex

-- | @- glyphAtIndex:@
glyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> IO CUInt
glyphAtIndex nsLayoutManager glyphIndex =
  sendMessage nsLayoutManager glyphAtIndexSelector glyphIndex

-- | @- rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:@
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRange -> NSRange -> container -> Ptr CULong -> IO (Ptr NSRect)
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount nsLayoutManager charRange selCharRange container rectCount =
  sendMessage nsLayoutManager rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector charRange selCharRange (toNSTextContainer container) rectCount

-- | @- rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:@
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount :: (IsNSLayoutManager nsLayoutManager, IsNSTextContainer container) => nsLayoutManager -> NSRange -> NSRange -> container -> Ptr CULong -> IO (Ptr NSRect)
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount nsLayoutManager glyphRange selGlyphRange container rectCount =
  sendMessage nsLayoutManager rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector glyphRange selGlyphRange (toNSTextContainer container) rectCount

-- | @- substituteFontForFont:@
substituteFontForFont :: (IsNSLayoutManager nsLayoutManager, IsNSFont originalFont) => nsLayoutManager -> originalFont -> IO (Id NSFont)
substituteFontForFont nsLayoutManager originalFont =
  sendMessage nsLayoutManager substituteFontForFontSelector (toNSFont originalFont)

-- | @- insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:@
insertGlyphs_length_forStartingGlyphAtIndex_characterIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Const RawId -> CULong -> CULong -> CULong -> IO ()
insertGlyphs_length_forStartingGlyphAtIndex_characterIndex nsLayoutManager glyphs length_ glyphIndex charIndex =
  sendMessage nsLayoutManager insertGlyphs_length_forStartingGlyphAtIndex_characterIndexSelector glyphs length_ glyphIndex charIndex

-- | @- insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CUInt -> CULong -> CULong -> IO ()
insertGlyph_atGlyphIndex_characterIndex nsLayoutManager glyph glyphIndex charIndex =
  sendMessage nsLayoutManager insertGlyph_atGlyphIndex_characterIndexSelector glyph glyphIndex charIndex

-- | @- replaceGlyphAtIndex:withGlyph:@
replaceGlyphAtIndex_withGlyph :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> CUInt -> IO ()
replaceGlyphAtIndex_withGlyph nsLayoutManager glyphIndex newGlyph =
  sendMessage nsLayoutManager replaceGlyphAtIndex_withGlyphSelector glyphIndex newGlyph

-- | @- deleteGlyphsInRange:@
deleteGlyphsInRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
deleteGlyphsInRange nsLayoutManager glyphRange =
  sendMessage nsLayoutManager deleteGlyphsInRangeSelector glyphRange

-- | @- setCharacterIndex:forGlyphAtIndex:@
setCharacterIndex_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CULong -> CULong -> IO ()
setCharacterIndex_forGlyphAtIndex nsLayoutManager charIndex glyphIndex =
  sendMessage nsLayoutManager setCharacterIndex_forGlyphAtIndexSelector charIndex glyphIndex

-- | @- setIntAttribute:value:forGlyphAtIndex:@
setIntAttribute_value_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CLong -> CLong -> CULong -> IO ()
setIntAttribute_value_forGlyphAtIndex nsLayoutManager attributeTag val glyphIndex =
  sendMessage nsLayoutManager setIntAttribute_value_forGlyphAtIndexSelector attributeTag val glyphIndex

-- | @- invalidateGlyphsOnLayoutInvalidationForGlyphRange:@
invalidateGlyphsOnLayoutInvalidationForGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> IO ()
invalidateGlyphsOnLayoutInvalidationForGlyphRange nsLayoutManager glyphRange =
  sendMessage nsLayoutManager invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector glyphRange

-- | @- intAttribute:forGlyphAtIndex:@
intAttribute_forGlyphAtIndex :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CLong -> CULong -> IO CLong
intAttribute_forGlyphAtIndex nsLayoutManager attributeTag glyphIndex =
  sendMessage nsLayoutManager intAttribute_forGlyphAtIndexSelector attributeTag glyphIndex

-- | @- getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> RawId -> Ptr CULong -> Ptr NSGlyphInscription -> Ptr Bool -> IO CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits nsLayoutManager glyphRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer =
  sendMessage nsLayoutManager getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector glyphRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer

-- | @- getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> RawId -> Ptr CULong -> Ptr NSGlyphInscription -> Ptr Bool -> Ptr CUChar -> IO CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels nsLayoutManager glyphRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer bidiLevelBuffer =
  sendMessage nsLayoutManager getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector glyphRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer bidiLevelBuffer

-- | @- getGlyphs:range:@
getGlyphs_range :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> RawId -> NSRange -> IO CULong
getGlyphs_range nsLayoutManager glyphArray glyphRange =
  sendMessage nsLayoutManager getGlyphs_rangeSelector glyphArray glyphRange

-- | @- invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:@
invalidateLayoutForCharacterRange_isSoft_actualCharacterRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSRange -> Bool -> Ptr NSRange -> IO ()
invalidateLayoutForCharacterRange_isSoft_actualCharacterRange nsLayoutManager charRange flag actualCharRange =
  sendMessage nsLayoutManager invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector charRange flag actualCharRange

-- | @- textStorage:edited:range:changeInLength:invalidatedRange:@
textStorage_edited_range_changeInLength_invalidatedRange :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage str) => nsLayoutManager -> str -> CULong -> NSRange -> CLong -> NSRange -> IO ()
textStorage_edited_range_changeInLength_invalidatedRange nsLayoutManager str editedMask newCharRange delta invalidatedCharRange =
  sendMessage nsLayoutManager textStorage_edited_range_changeInLength_invalidatedRangeSelector (toNSTextStorage str) editedMask newCharRange delta invalidatedCharRange

-- | @- setLocations:startingGlyphIndexes:count:forGlyphRange:@
setLocations_startingGlyphIndexes_count_forGlyphRange :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Ptr NSPoint -> Ptr CULong -> CULong -> NSRange -> IO ()
setLocations_startingGlyphIndexes_count_forGlyphRange nsLayoutManager locations glyphIndexes count glyphRange =
  sendMessage nsLayoutManager setLocations_startingGlyphIndexes_count_forGlyphRangeSelector locations glyphIndexes count glyphRange

-- | @- showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:@
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment :: (IsNSLayoutManager nsLayoutManager, IsNSFont font, IsNSColor color) => nsLayoutManager -> Ptr CChar -> CULong -> NSRange -> NSPoint -> font -> color -> NSSize -> IO ()
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment nsLayoutManager glyphs glyphLen glyphRange point font color printingAdjustment =
  sendMessage nsLayoutManager showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector glyphs glyphLen glyphRange point (toNSFont font) (toNSColor color) printingAdjustment

-- | @- showCGGlyphs:positions:count:font:matrix:attributes:inContext:@
showCGGlyphs_positions_count_font_matrix_attributes_inContext :: (IsNSLayoutManager nsLayoutManager, IsNSFont font, IsNSAffineTransform textMatrix, IsNSDictionary attributes, IsNSGraphicsContext graphicsContext) => nsLayoutManager -> Const RawId -> Const (Ptr NSPoint) -> CULong -> font -> textMatrix -> attributes -> graphicsContext -> IO ()
showCGGlyphs_positions_count_font_matrix_attributes_inContext nsLayoutManager glyphs positions glyphCount font textMatrix attributes graphicsContext =
  sendMessage nsLayoutManager showCGGlyphs_positions_count_font_matrix_attributes_inContextSelector glyphs positions glyphCount (toNSFont font) (toNSAffineTransform textMatrix) (toNSDictionary attributes) (toNSGraphicsContext graphicsContext)

-- | *************************** Ruler support ****************************
--
-- ObjC selector: @- rulerMarkersForTextView:paragraphStyle:ruler:@
rulerMarkersForTextView_paragraphStyle_ruler :: (IsNSLayoutManager nsLayoutManager, IsNSTextView view, IsNSParagraphStyle style, IsNSRulerView ruler) => nsLayoutManager -> view -> style -> ruler -> IO (Id NSArray)
rulerMarkersForTextView_paragraphStyle_ruler nsLayoutManager view style ruler =
  sendMessage nsLayoutManager rulerMarkersForTextView_paragraphStyle_rulerSelector (toNSTextView view) (toNSParagraphStyle style) (toNSRulerView ruler)

-- | @- rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:@
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled :: (IsNSLayoutManager nsLayoutManager, IsNSTextView view, IsNSParagraphStyle style, IsNSRulerView ruler) => nsLayoutManager -> view -> style -> ruler -> Bool -> IO (Id NSView)
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled nsLayoutManager view style ruler isEnabled =
  sendMessage nsLayoutManager rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector (toNSTextView view) (toNSParagraphStyle style) (toNSRulerView ruler) isEnabled

-- | ********************** First responder support ***********************
--
-- ObjC selector: @- layoutManagerOwnsFirstResponderInWindow:@
layoutManagerOwnsFirstResponderInWindow :: (IsNSLayoutManager nsLayoutManager, IsNSWindow window) => nsLayoutManager -> window -> IO Bool
layoutManagerOwnsFirstResponderInWindow nsLayoutManager window =
  sendMessage nsLayoutManager layoutManagerOwnsFirstResponderInWindowSelector (toNSWindow window)

-- | ************************* Text storage **************************
--
-- ObjC selector: @- textStorage@
textStorage :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextStorage)
textStorage nsLayoutManager =
  sendMessage nsLayoutManager textStorageSelector

-- | ************************* Text storage **************************
--
-- ObjC selector: @- setTextStorage:@
setTextStorage :: (IsNSLayoutManager nsLayoutManager, IsNSTextStorage value) => nsLayoutManager -> value -> IO ()
setTextStorage nsLayoutManager value =
  sendMessage nsLayoutManager setTextStorageSelector (toNSTextStorage value)

-- | ************************** Text containers ***************************
--
-- ObjC selector: @- textContainers@
textContainers :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSArray)
textContainers nsLayoutManager =
  sendMessage nsLayoutManager textContainersSelector

-- | ************************** Delegate ***************************
--
-- ObjC selector: @- delegate@
delegate :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO RawId
delegate nsLayoutManager =
  sendMessage nsLayoutManager delegateSelector

-- | ************************** Delegate ***************************
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> RawId -> IO ()
setDelegate nsLayoutManager value =
  sendMessage nsLayoutManager setDelegateSelector value

-- | ********************* Global layout manager options **********************
--
-- ObjC selector: @- showsInvisibleCharacters@
showsInvisibleCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
showsInvisibleCharacters nsLayoutManager =
  sendMessage nsLayoutManager showsInvisibleCharactersSelector

-- | ********************* Global layout manager options **********************
--
-- ObjC selector: @- setShowsInvisibleCharacters:@
setShowsInvisibleCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setShowsInvisibleCharacters nsLayoutManager value =
  sendMessage nsLayoutManager setShowsInvisibleCharactersSelector value

-- | @- showsControlCharacters@
showsControlCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
showsControlCharacters nsLayoutManager =
  sendMessage nsLayoutManager showsControlCharactersSelector

-- | @- setShowsControlCharacters:@
setShowsControlCharacters :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setShowsControlCharacters nsLayoutManager value =
  sendMessage nsLayoutManager setShowsControlCharactersSelector value

-- | @- usesDefaultHyphenation@
usesDefaultHyphenation :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
usesDefaultHyphenation nsLayoutManager =
  sendMessage nsLayoutManager usesDefaultHyphenationSelector

-- | @- setUsesDefaultHyphenation:@
setUsesDefaultHyphenation :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setUsesDefaultHyphenation nsLayoutManager value =
  sendMessage nsLayoutManager setUsesDefaultHyphenationSelector value

-- | @- usesFontLeading@
usesFontLeading :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
usesFontLeading nsLayoutManager =
  sendMessage nsLayoutManager usesFontLeadingSelector

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setUsesFontLeading nsLayoutManager value =
  sendMessage nsLayoutManager setUsesFontLeadingSelector value

-- | @- allowsNonContiguousLayout@
allowsNonContiguousLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
allowsNonContiguousLayout nsLayoutManager =
  sendMessage nsLayoutManager allowsNonContiguousLayoutSelector

-- | @- setAllowsNonContiguousLayout:@
setAllowsNonContiguousLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setAllowsNonContiguousLayout nsLayoutManager value =
  sendMessage nsLayoutManager setAllowsNonContiguousLayoutSelector value

-- | @- hasNonContiguousLayout@
hasNonContiguousLayout :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
hasNonContiguousLayout nsLayoutManager =
  sendMessage nsLayoutManager hasNonContiguousLayoutSelector

-- | @- limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContents :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
limitsLayoutForSuspiciousContents nsLayoutManager =
  sendMessage nsLayoutManager limitsLayoutForSuspiciousContentsSelector

-- | @- setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContents :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setLimitsLayoutForSuspiciousContents nsLayoutManager value =
  sendMessage nsLayoutManager setLimitsLayoutForSuspiciousContentsSelector value

-- | @- backgroundLayoutEnabled@
backgroundLayoutEnabled :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
backgroundLayoutEnabled nsLayoutManager =
  sendMessage nsLayoutManager backgroundLayoutEnabledSelector

-- | @- setBackgroundLayoutEnabled:@
setBackgroundLayoutEnabled :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setBackgroundLayoutEnabled nsLayoutManager value =
  sendMessage nsLayoutManager setBackgroundLayoutEnabledSelector value

-- | @- defaultAttachmentScaling@
defaultAttachmentScaling :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSImageScaling
defaultAttachmentScaling nsLayoutManager =
  sendMessage nsLayoutManager defaultAttachmentScalingSelector

-- | @- setDefaultAttachmentScaling:@
setDefaultAttachmentScaling :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSImageScaling -> IO ()
setDefaultAttachmentScaling nsLayoutManager value =
  sendMessage nsLayoutManager setDefaultAttachmentScalingSelector value

-- | ********************* Typesetter **********************
--
-- ObjC selector: @- typesetter@
typesetter :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTypesetter)
typesetter nsLayoutManager =
  sendMessage nsLayoutManager typesetterSelector

-- | ********************* Typesetter **********************
--
-- ObjC selector: @- setTypesetter:@
setTypesetter :: (IsNSLayoutManager nsLayoutManager, IsNSTypesetter value) => nsLayoutManager -> value -> IO ()
setTypesetter nsLayoutManager value =
  sendMessage nsLayoutManager setTypesetterSelector (toNSTypesetter value)

-- | @- typesetterBehavior@
typesetterBehavior :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSTypesetterBehavior
typesetterBehavior nsLayoutManager =
  sendMessage nsLayoutManager typesetterBehaviorSelector

-- | @- setTypesetterBehavior:@
setTypesetterBehavior :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> NSTypesetterBehavior -> IO ()
setTypesetterBehavior nsLayoutManager value =
  sendMessage nsLayoutManager setTypesetterBehaviorSelector value

-- | ********************** Get glyphs and glyph properties ***********************
--
-- ObjC selector: @- numberOfGlyphs@
numberOfGlyphs :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CULong
numberOfGlyphs nsLayoutManager =
  sendMessage nsLayoutManager numberOfGlyphsSelector

-- | @- extraLineFragmentRect@
extraLineFragmentRect :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSRect
extraLineFragmentRect nsLayoutManager =
  sendMessage nsLayoutManager extraLineFragmentRectSelector

-- | @- extraLineFragmentUsedRect@
extraLineFragmentUsedRect :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO NSRect
extraLineFragmentUsedRect nsLayoutManager =
  sendMessage nsLayoutManager extraLineFragmentUsedRectSelector

-- | @- extraLineFragmentTextContainer@
extraLineFragmentTextContainer :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextContainer)
extraLineFragmentTextContainer nsLayoutManager =
  sendMessage nsLayoutManager extraLineFragmentTextContainerSelector

-- | @- glyphGenerator@
glyphGenerator :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSGlyphGenerator)
glyphGenerator nsLayoutManager =
  sendMessage nsLayoutManager glyphGeneratorSelector

-- | @- setGlyphGenerator:@
setGlyphGenerator :: (IsNSLayoutManager nsLayoutManager, IsNSGlyphGenerator value) => nsLayoutManager -> value -> IO ()
setGlyphGenerator nsLayoutManager value =
  sendMessage nsLayoutManager setGlyphGeneratorSelector (toNSGlyphGenerator value)

-- | @- usesScreenFonts@
usesScreenFonts :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO Bool
usesScreenFonts nsLayoutManager =
  sendMessage nsLayoutManager usesScreenFontsSelector

-- | @- setUsesScreenFonts:@
setUsesScreenFonts :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> Bool -> IO ()
setUsesScreenFonts nsLayoutManager value =
  sendMessage nsLayoutManager setUsesScreenFontsSelector value

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO CFloat
hyphenationFactor nsLayoutManager =
  sendMessage nsLayoutManager hyphenationFactorSelector

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> CFloat -> IO ()
setHyphenationFactor nsLayoutManager value =
  sendMessage nsLayoutManager setHyphenationFactorSelector value

-- | @- firstTextView@
firstTextView :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextView)
firstTextView nsLayoutManager =
  sendMessage nsLayoutManager firstTextViewSelector

-- | @- textViewForBeginningOfSelection@
textViewForBeginningOfSelection :: IsNSLayoutManager nsLayoutManager => nsLayoutManager -> IO (Id NSTextView)
textViewForBeginningOfSelection nsLayoutManager =
  sendMessage nsLayoutManager textViewForBeginningOfSelectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSLayoutManager)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSLayoutManager)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @replaceTextStorage:@
replaceTextStorageSelector :: Selector '[Id NSTextStorage] ()
replaceTextStorageSelector = mkSelector "replaceTextStorage:"

-- | @Selector@ for @addTextContainer:@
addTextContainerSelector :: Selector '[Id NSTextContainer] ()
addTextContainerSelector = mkSelector "addTextContainer:"

-- | @Selector@ for @insertTextContainer:atIndex:@
insertTextContainer_atIndexSelector :: Selector '[Id NSTextContainer, CULong] ()
insertTextContainer_atIndexSelector = mkSelector "insertTextContainer:atIndex:"

-- | @Selector@ for @removeTextContainerAtIndex:@
removeTextContainerAtIndexSelector :: Selector '[CULong] ()
removeTextContainerAtIndexSelector = mkSelector "removeTextContainerAtIndex:"

-- | @Selector@ for @textContainerChangedGeometry:@
textContainerChangedGeometrySelector :: Selector '[Id NSTextContainer] ()
textContainerChangedGeometrySelector = mkSelector "textContainerChangedGeometry:"

-- | @Selector@ for @textContainerChangedTextView:@
textContainerChangedTextViewSelector :: Selector '[Id NSTextContainer] ()
textContainerChangedTextViewSelector = mkSelector "textContainerChangedTextView:"

-- | @Selector@ for @invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:@
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector :: Selector '[NSRange, CLong, Ptr NSRange] ()
invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRangeSelector = mkSelector "invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:"

-- | @Selector@ for @invalidateLayoutForCharacterRange:actualCharacterRange:@
invalidateLayoutForCharacterRange_actualCharacterRangeSelector :: Selector '[NSRange, Ptr NSRange] ()
invalidateLayoutForCharacterRange_actualCharacterRangeSelector = mkSelector "invalidateLayoutForCharacterRange:actualCharacterRange:"

-- | @Selector@ for @invalidateDisplayForCharacterRange:@
invalidateDisplayForCharacterRangeSelector :: Selector '[NSRange] ()
invalidateDisplayForCharacterRangeSelector = mkSelector "invalidateDisplayForCharacterRange:"

-- | @Selector@ for @invalidateDisplayForGlyphRange:@
invalidateDisplayForGlyphRangeSelector :: Selector '[NSRange] ()
invalidateDisplayForGlyphRangeSelector = mkSelector "invalidateDisplayForGlyphRange:"

-- | @Selector@ for @processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:@
processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector :: Selector '[Id NSTextStorage, NSTextStorageEditActions, NSRange, CLong, NSRange] ()
processEditingForTextStorage_edited_range_changeInLength_invalidatedRangeSelector = mkSelector "processEditingForTextStorage:edited:range:changeInLength:invalidatedRange:"

-- | @Selector@ for @ensureGlyphsForCharacterRange:@
ensureGlyphsForCharacterRangeSelector :: Selector '[NSRange] ()
ensureGlyphsForCharacterRangeSelector = mkSelector "ensureGlyphsForCharacterRange:"

-- | @Selector@ for @ensureGlyphsForGlyphRange:@
ensureGlyphsForGlyphRangeSelector :: Selector '[NSRange] ()
ensureGlyphsForGlyphRangeSelector = mkSelector "ensureGlyphsForGlyphRange:"

-- | @Selector@ for @ensureLayoutForCharacterRange:@
ensureLayoutForCharacterRangeSelector :: Selector '[NSRange] ()
ensureLayoutForCharacterRangeSelector = mkSelector "ensureLayoutForCharacterRange:"

-- | @Selector@ for @ensureLayoutForGlyphRange:@
ensureLayoutForGlyphRangeSelector :: Selector '[NSRange] ()
ensureLayoutForGlyphRangeSelector = mkSelector "ensureLayoutForGlyphRange:"

-- | @Selector@ for @ensureLayoutForTextContainer:@
ensureLayoutForTextContainerSelector :: Selector '[Id NSTextContainer] ()
ensureLayoutForTextContainerSelector = mkSelector "ensureLayoutForTextContainer:"

-- | @Selector@ for @ensureLayoutForBoundingRect:inTextContainer:@
ensureLayoutForBoundingRect_inTextContainerSelector :: Selector '[NSRect, Id NSTextContainer] ()
ensureLayoutForBoundingRect_inTextContainerSelector = mkSelector "ensureLayoutForBoundingRect:inTextContainer:"

-- | @Selector@ for @setGlyphs:properties:characterIndexes:font:forGlyphRange:@
setGlyphs_properties_characterIndexes_font_forGlyphRangeSelector :: Selector '[Const RawId, Const (Ptr NSGlyphProperty), Const (Ptr CULong), Id NSFont, NSRange] ()
setGlyphs_properties_characterIndexes_font_forGlyphRangeSelector = mkSelector "setGlyphs:properties:characterIndexes:font:forGlyphRange:"

-- | @Selector@ for @CGGlyphAtIndex:isValidIndex:@
cgGlyphAtIndex_isValidIndexSelector :: Selector '[CULong, Ptr Bool] CUShort
cgGlyphAtIndex_isValidIndexSelector = mkSelector "CGGlyphAtIndex:isValidIndex:"

-- | @Selector@ for @CGGlyphAtIndex:@
cgGlyphAtIndexSelector :: Selector '[CULong] CUShort
cgGlyphAtIndexSelector = mkSelector "CGGlyphAtIndex:"

-- | @Selector@ for @isValidGlyphIndex:@
isValidGlyphIndexSelector :: Selector '[CULong] Bool
isValidGlyphIndexSelector = mkSelector "isValidGlyphIndex:"

-- | @Selector@ for @propertyForGlyphAtIndex:@
propertyForGlyphAtIndexSelector :: Selector '[CULong] NSGlyphProperty
propertyForGlyphAtIndexSelector = mkSelector "propertyForGlyphAtIndex:"

-- | @Selector@ for @characterIndexForGlyphAtIndex:@
characterIndexForGlyphAtIndexSelector :: Selector '[CULong] CULong
characterIndexForGlyphAtIndexSelector = mkSelector "characterIndexForGlyphAtIndex:"

-- | @Selector@ for @glyphIndexForCharacterAtIndex:@
glyphIndexForCharacterAtIndexSelector :: Selector '[CULong] CULong
glyphIndexForCharacterAtIndexSelector = mkSelector "glyphIndexForCharacterAtIndex:"

-- | @Selector@ for @getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels:@
getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevelsSelector :: Selector '[NSRange, RawId, Ptr NSGlyphProperty, Ptr CULong, Ptr CUChar] CULong
getGlyphsInRange_glyphs_properties_characterIndexes_bidiLevelsSelector = mkSelector "getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels:"

-- | @Selector@ for @setTextContainer:forGlyphRange:@
setTextContainer_forGlyphRangeSelector :: Selector '[Id NSTextContainer, NSRange] ()
setTextContainer_forGlyphRangeSelector = mkSelector "setTextContainer:forGlyphRange:"

-- | @Selector@ for @setLineFragmentRect:forGlyphRange:usedRect:@
setLineFragmentRect_forGlyphRange_usedRectSelector :: Selector '[NSRect, NSRange, NSRect] ()
setLineFragmentRect_forGlyphRange_usedRectSelector = mkSelector "setLineFragmentRect:forGlyphRange:usedRect:"

-- | @Selector@ for @setExtraLineFragmentRect:usedRect:textContainer:@
setExtraLineFragmentRect_usedRect_textContainerSelector :: Selector '[NSRect, NSRect, Id NSTextContainer] ()
setExtraLineFragmentRect_usedRect_textContainerSelector = mkSelector "setExtraLineFragmentRect:usedRect:textContainer:"

-- | @Selector@ for @setLocation:forStartOfGlyphRange:@
setLocation_forStartOfGlyphRangeSelector :: Selector '[NSPoint, NSRange] ()
setLocation_forStartOfGlyphRangeSelector = mkSelector "setLocation:forStartOfGlyphRange:"

-- | @Selector@ for @setNotShownAttribute:forGlyphAtIndex:@
setNotShownAttribute_forGlyphAtIndexSelector :: Selector '[Bool, CULong] ()
setNotShownAttribute_forGlyphAtIndexSelector = mkSelector "setNotShownAttribute:forGlyphAtIndex:"

-- | @Selector@ for @setDrawsOutsideLineFragment:forGlyphAtIndex:@
setDrawsOutsideLineFragment_forGlyphAtIndexSelector :: Selector '[Bool, CULong] ()
setDrawsOutsideLineFragment_forGlyphAtIndexSelector = mkSelector "setDrawsOutsideLineFragment:forGlyphAtIndex:"

-- | @Selector@ for @setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRangeSelector :: Selector '[NSSize, NSRange] ()
setAttachmentSize_forGlyphRangeSelector = mkSelector "setAttachmentSize:forGlyphRange:"

-- | @Selector@ for @getFirstUnlaidCharacterIndex:glyphIndex:@
getFirstUnlaidCharacterIndex_glyphIndexSelector :: Selector '[Ptr CULong, Ptr CULong] ()
getFirstUnlaidCharacterIndex_glyphIndexSelector = mkSelector "getFirstUnlaidCharacterIndex:glyphIndex:"

-- | @Selector@ for @firstUnlaidCharacterIndex@
firstUnlaidCharacterIndexSelector :: Selector '[] CULong
firstUnlaidCharacterIndexSelector = mkSelector "firstUnlaidCharacterIndex"

-- | @Selector@ for @firstUnlaidGlyphIndex@
firstUnlaidGlyphIndexSelector :: Selector '[] CULong
firstUnlaidGlyphIndexSelector = mkSelector "firstUnlaidGlyphIndex"

-- | @Selector@ for @textContainerForGlyphAtIndex:effectiveRange:@
textContainerForGlyphAtIndex_effectiveRangeSelector :: Selector '[CULong, Ptr NSRange] (Id NSTextContainer)
textContainerForGlyphAtIndex_effectiveRangeSelector = mkSelector "textContainerForGlyphAtIndex:effectiveRange:"

-- | @Selector@ for @textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector :: Selector '[CULong, Ptr NSRange, Bool] (Id NSTextContainer)
textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector = mkSelector "textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:"

-- | @Selector@ for @usedRectForTextContainer:@
usedRectForTextContainerSelector :: Selector '[Id NSTextContainer] NSRect
usedRectForTextContainerSelector = mkSelector "usedRectForTextContainer:"

-- | @Selector@ for @lineFragmentRectForGlyphAtIndex:effectiveRange:@
lineFragmentRectForGlyphAtIndex_effectiveRangeSelector :: Selector '[CULong, Ptr NSRange] NSRect
lineFragmentRectForGlyphAtIndex_effectiveRangeSelector = mkSelector "lineFragmentRectForGlyphAtIndex:effectiveRange:"

-- | @Selector@ for @lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector :: Selector '[CULong, Ptr NSRange, Bool] NSRect
lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector = mkSelector "lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:"

-- | @Selector@ for @lineFragmentUsedRectForGlyphAtIndex:effectiveRange:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector :: Selector '[CULong, Ptr NSRange] NSRect
lineFragmentUsedRectForGlyphAtIndex_effectiveRangeSelector = mkSelector "lineFragmentUsedRectForGlyphAtIndex:effectiveRange:"

-- | @Selector@ for @lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:@
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector :: Selector '[CULong, Ptr NSRange, Bool] NSRect
lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayoutSelector = mkSelector "lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:"

-- | @Selector@ for @locationForGlyphAtIndex:@
locationForGlyphAtIndexSelector :: Selector '[CULong] NSPoint
locationForGlyphAtIndexSelector = mkSelector "locationForGlyphAtIndex:"

-- | @Selector@ for @notShownAttributeForGlyphAtIndex:@
notShownAttributeForGlyphAtIndexSelector :: Selector '[CULong] Bool
notShownAttributeForGlyphAtIndexSelector = mkSelector "notShownAttributeForGlyphAtIndex:"

-- | @Selector@ for @drawsOutsideLineFragmentForGlyphAtIndex:@
drawsOutsideLineFragmentForGlyphAtIndexSelector :: Selector '[CULong] Bool
drawsOutsideLineFragmentForGlyphAtIndexSelector = mkSelector "drawsOutsideLineFragmentForGlyphAtIndex:"

-- | @Selector@ for @attachmentSizeForGlyphAtIndex:@
attachmentSizeForGlyphAtIndexSelector :: Selector '[CULong] NSSize
attachmentSizeForGlyphAtIndexSelector = mkSelector "attachmentSizeForGlyphAtIndex:"

-- | @Selector@ for @truncatedGlyphRangeInLineFragmentForGlyphAtIndex:@
truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector :: Selector '[CULong] NSRange
truncatedGlyphRangeInLineFragmentForGlyphAtIndexSelector = mkSelector "truncatedGlyphRangeInLineFragmentForGlyphAtIndex:"

-- | @Selector@ for @glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRangeSelector :: Selector '[NSRange, Ptr NSRange] NSRange
glyphRangeForCharacterRange_actualCharacterRangeSelector = mkSelector "glyphRangeForCharacterRange:actualCharacterRange:"

-- | @Selector@ for @characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRangeSelector :: Selector '[NSRange, Ptr NSRange] NSRange
characterRangeForGlyphRange_actualGlyphRangeSelector = mkSelector "characterRangeForGlyphRange:actualGlyphRange:"

-- | @Selector@ for @glyphRangeForTextContainer:@
glyphRangeForTextContainerSelector :: Selector '[Id NSTextContainer] NSRange
glyphRangeForTextContainerSelector = mkSelector "glyphRangeForTextContainer:"

-- | @Selector@ for @rangeOfNominallySpacedGlyphsContainingIndex:@
rangeOfNominallySpacedGlyphsContainingIndexSelector :: Selector '[CULong] NSRange
rangeOfNominallySpacedGlyphsContainingIndexSelector = mkSelector "rangeOfNominallySpacedGlyphsContainingIndex:"

-- | @Selector@ for @boundingRectForGlyphRange:inTextContainer:@
boundingRectForGlyphRange_inTextContainerSelector :: Selector '[NSRange, Id NSTextContainer] NSRect
boundingRectForGlyphRange_inTextContainerSelector = mkSelector "boundingRectForGlyphRange:inTextContainer:"

-- | @Selector@ for @glyphRangeForBoundingRect:inTextContainer:@
glyphRangeForBoundingRect_inTextContainerSelector :: Selector '[NSRect, Id NSTextContainer] NSRange
glyphRangeForBoundingRect_inTextContainerSelector = mkSelector "glyphRangeForBoundingRect:inTextContainer:"

-- | @Selector@ for @glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:@
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector :: Selector '[NSRect, Id NSTextContainer] NSRange
glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainerSelector = mkSelector "glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:"

-- | @Selector@ for @glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:@
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector :: Selector '[NSPoint, Id NSTextContainer, Ptr CDouble] CULong
glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyphSelector = mkSelector "glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:"

-- | @Selector@ for @glyphIndexForPoint:inTextContainer:@
glyphIndexForPoint_inTextContainerSelector :: Selector '[NSPoint, Id NSTextContainer] CULong
glyphIndexForPoint_inTextContainerSelector = mkSelector "glyphIndexForPoint:inTextContainer:"

-- | @Selector@ for @fractionOfDistanceThroughGlyphForPoint:inTextContainer:@
fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector :: Selector '[NSPoint, Id NSTextContainer] CDouble
fractionOfDistanceThroughGlyphForPoint_inTextContainerSelector = mkSelector "fractionOfDistanceThroughGlyphForPoint:inTextContainer:"

-- | @Selector@ for @characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:@
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector :: Selector '[NSPoint, Id NSTextContainer, Ptr CDouble] CULong
characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPointsSelector = mkSelector "characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:"

-- | @Selector@ for @getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:@
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector :: Selector '[CULong, Bool, Bool, Ptr CDouble, Ptr CULong] CULong
getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexesSelector = mkSelector "getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:"

-- | @Selector@ for @enumerateLineFragmentsForGlyphRange:usingBlock:@
enumerateLineFragmentsForGlyphRange_usingBlockSelector :: Selector '[NSRange, Ptr ()] ()
enumerateLineFragmentsForGlyphRange_usingBlockSelector = mkSelector "enumerateLineFragmentsForGlyphRange:usingBlock:"

-- | @Selector@ for @enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:@
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector :: Selector '[NSRange, NSRange, Id NSTextContainer, Ptr ()] ()
enumerateEnclosingRectsForGlyphRange_withinSelectedGlyphRange_inTextContainer_usingBlockSelector = mkSelector "enumerateEnclosingRectsForGlyphRange:withinSelectedGlyphRange:inTextContainer:usingBlock:"

-- | @Selector@ for @drawBackgroundForGlyphRange:atPoint:@
drawBackgroundForGlyphRange_atPointSelector :: Selector '[NSRange, NSPoint] ()
drawBackgroundForGlyphRange_atPointSelector = mkSelector "drawBackgroundForGlyphRange:atPoint:"

-- | @Selector@ for @drawGlyphsForGlyphRange:atPoint:@
drawGlyphsForGlyphRange_atPointSelector :: Selector '[NSRange, NSPoint] ()
drawGlyphsForGlyphRange_atPointSelector = mkSelector "drawGlyphsForGlyphRange:atPoint:"

-- | @Selector@ for @fillBackgroundRectArray:count:forCharacterRange:color:@
fillBackgroundRectArray_count_forCharacterRange_colorSelector :: Selector '[Const (Ptr NSRect), CULong, NSRange, Id NSColor] ()
fillBackgroundRectArray_count_forCharacterRange_colorSelector = mkSelector "fillBackgroundRectArray:count:forCharacterRange:color:"

-- | @Selector@ for @drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector '[NSRange, NSUnderlineStyle, CDouble, NSRect, NSRange, NSPoint] ()
drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector '[NSRange, NSUnderlineStyle, NSRect, NSRange, NSPoint] ()
underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector '[NSRange, NSUnderlineStyle, CDouble, NSRect, NSRange, NSPoint] ()
drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:@
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector :: Selector '[NSRange, NSUnderlineStyle, NSRect, NSRange, NSPoint] ()
strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOriginSelector = mkSelector "strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:"

-- | @Selector@ for @showAttachmentCell:inRect:characterIndex:@
showAttachmentCell_inRect_characterIndexSelector :: Selector '[Id NSCell, NSRect, CULong] ()
showAttachmentCell_inRect_characterIndexSelector = mkSelector "showAttachmentCell:inRect:characterIndex:"

-- | @Selector@ for @setLayoutRect:forTextBlock:glyphRange:@
setLayoutRect_forTextBlock_glyphRangeSelector :: Selector '[NSRect, Id NSTextBlock, NSRange] ()
setLayoutRect_forTextBlock_glyphRangeSelector = mkSelector "setLayoutRect:forTextBlock:glyphRange:"

-- | @Selector@ for @setBoundsRect:forTextBlock:glyphRange:@
setBoundsRect_forTextBlock_glyphRangeSelector :: Selector '[NSRect, Id NSTextBlock, NSRange] ()
setBoundsRect_forTextBlock_glyphRangeSelector = mkSelector "setBoundsRect:forTextBlock:glyphRange:"

-- | @Selector@ for @layoutRectForTextBlock:glyphRange:@
layoutRectForTextBlock_glyphRangeSelector :: Selector '[Id NSTextBlock, NSRange] NSRect
layoutRectForTextBlock_glyphRangeSelector = mkSelector "layoutRectForTextBlock:glyphRange:"

-- | @Selector@ for @boundsRectForTextBlock:glyphRange:@
boundsRectForTextBlock_glyphRangeSelector :: Selector '[Id NSTextBlock, NSRange] NSRect
boundsRectForTextBlock_glyphRangeSelector = mkSelector "boundsRectForTextBlock:glyphRange:"

-- | @Selector@ for @layoutRectForTextBlock:atIndex:effectiveRange:@
layoutRectForTextBlock_atIndex_effectiveRangeSelector :: Selector '[Id NSTextBlock, CULong, Ptr NSRange] NSRect
layoutRectForTextBlock_atIndex_effectiveRangeSelector = mkSelector "layoutRectForTextBlock:atIndex:effectiveRange:"

-- | @Selector@ for @boundsRectForTextBlock:atIndex:effectiveRange:@
boundsRectForTextBlock_atIndex_effectiveRangeSelector :: Selector '[Id NSTextBlock, CULong, Ptr NSRange] NSRect
boundsRectForTextBlock_atIndex_effectiveRangeSelector = mkSelector "boundsRectForTextBlock:atIndex:effectiveRange:"

-- | @Selector@ for @temporaryAttributesAtCharacterIndex:effectiveRange:@
temporaryAttributesAtCharacterIndex_effectiveRangeSelector :: Selector '[CULong, Ptr NSRange] (Id NSDictionary)
temporaryAttributesAtCharacterIndex_effectiveRangeSelector = mkSelector "temporaryAttributesAtCharacterIndex:effectiveRange:"

-- | @Selector@ for @setTemporaryAttributes:forCharacterRange:@
setTemporaryAttributes_forCharacterRangeSelector :: Selector '[Id NSDictionary, NSRange] ()
setTemporaryAttributes_forCharacterRangeSelector = mkSelector "setTemporaryAttributes:forCharacterRange:"

-- | @Selector@ for @addTemporaryAttributes:forCharacterRange:@
addTemporaryAttributes_forCharacterRangeSelector :: Selector '[Id NSDictionary, NSRange] ()
addTemporaryAttributes_forCharacterRangeSelector = mkSelector "addTemporaryAttributes:forCharacterRange:"

-- | @Selector@ for @removeTemporaryAttribute:forCharacterRange:@
removeTemporaryAttribute_forCharacterRangeSelector :: Selector '[Id NSString, NSRange] ()
removeTemporaryAttribute_forCharacterRangeSelector = mkSelector "removeTemporaryAttribute:forCharacterRange:"

-- | @Selector@ for @temporaryAttribute:atCharacterIndex:effectiveRange:@
temporaryAttribute_atCharacterIndex_effectiveRangeSelector :: Selector '[Id NSString, CULong, Ptr NSRange] RawId
temporaryAttribute_atCharacterIndex_effectiveRangeSelector = mkSelector "temporaryAttribute:atCharacterIndex:effectiveRange:"

-- | @Selector@ for @temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector :: Selector '[Id NSString, CULong, Ptr NSRange, NSRange] RawId
temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRangeSelector = mkSelector "temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:@
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector :: Selector '[CULong, Ptr NSRange, NSRange] (Id NSDictionary)
temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRangeSelector = mkSelector "temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @addTemporaryAttribute:value:forCharacterRange:@
addTemporaryAttribute_value_forCharacterRangeSelector :: Selector '[Id NSString, RawId, NSRange] ()
addTemporaryAttribute_value_forCharacterRangeSelector = mkSelector "addTemporaryAttribute:value:forCharacterRange:"

-- | @Selector@ for @defaultLineHeightForFont:@
defaultLineHeightForFontSelector :: Selector '[Id NSFont] CDouble
defaultLineHeightForFontSelector = mkSelector "defaultLineHeightForFont:"

-- | @Selector@ for @defaultBaselineOffsetForFont:@
defaultBaselineOffsetForFontSelector :: Selector '[Id NSFont] CDouble
defaultBaselineOffsetForFontSelector = mkSelector "defaultBaselineOffsetForFont:"

-- | @Selector@ for @glyphAtIndex:isValidIndex:@
glyphAtIndex_isValidIndexSelector :: Selector '[CULong, Ptr Bool] CUInt
glyphAtIndex_isValidIndexSelector = mkSelector "glyphAtIndex:isValidIndex:"

-- | @Selector@ for @glyphAtIndex:@
glyphAtIndexSelector :: Selector '[CULong] CUInt
glyphAtIndexSelector = mkSelector "glyphAtIndex:"

-- | @Selector@ for @rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:@
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector :: Selector '[NSRange, NSRange, Id NSTextContainer, Ptr CULong] (Ptr NSRect)
rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCountSelector = mkSelector "rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:"

-- | @Selector@ for @rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:@
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector :: Selector '[NSRange, NSRange, Id NSTextContainer, Ptr CULong] (Ptr NSRect)
rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCountSelector = mkSelector "rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:"

-- | @Selector@ for @substituteFontForFont:@
substituteFontForFontSelector :: Selector '[Id NSFont] (Id NSFont)
substituteFontForFontSelector = mkSelector "substituteFontForFont:"

-- | @Selector@ for @insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:@
insertGlyphs_length_forStartingGlyphAtIndex_characterIndexSelector :: Selector '[Const RawId, CULong, CULong, CULong] ()
insertGlyphs_length_forStartingGlyphAtIndex_characterIndexSelector = mkSelector "insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:"

-- | @Selector@ for @insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndexSelector :: Selector '[CUInt, CULong, CULong] ()
insertGlyph_atGlyphIndex_characterIndexSelector = mkSelector "insertGlyph:atGlyphIndex:characterIndex:"

-- | @Selector@ for @replaceGlyphAtIndex:withGlyph:@
replaceGlyphAtIndex_withGlyphSelector :: Selector '[CULong, CUInt] ()
replaceGlyphAtIndex_withGlyphSelector = mkSelector "replaceGlyphAtIndex:withGlyph:"

-- | @Selector@ for @deleteGlyphsInRange:@
deleteGlyphsInRangeSelector :: Selector '[NSRange] ()
deleteGlyphsInRangeSelector = mkSelector "deleteGlyphsInRange:"

-- | @Selector@ for @setCharacterIndex:forGlyphAtIndex:@
setCharacterIndex_forGlyphAtIndexSelector :: Selector '[CULong, CULong] ()
setCharacterIndex_forGlyphAtIndexSelector = mkSelector "setCharacterIndex:forGlyphAtIndex:"

-- | @Selector@ for @setIntAttribute:value:forGlyphAtIndex:@
setIntAttribute_value_forGlyphAtIndexSelector :: Selector '[CLong, CLong, CULong] ()
setIntAttribute_value_forGlyphAtIndexSelector = mkSelector "setIntAttribute:value:forGlyphAtIndex:"

-- | @Selector@ for @invalidateGlyphsOnLayoutInvalidationForGlyphRange:@
invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector :: Selector '[NSRange] ()
invalidateGlyphsOnLayoutInvalidationForGlyphRangeSelector = mkSelector "invalidateGlyphsOnLayoutInvalidationForGlyphRange:"

-- | @Selector@ for @intAttribute:forGlyphAtIndex:@
intAttribute_forGlyphAtIndexSelector :: Selector '[CLong, CULong] CLong
intAttribute_forGlyphAtIndexSelector = mkSelector "intAttribute:forGlyphAtIndex:"

-- | @Selector@ for @getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector :: Selector '[NSRange, RawId, Ptr CULong, Ptr NSGlyphInscription, Ptr Bool] CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector = mkSelector "getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:"

-- | @Selector@ for @getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector :: Selector '[NSRange, RawId, Ptr CULong, Ptr NSGlyphInscription, Ptr Bool, Ptr CUChar] CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector = mkSelector "getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:"

-- | @Selector@ for @getGlyphs:range:@
getGlyphs_rangeSelector :: Selector '[RawId, NSRange] CULong
getGlyphs_rangeSelector = mkSelector "getGlyphs:range:"

-- | @Selector@ for @invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:@
invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector :: Selector '[NSRange, Bool, Ptr NSRange] ()
invalidateLayoutForCharacterRange_isSoft_actualCharacterRangeSelector = mkSelector "invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:"

-- | @Selector@ for @textStorage:edited:range:changeInLength:invalidatedRange:@
textStorage_edited_range_changeInLength_invalidatedRangeSelector :: Selector '[Id NSTextStorage, CULong, NSRange, CLong, NSRange] ()
textStorage_edited_range_changeInLength_invalidatedRangeSelector = mkSelector "textStorage:edited:range:changeInLength:invalidatedRange:"

-- | @Selector@ for @setLocations:startingGlyphIndexes:count:forGlyphRange:@
setLocations_startingGlyphIndexes_count_forGlyphRangeSelector :: Selector '[Ptr NSPoint, Ptr CULong, CULong, NSRange] ()
setLocations_startingGlyphIndexes_count_forGlyphRangeSelector = mkSelector "setLocations:startingGlyphIndexes:count:forGlyphRange:"

-- | @Selector@ for @showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:@
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector :: Selector '[Ptr CChar, CULong, NSRange, NSPoint, Id NSFont, Id NSColor, NSSize] ()
showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustmentSelector = mkSelector "showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:"

-- | @Selector@ for @showCGGlyphs:positions:count:font:matrix:attributes:inContext:@
showCGGlyphs_positions_count_font_matrix_attributes_inContextSelector :: Selector '[Const RawId, Const (Ptr NSPoint), CULong, Id NSFont, Id NSAffineTransform, Id NSDictionary, Id NSGraphicsContext] ()
showCGGlyphs_positions_count_font_matrix_attributes_inContextSelector = mkSelector "showCGGlyphs:positions:count:font:matrix:attributes:inContext:"

-- | @Selector@ for @rulerMarkersForTextView:paragraphStyle:ruler:@
rulerMarkersForTextView_paragraphStyle_rulerSelector :: Selector '[Id NSTextView, Id NSParagraphStyle, Id NSRulerView] (Id NSArray)
rulerMarkersForTextView_paragraphStyle_rulerSelector = mkSelector "rulerMarkersForTextView:paragraphStyle:ruler:"

-- | @Selector@ for @rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:@
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector :: Selector '[Id NSTextView, Id NSParagraphStyle, Id NSRulerView, Bool] (Id NSView)
rulerAccessoryViewForTextView_paragraphStyle_ruler_enabledSelector = mkSelector "rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:"

-- | @Selector@ for @layoutManagerOwnsFirstResponderInWindow:@
layoutManagerOwnsFirstResponderInWindowSelector :: Selector '[Id NSWindow] Bool
layoutManagerOwnsFirstResponderInWindowSelector = mkSelector "layoutManagerOwnsFirstResponderInWindow:"

-- | @Selector@ for @textStorage@
textStorageSelector :: Selector '[] (Id NSTextStorage)
textStorageSelector = mkSelector "textStorage"

-- | @Selector@ for @setTextStorage:@
setTextStorageSelector :: Selector '[Id NSTextStorage] ()
setTextStorageSelector = mkSelector "setTextStorage:"

-- | @Selector@ for @textContainers@
textContainersSelector :: Selector '[] (Id NSArray)
textContainersSelector = mkSelector "textContainers"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @showsInvisibleCharacters@
showsInvisibleCharactersSelector :: Selector '[] Bool
showsInvisibleCharactersSelector = mkSelector "showsInvisibleCharacters"

-- | @Selector@ for @setShowsInvisibleCharacters:@
setShowsInvisibleCharactersSelector :: Selector '[Bool] ()
setShowsInvisibleCharactersSelector = mkSelector "setShowsInvisibleCharacters:"

-- | @Selector@ for @showsControlCharacters@
showsControlCharactersSelector :: Selector '[] Bool
showsControlCharactersSelector = mkSelector "showsControlCharacters"

-- | @Selector@ for @setShowsControlCharacters:@
setShowsControlCharactersSelector :: Selector '[Bool] ()
setShowsControlCharactersSelector = mkSelector "setShowsControlCharacters:"

-- | @Selector@ for @usesDefaultHyphenation@
usesDefaultHyphenationSelector :: Selector '[] Bool
usesDefaultHyphenationSelector = mkSelector "usesDefaultHyphenation"

-- | @Selector@ for @setUsesDefaultHyphenation:@
setUsesDefaultHyphenationSelector :: Selector '[Bool] ()
setUsesDefaultHyphenationSelector = mkSelector "setUsesDefaultHyphenation:"

-- | @Selector@ for @usesFontLeading@
usesFontLeadingSelector :: Selector '[] Bool
usesFontLeadingSelector = mkSelector "usesFontLeading"

-- | @Selector@ for @setUsesFontLeading:@
setUsesFontLeadingSelector :: Selector '[Bool] ()
setUsesFontLeadingSelector = mkSelector "setUsesFontLeading:"

-- | @Selector@ for @allowsNonContiguousLayout@
allowsNonContiguousLayoutSelector :: Selector '[] Bool
allowsNonContiguousLayoutSelector = mkSelector "allowsNonContiguousLayout"

-- | @Selector@ for @setAllowsNonContiguousLayout:@
setAllowsNonContiguousLayoutSelector :: Selector '[Bool] ()
setAllowsNonContiguousLayoutSelector = mkSelector "setAllowsNonContiguousLayout:"

-- | @Selector@ for @hasNonContiguousLayout@
hasNonContiguousLayoutSelector :: Selector '[] Bool
hasNonContiguousLayoutSelector = mkSelector "hasNonContiguousLayout"

-- | @Selector@ for @limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContentsSelector :: Selector '[] Bool
limitsLayoutForSuspiciousContentsSelector = mkSelector "limitsLayoutForSuspiciousContents"

-- | @Selector@ for @setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContentsSelector :: Selector '[Bool] ()
setLimitsLayoutForSuspiciousContentsSelector = mkSelector "setLimitsLayoutForSuspiciousContents:"

-- | @Selector@ for @backgroundLayoutEnabled@
backgroundLayoutEnabledSelector :: Selector '[] Bool
backgroundLayoutEnabledSelector = mkSelector "backgroundLayoutEnabled"

-- | @Selector@ for @setBackgroundLayoutEnabled:@
setBackgroundLayoutEnabledSelector :: Selector '[Bool] ()
setBackgroundLayoutEnabledSelector = mkSelector "setBackgroundLayoutEnabled:"

-- | @Selector@ for @defaultAttachmentScaling@
defaultAttachmentScalingSelector :: Selector '[] NSImageScaling
defaultAttachmentScalingSelector = mkSelector "defaultAttachmentScaling"

-- | @Selector@ for @setDefaultAttachmentScaling:@
setDefaultAttachmentScalingSelector :: Selector '[NSImageScaling] ()
setDefaultAttachmentScalingSelector = mkSelector "setDefaultAttachmentScaling:"

-- | @Selector@ for @typesetter@
typesetterSelector :: Selector '[] (Id NSTypesetter)
typesetterSelector = mkSelector "typesetter"

-- | @Selector@ for @setTypesetter:@
setTypesetterSelector :: Selector '[Id NSTypesetter] ()
setTypesetterSelector = mkSelector "setTypesetter:"

-- | @Selector@ for @typesetterBehavior@
typesetterBehaviorSelector :: Selector '[] NSTypesetterBehavior
typesetterBehaviorSelector = mkSelector "typesetterBehavior"

-- | @Selector@ for @setTypesetterBehavior:@
setTypesetterBehaviorSelector :: Selector '[NSTypesetterBehavior] ()
setTypesetterBehaviorSelector = mkSelector "setTypesetterBehavior:"

-- | @Selector@ for @numberOfGlyphs@
numberOfGlyphsSelector :: Selector '[] CULong
numberOfGlyphsSelector = mkSelector "numberOfGlyphs"

-- | @Selector@ for @extraLineFragmentRect@
extraLineFragmentRectSelector :: Selector '[] NSRect
extraLineFragmentRectSelector = mkSelector "extraLineFragmentRect"

-- | @Selector@ for @extraLineFragmentUsedRect@
extraLineFragmentUsedRectSelector :: Selector '[] NSRect
extraLineFragmentUsedRectSelector = mkSelector "extraLineFragmentUsedRect"

-- | @Selector@ for @extraLineFragmentTextContainer@
extraLineFragmentTextContainerSelector :: Selector '[] (Id NSTextContainer)
extraLineFragmentTextContainerSelector = mkSelector "extraLineFragmentTextContainer"

-- | @Selector@ for @glyphGenerator@
glyphGeneratorSelector :: Selector '[] (Id NSGlyphGenerator)
glyphGeneratorSelector = mkSelector "glyphGenerator"

-- | @Selector@ for @setGlyphGenerator:@
setGlyphGeneratorSelector :: Selector '[Id NSGlyphGenerator] ()
setGlyphGeneratorSelector = mkSelector "setGlyphGenerator:"

-- | @Selector@ for @usesScreenFonts@
usesScreenFontsSelector :: Selector '[] Bool
usesScreenFontsSelector = mkSelector "usesScreenFonts"

-- | @Selector@ for @setUsesScreenFonts:@
setUsesScreenFontsSelector :: Selector '[Bool] ()
setUsesScreenFontsSelector = mkSelector "setUsesScreenFonts:"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector '[] CFloat
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @setHyphenationFactor:@
setHyphenationFactorSelector :: Selector '[CFloat] ()
setHyphenationFactorSelector = mkSelector "setHyphenationFactor:"

-- | @Selector@ for @firstTextView@
firstTextViewSelector :: Selector '[] (Id NSTextView)
firstTextViewSelector = mkSelector "firstTextView"

-- | @Selector@ for @textViewForBeginningOfSelection@
textViewForBeginningOfSelectionSelector :: Selector '[] (Id NSTextView)
textViewForBeginningOfSelectionSelector = mkSelector "textViewForBeginningOfSelection"

