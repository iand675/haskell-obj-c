{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTypesetter@.
module ObjC.AppKit.NSTypesetter
  ( NSTypesetter
  , IsNSTypesetter(..)
  , substituteFontForFont
  , textTabForGlyphLocation_writingDirection_maxLocation
  , setParagraphGlyphRange_separatorGlyphRange
  , layoutParagraphAtPoint
  , beginParagraph
  , endParagraph
  , beginLineWithGlyphAtIndex
  , endLineWithGlyphRange
  , lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect
  , paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect
  , paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect
  , getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin
  , setHardInvalidation_forGlyphRange
  , layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex
  , layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragments
  , printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_count
  , baselineOffsetInLayoutManager_glyphIndex
  , sharedSystemTypesetterForBehavior
  , actionForControlCharacterAtIndex
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels
  , substituteGlyphsInRange_withGlyphs
  , insertGlyph_atGlyphIndex_characterIndex
  , deleteGlyphsInRange
  , characterRangeForGlyphRange_actualGlyphRange
  , glyphRangeForCharacterRange_actualCharacterRange
  , getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfter
  , setLineFragmentRect_forGlyphRange_usedRect_baselineOffset
  , setNotShownAttribute_forGlyphRange
  , setDrawsOutsideLineFragment_forGlyphRange
  , setLocation_withAdvancements_forStartOfGlyphRange
  , setAttachmentSize_forGlyphRange
  , setBidiLevels_forGlyphRange
  , willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset
  , shouldBreakLineByWordBeforeCharacterAtIndex
  , shouldBreakLineByHyphenatingBeforeCharacterAtIndex
  , hyphenationFactorForGlyphAtIndex
  , hyphenCharacterForGlyphAtIndex
  , boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex
  , usesFontLeading
  , setUsesFontLeading
  , typesetterBehavior
  , setTypesetterBehavior
  , hyphenationFactor
  , setHyphenationFactor
  , lineFragmentPadding
  , setLineFragmentPadding
  , bidiProcessingEnabled
  , setBidiProcessingEnabled
  , attributedString
  , setAttributedString
  , paragraphGlyphRange
  , paragraphSeparatorGlyphRange
  , paragraphCharacterRange
  , paragraphSeparatorCharacterRange
  , attributesForExtraLineFragment
  , layoutManager
  , textContainers
  , currentTextContainer
  , currentParagraphStyle
  , sharedSystemTypesetter
  , defaultTypesetterBehavior
  , actionForControlCharacterAtIndexSelector
  , attributedStringSelector
  , attributesForExtraLineFragmentSelector
  , baselineOffsetInLayoutManager_glyphIndexSelector
  , beginLineWithGlyphAtIndexSelector
  , beginParagraphSelector
  , bidiProcessingEnabledSelector
  , boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector
  , characterRangeForGlyphRange_actualGlyphRangeSelector
  , currentParagraphStyleSelector
  , currentTextContainerSelector
  , defaultTypesetterBehaviorSelector
  , deleteGlyphsInRangeSelector
  , endLineWithGlyphRangeSelector
  , endParagraphSelector
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector
  , getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector
  , getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector
  , glyphRangeForCharacterRange_actualCharacterRangeSelector
  , hyphenCharacterForGlyphAtIndexSelector
  , hyphenationFactorForGlyphAtIndexSelector
  , hyphenationFactorSelector
  , insertGlyph_atGlyphIndex_characterIndexSelector
  , layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector
  , layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector
  , layoutManagerSelector
  , layoutParagraphAtPointSelector
  , lineFragmentPaddingSelector
  , lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphCharacterRangeSelector
  , paragraphGlyphRangeSelector
  , paragraphSeparatorCharacterRangeSelector
  , paragraphSeparatorGlyphRangeSelector
  , paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector
  , printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector
  , setAttachmentSize_forGlyphRangeSelector
  , setAttributedStringSelector
  , setBidiLevels_forGlyphRangeSelector
  , setBidiProcessingEnabledSelector
  , setDrawsOutsideLineFragment_forGlyphRangeSelector
  , setHardInvalidation_forGlyphRangeSelector
  , setHyphenationFactorSelector
  , setLineFragmentPaddingSelector
  , setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector
  , setLocation_withAdvancements_forStartOfGlyphRangeSelector
  , setNotShownAttribute_forGlyphRangeSelector
  , setParagraphGlyphRange_separatorGlyphRangeSelector
  , setTypesetterBehaviorSelector
  , setUsesFontLeadingSelector
  , sharedSystemTypesetterForBehaviorSelector
  , sharedSystemTypesetterSelector
  , shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector
  , shouldBreakLineByWordBeforeCharacterAtIndexSelector
  , substituteFontForFontSelector
  , substituteGlyphsInRange_withGlyphsSelector
  , textContainersSelector
  , textTabForGlyphLocation_writingDirection_maxLocationSelector
  , typesetterBehaviorSelector
  , usesFontLeadingSelector
  , willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector

  -- * Enum types
  , NSTypesetterBehavior(NSTypesetterBehavior)
  , pattern NSTypesetterLatestBehavior
  , pattern NSTypesetterOriginalBehavior
  , pattern NSTypesetterBehavior_10_2_WithCompatibility
  , pattern NSTypesetterBehavior_10_2
  , pattern NSTypesetterBehavior_10_3
  , pattern NSTypesetterBehavior_10_4
  , NSTypesetterControlCharacterAction(NSTypesetterControlCharacterAction)
  , pattern NSTypesetterZeroAdvancementAction
  , pattern NSTypesetterWhitespaceAction
  , pattern NSTypesetterHorizontalTabAction
  , pattern NSTypesetterLineBreakAction
  , pattern NSTypesetterParagraphBreakAction
  , pattern NSTypesetterContainerBreakAction
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

-- | @- substituteFontForFont:@
substituteFontForFont :: (IsNSTypesetter nsTypesetter, IsNSFont originalFont) => nsTypesetter -> originalFont -> IO (Id NSFont)
substituteFontForFont nsTypesetter originalFont =
  sendMessage nsTypesetter substituteFontForFontSelector (toNSFont originalFont)

-- | @- textTabForGlyphLocation:writingDirection:maxLocation:@
textTabForGlyphLocation_writingDirection_maxLocation :: IsNSTypesetter nsTypesetter => nsTypesetter -> CDouble -> NSWritingDirection -> CDouble -> IO (Id NSTextTab)
textTabForGlyphLocation_writingDirection_maxLocation nsTypesetter glyphLocation direction maxLocation =
  sendMessage nsTypesetter textTabForGlyphLocation_writingDirection_maxLocationSelector glyphLocation direction maxLocation

-- | @- setParagraphGlyphRange:separatorGlyphRange:@
setParagraphGlyphRange_separatorGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> NSRange -> IO ()
setParagraphGlyphRange_separatorGlyphRange nsTypesetter paragraphRange paragraphSeparatorRange =
  sendMessage nsTypesetter setParagraphGlyphRange_separatorGlyphRangeSelector paragraphRange paragraphSeparatorRange

-- | @- layoutParagraphAtPoint:@
layoutParagraphAtPoint :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSPoint -> IO CULong
layoutParagraphAtPoint nsTypesetter lineFragmentOrigin =
  sendMessage nsTypesetter layoutParagraphAtPointSelector lineFragmentOrigin

-- | @- beginParagraph@
beginParagraph :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO ()
beginParagraph nsTypesetter =
  sendMessage nsTypesetter beginParagraphSelector

-- | @- endParagraph@
endParagraph :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO ()
endParagraph nsTypesetter =
  sendMessage nsTypesetter endParagraphSelector

-- | @- beginLineWithGlyphAtIndex:@
beginLineWithGlyphAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO ()
beginLineWithGlyphAtIndex nsTypesetter glyphIndex =
  sendMessage nsTypesetter beginLineWithGlyphAtIndexSelector glyphIndex

-- | @- endLineWithGlyphRange:@
endLineWithGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> IO ()
endLineWithGlyphRange nsTypesetter lineGlyphRange =
  sendMessage nsTypesetter endLineWithGlyphRangeSelector lineGlyphRange

-- | @- lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> NSRect -> IO CDouble
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsTypesetter glyphIndex rect =
  sendMessage nsTypesetter lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector glyphIndex rect

-- | @- paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect nsTypesetter glyphIndex rect =
  sendMessage nsTypesetter paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector glyphIndex rect

-- | @- paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsTypesetter glyphIndex rect =
  sendMessage nsTypesetter paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector glyphIndex rect

-- | @- getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSRect -> Ptr NSRect -> NSRange -> NSPoint -> IO ()
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin nsTypesetter lineFragmentRect lineFragmentUsedRect paragraphSeparatorGlyphRange lineOrigin =
  sendMessage nsTypesetter getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector lineFragmentRect lineFragmentUsedRect paragraphSeparatorGlyphRange lineOrigin

-- | @- setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> NSRange -> IO ()
setHardInvalidation_forGlyphRange nsTypesetter flag glyphRange =
  sendMessage nsTypesetter setHardInvalidation_forGlyphRangeSelector flag glyphRange

-- | @- layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:@
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex :: (IsNSTypesetter nsTypesetter, IsNSLayoutManager layoutManager) => nsTypesetter -> layoutManager -> CULong -> CULong -> Ptr CULong -> IO ()
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex nsTypesetter layoutManager startGlyphIndex maxNumLines nextGlyph =
  sendMessage nsTypesetter layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector (toNSLayoutManager layoutManager) startGlyphIndex maxNumLines nextGlyph

-- | @- layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:@
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragments :: (IsNSTypesetter nsTypesetter, IsNSLayoutManager layoutManager) => nsTypesetter -> NSRange -> layoutManager -> CULong -> IO NSRange
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragments nsTypesetter characterRange layoutManager maxNumLines =
  sendMessage nsTypesetter layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector characterRange (toNSLayoutManager layoutManager) maxNumLines

-- | @+ printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:@
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_count :: IsNSLayoutManager layoutMgr => layoutMgr -> NSRange -> Const (Ptr CUChar) -> CULong -> IO NSSize
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_count layoutMgr nominallySpacedGlyphsRange packedGlyphs packedGlyphsCount =
  do
    cls' <- getRequiredClass "NSTypesetter"
    sendClassMessage cls' printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector (toNSLayoutManager layoutMgr) nominallySpacedGlyphsRange packedGlyphs packedGlyphsCount

-- | @- baselineOffsetInLayoutManager:glyphIndex:@
baselineOffsetInLayoutManager_glyphIndex :: (IsNSTypesetter nsTypesetter, IsNSLayoutManager layoutMgr) => nsTypesetter -> layoutMgr -> CULong -> IO CDouble
baselineOffsetInLayoutManager_glyphIndex nsTypesetter layoutMgr glyphIndex =
  sendMessage nsTypesetter baselineOffsetInLayoutManager_glyphIndexSelector (toNSLayoutManager layoutMgr) glyphIndex

-- | @+ sharedSystemTypesetterForBehavior:@
sharedSystemTypesetterForBehavior :: NSTypesetterBehavior -> IO RawId
sharedSystemTypesetterForBehavior behavior =
  do
    cls' <- getRequiredClass "NSTypesetter"
    sendClassMessage cls' sharedSystemTypesetterForBehaviorSelector behavior

-- | @- actionForControlCharacterAtIndex:@
actionForControlCharacterAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO NSTypesetterControlCharacterAction
actionForControlCharacterAtIndex nsTypesetter charIndex =
  sendMessage nsTypesetter actionForControlCharacterAtIndexSelector charIndex

-- | @- getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> RawId -> Ptr CULong -> Ptr NSGlyphInscription -> Ptr Bool -> Ptr CUChar -> IO CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels nsTypesetter glyphsRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer bidiLevelBuffer =
  sendMessage nsTypesetter getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector glyphsRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer bidiLevelBuffer

-- | @- substituteGlyphsInRange:withGlyphs:@
substituteGlyphsInRange_withGlyphs :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> RawId -> IO ()
substituteGlyphsInRange_withGlyphs nsTypesetter glyphRange glyphs =
  sendMessage nsTypesetter substituteGlyphsInRange_withGlyphsSelector glyphRange glyphs

-- | @- insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CUInt -> CULong -> CULong -> IO ()
insertGlyph_atGlyphIndex_characterIndex nsTypesetter glyph glyphIndex characterIndex =
  sendMessage nsTypesetter insertGlyph_atGlyphIndex_characterIndexSelector glyph glyphIndex characterIndex

-- | @- deleteGlyphsInRange:@
deleteGlyphsInRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> IO ()
deleteGlyphsInRange nsTypesetter glyphRange =
  sendMessage nsTypesetter deleteGlyphsInRangeSelector glyphRange

-- | @- characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> Ptr NSRange -> IO NSRange
characterRangeForGlyphRange_actualGlyphRange nsTypesetter glyphRange actualGlyphRange =
  sendMessage nsTypesetter characterRangeForGlyphRange_actualGlyphRangeSelector glyphRange actualGlyphRange

-- | @- glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> Ptr NSRange -> IO NSRange
glyphRangeForCharacterRange_actualCharacterRange nsTypesetter charRange actualCharRange =
  sendMessage nsTypesetter glyphRangeForCharacterRange_actualCharacterRangeSelector charRange actualCharRange

-- | @- getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:@
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfter :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSRect -> Ptr NSRect -> Ptr NSRect -> CULong -> NSRect -> CDouble -> CDouble -> CDouble -> IO ()
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfter nsTypesetter lineFragmentRect lineFragmentUsedRect remainingRect startingGlyphIndex proposedRect lineSpacing paragraphSpacingBefore paragraphSpacingAfter =
  sendMessage nsTypesetter getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector lineFragmentRect lineFragmentUsedRect remainingRect startingGlyphIndex proposedRect lineSpacing paragraphSpacingBefore paragraphSpacingAfter

-- | @- setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
setLineFragmentRect_forGlyphRange_usedRect_baselineOffset :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRect -> NSRange -> NSRect -> CDouble -> IO ()
setLineFragmentRect_forGlyphRange_usedRect_baselineOffset nsTypesetter fragmentRect glyphRange usedRect baselineOffset =
  sendMessage nsTypesetter setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector fragmentRect glyphRange usedRect baselineOffset

-- | @- setNotShownAttribute:forGlyphRange:@
setNotShownAttribute_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> NSRange -> IO ()
setNotShownAttribute_forGlyphRange nsTypesetter flag glyphRange =
  sendMessage nsTypesetter setNotShownAttribute_forGlyphRangeSelector flag glyphRange

-- | @- setDrawsOutsideLineFragment:forGlyphRange:@
setDrawsOutsideLineFragment_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> NSRange -> IO ()
setDrawsOutsideLineFragment_forGlyphRange nsTypesetter flag glyphRange =
  sendMessage nsTypesetter setDrawsOutsideLineFragment_forGlyphRangeSelector flag glyphRange

-- | @- setLocation:withAdvancements:forStartOfGlyphRange:@
setLocation_withAdvancements_forStartOfGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSPoint -> Const (Ptr CDouble) -> NSRange -> IO ()
setLocation_withAdvancements_forStartOfGlyphRange nsTypesetter location advancements glyphRange =
  sendMessage nsTypesetter setLocation_withAdvancements_forStartOfGlyphRangeSelector location advancements glyphRange

-- | @- setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSSize -> NSRange -> IO ()
setAttachmentSize_forGlyphRange nsTypesetter attachmentSize glyphRange =
  sendMessage nsTypesetter setAttachmentSize_forGlyphRangeSelector attachmentSize glyphRange

-- | @- setBidiLevels:forGlyphRange:@
setBidiLevels_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Const (Ptr CUChar) -> NSRange -> IO ()
setBidiLevels_forGlyphRange nsTypesetter levels glyphRange =
  sendMessage nsTypesetter setBidiLevels_forGlyphRangeSelector levels glyphRange

-- | @- willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSRect -> NSRange -> Ptr NSRect -> Ptr CDouble -> IO ()
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset nsTypesetter lineRect glyphRange usedRect baselineOffset =
  sendMessage nsTypesetter willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector lineRect glyphRange usedRect baselineOffset

-- | @- shouldBreakLineByWordBeforeCharacterAtIndex:@
shouldBreakLineByWordBeforeCharacterAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO Bool
shouldBreakLineByWordBeforeCharacterAtIndex nsTypesetter charIndex =
  sendMessage nsTypesetter shouldBreakLineByWordBeforeCharacterAtIndexSelector charIndex

-- | @- shouldBreakLineByHyphenatingBeforeCharacterAtIndex:@
shouldBreakLineByHyphenatingBeforeCharacterAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO Bool
shouldBreakLineByHyphenatingBeforeCharacterAtIndex nsTypesetter charIndex =
  sendMessage nsTypesetter shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector charIndex

-- | @- hyphenationFactorForGlyphAtIndex:@
hyphenationFactorForGlyphAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO CFloat
hyphenationFactorForGlyphAtIndex nsTypesetter glyphIndex =
  sendMessage nsTypesetter hyphenationFactorForGlyphAtIndexSelector glyphIndex

-- | @- hyphenCharacterForGlyphAtIndex:@
hyphenCharacterForGlyphAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO CUInt
hyphenCharacterForGlyphAtIndex nsTypesetter glyphIndex =
  sendMessage nsTypesetter hyphenCharacterForGlyphAtIndexSelector glyphIndex

-- | @- boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:@
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex :: (IsNSTypesetter nsTypesetter, IsNSTextContainer textContainer) => nsTypesetter -> CULong -> textContainer -> NSRect -> NSPoint -> CULong -> IO NSRect
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex nsTypesetter glyphIndex textContainer proposedRect glyphPosition charIndex =
  sendMessage nsTypesetter boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector glyphIndex (toNSTextContainer textContainer) proposedRect glyphPosition charIndex

-- | @- usesFontLeading@
usesFontLeading :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO Bool
usesFontLeading nsTypesetter =
  sendMessage nsTypesetter usesFontLeadingSelector

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> IO ()
setUsesFontLeading nsTypesetter value =
  sendMessage nsTypesetter setUsesFontLeadingSelector value

-- | @- typesetterBehavior@
typesetterBehavior :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSTypesetterBehavior
typesetterBehavior nsTypesetter =
  sendMessage nsTypesetter typesetterBehaviorSelector

-- | @- setTypesetterBehavior:@
setTypesetterBehavior :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSTypesetterBehavior -> IO ()
setTypesetterBehavior nsTypesetter value =
  sendMessage nsTypesetter setTypesetterBehaviorSelector value

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO CFloat
hyphenationFactor nsTypesetter =
  sendMessage nsTypesetter hyphenationFactorSelector

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSTypesetter nsTypesetter => nsTypesetter -> CFloat -> IO ()
setHyphenationFactor nsTypesetter value =
  sendMessage nsTypesetter setHyphenationFactorSelector value

-- | @- lineFragmentPadding@
lineFragmentPadding :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO CDouble
lineFragmentPadding nsTypesetter =
  sendMessage nsTypesetter lineFragmentPaddingSelector

-- | @- setLineFragmentPadding:@
setLineFragmentPadding :: IsNSTypesetter nsTypesetter => nsTypesetter -> CDouble -> IO ()
setLineFragmentPadding nsTypesetter value =
  sendMessage nsTypesetter setLineFragmentPaddingSelector value

-- | @- bidiProcessingEnabled@
bidiProcessingEnabled :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO Bool
bidiProcessingEnabled nsTypesetter =
  sendMessage nsTypesetter bidiProcessingEnabledSelector

-- | @- setBidiProcessingEnabled:@
setBidiProcessingEnabled :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> IO ()
setBidiProcessingEnabled nsTypesetter value =
  sendMessage nsTypesetter setBidiProcessingEnabledSelector value

-- | @- attributedString@
attributedString :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSAttributedString)
attributedString nsTypesetter =
  sendMessage nsTypesetter attributedStringSelector

-- | @- setAttributedString:@
setAttributedString :: (IsNSTypesetter nsTypesetter, IsNSAttributedString value) => nsTypesetter -> value -> IO ()
setAttributedString nsTypesetter value =
  sendMessage nsTypesetter setAttributedStringSelector (toNSAttributedString value)

-- | @- paragraphGlyphRange@
paragraphGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphGlyphRange nsTypesetter =
  sendMessage nsTypesetter paragraphGlyphRangeSelector

-- | @- paragraphSeparatorGlyphRange@
paragraphSeparatorGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphSeparatorGlyphRange nsTypesetter =
  sendMessage nsTypesetter paragraphSeparatorGlyphRangeSelector

-- | @- paragraphCharacterRange@
paragraphCharacterRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphCharacterRange nsTypesetter =
  sendMessage nsTypesetter paragraphCharacterRangeSelector

-- | @- paragraphSeparatorCharacterRange@
paragraphSeparatorCharacterRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphSeparatorCharacterRange nsTypesetter =
  sendMessage nsTypesetter paragraphSeparatorCharacterRangeSelector

-- | @- attributesForExtraLineFragment@
attributesForExtraLineFragment :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSDictionary)
attributesForExtraLineFragment nsTypesetter =
  sendMessage nsTypesetter attributesForExtraLineFragmentSelector

-- | @- layoutManager@
layoutManager :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSLayoutManager)
layoutManager nsTypesetter =
  sendMessage nsTypesetter layoutManagerSelector

-- | @- textContainers@
textContainers :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSArray)
textContainers nsTypesetter =
  sendMessage nsTypesetter textContainersSelector

-- | @- currentTextContainer@
currentTextContainer :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSTextContainer)
currentTextContainer nsTypesetter =
  sendMessage nsTypesetter currentTextContainerSelector

-- | @- currentParagraphStyle@
currentParagraphStyle :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSParagraphStyle)
currentParagraphStyle nsTypesetter =
  sendMessage nsTypesetter currentParagraphStyleSelector

-- | @+ sharedSystemTypesetter@
sharedSystemTypesetter :: IO (Id NSTypesetter)
sharedSystemTypesetter  =
  do
    cls' <- getRequiredClass "NSTypesetter"
    sendClassMessage cls' sharedSystemTypesetterSelector

-- | @+ defaultTypesetterBehavior@
defaultTypesetterBehavior :: IO NSTypesetterBehavior
defaultTypesetterBehavior  =
  do
    cls' <- getRequiredClass "NSTypesetter"
    sendClassMessage cls' defaultTypesetterBehaviorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @substituteFontForFont:@
substituteFontForFontSelector :: Selector '[Id NSFont] (Id NSFont)
substituteFontForFontSelector = mkSelector "substituteFontForFont:"

-- | @Selector@ for @textTabForGlyphLocation:writingDirection:maxLocation:@
textTabForGlyphLocation_writingDirection_maxLocationSelector :: Selector '[CDouble, NSWritingDirection, CDouble] (Id NSTextTab)
textTabForGlyphLocation_writingDirection_maxLocationSelector = mkSelector "textTabForGlyphLocation:writingDirection:maxLocation:"

-- | @Selector@ for @setParagraphGlyphRange:separatorGlyphRange:@
setParagraphGlyphRange_separatorGlyphRangeSelector :: Selector '[NSRange, NSRange] ()
setParagraphGlyphRange_separatorGlyphRangeSelector = mkSelector "setParagraphGlyphRange:separatorGlyphRange:"

-- | @Selector@ for @layoutParagraphAtPoint:@
layoutParagraphAtPointSelector :: Selector '[Ptr NSPoint] CULong
layoutParagraphAtPointSelector = mkSelector "layoutParagraphAtPoint:"

-- | @Selector@ for @beginParagraph@
beginParagraphSelector :: Selector '[] ()
beginParagraphSelector = mkSelector "beginParagraph"

-- | @Selector@ for @endParagraph@
endParagraphSelector :: Selector '[] ()
endParagraphSelector = mkSelector "endParagraph"

-- | @Selector@ for @beginLineWithGlyphAtIndex:@
beginLineWithGlyphAtIndexSelector :: Selector '[CULong] ()
beginLineWithGlyphAtIndexSelector = mkSelector "beginLineWithGlyphAtIndex:"

-- | @Selector@ for @endLineWithGlyphRange:@
endLineWithGlyphRangeSelector :: Selector '[NSRange] ()
endLineWithGlyphRangeSelector = mkSelector "endLineWithGlyphRange:"

-- | @Selector@ for @lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector '[CULong, NSRect] CDouble
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector '[CULong, NSRect] CDouble
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector '[CULong, NSRect] CDouble
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector :: Selector '[Ptr NSRect, Ptr NSRect, NSRange, NSPoint] ()
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector = mkSelector "getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:"

-- | @Selector@ for @setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRangeSelector :: Selector '[Bool, NSRange] ()
setHardInvalidation_forGlyphRangeSelector = mkSelector "setHardInvalidation:forGlyphRange:"

-- | @Selector@ for @layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:@
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector :: Selector '[Id NSLayoutManager, CULong, CULong, Ptr CULong] ()
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector = mkSelector "layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:"

-- | @Selector@ for @layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:@
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector :: Selector '[NSRange, Id NSLayoutManager, CULong] NSRange
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector = mkSelector "layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:"

-- | @Selector@ for @printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:@
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector :: Selector '[Id NSLayoutManager, NSRange, Const (Ptr CUChar), CULong] NSSize
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector = mkSelector "printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:"

-- | @Selector@ for @baselineOffsetInLayoutManager:glyphIndex:@
baselineOffsetInLayoutManager_glyphIndexSelector :: Selector '[Id NSLayoutManager, CULong] CDouble
baselineOffsetInLayoutManager_glyphIndexSelector = mkSelector "baselineOffsetInLayoutManager:glyphIndex:"

-- | @Selector@ for @sharedSystemTypesetterForBehavior:@
sharedSystemTypesetterForBehaviorSelector :: Selector '[NSTypesetterBehavior] RawId
sharedSystemTypesetterForBehaviorSelector = mkSelector "sharedSystemTypesetterForBehavior:"

-- | @Selector@ for @actionForControlCharacterAtIndex:@
actionForControlCharacterAtIndexSelector :: Selector '[CULong] NSTypesetterControlCharacterAction
actionForControlCharacterAtIndexSelector = mkSelector "actionForControlCharacterAtIndex:"

-- | @Selector@ for @getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector :: Selector '[NSRange, RawId, Ptr CULong, Ptr NSGlyphInscription, Ptr Bool, Ptr CUChar] CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevelsSelector = mkSelector "getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:"

-- | @Selector@ for @substituteGlyphsInRange:withGlyphs:@
substituteGlyphsInRange_withGlyphsSelector :: Selector '[NSRange, RawId] ()
substituteGlyphsInRange_withGlyphsSelector = mkSelector "substituteGlyphsInRange:withGlyphs:"

-- | @Selector@ for @insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndexSelector :: Selector '[CUInt, CULong, CULong] ()
insertGlyph_atGlyphIndex_characterIndexSelector = mkSelector "insertGlyph:atGlyphIndex:characterIndex:"

-- | @Selector@ for @deleteGlyphsInRange:@
deleteGlyphsInRangeSelector :: Selector '[NSRange] ()
deleteGlyphsInRangeSelector = mkSelector "deleteGlyphsInRange:"

-- | @Selector@ for @characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRangeSelector :: Selector '[NSRange, Ptr NSRange] NSRange
characterRangeForGlyphRange_actualGlyphRangeSelector = mkSelector "characterRangeForGlyphRange:actualGlyphRange:"

-- | @Selector@ for @glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRangeSelector :: Selector '[NSRange, Ptr NSRange] NSRange
glyphRangeForCharacterRange_actualCharacterRangeSelector = mkSelector "glyphRangeForCharacterRange:actualCharacterRange:"

-- | @Selector@ for @getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:@
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector :: Selector '[Ptr NSRect, Ptr NSRect, Ptr NSRect, CULong, NSRect, CDouble, CDouble, CDouble] ()
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector = mkSelector "getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:"

-- | @Selector@ for @setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector :: Selector '[NSRect, NSRange, NSRect, CDouble] ()
setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector = mkSelector "setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:"

-- | @Selector@ for @setNotShownAttribute:forGlyphRange:@
setNotShownAttribute_forGlyphRangeSelector :: Selector '[Bool, NSRange] ()
setNotShownAttribute_forGlyphRangeSelector = mkSelector "setNotShownAttribute:forGlyphRange:"

-- | @Selector@ for @setDrawsOutsideLineFragment:forGlyphRange:@
setDrawsOutsideLineFragment_forGlyphRangeSelector :: Selector '[Bool, NSRange] ()
setDrawsOutsideLineFragment_forGlyphRangeSelector = mkSelector "setDrawsOutsideLineFragment:forGlyphRange:"

-- | @Selector@ for @setLocation:withAdvancements:forStartOfGlyphRange:@
setLocation_withAdvancements_forStartOfGlyphRangeSelector :: Selector '[NSPoint, Const (Ptr CDouble), NSRange] ()
setLocation_withAdvancements_forStartOfGlyphRangeSelector = mkSelector "setLocation:withAdvancements:forStartOfGlyphRange:"

-- | @Selector@ for @setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRangeSelector :: Selector '[NSSize, NSRange] ()
setAttachmentSize_forGlyphRangeSelector = mkSelector "setAttachmentSize:forGlyphRange:"

-- | @Selector@ for @setBidiLevels:forGlyphRange:@
setBidiLevels_forGlyphRangeSelector :: Selector '[Const (Ptr CUChar), NSRange] ()
setBidiLevels_forGlyphRangeSelector = mkSelector "setBidiLevels:forGlyphRange:"

-- | @Selector@ for @willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector :: Selector '[Ptr NSRect, NSRange, Ptr NSRect, Ptr CDouble] ()
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector = mkSelector "willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:"

-- | @Selector@ for @shouldBreakLineByWordBeforeCharacterAtIndex:@
shouldBreakLineByWordBeforeCharacterAtIndexSelector :: Selector '[CULong] Bool
shouldBreakLineByWordBeforeCharacterAtIndexSelector = mkSelector "shouldBreakLineByWordBeforeCharacterAtIndex:"

-- | @Selector@ for @shouldBreakLineByHyphenatingBeforeCharacterAtIndex:@
shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector :: Selector '[CULong] Bool
shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector = mkSelector "shouldBreakLineByHyphenatingBeforeCharacterAtIndex:"

-- | @Selector@ for @hyphenationFactorForGlyphAtIndex:@
hyphenationFactorForGlyphAtIndexSelector :: Selector '[CULong] CFloat
hyphenationFactorForGlyphAtIndexSelector = mkSelector "hyphenationFactorForGlyphAtIndex:"

-- | @Selector@ for @hyphenCharacterForGlyphAtIndex:@
hyphenCharacterForGlyphAtIndexSelector :: Selector '[CULong] CUInt
hyphenCharacterForGlyphAtIndexSelector = mkSelector "hyphenCharacterForGlyphAtIndex:"

-- | @Selector@ for @boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:@
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector :: Selector '[CULong, Id NSTextContainer, NSRect, NSPoint, CULong] NSRect
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector = mkSelector "boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:"

-- | @Selector@ for @usesFontLeading@
usesFontLeadingSelector :: Selector '[] Bool
usesFontLeadingSelector = mkSelector "usesFontLeading"

-- | @Selector@ for @setUsesFontLeading:@
setUsesFontLeadingSelector :: Selector '[Bool] ()
setUsesFontLeadingSelector = mkSelector "setUsesFontLeading:"

-- | @Selector@ for @typesetterBehavior@
typesetterBehaviorSelector :: Selector '[] NSTypesetterBehavior
typesetterBehaviorSelector = mkSelector "typesetterBehavior"

-- | @Selector@ for @setTypesetterBehavior:@
setTypesetterBehaviorSelector :: Selector '[NSTypesetterBehavior] ()
setTypesetterBehaviorSelector = mkSelector "setTypesetterBehavior:"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector '[] CFloat
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @setHyphenationFactor:@
setHyphenationFactorSelector :: Selector '[CFloat] ()
setHyphenationFactorSelector = mkSelector "setHyphenationFactor:"

-- | @Selector@ for @lineFragmentPadding@
lineFragmentPaddingSelector :: Selector '[] CDouble
lineFragmentPaddingSelector = mkSelector "lineFragmentPadding"

-- | @Selector@ for @setLineFragmentPadding:@
setLineFragmentPaddingSelector :: Selector '[CDouble] ()
setLineFragmentPaddingSelector = mkSelector "setLineFragmentPadding:"

-- | @Selector@ for @bidiProcessingEnabled@
bidiProcessingEnabledSelector :: Selector '[] Bool
bidiProcessingEnabledSelector = mkSelector "bidiProcessingEnabled"

-- | @Selector@ for @setBidiProcessingEnabled:@
setBidiProcessingEnabledSelector :: Selector '[Bool] ()
setBidiProcessingEnabledSelector = mkSelector "setBidiProcessingEnabled:"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @setAttributedString:@
setAttributedStringSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringSelector = mkSelector "setAttributedString:"

-- | @Selector@ for @paragraphGlyphRange@
paragraphGlyphRangeSelector :: Selector '[] NSRange
paragraphGlyphRangeSelector = mkSelector "paragraphGlyphRange"

-- | @Selector@ for @paragraphSeparatorGlyphRange@
paragraphSeparatorGlyphRangeSelector :: Selector '[] NSRange
paragraphSeparatorGlyphRangeSelector = mkSelector "paragraphSeparatorGlyphRange"

-- | @Selector@ for @paragraphCharacterRange@
paragraphCharacterRangeSelector :: Selector '[] NSRange
paragraphCharacterRangeSelector = mkSelector "paragraphCharacterRange"

-- | @Selector@ for @paragraphSeparatorCharacterRange@
paragraphSeparatorCharacterRangeSelector :: Selector '[] NSRange
paragraphSeparatorCharacterRangeSelector = mkSelector "paragraphSeparatorCharacterRange"

-- | @Selector@ for @attributesForExtraLineFragment@
attributesForExtraLineFragmentSelector :: Selector '[] (Id NSDictionary)
attributesForExtraLineFragmentSelector = mkSelector "attributesForExtraLineFragment"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector '[] (Id NSLayoutManager)
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @textContainers@
textContainersSelector :: Selector '[] (Id NSArray)
textContainersSelector = mkSelector "textContainers"

-- | @Selector@ for @currentTextContainer@
currentTextContainerSelector :: Selector '[] (Id NSTextContainer)
currentTextContainerSelector = mkSelector "currentTextContainer"

-- | @Selector@ for @currentParagraphStyle@
currentParagraphStyleSelector :: Selector '[] (Id NSParagraphStyle)
currentParagraphStyleSelector = mkSelector "currentParagraphStyle"

-- | @Selector@ for @sharedSystemTypesetter@
sharedSystemTypesetterSelector :: Selector '[] (Id NSTypesetter)
sharedSystemTypesetterSelector = mkSelector "sharedSystemTypesetter"

-- | @Selector@ for @defaultTypesetterBehavior@
defaultTypesetterBehaviorSelector :: Selector '[] NSTypesetterBehavior
defaultTypesetterBehaviorSelector = mkSelector "defaultTypesetterBehavior"

