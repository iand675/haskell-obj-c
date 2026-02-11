{-# LANGUAGE PatternSynonyms #-}
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
  , substituteFontForFontSelector
  , textTabForGlyphLocation_writingDirection_maxLocationSelector
  , setParagraphGlyphRange_separatorGlyphRangeSelector
  , layoutParagraphAtPointSelector
  , beginParagraphSelector
  , endParagraphSelector
  , beginLineWithGlyphAtIndexSelector
  , endLineWithGlyphRangeSelector
  , lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector
  , setHardInvalidation_forGlyphRangeSelector
  , layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector
  , layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector
  , printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector
  , baselineOffsetInLayoutManager_glyphIndexSelector
  , sharedSystemTypesetterForBehaviorSelector
  , actionForControlCharacterAtIndexSelector
  , insertGlyph_atGlyphIndex_characterIndexSelector
  , deleteGlyphsInRangeSelector
  , characterRangeForGlyphRange_actualGlyphRangeSelector
  , glyphRangeForCharacterRange_actualCharacterRangeSelector
  , getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector
  , setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector
  , setNotShownAttribute_forGlyphRangeSelector
  , setDrawsOutsideLineFragment_forGlyphRangeSelector
  , setLocation_withAdvancements_forStartOfGlyphRangeSelector
  , setAttachmentSize_forGlyphRangeSelector
  , setBidiLevels_forGlyphRangeSelector
  , willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector
  , shouldBreakLineByWordBeforeCharacterAtIndexSelector
  , shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector
  , hyphenationFactorForGlyphAtIndexSelector
  , hyphenCharacterForGlyphAtIndexSelector
  , boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector
  , usesFontLeadingSelector
  , setUsesFontLeadingSelector
  , typesetterBehaviorSelector
  , setTypesetterBehaviorSelector
  , hyphenationFactorSelector
  , setHyphenationFactorSelector
  , lineFragmentPaddingSelector
  , setLineFragmentPaddingSelector
  , bidiProcessingEnabledSelector
  , setBidiProcessingEnabledSelector
  , attributedStringSelector
  , setAttributedStringSelector
  , paragraphGlyphRangeSelector
  , paragraphSeparatorGlyphRangeSelector
  , paragraphCharacterRangeSelector
  , paragraphSeparatorCharacterRangeSelector
  , attributesForExtraLineFragmentSelector
  , layoutManagerSelector
  , textContainersSelector
  , currentTextContainerSelector
  , currentParagraphStyleSelector
  , sharedSystemTypesetterSelector
  , defaultTypesetterBehaviorSelector

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

-- | @- substituteFontForFont:@
substituteFontForFont :: (IsNSTypesetter nsTypesetter, IsNSFont originalFont) => nsTypesetter -> originalFont -> IO (Id NSFont)
substituteFontForFont nsTypesetter  originalFont =
withObjCPtr originalFont $ \raw_originalFont ->
    sendMsg nsTypesetter (mkSelector "substituteFontForFont:") (retPtr retVoid) [argPtr (castPtr raw_originalFont :: Ptr ())] >>= retainedObject . castPtr

-- | @- textTabForGlyphLocation:writingDirection:maxLocation:@
textTabForGlyphLocation_writingDirection_maxLocation :: IsNSTypesetter nsTypesetter => nsTypesetter -> CDouble -> NSWritingDirection -> CDouble -> IO (Id NSTextTab)
textTabForGlyphLocation_writingDirection_maxLocation nsTypesetter  glyphLocation direction maxLocation =
  sendMsg nsTypesetter (mkSelector "textTabForGlyphLocation:writingDirection:maxLocation:") (retPtr retVoid) [argCDouble (fromIntegral glyphLocation), argCLong (coerce direction), argCDouble (fromIntegral maxLocation)] >>= retainedObject . castPtr

-- | @- setParagraphGlyphRange:separatorGlyphRange:@
setParagraphGlyphRange_separatorGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> NSRange -> IO ()
setParagraphGlyphRange_separatorGlyphRange nsTypesetter  paragraphRange paragraphSeparatorRange =
  sendMsg nsTypesetter (mkSelector "setParagraphGlyphRange:separatorGlyphRange:") retVoid [argNSRange paragraphRange, argNSRange paragraphSeparatorRange]

-- | @- layoutParagraphAtPoint:@
layoutParagraphAtPoint :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSPoint -> IO CULong
layoutParagraphAtPoint nsTypesetter  lineFragmentOrigin =
  sendMsg nsTypesetter (mkSelector "layoutParagraphAtPoint:") retCULong [argPtr lineFragmentOrigin]

-- | @- beginParagraph@
beginParagraph :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO ()
beginParagraph nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "beginParagraph") retVoid []

-- | @- endParagraph@
endParagraph :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO ()
endParagraph nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "endParagraph") retVoid []

-- | @- beginLineWithGlyphAtIndex:@
beginLineWithGlyphAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO ()
beginLineWithGlyphAtIndex nsTypesetter  glyphIndex =
  sendMsg nsTypesetter (mkSelector "beginLineWithGlyphAtIndex:") retVoid [argCULong (fromIntegral glyphIndex)]

-- | @- endLineWithGlyphRange:@
endLineWithGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> IO ()
endLineWithGlyphRange nsTypesetter  lineGlyphRange =
  sendMsg nsTypesetter (mkSelector "endLineWithGlyphRange:") retVoid [argNSRange lineGlyphRange]

-- | @- lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> NSRect -> IO CDouble
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsTypesetter  glyphIndex rect =
  sendMsg nsTypesetter (mkSelector "lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:") retCDouble [argCULong (fromIntegral glyphIndex), argNSRect rect]

-- | @- paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect nsTypesetter  glyphIndex rect =
  sendMsg nsTypesetter (mkSelector "paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:") retCDouble [argCULong (fromIntegral glyphIndex), argNSRect rect]

-- | @- paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsTypesetter  glyphIndex rect =
  sendMsg nsTypesetter (mkSelector "paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:") retCDouble [argCULong (fromIntegral glyphIndex), argNSRect rect]

-- | @- getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSRect -> Ptr NSRect -> NSRange -> NSPoint -> IO ()
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin nsTypesetter  lineFragmentRect lineFragmentUsedRect paragraphSeparatorGlyphRange lineOrigin =
  sendMsg nsTypesetter (mkSelector "getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:") retVoid [argPtr lineFragmentRect, argPtr lineFragmentUsedRect, argNSRange paragraphSeparatorGlyphRange, argNSPoint lineOrigin]

-- | @- setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> NSRange -> IO ()
setHardInvalidation_forGlyphRange nsTypesetter  flag glyphRange =
  sendMsg nsTypesetter (mkSelector "setHardInvalidation:forGlyphRange:") retVoid [argCULong (if flag then 1 else 0), argNSRange glyphRange]

-- | @- layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:@
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex :: (IsNSTypesetter nsTypesetter, IsNSLayoutManager layoutManager) => nsTypesetter -> layoutManager -> CULong -> CULong -> Ptr CULong -> IO ()
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex nsTypesetter  layoutManager startGlyphIndex maxNumLines nextGlyph =
withObjCPtr layoutManager $ \raw_layoutManager ->
    sendMsg nsTypesetter (mkSelector "layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:") retVoid [argPtr (castPtr raw_layoutManager :: Ptr ()), argCULong (fromIntegral startGlyphIndex), argCULong (fromIntegral maxNumLines), argPtr nextGlyph]

-- | @- layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:@
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragments :: (IsNSTypesetter nsTypesetter, IsNSLayoutManager layoutManager) => nsTypesetter -> NSRange -> layoutManager -> CULong -> IO NSRange
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragments nsTypesetter  characterRange layoutManager maxNumLines =
withObjCPtr layoutManager $ \raw_layoutManager ->
    sendMsgStret nsTypesetter (mkSelector "layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:") retNSRange [argNSRange characterRange, argPtr (castPtr raw_layoutManager :: Ptr ()), argCULong (fromIntegral maxNumLines)]

-- | @+ printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:@
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_count :: IsNSLayoutManager layoutMgr => layoutMgr -> NSRange -> Const (Ptr CUChar) -> CULong -> IO NSSize
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_count layoutMgr nominallySpacedGlyphsRange packedGlyphs packedGlyphsCount =
  do
    cls' <- getRequiredClass "NSTypesetter"
    withObjCPtr layoutMgr $ \raw_layoutMgr ->
      sendClassMsgStret cls' (mkSelector "printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:") retNSSize [argPtr (castPtr raw_layoutMgr :: Ptr ()), argNSRange nominallySpacedGlyphsRange, argPtr (unConst packedGlyphs), argCULong (fromIntegral packedGlyphsCount)]

-- | @- baselineOffsetInLayoutManager:glyphIndex:@
baselineOffsetInLayoutManager_glyphIndex :: (IsNSTypesetter nsTypesetter, IsNSLayoutManager layoutMgr) => nsTypesetter -> layoutMgr -> CULong -> IO CDouble
baselineOffsetInLayoutManager_glyphIndex nsTypesetter  layoutMgr glyphIndex =
withObjCPtr layoutMgr $ \raw_layoutMgr ->
    sendMsg nsTypesetter (mkSelector "baselineOffsetInLayoutManager:glyphIndex:") retCDouble [argPtr (castPtr raw_layoutMgr :: Ptr ()), argCULong (fromIntegral glyphIndex)]

-- | @+ sharedSystemTypesetterForBehavior:@
sharedSystemTypesetterForBehavior :: NSTypesetterBehavior -> IO RawId
sharedSystemTypesetterForBehavior behavior =
  do
    cls' <- getRequiredClass "NSTypesetter"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "sharedSystemTypesetterForBehavior:") (retPtr retVoid) [argCLong (coerce behavior)]

-- | @- actionForControlCharacterAtIndex:@
actionForControlCharacterAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO NSTypesetterControlCharacterAction
actionForControlCharacterAtIndex nsTypesetter  charIndex =
  fmap (coerce :: CULong -> NSTypesetterControlCharacterAction) $ sendMsg nsTypesetter (mkSelector "actionForControlCharacterAtIndex:") retCULong [argCULong (fromIntegral charIndex)]

-- | @- insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CUInt -> CULong -> CULong -> IO ()
insertGlyph_atGlyphIndex_characterIndex nsTypesetter  glyph glyphIndex characterIndex =
  sendMsg nsTypesetter (mkSelector "insertGlyph:atGlyphIndex:characterIndex:") retVoid [argCUInt (fromIntegral glyph), argCULong (fromIntegral glyphIndex), argCULong (fromIntegral characterIndex)]

-- | @- deleteGlyphsInRange:@
deleteGlyphsInRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> IO ()
deleteGlyphsInRange nsTypesetter  glyphRange =
  sendMsg nsTypesetter (mkSelector "deleteGlyphsInRange:") retVoid [argNSRange glyphRange]

-- | @- characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> Ptr NSRange -> IO NSRange
characterRangeForGlyphRange_actualGlyphRange nsTypesetter  glyphRange actualGlyphRange =
  sendMsgStret nsTypesetter (mkSelector "characterRangeForGlyphRange:actualGlyphRange:") retNSRange [argNSRange glyphRange, argPtr actualGlyphRange]

-- | @- glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRange -> Ptr NSRange -> IO NSRange
glyphRangeForCharacterRange_actualCharacterRange nsTypesetter  charRange actualCharRange =
  sendMsgStret nsTypesetter (mkSelector "glyphRangeForCharacterRange:actualCharacterRange:") retNSRange [argNSRange charRange, argPtr actualCharRange]

-- | @- getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:@
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfter :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSRect -> Ptr NSRect -> Ptr NSRect -> CULong -> NSRect -> CDouble -> CDouble -> CDouble -> IO ()
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfter nsTypesetter  lineFragmentRect lineFragmentUsedRect remainingRect startingGlyphIndex proposedRect lineSpacing paragraphSpacingBefore paragraphSpacingAfter =
  sendMsg nsTypesetter (mkSelector "getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:") retVoid [argPtr lineFragmentRect, argPtr lineFragmentUsedRect, argPtr remainingRect, argCULong (fromIntegral startingGlyphIndex), argNSRect proposedRect, argCDouble (fromIntegral lineSpacing), argCDouble (fromIntegral paragraphSpacingBefore), argCDouble (fromIntegral paragraphSpacingAfter)]

-- | @- setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
setLineFragmentRect_forGlyphRange_usedRect_baselineOffset :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSRect -> NSRange -> NSRect -> CDouble -> IO ()
setLineFragmentRect_forGlyphRange_usedRect_baselineOffset nsTypesetter  fragmentRect glyphRange usedRect baselineOffset =
  sendMsg nsTypesetter (mkSelector "setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:") retVoid [argNSRect fragmentRect, argNSRange glyphRange, argNSRect usedRect, argCDouble (fromIntegral baselineOffset)]

-- | @- setNotShownAttribute:forGlyphRange:@
setNotShownAttribute_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> NSRange -> IO ()
setNotShownAttribute_forGlyphRange nsTypesetter  flag glyphRange =
  sendMsg nsTypesetter (mkSelector "setNotShownAttribute:forGlyphRange:") retVoid [argCULong (if flag then 1 else 0), argNSRange glyphRange]

-- | @- setDrawsOutsideLineFragment:forGlyphRange:@
setDrawsOutsideLineFragment_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> NSRange -> IO ()
setDrawsOutsideLineFragment_forGlyphRange nsTypesetter  flag glyphRange =
  sendMsg nsTypesetter (mkSelector "setDrawsOutsideLineFragment:forGlyphRange:") retVoid [argCULong (if flag then 1 else 0), argNSRange glyphRange]

-- | @- setLocation:withAdvancements:forStartOfGlyphRange:@
setLocation_withAdvancements_forStartOfGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSPoint -> Const (Ptr CDouble) -> NSRange -> IO ()
setLocation_withAdvancements_forStartOfGlyphRange nsTypesetter  location advancements glyphRange =
  sendMsg nsTypesetter (mkSelector "setLocation:withAdvancements:forStartOfGlyphRange:") retVoid [argNSPoint location, argPtr (unConst advancements), argNSRange glyphRange]

-- | @- setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSSize -> NSRange -> IO ()
setAttachmentSize_forGlyphRange nsTypesetter  attachmentSize glyphRange =
  sendMsg nsTypesetter (mkSelector "setAttachmentSize:forGlyphRange:") retVoid [argNSSize attachmentSize, argNSRange glyphRange]

-- | @- setBidiLevels:forGlyphRange:@
setBidiLevels_forGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> Const (Ptr CUChar) -> NSRange -> IO ()
setBidiLevels_forGlyphRange nsTypesetter  levels glyphRange =
  sendMsg nsTypesetter (mkSelector "setBidiLevels:forGlyphRange:") retVoid [argPtr (unConst levels), argNSRange glyphRange]

-- | @- willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset :: IsNSTypesetter nsTypesetter => nsTypesetter -> Ptr NSRect -> NSRange -> Ptr NSRect -> Ptr CDouble -> IO ()
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset nsTypesetter  lineRect glyphRange usedRect baselineOffset =
  sendMsg nsTypesetter (mkSelector "willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:") retVoid [argPtr lineRect, argNSRange glyphRange, argPtr usedRect, argPtr baselineOffset]

-- | @- shouldBreakLineByWordBeforeCharacterAtIndex:@
shouldBreakLineByWordBeforeCharacterAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO Bool
shouldBreakLineByWordBeforeCharacterAtIndex nsTypesetter  charIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTypesetter (mkSelector "shouldBreakLineByWordBeforeCharacterAtIndex:") retCULong [argCULong (fromIntegral charIndex)]

-- | @- shouldBreakLineByHyphenatingBeforeCharacterAtIndex:@
shouldBreakLineByHyphenatingBeforeCharacterAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO Bool
shouldBreakLineByHyphenatingBeforeCharacterAtIndex nsTypesetter  charIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTypesetter (mkSelector "shouldBreakLineByHyphenatingBeforeCharacterAtIndex:") retCULong [argCULong (fromIntegral charIndex)]

-- | @- hyphenationFactorForGlyphAtIndex:@
hyphenationFactorForGlyphAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO CFloat
hyphenationFactorForGlyphAtIndex nsTypesetter  glyphIndex =
  sendMsg nsTypesetter (mkSelector "hyphenationFactorForGlyphAtIndex:") retCFloat [argCULong (fromIntegral glyphIndex)]

-- | @- hyphenCharacterForGlyphAtIndex:@
hyphenCharacterForGlyphAtIndex :: IsNSTypesetter nsTypesetter => nsTypesetter -> CULong -> IO CUInt
hyphenCharacterForGlyphAtIndex nsTypesetter  glyphIndex =
  sendMsg nsTypesetter (mkSelector "hyphenCharacterForGlyphAtIndex:") retCUInt [argCULong (fromIntegral glyphIndex)]

-- | @- boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:@
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex :: (IsNSTypesetter nsTypesetter, IsNSTextContainer textContainer) => nsTypesetter -> CULong -> textContainer -> NSRect -> NSPoint -> CULong -> IO NSRect
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex nsTypesetter  glyphIndex textContainer proposedRect glyphPosition charIndex =
withObjCPtr textContainer $ \raw_textContainer ->
    sendMsgStret nsTypesetter (mkSelector "boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:") retNSRect [argCULong (fromIntegral glyphIndex), argPtr (castPtr raw_textContainer :: Ptr ()), argNSRect proposedRect, argNSPoint glyphPosition, argCULong (fromIntegral charIndex)]

-- | @- usesFontLeading@
usesFontLeading :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO Bool
usesFontLeading nsTypesetter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTypesetter (mkSelector "usesFontLeading") retCULong []

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> IO ()
setUsesFontLeading nsTypesetter  value =
  sendMsg nsTypesetter (mkSelector "setUsesFontLeading:") retVoid [argCULong (if value then 1 else 0)]

-- | @- typesetterBehavior@
typesetterBehavior :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSTypesetterBehavior
typesetterBehavior nsTypesetter  =
  fmap (coerce :: CLong -> NSTypesetterBehavior) $ sendMsg nsTypesetter (mkSelector "typesetterBehavior") retCLong []

-- | @- setTypesetterBehavior:@
setTypesetterBehavior :: IsNSTypesetter nsTypesetter => nsTypesetter -> NSTypesetterBehavior -> IO ()
setTypesetterBehavior nsTypesetter  value =
  sendMsg nsTypesetter (mkSelector "setTypesetterBehavior:") retVoid [argCLong (coerce value)]

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO CFloat
hyphenationFactor nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "hyphenationFactor") retCFloat []

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSTypesetter nsTypesetter => nsTypesetter -> CFloat -> IO ()
setHyphenationFactor nsTypesetter  value =
  sendMsg nsTypesetter (mkSelector "setHyphenationFactor:") retVoid [argCFloat (fromIntegral value)]

-- | @- lineFragmentPadding@
lineFragmentPadding :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO CDouble
lineFragmentPadding nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "lineFragmentPadding") retCDouble []

-- | @- setLineFragmentPadding:@
setLineFragmentPadding :: IsNSTypesetter nsTypesetter => nsTypesetter -> CDouble -> IO ()
setLineFragmentPadding nsTypesetter  value =
  sendMsg nsTypesetter (mkSelector "setLineFragmentPadding:") retVoid [argCDouble (fromIntegral value)]

-- | @- bidiProcessingEnabled@
bidiProcessingEnabled :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO Bool
bidiProcessingEnabled nsTypesetter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTypesetter (mkSelector "bidiProcessingEnabled") retCULong []

-- | @- setBidiProcessingEnabled:@
setBidiProcessingEnabled :: IsNSTypesetter nsTypesetter => nsTypesetter -> Bool -> IO ()
setBidiProcessingEnabled nsTypesetter  value =
  sendMsg nsTypesetter (mkSelector "setBidiProcessingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- attributedString@
attributedString :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSAttributedString)
attributedString nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedString:@
setAttributedString :: (IsNSTypesetter nsTypesetter, IsNSAttributedString value) => nsTypesetter -> value -> IO ()
setAttributedString nsTypesetter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTypesetter (mkSelector "setAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paragraphGlyphRange@
paragraphGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphGlyphRange nsTypesetter  =
  sendMsgStret nsTypesetter (mkSelector "paragraphGlyphRange") retNSRange []

-- | @- paragraphSeparatorGlyphRange@
paragraphSeparatorGlyphRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphSeparatorGlyphRange nsTypesetter  =
  sendMsgStret nsTypesetter (mkSelector "paragraphSeparatorGlyphRange") retNSRange []

-- | @- paragraphCharacterRange@
paragraphCharacterRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphCharacterRange nsTypesetter  =
  sendMsgStret nsTypesetter (mkSelector "paragraphCharacterRange") retNSRange []

-- | @- paragraphSeparatorCharacterRange@
paragraphSeparatorCharacterRange :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO NSRange
paragraphSeparatorCharacterRange nsTypesetter  =
  sendMsgStret nsTypesetter (mkSelector "paragraphSeparatorCharacterRange") retNSRange []

-- | @- attributesForExtraLineFragment@
attributesForExtraLineFragment :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSDictionary)
attributesForExtraLineFragment nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "attributesForExtraLineFragment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- layoutManager@
layoutManager :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSLayoutManager)
layoutManager nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "layoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textContainers@
textContainers :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSArray)
textContainers nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "textContainers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentTextContainer@
currentTextContainer :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSTextContainer)
currentTextContainer nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "currentTextContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentParagraphStyle@
currentParagraphStyle :: IsNSTypesetter nsTypesetter => nsTypesetter -> IO (Id NSParagraphStyle)
currentParagraphStyle nsTypesetter  =
  sendMsg nsTypesetter (mkSelector "currentParagraphStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sharedSystemTypesetter@
sharedSystemTypesetter :: IO (Id NSTypesetter)
sharedSystemTypesetter  =
  do
    cls' <- getRequiredClass "NSTypesetter"
    sendClassMsg cls' (mkSelector "sharedSystemTypesetter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultTypesetterBehavior@
defaultTypesetterBehavior :: IO NSTypesetterBehavior
defaultTypesetterBehavior  =
  do
    cls' <- getRequiredClass "NSTypesetter"
    fmap (coerce :: CLong -> NSTypesetterBehavior) $ sendClassMsg cls' (mkSelector "defaultTypesetterBehavior") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @substituteFontForFont:@
substituteFontForFontSelector :: Selector
substituteFontForFontSelector = mkSelector "substituteFontForFont:"

-- | @Selector@ for @textTabForGlyphLocation:writingDirection:maxLocation:@
textTabForGlyphLocation_writingDirection_maxLocationSelector :: Selector
textTabForGlyphLocation_writingDirection_maxLocationSelector = mkSelector "textTabForGlyphLocation:writingDirection:maxLocation:"

-- | @Selector@ for @setParagraphGlyphRange:separatorGlyphRange:@
setParagraphGlyphRange_separatorGlyphRangeSelector :: Selector
setParagraphGlyphRange_separatorGlyphRangeSelector = mkSelector "setParagraphGlyphRange:separatorGlyphRange:"

-- | @Selector@ for @layoutParagraphAtPoint:@
layoutParagraphAtPointSelector :: Selector
layoutParagraphAtPointSelector = mkSelector "layoutParagraphAtPoint:"

-- | @Selector@ for @beginParagraph@
beginParagraphSelector :: Selector
beginParagraphSelector = mkSelector "beginParagraph"

-- | @Selector@ for @endParagraph@
endParagraphSelector :: Selector
endParagraphSelector = mkSelector "endParagraph"

-- | @Selector@ for @beginLineWithGlyphAtIndex:@
beginLineWithGlyphAtIndexSelector :: Selector
beginLineWithGlyphAtIndexSelector = mkSelector "beginLineWithGlyphAtIndex:"

-- | @Selector@ for @endLineWithGlyphRange:@
endLineWithGlyphRangeSelector :: Selector
endLineWithGlyphRangeSelector = mkSelector "endLineWithGlyphRange:"

-- | @Selector@ for @lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector :: Selector
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector = mkSelector "getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:"

-- | @Selector@ for @setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRangeSelector :: Selector
setHardInvalidation_forGlyphRangeSelector = mkSelector "setHardInvalidation:forGlyphRange:"

-- | @Selector@ for @layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:@
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector :: Selector
layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndexSelector = mkSelector "layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:"

-- | @Selector@ for @layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:@
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector :: Selector
layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragmentsSelector = mkSelector "layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:"

-- | @Selector@ for @printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:@
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector :: Selector
printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_countSelector = mkSelector "printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:"

-- | @Selector@ for @baselineOffsetInLayoutManager:glyphIndex:@
baselineOffsetInLayoutManager_glyphIndexSelector :: Selector
baselineOffsetInLayoutManager_glyphIndexSelector = mkSelector "baselineOffsetInLayoutManager:glyphIndex:"

-- | @Selector@ for @sharedSystemTypesetterForBehavior:@
sharedSystemTypesetterForBehaviorSelector :: Selector
sharedSystemTypesetterForBehaviorSelector = mkSelector "sharedSystemTypesetterForBehavior:"

-- | @Selector@ for @actionForControlCharacterAtIndex:@
actionForControlCharacterAtIndexSelector :: Selector
actionForControlCharacterAtIndexSelector = mkSelector "actionForControlCharacterAtIndex:"

-- | @Selector@ for @insertGlyph:atGlyphIndex:characterIndex:@
insertGlyph_atGlyphIndex_characterIndexSelector :: Selector
insertGlyph_atGlyphIndex_characterIndexSelector = mkSelector "insertGlyph:atGlyphIndex:characterIndex:"

-- | @Selector@ for @deleteGlyphsInRange:@
deleteGlyphsInRangeSelector :: Selector
deleteGlyphsInRangeSelector = mkSelector "deleteGlyphsInRange:"

-- | @Selector@ for @characterRangeForGlyphRange:actualGlyphRange:@
characterRangeForGlyphRange_actualGlyphRangeSelector :: Selector
characterRangeForGlyphRange_actualGlyphRangeSelector = mkSelector "characterRangeForGlyphRange:actualGlyphRange:"

-- | @Selector@ for @glyphRangeForCharacterRange:actualCharacterRange:@
glyphRangeForCharacterRange_actualCharacterRangeSelector :: Selector
glyphRangeForCharacterRange_actualCharacterRangeSelector = mkSelector "glyphRangeForCharacterRange:actualCharacterRange:"

-- | @Selector@ for @getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:@
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector :: Selector
getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfterSelector = mkSelector "getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:"

-- | @Selector@ for @setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector :: Selector
setLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector = mkSelector "setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:"

-- | @Selector@ for @setNotShownAttribute:forGlyphRange:@
setNotShownAttribute_forGlyphRangeSelector :: Selector
setNotShownAttribute_forGlyphRangeSelector = mkSelector "setNotShownAttribute:forGlyphRange:"

-- | @Selector@ for @setDrawsOutsideLineFragment:forGlyphRange:@
setDrawsOutsideLineFragment_forGlyphRangeSelector :: Selector
setDrawsOutsideLineFragment_forGlyphRangeSelector = mkSelector "setDrawsOutsideLineFragment:forGlyphRange:"

-- | @Selector@ for @setLocation:withAdvancements:forStartOfGlyphRange:@
setLocation_withAdvancements_forStartOfGlyphRangeSelector :: Selector
setLocation_withAdvancements_forStartOfGlyphRangeSelector = mkSelector "setLocation:withAdvancements:forStartOfGlyphRange:"

-- | @Selector@ for @setAttachmentSize:forGlyphRange:@
setAttachmentSize_forGlyphRangeSelector :: Selector
setAttachmentSize_forGlyphRangeSelector = mkSelector "setAttachmentSize:forGlyphRange:"

-- | @Selector@ for @setBidiLevels:forGlyphRange:@
setBidiLevels_forGlyphRangeSelector :: Selector
setBidiLevels_forGlyphRangeSelector = mkSelector "setBidiLevels:forGlyphRange:"

-- | @Selector@ for @willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector :: Selector
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector = mkSelector "willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:"

-- | @Selector@ for @shouldBreakLineByWordBeforeCharacterAtIndex:@
shouldBreakLineByWordBeforeCharacterAtIndexSelector :: Selector
shouldBreakLineByWordBeforeCharacterAtIndexSelector = mkSelector "shouldBreakLineByWordBeforeCharacterAtIndex:"

-- | @Selector@ for @shouldBreakLineByHyphenatingBeforeCharacterAtIndex:@
shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector :: Selector
shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector = mkSelector "shouldBreakLineByHyphenatingBeforeCharacterAtIndex:"

-- | @Selector@ for @hyphenationFactorForGlyphAtIndex:@
hyphenationFactorForGlyphAtIndexSelector :: Selector
hyphenationFactorForGlyphAtIndexSelector = mkSelector "hyphenationFactorForGlyphAtIndex:"

-- | @Selector@ for @hyphenCharacterForGlyphAtIndex:@
hyphenCharacterForGlyphAtIndexSelector :: Selector
hyphenCharacterForGlyphAtIndexSelector = mkSelector "hyphenCharacterForGlyphAtIndex:"

-- | @Selector@ for @boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:@
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector :: Selector
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector = mkSelector "boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:"

-- | @Selector@ for @usesFontLeading@
usesFontLeadingSelector :: Selector
usesFontLeadingSelector = mkSelector "usesFontLeading"

-- | @Selector@ for @setUsesFontLeading:@
setUsesFontLeadingSelector :: Selector
setUsesFontLeadingSelector = mkSelector "setUsesFontLeading:"

-- | @Selector@ for @typesetterBehavior@
typesetterBehaviorSelector :: Selector
typesetterBehaviorSelector = mkSelector "typesetterBehavior"

-- | @Selector@ for @setTypesetterBehavior:@
setTypesetterBehaviorSelector :: Selector
setTypesetterBehaviorSelector = mkSelector "setTypesetterBehavior:"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @setHyphenationFactor:@
setHyphenationFactorSelector :: Selector
setHyphenationFactorSelector = mkSelector "setHyphenationFactor:"

-- | @Selector@ for @lineFragmentPadding@
lineFragmentPaddingSelector :: Selector
lineFragmentPaddingSelector = mkSelector "lineFragmentPadding"

-- | @Selector@ for @setLineFragmentPadding:@
setLineFragmentPaddingSelector :: Selector
setLineFragmentPaddingSelector = mkSelector "setLineFragmentPadding:"

-- | @Selector@ for @bidiProcessingEnabled@
bidiProcessingEnabledSelector :: Selector
bidiProcessingEnabledSelector = mkSelector "bidiProcessingEnabled"

-- | @Selector@ for @setBidiProcessingEnabled:@
setBidiProcessingEnabledSelector :: Selector
setBidiProcessingEnabledSelector = mkSelector "setBidiProcessingEnabled:"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @setAttributedString:@
setAttributedStringSelector :: Selector
setAttributedStringSelector = mkSelector "setAttributedString:"

-- | @Selector@ for @paragraphGlyphRange@
paragraphGlyphRangeSelector :: Selector
paragraphGlyphRangeSelector = mkSelector "paragraphGlyphRange"

-- | @Selector@ for @paragraphSeparatorGlyphRange@
paragraphSeparatorGlyphRangeSelector :: Selector
paragraphSeparatorGlyphRangeSelector = mkSelector "paragraphSeparatorGlyphRange"

-- | @Selector@ for @paragraphCharacterRange@
paragraphCharacterRangeSelector :: Selector
paragraphCharacterRangeSelector = mkSelector "paragraphCharacterRange"

-- | @Selector@ for @paragraphSeparatorCharacterRange@
paragraphSeparatorCharacterRangeSelector :: Selector
paragraphSeparatorCharacterRangeSelector = mkSelector "paragraphSeparatorCharacterRange"

-- | @Selector@ for @attributesForExtraLineFragment@
attributesForExtraLineFragmentSelector :: Selector
attributesForExtraLineFragmentSelector = mkSelector "attributesForExtraLineFragment"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @textContainers@
textContainersSelector :: Selector
textContainersSelector = mkSelector "textContainers"

-- | @Selector@ for @currentTextContainer@
currentTextContainerSelector :: Selector
currentTextContainerSelector = mkSelector "currentTextContainer"

-- | @Selector@ for @currentParagraphStyle@
currentParagraphStyleSelector :: Selector
currentParagraphStyleSelector = mkSelector "currentParagraphStyle"

-- | @Selector@ for @sharedSystemTypesetter@
sharedSystemTypesetterSelector :: Selector
sharedSystemTypesetterSelector = mkSelector "sharedSystemTypesetter"

-- | @Selector@ for @defaultTypesetterBehavior@
defaultTypesetterBehaviorSelector :: Selector
defaultTypesetterBehaviorSelector = mkSelector "defaultTypesetterBehavior"

