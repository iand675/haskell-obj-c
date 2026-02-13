{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSATSTypesetter@.
module ObjC.AppKit.NSATSTypesetter
  ( NSATSTypesetter
  , IsNSATSTypesetter(..)
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits
  , willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset
  , shouldBreakLineByWordBeforeCharacterAtIndex
  , shouldBreakLineByHyphenatingBeforeCharacterAtIndex
  , hyphenationFactorForGlyphAtIndex
  , hyphenCharacterForGlyphAtIndex
  , boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex
  , substituteFontForFont
  , textTabForGlyphLocation_writingDirection_maxLocation
  , setParagraphGlyphRange_separatorGlyphRange
  , layoutParagraphAtPoint
  , lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect
  , paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect
  , paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect
  , setHardInvalidation_forGlyphRange
  , getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin
  , lineFragmentRectForProposedRect_remainingRect
  , sharedTypesetter
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
  , layoutManager
  , currentTextContainer
  , attributedStringSelector
  , bidiProcessingEnabledSelector
  , boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector
  , currentTextContainerSelector
  , getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector
  , getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector
  , hyphenCharacterForGlyphAtIndexSelector
  , hyphenationFactorForGlyphAtIndexSelector
  , hyphenationFactorSelector
  , layoutManagerSelector
  , layoutParagraphAtPointSelector
  , lineFragmentPaddingSelector
  , lineFragmentRectForProposedRect_remainingRectSelector
  , lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphGlyphRangeSelector
  , paragraphSeparatorGlyphRangeSelector
  , paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector
  , setAttributedStringSelector
  , setBidiProcessingEnabledSelector
  , setHardInvalidation_forGlyphRangeSelector
  , setHyphenationFactorSelector
  , setLineFragmentPaddingSelector
  , setParagraphGlyphRange_separatorGlyphRangeSelector
  , setTypesetterBehaviorSelector
  , setUsesFontLeadingSelector
  , sharedTypesetterSelector
  , shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector
  , shouldBreakLineByWordBeforeCharacterAtIndexSelector
  , substituteFontForFontSelector
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

-- | @- getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSRange -> RawId -> Ptr CULong -> Ptr NSGlyphInscription -> Ptr Bool -> IO CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits nsatsTypesetter glyphsRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer =
  sendMessage nsatsTypesetter getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector glyphsRange glyphBuffer charIndexBuffer inscribeBuffer elasticBuffer

-- | @- willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Ptr NSRect -> NSRange -> Ptr NSRect -> Ptr CDouble -> IO ()
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset nsatsTypesetter lineRect glyphRange usedRect baselineOffset =
  sendMessage nsatsTypesetter willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector lineRect glyphRange usedRect baselineOffset

-- | @- shouldBreakLineByWordBeforeCharacterAtIndex:@
shouldBreakLineByWordBeforeCharacterAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO Bool
shouldBreakLineByWordBeforeCharacterAtIndex nsatsTypesetter charIndex =
  sendMessage nsatsTypesetter shouldBreakLineByWordBeforeCharacterAtIndexSelector charIndex

-- | @- shouldBreakLineByHyphenatingBeforeCharacterAtIndex:@
shouldBreakLineByHyphenatingBeforeCharacterAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO Bool
shouldBreakLineByHyphenatingBeforeCharacterAtIndex nsatsTypesetter charIndex =
  sendMessage nsatsTypesetter shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector charIndex

-- | @- hyphenationFactorForGlyphAtIndex:@
hyphenationFactorForGlyphAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO CFloat
hyphenationFactorForGlyphAtIndex nsatsTypesetter glyphIndex =
  sendMessage nsatsTypesetter hyphenationFactorForGlyphAtIndexSelector glyphIndex

-- | @- hyphenCharacterForGlyphAtIndex:@
hyphenCharacterForGlyphAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO CUInt
hyphenCharacterForGlyphAtIndex nsatsTypesetter glyphIndex =
  sendMessage nsatsTypesetter hyphenCharacterForGlyphAtIndexSelector glyphIndex

-- | @- boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:@
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex :: (IsNSATSTypesetter nsatsTypesetter, IsNSTextContainer textContainer) => nsatsTypesetter -> CULong -> textContainer -> NSRect -> NSPoint -> CULong -> IO NSRect
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex nsatsTypesetter glyphIndex textContainer proposedRect glyphPosition charIndex =
  sendMessage nsatsTypesetter boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector glyphIndex (toNSTextContainer textContainer) proposedRect glyphPosition charIndex

-- | @- substituteFontForFont:@
substituteFontForFont :: (IsNSATSTypesetter nsatsTypesetter, IsNSFont originalFont) => nsatsTypesetter -> originalFont -> IO (Id NSFont)
substituteFontForFont nsatsTypesetter originalFont =
  sendMessage nsatsTypesetter substituteFontForFontSelector (toNSFont originalFont)

-- | @- textTabForGlyphLocation:writingDirection:maxLocation:@
textTabForGlyphLocation_writingDirection_maxLocation :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CDouble -> NSWritingDirection -> CDouble -> IO (Id NSTextTab)
textTabForGlyphLocation_writingDirection_maxLocation nsatsTypesetter glyphLocation direction maxLocation =
  sendMessage nsatsTypesetter textTabForGlyphLocation_writingDirection_maxLocationSelector glyphLocation direction maxLocation

-- | @- setParagraphGlyphRange:separatorGlyphRange:@
setParagraphGlyphRange_separatorGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSRange -> NSRange -> IO ()
setParagraphGlyphRange_separatorGlyphRange nsatsTypesetter paragraphRange paragraphSeparatorRange =
  sendMessage nsatsTypesetter setParagraphGlyphRange_separatorGlyphRangeSelector paragraphRange paragraphSeparatorRange

-- | @- layoutParagraphAtPoint:@
layoutParagraphAtPoint :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Ptr NSPoint -> IO CULong
layoutParagraphAtPoint nsatsTypesetter lineFragmentOrigin =
  sendMessage nsatsTypesetter layoutParagraphAtPointSelector lineFragmentOrigin

-- | @- lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> NSRect -> IO CDouble
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsatsTypesetter glyphIndex rect =
  sendMessage nsatsTypesetter lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector glyphIndex rect

-- | @- paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect nsatsTypesetter glyphIndex rect =
  sendMessage nsatsTypesetter paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector glyphIndex rect

-- | @- paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsatsTypesetter glyphIndex rect =
  sendMessage nsatsTypesetter paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector glyphIndex rect

-- | @- setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Bool -> NSRange -> IO ()
setHardInvalidation_forGlyphRange nsatsTypesetter flag glyphRange =
  sendMessage nsatsTypesetter setHardInvalidation_forGlyphRangeSelector flag glyphRange

-- | @- getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Ptr NSRect -> Ptr NSRect -> NSRange -> NSPoint -> IO ()
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin nsatsTypesetter lineFragmentRect lineFragmentUsedRect paragraphSeparatorGlyphRange lineOrigin =
  sendMessage nsatsTypesetter getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector lineFragmentRect lineFragmentUsedRect paragraphSeparatorGlyphRange lineOrigin

-- | @- lineFragmentRectForProposedRect:remainingRect:@
lineFragmentRectForProposedRect_remainingRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSRect -> Ptr NSRect -> IO NSRect
lineFragmentRectForProposedRect_remainingRect nsatsTypesetter proposedRect remainingRect =
  sendMessage nsatsTypesetter lineFragmentRectForProposedRect_remainingRectSelector proposedRect remainingRect

-- | @+ sharedTypesetter@
sharedTypesetter :: IO (Id NSATSTypesetter)
sharedTypesetter  =
  do
    cls' <- getRequiredClass "NSATSTypesetter"
    sendClassMessage cls' sharedTypesetterSelector

-- | @- usesFontLeading@
usesFontLeading :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO Bool
usesFontLeading nsatsTypesetter =
  sendMessage nsatsTypesetter usesFontLeadingSelector

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Bool -> IO ()
setUsesFontLeading nsatsTypesetter value =
  sendMessage nsatsTypesetter setUsesFontLeadingSelector value

-- | @- typesetterBehavior@
typesetterBehavior :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO NSTypesetterBehavior
typesetterBehavior nsatsTypesetter =
  sendMessage nsatsTypesetter typesetterBehaviorSelector

-- | @- setTypesetterBehavior:@
setTypesetterBehavior :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSTypesetterBehavior -> IO ()
setTypesetterBehavior nsatsTypesetter value =
  sendMessage nsatsTypesetter setTypesetterBehaviorSelector value

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO CFloat
hyphenationFactor nsatsTypesetter =
  sendMessage nsatsTypesetter hyphenationFactorSelector

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CFloat -> IO ()
setHyphenationFactor nsatsTypesetter value =
  sendMessage nsatsTypesetter setHyphenationFactorSelector value

-- | @- lineFragmentPadding@
lineFragmentPadding :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO CDouble
lineFragmentPadding nsatsTypesetter =
  sendMessage nsatsTypesetter lineFragmentPaddingSelector

-- | @- setLineFragmentPadding:@
setLineFragmentPadding :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CDouble -> IO ()
setLineFragmentPadding nsatsTypesetter value =
  sendMessage nsatsTypesetter setLineFragmentPaddingSelector value

-- | @- bidiProcessingEnabled@
bidiProcessingEnabled :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO Bool
bidiProcessingEnabled nsatsTypesetter =
  sendMessage nsatsTypesetter bidiProcessingEnabledSelector

-- | @- setBidiProcessingEnabled:@
setBidiProcessingEnabled :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Bool -> IO ()
setBidiProcessingEnabled nsatsTypesetter value =
  sendMessage nsatsTypesetter setBidiProcessingEnabledSelector value

-- | @- attributedString@
attributedString :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO (Id NSAttributedString)
attributedString nsatsTypesetter =
  sendMessage nsatsTypesetter attributedStringSelector

-- | @- setAttributedString:@
setAttributedString :: (IsNSATSTypesetter nsatsTypesetter, IsNSAttributedString value) => nsatsTypesetter -> value -> IO ()
setAttributedString nsatsTypesetter value =
  sendMessage nsatsTypesetter setAttributedStringSelector (toNSAttributedString value)

-- | @- paragraphGlyphRange@
paragraphGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO NSRange
paragraphGlyphRange nsatsTypesetter =
  sendMessage nsatsTypesetter paragraphGlyphRangeSelector

-- | @- paragraphSeparatorGlyphRange@
paragraphSeparatorGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO NSRange
paragraphSeparatorGlyphRange nsatsTypesetter =
  sendMessage nsatsTypesetter paragraphSeparatorGlyphRangeSelector

-- | @- layoutManager@
layoutManager :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO (Id NSLayoutManager)
layoutManager nsatsTypesetter =
  sendMessage nsatsTypesetter layoutManagerSelector

-- | @- currentTextContainer@
currentTextContainer :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO (Id NSTextContainer)
currentTextContainer nsatsTypesetter =
  sendMessage nsatsTypesetter currentTextContainerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:@
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector :: Selector '[NSRange, RawId, Ptr CULong, Ptr NSGlyphInscription, Ptr Bool] CULong
getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBitsSelector = mkSelector "getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:"

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

-- | @Selector@ for @lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector '[CULong, NSRect] CDouble
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector '[CULong, NSRect] CDouble
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector '[CULong, NSRect] CDouble
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRangeSelector :: Selector '[Bool, NSRange] ()
setHardInvalidation_forGlyphRangeSelector = mkSelector "setHardInvalidation:forGlyphRange:"

-- | @Selector@ for @getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector :: Selector '[Ptr NSRect, Ptr NSRect, NSRange, NSPoint] ()
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector = mkSelector "getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:"

-- | @Selector@ for @lineFragmentRectForProposedRect:remainingRect:@
lineFragmentRectForProposedRect_remainingRectSelector :: Selector '[NSRect, Ptr NSRect] NSRect
lineFragmentRectForProposedRect_remainingRectSelector = mkSelector "lineFragmentRectForProposedRect:remainingRect:"

-- | @Selector@ for @sharedTypesetter@
sharedTypesetterSelector :: Selector '[] (Id NSATSTypesetter)
sharedTypesetterSelector = mkSelector "sharedTypesetter"

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

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector '[] (Id NSLayoutManager)
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @currentTextContainer@
currentTextContainerSelector :: Selector '[] (Id NSTextContainer)
currentTextContainerSelector = mkSelector "currentTextContainer"

