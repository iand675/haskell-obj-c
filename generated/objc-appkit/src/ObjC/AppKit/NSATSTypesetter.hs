{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSATSTypesetter@.
module ObjC.AppKit.NSATSTypesetter
  ( NSATSTypesetter
  , IsNSATSTypesetter(..)
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
  , willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffsetSelector
  , shouldBreakLineByWordBeforeCharacterAtIndexSelector
  , shouldBreakLineByHyphenatingBeforeCharacterAtIndexSelector
  , hyphenationFactorForGlyphAtIndexSelector
  , hyphenCharacterForGlyphAtIndexSelector
  , boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndexSelector
  , substituteFontForFontSelector
  , textTabForGlyphLocation_writingDirection_maxLocationSelector
  , setParagraphGlyphRange_separatorGlyphRangeSelector
  , layoutParagraphAtPointSelector
  , lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector
  , paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector
  , setHardInvalidation_forGlyphRangeSelector
  , getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector
  , lineFragmentRectForProposedRect_remainingRectSelector
  , sharedTypesetterSelector
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
  , layoutManagerSelector
  , currentTextContainerSelector

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

-- | @- willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:@
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Ptr NSRect -> NSRange -> Ptr NSRect -> Ptr CDouble -> IO ()
willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset nsatsTypesetter  lineRect glyphRange usedRect baselineOffset =
  sendMsg nsatsTypesetter (mkSelector "willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:") retVoid [argPtr lineRect, argNSRange glyphRange, argPtr usedRect, argPtr baselineOffset]

-- | @- shouldBreakLineByWordBeforeCharacterAtIndex:@
shouldBreakLineByWordBeforeCharacterAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO Bool
shouldBreakLineByWordBeforeCharacterAtIndex nsatsTypesetter  charIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsatsTypesetter (mkSelector "shouldBreakLineByWordBeforeCharacterAtIndex:") retCULong [argCULong (fromIntegral charIndex)]

-- | @- shouldBreakLineByHyphenatingBeforeCharacterAtIndex:@
shouldBreakLineByHyphenatingBeforeCharacterAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO Bool
shouldBreakLineByHyphenatingBeforeCharacterAtIndex nsatsTypesetter  charIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsatsTypesetter (mkSelector "shouldBreakLineByHyphenatingBeforeCharacterAtIndex:") retCULong [argCULong (fromIntegral charIndex)]

-- | @- hyphenationFactorForGlyphAtIndex:@
hyphenationFactorForGlyphAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO CFloat
hyphenationFactorForGlyphAtIndex nsatsTypesetter  glyphIndex =
  sendMsg nsatsTypesetter (mkSelector "hyphenationFactorForGlyphAtIndex:") retCFloat [argCULong (fromIntegral glyphIndex)]

-- | @- hyphenCharacterForGlyphAtIndex:@
hyphenCharacterForGlyphAtIndex :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> IO CUInt
hyphenCharacterForGlyphAtIndex nsatsTypesetter  glyphIndex =
  sendMsg nsatsTypesetter (mkSelector "hyphenCharacterForGlyphAtIndex:") retCUInt [argCULong (fromIntegral glyphIndex)]

-- | @- boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:@
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex :: (IsNSATSTypesetter nsatsTypesetter, IsNSTextContainer textContainer) => nsatsTypesetter -> CULong -> textContainer -> NSRect -> NSPoint -> CULong -> IO NSRect
boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex nsatsTypesetter  glyphIndex textContainer proposedRect glyphPosition charIndex =
withObjCPtr textContainer $ \raw_textContainer ->
    sendMsgStret nsatsTypesetter (mkSelector "boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:") retNSRect [argCULong (fromIntegral glyphIndex), argPtr (castPtr raw_textContainer :: Ptr ()), argNSRect proposedRect, argNSPoint glyphPosition, argCULong (fromIntegral charIndex)]

-- | @- substituteFontForFont:@
substituteFontForFont :: (IsNSATSTypesetter nsatsTypesetter, IsNSFont originalFont) => nsatsTypesetter -> originalFont -> IO (Id NSFont)
substituteFontForFont nsatsTypesetter  originalFont =
withObjCPtr originalFont $ \raw_originalFont ->
    sendMsg nsatsTypesetter (mkSelector "substituteFontForFont:") (retPtr retVoid) [argPtr (castPtr raw_originalFont :: Ptr ())] >>= retainedObject . castPtr

-- | @- textTabForGlyphLocation:writingDirection:maxLocation:@
textTabForGlyphLocation_writingDirection_maxLocation :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CDouble -> NSWritingDirection -> CDouble -> IO (Id NSTextTab)
textTabForGlyphLocation_writingDirection_maxLocation nsatsTypesetter  glyphLocation direction maxLocation =
  sendMsg nsatsTypesetter (mkSelector "textTabForGlyphLocation:writingDirection:maxLocation:") (retPtr retVoid) [argCDouble (fromIntegral glyphLocation), argCLong (coerce direction), argCDouble (fromIntegral maxLocation)] >>= retainedObject . castPtr

-- | @- setParagraphGlyphRange:separatorGlyphRange:@
setParagraphGlyphRange_separatorGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSRange -> NSRange -> IO ()
setParagraphGlyphRange_separatorGlyphRange nsatsTypesetter  paragraphRange paragraphSeparatorRange =
  sendMsg nsatsTypesetter (mkSelector "setParagraphGlyphRange:separatorGlyphRange:") retVoid [argNSRange paragraphRange, argNSRange paragraphSeparatorRange]

-- | @- layoutParagraphAtPoint:@
layoutParagraphAtPoint :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Ptr NSPoint -> IO CULong
layoutParagraphAtPoint nsatsTypesetter  lineFragmentOrigin =
  sendMsg nsatsTypesetter (mkSelector "layoutParagraphAtPoint:") retCULong [argPtr lineFragmentOrigin]

-- | @- lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> NSRect -> IO CDouble
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsatsTypesetter  glyphIndex rect =
  sendMsg nsatsTypesetter (mkSelector "lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:") retCDouble [argCULong (fromIntegral glyphIndex), argNSRect rect]

-- | @- paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect nsatsTypesetter  glyphIndex rect =
  sendMsg nsatsTypesetter (mkSelector "paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:") retCDouble [argCULong (fromIntegral glyphIndex), argNSRect rect]

-- | @- paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CULong -> NSRect -> IO CDouble
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect nsatsTypesetter  glyphIndex rect =
  sendMsg nsatsTypesetter (mkSelector "paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:") retCDouble [argCULong (fromIntegral glyphIndex), argNSRect rect]

-- | @- setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Bool -> NSRange -> IO ()
setHardInvalidation_forGlyphRange nsatsTypesetter  flag glyphRange =
  sendMsg nsatsTypesetter (mkSelector "setHardInvalidation:forGlyphRange:") retVoid [argCULong (if flag then 1 else 0), argNSRange glyphRange]

-- | @- getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Ptr NSRect -> Ptr NSRect -> NSRange -> NSPoint -> IO ()
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin nsatsTypesetter  lineFragmentRect lineFragmentUsedRect paragraphSeparatorGlyphRange lineOrigin =
  sendMsg nsatsTypesetter (mkSelector "getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:") retVoid [argPtr lineFragmentRect, argPtr lineFragmentUsedRect, argNSRange paragraphSeparatorGlyphRange, argNSPoint lineOrigin]

-- | @- lineFragmentRectForProposedRect:remainingRect:@
lineFragmentRectForProposedRect_remainingRect :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSRect -> Ptr NSRect -> IO NSRect
lineFragmentRectForProposedRect_remainingRect nsatsTypesetter  proposedRect remainingRect =
  sendMsgStret nsatsTypesetter (mkSelector "lineFragmentRectForProposedRect:remainingRect:") retNSRect [argNSRect proposedRect, argPtr remainingRect]

-- | @+ sharedTypesetter@
sharedTypesetter :: IO (Id NSATSTypesetter)
sharedTypesetter  =
  do
    cls' <- getRequiredClass "NSATSTypesetter"
    sendClassMsg cls' (mkSelector "sharedTypesetter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- usesFontLeading@
usesFontLeading :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO Bool
usesFontLeading nsatsTypesetter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsatsTypesetter (mkSelector "usesFontLeading") retCULong []

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Bool -> IO ()
setUsesFontLeading nsatsTypesetter  value =
  sendMsg nsatsTypesetter (mkSelector "setUsesFontLeading:") retVoid [argCULong (if value then 1 else 0)]

-- | @- typesetterBehavior@
typesetterBehavior :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO NSTypesetterBehavior
typesetterBehavior nsatsTypesetter  =
  fmap (coerce :: CLong -> NSTypesetterBehavior) $ sendMsg nsatsTypesetter (mkSelector "typesetterBehavior") retCLong []

-- | @- setTypesetterBehavior:@
setTypesetterBehavior :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> NSTypesetterBehavior -> IO ()
setTypesetterBehavior nsatsTypesetter  value =
  sendMsg nsatsTypesetter (mkSelector "setTypesetterBehavior:") retVoid [argCLong (coerce value)]

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO CFloat
hyphenationFactor nsatsTypesetter  =
  sendMsg nsatsTypesetter (mkSelector "hyphenationFactor") retCFloat []

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CFloat -> IO ()
setHyphenationFactor nsatsTypesetter  value =
  sendMsg nsatsTypesetter (mkSelector "setHyphenationFactor:") retVoid [argCFloat (fromIntegral value)]

-- | @- lineFragmentPadding@
lineFragmentPadding :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO CDouble
lineFragmentPadding nsatsTypesetter  =
  sendMsg nsatsTypesetter (mkSelector "lineFragmentPadding") retCDouble []

-- | @- setLineFragmentPadding:@
setLineFragmentPadding :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> CDouble -> IO ()
setLineFragmentPadding nsatsTypesetter  value =
  sendMsg nsatsTypesetter (mkSelector "setLineFragmentPadding:") retVoid [argCDouble (fromIntegral value)]

-- | @- bidiProcessingEnabled@
bidiProcessingEnabled :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO Bool
bidiProcessingEnabled nsatsTypesetter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsatsTypesetter (mkSelector "bidiProcessingEnabled") retCULong []

-- | @- setBidiProcessingEnabled:@
setBidiProcessingEnabled :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> Bool -> IO ()
setBidiProcessingEnabled nsatsTypesetter  value =
  sendMsg nsatsTypesetter (mkSelector "setBidiProcessingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- attributedString@
attributedString :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO (Id NSAttributedString)
attributedString nsatsTypesetter  =
  sendMsg nsatsTypesetter (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedString:@
setAttributedString :: (IsNSATSTypesetter nsatsTypesetter, IsNSAttributedString value) => nsatsTypesetter -> value -> IO ()
setAttributedString nsatsTypesetter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsatsTypesetter (mkSelector "setAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paragraphGlyphRange@
paragraphGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO NSRange
paragraphGlyphRange nsatsTypesetter  =
  sendMsgStret nsatsTypesetter (mkSelector "paragraphGlyphRange") retNSRange []

-- | @- paragraphSeparatorGlyphRange@
paragraphSeparatorGlyphRange :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO NSRange
paragraphSeparatorGlyphRange nsatsTypesetter  =
  sendMsgStret nsatsTypesetter (mkSelector "paragraphSeparatorGlyphRange") retNSRange []

-- | @- layoutManager@
layoutManager :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO (Id NSLayoutManager)
layoutManager nsatsTypesetter  =
  sendMsg nsatsTypesetter (mkSelector "layoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentTextContainer@
currentTextContainer :: IsNSATSTypesetter nsatsTypesetter => nsatsTypesetter -> IO (Id NSTextContainer)
currentTextContainer nsatsTypesetter  =
  sendMsg nsatsTypesetter (mkSelector "currentTextContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector
lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector
paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:@
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector :: Selector
paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRectSelector = mkSelector "paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:"

-- | @Selector@ for @setHardInvalidation:forGlyphRange:@
setHardInvalidation_forGlyphRangeSelector :: Selector
setHardInvalidation_forGlyphRangeSelector = mkSelector "setHardInvalidation:forGlyphRange:"

-- | @Selector@ for @getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:@
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector :: Selector
getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOriginSelector = mkSelector "getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:"

-- | @Selector@ for @lineFragmentRectForProposedRect:remainingRect:@
lineFragmentRectForProposedRect_remainingRectSelector :: Selector
lineFragmentRectForProposedRect_remainingRectSelector = mkSelector "lineFragmentRectForProposedRect:remainingRect:"

-- | @Selector@ for @sharedTypesetter@
sharedTypesetterSelector :: Selector
sharedTypesetterSelector = mkSelector "sharedTypesetter"

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

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @currentTextContainer@
currentTextContainerSelector :: Selector
currentTextContainerSelector = mkSelector "currentTextContainer"

