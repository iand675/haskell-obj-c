{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSParagraphStyle@.
module ObjC.AppKit.NSParagraphStyle
  ( NSParagraphStyle
  , IsNSParagraphStyle(..)
  , defaultWritingDirectionForLanguage
  , defaultParagraphStyle
  , lineSpacing
  , paragraphSpacing
  , headIndent
  , tailIndent
  , firstLineHeadIndent
  , minimumLineHeight
  , maximumLineHeight
  , lineBreakMode
  , baseWritingDirection
  , lineHeightMultiple
  , paragraphSpacingBefore
  , hyphenationFactor
  , usesDefaultHyphenation
  , tabStops
  , defaultTabInterval
  , textLists
  , allowsDefaultTighteningForTruncation
  , lineBreakStrategy
  , alignment
  , tighteningFactorForTruncation
  , textBlocks
  , headerLevel
  , alignmentSelector
  , allowsDefaultTighteningForTruncationSelector
  , baseWritingDirectionSelector
  , defaultParagraphStyleSelector
  , defaultTabIntervalSelector
  , defaultWritingDirectionForLanguageSelector
  , firstLineHeadIndentSelector
  , headIndentSelector
  , headerLevelSelector
  , hyphenationFactorSelector
  , lineBreakModeSelector
  , lineBreakStrategySelector
  , lineHeightMultipleSelector
  , lineSpacingSelector
  , maximumLineHeightSelector
  , minimumLineHeightSelector
  , paragraphSpacingBeforeSelector
  , paragraphSpacingSelector
  , tabStopsSelector
  , tailIndentSelector
  , textBlocksSelector
  , textListsSelector
  , tighteningFactorForTruncationSelector
  , usesDefaultHyphenationSelector

  -- * Enum types
  , NSLineBreakMode(NSLineBreakMode)
  , pattern NSLineBreakByWordWrapping
  , pattern NSLineBreakByCharWrapping
  , pattern NSLineBreakByClipping
  , pattern NSLineBreakByTruncatingHead
  , pattern NSLineBreakByTruncatingTail
  , pattern NSLineBreakByTruncatingMiddle
  , NSLineBreakStrategy(NSLineBreakStrategy)
  , pattern NSLineBreakStrategyNone
  , pattern NSLineBreakStrategyPushOut
  , pattern NSLineBreakStrategyHangulWordPriority
  , pattern NSLineBreakStrategyStandard
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultWritingDirectionForLanguage:@
defaultWritingDirectionForLanguage :: IsNSString languageName => languageName -> IO NSWritingDirection
defaultWritingDirectionForLanguage languageName =
  do
    cls' <- getRequiredClass "NSParagraphStyle"
    sendClassMessage cls' defaultWritingDirectionForLanguageSelector (toNSString languageName)

-- | @+ defaultParagraphStyle@
defaultParagraphStyle :: IO (Id NSParagraphStyle)
defaultParagraphStyle  =
  do
    cls' <- getRequiredClass "NSParagraphStyle"
    sendClassMessage cls' defaultParagraphStyleSelector

-- | @- lineSpacing@
lineSpacing :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
lineSpacing nsParagraphStyle =
  sendMessage nsParagraphStyle lineSpacingSelector

-- | @- paragraphSpacing@
paragraphSpacing :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
paragraphSpacing nsParagraphStyle =
  sendMessage nsParagraphStyle paragraphSpacingSelector

-- | @- headIndent@
headIndent :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
headIndent nsParagraphStyle =
  sendMessage nsParagraphStyle headIndentSelector

-- | @- tailIndent@
tailIndent :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
tailIndent nsParagraphStyle =
  sendMessage nsParagraphStyle tailIndentSelector

-- | @- firstLineHeadIndent@
firstLineHeadIndent :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
firstLineHeadIndent nsParagraphStyle =
  sendMessage nsParagraphStyle firstLineHeadIndentSelector

-- | @- minimumLineHeight@
minimumLineHeight :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
minimumLineHeight nsParagraphStyle =
  sendMessage nsParagraphStyle minimumLineHeightSelector

-- | @- maximumLineHeight@
maximumLineHeight :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
maximumLineHeight nsParagraphStyle =
  sendMessage nsParagraphStyle maximumLineHeightSelector

-- | @- lineBreakMode@
lineBreakMode :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSLineBreakMode
lineBreakMode nsParagraphStyle =
  sendMessage nsParagraphStyle lineBreakModeSelector

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSWritingDirection
baseWritingDirection nsParagraphStyle =
  sendMessage nsParagraphStyle baseWritingDirectionSelector

-- | @- lineHeightMultiple@
lineHeightMultiple :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
lineHeightMultiple nsParagraphStyle =
  sendMessage nsParagraphStyle lineHeightMultipleSelector

-- | @- paragraphSpacingBefore@
paragraphSpacingBefore :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
paragraphSpacingBefore nsParagraphStyle =
  sendMessage nsParagraphStyle paragraphSpacingBeforeSelector

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CFloat
hyphenationFactor nsParagraphStyle =
  sendMessage nsParagraphStyle hyphenationFactorSelector

-- | @- usesDefaultHyphenation@
usesDefaultHyphenation :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO Bool
usesDefaultHyphenation nsParagraphStyle =
  sendMessage nsParagraphStyle usesDefaultHyphenationSelector

-- | @- tabStops@
tabStops :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO (Id NSArray)
tabStops nsParagraphStyle =
  sendMessage nsParagraphStyle tabStopsSelector

-- | @- defaultTabInterval@
defaultTabInterval :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
defaultTabInterval nsParagraphStyle =
  sendMessage nsParagraphStyle defaultTabIntervalSelector

-- | @- textLists@
textLists :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO (Id NSArray)
textLists nsParagraphStyle =
  sendMessage nsParagraphStyle textListsSelector

-- | @- allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncation :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO Bool
allowsDefaultTighteningForTruncation nsParagraphStyle =
  sendMessage nsParagraphStyle allowsDefaultTighteningForTruncationSelector

-- | @- lineBreakStrategy@
lineBreakStrategy :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSLineBreakStrategy
lineBreakStrategy nsParagraphStyle =
  sendMessage nsParagraphStyle lineBreakStrategySelector

-- | @- alignment@
alignment :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSTextAlignment
alignment nsParagraphStyle =
  sendMessage nsParagraphStyle alignmentSelector

-- | @- tighteningFactorForTruncation@
tighteningFactorForTruncation :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CFloat
tighteningFactorForTruncation nsParagraphStyle =
  sendMessage nsParagraphStyle tighteningFactorForTruncationSelector

-- | @- textBlocks@
textBlocks :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO (Id NSArray)
textBlocks nsParagraphStyle =
  sendMessage nsParagraphStyle textBlocksSelector

-- | @- headerLevel@
headerLevel :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CLong
headerLevel nsParagraphStyle =
  sendMessage nsParagraphStyle headerLevelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultWritingDirectionForLanguage:@
defaultWritingDirectionForLanguageSelector :: Selector '[Id NSString] NSWritingDirection
defaultWritingDirectionForLanguageSelector = mkSelector "defaultWritingDirectionForLanguage:"

-- | @Selector@ for @defaultParagraphStyle@
defaultParagraphStyleSelector :: Selector '[] (Id NSParagraphStyle)
defaultParagraphStyleSelector = mkSelector "defaultParagraphStyle"

-- | @Selector@ for @lineSpacing@
lineSpacingSelector :: Selector '[] CDouble
lineSpacingSelector = mkSelector "lineSpacing"

-- | @Selector@ for @paragraphSpacing@
paragraphSpacingSelector :: Selector '[] CDouble
paragraphSpacingSelector = mkSelector "paragraphSpacing"

-- | @Selector@ for @headIndent@
headIndentSelector :: Selector '[] CDouble
headIndentSelector = mkSelector "headIndent"

-- | @Selector@ for @tailIndent@
tailIndentSelector :: Selector '[] CDouble
tailIndentSelector = mkSelector "tailIndent"

-- | @Selector@ for @firstLineHeadIndent@
firstLineHeadIndentSelector :: Selector '[] CDouble
firstLineHeadIndentSelector = mkSelector "firstLineHeadIndent"

-- | @Selector@ for @minimumLineHeight@
minimumLineHeightSelector :: Selector '[] CDouble
minimumLineHeightSelector = mkSelector "minimumLineHeight"

-- | @Selector@ for @maximumLineHeight@
maximumLineHeightSelector :: Selector '[] CDouble
maximumLineHeightSelector = mkSelector "maximumLineHeight"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector '[] NSLineBreakMode
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector '[] NSWritingDirection
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @lineHeightMultiple@
lineHeightMultipleSelector :: Selector '[] CDouble
lineHeightMultipleSelector = mkSelector "lineHeightMultiple"

-- | @Selector@ for @paragraphSpacingBefore@
paragraphSpacingBeforeSelector :: Selector '[] CDouble
paragraphSpacingBeforeSelector = mkSelector "paragraphSpacingBefore"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector '[] CFloat
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @usesDefaultHyphenation@
usesDefaultHyphenationSelector :: Selector '[] Bool
usesDefaultHyphenationSelector = mkSelector "usesDefaultHyphenation"

-- | @Selector@ for @tabStops@
tabStopsSelector :: Selector '[] (Id NSArray)
tabStopsSelector = mkSelector "tabStops"

-- | @Selector@ for @defaultTabInterval@
defaultTabIntervalSelector :: Selector '[] CDouble
defaultTabIntervalSelector = mkSelector "defaultTabInterval"

-- | @Selector@ for @textLists@
textListsSelector :: Selector '[] (Id NSArray)
textListsSelector = mkSelector "textLists"

-- | @Selector@ for @allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncationSelector :: Selector '[] Bool
allowsDefaultTighteningForTruncationSelector = mkSelector "allowsDefaultTighteningForTruncation"

-- | @Selector@ for @lineBreakStrategy@
lineBreakStrategySelector :: Selector '[] NSLineBreakStrategy
lineBreakStrategySelector = mkSelector "lineBreakStrategy"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @tighteningFactorForTruncation@
tighteningFactorForTruncationSelector :: Selector '[] CFloat
tighteningFactorForTruncationSelector = mkSelector "tighteningFactorForTruncation"

-- | @Selector@ for @textBlocks@
textBlocksSelector :: Selector '[] (Id NSArray)
textBlocksSelector = mkSelector "textBlocks"

-- | @Selector@ for @headerLevel@
headerLevelSelector :: Selector '[] CLong
headerLevelSelector = mkSelector "headerLevel"

