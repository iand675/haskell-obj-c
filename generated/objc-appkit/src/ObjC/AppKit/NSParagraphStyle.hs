{-# LANGUAGE PatternSynonyms #-}
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
  , defaultWritingDirectionForLanguageSelector
  , defaultParagraphStyleSelector
  , lineSpacingSelector
  , paragraphSpacingSelector
  , headIndentSelector
  , tailIndentSelector
  , firstLineHeadIndentSelector
  , minimumLineHeightSelector
  , maximumLineHeightSelector
  , lineBreakModeSelector
  , baseWritingDirectionSelector
  , lineHeightMultipleSelector
  , paragraphSpacingBeforeSelector
  , hyphenationFactorSelector
  , usesDefaultHyphenationSelector
  , tabStopsSelector
  , defaultTabIntervalSelector
  , textListsSelector
  , allowsDefaultTighteningForTruncationSelector
  , lineBreakStrategySelector
  , alignmentSelector
  , tighteningFactorForTruncationSelector
  , textBlocksSelector
  , headerLevelSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultWritingDirectionForLanguage:@
defaultWritingDirectionForLanguage :: IsNSString languageName => languageName -> IO NSWritingDirection
defaultWritingDirectionForLanguage languageName =
  do
    cls' <- getRequiredClass "NSParagraphStyle"
    withObjCPtr languageName $ \raw_languageName ->
      fmap (coerce :: CLong -> NSWritingDirection) $ sendClassMsg cls' (mkSelector "defaultWritingDirectionForLanguage:") retCLong [argPtr (castPtr raw_languageName :: Ptr ())]

-- | @+ defaultParagraphStyle@
defaultParagraphStyle :: IO (Id NSParagraphStyle)
defaultParagraphStyle  =
  do
    cls' <- getRequiredClass "NSParagraphStyle"
    sendClassMsg cls' (mkSelector "defaultParagraphStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lineSpacing@
lineSpacing :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
lineSpacing nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "lineSpacing") retCDouble []

-- | @- paragraphSpacing@
paragraphSpacing :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
paragraphSpacing nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "paragraphSpacing") retCDouble []

-- | @- headIndent@
headIndent :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
headIndent nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "headIndent") retCDouble []

-- | @- tailIndent@
tailIndent :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
tailIndent nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "tailIndent") retCDouble []

-- | @- firstLineHeadIndent@
firstLineHeadIndent :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
firstLineHeadIndent nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "firstLineHeadIndent") retCDouble []

-- | @- minimumLineHeight@
minimumLineHeight :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
minimumLineHeight nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "minimumLineHeight") retCDouble []

-- | @- maximumLineHeight@
maximumLineHeight :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
maximumLineHeight nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "maximumLineHeight") retCDouble []

-- | @- lineBreakMode@
lineBreakMode :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSLineBreakMode
lineBreakMode nsParagraphStyle  =
  fmap (coerce :: CULong -> NSLineBreakMode) $ sendMsg nsParagraphStyle (mkSelector "lineBreakMode") retCULong []

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSWritingDirection
baseWritingDirection nsParagraphStyle  =
  fmap (coerce :: CLong -> NSWritingDirection) $ sendMsg nsParagraphStyle (mkSelector "baseWritingDirection") retCLong []

-- | @- lineHeightMultiple@
lineHeightMultiple :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
lineHeightMultiple nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "lineHeightMultiple") retCDouble []

-- | @- paragraphSpacingBefore@
paragraphSpacingBefore :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
paragraphSpacingBefore nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "paragraphSpacingBefore") retCDouble []

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CFloat
hyphenationFactor nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "hyphenationFactor") retCFloat []

-- | @- usesDefaultHyphenation@
usesDefaultHyphenation :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO Bool
usesDefaultHyphenation nsParagraphStyle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsParagraphStyle (mkSelector "usesDefaultHyphenation") retCULong []

-- | @- tabStops@
tabStops :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO (Id NSArray)
tabStops nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "tabStops") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defaultTabInterval@
defaultTabInterval :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CDouble
defaultTabInterval nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "defaultTabInterval") retCDouble []

-- | @- textLists@
textLists :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO (Id NSArray)
textLists nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "textLists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncation :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO Bool
allowsDefaultTighteningForTruncation nsParagraphStyle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsParagraphStyle (mkSelector "allowsDefaultTighteningForTruncation") retCULong []

-- | @- lineBreakStrategy@
lineBreakStrategy :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSLineBreakStrategy
lineBreakStrategy nsParagraphStyle  =
  fmap (coerce :: CULong -> NSLineBreakStrategy) $ sendMsg nsParagraphStyle (mkSelector "lineBreakStrategy") retCULong []

-- | @- alignment@
alignment :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO NSTextAlignment
alignment nsParagraphStyle  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsParagraphStyle (mkSelector "alignment") retCLong []

-- | @- tighteningFactorForTruncation@
tighteningFactorForTruncation :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CFloat
tighteningFactorForTruncation nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "tighteningFactorForTruncation") retCFloat []

-- | @- textBlocks@
textBlocks :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO (Id NSArray)
textBlocks nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "textBlocks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- headerLevel@
headerLevel :: IsNSParagraphStyle nsParagraphStyle => nsParagraphStyle -> IO CLong
headerLevel nsParagraphStyle  =
  sendMsg nsParagraphStyle (mkSelector "headerLevel") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultWritingDirectionForLanguage:@
defaultWritingDirectionForLanguageSelector :: Selector
defaultWritingDirectionForLanguageSelector = mkSelector "defaultWritingDirectionForLanguage:"

-- | @Selector@ for @defaultParagraphStyle@
defaultParagraphStyleSelector :: Selector
defaultParagraphStyleSelector = mkSelector "defaultParagraphStyle"

-- | @Selector@ for @lineSpacing@
lineSpacingSelector :: Selector
lineSpacingSelector = mkSelector "lineSpacing"

-- | @Selector@ for @paragraphSpacing@
paragraphSpacingSelector :: Selector
paragraphSpacingSelector = mkSelector "paragraphSpacing"

-- | @Selector@ for @headIndent@
headIndentSelector :: Selector
headIndentSelector = mkSelector "headIndent"

-- | @Selector@ for @tailIndent@
tailIndentSelector :: Selector
tailIndentSelector = mkSelector "tailIndent"

-- | @Selector@ for @firstLineHeadIndent@
firstLineHeadIndentSelector :: Selector
firstLineHeadIndentSelector = mkSelector "firstLineHeadIndent"

-- | @Selector@ for @minimumLineHeight@
minimumLineHeightSelector :: Selector
minimumLineHeightSelector = mkSelector "minimumLineHeight"

-- | @Selector@ for @maximumLineHeight@
maximumLineHeightSelector :: Selector
maximumLineHeightSelector = mkSelector "maximumLineHeight"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @lineHeightMultiple@
lineHeightMultipleSelector :: Selector
lineHeightMultipleSelector = mkSelector "lineHeightMultiple"

-- | @Selector@ for @paragraphSpacingBefore@
paragraphSpacingBeforeSelector :: Selector
paragraphSpacingBeforeSelector = mkSelector "paragraphSpacingBefore"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @usesDefaultHyphenation@
usesDefaultHyphenationSelector :: Selector
usesDefaultHyphenationSelector = mkSelector "usesDefaultHyphenation"

-- | @Selector@ for @tabStops@
tabStopsSelector :: Selector
tabStopsSelector = mkSelector "tabStops"

-- | @Selector@ for @defaultTabInterval@
defaultTabIntervalSelector :: Selector
defaultTabIntervalSelector = mkSelector "defaultTabInterval"

-- | @Selector@ for @textLists@
textListsSelector :: Selector
textListsSelector = mkSelector "textLists"

-- | @Selector@ for @allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncationSelector :: Selector
allowsDefaultTighteningForTruncationSelector = mkSelector "allowsDefaultTighteningForTruncation"

-- | @Selector@ for @lineBreakStrategy@
lineBreakStrategySelector :: Selector
lineBreakStrategySelector = mkSelector "lineBreakStrategy"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @tighteningFactorForTruncation@
tighteningFactorForTruncationSelector :: Selector
tighteningFactorForTruncationSelector = mkSelector "tighteningFactorForTruncation"

-- | @Selector@ for @textBlocks@
textBlocksSelector :: Selector
textBlocksSelector = mkSelector "textBlocks"

-- | @Selector@ for @headerLevel@
headerLevelSelector :: Selector
headerLevelSelector = mkSelector "headerLevel"

