{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableParagraphStyle@.
module ObjC.AppKit.NSMutableParagraphStyle
  ( NSMutableParagraphStyle
  , IsNSMutableParagraphStyle(..)
  , addTabStop
  , removeTabStop
  , setParagraphStyle
  , lineSpacing
  , setLineSpacing
  , paragraphSpacing
  , setParagraphSpacing
  , firstLineHeadIndent
  , setFirstLineHeadIndent
  , headIndent
  , setHeadIndent
  , tailIndent
  , setTailIndent
  , lineBreakMode
  , setLineBreakMode
  , minimumLineHeight
  , setMinimumLineHeight
  , maximumLineHeight
  , setMaximumLineHeight
  , baseWritingDirection
  , setBaseWritingDirection
  , lineHeightMultiple
  , setLineHeightMultiple
  , paragraphSpacingBefore
  , setParagraphSpacingBefore
  , hyphenationFactor
  , setHyphenationFactor
  , usesDefaultHyphenation
  , setUsesDefaultHyphenation
  , tabStops
  , setTabStops
  , defaultTabInterval
  , setDefaultTabInterval
  , allowsDefaultTighteningForTruncation
  , setAllowsDefaultTighteningForTruncation
  , lineBreakStrategy
  , setLineBreakStrategy
  , textLists
  , setTextLists
  , alignment
  , setAlignment
  , tighteningFactorForTruncation
  , setTighteningFactorForTruncation
  , textBlocks
  , setTextBlocks
  , headerLevel
  , setHeaderLevel
  , addTabStopSelector
  , alignmentSelector
  , allowsDefaultTighteningForTruncationSelector
  , baseWritingDirectionSelector
  , defaultTabIntervalSelector
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
  , removeTabStopSelector
  , setAlignmentSelector
  , setAllowsDefaultTighteningForTruncationSelector
  , setBaseWritingDirectionSelector
  , setDefaultTabIntervalSelector
  , setFirstLineHeadIndentSelector
  , setHeadIndentSelector
  , setHeaderLevelSelector
  , setHyphenationFactorSelector
  , setLineBreakModeSelector
  , setLineBreakStrategySelector
  , setLineHeightMultipleSelector
  , setLineSpacingSelector
  , setMaximumLineHeightSelector
  , setMinimumLineHeightSelector
  , setParagraphSpacingBeforeSelector
  , setParagraphSpacingSelector
  , setParagraphStyleSelector
  , setTabStopsSelector
  , setTailIndentSelector
  , setTextBlocksSelector
  , setTextListsSelector
  , setTighteningFactorForTruncationSelector
  , setUsesDefaultHyphenationSelector
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

-- | @- addTabStop:@
addTabStop :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSTextTab anObject) => nsMutableParagraphStyle -> anObject -> IO ()
addTabStop nsMutableParagraphStyle anObject =
  sendMessage nsMutableParagraphStyle addTabStopSelector (toNSTextTab anObject)

-- | @- removeTabStop:@
removeTabStop :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSTextTab anObject) => nsMutableParagraphStyle -> anObject -> IO ()
removeTabStop nsMutableParagraphStyle anObject =
  sendMessage nsMutableParagraphStyle removeTabStopSelector (toNSTextTab anObject)

-- | @- setParagraphStyle:@
setParagraphStyle :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSParagraphStyle obj_) => nsMutableParagraphStyle -> obj_ -> IO ()
setParagraphStyle nsMutableParagraphStyle obj_ =
  sendMessage nsMutableParagraphStyle setParagraphStyleSelector (toNSParagraphStyle obj_)

-- | @- lineSpacing@
lineSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
lineSpacing nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle lineSpacingSelector

-- | @- setLineSpacing:@
setLineSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setLineSpacing nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setLineSpacingSelector value

-- | @- paragraphSpacing@
paragraphSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
paragraphSpacing nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle paragraphSpacingSelector

-- | @- setParagraphSpacing:@
setParagraphSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setParagraphSpacing nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setParagraphSpacingSelector value

-- | @- firstLineHeadIndent@
firstLineHeadIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
firstLineHeadIndent nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle firstLineHeadIndentSelector

-- | @- setFirstLineHeadIndent:@
setFirstLineHeadIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setFirstLineHeadIndent nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setFirstLineHeadIndentSelector value

-- | @- headIndent@
headIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
headIndent nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle headIndentSelector

-- | @- setHeadIndent:@
setHeadIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setHeadIndent nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setHeadIndentSelector value

-- | @- tailIndent@
tailIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
tailIndent nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle tailIndentSelector

-- | @- setTailIndent:@
setTailIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setTailIndent nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setTailIndentSelector value

-- | @- lineBreakMode@
lineBreakMode :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSLineBreakMode
lineBreakMode nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle lineBreakModeSelector

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSLineBreakMode -> IO ()
setLineBreakMode nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setLineBreakModeSelector value

-- | @- minimumLineHeight@
minimumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
minimumLineHeight nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle minimumLineHeightSelector

-- | @- setMinimumLineHeight:@
setMinimumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setMinimumLineHeight nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setMinimumLineHeightSelector value

-- | @- maximumLineHeight@
maximumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
maximumLineHeight nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle maximumLineHeightSelector

-- | @- setMaximumLineHeight:@
setMaximumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setMaximumLineHeight nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setMaximumLineHeightSelector value

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSWritingDirection
baseWritingDirection nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle baseWritingDirectionSelector

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSWritingDirection -> IO ()
setBaseWritingDirection nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setBaseWritingDirectionSelector value

-- | @- lineHeightMultiple@
lineHeightMultiple :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
lineHeightMultiple nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle lineHeightMultipleSelector

-- | @- setLineHeightMultiple:@
setLineHeightMultiple :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setLineHeightMultiple nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setLineHeightMultipleSelector value

-- | @- paragraphSpacingBefore@
paragraphSpacingBefore :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
paragraphSpacingBefore nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle paragraphSpacingBeforeSelector

-- | @- setParagraphSpacingBefore:@
setParagraphSpacingBefore :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setParagraphSpacingBefore nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setParagraphSpacingBeforeSelector value

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CFloat
hyphenationFactor nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle hyphenationFactorSelector

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CFloat -> IO ()
setHyphenationFactor nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setHyphenationFactorSelector value

-- | @- usesDefaultHyphenation@
usesDefaultHyphenation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO Bool
usesDefaultHyphenation nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle usesDefaultHyphenationSelector

-- | @- setUsesDefaultHyphenation:@
setUsesDefaultHyphenation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> Bool -> IO ()
setUsesDefaultHyphenation nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setUsesDefaultHyphenationSelector value

-- | @- tabStops@
tabStops :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO (Id NSArray)
tabStops nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle tabStopsSelector

-- | @- setTabStops:@
setTabStops :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSArray value) => nsMutableParagraphStyle -> value -> IO ()
setTabStops nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setTabStopsSelector (toNSArray value)

-- | @- defaultTabInterval@
defaultTabInterval :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
defaultTabInterval nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle defaultTabIntervalSelector

-- | @- setDefaultTabInterval:@
setDefaultTabInterval :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setDefaultTabInterval nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setDefaultTabIntervalSelector value

-- | @- allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO Bool
allowsDefaultTighteningForTruncation nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle allowsDefaultTighteningForTruncationSelector

-- | @- setAllowsDefaultTighteningForTruncation:@
setAllowsDefaultTighteningForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> Bool -> IO ()
setAllowsDefaultTighteningForTruncation nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setAllowsDefaultTighteningForTruncationSelector value

-- | @- lineBreakStrategy@
lineBreakStrategy :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSLineBreakStrategy
lineBreakStrategy nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle lineBreakStrategySelector

-- | @- setLineBreakStrategy:@
setLineBreakStrategy :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSLineBreakStrategy -> IO ()
setLineBreakStrategy nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setLineBreakStrategySelector value

-- | @- textLists@
textLists :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO (Id NSArray)
textLists nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle textListsSelector

-- | @- setTextLists:@
setTextLists :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSArray value) => nsMutableParagraphStyle -> value -> IO ()
setTextLists nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setTextListsSelector (toNSArray value)

-- | @- alignment@
alignment :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSTextAlignment
alignment nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSTextAlignment -> IO ()
setAlignment nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setAlignmentSelector value

-- | @- tighteningFactorForTruncation@
tighteningFactorForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CFloat
tighteningFactorForTruncation nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle tighteningFactorForTruncationSelector

-- | @- setTighteningFactorForTruncation:@
setTighteningFactorForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CFloat -> IO ()
setTighteningFactorForTruncation nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setTighteningFactorForTruncationSelector value

-- | @- textBlocks@
textBlocks :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO (Id NSArray)
textBlocks nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle textBlocksSelector

-- | @- setTextBlocks:@
setTextBlocks :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSArray value) => nsMutableParagraphStyle -> value -> IO ()
setTextBlocks nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setTextBlocksSelector (toNSArray value)

-- | @- headerLevel@
headerLevel :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CLong
headerLevel nsMutableParagraphStyle =
  sendMessage nsMutableParagraphStyle headerLevelSelector

-- | @- setHeaderLevel:@
setHeaderLevel :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CLong -> IO ()
setHeaderLevel nsMutableParagraphStyle value =
  sendMessage nsMutableParagraphStyle setHeaderLevelSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTabStop:@
addTabStopSelector :: Selector '[Id NSTextTab] ()
addTabStopSelector = mkSelector "addTabStop:"

-- | @Selector@ for @removeTabStop:@
removeTabStopSelector :: Selector '[Id NSTextTab] ()
removeTabStopSelector = mkSelector "removeTabStop:"

-- | @Selector@ for @setParagraphStyle:@
setParagraphStyleSelector :: Selector '[Id NSParagraphStyle] ()
setParagraphStyleSelector = mkSelector "setParagraphStyle:"

-- | @Selector@ for @lineSpacing@
lineSpacingSelector :: Selector '[] CDouble
lineSpacingSelector = mkSelector "lineSpacing"

-- | @Selector@ for @setLineSpacing:@
setLineSpacingSelector :: Selector '[CDouble] ()
setLineSpacingSelector = mkSelector "setLineSpacing:"

-- | @Selector@ for @paragraphSpacing@
paragraphSpacingSelector :: Selector '[] CDouble
paragraphSpacingSelector = mkSelector "paragraphSpacing"

-- | @Selector@ for @setParagraphSpacing:@
setParagraphSpacingSelector :: Selector '[CDouble] ()
setParagraphSpacingSelector = mkSelector "setParagraphSpacing:"

-- | @Selector@ for @firstLineHeadIndent@
firstLineHeadIndentSelector :: Selector '[] CDouble
firstLineHeadIndentSelector = mkSelector "firstLineHeadIndent"

-- | @Selector@ for @setFirstLineHeadIndent:@
setFirstLineHeadIndentSelector :: Selector '[CDouble] ()
setFirstLineHeadIndentSelector = mkSelector "setFirstLineHeadIndent:"

-- | @Selector@ for @headIndent@
headIndentSelector :: Selector '[] CDouble
headIndentSelector = mkSelector "headIndent"

-- | @Selector@ for @setHeadIndent:@
setHeadIndentSelector :: Selector '[CDouble] ()
setHeadIndentSelector = mkSelector "setHeadIndent:"

-- | @Selector@ for @tailIndent@
tailIndentSelector :: Selector '[] CDouble
tailIndentSelector = mkSelector "tailIndent"

-- | @Selector@ for @setTailIndent:@
setTailIndentSelector :: Selector '[CDouble] ()
setTailIndentSelector = mkSelector "setTailIndent:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector '[] NSLineBreakMode
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector '[NSLineBreakMode] ()
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @minimumLineHeight@
minimumLineHeightSelector :: Selector '[] CDouble
minimumLineHeightSelector = mkSelector "minimumLineHeight"

-- | @Selector@ for @setMinimumLineHeight:@
setMinimumLineHeightSelector :: Selector '[CDouble] ()
setMinimumLineHeightSelector = mkSelector "setMinimumLineHeight:"

-- | @Selector@ for @maximumLineHeight@
maximumLineHeightSelector :: Selector '[] CDouble
maximumLineHeightSelector = mkSelector "maximumLineHeight"

-- | @Selector@ for @setMaximumLineHeight:@
setMaximumLineHeightSelector :: Selector '[CDouble] ()
setMaximumLineHeightSelector = mkSelector "setMaximumLineHeight:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector '[] NSWritingDirection
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @lineHeightMultiple@
lineHeightMultipleSelector :: Selector '[] CDouble
lineHeightMultipleSelector = mkSelector "lineHeightMultiple"

-- | @Selector@ for @setLineHeightMultiple:@
setLineHeightMultipleSelector :: Selector '[CDouble] ()
setLineHeightMultipleSelector = mkSelector "setLineHeightMultiple:"

-- | @Selector@ for @paragraphSpacingBefore@
paragraphSpacingBeforeSelector :: Selector '[] CDouble
paragraphSpacingBeforeSelector = mkSelector "paragraphSpacingBefore"

-- | @Selector@ for @setParagraphSpacingBefore:@
setParagraphSpacingBeforeSelector :: Selector '[CDouble] ()
setParagraphSpacingBeforeSelector = mkSelector "setParagraphSpacingBefore:"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector '[] CFloat
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @setHyphenationFactor:@
setHyphenationFactorSelector :: Selector '[CFloat] ()
setHyphenationFactorSelector = mkSelector "setHyphenationFactor:"

-- | @Selector@ for @usesDefaultHyphenation@
usesDefaultHyphenationSelector :: Selector '[] Bool
usesDefaultHyphenationSelector = mkSelector "usesDefaultHyphenation"

-- | @Selector@ for @setUsesDefaultHyphenation:@
setUsesDefaultHyphenationSelector :: Selector '[Bool] ()
setUsesDefaultHyphenationSelector = mkSelector "setUsesDefaultHyphenation:"

-- | @Selector@ for @tabStops@
tabStopsSelector :: Selector '[] (Id NSArray)
tabStopsSelector = mkSelector "tabStops"

-- | @Selector@ for @setTabStops:@
setTabStopsSelector :: Selector '[Id NSArray] ()
setTabStopsSelector = mkSelector "setTabStops:"

-- | @Selector@ for @defaultTabInterval@
defaultTabIntervalSelector :: Selector '[] CDouble
defaultTabIntervalSelector = mkSelector "defaultTabInterval"

-- | @Selector@ for @setDefaultTabInterval:@
setDefaultTabIntervalSelector :: Selector '[CDouble] ()
setDefaultTabIntervalSelector = mkSelector "setDefaultTabInterval:"

-- | @Selector@ for @allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncationSelector :: Selector '[] Bool
allowsDefaultTighteningForTruncationSelector = mkSelector "allowsDefaultTighteningForTruncation"

-- | @Selector@ for @setAllowsDefaultTighteningForTruncation:@
setAllowsDefaultTighteningForTruncationSelector :: Selector '[Bool] ()
setAllowsDefaultTighteningForTruncationSelector = mkSelector "setAllowsDefaultTighteningForTruncation:"

-- | @Selector@ for @lineBreakStrategy@
lineBreakStrategySelector :: Selector '[] NSLineBreakStrategy
lineBreakStrategySelector = mkSelector "lineBreakStrategy"

-- | @Selector@ for @setLineBreakStrategy:@
setLineBreakStrategySelector :: Selector '[NSLineBreakStrategy] ()
setLineBreakStrategySelector = mkSelector "setLineBreakStrategy:"

-- | @Selector@ for @textLists@
textListsSelector :: Selector '[] (Id NSArray)
textListsSelector = mkSelector "textLists"

-- | @Selector@ for @setTextLists:@
setTextListsSelector :: Selector '[Id NSArray] ()
setTextListsSelector = mkSelector "setTextLists:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSTextAlignment] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @tighteningFactorForTruncation@
tighteningFactorForTruncationSelector :: Selector '[] CFloat
tighteningFactorForTruncationSelector = mkSelector "tighteningFactorForTruncation"

-- | @Selector@ for @setTighteningFactorForTruncation:@
setTighteningFactorForTruncationSelector :: Selector '[CFloat] ()
setTighteningFactorForTruncationSelector = mkSelector "setTighteningFactorForTruncation:"

-- | @Selector@ for @textBlocks@
textBlocksSelector :: Selector '[] (Id NSArray)
textBlocksSelector = mkSelector "textBlocks"

-- | @Selector@ for @setTextBlocks:@
setTextBlocksSelector :: Selector '[Id NSArray] ()
setTextBlocksSelector = mkSelector "setTextBlocks:"

-- | @Selector@ for @headerLevel@
headerLevelSelector :: Selector '[] CLong
headerLevelSelector = mkSelector "headerLevel"

-- | @Selector@ for @setHeaderLevel:@
setHeaderLevelSelector :: Selector '[CLong] ()
setHeaderLevelSelector = mkSelector "setHeaderLevel:"

