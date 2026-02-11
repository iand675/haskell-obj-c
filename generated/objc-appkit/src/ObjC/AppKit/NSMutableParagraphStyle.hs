{-# LANGUAGE PatternSynonyms #-}
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
  , removeTabStopSelector
  , setParagraphStyleSelector
  , lineSpacingSelector
  , setLineSpacingSelector
  , paragraphSpacingSelector
  , setParagraphSpacingSelector
  , firstLineHeadIndentSelector
  , setFirstLineHeadIndentSelector
  , headIndentSelector
  , setHeadIndentSelector
  , tailIndentSelector
  , setTailIndentSelector
  , lineBreakModeSelector
  , setLineBreakModeSelector
  , minimumLineHeightSelector
  , setMinimumLineHeightSelector
  , maximumLineHeightSelector
  , setMaximumLineHeightSelector
  , baseWritingDirectionSelector
  , setBaseWritingDirectionSelector
  , lineHeightMultipleSelector
  , setLineHeightMultipleSelector
  , paragraphSpacingBeforeSelector
  , setParagraphSpacingBeforeSelector
  , hyphenationFactorSelector
  , setHyphenationFactorSelector
  , usesDefaultHyphenationSelector
  , setUsesDefaultHyphenationSelector
  , tabStopsSelector
  , setTabStopsSelector
  , defaultTabIntervalSelector
  , setDefaultTabIntervalSelector
  , allowsDefaultTighteningForTruncationSelector
  , setAllowsDefaultTighteningForTruncationSelector
  , lineBreakStrategySelector
  , setLineBreakStrategySelector
  , textListsSelector
  , setTextListsSelector
  , alignmentSelector
  , setAlignmentSelector
  , tighteningFactorForTruncationSelector
  , setTighteningFactorForTruncationSelector
  , textBlocksSelector
  , setTextBlocksSelector
  , headerLevelSelector
  , setHeaderLevelSelector

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

-- | @- addTabStop:@
addTabStop :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSTextTab anObject) => nsMutableParagraphStyle -> anObject -> IO ()
addTabStop nsMutableParagraphStyle  anObject =
withObjCPtr anObject $ \raw_anObject ->
    sendMsg nsMutableParagraphStyle (mkSelector "addTabStop:") retVoid [argPtr (castPtr raw_anObject :: Ptr ())]

-- | @- removeTabStop:@
removeTabStop :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSTextTab anObject) => nsMutableParagraphStyle -> anObject -> IO ()
removeTabStop nsMutableParagraphStyle  anObject =
withObjCPtr anObject $ \raw_anObject ->
    sendMsg nsMutableParagraphStyle (mkSelector "removeTabStop:") retVoid [argPtr (castPtr raw_anObject :: Ptr ())]

-- | @- setParagraphStyle:@
setParagraphStyle :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSParagraphStyle obj_) => nsMutableParagraphStyle -> obj_ -> IO ()
setParagraphStyle nsMutableParagraphStyle  obj_ =
withObjCPtr obj_ $ \raw_obj_ ->
    sendMsg nsMutableParagraphStyle (mkSelector "setParagraphStyle:") retVoid [argPtr (castPtr raw_obj_ :: Ptr ())]

-- | @- lineSpacing@
lineSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
lineSpacing nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "lineSpacing") retCDouble []

-- | @- setLineSpacing:@
setLineSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setLineSpacing nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setLineSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- paragraphSpacing@
paragraphSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
paragraphSpacing nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "paragraphSpacing") retCDouble []

-- | @- setParagraphSpacing:@
setParagraphSpacing :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setParagraphSpacing nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setParagraphSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- firstLineHeadIndent@
firstLineHeadIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
firstLineHeadIndent nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "firstLineHeadIndent") retCDouble []

-- | @- setFirstLineHeadIndent:@
setFirstLineHeadIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setFirstLineHeadIndent nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setFirstLineHeadIndent:") retVoid [argCDouble (fromIntegral value)]

-- | @- headIndent@
headIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
headIndent nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "headIndent") retCDouble []

-- | @- setHeadIndent:@
setHeadIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setHeadIndent nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setHeadIndent:") retVoid [argCDouble (fromIntegral value)]

-- | @- tailIndent@
tailIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
tailIndent nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "tailIndent") retCDouble []

-- | @- setTailIndent:@
setTailIndent :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setTailIndent nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setTailIndent:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineBreakMode@
lineBreakMode :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSLineBreakMode
lineBreakMode nsMutableParagraphStyle  =
  fmap (coerce :: CULong -> NSLineBreakMode) $ sendMsg nsMutableParagraphStyle (mkSelector "lineBreakMode") retCULong []

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSLineBreakMode -> IO ()
setLineBreakMode nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setLineBreakMode:") retVoid [argCULong (coerce value)]

-- | @- minimumLineHeight@
minimumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
minimumLineHeight nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "minimumLineHeight") retCDouble []

-- | @- setMinimumLineHeight:@
setMinimumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setMinimumLineHeight nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setMinimumLineHeight:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumLineHeight@
maximumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
maximumLineHeight nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "maximumLineHeight") retCDouble []

-- | @- setMaximumLineHeight:@
setMaximumLineHeight :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setMaximumLineHeight nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setMaximumLineHeight:") retVoid [argCDouble (fromIntegral value)]

-- | @- baseWritingDirection@
baseWritingDirection :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSWritingDirection
baseWritingDirection nsMutableParagraphStyle  =
  fmap (coerce :: CLong -> NSWritingDirection) $ sendMsg nsMutableParagraphStyle (mkSelector "baseWritingDirection") retCLong []

-- | @- setBaseWritingDirection:@
setBaseWritingDirection :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSWritingDirection -> IO ()
setBaseWritingDirection nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setBaseWritingDirection:") retVoid [argCLong (coerce value)]

-- | @- lineHeightMultiple@
lineHeightMultiple :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
lineHeightMultiple nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "lineHeightMultiple") retCDouble []

-- | @- setLineHeightMultiple:@
setLineHeightMultiple :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setLineHeightMultiple nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setLineHeightMultiple:") retVoid [argCDouble (fromIntegral value)]

-- | @- paragraphSpacingBefore@
paragraphSpacingBefore :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
paragraphSpacingBefore nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "paragraphSpacingBefore") retCDouble []

-- | @- setParagraphSpacingBefore:@
setParagraphSpacingBefore :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setParagraphSpacingBefore nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setParagraphSpacingBefore:") retVoid [argCDouble (fromIntegral value)]

-- | @- hyphenationFactor@
hyphenationFactor :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CFloat
hyphenationFactor nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "hyphenationFactor") retCFloat []

-- | @- setHyphenationFactor:@
setHyphenationFactor :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CFloat -> IO ()
setHyphenationFactor nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setHyphenationFactor:") retVoid [argCFloat (fromIntegral value)]

-- | @- usesDefaultHyphenation@
usesDefaultHyphenation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO Bool
usesDefaultHyphenation nsMutableParagraphStyle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableParagraphStyle (mkSelector "usesDefaultHyphenation") retCULong []

-- | @- setUsesDefaultHyphenation:@
setUsesDefaultHyphenation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> Bool -> IO ()
setUsesDefaultHyphenation nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setUsesDefaultHyphenation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tabStops@
tabStops :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO (Id NSArray)
tabStops nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "tabStops") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTabStops:@
setTabStops :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSArray value) => nsMutableParagraphStyle -> value -> IO ()
setTabStops nsMutableParagraphStyle  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableParagraphStyle (mkSelector "setTabStops:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultTabInterval@
defaultTabInterval :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CDouble
defaultTabInterval nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "defaultTabInterval") retCDouble []

-- | @- setDefaultTabInterval:@
setDefaultTabInterval :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CDouble -> IO ()
setDefaultTabInterval nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setDefaultTabInterval:") retVoid [argCDouble (fromIntegral value)]

-- | @- allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO Bool
allowsDefaultTighteningForTruncation nsMutableParagraphStyle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableParagraphStyle (mkSelector "allowsDefaultTighteningForTruncation") retCULong []

-- | @- setAllowsDefaultTighteningForTruncation:@
setAllowsDefaultTighteningForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> Bool -> IO ()
setAllowsDefaultTighteningForTruncation nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setAllowsDefaultTighteningForTruncation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- lineBreakStrategy@
lineBreakStrategy :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSLineBreakStrategy
lineBreakStrategy nsMutableParagraphStyle  =
  fmap (coerce :: CULong -> NSLineBreakStrategy) $ sendMsg nsMutableParagraphStyle (mkSelector "lineBreakStrategy") retCULong []

-- | @- setLineBreakStrategy:@
setLineBreakStrategy :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSLineBreakStrategy -> IO ()
setLineBreakStrategy nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setLineBreakStrategy:") retVoid [argCULong (coerce value)]

-- | @- textLists@
textLists :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO (Id NSArray)
textLists nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "textLists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextLists:@
setTextLists :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSArray value) => nsMutableParagraphStyle -> value -> IO ()
setTextLists nsMutableParagraphStyle  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableParagraphStyle (mkSelector "setTextLists:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alignment@
alignment :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO NSTextAlignment
alignment nsMutableParagraphStyle  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsMutableParagraphStyle (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> NSTextAlignment -> IO ()
setAlignment nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setAlignment:") retVoid [argCLong (coerce value)]

-- | @- tighteningFactorForTruncation@
tighteningFactorForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CFloat
tighteningFactorForTruncation nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "tighteningFactorForTruncation") retCFloat []

-- | @- setTighteningFactorForTruncation:@
setTighteningFactorForTruncation :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CFloat -> IO ()
setTighteningFactorForTruncation nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setTighteningFactorForTruncation:") retVoid [argCFloat (fromIntegral value)]

-- | @- textBlocks@
textBlocks :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO (Id NSArray)
textBlocks nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "textBlocks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextBlocks:@
setTextBlocks :: (IsNSMutableParagraphStyle nsMutableParagraphStyle, IsNSArray value) => nsMutableParagraphStyle -> value -> IO ()
setTextBlocks nsMutableParagraphStyle  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableParagraphStyle (mkSelector "setTextBlocks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- headerLevel@
headerLevel :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> IO CLong
headerLevel nsMutableParagraphStyle  =
  sendMsg nsMutableParagraphStyle (mkSelector "headerLevel") retCLong []

-- | @- setHeaderLevel:@
setHeaderLevel :: IsNSMutableParagraphStyle nsMutableParagraphStyle => nsMutableParagraphStyle -> CLong -> IO ()
setHeaderLevel nsMutableParagraphStyle  value =
  sendMsg nsMutableParagraphStyle (mkSelector "setHeaderLevel:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTabStop:@
addTabStopSelector :: Selector
addTabStopSelector = mkSelector "addTabStop:"

-- | @Selector@ for @removeTabStop:@
removeTabStopSelector :: Selector
removeTabStopSelector = mkSelector "removeTabStop:"

-- | @Selector@ for @setParagraphStyle:@
setParagraphStyleSelector :: Selector
setParagraphStyleSelector = mkSelector "setParagraphStyle:"

-- | @Selector@ for @lineSpacing@
lineSpacingSelector :: Selector
lineSpacingSelector = mkSelector "lineSpacing"

-- | @Selector@ for @setLineSpacing:@
setLineSpacingSelector :: Selector
setLineSpacingSelector = mkSelector "setLineSpacing:"

-- | @Selector@ for @paragraphSpacing@
paragraphSpacingSelector :: Selector
paragraphSpacingSelector = mkSelector "paragraphSpacing"

-- | @Selector@ for @setParagraphSpacing:@
setParagraphSpacingSelector :: Selector
setParagraphSpacingSelector = mkSelector "setParagraphSpacing:"

-- | @Selector@ for @firstLineHeadIndent@
firstLineHeadIndentSelector :: Selector
firstLineHeadIndentSelector = mkSelector "firstLineHeadIndent"

-- | @Selector@ for @setFirstLineHeadIndent:@
setFirstLineHeadIndentSelector :: Selector
setFirstLineHeadIndentSelector = mkSelector "setFirstLineHeadIndent:"

-- | @Selector@ for @headIndent@
headIndentSelector :: Selector
headIndentSelector = mkSelector "headIndent"

-- | @Selector@ for @setHeadIndent:@
setHeadIndentSelector :: Selector
setHeadIndentSelector = mkSelector "setHeadIndent:"

-- | @Selector@ for @tailIndent@
tailIndentSelector :: Selector
tailIndentSelector = mkSelector "tailIndent"

-- | @Selector@ for @setTailIndent:@
setTailIndentSelector :: Selector
setTailIndentSelector = mkSelector "setTailIndent:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @minimumLineHeight@
minimumLineHeightSelector :: Selector
minimumLineHeightSelector = mkSelector "minimumLineHeight"

-- | @Selector@ for @setMinimumLineHeight:@
setMinimumLineHeightSelector :: Selector
setMinimumLineHeightSelector = mkSelector "setMinimumLineHeight:"

-- | @Selector@ for @maximumLineHeight@
maximumLineHeightSelector :: Selector
maximumLineHeightSelector = mkSelector "maximumLineHeight"

-- | @Selector@ for @setMaximumLineHeight:@
setMaximumLineHeightSelector :: Selector
setMaximumLineHeightSelector = mkSelector "setMaximumLineHeight:"

-- | @Selector@ for @baseWritingDirection@
baseWritingDirectionSelector :: Selector
baseWritingDirectionSelector = mkSelector "baseWritingDirection"

-- | @Selector@ for @setBaseWritingDirection:@
setBaseWritingDirectionSelector :: Selector
setBaseWritingDirectionSelector = mkSelector "setBaseWritingDirection:"

-- | @Selector@ for @lineHeightMultiple@
lineHeightMultipleSelector :: Selector
lineHeightMultipleSelector = mkSelector "lineHeightMultiple"

-- | @Selector@ for @setLineHeightMultiple:@
setLineHeightMultipleSelector :: Selector
setLineHeightMultipleSelector = mkSelector "setLineHeightMultiple:"

-- | @Selector@ for @paragraphSpacingBefore@
paragraphSpacingBeforeSelector :: Selector
paragraphSpacingBeforeSelector = mkSelector "paragraphSpacingBefore"

-- | @Selector@ for @setParagraphSpacingBefore:@
setParagraphSpacingBeforeSelector :: Selector
setParagraphSpacingBeforeSelector = mkSelector "setParagraphSpacingBefore:"

-- | @Selector@ for @hyphenationFactor@
hyphenationFactorSelector :: Selector
hyphenationFactorSelector = mkSelector "hyphenationFactor"

-- | @Selector@ for @setHyphenationFactor:@
setHyphenationFactorSelector :: Selector
setHyphenationFactorSelector = mkSelector "setHyphenationFactor:"

-- | @Selector@ for @usesDefaultHyphenation@
usesDefaultHyphenationSelector :: Selector
usesDefaultHyphenationSelector = mkSelector "usesDefaultHyphenation"

-- | @Selector@ for @setUsesDefaultHyphenation:@
setUsesDefaultHyphenationSelector :: Selector
setUsesDefaultHyphenationSelector = mkSelector "setUsesDefaultHyphenation:"

-- | @Selector@ for @tabStops@
tabStopsSelector :: Selector
tabStopsSelector = mkSelector "tabStops"

-- | @Selector@ for @setTabStops:@
setTabStopsSelector :: Selector
setTabStopsSelector = mkSelector "setTabStops:"

-- | @Selector@ for @defaultTabInterval@
defaultTabIntervalSelector :: Selector
defaultTabIntervalSelector = mkSelector "defaultTabInterval"

-- | @Selector@ for @setDefaultTabInterval:@
setDefaultTabIntervalSelector :: Selector
setDefaultTabIntervalSelector = mkSelector "setDefaultTabInterval:"

-- | @Selector@ for @allowsDefaultTighteningForTruncation@
allowsDefaultTighteningForTruncationSelector :: Selector
allowsDefaultTighteningForTruncationSelector = mkSelector "allowsDefaultTighteningForTruncation"

-- | @Selector@ for @setAllowsDefaultTighteningForTruncation:@
setAllowsDefaultTighteningForTruncationSelector :: Selector
setAllowsDefaultTighteningForTruncationSelector = mkSelector "setAllowsDefaultTighteningForTruncation:"

-- | @Selector@ for @lineBreakStrategy@
lineBreakStrategySelector :: Selector
lineBreakStrategySelector = mkSelector "lineBreakStrategy"

-- | @Selector@ for @setLineBreakStrategy:@
setLineBreakStrategySelector :: Selector
setLineBreakStrategySelector = mkSelector "setLineBreakStrategy:"

-- | @Selector@ for @textLists@
textListsSelector :: Selector
textListsSelector = mkSelector "textLists"

-- | @Selector@ for @setTextLists:@
setTextListsSelector :: Selector
setTextListsSelector = mkSelector "setTextLists:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @tighteningFactorForTruncation@
tighteningFactorForTruncationSelector :: Selector
tighteningFactorForTruncationSelector = mkSelector "tighteningFactorForTruncation"

-- | @Selector@ for @setTighteningFactorForTruncation:@
setTighteningFactorForTruncationSelector :: Selector
setTighteningFactorForTruncationSelector = mkSelector "setTighteningFactorForTruncation:"

-- | @Selector@ for @textBlocks@
textBlocksSelector :: Selector
textBlocksSelector = mkSelector "textBlocks"

-- | @Selector@ for @setTextBlocks:@
setTextBlocksSelector :: Selector
setTextBlocksSelector = mkSelector "setTextBlocks:"

-- | @Selector@ for @headerLevel@
headerLevelSelector :: Selector
headerLevelSelector = mkSelector "headerLevel"

-- | @Selector@ for @setHeaderLevel:@
setHeaderLevelSelector :: Selector
setHeaderLevelSelector = mkSelector "setHeaderLevel:"

