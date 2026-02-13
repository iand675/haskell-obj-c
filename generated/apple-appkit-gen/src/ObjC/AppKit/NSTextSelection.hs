{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextSelection@.
module ObjC.AppKit.NSTextSelection
  ( NSTextSelection
  , IsNSTextSelection(..)
  , initWithRanges_affinity_granularity
  , initWithCoder
  , initWithRange_affinity_granularity
  , initWithLocation_affinity
  , init_
  , textSelectionWithTextRanges
  , textRanges
  , granularity
  , affinity
  , transient
  , anchorPositionOffset
  , setAnchorPositionOffset
  , logical
  , setLogical
  , secondarySelectionLocation
  , setSecondarySelectionLocation
  , typingAttributes
  , setTypingAttributes
  , affinitySelector
  , anchorPositionOffsetSelector
  , granularitySelector
  , initSelector
  , initWithCoderSelector
  , initWithLocation_affinitySelector
  , initWithRange_affinity_granularitySelector
  , initWithRanges_affinity_granularitySelector
  , logicalSelector
  , secondarySelectionLocationSelector
  , setAnchorPositionOffsetSelector
  , setLogicalSelector
  , setSecondarySelectionLocationSelector
  , setTypingAttributesSelector
  , textRangesSelector
  , textSelectionWithTextRangesSelector
  , transientSelector
  , typingAttributesSelector

  -- * Enum types
  , NSTextSelectionAffinity(NSTextSelectionAffinity)
  , pattern NSTextSelectionAffinityUpstream
  , pattern NSTextSelectionAffinityDownstream
  , NSTextSelectionGranularity(NSTextSelectionGranularity)
  , pattern NSTextSelectionGranularityCharacter
  , pattern NSTextSelectionGranularityWord
  , pattern NSTextSelectionGranularityParagraph
  , pattern NSTextSelectionGranularityLine
  , pattern NSTextSelectionGranularitySentence

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

-- | @- initWithRanges:affinity:granularity:@
initWithRanges_affinity_granularity :: (IsNSTextSelection nsTextSelection, IsNSArray textRanges) => nsTextSelection -> textRanges -> NSTextSelectionAffinity -> NSTextSelectionGranularity -> IO (Id NSTextSelection)
initWithRanges_affinity_granularity nsTextSelection textRanges affinity granularity =
  sendOwnedMessage nsTextSelection initWithRanges_affinity_granularitySelector (toNSArray textRanges) affinity granularity

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextSelection nsTextSelection, IsNSCoder coder) => nsTextSelection -> coder -> IO (Id NSTextSelection)
initWithCoder nsTextSelection coder =
  sendOwnedMessage nsTextSelection initWithCoderSelector (toNSCoder coder)

-- | @- initWithRange:affinity:granularity:@
initWithRange_affinity_granularity :: (IsNSTextSelection nsTextSelection, IsNSTextRange range) => nsTextSelection -> range -> NSTextSelectionAffinity -> NSTextSelectionGranularity -> IO (Id NSTextSelection)
initWithRange_affinity_granularity nsTextSelection range affinity granularity =
  sendOwnedMessage nsTextSelection initWithRange_affinity_granularitySelector (toNSTextRange range) affinity granularity

-- | @- initWithLocation:affinity:@
initWithLocation_affinity :: IsNSTextSelection nsTextSelection => nsTextSelection -> RawId -> NSTextSelectionAffinity -> IO (Id NSTextSelection)
initWithLocation_affinity nsTextSelection location affinity =
  sendOwnedMessage nsTextSelection initWithLocation_affinitySelector location affinity

-- | @- init@
init_ :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO (Id NSTextSelection)
init_ nsTextSelection =
  sendOwnedMessage nsTextSelection initSelector

-- | @- textSelectionWithTextRanges:@
textSelectionWithTextRanges :: (IsNSTextSelection nsTextSelection, IsNSArray textRanges) => nsTextSelection -> textRanges -> IO (Id NSTextSelection)
textSelectionWithTextRanges nsTextSelection textRanges =
  sendMessage nsTextSelection textSelectionWithTextRangesSelector (toNSArray textRanges)

-- | @- textRanges@
textRanges :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO (Id NSArray)
textRanges nsTextSelection =
  sendMessage nsTextSelection textRangesSelector

-- | @- granularity@
granularity :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO NSTextSelectionGranularity
granularity nsTextSelection =
  sendMessage nsTextSelection granularitySelector

-- | @- affinity@
affinity :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO NSTextSelectionAffinity
affinity nsTextSelection =
  sendMessage nsTextSelection affinitySelector

-- | @- transient@
transient :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO Bool
transient nsTextSelection =
  sendMessage nsTextSelection transientSelector

-- | @- anchorPositionOffset@
anchorPositionOffset :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO CDouble
anchorPositionOffset nsTextSelection =
  sendMessage nsTextSelection anchorPositionOffsetSelector

-- | @- setAnchorPositionOffset:@
setAnchorPositionOffset :: IsNSTextSelection nsTextSelection => nsTextSelection -> CDouble -> IO ()
setAnchorPositionOffset nsTextSelection value =
  sendMessage nsTextSelection setAnchorPositionOffsetSelector value

-- | @- logical@
logical :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO Bool
logical nsTextSelection =
  sendMessage nsTextSelection logicalSelector

-- | @- setLogical:@
setLogical :: IsNSTextSelection nsTextSelection => nsTextSelection -> Bool -> IO ()
setLogical nsTextSelection value =
  sendMessage nsTextSelection setLogicalSelector value

-- | @- secondarySelectionLocation@
secondarySelectionLocation :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO RawId
secondarySelectionLocation nsTextSelection =
  sendMessage nsTextSelection secondarySelectionLocationSelector

-- | @- setSecondarySelectionLocation:@
setSecondarySelectionLocation :: IsNSTextSelection nsTextSelection => nsTextSelection -> RawId -> IO ()
setSecondarySelectionLocation nsTextSelection value =
  sendMessage nsTextSelection setSecondarySelectionLocationSelector value

-- | @- typingAttributes@
typingAttributes :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO (Id NSDictionary)
typingAttributes nsTextSelection =
  sendMessage nsTextSelection typingAttributesSelector

-- | @- setTypingAttributes:@
setTypingAttributes :: (IsNSTextSelection nsTextSelection, IsNSDictionary value) => nsTextSelection -> value -> IO ()
setTypingAttributes nsTextSelection value =
  sendMessage nsTextSelection setTypingAttributesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRanges:affinity:granularity:@
initWithRanges_affinity_granularitySelector :: Selector '[Id NSArray, NSTextSelectionAffinity, NSTextSelectionGranularity] (Id NSTextSelection)
initWithRanges_affinity_granularitySelector = mkSelector "initWithRanges:affinity:granularity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextSelection)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithRange:affinity:granularity:@
initWithRange_affinity_granularitySelector :: Selector '[Id NSTextRange, NSTextSelectionAffinity, NSTextSelectionGranularity] (Id NSTextSelection)
initWithRange_affinity_granularitySelector = mkSelector "initWithRange:affinity:granularity:"

-- | @Selector@ for @initWithLocation:affinity:@
initWithLocation_affinitySelector :: Selector '[RawId, NSTextSelectionAffinity] (Id NSTextSelection)
initWithLocation_affinitySelector = mkSelector "initWithLocation:affinity:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextSelection)
initSelector = mkSelector "init"

-- | @Selector@ for @textSelectionWithTextRanges:@
textSelectionWithTextRangesSelector :: Selector '[Id NSArray] (Id NSTextSelection)
textSelectionWithTextRangesSelector = mkSelector "textSelectionWithTextRanges:"

-- | @Selector@ for @textRanges@
textRangesSelector :: Selector '[] (Id NSArray)
textRangesSelector = mkSelector "textRanges"

-- | @Selector@ for @granularity@
granularitySelector :: Selector '[] NSTextSelectionGranularity
granularitySelector = mkSelector "granularity"

-- | @Selector@ for @affinity@
affinitySelector :: Selector '[] NSTextSelectionAffinity
affinitySelector = mkSelector "affinity"

-- | @Selector@ for @transient@
transientSelector :: Selector '[] Bool
transientSelector = mkSelector "transient"

-- | @Selector@ for @anchorPositionOffset@
anchorPositionOffsetSelector :: Selector '[] CDouble
anchorPositionOffsetSelector = mkSelector "anchorPositionOffset"

-- | @Selector@ for @setAnchorPositionOffset:@
setAnchorPositionOffsetSelector :: Selector '[CDouble] ()
setAnchorPositionOffsetSelector = mkSelector "setAnchorPositionOffset:"

-- | @Selector@ for @logical@
logicalSelector :: Selector '[] Bool
logicalSelector = mkSelector "logical"

-- | @Selector@ for @setLogical:@
setLogicalSelector :: Selector '[Bool] ()
setLogicalSelector = mkSelector "setLogical:"

-- | @Selector@ for @secondarySelectionLocation@
secondarySelectionLocationSelector :: Selector '[] RawId
secondarySelectionLocationSelector = mkSelector "secondarySelectionLocation"

-- | @Selector@ for @setSecondarySelectionLocation:@
setSecondarySelectionLocationSelector :: Selector '[RawId] ()
setSecondarySelectionLocationSelector = mkSelector "setSecondarySelectionLocation:"

-- | @Selector@ for @typingAttributes@
typingAttributesSelector :: Selector '[] (Id NSDictionary)
typingAttributesSelector = mkSelector "typingAttributes"

-- | @Selector@ for @setTypingAttributes:@
setTypingAttributesSelector :: Selector '[Id NSDictionary] ()
setTypingAttributesSelector = mkSelector "setTypingAttributes:"

