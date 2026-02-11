{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRanges_affinity_granularitySelector
  , initWithCoderSelector
  , initWithRange_affinity_granularitySelector
  , initWithLocation_affinitySelector
  , initSelector
  , textSelectionWithTextRangesSelector
  , textRangesSelector
  , granularitySelector
  , affinitySelector
  , transientSelector
  , anchorPositionOffsetSelector
  , setAnchorPositionOffsetSelector
  , logicalSelector
  , setLogicalSelector
  , secondarySelectionLocationSelector
  , setSecondarySelectionLocationSelector
  , typingAttributesSelector
  , setTypingAttributesSelector

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

-- | @- initWithRanges:affinity:granularity:@
initWithRanges_affinity_granularity :: (IsNSTextSelection nsTextSelection, IsNSArray textRanges) => nsTextSelection -> textRanges -> NSTextSelectionAffinity -> NSTextSelectionGranularity -> IO (Id NSTextSelection)
initWithRanges_affinity_granularity nsTextSelection  textRanges affinity granularity =
  withObjCPtr textRanges $ \raw_textRanges ->
      sendMsg nsTextSelection (mkSelector "initWithRanges:affinity:granularity:") (retPtr retVoid) [argPtr (castPtr raw_textRanges :: Ptr ()), argCLong (coerce affinity), argCLong (coerce granularity)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextSelection nsTextSelection, IsNSCoder coder) => nsTextSelection -> coder -> IO (Id NSTextSelection)
initWithCoder nsTextSelection  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsTextSelection (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRange:affinity:granularity:@
initWithRange_affinity_granularity :: (IsNSTextSelection nsTextSelection, IsNSTextRange range) => nsTextSelection -> range -> NSTextSelectionAffinity -> NSTextSelectionGranularity -> IO (Id NSTextSelection)
initWithRange_affinity_granularity nsTextSelection  range affinity granularity =
  withObjCPtr range $ \raw_range ->
      sendMsg nsTextSelection (mkSelector "initWithRange:affinity:granularity:") (retPtr retVoid) [argPtr (castPtr raw_range :: Ptr ()), argCLong (coerce affinity), argCLong (coerce granularity)] >>= ownedObject . castPtr

-- | @- initWithLocation:affinity:@
initWithLocation_affinity :: IsNSTextSelection nsTextSelection => nsTextSelection -> RawId -> NSTextSelectionAffinity -> IO (Id NSTextSelection)
initWithLocation_affinity nsTextSelection  location affinity =
    sendMsg nsTextSelection (mkSelector "initWithLocation:affinity:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ()), argCLong (coerce affinity)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO (Id NSTextSelection)
init_ nsTextSelection  =
    sendMsg nsTextSelection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- textSelectionWithTextRanges:@
textSelectionWithTextRanges :: (IsNSTextSelection nsTextSelection, IsNSArray textRanges) => nsTextSelection -> textRanges -> IO (Id NSTextSelection)
textSelectionWithTextRanges nsTextSelection  textRanges =
  withObjCPtr textRanges $ \raw_textRanges ->
      sendMsg nsTextSelection (mkSelector "textSelectionWithTextRanges:") (retPtr retVoid) [argPtr (castPtr raw_textRanges :: Ptr ())] >>= retainedObject . castPtr

-- | @- textRanges@
textRanges :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO (Id NSArray)
textRanges nsTextSelection  =
    sendMsg nsTextSelection (mkSelector "textRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- granularity@
granularity :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO NSTextSelectionGranularity
granularity nsTextSelection  =
    fmap (coerce :: CLong -> NSTextSelectionGranularity) $ sendMsg nsTextSelection (mkSelector "granularity") retCLong []

-- | @- affinity@
affinity :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO NSTextSelectionAffinity
affinity nsTextSelection  =
    fmap (coerce :: CLong -> NSTextSelectionAffinity) $ sendMsg nsTextSelection (mkSelector "affinity") retCLong []

-- | @- transient@
transient :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO Bool
transient nsTextSelection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextSelection (mkSelector "transient") retCULong []

-- | @- anchorPositionOffset@
anchorPositionOffset :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO CDouble
anchorPositionOffset nsTextSelection  =
    sendMsg nsTextSelection (mkSelector "anchorPositionOffset") retCDouble []

-- | @- setAnchorPositionOffset:@
setAnchorPositionOffset :: IsNSTextSelection nsTextSelection => nsTextSelection -> CDouble -> IO ()
setAnchorPositionOffset nsTextSelection  value =
    sendMsg nsTextSelection (mkSelector "setAnchorPositionOffset:") retVoid [argCDouble value]

-- | @- logical@
logical :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO Bool
logical nsTextSelection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextSelection (mkSelector "logical") retCULong []

-- | @- setLogical:@
setLogical :: IsNSTextSelection nsTextSelection => nsTextSelection -> Bool -> IO ()
setLogical nsTextSelection  value =
    sendMsg nsTextSelection (mkSelector "setLogical:") retVoid [argCULong (if value then 1 else 0)]

-- | @- secondarySelectionLocation@
secondarySelectionLocation :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO RawId
secondarySelectionLocation nsTextSelection  =
    fmap (RawId . castPtr) $ sendMsg nsTextSelection (mkSelector "secondarySelectionLocation") (retPtr retVoid) []

-- | @- setSecondarySelectionLocation:@
setSecondarySelectionLocation :: IsNSTextSelection nsTextSelection => nsTextSelection -> RawId -> IO ()
setSecondarySelectionLocation nsTextSelection  value =
    sendMsg nsTextSelection (mkSelector "setSecondarySelectionLocation:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- typingAttributes@
typingAttributes :: IsNSTextSelection nsTextSelection => nsTextSelection -> IO (Id NSDictionary)
typingAttributes nsTextSelection  =
    sendMsg nsTextSelection (mkSelector "typingAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTypingAttributes:@
setTypingAttributes :: (IsNSTextSelection nsTextSelection, IsNSDictionary value) => nsTextSelection -> value -> IO ()
setTypingAttributes nsTextSelection  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextSelection (mkSelector "setTypingAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRanges:affinity:granularity:@
initWithRanges_affinity_granularitySelector :: Selector
initWithRanges_affinity_granularitySelector = mkSelector "initWithRanges:affinity:granularity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithRange:affinity:granularity:@
initWithRange_affinity_granularitySelector :: Selector
initWithRange_affinity_granularitySelector = mkSelector "initWithRange:affinity:granularity:"

-- | @Selector@ for @initWithLocation:affinity:@
initWithLocation_affinitySelector :: Selector
initWithLocation_affinitySelector = mkSelector "initWithLocation:affinity:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @textSelectionWithTextRanges:@
textSelectionWithTextRangesSelector :: Selector
textSelectionWithTextRangesSelector = mkSelector "textSelectionWithTextRanges:"

-- | @Selector@ for @textRanges@
textRangesSelector :: Selector
textRangesSelector = mkSelector "textRanges"

-- | @Selector@ for @granularity@
granularitySelector :: Selector
granularitySelector = mkSelector "granularity"

-- | @Selector@ for @affinity@
affinitySelector :: Selector
affinitySelector = mkSelector "affinity"

-- | @Selector@ for @transient@
transientSelector :: Selector
transientSelector = mkSelector "transient"

-- | @Selector@ for @anchorPositionOffset@
anchorPositionOffsetSelector :: Selector
anchorPositionOffsetSelector = mkSelector "anchorPositionOffset"

-- | @Selector@ for @setAnchorPositionOffset:@
setAnchorPositionOffsetSelector :: Selector
setAnchorPositionOffsetSelector = mkSelector "setAnchorPositionOffset:"

-- | @Selector@ for @logical@
logicalSelector :: Selector
logicalSelector = mkSelector "logical"

-- | @Selector@ for @setLogical:@
setLogicalSelector :: Selector
setLogicalSelector = mkSelector "setLogical:"

-- | @Selector@ for @secondarySelectionLocation@
secondarySelectionLocationSelector :: Selector
secondarySelectionLocationSelector = mkSelector "secondarySelectionLocation"

-- | @Selector@ for @setSecondarySelectionLocation:@
setSecondarySelectionLocationSelector :: Selector
setSecondarySelectionLocationSelector = mkSelector "setSecondarySelectionLocation:"

-- | @Selector@ for @typingAttributes@
typingAttributesSelector :: Selector
typingAttributesSelector = mkSelector "typingAttributes"

-- | @Selector@ for @setTypingAttributes:@
setTypingAttributesSelector :: Selector
setTypingAttributesSelector = mkSelector "setTypingAttributes:"

