{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents how one feels, including descriptors of a feeling and optionally, its source.
--
-- Generated bindings for @HKStateOfMind@.
module ObjC.HealthKit.HKStateOfMind
  ( HKStateOfMind
  , IsHKStateOfMind(..)
  , stateOfMindWithDate_kind_valence_labels_associations
  , stateOfMindWithDate_kind_valence_labels_associations_metadata
  , init_
  , new
  , kind
  , valence
  , valenceClassification
  , labels
  , associations
  , stateOfMindWithDate_kind_valence_labels_associationsSelector
  , stateOfMindWithDate_kind_valence_labels_associations_metadataSelector
  , initSelector
  , newSelector
  , kindSelector
  , valenceSelector
  , valenceClassificationSelector
  , labelsSelector
  , associationsSelector

  -- * Enum types
  , HKStateOfMindKind(HKStateOfMindKind)
  , pattern HKStateOfMindKindMomentaryEmotion
  , pattern HKStateOfMindKindDailyMood
  , HKStateOfMindValenceClassification(HKStateOfMindValenceClassification)
  , pattern HKStateOfMindValenceClassificationVeryUnpleasant
  , pattern HKStateOfMindValenceClassificationUnpleasant
  , pattern HKStateOfMindValenceClassificationSlightlyUnpleasant
  , pattern HKStateOfMindValenceClassificationNeutral
  , pattern HKStateOfMindValenceClassificationSlightlyPleasant
  , pattern HKStateOfMindValenceClassificationPleasant
  , pattern HKStateOfMindValenceClassificationVeryPleasant

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

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a new log describing an experienced emotion at a moment in time.
--
-- ObjC selector: @+ stateOfMindWithDate:kind:valence:labels:associations:@
stateOfMindWithDate_kind_valence_labels_associations :: (IsNSDate date, IsNSArray labels, IsNSArray associations) => date -> HKStateOfMindKind -> CDouble -> labels -> associations -> IO (Id HKStateOfMind)
stateOfMindWithDate_kind_valence_labels_associations date kind valence labels associations =
  do
    cls' <- getRequiredClass "HKStateOfMind"
    withObjCPtr date $ \raw_date ->
      withObjCPtr labels $ \raw_labels ->
        withObjCPtr associations $ \raw_associations ->
          sendClassMsg cls' (mkSelector "stateOfMindWithDate:kind:valence:labels:associations:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCLong (coerce kind), argCDouble valence, argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_associations :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a new log describing an experienced emotion at a moment in time.
--
-- ObjC selector: @+ stateOfMindWithDate:kind:valence:labels:associations:metadata:@
stateOfMindWithDate_kind_valence_labels_associations_metadata :: (IsNSDate date, IsNSArray labels, IsNSArray associations, IsNSDictionary metadata) => date -> HKStateOfMindKind -> CDouble -> labels -> associations -> metadata -> IO (Id HKStateOfMind)
stateOfMindWithDate_kind_valence_labels_associations_metadata date kind valence labels associations metadata =
  do
    cls' <- getRequiredClass "HKStateOfMind"
    withObjCPtr date $ \raw_date ->
      withObjCPtr labels $ \raw_labels ->
        withObjCPtr associations $ \raw_associations ->
          withObjCPtr metadata $ \raw_metadata ->
            sendClassMsg cls' (mkSelector "stateOfMindWithDate:kind:valence:labels:associations:metadata:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCLong (coerce kind), argCDouble valence, argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_associations :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO (Id HKStateOfMind)
init_ hkStateOfMind  =
    sendMsg hkStateOfMind (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKStateOfMind)
new  =
  do
    cls' <- getRequiredClass "HKStateOfMind"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A description of the kind of feeling type captured by this state of mind.
--
-- Feeling types can be understood by the timeframe considered to create this log, possibly indicated by the context used to create it. For example, a @momentary emotion@ log might be in response to 'how are you feeling right now?' while a @daily mood@ log might be in response to 'how have you been feeling today?'.
--
-- ObjC selector: @- kind@
kind :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO HKStateOfMindKind
kind hkStateOfMind  =
    fmap (coerce :: CLong -> HKStateOfMindKind) $ sendMsg hkStateOfMind (mkSelector "kind") retCLong []

-- | A signed, self-reported measure of how positive or negative one is feeling, on a continuous scale from -1 to +1.
--
-- ObjC selector: @- valence@
valence :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO CDouble
valence hkStateOfMind  =
    sendMsg hkStateOfMind (mkSelector "valence") retCDouble []

-- | A general region of pleasantness based on this sample's valence value.
--
-- ObjC selector: @- valenceClassification@
valenceClassification :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO HKStateOfMindValenceClassification
valenceClassification hkStateOfMind  =
    fmap (coerce :: CLong -> HKStateOfMindValenceClassification) $ sendMsg hkStateOfMind (mkSelector "valenceClassification") retCLong []

-- | Zero or more specific sentiments selected to represent a felt experience.
--
-- ObjC selector: @- labels@
labels :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO (Id NSArray)
labels hkStateOfMind  =
    sendMsg hkStateOfMind (mkSelector "labels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Zero or more facets of life with which this felt experience is associated.
--
-- ObjC selector: @- associations@
associations :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO (Id NSArray)
associations hkStateOfMind  =
    sendMsg hkStateOfMind (mkSelector "associations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateOfMindWithDate:kind:valence:labels:associations:@
stateOfMindWithDate_kind_valence_labels_associationsSelector :: Selector
stateOfMindWithDate_kind_valence_labels_associationsSelector = mkSelector "stateOfMindWithDate:kind:valence:labels:associations:"

-- | @Selector@ for @stateOfMindWithDate:kind:valence:labels:associations:metadata:@
stateOfMindWithDate_kind_valence_labels_associations_metadataSelector :: Selector
stateOfMindWithDate_kind_valence_labels_associations_metadataSelector = mkSelector "stateOfMindWithDate:kind:valence:labels:associations:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @valence@
valenceSelector :: Selector
valenceSelector = mkSelector "valence"

-- | @Selector@ for @valenceClassification@
valenceClassificationSelector :: Selector
valenceClassificationSelector = mkSelector "valenceClassification"

-- | @Selector@ for @labels@
labelsSelector :: Selector
labelsSelector = mkSelector "labels"

-- | @Selector@ for @associations@
associationsSelector :: Selector
associationsSelector = mkSelector "associations"

