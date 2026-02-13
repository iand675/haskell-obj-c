{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , associationsSelector
  , initSelector
  , kindSelector
  , labelsSelector
  , newSelector
  , stateOfMindWithDate_kind_valence_labels_associationsSelector
  , stateOfMindWithDate_kind_valence_labels_associations_metadataSelector
  , valenceClassificationSelector
  , valenceSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' stateOfMindWithDate_kind_valence_labels_associationsSelector (toNSDate date) kind valence (toNSArray labels) (toNSArray associations)

-- | Creates a new log describing an experienced emotion at a moment in time.
--
-- ObjC selector: @+ stateOfMindWithDate:kind:valence:labels:associations:metadata:@
stateOfMindWithDate_kind_valence_labels_associations_metadata :: (IsNSDate date, IsNSArray labels, IsNSArray associations, IsNSDictionary metadata) => date -> HKStateOfMindKind -> CDouble -> labels -> associations -> metadata -> IO (Id HKStateOfMind)
stateOfMindWithDate_kind_valence_labels_associations_metadata date kind valence labels associations metadata =
  do
    cls' <- getRequiredClass "HKStateOfMind"
    sendClassMessage cls' stateOfMindWithDate_kind_valence_labels_associations_metadataSelector (toNSDate date) kind valence (toNSArray labels) (toNSArray associations) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO (Id HKStateOfMind)
init_ hkStateOfMind =
  sendOwnedMessage hkStateOfMind initSelector

-- | @+ new@
new :: IO (Id HKStateOfMind)
new  =
  do
    cls' <- getRequiredClass "HKStateOfMind"
    sendOwnedClassMessage cls' newSelector

-- | A description of the kind of feeling type captured by this state of mind.
--
-- Feeling types can be understood by the timeframe considered to create this log, possibly indicated by the context used to create it. For example, a @momentary emotion@ log might be in response to 'how are you feeling right now?' while a @daily mood@ log might be in response to 'how have you been feeling today?'.
--
-- ObjC selector: @- kind@
kind :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO HKStateOfMindKind
kind hkStateOfMind =
  sendMessage hkStateOfMind kindSelector

-- | A signed, self-reported measure of how positive or negative one is feeling, on a continuous scale from -1 to +1.
--
-- ObjC selector: @- valence@
valence :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO CDouble
valence hkStateOfMind =
  sendMessage hkStateOfMind valenceSelector

-- | A general region of pleasantness based on this sample's valence value.
--
-- ObjC selector: @- valenceClassification@
valenceClassification :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO HKStateOfMindValenceClassification
valenceClassification hkStateOfMind =
  sendMessage hkStateOfMind valenceClassificationSelector

-- | Zero or more specific sentiments selected to represent a felt experience.
--
-- ObjC selector: @- labels@
labels :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO (Id NSArray)
labels hkStateOfMind =
  sendMessage hkStateOfMind labelsSelector

-- | Zero or more facets of life with which this felt experience is associated.
--
-- ObjC selector: @- associations@
associations :: IsHKStateOfMind hkStateOfMind => hkStateOfMind -> IO (Id NSArray)
associations hkStateOfMind =
  sendMessage hkStateOfMind associationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateOfMindWithDate:kind:valence:labels:associations:@
stateOfMindWithDate_kind_valence_labels_associationsSelector :: Selector '[Id NSDate, HKStateOfMindKind, CDouble, Id NSArray, Id NSArray] (Id HKStateOfMind)
stateOfMindWithDate_kind_valence_labels_associationsSelector = mkSelector "stateOfMindWithDate:kind:valence:labels:associations:"

-- | @Selector@ for @stateOfMindWithDate:kind:valence:labels:associations:metadata:@
stateOfMindWithDate_kind_valence_labels_associations_metadataSelector :: Selector '[Id NSDate, HKStateOfMindKind, CDouble, Id NSArray, Id NSArray, Id NSDictionary] (Id HKStateOfMind)
stateOfMindWithDate_kind_valence_labels_associations_metadataSelector = mkSelector "stateOfMindWithDate:kind:valence:labels:associations:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKStateOfMind)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKStateOfMind)
newSelector = mkSelector "new"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] HKStateOfMindKind
kindSelector = mkSelector "kind"

-- | @Selector@ for @valence@
valenceSelector :: Selector '[] CDouble
valenceSelector = mkSelector "valence"

-- | @Selector@ for @valenceClassification@
valenceClassificationSelector :: Selector '[] HKStateOfMindValenceClassification
valenceClassificationSelector = mkSelector "valenceClassification"

-- | @Selector@ for @labels@
labelsSelector :: Selector '[] (Id NSArray)
labelsSelector = mkSelector "labels"

-- | @Selector@ for @associations@
associationsSelector :: Selector '[] (Id NSArray)
associationsSelector = mkSelector "associations"

