{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerMediaSelectionCriteria
--
-- The preferred languages and media characteristics for a player.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerMediaSelectionCriteria@.
module ObjC.AVFoundation.AVPlayerMediaSelectionCriteria
  ( AVPlayerMediaSelectionCriteria
  , IsAVPlayerMediaSelectionCriteria(..)
  , initWithPreferredLanguages_preferredMediaCharacteristics
  , initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristics
  , preferredLanguages
  , preferredMediaCharacteristics
  , principalMediaCharacteristics
  , initWithPreferredLanguages_preferredMediaCharacteristicsSelector
  , initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristicsSelector
  , preferredLanguagesSelector
  , preferredMediaCharacteristicsSelector
  , principalMediaCharacteristicsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithPreferredLanguages:preferredMediaCharacteristics:
--
-- Creates an instance of AVPlayerMediaSelectionCriteria.
--
-- @preferredLanguages@ — An NSArray of NSStrings containing language identifiers, in order of desirability, that are preferred for selection. Can be nil.
--
-- @preferredMediaCharacteristics@ — An NSArray of AVMediaCharacteristics indicating additional media characteristics, in order of desirability, that are preferred when selecting media with the characteristic for which the receiver is set on the AVPlayer as the selection criteria. Can be nil.
--
-- Returns: An instance of AVPlayerMediaSelectionCriteria.
--
-- ObjC selector: @- initWithPreferredLanguages:preferredMediaCharacteristics:@
initWithPreferredLanguages_preferredMediaCharacteristics :: (IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria, IsNSArray preferredLanguages, IsNSArray preferredMediaCharacteristics) => avPlayerMediaSelectionCriteria -> preferredLanguages -> preferredMediaCharacteristics -> IO (Id AVPlayerMediaSelectionCriteria)
initWithPreferredLanguages_preferredMediaCharacteristics avPlayerMediaSelectionCriteria preferredLanguages preferredMediaCharacteristics =
  sendOwnedMessage avPlayerMediaSelectionCriteria initWithPreferredLanguages_preferredMediaCharacteristicsSelector (toNSArray preferredLanguages) (toNSArray preferredMediaCharacteristics)

-- | initWithPrincipalMediaCharacteristics:principalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:
--
-- Creates an instance of AVPlayerMediaSelectionCriteria.
--
-- @principalMediaCharacteristics@ — An NSArray of AVMediaCharacteristics indicating media characteristics that are considered essential when selecting media with the characteristic for which the receiver is set on the AVPlayer as the selection criteria. Can be nil.
--
-- @preferredLanguages@ — An NSArray of NSStrings containing language identifiers, in order of desirability, that are preferred for selection. Can be nil.
--
-- @preferredMediaCharacteristics@ — An NSArray of AVMediaCharacteristics indicating additional media characteristics, in order of desirability, that are preferred when selecting media with the characteristic for which the receiver is set on the AVPlayer as the selection criteria. Can be nil.
--
-- Returns: An instance of AVPlayerMediaSelectionCriteria.
--
-- Note that even though principal media characteristics, when present, will override language preferences when making a selection within a specific media selection group, language preferences may still pertain to selections in other groups. For example, language preferences for the group that corresponds to the audible characteristic may be considered when choosing whether or not to select non-forced subtitles for translation purposes.
--
-- ObjC selector: @- initWithPrincipalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:@
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristics :: (IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria, IsNSArray principalMediaCharacteristics, IsNSArray preferredLanguages, IsNSArray preferredMediaCharacteristics) => avPlayerMediaSelectionCriteria -> principalMediaCharacteristics -> preferredLanguages -> preferredMediaCharacteristics -> IO (Id AVPlayerMediaSelectionCriteria)
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristics avPlayerMediaSelectionCriteria principalMediaCharacteristics preferredLanguages preferredMediaCharacteristics =
  sendOwnedMessage avPlayerMediaSelectionCriteria initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristicsSelector (toNSArray principalMediaCharacteristics) (toNSArray preferredLanguages) (toNSArray preferredMediaCharacteristics)

-- | @- preferredLanguages@
preferredLanguages :: IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria => avPlayerMediaSelectionCriteria -> IO (Id NSArray)
preferredLanguages avPlayerMediaSelectionCriteria =
  sendMessage avPlayerMediaSelectionCriteria preferredLanguagesSelector

-- | @- preferredMediaCharacteristics@
preferredMediaCharacteristics :: IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria => avPlayerMediaSelectionCriteria -> IO (Id NSArray)
preferredMediaCharacteristics avPlayerMediaSelectionCriteria =
  sendMessage avPlayerMediaSelectionCriteria preferredMediaCharacteristicsSelector

-- | @- principalMediaCharacteristics@
principalMediaCharacteristics :: IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria => avPlayerMediaSelectionCriteria -> IO (Id NSArray)
principalMediaCharacteristics avPlayerMediaSelectionCriteria =
  sendMessage avPlayerMediaSelectionCriteria principalMediaCharacteristicsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPreferredLanguages:preferredMediaCharacteristics:@
initWithPreferredLanguages_preferredMediaCharacteristicsSelector :: Selector '[Id NSArray, Id NSArray] (Id AVPlayerMediaSelectionCriteria)
initWithPreferredLanguages_preferredMediaCharacteristicsSelector = mkSelector "initWithPreferredLanguages:preferredMediaCharacteristics:"

-- | @Selector@ for @initWithPrincipalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:@
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristicsSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id AVPlayerMediaSelectionCriteria)
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristicsSelector = mkSelector "initWithPrincipalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:"

-- | @Selector@ for @preferredLanguages@
preferredLanguagesSelector :: Selector '[] (Id NSArray)
preferredLanguagesSelector = mkSelector "preferredLanguages"

-- | @Selector@ for @preferredMediaCharacteristics@
preferredMediaCharacteristicsSelector :: Selector '[] (Id NSArray)
preferredMediaCharacteristicsSelector = mkSelector "preferredMediaCharacteristics"

-- | @Selector@ for @principalMediaCharacteristics@
principalMediaCharacteristicsSelector :: Selector '[] (Id NSArray)
principalMediaCharacteristicsSelector = mkSelector "principalMediaCharacteristics"

