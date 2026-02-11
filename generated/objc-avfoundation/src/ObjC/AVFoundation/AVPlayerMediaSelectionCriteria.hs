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
initWithPreferredLanguages_preferredMediaCharacteristics avPlayerMediaSelectionCriteria  preferredLanguages preferredMediaCharacteristics =
withObjCPtr preferredLanguages $ \raw_preferredLanguages ->
  withObjCPtr preferredMediaCharacteristics $ \raw_preferredMediaCharacteristics ->
      sendMsg avPlayerMediaSelectionCriteria (mkSelector "initWithPreferredLanguages:preferredMediaCharacteristics:") (retPtr retVoid) [argPtr (castPtr raw_preferredLanguages :: Ptr ()), argPtr (castPtr raw_preferredMediaCharacteristics :: Ptr ())] >>= ownedObject . castPtr

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
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristics avPlayerMediaSelectionCriteria  principalMediaCharacteristics preferredLanguages preferredMediaCharacteristics =
withObjCPtr principalMediaCharacteristics $ \raw_principalMediaCharacteristics ->
  withObjCPtr preferredLanguages $ \raw_preferredLanguages ->
    withObjCPtr preferredMediaCharacteristics $ \raw_preferredMediaCharacteristics ->
        sendMsg avPlayerMediaSelectionCriteria (mkSelector "initWithPrincipalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:") (retPtr retVoid) [argPtr (castPtr raw_principalMediaCharacteristics :: Ptr ()), argPtr (castPtr raw_preferredLanguages :: Ptr ()), argPtr (castPtr raw_preferredMediaCharacteristics :: Ptr ())] >>= ownedObject . castPtr

-- | @- preferredLanguages@
preferredLanguages :: IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria => avPlayerMediaSelectionCriteria -> IO (Id NSArray)
preferredLanguages avPlayerMediaSelectionCriteria  =
  sendMsg avPlayerMediaSelectionCriteria (mkSelector "preferredLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- preferredMediaCharacteristics@
preferredMediaCharacteristics :: IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria => avPlayerMediaSelectionCriteria -> IO (Id NSArray)
preferredMediaCharacteristics avPlayerMediaSelectionCriteria  =
  sendMsg avPlayerMediaSelectionCriteria (mkSelector "preferredMediaCharacteristics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- principalMediaCharacteristics@
principalMediaCharacteristics :: IsAVPlayerMediaSelectionCriteria avPlayerMediaSelectionCriteria => avPlayerMediaSelectionCriteria -> IO (Id NSArray)
principalMediaCharacteristics avPlayerMediaSelectionCriteria  =
  sendMsg avPlayerMediaSelectionCriteria (mkSelector "principalMediaCharacteristics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPreferredLanguages:preferredMediaCharacteristics:@
initWithPreferredLanguages_preferredMediaCharacteristicsSelector :: Selector
initWithPreferredLanguages_preferredMediaCharacteristicsSelector = mkSelector "initWithPreferredLanguages:preferredMediaCharacteristics:"

-- | @Selector@ for @initWithPrincipalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:@
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristicsSelector :: Selector
initWithPrincipalMediaCharacteristics_preferredLanguages_preferredMediaCharacteristicsSelector = mkSelector "initWithPrincipalMediaCharacteristics:preferredLanguages:preferredMediaCharacteristics:"

-- | @Selector@ for @preferredLanguages@
preferredLanguagesSelector :: Selector
preferredLanguagesSelector = mkSelector "preferredLanguages"

-- | @Selector@ for @preferredMediaCharacteristics@
preferredMediaCharacteristicsSelector :: Selector
preferredMediaCharacteristicsSelector = mkSelector "preferredMediaCharacteristics"

-- | @Selector@ for @principalMediaCharacteristics@
principalMediaCharacteristicsSelector :: Selector
principalMediaCharacteristicsSelector = mkSelector "principalMediaCharacteristics"

