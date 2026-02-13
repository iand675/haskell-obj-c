{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that manages playback routing preferences.
--
-- This object manages instances of ``AVRoutingPlaybackParticipant`` for arbitration of media playback routing priorities and preferences on restricted playback interfaces. The playback routing arbiter is responsible for collecting and applying preferences, such as priorities in non-mixable audio routes and external playback states where the number of allowed players is limited.
--
-- Generated bindings for @AVRoutingPlaybackArbiter@.
module ObjC.AVRouting.AVRoutingPlaybackArbiter
  ( AVRoutingPlaybackArbiter
  , IsAVRoutingPlaybackArbiter(..)
  , sharedRoutingPlaybackArbiter
  , init_
  , new
  , preferredParticipantForNonMixableAudioRoutes
  , setPreferredParticipantForNonMixableAudioRoutes
  , preferredParticipantForExternalPlayback
  , setPreferredParticipantForExternalPlayback
  , initSelector
  , newSelector
  , preferredParticipantForExternalPlaybackSelector
  , preferredParticipantForNonMixableAudioRoutesSelector
  , setPreferredParticipantForExternalPlaybackSelector
  , setPreferredParticipantForNonMixableAudioRoutesSelector
  , sharedRoutingPlaybackArbiterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the singleton playback arbiter instance.
--
-- ObjC selector: @+ sharedRoutingPlaybackArbiter@
sharedRoutingPlaybackArbiter :: IO (Id AVRoutingPlaybackArbiter)
sharedRoutingPlaybackArbiter  =
  do
    cls' <- getRequiredClass "AVRoutingPlaybackArbiter"
    sendClassMessage cls' sharedRoutingPlaybackArbiterSelector

-- | @- init@
init_ :: IsAVRoutingPlaybackArbiter avRoutingPlaybackArbiter => avRoutingPlaybackArbiter -> IO (Id AVRoutingPlaybackArbiter)
init_ avRoutingPlaybackArbiter =
  sendOwnedMessage avRoutingPlaybackArbiter initSelector

-- | @+ new@
new :: IO (Id AVRoutingPlaybackArbiter)
new  =
  do
    cls' <- getRequiredClass "AVRoutingPlaybackArbiter"
    sendOwnedClassMessage cls' newSelector

-- | The participant that has priority to play audio when it's not possible to play multiple audio sources concurrently.
--
-- This participant takes precedence over all other participants to play audio in non-mixable audio routes when concurrent audio playback isn't possible, and only a single participant can play audio. The system unmutes this participant's audio, and mutes the audio of all other participants.
--
-- By default, this value is @nil@. When the current preferred participant finishes, the system sets this value to @nil@. If this value is @nil@, the arbiter doesn't impose any priority on the participants, and the participant that's unmuted is based on the existing selection mechanism.
--
-- ObjC selector: @- preferredParticipantForNonMixableAudioRoutes@
preferredParticipantForNonMixableAudioRoutes :: IsAVRoutingPlaybackArbiter avRoutingPlaybackArbiter => avRoutingPlaybackArbiter -> IO RawId
preferredParticipantForNonMixableAudioRoutes avRoutingPlaybackArbiter =
  sendMessage avRoutingPlaybackArbiter preferredParticipantForNonMixableAudioRoutesSelector

-- | The participant that has priority to play audio when it's not possible to play multiple audio sources concurrently.
--
-- This participant takes precedence over all other participants to play audio in non-mixable audio routes when concurrent audio playback isn't possible, and only a single participant can play audio. The system unmutes this participant's audio, and mutes the audio of all other participants.
--
-- By default, this value is @nil@. When the current preferred participant finishes, the system sets this value to @nil@. If this value is @nil@, the arbiter doesn't impose any priority on the participants, and the participant that's unmuted is based on the existing selection mechanism.
--
-- ObjC selector: @- setPreferredParticipantForNonMixableAudioRoutes:@
setPreferredParticipantForNonMixableAudioRoutes :: IsAVRoutingPlaybackArbiter avRoutingPlaybackArbiter => avRoutingPlaybackArbiter -> RawId -> IO ()
setPreferredParticipantForNonMixableAudioRoutes avRoutingPlaybackArbiter value =
  sendMessage avRoutingPlaybackArbiter setPreferredParticipantForNonMixableAudioRoutesSelector value

-- | The participant that has priority to play on external playback interfaces.
--
-- This participant takes precedence over all others to play on external playback interfaces (specifically for AirPlay video and Apple Lightning Digital AV Adapters).
--
-- By default, this value is @nil@. When the value is @nil@, the arbiter doesn't impose any priority on the participants, and the participant that is selected to playback externally falls back to the existing selection mechanism.
--
-- ObjC selector: @- preferredParticipantForExternalPlayback@
preferredParticipantForExternalPlayback :: IsAVRoutingPlaybackArbiter avRoutingPlaybackArbiter => avRoutingPlaybackArbiter -> IO RawId
preferredParticipantForExternalPlayback avRoutingPlaybackArbiter =
  sendMessage avRoutingPlaybackArbiter preferredParticipantForExternalPlaybackSelector

-- | The participant that has priority to play on external playback interfaces.
--
-- This participant takes precedence over all others to play on external playback interfaces (specifically for AirPlay video and Apple Lightning Digital AV Adapters).
--
-- By default, this value is @nil@. When the value is @nil@, the arbiter doesn't impose any priority on the participants, and the participant that is selected to playback externally falls back to the existing selection mechanism.
--
-- ObjC selector: @- setPreferredParticipantForExternalPlayback:@
setPreferredParticipantForExternalPlayback :: IsAVRoutingPlaybackArbiter avRoutingPlaybackArbiter => avRoutingPlaybackArbiter -> RawId -> IO ()
setPreferredParticipantForExternalPlayback avRoutingPlaybackArbiter value =
  sendMessage avRoutingPlaybackArbiter setPreferredParticipantForExternalPlaybackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedRoutingPlaybackArbiter@
sharedRoutingPlaybackArbiterSelector :: Selector '[] (Id AVRoutingPlaybackArbiter)
sharedRoutingPlaybackArbiterSelector = mkSelector "sharedRoutingPlaybackArbiter"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVRoutingPlaybackArbiter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVRoutingPlaybackArbiter)
newSelector = mkSelector "new"

-- | @Selector@ for @preferredParticipantForNonMixableAudioRoutes@
preferredParticipantForNonMixableAudioRoutesSelector :: Selector '[] RawId
preferredParticipantForNonMixableAudioRoutesSelector = mkSelector "preferredParticipantForNonMixableAudioRoutes"

-- | @Selector@ for @setPreferredParticipantForNonMixableAudioRoutes:@
setPreferredParticipantForNonMixableAudioRoutesSelector :: Selector '[RawId] ()
setPreferredParticipantForNonMixableAudioRoutesSelector = mkSelector "setPreferredParticipantForNonMixableAudioRoutes:"

-- | @Selector@ for @preferredParticipantForExternalPlayback@
preferredParticipantForExternalPlaybackSelector :: Selector '[] RawId
preferredParticipantForExternalPlaybackSelector = mkSelector "preferredParticipantForExternalPlayback"

-- | @Selector@ for @setPreferredParticipantForExternalPlayback:@
setPreferredParticipantForExternalPlaybackSelector :: Selector '[RawId] ()
setPreferredParticipantForExternalPlaybackSelector = mkSelector "setPreferredParticipantForExternalPlayback:"

