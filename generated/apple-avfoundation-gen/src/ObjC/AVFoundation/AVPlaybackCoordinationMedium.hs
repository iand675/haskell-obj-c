{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVPlaybackCoordinationMedium@.
module ObjC.AVFoundation.AVPlaybackCoordinationMedium
  ( AVPlaybackCoordinationMedium
  , IsAVPlaybackCoordinationMedium(..)
  , init_
  , connectedPlaybackCoordinators
  , connectedPlaybackCoordinatorsSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes an AVPlaybackCoordinationMedium
--
-- ObjC selector: @- init@
init_ :: IsAVPlaybackCoordinationMedium avPlaybackCoordinationMedium => avPlaybackCoordinationMedium -> IO (Id AVPlaybackCoordinationMedium)
init_ avPlaybackCoordinationMedium =
  sendOwnedMessage avPlaybackCoordinationMedium initSelector

-- | All playback coordinators that are connected to the coordination medium.
--
-- Returns an array of all the AVPlayerPlaybackCoordinators that are connected to the coordination medium. This coordination is specifically for AVPlayerPlaybackCoordinators, and we exclude AVDelegatingPlaybackCoordinators. AVPlaybackCoordinator properties and methods are individually configurable for each playback coordinator. To ensure correct synchronized behavior across all local playback coordinators, any common AVPlaybackCoordinator properties and methods should be set and called on all playback coordinators in the coordination medium. The properties and methods @otherParticipants@, @setParticipantLimit:forWaitingOutSuspensionsWithReason:@, and @participantLimitForWaitingOutSuspensionsWithReason:@ refer specifically to remote participants that are coordinated through a group session rather than through the playback coordination medium. @otherParticipants@ only returns participants connected to the same group session. @setParticipantLimit@ and @participantLimitForWaitingOutSuspensionsWithReason@ affect only policies and behavior with the group session.
--
-- ObjC selector: @- connectedPlaybackCoordinators@
connectedPlaybackCoordinators :: IsAVPlaybackCoordinationMedium avPlaybackCoordinationMedium => avPlaybackCoordinationMedium -> IO (Id NSArray)
connectedPlaybackCoordinators avPlaybackCoordinationMedium =
  sendMessage avPlaybackCoordinationMedium connectedPlaybackCoordinatorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlaybackCoordinationMedium)
initSelector = mkSelector "init"

-- | @Selector@ for @connectedPlaybackCoordinators@
connectedPlaybackCoordinatorsSelector :: Selector '[] (Id NSArray)
connectedPlaybackCoordinatorsSelector = mkSelector "connectedPlaybackCoordinators"

