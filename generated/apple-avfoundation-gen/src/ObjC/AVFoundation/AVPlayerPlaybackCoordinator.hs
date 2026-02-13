{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlaybackCoordinator subclass for controlling an AVPlayer
--
-- While the coordinator is connected to other participants, it will intercept rate changes and seeks issued to the player to share these with other participants if appropriate. Clients of AVPlayer can thus use the AVPlayer interfaces to modify the playback state of connected participants. When appropriate, the coordinator will also impose rate changes and seeks from other participants on the player. If this occurs, the corresponding notifications will carry an originating participant in their payload. See AVPlayer's playbackCoordinator property for more details about player behavior changes. AVPlayerPlaybackCoordinator may begin suspensions on behalf of the player when the player's timeControlStatus changes from AVPlayerTimeControlStatusPlaying to AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate or AVPlayerTimeControlStatusPaused. These suspensions will end when the player's timeControlStatus changes back to AVPlayerTimeControlStatusPlaying. This means that a suspension that begun because the player entered a waiting state, will end automatically when the player is done waiting. A suspension that begun because the player paused, will only end once the player's rate changes back to non-zero.
--
-- Generated bindings for @AVPlayerPlaybackCoordinator@.
module ObjC.AVFoundation.AVPlayerPlaybackCoordinator
  ( AVPlayerPlaybackCoordinator
  , IsAVPlayerPlaybackCoordinator(..)
  , init_
  , new
  , coordinateUsingCoordinationMedium_error
  , player
  , delegate
  , setDelegate
  , playbackCoordinationMedium
  , coordinateUsingCoordinationMedium_errorSelector
  , delegateSelector
  , initSelector
  , newSelector
  , playbackCoordinationMediumSelector
  , playerSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerPlaybackCoordinator avPlayerPlaybackCoordinator => avPlayerPlaybackCoordinator -> IO (Id AVPlayerPlaybackCoordinator)
init_ avPlayerPlaybackCoordinator =
  sendOwnedMessage avPlayerPlaybackCoordinator initSelector

-- | @+ new@
new :: IO (Id AVPlayerPlaybackCoordinator)
new  =
  do
    cls' <- getRequiredClass "AVPlayerPlaybackCoordinator"
    sendOwnedClassMessage cls' newSelector

-- | Connects the playback coordinator to the coordination medium
--
-- This connects the playback coordinator to a coordination medium to enable sending and receiving messages from other connected playback coordinators. If the coordination medium is non-NULL, this will connect the playback coordinator to the specified coordination medium. If the coordination medium is set to NULL, this will disconnect the playback coordinator from the playback coordination medium. The player will no longer be coordinated with the other players connected to the coordination medium. The playback coordinator can either only coordinate with local players through an AVPlaybackCoordinationMedium or coordinate with a remote group session through the @coordinateWithSession@ API. If the client attempts to connect to an AVPlaybackCoordinationMedium while already connected to a group session, this method will populate the outError parameter If the playback coordinator successfully connects to the coordination medium or disconnects from a coordination medium, the @outError@ parameter will be nil. If the playback coordinator fails to connect to the specified coordination medium, the @outError@ parameter will describe what went wrong.
--
-- - Parameter coordinationMedium: The coordination medium the playback coordinator connects to. If NULL, the playback coordinator disconnects from any existing coordination medium. - Parameter outError: A pointer to an NSError object that will be populated with failure information if connecting to or disconnecting from the coordination medium fails.
--
-- ObjC selector: @- coordinateUsingCoordinationMedium:error:@
coordinateUsingCoordinationMedium_error :: (IsAVPlayerPlaybackCoordinator avPlayerPlaybackCoordinator, IsAVPlaybackCoordinationMedium coordinationMedium, IsNSError outError) => avPlayerPlaybackCoordinator -> coordinationMedium -> outError -> IO Bool
coordinateUsingCoordinationMedium_error avPlayerPlaybackCoordinator coordinationMedium outError =
  sendMessage avPlayerPlaybackCoordinator coordinateUsingCoordinationMedium_errorSelector (toAVPlaybackCoordinationMedium coordinationMedium) (toNSError outError)

-- | The AVPlayer this coordinator is controlling.
--
-- ObjC selector: @- player@
player :: IsAVPlayerPlaybackCoordinator avPlayerPlaybackCoordinator => avPlayerPlaybackCoordinator -> IO (Id AVPlayer)
player avPlayerPlaybackCoordinator =
  sendMessage avPlayerPlaybackCoordinator playerSelector

-- | An object implementing the AVPlaybackCoordinatorDelegate protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerPlaybackCoordinator avPlayerPlaybackCoordinator => avPlayerPlaybackCoordinator -> IO RawId
delegate avPlayerPlaybackCoordinator =
  sendMessage avPlayerPlaybackCoordinator delegateSelector

-- | An object implementing the AVPlaybackCoordinatorDelegate protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVPlayerPlaybackCoordinator avPlayerPlaybackCoordinator => avPlayerPlaybackCoordinator -> RawId -> IO ()
setDelegate avPlayerPlaybackCoordinator value =
  sendMessage avPlayerPlaybackCoordinator setDelegateSelector value

-- | The AVPlaybackCoordinationMedium this playback coordinator is connected to.
--
-- This is the AVPlaybackCoordinationMedium the playback coordinator is connected to. If not NULL, the playback coordinator is connected to the specified coordination medium. The playback coordinator is not available to coordinate with a group session. If NULL, the playback coordinator is not connected to any playback coordination medium. The playback coordinator is available to coordinate with a group session through the @coordinateWithSession@ API.
--
-- ObjC selector: @- playbackCoordinationMedium@
playbackCoordinationMedium :: IsAVPlayerPlaybackCoordinator avPlayerPlaybackCoordinator => avPlayerPlaybackCoordinator -> IO (Id AVPlaybackCoordinationMedium)
playbackCoordinationMedium avPlayerPlaybackCoordinator =
  sendMessage avPlayerPlaybackCoordinator playbackCoordinationMediumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerPlaybackCoordinator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerPlaybackCoordinator)
newSelector = mkSelector "new"

-- | @Selector@ for @coordinateUsingCoordinationMedium:error:@
coordinateUsingCoordinationMedium_errorSelector :: Selector '[Id AVPlaybackCoordinationMedium, Id NSError] Bool
coordinateUsingCoordinationMedium_errorSelector = mkSelector "coordinateUsingCoordinationMedium:error:"

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id AVPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @playbackCoordinationMedium@
playbackCoordinationMediumSelector :: Selector '[] (Id AVPlaybackCoordinationMedium)
playbackCoordinationMediumSelector = mkSelector "playbackCoordinationMedium"

