{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPNowPlayingSession@.
module ObjC.MediaPlayer.MPNowPlayingSession
  ( MPNowPlayingSession
  , IsMPNowPlayingSession(..)
  , initWithPlayers
  , new
  , init_
  , becomeActiveIfPossibleWithCompletion
  , addPlayer
  , removePlayer
  , players
  , delegate
  , setDelegate
  , automaticallyPublishesNowPlayingInfo
  , setAutomaticallyPublishesNowPlayingInfo
  , nowPlayingInfoCenter
  , remoteCommandCenter
  , canBecomeActive
  , active
  , activeSelector
  , addPlayerSelector
  , automaticallyPublishesNowPlayingInfoSelector
  , becomeActiveIfPossibleWithCompletionSelector
  , canBecomeActiveSelector
  , delegateSelector
  , initSelector
  , initWithPlayersSelector
  , newSelector
  , nowPlayingInfoCenterSelector
  , playersSelector
  , remoteCommandCenterSelector
  , removePlayerSelector
  , setAutomaticallyPublishesNowPlayingInfoSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a session associated with a given AVPlayer instance. This will assert if players is nil or empty.
--
-- ObjC selector: @- initWithPlayers:@
initWithPlayers :: (IsMPNowPlayingSession mpNowPlayingSession, IsNSArray players) => mpNowPlayingSession -> players -> IO (Id MPNowPlayingSession)
initWithPlayers mpNowPlayingSession players =
  sendOwnedMessage mpNowPlayingSession initWithPlayersSelector (toNSArray players)

-- | @+ new@
new :: IO (Id MPNowPlayingSession)
new  =
  do
    cls' <- getRequiredClass "MPNowPlayingSession"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id MPNowPlayingSession)
init_ mpNowPlayingSession =
  sendOwnedMessage mpNowPlayingSession initSelector

-- | Asks the system to make this session the active now playing sessin for the App.
--
-- ObjC selector: @- becomeActiveIfPossibleWithCompletion:@
becomeActiveIfPossibleWithCompletion :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> Ptr () -> IO ()
becomeActiveIfPossibleWithCompletion mpNowPlayingSession completion =
  sendMessage mpNowPlayingSession becomeActiveIfPossibleWithCompletionSelector completion

-- | Add AVPlayer instance to this session.
--
-- ObjC selector: @- addPlayer:@
addPlayer :: (IsMPNowPlayingSession mpNowPlayingSession, IsAVPlayer player) => mpNowPlayingSession -> player -> IO ()
addPlayer mpNowPlayingSession player =
  sendMessage mpNowPlayingSession addPlayerSelector (toAVPlayer player)

-- | Remove AVPlayer instance from this session.
--
-- ObjC selector: @- removePlayer:@
removePlayer :: (IsMPNowPlayingSession mpNowPlayingSession, IsAVPlayer player) => mpNowPlayingSession -> player -> IO ()
removePlayer mpNowPlayingSession player =
  sendMessage mpNowPlayingSession removePlayerSelector (toAVPlayer player)

-- | AVPlayer instances associated with this session.
--
-- ObjC selector: @- players@
players :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id NSArray)
players mpNowPlayingSession =
  sendMessage mpNowPlayingSession playersSelector

-- | @- delegate@
delegate :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO RawId
delegate mpNowPlayingSession =
  sendMessage mpNowPlayingSession delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> RawId -> IO ()
setDelegate mpNowPlayingSession value =
  sendMessage mpNowPlayingSession setDelegateSelector value

-- | When YES, now playing info will be automatically published, and nowPlayingInfoCenter must not be used.  Now playing info keys to be incorporated by automatic publishing can be set on the AVPlayerItem's nowPlayingInfo property.
--
-- ObjC selector: @- automaticallyPublishesNowPlayingInfo@
automaticallyPublishesNowPlayingInfo :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO Bool
automaticallyPublishesNowPlayingInfo mpNowPlayingSession =
  sendMessage mpNowPlayingSession automaticallyPublishesNowPlayingInfoSelector

-- | When YES, now playing info will be automatically published, and nowPlayingInfoCenter must not be used.  Now playing info keys to be incorporated by automatic publishing can be set on the AVPlayerItem's nowPlayingInfo property.
--
-- ObjC selector: @- setAutomaticallyPublishesNowPlayingInfo:@
setAutomaticallyPublishesNowPlayingInfo :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> Bool -> IO ()
setAutomaticallyPublishesNowPlayingInfo mpNowPlayingSession value =
  sendMessage mpNowPlayingSession setAutomaticallyPublishesNowPlayingInfoSelector value

-- | The now playing info center that is associated with this session.
--
-- ObjC selector: @- nowPlayingInfoCenter@
nowPlayingInfoCenter :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id MPNowPlayingInfoCenter)
nowPlayingInfoCenter mpNowPlayingSession =
  sendMessage mpNowPlayingSession nowPlayingInfoCenterSelector

-- | The remote command center that is associated with this session.
--
-- ObjC selector: @- remoteCommandCenter@
remoteCommandCenter :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id MPRemoteCommandCenter)
remoteCommandCenter mpNowPlayingSession =
  sendMessage mpNowPlayingSession remoteCommandCenterSelector

-- | Returns a Boolean value indicating whether this session can become the App's active now playing session.
--
-- ObjC selector: @- canBecomeActive@
canBecomeActive :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO Bool
canBecomeActive mpNowPlayingSession =
  sendMessage mpNowPlayingSession canBecomeActiveSelector

-- | Returns a Boolean value indicating whether this session is the App's active now playing session.
--
-- ObjC selector: @- active@
active :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO Bool
active mpNowPlayingSession =
  sendMessage mpNowPlayingSession activeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlayers:@
initWithPlayersSelector :: Selector '[Id NSArray] (Id MPNowPlayingSession)
initWithPlayersSelector = mkSelector "initWithPlayers:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPNowPlayingSession)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPNowPlayingSession)
initSelector = mkSelector "init"

-- | @Selector@ for @becomeActiveIfPossibleWithCompletion:@
becomeActiveIfPossibleWithCompletionSelector :: Selector '[Ptr ()] ()
becomeActiveIfPossibleWithCompletionSelector = mkSelector "becomeActiveIfPossibleWithCompletion:"

-- | @Selector@ for @addPlayer:@
addPlayerSelector :: Selector '[Id AVPlayer] ()
addPlayerSelector = mkSelector "addPlayer:"

-- | @Selector@ for @removePlayer:@
removePlayerSelector :: Selector '[Id AVPlayer] ()
removePlayerSelector = mkSelector "removePlayer:"

-- | @Selector@ for @players@
playersSelector :: Selector '[] (Id NSArray)
playersSelector = mkSelector "players"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @automaticallyPublishesNowPlayingInfo@
automaticallyPublishesNowPlayingInfoSelector :: Selector '[] Bool
automaticallyPublishesNowPlayingInfoSelector = mkSelector "automaticallyPublishesNowPlayingInfo"

-- | @Selector@ for @setAutomaticallyPublishesNowPlayingInfo:@
setAutomaticallyPublishesNowPlayingInfoSelector :: Selector '[Bool] ()
setAutomaticallyPublishesNowPlayingInfoSelector = mkSelector "setAutomaticallyPublishesNowPlayingInfo:"

-- | @Selector@ for @nowPlayingInfoCenter@
nowPlayingInfoCenterSelector :: Selector '[] (Id MPNowPlayingInfoCenter)
nowPlayingInfoCenterSelector = mkSelector "nowPlayingInfoCenter"

-- | @Selector@ for @remoteCommandCenter@
remoteCommandCenterSelector :: Selector '[] (Id MPRemoteCommandCenter)
remoteCommandCenterSelector = mkSelector "remoteCommandCenter"

-- | @Selector@ for @canBecomeActive@
canBecomeActiveSelector :: Selector '[] Bool
canBecomeActiveSelector = mkSelector "canBecomeActive"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

