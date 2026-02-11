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
  , initWithPlayersSelector
  , newSelector
  , initSelector
  , becomeActiveIfPossibleWithCompletionSelector
  , addPlayerSelector
  , removePlayerSelector
  , playersSelector
  , delegateSelector
  , setDelegateSelector
  , automaticallyPublishesNowPlayingInfoSelector
  , setAutomaticallyPublishesNowPlayingInfoSelector
  , nowPlayingInfoCenterSelector
  , remoteCommandCenterSelector
  , canBecomeActiveSelector
  , activeSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a session associated with a given AVPlayer instance. This will assert if players is nil or empty.
--
-- ObjC selector: @- initWithPlayers:@
initWithPlayers :: (IsMPNowPlayingSession mpNowPlayingSession, IsNSArray players) => mpNowPlayingSession -> players -> IO (Id MPNowPlayingSession)
initWithPlayers mpNowPlayingSession  players =
  withObjCPtr players $ \raw_players ->
      sendMsg mpNowPlayingSession (mkSelector "initWithPlayers:") (retPtr retVoid) [argPtr (castPtr raw_players :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MPNowPlayingSession)
new  =
  do
    cls' <- getRequiredClass "MPNowPlayingSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id MPNowPlayingSession)
init_ mpNowPlayingSession  =
    sendMsg mpNowPlayingSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Asks the system to make this session the active now playing sessin for the App.
--
-- ObjC selector: @- becomeActiveIfPossibleWithCompletion:@
becomeActiveIfPossibleWithCompletion :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> Ptr () -> IO ()
becomeActiveIfPossibleWithCompletion mpNowPlayingSession  completion =
    sendMsg mpNowPlayingSession (mkSelector "becomeActiveIfPossibleWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Add AVPlayer instance to this session.
--
-- ObjC selector: @- addPlayer:@
addPlayer :: (IsMPNowPlayingSession mpNowPlayingSession, IsAVPlayer player) => mpNowPlayingSession -> player -> IO ()
addPlayer mpNowPlayingSession  player =
  withObjCPtr player $ \raw_player ->
      sendMsg mpNowPlayingSession (mkSelector "addPlayer:") retVoid [argPtr (castPtr raw_player :: Ptr ())]

-- | Remove AVPlayer instance from this session.
--
-- ObjC selector: @- removePlayer:@
removePlayer :: (IsMPNowPlayingSession mpNowPlayingSession, IsAVPlayer player) => mpNowPlayingSession -> player -> IO ()
removePlayer mpNowPlayingSession  player =
  withObjCPtr player $ \raw_player ->
      sendMsg mpNowPlayingSession (mkSelector "removePlayer:") retVoid [argPtr (castPtr raw_player :: Ptr ())]

-- | AVPlayer instances associated with this session.
--
-- ObjC selector: @- players@
players :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id NSArray)
players mpNowPlayingSession  =
    sendMsg mpNowPlayingSession (mkSelector "players") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO RawId
delegate mpNowPlayingSession  =
    fmap (RawId . castPtr) $ sendMsg mpNowPlayingSession (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> RawId -> IO ()
setDelegate mpNowPlayingSession  value =
    sendMsg mpNowPlayingSession (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | When YES, now playing info will be automatically published, and nowPlayingInfoCenter must not be used.  Now playing info keys to be incorporated by automatic publishing can be set on the AVPlayerItem's nowPlayingInfo property.
--
-- ObjC selector: @- automaticallyPublishesNowPlayingInfo@
automaticallyPublishesNowPlayingInfo :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO Bool
automaticallyPublishesNowPlayingInfo mpNowPlayingSession  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpNowPlayingSession (mkSelector "automaticallyPublishesNowPlayingInfo") retCULong []

-- | When YES, now playing info will be automatically published, and nowPlayingInfoCenter must not be used.  Now playing info keys to be incorporated by automatic publishing can be set on the AVPlayerItem's nowPlayingInfo property.
--
-- ObjC selector: @- setAutomaticallyPublishesNowPlayingInfo:@
setAutomaticallyPublishesNowPlayingInfo :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> Bool -> IO ()
setAutomaticallyPublishesNowPlayingInfo mpNowPlayingSession  value =
    sendMsg mpNowPlayingSession (mkSelector "setAutomaticallyPublishesNowPlayingInfo:") retVoid [argCULong (if value then 1 else 0)]

-- | The now playing info center that is associated with this session.
--
-- ObjC selector: @- nowPlayingInfoCenter@
nowPlayingInfoCenter :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id MPNowPlayingInfoCenter)
nowPlayingInfoCenter mpNowPlayingSession  =
    sendMsg mpNowPlayingSession (mkSelector "nowPlayingInfoCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The remote command center that is associated with this session.
--
-- ObjC selector: @- remoteCommandCenter@
remoteCommandCenter :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO (Id MPRemoteCommandCenter)
remoteCommandCenter mpNowPlayingSession  =
    sendMsg mpNowPlayingSession (mkSelector "remoteCommandCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a Boolean value indicating whether this session can become the App's active now playing session.
--
-- ObjC selector: @- canBecomeActive@
canBecomeActive :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO Bool
canBecomeActive mpNowPlayingSession  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpNowPlayingSession (mkSelector "canBecomeActive") retCULong []

-- | Returns a Boolean value indicating whether this session is the App's active now playing session.
--
-- ObjC selector: @- active@
active :: IsMPNowPlayingSession mpNowPlayingSession => mpNowPlayingSession -> IO Bool
active mpNowPlayingSession  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpNowPlayingSession (mkSelector "active") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlayers:@
initWithPlayersSelector :: Selector
initWithPlayersSelector = mkSelector "initWithPlayers:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @becomeActiveIfPossibleWithCompletion:@
becomeActiveIfPossibleWithCompletionSelector :: Selector
becomeActiveIfPossibleWithCompletionSelector = mkSelector "becomeActiveIfPossibleWithCompletion:"

-- | @Selector@ for @addPlayer:@
addPlayerSelector :: Selector
addPlayerSelector = mkSelector "addPlayer:"

-- | @Selector@ for @removePlayer:@
removePlayerSelector :: Selector
removePlayerSelector = mkSelector "removePlayer:"

-- | @Selector@ for @players@
playersSelector :: Selector
playersSelector = mkSelector "players"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @automaticallyPublishesNowPlayingInfo@
automaticallyPublishesNowPlayingInfoSelector :: Selector
automaticallyPublishesNowPlayingInfoSelector = mkSelector "automaticallyPublishesNowPlayingInfo"

-- | @Selector@ for @setAutomaticallyPublishesNowPlayingInfo:@
setAutomaticallyPublishesNowPlayingInfoSelector :: Selector
setAutomaticallyPublishesNowPlayingInfoSelector = mkSelector "setAutomaticallyPublishesNowPlayingInfo:"

-- | @Selector@ for @nowPlayingInfoCenter@
nowPlayingInfoCenterSelector :: Selector
nowPlayingInfoCenterSelector = mkSelector "nowPlayingInfoCenter"

-- | @Selector@ for @remoteCommandCenter@
remoteCommandCenterSelector :: Selector
remoteCommandCenterSelector = mkSelector "remoteCommandCenter"

-- | @Selector@ for @canBecomeActive@
canBecomeActiveSelector :: Selector
canBecomeActiveSelector = mkSelector "canBecomeActive"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

