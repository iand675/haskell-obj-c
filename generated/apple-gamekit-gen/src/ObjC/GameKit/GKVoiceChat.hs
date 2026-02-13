{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKVoiceChat represents an instance of a named voice communications channel
--
-- Generated bindings for @GKVoiceChat@.
module ObjC.GameKit.GKVoiceChat
  ( GKVoiceChat
  , IsGKVoiceChat(..)
  , start
  , stop
  , setPlayer_muted
  , isVoIPAllowed
  , setMute_forPlayer
  , playerVoiceChatStateDidChangeHandler
  , setPlayerVoiceChatStateDidChangeHandler
  , name
  , active
  , setActive
  , volume
  , setVolume
  , players
  , playerIDs
  , playerStateUpdateHandler
  , setPlayerStateUpdateHandler
  , activeSelector
  , isVoIPAllowedSelector
  , nameSelector
  , playerIDsSelector
  , playerStateUpdateHandlerSelector
  , playerVoiceChatStateDidChangeHandlerSelector
  , playersSelector
  , setActiveSelector
  , setMute_forPlayerSelector
  , setPlayerStateUpdateHandlerSelector
  , setPlayerVoiceChatStateDidChangeHandlerSelector
  , setPlayer_mutedSelector
  , setVolumeSelector
  , startSelector
  , stopSelector
  , volumeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- start@
start :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO ()
start gkVoiceChat =
  sendMessage gkVoiceChat startSelector

-- | start receiving audio from the chat
--
-- ObjC selector: @- stop@
stop :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO ()
stop gkVoiceChat =
  sendMessage gkVoiceChat stopSelector

-- | stop receiving audio from the chat
--
-- ObjC selector: @- setPlayer:muted:@
setPlayer_muted :: (IsGKVoiceChat gkVoiceChat, IsGKPlayer player) => gkVoiceChat -> player -> Bool -> IO ()
setPlayer_muted gkVoiceChat player isMuted =
  sendMessage gkVoiceChat setPlayer_mutedSelector (toGKPlayer player) isMuted

-- | @+ isVoIPAllowed@
isVoIPAllowed :: IO Bool
isVoIPAllowed  =
  do
    cls' <- getRequiredClass "GKVoiceChat"
    sendClassMessage cls' isVoIPAllowedSelector

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- setMute:forPlayer:@
setMute_forPlayer :: (IsGKVoiceChat gkVoiceChat, IsNSString playerID) => gkVoiceChat -> Bool -> playerID -> IO ()
setMute_forPlayer gkVoiceChat isMuted playerID =
  sendMessage gkVoiceChat setMute_forPlayerSelector isMuted (toNSString playerID)

-- | @- playerVoiceChatStateDidChangeHandler@
playerVoiceChatStateDidChangeHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Ptr ())
playerVoiceChatStateDidChangeHandler gkVoiceChat =
  sendMessage gkVoiceChat playerVoiceChatStateDidChangeHandlerSelector

-- | @- setPlayerVoiceChatStateDidChangeHandler:@
setPlayerVoiceChatStateDidChangeHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> Ptr () -> IO ()
setPlayerVoiceChatStateDidChangeHandler gkVoiceChat value =
  sendMessage gkVoiceChat setPlayerVoiceChatStateDidChangeHandlerSelector value

-- | @- name@
name :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Id NSString)
name gkVoiceChat =
  sendMessage gkVoiceChat nameSelector

-- | @- active@
active :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO Bool
active gkVoiceChat =
  sendMessage gkVoiceChat activeSelector

-- | @- setActive:@
setActive :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> Bool -> IO ()
setActive gkVoiceChat value =
  sendMessage gkVoiceChat setActiveSelector value

-- | @- volume@
volume :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO CFloat
volume gkVoiceChat =
  sendMessage gkVoiceChat volumeSelector

-- | @- setVolume:@
setVolume :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> CFloat -> IO ()
setVolume gkVoiceChat value =
  sendMessage gkVoiceChat setVolumeSelector value

-- | @- players@
players :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Id NSArray)
players gkVoiceChat =
  sendMessage gkVoiceChat playersSelector

-- | * This property is obsolete. **
--
-- ObjC selector: @- playerIDs@
playerIDs :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Id NSArray)
playerIDs gkVoiceChat =
  sendMessage gkVoiceChat playerIDsSelector

-- | @- playerStateUpdateHandler@
playerStateUpdateHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Ptr ())
playerStateUpdateHandler gkVoiceChat =
  sendMessage gkVoiceChat playerStateUpdateHandlerSelector

-- | @- setPlayerStateUpdateHandler:@
setPlayerStateUpdateHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> Ptr () -> IO ()
setPlayerStateUpdateHandler gkVoiceChat value =
  sendMessage gkVoiceChat setPlayerStateUpdateHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @setPlayer:muted:@
setPlayer_mutedSelector :: Selector '[Id GKPlayer, Bool] ()
setPlayer_mutedSelector = mkSelector "setPlayer:muted:"

-- | @Selector@ for @isVoIPAllowed@
isVoIPAllowedSelector :: Selector '[] Bool
isVoIPAllowedSelector = mkSelector "isVoIPAllowed"

-- | @Selector@ for @setMute:forPlayer:@
setMute_forPlayerSelector :: Selector '[Bool, Id NSString] ()
setMute_forPlayerSelector = mkSelector "setMute:forPlayer:"

-- | @Selector@ for @playerVoiceChatStateDidChangeHandler@
playerVoiceChatStateDidChangeHandlerSelector :: Selector '[] (Ptr ())
playerVoiceChatStateDidChangeHandlerSelector = mkSelector "playerVoiceChatStateDidChangeHandler"

-- | @Selector@ for @setPlayerVoiceChatStateDidChangeHandler:@
setPlayerVoiceChatStateDidChangeHandlerSelector :: Selector '[Ptr ()] ()
setPlayerVoiceChatStateDidChangeHandlerSelector = mkSelector "setPlayerVoiceChatStateDidChangeHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Bool] ()
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @players@
playersSelector :: Selector '[] (Id NSArray)
playersSelector = mkSelector "players"

-- | @Selector@ for @playerIDs@
playerIDsSelector :: Selector '[] (Id NSArray)
playerIDsSelector = mkSelector "playerIDs"

-- | @Selector@ for @playerStateUpdateHandler@
playerStateUpdateHandlerSelector :: Selector '[] (Ptr ())
playerStateUpdateHandlerSelector = mkSelector "playerStateUpdateHandler"

-- | @Selector@ for @setPlayerStateUpdateHandler:@
setPlayerStateUpdateHandlerSelector :: Selector '[Ptr ()] ()
setPlayerStateUpdateHandlerSelector = mkSelector "setPlayerStateUpdateHandler:"

