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
  , playerIDs
  , playerStateUpdateHandler
  , setPlayerStateUpdateHandler
  , startSelector
  , stopSelector
  , setPlayer_mutedSelector
  , isVoIPAllowedSelector
  , setMute_forPlayerSelector
  , playerVoiceChatStateDidChangeHandlerSelector
  , setPlayerVoiceChatStateDidChangeHandlerSelector
  , nameSelector
  , activeSelector
  , setActiveSelector
  , volumeSelector
  , setVolumeSelector
  , playerIDsSelector
  , playerStateUpdateHandlerSelector
  , setPlayerStateUpdateHandlerSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- start@
start :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO ()
start gkVoiceChat  =
  sendMsg gkVoiceChat (mkSelector "start") retVoid []

-- | start receiving audio from the chat
--
-- ObjC selector: @- stop@
stop :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO ()
stop gkVoiceChat  =
  sendMsg gkVoiceChat (mkSelector "stop") retVoid []

-- | stop receiving audio from the chat
--
-- ObjC selector: @- setPlayer:muted:@
setPlayer_muted :: (IsGKVoiceChat gkVoiceChat, IsGKPlayer player) => gkVoiceChat -> player -> Bool -> IO ()
setPlayer_muted gkVoiceChat  player isMuted =
withObjCPtr player $ \raw_player ->
    sendMsg gkVoiceChat (mkSelector "setPlayer:muted:") retVoid [argPtr (castPtr raw_player :: Ptr ()), argCULong (if isMuted then 1 else 0)]

-- | @+ isVoIPAllowed@
isVoIPAllowed :: IO Bool
isVoIPAllowed  =
  do
    cls' <- getRequiredClass "GKVoiceChat"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isVoIPAllowed") retCULong []

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- setMute:forPlayer:@
setMute_forPlayer :: (IsGKVoiceChat gkVoiceChat, IsNSString playerID) => gkVoiceChat -> Bool -> playerID -> IO ()
setMute_forPlayer gkVoiceChat  isMuted playerID =
withObjCPtr playerID $ \raw_playerID ->
    sendMsg gkVoiceChat (mkSelector "setMute:forPlayer:") retVoid [argCULong (if isMuted then 1 else 0), argPtr (castPtr raw_playerID :: Ptr ())]

-- | @- playerVoiceChatStateDidChangeHandler@
playerVoiceChatStateDidChangeHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Ptr ())
playerVoiceChatStateDidChangeHandler gkVoiceChat  =
  fmap castPtr $ sendMsg gkVoiceChat (mkSelector "playerVoiceChatStateDidChangeHandler") (retPtr retVoid) []

-- | @- setPlayerVoiceChatStateDidChangeHandler:@
setPlayerVoiceChatStateDidChangeHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> Ptr () -> IO ()
setPlayerVoiceChatStateDidChangeHandler gkVoiceChat  value =
  sendMsg gkVoiceChat (mkSelector "setPlayerVoiceChatStateDidChangeHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- name@
name :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Id NSString)
name gkVoiceChat  =
  sendMsg gkVoiceChat (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- active@
active :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO Bool
active gkVoiceChat  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoiceChat (mkSelector "active") retCULong []

-- | @- setActive:@
setActive :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> Bool -> IO ()
setActive gkVoiceChat  value =
  sendMsg gkVoiceChat (mkSelector "setActive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- volume@
volume :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO CFloat
volume gkVoiceChat  =
  sendMsg gkVoiceChat (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> CFloat -> IO ()
setVolume gkVoiceChat  value =
  sendMsg gkVoiceChat (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | * This property is obsolete. **
--
-- ObjC selector: @- playerIDs@
playerIDs :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Id NSArray)
playerIDs gkVoiceChat  =
  sendMsg gkVoiceChat (mkSelector "playerIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playerStateUpdateHandler@
playerStateUpdateHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> IO (Ptr ())
playerStateUpdateHandler gkVoiceChat  =
  fmap castPtr $ sendMsg gkVoiceChat (mkSelector "playerStateUpdateHandler") (retPtr retVoid) []

-- | @- setPlayerStateUpdateHandler:@
setPlayerStateUpdateHandler :: IsGKVoiceChat gkVoiceChat => gkVoiceChat -> Ptr () -> IO ()
setPlayerStateUpdateHandler gkVoiceChat  value =
  sendMsg gkVoiceChat (mkSelector "setPlayerStateUpdateHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @setPlayer:muted:@
setPlayer_mutedSelector :: Selector
setPlayer_mutedSelector = mkSelector "setPlayer:muted:"

-- | @Selector@ for @isVoIPAllowed@
isVoIPAllowedSelector :: Selector
isVoIPAllowedSelector = mkSelector "isVoIPAllowed"

-- | @Selector@ for @setMute:forPlayer:@
setMute_forPlayerSelector :: Selector
setMute_forPlayerSelector = mkSelector "setMute:forPlayer:"

-- | @Selector@ for @playerVoiceChatStateDidChangeHandler@
playerVoiceChatStateDidChangeHandlerSelector :: Selector
playerVoiceChatStateDidChangeHandlerSelector = mkSelector "playerVoiceChatStateDidChangeHandler"

-- | @Selector@ for @setPlayerVoiceChatStateDidChangeHandler:@
setPlayerVoiceChatStateDidChangeHandlerSelector :: Selector
setPlayerVoiceChatStateDidChangeHandlerSelector = mkSelector "setPlayerVoiceChatStateDidChangeHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @playerIDs@
playerIDsSelector :: Selector
playerIDsSelector = mkSelector "playerIDs"

-- | @Selector@ for @playerStateUpdateHandler@
playerStateUpdateHandlerSelector :: Selector
playerStateUpdateHandlerSelector = mkSelector "playerStateUpdateHandler"

-- | @Selector@ for @setPlayerStateUpdateHandler:@
setPlayerStateUpdateHandlerSelector :: Selector
setPlayerStateUpdateHandlerSelector = mkSelector "setPlayerStateUpdateHandler:"

