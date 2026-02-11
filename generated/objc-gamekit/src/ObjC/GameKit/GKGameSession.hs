{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKGameSession@.
module ObjC.GameKit.GKGameSession
  ( GKGameSession
  , IsGKGameSession(..)
  , createSessionInContainer_withTitle_maxConnectedPlayers_completionHandler
  , loadSessionWithIdentifier_completionHandler
  , removeSessionWithIdentifier_completionHandler
  , getShareURLWithCompletionHandler
  , loadDataWithCompletionHandler
  , saveData_completionHandler
  , setConnectionState_completionHandler
  , playersWithConnectionState
  , sendData_withTransportType_completionHandler
  , sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandler
  , clearBadgeForPlayers_completionHandler
  , addEventListener
  , removeEventListener
  , identifier
  , title
  , owner
  , players
  , lastModifiedDate
  , lastModifiedPlayer
  , maxNumberOfConnectedPlayers
  , badgedPlayers
  , createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector
  , loadSessionWithIdentifier_completionHandlerSelector
  , removeSessionWithIdentifier_completionHandlerSelector
  , getShareURLWithCompletionHandlerSelector
  , loadDataWithCompletionHandlerSelector
  , saveData_completionHandlerSelector
  , setConnectionState_completionHandlerSelector
  , playersWithConnectionStateSelector
  , sendData_withTransportType_completionHandlerSelector
  , sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector
  , clearBadgeForPlayers_completionHandlerSelector
  , addEventListenerSelector
  , removeEventListenerSelector
  , identifierSelector
  , titleSelector
  , ownerSelector
  , playersSelector
  , lastModifiedDateSelector
  , lastModifiedPlayerSelector
  , maxNumberOfConnectedPlayersSelector
  , badgedPlayersSelector

  -- * Enum types
  , GKConnectionState(GKConnectionState)
  , pattern GKConnectionStateNotConnected
  , pattern GKConnectionStateConnected
  , GKTransportType(GKTransportType)
  , pattern GKTransportTypeUnreliable
  , pattern GKTransportTypeReliable

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
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ createSessionInContainer:withTitle:maxConnectedPlayers:completionHandler:@
createSessionInContainer_withTitle_maxConnectedPlayers_completionHandler :: (IsNSString containerName, IsNSString title) => containerName -> title -> CLong -> Ptr () -> IO ()
createSessionInContainer_withTitle_maxConnectedPlayers_completionHandler containerName title maxPlayers completionHandler =
  do
    cls' <- getRequiredClass "GKGameSession"
    withObjCPtr containerName $ \raw_containerName ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "createSessionInContainer:withTitle:maxConnectedPlayers:completionHandler:") retVoid [argPtr (castPtr raw_containerName :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCLong (fromIntegral maxPlayers), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ loadSessionWithIdentifier:completionHandler:@
loadSessionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
loadSessionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "GKGameSession"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "loadSessionWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ removeSessionWithIdentifier:completionHandler:@
removeSessionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
removeSessionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "GKGameSession"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "removeSessionWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getShareURLWithCompletionHandler:@
getShareURLWithCompletionHandler :: IsGKGameSession gkGameSession => gkGameSession -> Ptr () -> IO ()
getShareURLWithCompletionHandler gkGameSession  completionHandler =
  sendMsg gkGameSession (mkSelector "getShareURLWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- loadDataWithCompletionHandler:@
loadDataWithCompletionHandler :: IsGKGameSession gkGameSession => gkGameSession -> Ptr () -> IO ()
loadDataWithCompletionHandler gkGameSession  completionHandler =
  sendMsg gkGameSession (mkSelector "loadDataWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- saveData:completionHandler:@
saveData_completionHandler :: (IsGKGameSession gkGameSession, IsNSData data_) => gkGameSession -> data_ -> Ptr () -> IO ()
saveData_completionHandler gkGameSession  data_ completionHandler =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg gkGameSession (mkSelector "saveData:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setConnectionState:completionHandler:@
setConnectionState_completionHandler :: IsGKGameSession gkGameSession => gkGameSession -> GKConnectionState -> Ptr () -> IO ()
setConnectionState_completionHandler gkGameSession  state completionHandler =
  sendMsg gkGameSession (mkSelector "setConnectionState:completionHandler:") retVoid [argCLong (coerce state), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- playersWithConnectionState:@
playersWithConnectionState :: IsGKGameSession gkGameSession => gkGameSession -> GKConnectionState -> IO (Id NSArray)
playersWithConnectionState gkGameSession  state =
  sendMsg gkGameSession (mkSelector "playersWithConnectionState:") (retPtr retVoid) [argCLong (coerce state)] >>= retainedObject . castPtr

-- | @- sendData:withTransportType:completionHandler:@
sendData_withTransportType_completionHandler :: (IsGKGameSession gkGameSession, IsNSData data_) => gkGameSession -> data_ -> GKTransportType -> Ptr () -> IO ()
sendData_withTransportType_completionHandler gkGameSession  data_ transport completionHandler =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg gkGameSession (mkSelector "sendData:withTransportType:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argCLong (coerce transport), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:@
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandler :: (IsGKGameSession gkGameSession, IsNSString key, IsNSArray arguments, IsNSData data_, IsNSArray players) => gkGameSession -> key -> arguments -> data_ -> players -> Bool -> Ptr () -> IO ()
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandler gkGameSession  key arguments data_ players badgePlayers completionHandler =
withObjCPtr key $ \raw_key ->
  withObjCPtr arguments $ \raw_arguments ->
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr players $ \raw_players ->
          sendMsg gkGameSession (mkSelector "sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argCULong (if badgePlayers then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearBadgeForPlayers:completionHandler:@
clearBadgeForPlayers_completionHandler :: (IsGKGameSession gkGameSession, IsNSArray players) => gkGameSession -> players -> Ptr () -> IO ()
clearBadgeForPlayers_completionHandler gkGameSession  players completionHandler =
withObjCPtr players $ \raw_players ->
    sendMsg gkGameSession (mkSelector "clearBadgeForPlayers:completionHandler:") retVoid [argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ addEventListener:@
addEventListener :: IsNSObject listener => listener -> IO ()
addEventListener listener =
  do
    cls' <- getRequiredClass "GKGameSession"
    withObjCPtr listener $ \raw_listener ->
      sendClassMsg cls' (mkSelector "addEventListener:") retVoid [argPtr (castPtr raw_listener :: Ptr ())]

-- | @+ removeEventListener:@
removeEventListener :: IsNSObject listener => listener -> IO ()
removeEventListener listener =
  do
    cls' <- getRequiredClass "GKGameSession"
    withObjCPtr listener $ \raw_listener ->
      sendClassMsg cls' (mkSelector "removeEventListener:") retVoid [argPtr (castPtr raw_listener :: Ptr ())]

-- | @- identifier@
identifier :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSString)
identifier gkGameSession  =
  sendMsg gkGameSession (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSString)
title gkGameSession  =
  sendMsg gkGameSession (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- owner@
owner :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id GKCloudPlayer)
owner gkGameSession  =
  sendMsg gkGameSession (mkSelector "owner") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- players@
players :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSArray)
players gkGameSession  =
  sendMsg gkGameSession (mkSelector "players") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastModifiedDate@
lastModifiedDate :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSDate)
lastModifiedDate gkGameSession  =
  sendMsg gkGameSession (mkSelector "lastModifiedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastModifiedPlayer@
lastModifiedPlayer :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id GKCloudPlayer)
lastModifiedPlayer gkGameSession  =
  sendMsg gkGameSession (mkSelector "lastModifiedPlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maxNumberOfConnectedPlayers@
maxNumberOfConnectedPlayers :: IsGKGameSession gkGameSession => gkGameSession -> IO CLong
maxNumberOfConnectedPlayers gkGameSession  =
  sendMsg gkGameSession (mkSelector "maxNumberOfConnectedPlayers") retCLong []

-- | @- badgedPlayers@
badgedPlayers :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSArray)
badgedPlayers gkGameSession  =
  sendMsg gkGameSession (mkSelector "badgedPlayers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createSessionInContainer:withTitle:maxConnectedPlayers:completionHandler:@
createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector :: Selector
createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector = mkSelector "createSessionInContainer:withTitle:maxConnectedPlayers:completionHandler:"

-- | @Selector@ for @loadSessionWithIdentifier:completionHandler:@
loadSessionWithIdentifier_completionHandlerSelector :: Selector
loadSessionWithIdentifier_completionHandlerSelector = mkSelector "loadSessionWithIdentifier:completionHandler:"

-- | @Selector@ for @removeSessionWithIdentifier:completionHandler:@
removeSessionWithIdentifier_completionHandlerSelector :: Selector
removeSessionWithIdentifier_completionHandlerSelector = mkSelector "removeSessionWithIdentifier:completionHandler:"

-- | @Selector@ for @getShareURLWithCompletionHandler:@
getShareURLWithCompletionHandlerSelector :: Selector
getShareURLWithCompletionHandlerSelector = mkSelector "getShareURLWithCompletionHandler:"

-- | @Selector@ for @loadDataWithCompletionHandler:@
loadDataWithCompletionHandlerSelector :: Selector
loadDataWithCompletionHandlerSelector = mkSelector "loadDataWithCompletionHandler:"

-- | @Selector@ for @saveData:completionHandler:@
saveData_completionHandlerSelector :: Selector
saveData_completionHandlerSelector = mkSelector "saveData:completionHandler:"

-- | @Selector@ for @setConnectionState:completionHandler:@
setConnectionState_completionHandlerSelector :: Selector
setConnectionState_completionHandlerSelector = mkSelector "setConnectionState:completionHandler:"

-- | @Selector@ for @playersWithConnectionState:@
playersWithConnectionStateSelector :: Selector
playersWithConnectionStateSelector = mkSelector "playersWithConnectionState:"

-- | @Selector@ for @sendData:withTransportType:completionHandler:@
sendData_withTransportType_completionHandlerSelector :: Selector
sendData_withTransportType_completionHandlerSelector = mkSelector "sendData:withTransportType:completionHandler:"

-- | @Selector@ for @sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:@
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector :: Selector
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector = mkSelector "sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:"

-- | @Selector@ for @clearBadgeForPlayers:completionHandler:@
clearBadgeForPlayers_completionHandlerSelector :: Selector
clearBadgeForPlayers_completionHandlerSelector = mkSelector "clearBadgeForPlayers:completionHandler:"

-- | @Selector@ for @addEventListener:@
addEventListenerSelector :: Selector
addEventListenerSelector = mkSelector "addEventListener:"

-- | @Selector@ for @removeEventListener:@
removeEventListenerSelector :: Selector
removeEventListenerSelector = mkSelector "removeEventListener:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @owner@
ownerSelector :: Selector
ownerSelector = mkSelector "owner"

-- | @Selector@ for @players@
playersSelector :: Selector
playersSelector = mkSelector "players"

-- | @Selector@ for @lastModifiedDate@
lastModifiedDateSelector :: Selector
lastModifiedDateSelector = mkSelector "lastModifiedDate"

-- | @Selector@ for @lastModifiedPlayer@
lastModifiedPlayerSelector :: Selector
lastModifiedPlayerSelector = mkSelector "lastModifiedPlayer"

-- | @Selector@ for @maxNumberOfConnectedPlayers@
maxNumberOfConnectedPlayersSelector :: Selector
maxNumberOfConnectedPlayersSelector = mkSelector "maxNumberOfConnectedPlayers"

-- | @Selector@ for @badgedPlayers@
badgedPlayersSelector :: Selector
badgedPlayersSelector = mkSelector "badgedPlayers"

