{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addEventListenerSelector
  , badgedPlayersSelector
  , clearBadgeForPlayers_completionHandlerSelector
  , createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector
  , getShareURLWithCompletionHandlerSelector
  , identifierSelector
  , lastModifiedDateSelector
  , lastModifiedPlayerSelector
  , loadDataWithCompletionHandlerSelector
  , loadSessionWithIdentifier_completionHandlerSelector
  , maxNumberOfConnectedPlayersSelector
  , ownerSelector
  , playersSelector
  , playersWithConnectionStateSelector
  , removeEventListenerSelector
  , removeSessionWithIdentifier_completionHandlerSelector
  , saveData_completionHandlerSelector
  , sendData_withTransportType_completionHandlerSelector
  , sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector
  , setConnectionState_completionHandlerSelector
  , titleSelector

  -- * Enum types
  , GKConnectionState(GKConnectionState)
  , pattern GKConnectionStateNotConnected
  , pattern GKConnectionStateConnected
  , GKTransportType(GKTransportType)
  , pattern GKTransportTypeUnreliable
  , pattern GKTransportTypeReliable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector (toNSString containerName) (toNSString title) maxPlayers completionHandler

-- | @+ loadSessionWithIdentifier:completionHandler:@
loadSessionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
loadSessionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "GKGameSession"
    sendClassMessage cls' loadSessionWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | @+ removeSessionWithIdentifier:completionHandler:@
removeSessionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
removeSessionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "GKGameSession"
    sendClassMessage cls' removeSessionWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | @- getShareURLWithCompletionHandler:@
getShareURLWithCompletionHandler :: IsGKGameSession gkGameSession => gkGameSession -> Ptr () -> IO ()
getShareURLWithCompletionHandler gkGameSession completionHandler =
  sendMessage gkGameSession getShareURLWithCompletionHandlerSelector completionHandler

-- | @- loadDataWithCompletionHandler:@
loadDataWithCompletionHandler :: IsGKGameSession gkGameSession => gkGameSession -> Ptr () -> IO ()
loadDataWithCompletionHandler gkGameSession completionHandler =
  sendMessage gkGameSession loadDataWithCompletionHandlerSelector completionHandler

-- | @- saveData:completionHandler:@
saveData_completionHandler :: (IsGKGameSession gkGameSession, IsNSData data_) => gkGameSession -> data_ -> Ptr () -> IO ()
saveData_completionHandler gkGameSession data_ completionHandler =
  sendMessage gkGameSession saveData_completionHandlerSelector (toNSData data_) completionHandler

-- | @- setConnectionState:completionHandler:@
setConnectionState_completionHandler :: IsGKGameSession gkGameSession => gkGameSession -> GKConnectionState -> Ptr () -> IO ()
setConnectionState_completionHandler gkGameSession state completionHandler =
  sendMessage gkGameSession setConnectionState_completionHandlerSelector state completionHandler

-- | @- playersWithConnectionState:@
playersWithConnectionState :: IsGKGameSession gkGameSession => gkGameSession -> GKConnectionState -> IO (Id NSArray)
playersWithConnectionState gkGameSession state =
  sendMessage gkGameSession playersWithConnectionStateSelector state

-- | @- sendData:withTransportType:completionHandler:@
sendData_withTransportType_completionHandler :: (IsGKGameSession gkGameSession, IsNSData data_) => gkGameSession -> data_ -> GKTransportType -> Ptr () -> IO ()
sendData_withTransportType_completionHandler gkGameSession data_ transport completionHandler =
  sendMessage gkGameSession sendData_withTransportType_completionHandlerSelector (toNSData data_) transport completionHandler

-- | @- sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:@
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandler :: (IsGKGameSession gkGameSession, IsNSString key, IsNSArray arguments, IsNSData data_, IsNSArray players) => gkGameSession -> key -> arguments -> data_ -> players -> Bool -> Ptr () -> IO ()
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandler gkGameSession key arguments data_ players badgePlayers completionHandler =
  sendMessage gkGameSession sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector (toNSString key) (toNSArray arguments) (toNSData data_) (toNSArray players) badgePlayers completionHandler

-- | @- clearBadgeForPlayers:completionHandler:@
clearBadgeForPlayers_completionHandler :: (IsGKGameSession gkGameSession, IsNSArray players) => gkGameSession -> players -> Ptr () -> IO ()
clearBadgeForPlayers_completionHandler gkGameSession players completionHandler =
  sendMessage gkGameSession clearBadgeForPlayers_completionHandlerSelector (toNSArray players) completionHandler

-- | @+ addEventListener:@
addEventListener :: IsNSObject listener => listener -> IO ()
addEventListener listener =
  do
    cls' <- getRequiredClass "GKGameSession"
    sendClassMessage cls' addEventListenerSelector (toNSObject listener)

-- | @+ removeEventListener:@
removeEventListener :: IsNSObject listener => listener -> IO ()
removeEventListener listener =
  do
    cls' <- getRequiredClass "GKGameSession"
    sendClassMessage cls' removeEventListenerSelector (toNSObject listener)

-- | @- identifier@
identifier :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSString)
identifier gkGameSession =
  sendMessage gkGameSession identifierSelector

-- | @- title@
title :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSString)
title gkGameSession =
  sendMessage gkGameSession titleSelector

-- | @- owner@
owner :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id GKCloudPlayer)
owner gkGameSession =
  sendMessage gkGameSession ownerSelector

-- | @- players@
players :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSArray)
players gkGameSession =
  sendMessage gkGameSession playersSelector

-- | @- lastModifiedDate@
lastModifiedDate :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSDate)
lastModifiedDate gkGameSession =
  sendMessage gkGameSession lastModifiedDateSelector

-- | @- lastModifiedPlayer@
lastModifiedPlayer :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id GKCloudPlayer)
lastModifiedPlayer gkGameSession =
  sendMessage gkGameSession lastModifiedPlayerSelector

-- | @- maxNumberOfConnectedPlayers@
maxNumberOfConnectedPlayers :: IsGKGameSession gkGameSession => gkGameSession -> IO CLong
maxNumberOfConnectedPlayers gkGameSession =
  sendMessage gkGameSession maxNumberOfConnectedPlayersSelector

-- | @- badgedPlayers@
badgedPlayers :: IsGKGameSession gkGameSession => gkGameSession -> IO (Id NSArray)
badgedPlayers gkGameSession =
  sendMessage gkGameSession badgedPlayersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createSessionInContainer:withTitle:maxConnectedPlayers:completionHandler:@
createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector :: Selector '[Id NSString, Id NSString, CLong, Ptr ()] ()
createSessionInContainer_withTitle_maxConnectedPlayers_completionHandlerSelector = mkSelector "createSessionInContainer:withTitle:maxConnectedPlayers:completionHandler:"

-- | @Selector@ for @loadSessionWithIdentifier:completionHandler:@
loadSessionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
loadSessionWithIdentifier_completionHandlerSelector = mkSelector "loadSessionWithIdentifier:completionHandler:"

-- | @Selector@ for @removeSessionWithIdentifier:completionHandler:@
removeSessionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
removeSessionWithIdentifier_completionHandlerSelector = mkSelector "removeSessionWithIdentifier:completionHandler:"

-- | @Selector@ for @getShareURLWithCompletionHandler:@
getShareURLWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getShareURLWithCompletionHandlerSelector = mkSelector "getShareURLWithCompletionHandler:"

-- | @Selector@ for @loadDataWithCompletionHandler:@
loadDataWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadDataWithCompletionHandlerSelector = mkSelector "loadDataWithCompletionHandler:"

-- | @Selector@ for @saveData:completionHandler:@
saveData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
saveData_completionHandlerSelector = mkSelector "saveData:completionHandler:"

-- | @Selector@ for @setConnectionState:completionHandler:@
setConnectionState_completionHandlerSelector :: Selector '[GKConnectionState, Ptr ()] ()
setConnectionState_completionHandlerSelector = mkSelector "setConnectionState:completionHandler:"

-- | @Selector@ for @playersWithConnectionState:@
playersWithConnectionStateSelector :: Selector '[GKConnectionState] (Id NSArray)
playersWithConnectionStateSelector = mkSelector "playersWithConnectionState:"

-- | @Selector@ for @sendData:withTransportType:completionHandler:@
sendData_withTransportType_completionHandlerSelector :: Selector '[Id NSData, GKTransportType, Ptr ()] ()
sendData_withTransportType_completionHandlerSelector = mkSelector "sendData:withTransportType:completionHandler:"

-- | @Selector@ for @sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:@
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector :: Selector '[Id NSString, Id NSArray, Id NSData, Id NSArray, Bool, Ptr ()] ()
sendMessageWithLocalizedFormatKey_arguments_data_toPlayers_badgePlayers_completionHandlerSelector = mkSelector "sendMessageWithLocalizedFormatKey:arguments:data:toPlayers:badgePlayers:completionHandler:"

-- | @Selector@ for @clearBadgeForPlayers:completionHandler:@
clearBadgeForPlayers_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
clearBadgeForPlayers_completionHandlerSelector = mkSelector "clearBadgeForPlayers:completionHandler:"

-- | @Selector@ for @addEventListener:@
addEventListenerSelector :: Selector '[Id NSObject] ()
addEventListenerSelector = mkSelector "addEventListener:"

-- | @Selector@ for @removeEventListener:@
removeEventListenerSelector :: Selector '[Id NSObject] ()
removeEventListenerSelector = mkSelector "removeEventListener:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @owner@
ownerSelector :: Selector '[] (Id GKCloudPlayer)
ownerSelector = mkSelector "owner"

-- | @Selector@ for @players@
playersSelector :: Selector '[] (Id NSArray)
playersSelector = mkSelector "players"

-- | @Selector@ for @lastModifiedDate@
lastModifiedDateSelector :: Selector '[] (Id NSDate)
lastModifiedDateSelector = mkSelector "lastModifiedDate"

-- | @Selector@ for @lastModifiedPlayer@
lastModifiedPlayerSelector :: Selector '[] (Id GKCloudPlayer)
lastModifiedPlayerSelector = mkSelector "lastModifiedPlayer"

-- | @Selector@ for @maxNumberOfConnectedPlayers@
maxNumberOfConnectedPlayersSelector :: Selector '[] CLong
maxNumberOfConnectedPlayersSelector = mkSelector "maxNumberOfConnectedPlayers"

-- | @Selector@ for @badgedPlayers@
badgedPlayersSelector :: Selector '[] (Id NSArray)
badgedPlayersSelector = mkSelector "badgedPlayers"

