{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKMatch represents an active networking sessions between players. It handles network communications and can report player connection status. All matches are created by a GKMatchmaker.
--
-- Generated bindings for @GKMatch@.
module ObjC.GameKit.GKMatch
  ( GKMatch
  , IsGKMatch(..)
  , sendData_toPlayers_dataMode_error
  , sendDataToAllPlayers_withDataMode_error
  , disconnect
  , chooseBestHostingPlayerWithCompletionHandler
  , rematchWithCompletionHandler
  , voiceChatWithName
  , chooseBestHostPlayerWithCompletionHandler
  , sendData_toPlayers_withDataMode_error
  , players
  , delegate
  , setDelegate
  , expectedPlayerCount
  , properties
  , playerProperties
  , playerIDs
  , chooseBestHostPlayerWithCompletionHandlerSelector
  , chooseBestHostingPlayerWithCompletionHandlerSelector
  , delegateSelector
  , disconnectSelector
  , expectedPlayerCountSelector
  , playerIDsSelector
  , playerPropertiesSelector
  , playersSelector
  , propertiesSelector
  , rematchWithCompletionHandlerSelector
  , sendDataToAllPlayers_withDataMode_errorSelector
  , sendData_toPlayers_dataMode_errorSelector
  , sendData_toPlayers_withDataMode_errorSelector
  , setDelegateSelector
  , voiceChatWithNameSelector

  -- * Enum types
  , GKMatchSendDataMode(GKMatchSendDataMode)
  , pattern GKMatchSendDataReliable
  , pattern GKMatchSendDataUnreliable

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

-- | Asynchronously send data to one or more GKPlayers. Returns YES if delivery started, NO if unable to start sending and error will be set.
--
-- ObjC selector: @- sendData:toPlayers:dataMode:error:@
sendData_toPlayers_dataMode_error :: (IsGKMatch gkMatch, IsNSData data_, IsNSArray players, IsNSError error_) => gkMatch -> data_ -> players -> GKMatchSendDataMode -> error_ -> IO Bool
sendData_toPlayers_dataMode_error gkMatch data_ players mode error_ =
  sendMessage gkMatch sendData_toPlayers_dataMode_errorSelector (toNSData data_) (toNSArray players) mode (toNSError error_)

-- | Asynchronously broadcasts data to all players. Returns YES if delivery started, NO if unable to start sending and error will be set.
--
-- ObjC selector: @- sendDataToAllPlayers:withDataMode:error:@
sendDataToAllPlayers_withDataMode_error :: (IsGKMatch gkMatch, IsNSData data_, IsNSError error_) => gkMatch -> data_ -> GKMatchSendDataMode -> error_ -> IO Bool
sendDataToAllPlayers_withDataMode_error gkMatch data_ mode error_ =
  sendMessage gkMatch sendDataToAllPlayers_withDataMode_errorSelector (toNSData data_) mode (toNSError error_)

-- | Disconnect the match. This will show all other players in the match that the local player has disconnected. This should be called before releasing the match instance.
--
-- ObjC selector: @- disconnect@
disconnect :: IsGKMatch gkMatch => gkMatch -> IO ()
disconnect gkMatch =
  sendMessage gkMatch disconnectSelector

-- | Choose the best host from among the connected players using gathered estimates for bandwidth and packet loss. This is intended for applications that wish to implement a client-server model on top of the match. The returned player ID will be nil if the best host cannot currently be determined (e.g. players are still connecting).
--
-- ObjC selector: @- chooseBestHostingPlayerWithCompletionHandler:@
chooseBestHostingPlayerWithCompletionHandler :: IsGKMatch gkMatch => gkMatch -> Ptr () -> IO ()
chooseBestHostingPlayerWithCompletionHandler gkMatch completionHandler =
  sendMessage gkMatch chooseBestHostingPlayerWithCompletionHandlerSelector completionHandler

-- | Automatching to recreate a previous peer-to-peer match that became disconnected. A new match with the same set of players will be returned by the completion handler. All players should perform this when the match has ended for automatching to succeed. Error will be nil on success. Possible reasons for error: 1. Communications failure 2. Timeout
--
-- ObjC selector: @- rematchWithCompletionHandler:@
rematchWithCompletionHandler :: IsGKMatch gkMatch => gkMatch -> Ptr () -> IO ()
rematchWithCompletionHandler gkMatch completionHandler =
  sendMessage gkMatch rematchWithCompletionHandlerSelector completionHandler

-- | * This method is deprecated. GKVoiceChat is no longer supported. **
--
-- ObjC selector: @- voiceChatWithName:@
voiceChatWithName :: (IsGKMatch gkMatch, IsNSString name) => gkMatch -> name -> IO (Id GKVoiceChat)
voiceChatWithName gkMatch name =
  sendMessage gkMatch voiceChatWithNameSelector (toNSString name)

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- chooseBestHostPlayerWithCompletionHandler:@
chooseBestHostPlayerWithCompletionHandler :: IsGKMatch gkMatch => gkMatch -> Ptr () -> IO ()
chooseBestHostPlayerWithCompletionHandler gkMatch completionHandler =
  sendMessage gkMatch chooseBestHostPlayerWithCompletionHandlerSelector completionHandler

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- sendData:toPlayers:withDataMode:error:@
sendData_toPlayers_withDataMode_error :: (IsGKMatch gkMatch, IsNSData data_, IsNSArray playerIDs, IsNSError error_) => gkMatch -> data_ -> playerIDs -> GKMatchSendDataMode -> error_ -> IO Bool
sendData_toPlayers_withDataMode_error gkMatch data_ playerIDs mode error_ =
  sendMessage gkMatch sendData_toPlayers_withDataMode_errorSelector (toNSData data_) (toNSArray playerIDs) mode (toNSError error_)

-- | @- players@
players :: IsGKMatch gkMatch => gkMatch -> IO (Id NSArray)
players gkMatch =
  sendMessage gkMatch playersSelector

-- | all the GKPlayers in the match
--
-- ObjC selector: @- delegate@
delegate :: IsGKMatch gkMatch => gkMatch -> IO RawId
delegate gkMatch =
  sendMessage gkMatch delegateSelector

-- | all the GKPlayers in the match
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsGKMatch gkMatch => gkMatch -> RawId -> IO ()
setDelegate gkMatch value =
  sendMessage gkMatch setDelegateSelector value

-- | @- expectedPlayerCount@
expectedPlayerCount :: IsGKMatch gkMatch => gkMatch -> IO CULong
expectedPlayerCount gkMatch =
  sendMessage gkMatch expectedPlayerCountSelector

-- | @- properties@
properties :: IsGKMatch gkMatch => gkMatch -> IO RawId
properties gkMatch =
  sendMessage gkMatch propertiesSelector

-- | @- playerProperties@
playerProperties :: IsGKMatch gkMatch => gkMatch -> IO (Id NSDictionary)
playerProperties gkMatch =
  sendMessage gkMatch playerPropertiesSelector

-- | * This property is obsolete.  **
--
-- ObjC selector: @- playerIDs@
playerIDs :: IsGKMatch gkMatch => gkMatch -> IO (Id NSArray)
playerIDs gkMatch =
  sendMessage gkMatch playerIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendData:toPlayers:dataMode:error:@
sendData_toPlayers_dataMode_errorSelector :: Selector '[Id NSData, Id NSArray, GKMatchSendDataMode, Id NSError] Bool
sendData_toPlayers_dataMode_errorSelector = mkSelector "sendData:toPlayers:dataMode:error:"

-- | @Selector@ for @sendDataToAllPlayers:withDataMode:error:@
sendDataToAllPlayers_withDataMode_errorSelector :: Selector '[Id NSData, GKMatchSendDataMode, Id NSError] Bool
sendDataToAllPlayers_withDataMode_errorSelector = mkSelector "sendDataToAllPlayers:withDataMode:error:"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector '[] ()
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @chooseBestHostingPlayerWithCompletionHandler:@
chooseBestHostingPlayerWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
chooseBestHostingPlayerWithCompletionHandlerSelector = mkSelector "chooseBestHostingPlayerWithCompletionHandler:"

-- | @Selector@ for @rematchWithCompletionHandler:@
rematchWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
rematchWithCompletionHandlerSelector = mkSelector "rematchWithCompletionHandler:"

-- | @Selector@ for @voiceChatWithName:@
voiceChatWithNameSelector :: Selector '[Id NSString] (Id GKVoiceChat)
voiceChatWithNameSelector = mkSelector "voiceChatWithName:"

-- | @Selector@ for @chooseBestHostPlayerWithCompletionHandler:@
chooseBestHostPlayerWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
chooseBestHostPlayerWithCompletionHandlerSelector = mkSelector "chooseBestHostPlayerWithCompletionHandler:"

-- | @Selector@ for @sendData:toPlayers:withDataMode:error:@
sendData_toPlayers_withDataMode_errorSelector :: Selector '[Id NSData, Id NSArray, GKMatchSendDataMode, Id NSError] Bool
sendData_toPlayers_withDataMode_errorSelector = mkSelector "sendData:toPlayers:withDataMode:error:"

-- | @Selector@ for @players@
playersSelector :: Selector '[] (Id NSArray)
playersSelector = mkSelector "players"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @expectedPlayerCount@
expectedPlayerCountSelector :: Selector '[] CULong
expectedPlayerCountSelector = mkSelector "expectedPlayerCount"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] RawId
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @playerProperties@
playerPropertiesSelector :: Selector '[] (Id NSDictionary)
playerPropertiesSelector = mkSelector "playerProperties"

-- | @Selector@ for @playerIDs@
playerIDsSelector :: Selector '[] (Id NSArray)
playerIDsSelector = mkSelector "playerIDs"

