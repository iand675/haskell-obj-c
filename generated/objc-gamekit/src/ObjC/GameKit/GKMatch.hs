{-# LANGUAGE PatternSynonyms #-}
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
  , expectedPlayerCount
  , playerIDs
  , sendData_toPlayers_dataMode_errorSelector
  , sendDataToAllPlayers_withDataMode_errorSelector
  , disconnectSelector
  , chooseBestHostingPlayerWithCompletionHandlerSelector
  , rematchWithCompletionHandlerSelector
  , voiceChatWithNameSelector
  , chooseBestHostPlayerWithCompletionHandlerSelector
  , sendData_toPlayers_withDataMode_errorSelector
  , expectedPlayerCountSelector
  , playerIDsSelector

  -- * Enum types
  , GKMatchSendDataMode(GKMatchSendDataMode)
  , pattern GKMatchSendDataReliable
  , pattern GKMatchSendDataUnreliable

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

-- | Asynchronously send data to one or more GKPlayers. Returns YES if delivery started, NO if unable to start sending and error will be set.
--
-- ObjC selector: @- sendData:toPlayers:dataMode:error:@
sendData_toPlayers_dataMode_error :: (IsGKMatch gkMatch, IsNSData data_, IsNSArray players, IsNSError error_) => gkMatch -> data_ -> players -> GKMatchSendDataMode -> error_ -> IO Bool
sendData_toPlayers_dataMode_error gkMatch  data_ players mode error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr players $ \raw_players ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkMatch (mkSelector "sendData:toPlayers:dataMode:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argCLong (coerce mode), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Asynchronously broadcasts data to all players. Returns YES if delivery started, NO if unable to start sending and error will be set.
--
-- ObjC selector: @- sendDataToAllPlayers:withDataMode:error:@
sendDataToAllPlayers_withDataMode_error :: (IsGKMatch gkMatch, IsNSData data_, IsNSError error_) => gkMatch -> data_ -> GKMatchSendDataMode -> error_ -> IO Bool
sendDataToAllPlayers_withDataMode_error gkMatch  data_ mode error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkMatch (mkSelector "sendDataToAllPlayers:withDataMode:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argCLong (coerce mode), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Disconnect the match. This will show all other players in the match that the local player has disconnected. This should be called before releasing the match instance.
--
-- ObjC selector: @- disconnect@
disconnect :: IsGKMatch gkMatch => gkMatch -> IO ()
disconnect gkMatch  =
  sendMsg gkMatch (mkSelector "disconnect") retVoid []

-- | Choose the best host from among the connected players using gathered estimates for bandwidth and packet loss. This is intended for applications that wish to implement a client-server model on top of the match. The returned player ID will be nil if the best host cannot currently be determined (e.g. players are still connecting).
--
-- ObjC selector: @- chooseBestHostingPlayerWithCompletionHandler:@
chooseBestHostingPlayerWithCompletionHandler :: IsGKMatch gkMatch => gkMatch -> Ptr () -> IO ()
chooseBestHostingPlayerWithCompletionHandler gkMatch  completionHandler =
  sendMsg gkMatch (mkSelector "chooseBestHostingPlayerWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Automatching to recreate a previous peer-to-peer match that became disconnected. A new match with the same set of players will be returned by the completion handler. All players should perform this when the match has ended for automatching to succeed. Error will be nil on success. Possible reasons for error: 1. Communications failure 2. Timeout
--
-- ObjC selector: @- rematchWithCompletionHandler:@
rematchWithCompletionHandler :: IsGKMatch gkMatch => gkMatch -> Ptr () -> IO ()
rematchWithCompletionHandler gkMatch  completionHandler =
  sendMsg gkMatch (mkSelector "rematchWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | * This method is deprecated. GKVoiceChat is no longer supported. **
--
-- ObjC selector: @- voiceChatWithName:@
voiceChatWithName :: (IsGKMatch gkMatch, IsNSString name) => gkMatch -> name -> IO (Id GKVoiceChat)
voiceChatWithName gkMatch  name =
withObjCPtr name $ \raw_name ->
    sendMsg gkMatch (mkSelector "voiceChatWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- chooseBestHostPlayerWithCompletionHandler:@
chooseBestHostPlayerWithCompletionHandler :: IsGKMatch gkMatch => gkMatch -> Ptr () -> IO ()
chooseBestHostPlayerWithCompletionHandler gkMatch  completionHandler =
  sendMsg gkMatch (mkSelector "chooseBestHostPlayerWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- sendData:toPlayers:withDataMode:error:@
sendData_toPlayers_withDataMode_error :: (IsGKMatch gkMatch, IsNSData data_, IsNSArray playerIDs, IsNSError error_) => gkMatch -> data_ -> playerIDs -> GKMatchSendDataMode -> error_ -> IO Bool
sendData_toPlayers_withDataMode_error gkMatch  data_ playerIDs mode error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr playerIDs $ \raw_playerIDs ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkMatch (mkSelector "sendData:toPlayers:withDataMode:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_playerIDs :: Ptr ()), argCLong (coerce mode), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- expectedPlayerCount@
expectedPlayerCount :: IsGKMatch gkMatch => gkMatch -> IO CULong
expectedPlayerCount gkMatch  =
  sendMsg gkMatch (mkSelector "expectedPlayerCount") retCULong []

-- | * This property is obsolete.  **
--
-- ObjC selector: @- playerIDs@
playerIDs :: IsGKMatch gkMatch => gkMatch -> IO (Id NSArray)
playerIDs gkMatch  =
  sendMsg gkMatch (mkSelector "playerIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendData:toPlayers:dataMode:error:@
sendData_toPlayers_dataMode_errorSelector :: Selector
sendData_toPlayers_dataMode_errorSelector = mkSelector "sendData:toPlayers:dataMode:error:"

-- | @Selector@ for @sendDataToAllPlayers:withDataMode:error:@
sendDataToAllPlayers_withDataMode_errorSelector :: Selector
sendDataToAllPlayers_withDataMode_errorSelector = mkSelector "sendDataToAllPlayers:withDataMode:error:"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @chooseBestHostingPlayerWithCompletionHandler:@
chooseBestHostingPlayerWithCompletionHandlerSelector :: Selector
chooseBestHostingPlayerWithCompletionHandlerSelector = mkSelector "chooseBestHostingPlayerWithCompletionHandler:"

-- | @Selector@ for @rematchWithCompletionHandler:@
rematchWithCompletionHandlerSelector :: Selector
rematchWithCompletionHandlerSelector = mkSelector "rematchWithCompletionHandler:"

-- | @Selector@ for @voiceChatWithName:@
voiceChatWithNameSelector :: Selector
voiceChatWithNameSelector = mkSelector "voiceChatWithName:"

-- | @Selector@ for @chooseBestHostPlayerWithCompletionHandler:@
chooseBestHostPlayerWithCompletionHandlerSelector :: Selector
chooseBestHostPlayerWithCompletionHandlerSelector = mkSelector "chooseBestHostPlayerWithCompletionHandler:"

-- | @Selector@ for @sendData:toPlayers:withDataMode:error:@
sendData_toPlayers_withDataMode_errorSelector :: Selector
sendData_toPlayers_withDataMode_errorSelector = mkSelector "sendData:toPlayers:withDataMode:error:"

-- | @Selector@ for @expectedPlayerCount@
expectedPlayerCountSelector :: Selector
expectedPlayerCountSelector = mkSelector "expectedPlayerCount"

-- | @Selector@ for @playerIDs@
playerIDsSelector :: Selector
playerIDsSelector = mkSelector "playerIDs"

