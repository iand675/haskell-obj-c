{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionWebSocketTask@.
module ObjC.Foundation.NSURLSessionWebSocketTask
  ( NSURLSessionWebSocketTask
  , IsNSURLSessionWebSocketTask(..)
  , sendMessage_completionHandler
  , receiveMessageWithCompletionHandler
  , sendPingWithPongReceiveHandler
  , cancelWithCloseCode_reason
  , init_
  , new
  , maximumMessageSize
  , setMaximumMessageSize
  , closeCode
  , closeReason
  , cancelWithCloseCode_reasonSelector
  , closeCodeSelector
  , closeReasonSelector
  , initSelector
  , maximumMessageSizeSelector
  , newSelector
  , receiveMessageWithCompletionHandlerSelector
  , sendMessage_completionHandlerSelector
  , sendPingWithPongReceiveHandlerSelector
  , setMaximumMessageSizeSelector

  -- * Enum types
  , NSURLSessionWebSocketCloseCode(NSURLSessionWebSocketCloseCode)
  , pattern NSURLSessionWebSocketCloseCodeInvalid
  , pattern NSURLSessionWebSocketCloseCodeNormalClosure
  , pattern NSURLSessionWebSocketCloseCodeGoingAway
  , pattern NSURLSessionWebSocketCloseCodeProtocolError
  , pattern NSURLSessionWebSocketCloseCodeUnsupportedData
  , pattern NSURLSessionWebSocketCloseCodeNoStatusReceived
  , pattern NSURLSessionWebSocketCloseCodeAbnormalClosure
  , pattern NSURLSessionWebSocketCloseCodeInvalidFramePayloadData
  , pattern NSURLSessionWebSocketCloseCodePolicyViolation
  , pattern NSURLSessionWebSocketCloseCodeMessageTooBig
  , pattern NSURLSessionWebSocketCloseCodeMandatoryExtensionMissing
  , pattern NSURLSessionWebSocketCloseCodeInternalServerError
  , pattern NSURLSessionWebSocketCloseCodeTLSHandshakeFailure

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- sendMessage:completionHandler:@
sendMessage_completionHandler :: (IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask, IsNSURLSessionWebSocketMessage message) => nsurlSessionWebSocketTask -> message -> Ptr () -> IO ()
sendMessage_completionHandler nsurlSessionWebSocketTask message completionHandler =
  sendMessage nsurlSessionWebSocketTask sendMessage_completionHandlerSelector (toNSURLSessionWebSocketMessage message) completionHandler

-- | @- receiveMessageWithCompletionHandler:@
receiveMessageWithCompletionHandler :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> Ptr () -> IO ()
receiveMessageWithCompletionHandler nsurlSessionWebSocketTask completionHandler =
  sendMessage nsurlSessionWebSocketTask receiveMessageWithCompletionHandlerSelector completionHandler

-- | @- sendPingWithPongReceiveHandler:@
sendPingWithPongReceiveHandler :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> Ptr () -> IO ()
sendPingWithPongReceiveHandler nsurlSessionWebSocketTask pongReceiveHandler =
  sendMessage nsurlSessionWebSocketTask sendPingWithPongReceiveHandlerSelector pongReceiveHandler

-- | @- cancelWithCloseCode:reason:@
cancelWithCloseCode_reason :: (IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask, IsNSData reason) => nsurlSessionWebSocketTask -> NSURLSessionWebSocketCloseCode -> reason -> IO ()
cancelWithCloseCode_reason nsurlSessionWebSocketTask closeCode reason =
  sendMessage nsurlSessionWebSocketTask cancelWithCloseCode_reasonSelector closeCode (toNSData reason)

-- | @- init@
init_ :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO (Id NSURLSessionWebSocketTask)
init_ nsurlSessionWebSocketTask =
  sendOwnedMessage nsurlSessionWebSocketTask initSelector

-- | @+ new@
new :: IO (Id NSURLSessionWebSocketTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionWebSocketTask"
    sendOwnedClassMessage cls' newSelector

-- | @- maximumMessageSize@
maximumMessageSize :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO CLong
maximumMessageSize nsurlSessionWebSocketTask =
  sendMessage nsurlSessionWebSocketTask maximumMessageSizeSelector

-- | @- setMaximumMessageSize:@
setMaximumMessageSize :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> CLong -> IO ()
setMaximumMessageSize nsurlSessionWebSocketTask value =
  sendMessage nsurlSessionWebSocketTask setMaximumMessageSizeSelector value

-- | @- closeCode@
closeCode :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO NSURLSessionWebSocketCloseCode
closeCode nsurlSessionWebSocketTask =
  sendMessage nsurlSessionWebSocketTask closeCodeSelector

-- | @- closeReason@
closeReason :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO (Id NSData)
closeReason nsurlSessionWebSocketTask =
  sendMessage nsurlSessionWebSocketTask closeReasonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendMessage:completionHandler:@
sendMessage_completionHandlerSelector :: Selector '[Id NSURLSessionWebSocketMessage, Ptr ()] ()
sendMessage_completionHandlerSelector = mkSelector "sendMessage:completionHandler:"

-- | @Selector@ for @receiveMessageWithCompletionHandler:@
receiveMessageWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
receiveMessageWithCompletionHandlerSelector = mkSelector "receiveMessageWithCompletionHandler:"

-- | @Selector@ for @sendPingWithPongReceiveHandler:@
sendPingWithPongReceiveHandlerSelector :: Selector '[Ptr ()] ()
sendPingWithPongReceiveHandlerSelector = mkSelector "sendPingWithPongReceiveHandler:"

-- | @Selector@ for @cancelWithCloseCode:reason:@
cancelWithCloseCode_reasonSelector :: Selector '[NSURLSessionWebSocketCloseCode, Id NSData] ()
cancelWithCloseCode_reasonSelector = mkSelector "cancelWithCloseCode:reason:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionWebSocketTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionWebSocketTask)
newSelector = mkSelector "new"

-- | @Selector@ for @maximumMessageSize@
maximumMessageSizeSelector :: Selector '[] CLong
maximumMessageSizeSelector = mkSelector "maximumMessageSize"

-- | @Selector@ for @setMaximumMessageSize:@
setMaximumMessageSizeSelector :: Selector '[CLong] ()
setMaximumMessageSizeSelector = mkSelector "setMaximumMessageSize:"

-- | @Selector@ for @closeCode@
closeCodeSelector :: Selector '[] NSURLSessionWebSocketCloseCode
closeCodeSelector = mkSelector "closeCode"

-- | @Selector@ for @closeReason@
closeReasonSelector :: Selector '[] (Id NSData)
closeReasonSelector = mkSelector "closeReason"

