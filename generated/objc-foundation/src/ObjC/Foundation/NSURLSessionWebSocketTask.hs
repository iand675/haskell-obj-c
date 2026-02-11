{-# LANGUAGE PatternSynonyms #-}
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
  , sendMessage_completionHandlerSelector
  , receiveMessageWithCompletionHandlerSelector
  , sendPingWithPongReceiveHandlerSelector
  , cancelWithCloseCode_reasonSelector
  , initSelector
  , newSelector
  , maximumMessageSizeSelector
  , setMaximumMessageSizeSelector
  , closeCodeSelector
  , closeReasonSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- sendMessage:completionHandler:@
sendMessage_completionHandler :: (IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask, IsNSURLSessionWebSocketMessage message) => nsurlSessionWebSocketTask -> message -> Ptr () -> IO ()
sendMessage_completionHandler nsurlSessionWebSocketTask  message completionHandler =
withObjCPtr message $ \raw_message ->
    sendMsg nsurlSessionWebSocketTask (mkSelector "sendMessage:completionHandler:") retVoid [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- receiveMessageWithCompletionHandler:@
receiveMessageWithCompletionHandler :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> Ptr () -> IO ()
receiveMessageWithCompletionHandler nsurlSessionWebSocketTask  completionHandler =
  sendMsg nsurlSessionWebSocketTask (mkSelector "receiveMessageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- sendPingWithPongReceiveHandler:@
sendPingWithPongReceiveHandler :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> Ptr () -> IO ()
sendPingWithPongReceiveHandler nsurlSessionWebSocketTask  pongReceiveHandler =
  sendMsg nsurlSessionWebSocketTask (mkSelector "sendPingWithPongReceiveHandler:") retVoid [argPtr (castPtr pongReceiveHandler :: Ptr ())]

-- | @- cancelWithCloseCode:reason:@
cancelWithCloseCode_reason :: (IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask, IsNSData reason) => nsurlSessionWebSocketTask -> NSURLSessionWebSocketCloseCode -> reason -> IO ()
cancelWithCloseCode_reason nsurlSessionWebSocketTask  closeCode reason =
withObjCPtr reason $ \raw_reason ->
    sendMsg nsurlSessionWebSocketTask (mkSelector "cancelWithCloseCode:reason:") retVoid [argCLong (coerce closeCode), argPtr (castPtr raw_reason :: Ptr ())]

-- | @- init@
init_ :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO (Id NSURLSessionWebSocketTask)
init_ nsurlSessionWebSocketTask  =
  sendMsg nsurlSessionWebSocketTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionWebSocketTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionWebSocketTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- maximumMessageSize@
maximumMessageSize :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO CLong
maximumMessageSize nsurlSessionWebSocketTask  =
  sendMsg nsurlSessionWebSocketTask (mkSelector "maximumMessageSize") retCLong []

-- | @- setMaximumMessageSize:@
setMaximumMessageSize :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> CLong -> IO ()
setMaximumMessageSize nsurlSessionWebSocketTask  value =
  sendMsg nsurlSessionWebSocketTask (mkSelector "setMaximumMessageSize:") retVoid [argCLong (fromIntegral value)]

-- | @- closeCode@
closeCode :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO NSURLSessionWebSocketCloseCode
closeCode nsurlSessionWebSocketTask  =
  fmap (coerce :: CLong -> NSURLSessionWebSocketCloseCode) $ sendMsg nsurlSessionWebSocketTask (mkSelector "closeCode") retCLong []

-- | @- closeReason@
closeReason :: IsNSURLSessionWebSocketTask nsurlSessionWebSocketTask => nsurlSessionWebSocketTask -> IO (Id NSData)
closeReason nsurlSessionWebSocketTask  =
  sendMsg nsurlSessionWebSocketTask (mkSelector "closeReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendMessage:completionHandler:@
sendMessage_completionHandlerSelector :: Selector
sendMessage_completionHandlerSelector = mkSelector "sendMessage:completionHandler:"

-- | @Selector@ for @receiveMessageWithCompletionHandler:@
receiveMessageWithCompletionHandlerSelector :: Selector
receiveMessageWithCompletionHandlerSelector = mkSelector "receiveMessageWithCompletionHandler:"

-- | @Selector@ for @sendPingWithPongReceiveHandler:@
sendPingWithPongReceiveHandlerSelector :: Selector
sendPingWithPongReceiveHandlerSelector = mkSelector "sendPingWithPongReceiveHandler:"

-- | @Selector@ for @cancelWithCloseCode:reason:@
cancelWithCloseCode_reasonSelector :: Selector
cancelWithCloseCode_reasonSelector = mkSelector "cancelWithCloseCode:reason:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @maximumMessageSize@
maximumMessageSizeSelector :: Selector
maximumMessageSizeSelector = mkSelector "maximumMessageSize"

-- | @Selector@ for @setMaximumMessageSize:@
setMaximumMessageSizeSelector :: Selector
setMaximumMessageSizeSelector = mkSelector "setMaximumMessageSize:"

-- | @Selector@ for @closeCode@
closeCodeSelector :: Selector
closeCodeSelector = mkSelector "closeCode"

-- | @Selector@ for @closeReason@
closeReasonSelector :: Selector
closeReasonSelector = mkSelector "closeReason"

