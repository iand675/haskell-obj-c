{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionMessagePort`` object manages message-based communication with a web extension.
--
-- Contains properties and methods to handle message exchanges with a web extension.
--
-- Generated bindings for @WKWebExtensionMessagePort@.
module ObjC.WebKit.WKWebExtensionMessagePort
  ( WKWebExtensionMessagePort
  , IsWKWebExtensionMessagePort(..)
  , new
  , init_
  , sendMessage_completionHandler
  , disconnect
  , disconnectWithError
  , applicationIdentifier
  , messageHandler
  , setMessageHandler
  , disconnectHandler
  , setDisconnectHandler
  , disconnected
  , applicationIdentifierSelector
  , disconnectHandlerSelector
  , disconnectSelector
  , disconnectWithErrorSelector
  , disconnectedSelector
  , initSelector
  , messageHandlerSelector
  , newSelector
  , sendMessage_completionHandlerSelector
  , setDisconnectHandlerSelector
  , setMessageHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionMessagePort)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionMessagePort"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Id WKWebExtensionMessagePort)
init_ wkWebExtensionMessagePort =
  sendOwnedMessage wkWebExtensionMessagePort initSelector

-- | Sends a message to the connected web extension.
--
-- @message@ — The JSON-serializable message to be sent.
--
-- @completionHandler@ — An optional block to be invoked after the message is sent, taking an optional error.
--
-- Note: The message must be JSON-serializable according to ``NSJSONSerialization``.
--
-- ObjC selector: @- sendMessage:completionHandler:@
sendMessage_completionHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> RawId -> Ptr () -> IO ()
sendMessage_completionHandler wkWebExtensionMessagePort message completionHandler =
  sendMessage wkWebExtensionMessagePort sendMessage_completionHandlerSelector message completionHandler

-- | Disconnects the port, terminating all further messages.
--
-- ObjC selector: @- disconnect@
disconnect :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO ()
disconnect wkWebExtensionMessagePort =
  sendMessage wkWebExtensionMessagePort disconnectSelector

-- | Disconnects the port, terminating all further messages with an optional error.
--
-- @error@ — An optional error indicating the reason for disconnection.
--
-- ObjC selector: @- disconnectWithError:@
disconnectWithError :: (IsWKWebExtensionMessagePort wkWebExtensionMessagePort, IsNSError error_) => wkWebExtensionMessagePort -> error_ -> IO ()
disconnectWithError wkWebExtensionMessagePort error_ =
  sendMessage wkWebExtensionMessagePort disconnectWithErrorSelector (toNSError error_)

-- | The unique identifier for the app to which this port should be connected.
--
-- This identifier is provided by the web extension and may or may not be used by the app. It's up to the app to decide how to interpret this identifier.
--
-- ObjC selector: @- applicationIdentifier@
applicationIdentifier :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Id NSString)
applicationIdentifier wkWebExtensionMessagePort =
  sendMessage wkWebExtensionMessagePort applicationIdentifierSelector

-- | The block to be executed when a message is received from the web extension.
--
-- An optional block to be invoked when a message is received, taking two parameters: the message and an optional error.
--
-- ObjC selector: @- messageHandler@
messageHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Ptr ())
messageHandler wkWebExtensionMessagePort =
  sendMessage wkWebExtensionMessagePort messageHandlerSelector

-- | The block to be executed when a message is received from the web extension.
--
-- An optional block to be invoked when a message is received, taking two parameters: the message and an optional error.
--
-- ObjC selector: @- setMessageHandler:@
setMessageHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> Ptr () -> IO ()
setMessageHandler wkWebExtensionMessagePort value =
  sendMessage wkWebExtensionMessagePort setMessageHandlerSelector value

-- | The block to be executed when the port disconnects.
--
-- An optional block to be invoked when the port disconnects, taking an optional error that indicates if the disconnection was caused by an error.
--
-- ObjC selector: @- disconnectHandler@
disconnectHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Ptr ())
disconnectHandler wkWebExtensionMessagePort =
  sendMessage wkWebExtensionMessagePort disconnectHandlerSelector

-- | The block to be executed when the port disconnects.
--
-- An optional block to be invoked when the port disconnects, taking an optional error that indicates if the disconnection was caused by an error.
--
-- ObjC selector: @- setDisconnectHandler:@
setDisconnectHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> Ptr () -> IO ()
setDisconnectHandler wkWebExtensionMessagePort value =
  sendMessage wkWebExtensionMessagePort setDisconnectHandlerSelector value

-- | Indicates whether the message port is disconnected.
--
-- ObjC selector: @- disconnected@
disconnected :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO Bool
disconnected wkWebExtensionMessagePort =
  sendMessage wkWebExtensionMessagePort disconnectedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionMessagePort)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionMessagePort)
initSelector = mkSelector "init"

-- | @Selector@ for @sendMessage:completionHandler:@
sendMessage_completionHandlerSelector :: Selector '[RawId, Ptr ()] ()
sendMessage_completionHandlerSelector = mkSelector "sendMessage:completionHandler:"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector '[] ()
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @disconnectWithError:@
disconnectWithErrorSelector :: Selector '[Id NSError] ()
disconnectWithErrorSelector = mkSelector "disconnectWithError:"

-- | @Selector@ for @applicationIdentifier@
applicationIdentifierSelector :: Selector '[] (Id NSString)
applicationIdentifierSelector = mkSelector "applicationIdentifier"

-- | @Selector@ for @messageHandler@
messageHandlerSelector :: Selector '[] (Ptr ())
messageHandlerSelector = mkSelector "messageHandler"

-- | @Selector@ for @setMessageHandler:@
setMessageHandlerSelector :: Selector '[Ptr ()] ()
setMessageHandlerSelector = mkSelector "setMessageHandler:"

-- | @Selector@ for @disconnectHandler@
disconnectHandlerSelector :: Selector '[] (Ptr ())
disconnectHandlerSelector = mkSelector "disconnectHandler"

-- | @Selector@ for @setDisconnectHandler:@
setDisconnectHandlerSelector :: Selector '[Ptr ()] ()
setDisconnectHandlerSelector = mkSelector "setDisconnectHandler:"

-- | @Selector@ for @disconnected@
disconnectedSelector :: Selector '[] Bool
disconnectedSelector = mkSelector "disconnected"

