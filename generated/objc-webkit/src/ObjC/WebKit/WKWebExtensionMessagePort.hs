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
  , newSelector
  , initSelector
  , sendMessage_completionHandlerSelector
  , disconnectSelector
  , disconnectWithErrorSelector
  , applicationIdentifierSelector
  , messageHandlerSelector
  , setMessageHandlerSelector
  , disconnectHandlerSelector
  , setDisconnectHandlerSelector
  , disconnectedSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionMessagePort)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionMessagePort"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Id WKWebExtensionMessagePort)
init_ wkWebExtensionMessagePort  =
  sendMsg wkWebExtensionMessagePort (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
sendMessage_completionHandler wkWebExtensionMessagePort  message completionHandler =
  sendMsg wkWebExtensionMessagePort (mkSelector "sendMessage:completionHandler:") retVoid [argPtr (castPtr (unRawId message) :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Disconnects the port, terminating all further messages.
--
-- ObjC selector: @- disconnect@
disconnect :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO ()
disconnect wkWebExtensionMessagePort  =
  sendMsg wkWebExtensionMessagePort (mkSelector "disconnect") retVoid []

-- | Disconnects the port, terminating all further messages with an optional error.
--
-- @error@ — An optional error indicating the reason for disconnection.
--
-- ObjC selector: @- disconnectWithError:@
disconnectWithError :: (IsWKWebExtensionMessagePort wkWebExtensionMessagePort, IsNSError error_) => wkWebExtensionMessagePort -> error_ -> IO ()
disconnectWithError wkWebExtensionMessagePort  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg wkWebExtensionMessagePort (mkSelector "disconnectWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | The unique identifier for the app to which this port should be connected.
--
-- This identifier is provided by the web extension and may or may not be used by the app. It's up to the app to decide how to interpret this identifier.
--
-- ObjC selector: @- applicationIdentifier@
applicationIdentifier :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Id NSString)
applicationIdentifier wkWebExtensionMessagePort  =
  sendMsg wkWebExtensionMessagePort (mkSelector "applicationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The block to be executed when a message is received from the web extension.
--
-- An optional block to be invoked when a message is received, taking two parameters: the message and an optional error.
--
-- ObjC selector: @- messageHandler@
messageHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Ptr ())
messageHandler wkWebExtensionMessagePort  =
  fmap castPtr $ sendMsg wkWebExtensionMessagePort (mkSelector "messageHandler") (retPtr retVoid) []

-- | The block to be executed when a message is received from the web extension.
--
-- An optional block to be invoked when a message is received, taking two parameters: the message and an optional error.
--
-- ObjC selector: @- setMessageHandler:@
setMessageHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> Ptr () -> IO ()
setMessageHandler wkWebExtensionMessagePort  value =
  sendMsg wkWebExtensionMessagePort (mkSelector "setMessageHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | The block to be executed when the port disconnects.
--
-- An optional block to be invoked when the port disconnects, taking an optional error that indicates if the disconnection was caused by an error.
--
-- ObjC selector: @- disconnectHandler@
disconnectHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO (Ptr ())
disconnectHandler wkWebExtensionMessagePort  =
  fmap castPtr $ sendMsg wkWebExtensionMessagePort (mkSelector "disconnectHandler") (retPtr retVoid) []

-- | The block to be executed when the port disconnects.
--
-- An optional block to be invoked when the port disconnects, taking an optional error that indicates if the disconnection was caused by an error.
--
-- ObjC selector: @- setDisconnectHandler:@
setDisconnectHandler :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> Ptr () -> IO ()
setDisconnectHandler wkWebExtensionMessagePort  value =
  sendMsg wkWebExtensionMessagePort (mkSelector "setDisconnectHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Indicates whether the message port is disconnected.
--
-- ObjC selector: @- disconnected@
disconnected :: IsWKWebExtensionMessagePort wkWebExtensionMessagePort => wkWebExtensionMessagePort -> IO Bool
disconnected wkWebExtensionMessagePort  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMessagePort (mkSelector "disconnected") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sendMessage:completionHandler:@
sendMessage_completionHandlerSelector :: Selector
sendMessage_completionHandlerSelector = mkSelector "sendMessage:completionHandler:"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @disconnectWithError:@
disconnectWithErrorSelector :: Selector
disconnectWithErrorSelector = mkSelector "disconnectWithError:"

-- | @Selector@ for @applicationIdentifier@
applicationIdentifierSelector :: Selector
applicationIdentifierSelector = mkSelector "applicationIdentifier"

-- | @Selector@ for @messageHandler@
messageHandlerSelector :: Selector
messageHandlerSelector = mkSelector "messageHandler"

-- | @Selector@ for @setMessageHandler:@
setMessageHandlerSelector :: Selector
setMessageHandlerSelector = mkSelector "setMessageHandler:"

-- | @Selector@ for @disconnectHandler@
disconnectHandlerSelector :: Selector
disconnectHandlerSelector = mkSelector "disconnectHandler"

-- | @Selector@ for @setDisconnectHandler:@
setDisconnectHandlerSelector :: Selector
setDisconnectHandlerSelector = mkSelector "setDisconnectHandler:"

-- | @Selector@ for @disconnected@
disconnectedSelector :: Selector
disconnectedSelector = mkSelector "disconnected"

