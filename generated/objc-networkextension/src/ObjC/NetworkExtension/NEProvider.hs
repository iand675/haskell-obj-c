{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEProvider
--
-- The NEProvider class declares the programmatic interface that is common for all Network Extension providers.
--
-- See the sub classes of NEProvider for more details. Developers of Network Extension providers should create sub classes of the sub classes of NEProvider.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEProvider@.
module ObjC.NetworkExtension.NEProvider
  ( NEProvider
  , IsNEProvider(..)
  , sleepWithCompletionHandler
  , wake
  , createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegate
  , createUDPSessionToEndpoint_fromEndpoint
  , displayMessage_completionHandler
  , startSystemExtensionMode
  , sleepWithCompletionHandlerSelector
  , wakeSelector
  , createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector
  , createUDPSessionToEndpoint_fromEndpointSelector
  , displayMessage_completionHandlerSelector
  , startSystemExtensionModeSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sleepWithCompletionHandler:
--
-- This function is called by the framework when the system is about to go to sleep. Subclass developers can override this method to implement custom behavior such as closing connections or pausing some network activity.
--
-- @completionHandler@ — When the method is finished handling the sleep event it must execute this completion handler.
--
-- ObjC selector: @- sleepWithCompletionHandler:@
sleepWithCompletionHandler :: IsNEProvider neProvider => neProvider -> Ptr () -> IO ()
sleepWithCompletionHandler neProvider  completionHandler =
  sendMsg neProvider (mkSelector "sleepWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | wake
--
-- This function is called by the framework immediately after the system wakes up from sleep. Subclass developers can override this method to implement custom behavior such as re-establishing connections or resuming some network activity.
--
-- ObjC selector: @- wake@
wake :: IsNEProvider neProvider => neProvider -> IO ()
wake neProvider  =
  sendMsg neProvider (mkSelector "wake") retVoid []

-- | createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:
--
-- This function can be called by subclass implementations to create a TCP connection to a given network endpoint. This function should not be overridden by subclasses.
--
-- @remoteEndpoint@ — An NWEndpoint object that specifies the remote network endpoint to connect to.
--
-- @enableTLS@ — A flag indicating if a TLS session should be negotiated on the connection.
--
-- @TLSParameters@ — A set of optional TLS parameters. Only valid if enableTLS is YES. If TLSParameters is nil, the default system parameters will be used for TLS negotiation.
--
-- @delegate@ — An object to use as the connections delegate. This object should conform to the NWTCPConnectionAuthenticationDelegate protocol.
--
-- Returns: An NWTCPConnection object.
--
-- ObjC selector: @- createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:@
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegate :: (IsNEProvider neProvider, IsNWEndpoint remoteEndpoint, IsNWTLSParameters tlsParameters) => neProvider -> remoteEndpoint -> Bool -> tlsParameters -> RawId -> IO (Id NWTCPConnection)
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegate neProvider  remoteEndpoint enableTLS tlsParameters delegate =
withObjCPtr remoteEndpoint $ \raw_remoteEndpoint ->
  withObjCPtr tlsParameters $ \raw_tlsParameters ->
      sendMsg neProvider (mkSelector "createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:") (retPtr retVoid) [argPtr (castPtr raw_remoteEndpoint :: Ptr ()), argCULong (if enableTLS then 1 else 0), argPtr (castPtr raw_tlsParameters :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= retainedObject . castPtr

-- | createUDPSessionToEndpoint:fromEndpoint:
--
-- This function can be called by subclass implementations to create a UDP session between a local network endpoint and a remote network endpoint. This function should not be overridden by subclasses.
--
-- @remoteEndpoint@ — An NWEndpoint object that specifies the remote endpoint to which UDP datagrams will be sent by the UDP session.
--
-- @localEndpoint@ — An NWHostEndpoint object that specifies the local IP address endpoint to use as the source endpoint of the UDP session.
--
-- Returns: An NWUDPSession object.
--
-- ObjC selector: @- createUDPSessionToEndpoint:fromEndpoint:@
createUDPSessionToEndpoint_fromEndpoint :: (IsNEProvider neProvider, IsNWEndpoint remoteEndpoint, IsNWHostEndpoint localEndpoint) => neProvider -> remoteEndpoint -> localEndpoint -> IO (Id NWUDPSession)
createUDPSessionToEndpoint_fromEndpoint neProvider  remoteEndpoint localEndpoint =
withObjCPtr remoteEndpoint $ \raw_remoteEndpoint ->
  withObjCPtr localEndpoint $ \raw_localEndpoint ->
      sendMsg neProvider (mkSelector "createUDPSessionToEndpoint:fromEndpoint:") (retPtr retVoid) [argPtr (castPtr raw_remoteEndpoint :: Ptr ()), argPtr (castPtr raw_localEndpoint :: Ptr ())] >>= retainedObject . castPtr

-- | displayMessage:completionHandler:
--
-- This method can be called by subclass implementations to display a message to the user.
--
-- @message@ — The message to be displayed.
--
-- @completionHandler@ — A block that is executed when the user acknowledges the message. If this method is called on a NEFilterDataProvider instance or the message cannot be displayed, then the completion handler block will be executed immediately with success parameter set to NO. If the message was successfully displayed to the user, then the completion handler block is executed with the success parameter set to YES when the user dismisses the message.
--
-- ObjC selector: @- displayMessage:completionHandler:@
displayMessage_completionHandler :: (IsNEProvider neProvider, IsNSString message) => neProvider -> message -> Ptr () -> IO ()
displayMessage_completionHandler neProvider  message completionHandler =
withObjCPtr message $ \raw_message ->
    sendMsg neProvider (mkSelector "displayMessage:completionHandler:") retVoid [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | startSystemExtensionMode
--
-- Start the Network Extension machinery in a system extension (.system bundle). This class method will cause the calling system extension to start handling    requests from nesessionmanager to instantiate appropriate NEProvider sub-class instances. The system extension must declare a mapping of Network Extension extension points to    NEProvider sub-class instances in its Info.plist:        Key: NetworkExtension        Type: Dictionary containing information about the NetworkExtension capabilities of the system extension.
--
-- Key: NEProviderClasses            Type: Dictionary mapping NetworkExtension extension point identifiers to NEProvider sub-classes
--
-- Example:
--
-- <key>NetworkExtension</key>        <dict>            <key>NEProviderClasses</key>            <dict>                <key>com.apple.networkextension.app-proxy</key>                <string>$(PRODUCT_MODULE_NAME).AppProxyProvider</string>                <key>com.apple.networkextension.filter-data</key>                <string>$(PRODUCT_MODULE_NAME).FilterDataProvider</string>            </dict>        </dict>
--
-- This method should be called as early as possible after the system extension starts.
--
-- ObjC selector: @+ startSystemExtensionMode@
startSystemExtensionMode :: IO ()
startSystemExtensionMode  =
  do
    cls' <- getRequiredClass "NEProvider"
    sendClassMsg cls' (mkSelector "startSystemExtensionMode") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sleepWithCompletionHandler:@
sleepWithCompletionHandlerSelector :: Selector
sleepWithCompletionHandlerSelector = mkSelector "sleepWithCompletionHandler:"

-- | @Selector@ for @wake@
wakeSelector :: Selector
wakeSelector = mkSelector "wake"

-- | @Selector@ for @createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:@
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector :: Selector
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector = mkSelector "createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:"

-- | @Selector@ for @createUDPSessionToEndpoint:fromEndpoint:@
createUDPSessionToEndpoint_fromEndpointSelector :: Selector
createUDPSessionToEndpoint_fromEndpointSelector = mkSelector "createUDPSessionToEndpoint:fromEndpoint:"

-- | @Selector@ for @displayMessage:completionHandler:@
displayMessage_completionHandlerSelector :: Selector
displayMessage_completionHandlerSelector = mkSelector "displayMessage:completionHandler:"

-- | @Selector@ for @startSystemExtensionMode@
startSystemExtensionModeSelector :: Selector
startSystemExtensionModeSelector = mkSelector "startSystemExtensionMode"

