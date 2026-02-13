{-# LANGUAGE DataKinds #-}
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
  , defaultPath
  , createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector
  , createUDPSessionToEndpoint_fromEndpointSelector
  , defaultPathSelector
  , displayMessage_completionHandlerSelector
  , sleepWithCompletionHandlerSelector
  , startSystemExtensionModeSelector
  , wakeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
sleepWithCompletionHandler neProvider completionHandler =
  sendMessage neProvider sleepWithCompletionHandlerSelector completionHandler

-- | wake
--
-- This function is called by the framework immediately after the system wakes up from sleep. Subclass developers can override this method to implement custom behavior such as re-establishing connections or resuming some network activity.
--
-- ObjC selector: @- wake@
wake :: IsNEProvider neProvider => neProvider -> IO ()
wake neProvider =
  sendMessage neProvider wakeSelector

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
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegate neProvider remoteEndpoint enableTLS tlsParameters delegate =
  sendMessage neProvider createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector (toNWEndpoint remoteEndpoint) enableTLS (toNWTLSParameters tlsParameters) delegate

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
createUDPSessionToEndpoint_fromEndpoint neProvider remoteEndpoint localEndpoint =
  sendMessage neProvider createUDPSessionToEndpoint_fromEndpointSelector (toNWEndpoint remoteEndpoint) (toNWHostEndpoint localEndpoint)

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
displayMessage_completionHandler neProvider message completionHandler =
  sendMessage neProvider displayMessage_completionHandlerSelector (toNSString message) completionHandler

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
    sendClassMessage cls' startSystemExtensionModeSelector

-- | defaultPath
--
-- The current default path for connections created by the provider. Use KVO to watch for network changes.
--
-- ObjC selector: @- defaultPath@
defaultPath :: IsNEProvider neProvider => neProvider -> IO (Id NWPath)
defaultPath neProvider =
  sendMessage neProvider defaultPathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sleepWithCompletionHandler:@
sleepWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
sleepWithCompletionHandlerSelector = mkSelector "sleepWithCompletionHandler:"

-- | @Selector@ for @wake@
wakeSelector :: Selector '[] ()
wakeSelector = mkSelector "wake"

-- | @Selector@ for @createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:@
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector :: Selector '[Id NWEndpoint, Bool, Id NWTLSParameters, RawId] (Id NWTCPConnection)
createTCPConnectionToEndpoint_enableTLS_TLSParameters_delegateSelector = mkSelector "createTCPConnectionToEndpoint:enableTLS:TLSParameters:delegate:"

-- | @Selector@ for @createUDPSessionToEndpoint:fromEndpoint:@
createUDPSessionToEndpoint_fromEndpointSelector :: Selector '[Id NWEndpoint, Id NWHostEndpoint] (Id NWUDPSession)
createUDPSessionToEndpoint_fromEndpointSelector = mkSelector "createUDPSessionToEndpoint:fromEndpoint:"

-- | @Selector@ for @displayMessage:completionHandler:@
displayMessage_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
displayMessage_completionHandlerSelector = mkSelector "displayMessage:completionHandler:"

-- | @Selector@ for @startSystemExtensionMode@
startSystemExtensionModeSelector :: Selector '[] ()
startSystemExtensionModeSelector = mkSelector "startSystemExtensionMode"

-- | @Selector@ for @defaultPath@
defaultPathSelector :: Selector '[] (Id NWPath)
defaultPathSelector = mkSelector "defaultPath"

