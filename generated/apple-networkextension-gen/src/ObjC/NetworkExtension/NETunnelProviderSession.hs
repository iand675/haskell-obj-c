{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelProviderSession.h
--
-- This file declares the NETunnelProviderSession API. The NETunnelProviderSession API is used to control network tunnel services provided by NETunnelProvider implementations.
--
-- This API is part of NetworkExtension.framework.
--
-- Generated bindings for @NETunnelProviderSession@.
module ObjC.NetworkExtension.NETunnelProviderSession
  ( NETunnelProviderSession
  , IsNETunnelProviderSession(..)
  , startTunnelWithOptions_andReturnError
  , stopTunnel
  , sendProviderMessage_returnError_responseHandler
  , sendProviderMessage_returnError_responseHandlerSelector
  , startTunnelWithOptions_andReturnErrorSelector
  , stopTunnelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | startTunnelWithOptions:andReturnError:
--
-- This function is used to start the tunnel using the configuration associated with this connection object. The tunnel connection process is started and this function returns immediately.
--
-- @options@ — A dictionary that will be passed as-is to the tunnel provider during the process of starting the tunnel.
--
-- @error@ — If the tunnel was started successfully, this parameter is set to nil. Otherwise this parameter is set to the error that occurred. Possible errors include:    1. NEVPNErrorConfigurationInvalid    2. NEVPNErrorConfigurationDisabled
--
-- Returns: YES if the tunnel was started successfully, NO if an error occurred.
--
-- ObjC selector: @- startTunnelWithOptions:andReturnError:@
startTunnelWithOptions_andReturnError :: (IsNETunnelProviderSession neTunnelProviderSession, IsNSDictionary options, IsNSError error_) => neTunnelProviderSession -> options -> error_ -> IO Bool
startTunnelWithOptions_andReturnError neTunnelProviderSession options error_ =
  sendMessage neTunnelProviderSession startTunnelWithOptions_andReturnErrorSelector (toNSDictionary options) (toNSError error_)

-- | stopTunnel
--
-- This function is used to stop the tunnel. The tunnel disconnect process is started and this function returns immediately.
--
-- ObjC selector: @- stopTunnel@
stopTunnel :: IsNETunnelProviderSession neTunnelProviderSession => neTunnelProviderSession -> IO ()
stopTunnel neTunnelProviderSession =
  sendMessage neTunnelProviderSession stopTunnelSelector

-- | sendProviderMessage:responseHandler:
--
-- This function sends a message to the NETunnelProvider and provides a way to receive a response.
--
-- @messageData@ — An NSData object containing the message to be sent.
--
-- @error@ — If the message was sent successfully, this parameter is set to nil. Otherwise this parameter is set to the error that occurred. Possible errors include:    1. NEVPNErrorConfigurationInvalid    2. NEVPNErrorConfigurationDisabled
--
-- @responseHandler@ — A block that handles the response. Can be set to nil if no response is expected.
--
-- Returns: YES if the message was sent successfully, NO if an error occurred.
--
-- ObjC selector: @- sendProviderMessage:returnError:responseHandler:@
sendProviderMessage_returnError_responseHandler :: (IsNETunnelProviderSession neTunnelProviderSession, IsNSData messageData, IsNSError error_) => neTunnelProviderSession -> messageData -> error_ -> Ptr () -> IO Bool
sendProviderMessage_returnError_responseHandler neTunnelProviderSession messageData error_ responseHandler =
  sendMessage neTunnelProviderSession sendProviderMessage_returnError_responseHandlerSelector (toNSData messageData) (toNSError error_) responseHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTunnelWithOptions:andReturnError:@
startTunnelWithOptions_andReturnErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
startTunnelWithOptions_andReturnErrorSelector = mkSelector "startTunnelWithOptions:andReturnError:"

-- | @Selector@ for @stopTunnel@
stopTunnelSelector :: Selector '[] ()
stopTunnelSelector = mkSelector "stopTunnel"

-- | @Selector@ for @sendProviderMessage:returnError:responseHandler:@
sendProviderMessage_returnError_responseHandlerSelector :: Selector '[Id NSData, Id NSError, Ptr ()] Bool
sendProviderMessage_returnError_responseHandlerSelector = mkSelector "sendProviderMessage:returnError:responseHandler:"

