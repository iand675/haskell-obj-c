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
  , startTunnelWithOptions_andReturnErrorSelector
  , stopTunnelSelector
  , sendProviderMessage_returnError_responseHandlerSelector


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
startTunnelWithOptions_andReturnError neTunnelProviderSession  options error_ =
withObjCPtr options $ \raw_options ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg neTunnelProviderSession (mkSelector "startTunnelWithOptions:andReturnError:") retCULong [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | stopTunnel
--
-- This function is used to stop the tunnel. The tunnel disconnect process is started and this function returns immediately.
--
-- ObjC selector: @- stopTunnel@
stopTunnel :: IsNETunnelProviderSession neTunnelProviderSession => neTunnelProviderSession -> IO ()
stopTunnel neTunnelProviderSession  =
  sendMsg neTunnelProviderSession (mkSelector "stopTunnel") retVoid []

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
sendProviderMessage_returnError_responseHandler neTunnelProviderSession  messageData error_ responseHandler =
withObjCPtr messageData $ \raw_messageData ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg neTunnelProviderSession (mkSelector "sendProviderMessage:returnError:responseHandler:") retCULong [argPtr (castPtr raw_messageData :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr responseHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTunnelWithOptions:andReturnError:@
startTunnelWithOptions_andReturnErrorSelector :: Selector
startTunnelWithOptions_andReturnErrorSelector = mkSelector "startTunnelWithOptions:andReturnError:"

-- | @Selector@ for @stopTunnel@
stopTunnelSelector :: Selector
stopTunnelSelector = mkSelector "stopTunnel"

-- | @Selector@ for @sendProviderMessage:returnError:responseHandler:@
sendProviderMessage_returnError_responseHandlerSelector :: Selector
sendProviderMessage_returnError_responseHandlerSelector = mkSelector "sendProviderMessage:returnError:responseHandler:"

