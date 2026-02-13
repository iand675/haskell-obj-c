{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Socket Device
--
-- This is a paravirtualized socket device which facilitates data transfer between the guest and the host without using Ethernet or IP protocols.    This device is created through instantiating a VZVirtioSocketDeviceConfiguration in a VZVirtualMachineConfiguration and is available in the VZVirtualMachine.socketDevices property.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- See: VZVirtioSocketDeviceConnection
--
-- See: VZVirtioSocketDeviceListener
--
-- Generated bindings for @VZVirtioSocketDevice@.
module ObjC.Virtualization.VZVirtioSocketDevice
  ( VZVirtioSocketDevice
  , IsVZVirtioSocketDevice(..)
  , new
  , init_
  , setSocketListener_forPort
  , removeSocketListenerForPort
  , connectToPort_completionHandler
  , connectToPort_completionHandlerSelector
  , initSelector
  , newSelector
  , removeSocketListenerForPortSelector
  , setSocketListener_forPortSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZVirtioSocketDevice)
new  =
  do
    cls' <- getRequiredClass "VZVirtioSocketDevice"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioSocketDevice vzVirtioSocketDevice => vzVirtioSocketDevice -> IO (Id VZVirtioSocketDevice)
init_ vzVirtioSocketDevice =
  sendOwnedMessage vzVirtioSocketDevice initSelector

-- | Sets a listener at a specified port.
--
-- There is only one listener per port, any existing listener will be removed, and the specified listener here will be set instead.    The same listener can be registered on multiple ports.    The listener's delegate will be called whenever the guest connects to that port.
--
-- @listener@ — The VZVirtioSocketListener object to be set.
--
-- @port@ — The port number to set the listener at.
--
-- ObjC selector: @- setSocketListener:forPort:@
setSocketListener_forPort :: (IsVZVirtioSocketDevice vzVirtioSocketDevice, IsVZVirtioSocketListener listener) => vzVirtioSocketDevice -> listener -> CUInt -> IO ()
setSocketListener_forPort vzVirtioSocketDevice listener port =
  sendMessage vzVirtioSocketDevice setSocketListener_forPortSelector (toVZVirtioSocketListener listener) port

-- | Removes the listener at a specified port.
--
-- Does nothing if the port had no listener.
--
-- @port@ — The port number at which the listener is to be removed.
--
-- ObjC selector: @- removeSocketListenerForPort:@
removeSocketListenerForPort :: IsVZVirtioSocketDevice vzVirtioSocketDevice => vzVirtioSocketDevice -> CUInt -> IO ()
removeSocketListenerForPort vzVirtioSocketDevice port =
  sendMessage vzVirtioSocketDevice removeSocketListenerForPortSelector port

-- | Connects to a specified port.
--
-- Does nothing if the guest does not listen on that port.
--
-- @port@ — The port number to connect to.
--
-- @completionHandler@ — Block called after the connection has been successfully established or on error.    The error parameter passed to the block is nil if the connection was successful.
--
-- ObjC selector: @- connectToPort:completionHandler:@
connectToPort_completionHandler :: IsVZVirtioSocketDevice vzVirtioSocketDevice => vzVirtioSocketDevice -> CUInt -> Ptr () -> IO ()
connectToPort_completionHandler vzVirtioSocketDevice port completionHandler =
  sendMessage vzVirtioSocketDevice connectToPort_completionHandlerSelector port completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioSocketDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSocketDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @setSocketListener:forPort:@
setSocketListener_forPortSelector :: Selector '[Id VZVirtioSocketListener, CUInt] ()
setSocketListener_forPortSelector = mkSelector "setSocketListener:forPort:"

-- | @Selector@ for @removeSocketListenerForPort:@
removeSocketListenerForPortSelector :: Selector '[CUInt] ()
removeSocketListenerForPortSelector = mkSelector "removeSocketListenerForPort:"

-- | @Selector@ for @connectToPort:completionHandler:@
connectToPort_completionHandlerSelector :: Selector '[CUInt, Ptr ()] ()
connectToPort_completionHandlerSelector = mkSelector "connectToPort:completionHandler:"

