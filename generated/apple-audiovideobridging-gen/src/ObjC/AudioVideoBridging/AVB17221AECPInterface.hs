{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPInterface
--
-- AVB17221AECPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol interface.
--
-- AVB17221AECPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP) interface.				It provides callbacks per entity EntityID via a handler object implementing the AVB17221AECPClient protocol. AVB17221AECPInterface objects				are typically not created directly but are created indirectly and accessed via the aecp property of the AVBInterface object.
--
-- Generated bindings for @AVB17221AECPInterface@.
module ObjC.AudioVideoBridging.AVB17221AECPInterface
  ( AVB17221AECPInterface
  , IsAVB17221AECPInterface(..)
  , aecpInterfaceWithInterface
  , aecpInterfaceWithInterfaceNamed
  , setCommandHandler_forEntityID
  , removeCommandHandlerForEntityID
  , setResponseHandler_forControllerEntityID
  , removeResponseHandlerForControllerEntityID
  , sendCommand_toMACAddress_completionHandler
  , sendResponse_toMACAddress_error
  , sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandler
  , aecpInterfaceWithInterfaceNamedSelector
  , aecpInterfaceWithInterfaceSelector
  , removeCommandHandlerForEntityIDSelector
  , removeResponseHandlerForControllerEntityIDSelector
  , sendCommand_toMACAddress_completionHandlerSelector
  , sendResponse_toMACAddress_errorSelector
  , sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector
  , setCommandHandler_forEntityIDSelector
  , setResponseHandler_forControllerEntityIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | AECPInterfaceWithInterface:
--
-- Creates an autoreleased instance of AVB17221AECPInterface for the specified AVBInterface
--
-- @anInterface@ — The interface on which to create the instance.
--
-- Returns: An autoreleased instance of the control service class.
--
-- ObjC selector: @+ AECPInterfaceWithInterface:@
aecpInterfaceWithInterface :: IsAVBInterface anInterface => anInterface -> IO (Id AVB17221AECPInterface)
aecpInterfaceWithInterface anInterface =
  do
    cls' <- getRequiredClass "AVB17221AECPInterface"
    sendClassMessage cls' aecpInterfaceWithInterfaceSelector (toAVBInterface anInterface)

-- | AECPInterfaceWithInterfaceNamed:
--
-- Creates an autoreleased instance of AVB17221AECPInterfaceWithInterfaceNamed for the specified network interface with the specified BSD name.
--
-- @anInterfaceName@ — The BSD name of the interface.
--
-- Returns: An autoreleased instance of the control service class.
--
-- ObjC selector: @+ AECPInterfaceWithInterfaceNamed:@
aecpInterfaceWithInterfaceNamed :: IsNSString anInterfaceName => anInterfaceName -> IO (Id AVB17221AECPInterface)
aecpInterfaceWithInterfaceNamed anInterfaceName =
  do
    cls' <- getRequiredClass "AVB17221AECPInterface"
    sendClassMessage cls' aecpInterfaceWithInterfaceNamedSelector (toNSString anInterfaceName)

-- | setCommandHandler:forEntityID:
--
-- Add an object implementing the AVB17221AECPClient protocol as a handler for command messages to a specified Entity ID.
--
-- @handler@ — The object which will handle all of the commands.
--
-- @targetEntityID@ — The Entity ID of the entity the messages are to.
--
-- Returns: YES if the handler was added, NO if there is already a handler or if the routing ID couldn't be added.
--
-- ObjC selector: @- setCommandHandler:forEntityID:@
setCommandHandler_forEntityID :: IsAVB17221AECPInterface avB17221AECPInterface => avB17221AECPInterface -> RawId -> CULong -> IO Bool
setCommandHandler_forEntityID avB17221AECPInterface handler targetEntityID =
  sendMessage avB17221AECPInterface setCommandHandler_forEntityIDSelector handler targetEntityID

-- | removeCommandHandlerForEntityID:
--
-- Removed a handler for command messages to or from a specified EntityID.
--
-- @targetEntityID@ — The EntityID of the entity the messages are to.
--
-- ObjC selector: @- removeCommandHandlerForEntityID:@
removeCommandHandlerForEntityID :: IsAVB17221AECPInterface avB17221AECPInterface => avB17221AECPInterface -> CULong -> IO ()
removeCommandHandlerForEntityID avB17221AECPInterface targetEntityID =
  sendMessage avB17221AECPInterface removeCommandHandlerForEntityIDSelector targetEntityID

-- | setResponseHandler:forControllerEntityID:
--
-- Add an object implementing the AVB17221AECPClient protocol as a handler for response messages for a specified controller EntityID.
--
-- @handler@ — The object which will handle all of the responses for the Controller Entity ID.
--
-- @controllerEntityID@ — The Entity ID of the controller the messages are for.
--
-- Returns: YES if the handler was added, NO if there is already a handler or if the routing ID couldn't be added.
--
-- ObjC selector: @- setResponseHandler:forControllerEntityID:@
setResponseHandler_forControllerEntityID :: IsAVB17221AECPInterface avB17221AECPInterface => avB17221AECPInterface -> RawId -> CULong -> IO Bool
setResponseHandler_forControllerEntityID avB17221AECPInterface handler controllerEntityID =
  sendMessage avB17221AECPInterface setResponseHandler_forControllerEntityIDSelector handler controllerEntityID

-- | removeResponseHandlerForControllerEntityID:
--
-- Removed a handler for response messages to or from a specified EntityID.
--
-- @controllerEntityID@ — The EntityID of the controller the messages are for.
--
-- ObjC selector: @- removeResponseHandlerForControllerEntityID:@
removeResponseHandlerForControllerEntityID :: IsAVB17221AECPInterface avB17221AECPInterface => avB17221AECPInterface -> CULong -> IO ()
removeResponseHandlerForControllerEntityID avB17221AECPInterface controllerEntityID =
  sendMessage avB17221AECPInterface removeResponseHandlerForControllerEntityIDSelector controllerEntityID

-- | sendCommand:toMACAddress:completionHandler:
--
-- Send an AECP command message.
--
-- @message@ — An instance of a subclass of AVB17221AECPMessage which contains the command message.
--
-- @destMAC@ — The MAC address of the end station to send the message to.
--
-- @completionHandler@ — A block containing code to execute when the command has been sent or timed out.
--
-- Returns: A BOOL indicating success or failure
--
-- This method synchronizes access to the kernel service providing transport for the message. This method is safe to call from any thread.
--
-- ObjC selector: @- sendCommand:toMACAddress:completionHandler:@
sendCommand_toMACAddress_completionHandler :: (IsAVB17221AECPInterface avB17221AECPInterface, IsAVB17221AECPMessage message, IsAVBMACAddress destMAC) => avB17221AECPInterface -> message -> destMAC -> Ptr () -> IO Bool
sendCommand_toMACAddress_completionHandler avB17221AECPInterface message destMAC completionHandler =
  sendMessage avB17221AECPInterface sendCommand_toMACAddress_completionHandlerSelector (toAVB17221AECPMessage message) (toAVBMACAddress destMAC) completionHandler

-- | sendResponse:toMACAddress:error:
--
-- Send an AECP response.
--
-- @message@ — An instance of a subclass of AVB17221AECPMessage which contains the response message.
--
-- @destMAC@ — The MAC address of the end station to send the message to. This argument needs to points to kIOEthernetAddressSize bytes of memory.
--
-- Returns: IOReturn indicating success or failure and reason for failure.
--
-- This method synchronizes access to the kernel service providing transport for the message. This method is safe to call from any thread.
--
-- ObjC selector: @- sendResponse:toMACAddress:error:@
sendResponse_toMACAddress_error :: (IsAVB17221AECPInterface avB17221AECPInterface, IsAVB17221AECPMessage message, IsAVBMACAddress destMAC, IsNSError error_) => avB17221AECPInterface -> message -> destMAC -> error_ -> IO Bool
sendResponse_toMACAddress_error avB17221AECPInterface message destMAC error_ =
  sendMessage avB17221AECPInterface sendResponse_toMACAddress_errorSelector (toAVB17221AECPMessage message) (toAVBMACAddress destMAC) (toNSError error_)

-- | sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:
--
-- Send an AECP vendor unique command message expected to receive a matching response.
--
-- @message@ — An instance of AVB17221AECPVendorMessage which contains the command message.
--
-- @destMAC@ — The MAC address of the end station to send the message to.
--
-- @timeout@ — The number of milliseconds before the command times out.
--
-- @completionHandler@ — A block containing code to execute when the command has been sent or timed out.
--
-- Returns: A BOOL indicating success or failure
--
-- This method synchronizes access to the kernel service providing transport for the message. This method is safe to call from any thread.
--
-- ObjC selector: @- sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:@
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandler :: (IsAVB17221AECPInterface avB17221AECPInterface, IsAVB17221AECPVendorMessage message, IsAVBMACAddress destMAC) => avB17221AECPInterface -> message -> destMAC -> CLong -> Ptr () -> IO Bool
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandler avB17221AECPInterface message destMAC timeout completionHandler =
  sendMessage avB17221AECPInterface sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector (toAVB17221AECPVendorMessage message) (toAVBMACAddress destMAC) timeout completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @AECPInterfaceWithInterface:@
aecpInterfaceWithInterfaceSelector :: Selector '[Id AVBInterface] (Id AVB17221AECPInterface)
aecpInterfaceWithInterfaceSelector = mkSelector "AECPInterfaceWithInterface:"

-- | @Selector@ for @AECPInterfaceWithInterfaceNamed:@
aecpInterfaceWithInterfaceNamedSelector :: Selector '[Id NSString] (Id AVB17221AECPInterface)
aecpInterfaceWithInterfaceNamedSelector = mkSelector "AECPInterfaceWithInterfaceNamed:"

-- | @Selector@ for @setCommandHandler:forEntityID:@
setCommandHandler_forEntityIDSelector :: Selector '[RawId, CULong] Bool
setCommandHandler_forEntityIDSelector = mkSelector "setCommandHandler:forEntityID:"

-- | @Selector@ for @removeCommandHandlerForEntityID:@
removeCommandHandlerForEntityIDSelector :: Selector '[CULong] ()
removeCommandHandlerForEntityIDSelector = mkSelector "removeCommandHandlerForEntityID:"

-- | @Selector@ for @setResponseHandler:forControllerEntityID:@
setResponseHandler_forControllerEntityIDSelector :: Selector '[RawId, CULong] Bool
setResponseHandler_forControllerEntityIDSelector = mkSelector "setResponseHandler:forControllerEntityID:"

-- | @Selector@ for @removeResponseHandlerForControllerEntityID:@
removeResponseHandlerForControllerEntityIDSelector :: Selector '[CULong] ()
removeResponseHandlerForControllerEntityIDSelector = mkSelector "removeResponseHandlerForControllerEntityID:"

-- | @Selector@ for @sendCommand:toMACAddress:completionHandler:@
sendCommand_toMACAddress_completionHandlerSelector :: Selector '[Id AVB17221AECPMessage, Id AVBMACAddress, Ptr ()] Bool
sendCommand_toMACAddress_completionHandlerSelector = mkSelector "sendCommand:toMACAddress:completionHandler:"

-- | @Selector@ for @sendResponse:toMACAddress:error:@
sendResponse_toMACAddress_errorSelector :: Selector '[Id AVB17221AECPMessage, Id AVBMACAddress, Id NSError] Bool
sendResponse_toMACAddress_errorSelector = mkSelector "sendResponse:toMACAddress:error:"

-- | @Selector@ for @sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:@
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector :: Selector '[Id AVB17221AECPVendorMessage, Id AVBMACAddress, CLong, Ptr ()] Bool
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector = mkSelector "sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:"

