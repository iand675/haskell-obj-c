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
  , aecpInterfaceWithInterfaceSelector
  , aecpInterfaceWithInterfaceNamedSelector
  , setCommandHandler_forEntityIDSelector
  , removeCommandHandlerForEntityIDSelector
  , setResponseHandler_forControllerEntityIDSelector
  , removeResponseHandlerForControllerEntityIDSelector
  , sendCommand_toMACAddress_completionHandlerSelector
  , sendResponse_toMACAddress_errorSelector
  , sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector


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
    withObjCPtr anInterface $ \raw_anInterface ->
      sendClassMsg cls' (mkSelector "AECPInterfaceWithInterface:") (retPtr retVoid) [argPtr (castPtr raw_anInterface :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
      sendClassMsg cls' (mkSelector "AECPInterfaceWithInterfaceNamed:") (retPtr retVoid) [argPtr (castPtr raw_anInterfaceName :: Ptr ())] >>= retainedObject . castPtr

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
setCommandHandler_forEntityID avB17221AECPInterface  handler targetEntityID =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPInterface (mkSelector "setCommandHandler:forEntityID:") retCULong [argPtr (castPtr (unRawId handler) :: Ptr ()), argCULong (fromIntegral targetEntityID)]

-- | removeCommandHandlerForEntityID:
--
-- Removed a handler for command messages to or from a specified EntityID.
--
-- @targetEntityID@ — The EntityID of the entity the messages are to.
--
-- ObjC selector: @- removeCommandHandlerForEntityID:@
removeCommandHandlerForEntityID :: IsAVB17221AECPInterface avB17221AECPInterface => avB17221AECPInterface -> CULong -> IO ()
removeCommandHandlerForEntityID avB17221AECPInterface  targetEntityID =
  sendMsg avB17221AECPInterface (mkSelector "removeCommandHandlerForEntityID:") retVoid [argCULong (fromIntegral targetEntityID)]

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
setResponseHandler_forControllerEntityID avB17221AECPInterface  handler controllerEntityID =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPInterface (mkSelector "setResponseHandler:forControllerEntityID:") retCULong [argPtr (castPtr (unRawId handler) :: Ptr ()), argCULong (fromIntegral controllerEntityID)]

-- | removeResponseHandlerForControllerEntityID:
--
-- Removed a handler for response messages to or from a specified EntityID.
--
-- @controllerEntityID@ — The EntityID of the controller the messages are for.
--
-- ObjC selector: @- removeResponseHandlerForControllerEntityID:@
removeResponseHandlerForControllerEntityID :: IsAVB17221AECPInterface avB17221AECPInterface => avB17221AECPInterface -> CULong -> IO ()
removeResponseHandlerForControllerEntityID avB17221AECPInterface  controllerEntityID =
  sendMsg avB17221AECPInterface (mkSelector "removeResponseHandlerForControllerEntityID:") retVoid [argCULong (fromIntegral controllerEntityID)]

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
sendCommand_toMACAddress_completionHandler avB17221AECPInterface  message destMAC completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr destMAC $ \raw_destMAC ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPInterface (mkSelector "sendCommand:toMACAddress:completionHandler:") retCULong [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_destMAC :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
sendResponse_toMACAddress_error avB17221AECPInterface  message destMAC error_ =
withObjCPtr message $ \raw_message ->
  withObjCPtr destMAC $ \raw_destMAC ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPInterface (mkSelector "sendResponse:toMACAddress:error:") retCULong [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_destMAC :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

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
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandler avB17221AECPInterface  message destMAC timeout completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr destMAC $ \raw_destMAC ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPInterface (mkSelector "sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:") retCULong [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_destMAC :: Ptr ()), argCLong (fromIntegral timeout), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @AECPInterfaceWithInterface:@
aecpInterfaceWithInterfaceSelector :: Selector
aecpInterfaceWithInterfaceSelector = mkSelector "AECPInterfaceWithInterface:"

-- | @Selector@ for @AECPInterfaceWithInterfaceNamed:@
aecpInterfaceWithInterfaceNamedSelector :: Selector
aecpInterfaceWithInterfaceNamedSelector = mkSelector "AECPInterfaceWithInterfaceNamed:"

-- | @Selector@ for @setCommandHandler:forEntityID:@
setCommandHandler_forEntityIDSelector :: Selector
setCommandHandler_forEntityIDSelector = mkSelector "setCommandHandler:forEntityID:"

-- | @Selector@ for @removeCommandHandlerForEntityID:@
removeCommandHandlerForEntityIDSelector :: Selector
removeCommandHandlerForEntityIDSelector = mkSelector "removeCommandHandlerForEntityID:"

-- | @Selector@ for @setResponseHandler:forControllerEntityID:@
setResponseHandler_forControllerEntityIDSelector :: Selector
setResponseHandler_forControllerEntityIDSelector = mkSelector "setResponseHandler:forControllerEntityID:"

-- | @Selector@ for @removeResponseHandlerForControllerEntityID:@
removeResponseHandlerForControllerEntityIDSelector :: Selector
removeResponseHandlerForControllerEntityIDSelector = mkSelector "removeResponseHandlerForControllerEntityID:"

-- | @Selector@ for @sendCommand:toMACAddress:completionHandler:@
sendCommand_toMACAddress_completionHandlerSelector :: Selector
sendCommand_toMACAddress_completionHandlerSelector = mkSelector "sendCommand:toMACAddress:completionHandler:"

-- | @Selector@ for @sendResponse:toMACAddress:error:@
sendResponse_toMACAddress_errorSelector :: Selector
sendResponse_toMACAddress_errorSelector = mkSelector "sendResponse:toMACAddress:error:"

-- | @Selector@ for @sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:@
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector :: Selector
sendVendorUniqueCommand_toMACAddress_expectResponseWithinTimeout_completionHandlerSelector = mkSelector "sendVendorUniqueCommand:toMACAddress:expectResponseWithinTimeout:completionHandler:"

