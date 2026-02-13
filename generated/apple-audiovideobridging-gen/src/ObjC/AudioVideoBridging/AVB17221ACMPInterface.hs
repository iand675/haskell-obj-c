{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221ACMPInterface
--
-- AVB17221ACMPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol interface.
--
-- AVB17221ACMPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol (ACMP) interface.				It provides callbacks per entity EntityID via a handler object implementing the AVB17221ACMPClient protocol. AVB17221ACMPInterface objects				are typically not created directly but are created indirectly and accessed via the acmp property of the AVBInterface object.
--
-- Generated bindings for @AVB17221ACMPInterface@.
module ObjC.AudioVideoBridging.AVB17221ACMPInterface
  ( AVB17221ACMPInterface
  , IsAVB17221ACMPInterface(..)
  , acmpInterfaceWithInterface
  , acmpInterfaceWithInterfaceNamed
  , setHandler_forEntityID
  , removeHandlerForEntityID
  , sendACMPResponseMessage_error
  , sendACMPCommandMessage_completionHandler
  , multicastDestinationAddress
  , acmpInterfaceWithInterfaceNamedSelector
  , acmpInterfaceWithInterfaceSelector
  , multicastDestinationAddressSelector
  , removeHandlerForEntityIDSelector
  , sendACMPCommandMessage_completionHandlerSelector
  , sendACMPResponseMessage_errorSelector
  , setHandler_forEntityIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | AVB17221ACMPInterfaceWithInterface:
--
-- Creates an autoreleased instance of AVB17221ACMPInterface for the specified AVBInterface
--
-- @anInterface@ — The interface on which to create the instance.
--
-- Returns: An autoreleased instance of the control service class.
--
-- ObjC selector: @+ ACMPInterfaceWithInterface:@
acmpInterfaceWithInterface :: IsAVBInterface anInterface => anInterface -> IO (Id AVB17221ACMPInterface)
acmpInterfaceWithInterface anInterface =
  do
    cls' <- getRequiredClass "AVB17221ACMPInterface"
    sendClassMessage cls' acmpInterfaceWithInterfaceSelector (toAVBInterface anInterface)

-- | AVB17221ACMPInterfaceWithInterfaceNamed:
--
-- Creates an autoreleased instance of AVB17221ACMPInterfaceWithInterfaceNamed for the specified network interface with the specified BSD name.
--
-- @anInterfaceName@ — The BSD name of the interface.
--
-- Returns: An autoreleased instance of the control service class.
--
-- ObjC selector: @+ ACMPInterfaceWithInterfaceNamed:@
acmpInterfaceWithInterfaceNamed :: IsNSString anInterfaceName => anInterfaceName -> IO (Id AVB17221ACMPInterface)
acmpInterfaceWithInterfaceNamed anInterfaceName =
  do
    cls' <- getRequiredClass "AVB17221ACMPInterface"
    sendClassMessage cls' acmpInterfaceWithInterfaceNamedSelector (toNSString anInterfaceName)

-- | setHandler:forEntityID:
--
-- Add an object implementing the AVB17221ACMPClient protocol as a handler for messages to or from a specified EntityID.
--
-- @handler@ — The object which will handle all of the commands and responses.
--
-- @targetEntityID@ — The EntityID of the entity the messages are to or from.
--
-- Returns: YES if the handler was added, NO if there is already a handler or if the routing ID couldn't be added.
--
-- ObjC selector: @- setHandler:forEntityID:@
setHandler_forEntityID :: IsAVB17221ACMPInterface avB17221ACMPInterface => avB17221ACMPInterface -> RawId -> CULong -> IO Bool
setHandler_forEntityID avB17221ACMPInterface handler targetEntityID =
  sendMessage avB17221ACMPInterface setHandler_forEntityIDSelector handler targetEntityID

-- | removeHandlerForEntityID:
--
-- Removed a handler  for messages to or from a specified EntityID.
--
-- @targetEntityID@ — The EntityID of the entity the messages are to or from.
--
-- ObjC selector: @- removeHandlerForEntityID:@
removeHandlerForEntityID :: IsAVB17221ACMPInterface avB17221ACMPInterface => avB17221ACMPInterface -> CULong -> IO ()
removeHandlerForEntityID avB17221ACMPInterface targetEntityID =
  sendMessage avB17221ACMPInterface removeHandlerForEntityIDSelector targetEntityID

-- | sendACMPResponseMessage:
--
-- Send an ACMP response message.
--
-- @message@ — A pointer to an AVB17221ACMPMessage struct containing the ACMP message to send.
--
-- Returns: kIOReturnSuccess if the message was successfully sent, otherwise an error indicating failure reason.
--
-- This method synchronizes access to sending ACMP messages, and can safely be called from multiple threads and while handling a received command.
--
-- ObjC selector: @- sendACMPResponseMessage:error:@
sendACMPResponseMessage_error :: (IsAVB17221ACMPInterface avB17221ACMPInterface, IsAVB17221ACMPMessage message, IsNSError error_) => avB17221ACMPInterface -> message -> error_ -> IO Bool
sendACMPResponseMessage_error avB17221ACMPInterface message error_ =
  sendMessage avB17221ACMPInterface sendACMPResponseMessage_errorSelector (toAVB17221ACMPMessage message) (toNSError error_)

-- | sendACMPCommandMessage:completionHandler:
--
-- Send an ACMP command message.
--
-- @message@ — A pointer to an AVB17221ACMPMessage struct containing the ACMP message.
--
-- @completionHandler@ — A block containing code to execute when the command has been sent or timed out.
--
-- Returns: kIOReturnSuccess if the message was successfully sent, otherwise an error indicating failure reason.
--
-- This method synchronizes access to sending ACMP messages, and can safely be called from multiple threads. The completionHandler 				is synchronized with the reception of messages from the kernel object providing the command transport. This method handles the retry				and message timeout per the IEEE Std 1722.1™-2013 standard timeouts.
--
-- ObjC selector: @- sendACMPCommandMessage:completionHandler:@
sendACMPCommandMessage_completionHandler :: (IsAVB17221ACMPInterface avB17221ACMPInterface, IsAVB17221ACMPMessage message) => avB17221ACMPInterface -> message -> Ptr () -> IO Bool
sendACMPCommandMessage_completionHandler avB17221ACMPInterface message completionHandler =
  sendMessage avB17221ACMPInterface sendACMPCommandMessage_completionHandlerSelector (toAVB17221ACMPMessage message) completionHandler

-- | multicastDestinationAddress
--
-- An AVBMACAddress of the multicast destination MAC address being used for all ACMP messages on the interface.
--
-- The MAC Address pointed to by the property is pre-initialized with the IEEE Std 1722.1™-2013 standard value, 91:e0:f0:01:00:00
--
-- ObjC selector: @- multicastDestinationAddress@
multicastDestinationAddress :: IsAVB17221ACMPInterface avB17221ACMPInterface => avB17221ACMPInterface -> IO (Id AVBMACAddress)
multicastDestinationAddress avB17221ACMPInterface =
  sendMessage avB17221ACMPInterface multicastDestinationAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ACMPInterfaceWithInterface:@
acmpInterfaceWithInterfaceSelector :: Selector '[Id AVBInterface] (Id AVB17221ACMPInterface)
acmpInterfaceWithInterfaceSelector = mkSelector "ACMPInterfaceWithInterface:"

-- | @Selector@ for @ACMPInterfaceWithInterfaceNamed:@
acmpInterfaceWithInterfaceNamedSelector :: Selector '[Id NSString] (Id AVB17221ACMPInterface)
acmpInterfaceWithInterfaceNamedSelector = mkSelector "ACMPInterfaceWithInterfaceNamed:"

-- | @Selector@ for @setHandler:forEntityID:@
setHandler_forEntityIDSelector :: Selector '[RawId, CULong] Bool
setHandler_forEntityIDSelector = mkSelector "setHandler:forEntityID:"

-- | @Selector@ for @removeHandlerForEntityID:@
removeHandlerForEntityIDSelector :: Selector '[CULong] ()
removeHandlerForEntityIDSelector = mkSelector "removeHandlerForEntityID:"

-- | @Selector@ for @sendACMPResponseMessage:error:@
sendACMPResponseMessage_errorSelector :: Selector '[Id AVB17221ACMPMessage, Id NSError] Bool
sendACMPResponseMessage_errorSelector = mkSelector "sendACMPResponseMessage:error:"

-- | @Selector@ for @sendACMPCommandMessage:completionHandler:@
sendACMPCommandMessage_completionHandlerSelector :: Selector '[Id AVB17221ACMPMessage, Ptr ()] Bool
sendACMPCommandMessage_completionHandlerSelector = mkSelector "sendACMPCommandMessage:completionHandler:"

-- | @Selector@ for @multicastDestinationAddress@
multicastDestinationAddressSelector :: Selector '[] (Id AVBMACAddress)
multicastDestinationAddressSelector = mkSelector "multicastDestinationAddress"

