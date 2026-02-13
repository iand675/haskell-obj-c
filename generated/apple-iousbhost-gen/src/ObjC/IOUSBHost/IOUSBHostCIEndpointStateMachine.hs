{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostCIEndpointStateMachine
--
-- The object representing the state of a user-mode USB host controller endpoint
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller endpoint, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.  Clients should create an IOUSBHostCIEndpointStateMachine in          response to an IOUSBHostCIMessageTypeEndpointCreate command, and then use the provided interfaces to identify and process commands,          doorbells, and transfer structures for the endpoint.  The IOUSBHostCIEndpointStateMachine should be destroyed in response to an          IOUSBHostCIMessageTypeEndpointDestroy command.
--
-- Endpoint state is controlled by IOUSBHostCIMessage structures representing commands and transfer completions, and IOUSBHostCIDoorbell messages.          Only an endpoint in the IOUSBHostCIEndpointStateActive state may inspect transfer structures, read or modify IO buffers, and generate transfer completions.
--
-- IOUSBHostCIEndpointStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
--
-- Generated bindings for @IOUSBHostCIEndpointStateMachine@.
module ObjC.IOUSBHost.IOUSBHostCIEndpointStateMachine
  ( IOUSBHostCIEndpointStateMachine
  , IsIOUSBHostCIEndpointStateMachine(..)
  , init_
  , initWithInterface_command_error
  , inspectCommand_error
  , processDoorbell_error
  , deviceAddress
  , endpointAddress
  , currentTransferMessage
  , controllerInterface
  , controllerInterfaceSelector
  , currentTransferMessageSelector
  , deviceAddressSelector
  , endpointAddressSelector
  , initSelector
  , initWithInterface_command_errorSelector
  , inspectCommand_errorSelector
  , processDoorbell_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOUSBHost.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine => iousbHostCIEndpointStateMachine -> IO (Id IOUSBHostCIEndpointStateMachine)
init_ iousbHostCIEndpointStateMachine =
  sendOwnedMessage iousbHostCIEndpointStateMachine initSelector

-- | Initializes an IOUSBHostCIEndpointStateMachine object
--
-- The IOUSBHostCIEndpointStateMachine defaults to the IOUSBHostCIEndpointStatePaused state.
--
-- @interface@ — IOUSBHostControllerInterface which will be used to send command responses.
--
-- @command@ — IOUSBHostCIMessage with type IOUSBHostCIMessageTypeEndpointCreate
--
-- Returns: IOUSBHostCIEndpointStateMachine instance, to be released by the caller.
--
-- ObjC selector: @- initWithInterface:command:error:@
initWithInterface_command_error :: (IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine, IsIOUSBHostControllerInterface interface, IsNSError error_) => iousbHostCIEndpointStateMachine -> interface -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO (Id IOUSBHostCIEndpointStateMachine)
initWithInterface_command_error iousbHostCIEndpointStateMachine interface command error_ =
  sendOwnedMessage iousbHostCIEndpointStateMachine initWithInterface_command_errorSelector (toIOUSBHostControllerInterface interface) command (toNSError error_)

-- | Inspect an IOUSBHostCIMessage command
--
-- The IOUSBHostCIMessage command is inspected to determine if it is handled by this state machine and is appropriate for the current state.
--
-- @command@ — IOUSBHostCIMessage command structure received from the kernel driver.
--
-- Returns: BOOL YES if the command is targeting this endpoint, and can be handled in the current state              BOOL NO if the command does not target this endpoint, or cannot be handled in the current state
--
-- ObjC selector: @- inspectCommand:error:@
inspectCommand_error :: (IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine, IsNSError error_) => iousbHostCIEndpointStateMachine -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO Bool
inspectCommand_error iousbHostCIEndpointStateMachine command error_ =
  sendMessage iousbHostCIEndpointStateMachine inspectCommand_errorSelector command (toNSError error_)

-- | Advance the state machine and process an IOUSBHostCIDoorbell message
--
-- The IOUSBHostCIDoorbell is inspected to determine if it is handled by this state machine and is appropriate for the current state.  If successful,              the client should check for an IOUSBHostCIEndpointStateActive endpointState and a currentTransferMessage with IOUSBHostCIMessageControlValid set to determine              if more IOUSBHostCIMessages should be processed.
--
-- @doorbell@ — IOUSBHostCIDoorbell message received from the kernel driver.
--
-- Returns: BOOL YES if the doorbell is targeting this endpoint and can be handled in the current state.              BOOL NO is the doorbell does not target this endpoint or cannot be handled in the current state.
--
-- ObjC selector: @- processDoorbell:error:@
processDoorbell_error :: (IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine, IsNSError error_) => iousbHostCIEndpointStateMachine -> Const CUInt -> error_ -> IO Bool
processDoorbell_error iousbHostCIEndpointStateMachine doorbell error_ =
  sendMessage iousbHostCIEndpointStateMachine processDoorbell_errorSelector doorbell (toNSError error_)

-- | @- deviceAddress@
deviceAddress :: IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine => iousbHostCIEndpointStateMachine -> IO CULong
deviceAddress iousbHostCIEndpointStateMachine =
  sendMessage iousbHostCIEndpointStateMachine deviceAddressSelector

-- | @- endpointAddress@
endpointAddress :: IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine => iousbHostCIEndpointStateMachine -> IO CULong
endpointAddress iousbHostCIEndpointStateMachine =
  sendMessage iousbHostCIEndpointStateMachine endpointAddressSelector

-- | @- currentTransferMessage@
currentTransferMessage :: IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine => iousbHostCIEndpointStateMachine -> IO (Const (Ptr IOUSBHostCIMessage))
currentTransferMessage iousbHostCIEndpointStateMachine =
  sendMessage iousbHostCIEndpointStateMachine currentTransferMessageSelector

-- | @- controllerInterface@
controllerInterface :: IsIOUSBHostCIEndpointStateMachine iousbHostCIEndpointStateMachine => iousbHostCIEndpointStateMachine -> IO (Id IOUSBHostControllerInterface)
controllerInterface iousbHostCIEndpointStateMachine =
  sendMessage iousbHostCIEndpointStateMachine controllerInterfaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id IOUSBHostCIEndpointStateMachine)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterface:command:error:@
initWithInterface_command_errorSelector :: Selector '[Id IOUSBHostControllerInterface, Const (Ptr IOUSBHostCIMessage), Id NSError] (Id IOUSBHostCIEndpointStateMachine)
initWithInterface_command_errorSelector = mkSelector "initWithInterface:command:error:"

-- | @Selector@ for @inspectCommand:error:@
inspectCommand_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), Id NSError] Bool
inspectCommand_errorSelector = mkSelector "inspectCommand:error:"

-- | @Selector@ for @processDoorbell:error:@
processDoorbell_errorSelector :: Selector '[Const CUInt, Id NSError] Bool
processDoorbell_errorSelector = mkSelector "processDoorbell:error:"

-- | @Selector@ for @deviceAddress@
deviceAddressSelector :: Selector '[] CULong
deviceAddressSelector = mkSelector "deviceAddress"

-- | @Selector@ for @endpointAddress@
endpointAddressSelector :: Selector '[] CULong
endpointAddressSelector = mkSelector "endpointAddress"

-- | @Selector@ for @currentTransferMessage@
currentTransferMessageSelector :: Selector '[] (Const (Ptr IOUSBHostCIMessage))
currentTransferMessageSelector = mkSelector "currentTransferMessage"

-- | @Selector@ for @controllerInterface@
controllerInterfaceSelector :: Selector '[] (Id IOUSBHostControllerInterface)
controllerInterfaceSelector = mkSelector "controllerInterface"

