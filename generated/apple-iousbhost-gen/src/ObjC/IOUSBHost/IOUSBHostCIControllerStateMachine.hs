{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostCIControllerStateMachine
--
-- The object representing the state of a user-mode USB host controller
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.
--
-- IOUSBHostCIControllerStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
--
-- Generated bindings for @IOUSBHostCIControllerStateMachine@.
module ObjC.IOUSBHost.IOUSBHostCIControllerStateMachine
  ( IOUSBHostCIControllerStateMachine
  , IsIOUSBHostCIControllerStateMachine(..)
  , init_
  , initWithInterface_error
  , inspectCommand_error
  , enqueueUpdatedFrame_timestamp_error
  , controllerInterface
  , controllerInterfaceSelector
  , enqueueUpdatedFrame_timestamp_errorSelector
  , initSelector
  , initWithInterface_errorSelector
  , inspectCommand_errorSelector


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
init_ :: IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine => iousbHostCIControllerStateMachine -> IO (Id IOUSBHostCIControllerStateMachine)
init_ iousbHostCIControllerStateMachine =
  sendOwnedMessage iousbHostCIControllerStateMachine initSelector

-- | Initializes an IOUSBHostCIControllerStateMachine object
--
-- The IOUSBHostCIControllerStateMachine defaults to the IOUSBHostCIControllerStateOff state.
--
-- @interface@ — IOUSBHostControllerInterface which will be used to send command responses.
--
-- Returns: IOUSBHostCIControllerStateMachine instance, to be released by the caller.
--
-- ObjC selector: @- initWithInterface:error:@
initWithInterface_error :: (IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine, IsIOUSBHostControllerInterface interface, IsNSError error_) => iousbHostCIControllerStateMachine -> interface -> error_ -> IO (Id IOUSBHostCIControllerStateMachine)
initWithInterface_error iousbHostCIControllerStateMachine interface error_ =
  sendOwnedMessage iousbHostCIControllerStateMachine initWithInterface_errorSelector (toIOUSBHostControllerInterface interface) (toNSError error_)

-- | Inspect an IOUSBHostCIMessage command
--
-- The IOUSBHostCIMessage command is inspected to determine if it is handled by the state machine, and              is appropriate for the current state.
--
-- @command@ — IOUSBHostCIMessage command structure received from the kernel driver.
--
-- Returns: BOOL YES if the command is targeting a controller, and can be handled in the current state              BOOL NO if the command does not target a controller, or cannot be handled in the current state
--
-- ObjC selector: @- inspectCommand:error:@
inspectCommand_error :: (IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine, IsNSError error_) => iousbHostCIControllerStateMachine -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO Bool
inspectCommand_error iousbHostCIControllerStateMachine command error_ =
  sendMessage iousbHostCIControllerStateMachine inspectCommand_errorSelector command (toNSError error_)

-- | Enqueue frame and timestamp messages for delivery to the kernel driver
--
-- If the controller interface is in the IOUSBHostCIControllerStateActive state, messages with the type IOUSBHostCIMessageTypeFrameNumberUpdate and              IOUSBHostCIMessageTypeFrameTimestampUpdate will be generated using the provided inputs, and enqueued for delivery to the kernel driver.              The frame and timestamp information provided effectively measure the duration of the controller's 1ms frame in terms of system time.  A 1% frame duration              variation is permitted.  A larger frame duration variation will result in a IOUSBHostCIExceptionTypeFrameUpdateError.
--
-- @frame@ — uint64_t containing the number of 1ms frames that have elapsed since the controller began counting frames
--
-- @timestamp@ — uint64_t containing the mach_absolute_time() correlated to the beginning of the frameNumber
--
-- Returns: BOOL YES if the messages were enqueued for delivery to the kernel.              BOOL NO if the messages were not enqueued for delivery to the kernel.
--
-- ObjC selector: @- enqueueUpdatedFrame:timestamp:error:@
enqueueUpdatedFrame_timestamp_error :: (IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine, IsNSError error_) => iousbHostCIControllerStateMachine -> CULong -> CULong -> error_ -> IO Bool
enqueueUpdatedFrame_timestamp_error iousbHostCIControllerStateMachine frame timestamp error_ =
  sendMessage iousbHostCIControllerStateMachine enqueueUpdatedFrame_timestamp_errorSelector frame timestamp (toNSError error_)

-- | @- controllerInterface@
controllerInterface :: IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine => iousbHostCIControllerStateMachine -> IO (Id IOUSBHostControllerInterface)
controllerInterface iousbHostCIControllerStateMachine =
  sendMessage iousbHostCIControllerStateMachine controllerInterfaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id IOUSBHostCIControllerStateMachine)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterface:error:@
initWithInterface_errorSelector :: Selector '[Id IOUSBHostControllerInterface, Id NSError] (Id IOUSBHostCIControllerStateMachine)
initWithInterface_errorSelector = mkSelector "initWithInterface:error:"

-- | @Selector@ for @inspectCommand:error:@
inspectCommand_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), Id NSError] Bool
inspectCommand_errorSelector = mkSelector "inspectCommand:error:"

-- | @Selector@ for @enqueueUpdatedFrame:timestamp:error:@
enqueueUpdatedFrame_timestamp_errorSelector :: Selector '[CULong, CULong, Id NSError] Bool
enqueueUpdatedFrame_timestamp_errorSelector = mkSelector "enqueueUpdatedFrame:timestamp:error:"

-- | @Selector@ for @controllerInterface@
controllerInterfaceSelector :: Selector '[] (Id IOUSBHostControllerInterface)
controllerInterfaceSelector = mkSelector "controllerInterface"

