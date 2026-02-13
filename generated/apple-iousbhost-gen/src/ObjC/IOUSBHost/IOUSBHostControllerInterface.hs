{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IOUSBHostControllerInterface@.
module ObjC.IOUSBHost.IOUSBHostControllerInterface
  ( IOUSBHostControllerInterface
  , IsIOUSBHostControllerInterface(..)
  , init_
  , initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandler
  , destroy
  , enqueueInterrupt_error
  , enqueueInterrupt_expedite_error
  , enqueueInterrupts_count_error
  , enqueueInterrupts_count_expedite_error
  , descriptionForMessage
  , getPortStateMachineForCommand_error
  , getPortStateMachineForPort_error
  , capabilitiesForPort
  , queue
  , interruptRateHz
  , setInterruptRateHz
  , controllerStateMachine
  , capabilities
  , uuid
  , capabilitiesForPortSelector
  , capabilitiesSelector
  , controllerStateMachineSelector
  , descriptionForMessageSelector
  , destroySelector
  , enqueueInterrupt_errorSelector
  , enqueueInterrupt_expedite_errorSelector
  , enqueueInterrupts_count_errorSelector
  , enqueueInterrupts_count_expedite_errorSelector
  , getPortStateMachineForCommand_errorSelector
  , getPortStateMachineForPort_errorSelector
  , initSelector
  , initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandlerSelector
  , interruptRateHzSelector
  , queueSelector
  , setInterruptRateHzSelector
  , uuidSelector


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
init_ :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO (Id IOUSBHostControllerInterface)
init_ iousbHostControllerInterface =
  sendOwnedMessage iousbHostControllerInterface initSelector

-- | Initializes IOUSBHostControllerInterface object along with a user client
--
-- If the user client cannot be created, nil will be returned.              When done using the object, destroy must be called on the object.
--
-- @capabilities@ — NSData containing an array of IOUSBHostCIMessage structures.  The first must have an IOUSBHostCIMessageControlType of IOUSBHostCIMessageTypeControllerCapabilities,              followed by at least one message with an IOUSBHostCIMessageControlType of IOUSBHostCIMessageTypePortCapabilities.
--
-- @queue@ — A serial queue to service asynchronous operations. If nil, a serial queue will be created on behalf of the client.
--
-- @interruptRateHz@ — NSUInteger representing the rate in Hz at which interrupts will be delivered to the kernel driver.              A value ot 0 will send all interrupts to the kernel immediately.
--
-- @commandHandler@ — IOUSBHostControllerInterfaceCommandHandler used to process IOUSBHostCIMessage messages sent by the kernel driver.
--
-- @doorbellHandler@ — IOUSBHostControllerInterfaceDoorbellHandler used to process IOUSBHostCIDoorbell values sent by the kernel driver.
--
-- @interestHandler@ — IOServiceInterestCallback used to process service state changes such as termination. See IOServiceAddInterestNotification              in IOKitLib for more details. All notifications will be serviced on an internal serial queue separate from command and doorbell handlers.
--
-- Returns: An IOUSBHostControllerInterface. The object is to be released by the caller.
--
-- ObjC selector: @- initWithCapabilities:queue:interruptRateHz:error:commandHandler:doorbellHandler:interestHandler:@
initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandler :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSData capabilities, IsNSObject queue, IsNSError error_) => iousbHostControllerInterface -> capabilities -> queue -> CULong -> error_ -> Ptr () -> Ptr () -> Ptr () -> IO (Id IOUSBHostControllerInterface)
initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandler iousbHostControllerInterface capabilities queue interruptRateHz error_ commandHandler doorbellHandler interestHandler =
  sendOwnedMessage iousbHostControllerInterface initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandlerSelector (toNSData capabilities) (toNSObject queue) interruptRateHz (toNSError error_) commandHandler doorbellHandler interestHandler

-- | Removes underlying allocations of the IOUSBHostControllerInterface object along with user client
--
-- When the IOUSBHostControllerInterface is no longer needed, destroy must be called. This will destroy              the connection with the user client and de-register interest on the service. If the object              is freed, destroy will be called automatically. Calling destroy multiple times has no effect.
--
-- ObjC selector: @- destroy@
destroy :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO ()
destroy iousbHostControllerInterface =
  sendMessage iousbHostControllerInterface destroySelector

-- | Enqueue an interrupt for delivery to the kernel service
--
-- This method enqueues one interrupt message for delivery to the kernel service.  interruptRateHz is used to determine when the interrupt message is              delivered to the kernel service.
--
-- @interrupt@ — An IOUSBHostCIMessage structure representing an interrupt message
--
-- ObjC selector: @- enqueueInterrupt:error:@
enqueueInterrupt_error :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSError error_) => iousbHostControllerInterface -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO Bool
enqueueInterrupt_error iousbHostControllerInterface interrupt error_ =
  sendMessage iousbHostControllerInterface enqueueInterrupt_errorSelector interrupt (toNSError error_)

-- | Enqueue an interrupt for delivery to the kernel service
--
-- This method enqueues one interrupt message for delivery to the kernel service.
--
-- @interrupt@ — An IOUSBHostCIMessage structure representing an interrupt message
--
-- @expedite@ — Bool NO to use interruptRateHz to determine when the interrupt message is delivered to the kernel service. Bool YES if interruptRateHz              should be ignored, sending the message to the kernel driver at the next opportunity while maintaining in-order delivery of all interrupt messages.
--
-- ObjC selector: @- enqueueInterrupt:expedite:error:@
enqueueInterrupt_expedite_error :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSError error_) => iousbHostControllerInterface -> Const (Ptr IOUSBHostCIMessage) -> Bool -> error_ -> IO Bool
enqueueInterrupt_expedite_error iousbHostControllerInterface interrupt expedite error_ =
  sendMessage iousbHostControllerInterface enqueueInterrupt_expedite_errorSelector interrupt expedite (toNSError error_)

-- | Enqueue interrupts for delivery to the kernel service
--
-- This method enqueues one or more interrupt messages for delivery to the kernel service.   interruptRateHz is used to determine when the interrupt message is              delivered to the kernel service.
--
-- @interrupts@ — An IOUSBHostCIMessage structure representing one or more interrupt messages
--
-- @count@ — The number of interrupt messages represented by the interrupts parameter
--
-- ObjC selector: @- enqueueInterrupts:count:error:@
enqueueInterrupts_count_error :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSError error_) => iousbHostControllerInterface -> Const (Ptr IOUSBHostCIMessage) -> CULong -> error_ -> IO Bool
enqueueInterrupts_count_error iousbHostControllerInterface interrupts count error_ =
  sendMessage iousbHostControllerInterface enqueueInterrupts_count_errorSelector interrupts count (toNSError error_)

-- | Enqueue interrupts for delivery to the kernel service
--
-- This method enqueues one or more interrupt messages for delivery to the kernel service.   interruptRateHz is used to determine when the interrupt message is              delivered to the kernel service.
--
-- @interrupts@ — An IOUSBHostCIMessage structure representing one or more interrupt messages
--
-- @count@ — The number of interrupt messages represented by the interrupts parameter
--
-- @expedite@ — Bool NO to use interruptRateHz to determine when the interrupt message is delivered to the kernel service. Bool YES if interruptRateHz              should be ignored, sending the message to the kernel driver at the next opportunity while maintaining in-order delivery of all interrupt messages.
--
-- ObjC selector: @- enqueueInterrupts:count:expedite:error:@
enqueueInterrupts_count_expedite_error :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSError error_) => iousbHostControllerInterface -> Const (Ptr IOUSBHostCIMessage) -> CULong -> Bool -> error_ -> IO Bool
enqueueInterrupts_count_expedite_error iousbHostControllerInterface interrupts count expedite error_ =
  sendMessage iousbHostControllerInterface enqueueInterrupts_count_expedite_errorSelector interrupts count expedite (toNSError error_)

-- | @- descriptionForMessage:@
descriptionForMessage :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> Const (Ptr IOUSBHostCIMessage) -> IO (Id NSString)
descriptionForMessage iousbHostControllerInterface message =
  sendMessage iousbHostControllerInterface descriptionForMessageSelector message

-- | @- getPortStateMachineForCommand:error:@
getPortStateMachineForCommand_error :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSError error_) => iousbHostControllerInterface -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO (Id IOUSBHostCIPortStateMachine)
getPortStateMachineForCommand_error iousbHostControllerInterface command error_ =
  sendMessage iousbHostControllerInterface getPortStateMachineForCommand_errorSelector command (toNSError error_)

-- | @- getPortStateMachineForPort:error:@
getPortStateMachineForPort_error :: (IsIOUSBHostControllerInterface iousbHostControllerInterface, IsNSError error_) => iousbHostControllerInterface -> CULong -> error_ -> IO (Id IOUSBHostCIPortStateMachine)
getPortStateMachineForPort_error iousbHostControllerInterface port error_ =
  sendMessage iousbHostControllerInterface getPortStateMachineForPort_errorSelector port (toNSError error_)

-- | Retrieve a port capabilities structure passed in during initialization
--
-- ObjC selector: @- capabilitiesForPort:@
capabilitiesForPort :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> CULong -> IO (Const (Ptr IOUSBHostCIMessage))
capabilitiesForPort iousbHostControllerInterface port =
  sendMessage iousbHostControllerInterface capabilitiesForPortSelector port

-- | The dispatch queue for asynchronous operations.
--
-- ObjC selector: @- queue@
queue :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO (Id NSObject)
queue iousbHostControllerInterface =
  sendMessage iousbHostControllerInterface queueSelector

-- | The interrupt moderation rate for sending interrupt messages to the kernel driver
--
-- interruptRateHz will cause submitted interrupt messages to be batched together and submitted to the kernel              at the specified rate.  A value ot 0 will deliver all interrupts to the kernel driver as soon as possible.
--
-- ObjC selector: @- interruptRateHz@
interruptRateHz :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO CULong
interruptRateHz iousbHostControllerInterface =
  sendMessage iousbHostControllerInterface interruptRateHzSelector

-- | The interrupt moderation rate for sending interrupt messages to the kernel driver
--
-- interruptRateHz will cause submitted interrupt messages to be batched together and submitted to the kernel              at the specified rate.  A value ot 0 will deliver all interrupts to the kernel driver as soon as possible.
--
-- ObjC selector: @- setInterruptRateHz:@
setInterruptRateHz :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> CULong -> IO ()
setInterruptRateHz iousbHostControllerInterface value =
  sendMessage iousbHostControllerInterface setInterruptRateHzSelector value

-- | @- controllerStateMachine@
controllerStateMachine :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO (Id IOUSBHostCIControllerStateMachine)
controllerStateMachine iousbHostControllerInterface =
  sendMessage iousbHostControllerInterface controllerStateMachineSelector

-- | The capabilities structure passed in during initialization
--
-- The capabilities passed into the initializer can be retrieved for reference.
--
-- ObjC selector: @- capabilities@
capabilities :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO (Const (Ptr IOUSBHostCIMessage))
capabilities iousbHostControllerInterface =
  sendMessage iousbHostControllerInterface capabilitiesSelector

-- | A UUID used to identify the host controller interface in this process and the kernel
--
-- ObjC selector: @- uuid@
uuid :: IsIOUSBHostControllerInterface iousbHostControllerInterface => iousbHostControllerInterface -> IO (Id NSUUID)
uuid iousbHostControllerInterface =
  sendMessage iousbHostControllerInterface uuidSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id IOUSBHostControllerInterface)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapabilities:queue:interruptRateHz:error:commandHandler:doorbellHandler:interestHandler:@
initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandlerSelector :: Selector '[Id NSData, Id NSObject, CULong, Id NSError, Ptr (), Ptr (), Ptr ()] (Id IOUSBHostControllerInterface)
initWithCapabilities_queue_interruptRateHz_error_commandHandler_doorbellHandler_interestHandlerSelector = mkSelector "initWithCapabilities:queue:interruptRateHz:error:commandHandler:doorbellHandler:interestHandler:"

-- | @Selector@ for @destroy@
destroySelector :: Selector '[] ()
destroySelector = mkSelector "destroy"

-- | @Selector@ for @enqueueInterrupt:error:@
enqueueInterrupt_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), Id NSError] Bool
enqueueInterrupt_errorSelector = mkSelector "enqueueInterrupt:error:"

-- | @Selector@ for @enqueueInterrupt:expedite:error:@
enqueueInterrupt_expedite_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), Bool, Id NSError] Bool
enqueueInterrupt_expedite_errorSelector = mkSelector "enqueueInterrupt:expedite:error:"

-- | @Selector@ for @enqueueInterrupts:count:error:@
enqueueInterrupts_count_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), CULong, Id NSError] Bool
enqueueInterrupts_count_errorSelector = mkSelector "enqueueInterrupts:count:error:"

-- | @Selector@ for @enqueueInterrupts:count:expedite:error:@
enqueueInterrupts_count_expedite_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), CULong, Bool, Id NSError] Bool
enqueueInterrupts_count_expedite_errorSelector = mkSelector "enqueueInterrupts:count:expedite:error:"

-- | @Selector@ for @descriptionForMessage:@
descriptionForMessageSelector :: Selector '[Const (Ptr IOUSBHostCIMessage)] (Id NSString)
descriptionForMessageSelector = mkSelector "descriptionForMessage:"

-- | @Selector@ for @getPortStateMachineForCommand:error:@
getPortStateMachineForCommand_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), Id NSError] (Id IOUSBHostCIPortStateMachine)
getPortStateMachineForCommand_errorSelector = mkSelector "getPortStateMachineForCommand:error:"

-- | @Selector@ for @getPortStateMachineForPort:error:@
getPortStateMachineForPort_errorSelector :: Selector '[CULong, Id NSError] (Id IOUSBHostCIPortStateMachine)
getPortStateMachineForPort_errorSelector = mkSelector "getPortStateMachineForPort:error:"

-- | @Selector@ for @capabilitiesForPort:@
capabilitiesForPortSelector :: Selector '[CULong] (Const (Ptr IOUSBHostCIMessage))
capabilitiesForPortSelector = mkSelector "capabilitiesForPort:"

-- | @Selector@ for @queue@
queueSelector :: Selector '[] (Id NSObject)
queueSelector = mkSelector "queue"

-- | @Selector@ for @interruptRateHz@
interruptRateHzSelector :: Selector '[] CULong
interruptRateHzSelector = mkSelector "interruptRateHz"

-- | @Selector@ for @setInterruptRateHz:@
setInterruptRateHzSelector :: Selector '[CULong] ()
setInterruptRateHzSelector = mkSelector "setInterruptRateHz:"

-- | @Selector@ for @controllerStateMachine@
controllerStateMachineSelector :: Selector '[] (Id IOUSBHostCIControllerStateMachine)
controllerStateMachineSelector = mkSelector "controllerStateMachine"

-- | @Selector@ for @capabilities@
capabilitiesSelector :: Selector '[] (Const (Ptr IOUSBHostCIMessage))
capabilitiesSelector = mkSelector "capabilities"

-- | @Selector@ for @uuid@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "uuid"

