{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostCIPortStateMachine
--
-- The object representing the state of a user-mode USB host controller root port
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller root port, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.
--
-- IOUSBHostCIPortStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
--
-- Generated bindings for @IOUSBHostCIPortStateMachine@.
module ObjC.IOUSBHost.IOUSBHostCIPortStateMachine
  ( IOUSBHostCIPortStateMachine
  , IsIOUSBHostCIPortStateMachine(..)
  , init_
  , initWithInterface_portNumber_error
  , inspectCommand_error
  , portNumber
  , portStatus
  , controllerInterface
  , powered
  , setPowered
  , connected
  , setConnected
  , overcurrent
  , setOvercurrent
  , connectedSelector
  , controllerInterfaceSelector
  , initSelector
  , initWithInterface_portNumber_errorSelector
  , inspectCommand_errorSelector
  , overcurrentSelector
  , portNumberSelector
  , portStatusSelector
  , poweredSelector
  , setConnectedSelector
  , setOvercurrentSelector
  , setPoweredSelector


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
init_ :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO (Id IOUSBHostCIPortStateMachine)
init_ iousbHostCIPortStateMachine =
  sendOwnedMessage iousbHostCIPortStateMachine initSelector

-- | Initializes an IOUSBHostCIPortStateMachine object
--
-- The IOUSBHostCIPortStateMachine defaults to the IOUSBHostCIPortStateOff state.
--
-- @interface@ — IOUSBHostControllerInterface which will be used to send command responses.
--
-- @portNumber@ — NSUInteger for the root port number tracked by this instance
--
-- Returns: IOUSBHostCIPortStateMachine instance, to be released by the caller.
--
-- ObjC selector: @- initWithInterface:portNumber:error:@
initWithInterface_portNumber_error :: (IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine, IsIOUSBHostControllerInterface interface, IsNSError error_) => iousbHostCIPortStateMachine -> interface -> CULong -> error_ -> IO (Id IOUSBHostCIPortStateMachine)
initWithInterface_portNumber_error iousbHostCIPortStateMachine interface portNumber error_ =
  sendOwnedMessage iousbHostCIPortStateMachine initWithInterface_portNumber_errorSelector (toIOUSBHostControllerInterface interface) portNumber (toNSError error_)

-- | Inspect an IOUSBHostCIMessage command
--
-- The IOUSBHostCIMessage command is inspected to determine if it is handled by the state machine, and              is appropriate for the current state.
--
-- @command@ — IOUSBHostCIMessage command structure received from the kernel driver.
--
-- Returns: BOOL YES if the command is targeting a controller, and can be handled in the current state              BOOL NO if the command does not target a controller, or cannot be handled in the current state
--
-- ObjC selector: @- inspectCommand:error:@
inspectCommand_error :: (IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine, IsNSError error_) => iousbHostCIPortStateMachine -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO Bool
inspectCommand_error iousbHostCIPortStateMachine command error_ =
  sendMessage iousbHostCIPortStateMachine inspectCommand_errorSelector command (toNSError error_)

-- | @- portNumber@
portNumber :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO CULong
portNumber iousbHostCIPortStateMachine =
  sendMessage iousbHostCIPortStateMachine portNumberSelector

-- | @- portStatus@
portStatus :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO CUInt
portStatus iousbHostCIPortStateMachine =
  sendMessage iousbHostCIPortStateMachine portStatusSelector

-- | @- controllerInterface@
controllerInterface :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO (Id IOUSBHostControllerInterface)
controllerInterface iousbHostCIPortStateMachine =
  sendMessage iousbHostCIPortStateMachine controllerInterfaceSelector

-- | Set the powered state of the port
--
-- Before a port can be used, it must be powered on via a IOUSBHostCIMessageTypePortPowerOn command.  As part of successfully processing this command              the powered property must be set to YES.  Similarly, successful processing of the IOUSBHostCIMessageTypePortPowerOff command must set the powered              property to NO.
--
-- ObjC selector: @- powered@
powered :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO Bool
powered iousbHostCIPortStateMachine =
  sendMessage iousbHostCIPortStateMachine poweredSelector

-- | Set the powered state of the port
--
-- Before a port can be used, it must be powered on via a IOUSBHostCIMessageTypePortPowerOn command.  As part of successfully processing this command              the powered property must be set to YES.  Similarly, successful processing of the IOUSBHostCIMessageTypePortPowerOff command must set the powered              property to NO.
--
-- ObjC selector: @- setPowered:@
setPowered :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> Bool -> IO ()
setPowered iousbHostCIPortStateMachine value =
  sendMessage iousbHostCIPortStateMachine setPoweredSelector value

-- | Set the connection state of the port
--
-- The connected property cannot be set for an unpowered port, and will read back as NO, just at IOUSBHostCIPortStatusConnected in the port status will always              read as 0.  For a powered port, writing to the connected property will set IOUSBHostCIPortStatusConnected to match the provided value, and if the              new value is different from the previous value an IOUSBHostCIMessageTypePortEvent message will be sent to the kernel with IOUSBHostCIPortStatusConnectChange              set.
--
-- ObjC selector: @- connected@
connected :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO Bool
connected iousbHostCIPortStateMachine =
  sendMessage iousbHostCIPortStateMachine connectedSelector

-- | Set the connection state of the port
--
-- The connected property cannot be set for an unpowered port, and will read back as NO, just at IOUSBHostCIPortStatusConnected in the port status will always              read as 0.  For a powered port, writing to the connected property will set IOUSBHostCIPortStatusConnected to match the provided value, and if the              new value is different from the previous value an IOUSBHostCIMessageTypePortEvent message will be sent to the kernel with IOUSBHostCIPortStatusConnectChange              set.
--
-- ObjC selector: @- setConnected:@
setConnected :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> Bool -> IO ()
setConnected iousbHostCIPortStateMachine value =
  sendMessage iousbHostCIPortStateMachine setConnectedSelector value

-- | Set the overcurrent state of the port
--
-- The overcurrent property cannot be set for an unpowered port, and will read back as NO, just as IOUSBHostCIPortStatusOvercurrent in the port status will always              read as 0.  For a powered port, writing to the overcurrent property will set IOUSBHostCIPortStatusOvercurrent to match the provided value, and if the              new value is different from the previous value an IOUSBHostCIMessageTypePortEvent message will be sent to the kernel with IOUSBHostCIPortStatusOvercurrentChange              set.
--
-- ObjC selector: @- overcurrent@
overcurrent :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> IO Bool
overcurrent iousbHostCIPortStateMachine =
  sendMessage iousbHostCIPortStateMachine overcurrentSelector

-- | Set the overcurrent state of the port
--
-- The overcurrent property cannot be set for an unpowered port, and will read back as NO, just as IOUSBHostCIPortStatusOvercurrent in the port status will always              read as 0.  For a powered port, writing to the overcurrent property will set IOUSBHostCIPortStatusOvercurrent to match the provided value, and if the              new value is different from the previous value an IOUSBHostCIMessageTypePortEvent message will be sent to the kernel with IOUSBHostCIPortStatusOvercurrentChange              set.
--
-- ObjC selector: @- setOvercurrent:@
setOvercurrent :: IsIOUSBHostCIPortStateMachine iousbHostCIPortStateMachine => iousbHostCIPortStateMachine -> Bool -> IO ()
setOvercurrent iousbHostCIPortStateMachine value =
  sendMessage iousbHostCIPortStateMachine setOvercurrentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id IOUSBHostCIPortStateMachine)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterface:portNumber:error:@
initWithInterface_portNumber_errorSelector :: Selector '[Id IOUSBHostControllerInterface, CULong, Id NSError] (Id IOUSBHostCIPortStateMachine)
initWithInterface_portNumber_errorSelector = mkSelector "initWithInterface:portNumber:error:"

-- | @Selector@ for @inspectCommand:error:@
inspectCommand_errorSelector :: Selector '[Const (Ptr IOUSBHostCIMessage), Id NSError] Bool
inspectCommand_errorSelector = mkSelector "inspectCommand:error:"

-- | @Selector@ for @portNumber@
portNumberSelector :: Selector '[] CULong
portNumberSelector = mkSelector "portNumber"

-- | @Selector@ for @portStatus@
portStatusSelector :: Selector '[] CUInt
portStatusSelector = mkSelector "portStatus"

-- | @Selector@ for @controllerInterface@
controllerInterfaceSelector :: Selector '[] (Id IOUSBHostControllerInterface)
controllerInterfaceSelector = mkSelector "controllerInterface"

-- | @Selector@ for @powered@
poweredSelector :: Selector '[] Bool
poweredSelector = mkSelector "powered"

-- | @Selector@ for @setPowered:@
setPoweredSelector :: Selector '[Bool] ()
setPoweredSelector = mkSelector "setPowered:"

-- | @Selector@ for @connected@
connectedSelector :: Selector '[] Bool
connectedSelector = mkSelector "connected"

-- | @Selector@ for @setConnected:@
setConnectedSelector :: Selector '[Bool] ()
setConnectedSelector = mkSelector "setConnected:"

-- | @Selector@ for @overcurrent@
overcurrentSelector :: Selector '[] Bool
overcurrentSelector = mkSelector "overcurrent"

-- | @Selector@ for @setOvercurrent:@
setOvercurrentSelector :: Selector '[Bool] ()
setOvercurrentSelector = mkSelector "setOvercurrent:"

