{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | APIs for managing a hands free device
--
-- (c) 2010 by Apple Inc. All rights reserved.
--
-- Generated bindings for @IOBluetoothHandsFreeDevice@.
module ObjC.IOBluetooth.IOBluetoothHandsFreeDevice
  ( IOBluetoothHandsFreeDevice
  , IsIOBluetoothHandsFreeDevice(..)
  , initWithDevice_delegate
  , dialNumber
  , memoryDial
  , redial
  , endCall
  , acceptCall
  , acceptCallOnPhone
  , sendDTMF
  , subscriberNumber
  , currentCallList
  , releaseHeldCalls
  , releaseActiveCalls
  , releaseCall
  , holdCall
  , placeAllOthersOnHold
  , addHeldCall
  , callTransfer
  , transferAudioToComputer
  , transferAudioToPhone
  , sendSMS_message
  , sendATCommand
  , sendATCommand_timeout_selector_target
  , initWithDevice_delegateSelector
  , dialNumberSelector
  , memoryDialSelector
  , redialSelector
  , endCallSelector
  , acceptCallSelector
  , acceptCallOnPhoneSelector
  , sendDTMFSelector
  , subscriberNumberSelector
  , currentCallListSelector
  , releaseHeldCallsSelector
  , releaseActiveCallsSelector
  , releaseCallSelector
  , holdCallSelector
  , placeAllOthersOnHoldSelector
  , addHeldCallSelector
  , callTransferSelector
  , transferAudioToComputerSelector
  , transferAudioToPhoneSelector
  , sendSMS_messageSelector
  , sendATCommandSelector
  , sendATCommand_timeout_selector_targetSelector


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

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithDevice:delegate:
--
-- Create a new IOBluetoothHandsFreeDevice to act as a hands free device
--
-- This will register a listener for incoming connections.
--
-- @device@ — An IOBluetoothDevice
--
-- @inDelegate@ — An object to act as delegate
--
-- Returns: A newly created IOBluetoothHandsFreeDevice object on success, nil on failure
--
-- ObjC selector: @- initWithDevice:delegate:@
initWithDevice_delegate :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsIOBluetoothDevice device) => ioBluetoothHandsFreeDevice -> device -> RawId -> IO (Id IOBluetoothHandsFreeDevice)
initWithDevice_delegate ioBluetoothHandsFreeDevice  device delegate =
withObjCPtr device $ \raw_device ->
    sendMsg ioBluetoothHandsFreeDevice (mkSelector "initWithDevice:delegate:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | dialNumber:aNumber
--
-- Dial a number
--
-- Calls aNumber
--
-- @aNumber@ — The number to call - the gateway determines acceptable formats.
--
-- ObjC selector: @- dialNumber:@
dialNumber :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString aNumber) => ioBluetoothHandsFreeDevice -> aNumber -> IO ()
dialNumber ioBluetoothHandsFreeDevice  aNumber =
withObjCPtr aNumber $ \raw_aNumber ->
    sendMsg ioBluetoothHandsFreeDevice (mkSelector "dialNumber:") retVoid [argPtr (castPtr raw_aNumber :: Ptr ())]

-- | memoryDial:memoryLocation
--
-- Dial a number from memory
--
-- Calls a number from a memory (or speed dial) location.
--
-- @memoryLocation@ — The location in memory
--
-- ObjC selector: @- memoryDial:@
memoryDial :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> CInt -> IO ()
memoryDial ioBluetoothHandsFreeDevice  memoryLocation =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "memoryDial:") retVoid [argCInt (fromIntegral memoryLocation)]

-- | redial
--
-- Redial a number
--
-- Redials the previous number stored by the hands free gateway.
--
-- ObjC selector: @- redial@
redial :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
redial ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "redial") retVoid []

-- | endCall
--
-- Hang up a call or reject an incoming call
--
-- Hangs up the current call, or rejects an incoming call.
--
-- ObjC selector: @- endCall@
endCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
endCall ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "endCall") retVoid []

-- | acceptCall
--
-- Accept an incoming call
--
-- Accepts an incoming call.
--
-- ObjC selector: @- acceptCall@
acceptCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
acceptCall ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "acceptCall") retVoid []

-- | acceptCallOnPhone
--
-- Accept an incoming call on the phone
--
-- Accepts an incoming call and then quickly transfer audio to the phone.
--
-- ObjC selector: @- acceptCallOnPhone@
acceptCallOnPhone :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
acceptCallOnPhone ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "acceptCallOnPhone") retVoid []

-- | sendDTMF:character
--
-- Send a key press
--
-- Sends a DTMF tone.
--
-- @character@ — A single character in the set 0-9, #,*,A-D
--
-- ObjC selector: @- sendDTMF:@
sendDTMF :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString character) => ioBluetoothHandsFreeDevice -> character -> IO ()
sendDTMF ioBluetoothHandsFreeDevice  character =
withObjCPtr character $ \raw_character ->
    sendMsg ioBluetoothHandsFreeDevice (mkSelector "sendDTMF:") retVoid [argPtr (castPtr raw_character :: Ptr ())]

-- | subscriberNumber
--
-- Get the subscriber number(s)
--
-- Gets the subscriber number(s) stored on the gateway. Each subscriber number is returned on the delegate method handsFree:subscriberNumber.				There is no guarantee that the gateway will have a subscriber number.
--
-- ObjC selector: @- subscriberNumber@
subscriberNumber :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
subscriberNumber ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "subscriberNumber") retVoid []

-- | currentCallList
--
-- Get the current call list
--
-- Gets the current call list (active, held, and setup in process). Each call is returned on the delegate method handsFree:currentCall.
--
-- ObjC selector: @- currentCallList@
currentCallList :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
currentCallList ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "currentCallList") retVoid []

-- | releaseHeldCalls
--
-- Release all held calls
--
-- Releases all held calls or sets User Determined User Busy for a waiting call.
--
-- ObjC selector: @- releaseHeldCalls@
releaseHeldCalls :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
releaseHeldCalls ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "releaseHeldCalls") retVoid []

-- | releaseActiveCalls
--
-- Release all active calls
--
-- Releases all active calls (if any exist) and accepts the other (held or waiting) call.
--
-- ObjC selector: @- releaseActiveCalls@
releaseActiveCalls :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
releaseActiveCalls ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "releaseActiveCalls") retVoid []

-- | releaseCall:index
--
-- Release an active call.
--
-- Releases the active call with index.
--
-- @index@ — Index of the call to release
--
-- ObjC selector: @- releaseCall:@
releaseCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> CInt -> IO ()
releaseCall ioBluetoothHandsFreeDevice  index =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "releaseCall:") retVoid [argCInt (fromIntegral index)]

-- | holdCall
--
-- Place all active calls on hold
--
-- Places all active calls (if any exist) on hold and accepts the other (held or waiting) call.
--
-- ObjC selector: @- holdCall@
holdCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
holdCall ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "holdCall") retVoid []

-- | placeAllOthersOnHold:index
--
-- Place all other calls on hold.
--
-- Places all calls on hold except call with index.
--
-- @index@ — Index of the call to not place on hold
--
-- ObjC selector: @- placeAllOthersOnHold:@
placeAllOthersOnHold :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> CInt -> IO ()
placeAllOthersOnHold ioBluetoothHandsFreeDevice  index =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "placeAllOthersOnHold:") retVoid [argCInt (fromIntegral index)]

-- | addHeldCall
--
-- Add a held call to the current conversation
--
-- Adds a held call to the current conversation.
--
-- ObjC selector: @- addHeldCall@
addHeldCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
addHeldCall ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "addHeldCall") retVoid []

-- | callTransfer
--
-- Release all active calls
--
-- Releases all active calls (if any exist) and accepts the other (held or waiting) call.
--
-- ObjC selector: @- callTransfer@
callTransfer :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
callTransfer ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "callTransfer") retVoid []

-- | transferAudioToComputer
--
-- Transfer audio source to the computer.
--
-- Transfers audio to the computer.
--
-- ObjC selector: @- transferAudioToComputer@
transferAudioToComputer :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
transferAudioToComputer ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "transferAudioToComputer") retVoid []

-- | transferAudioToPhone
--
-- Transfer audio source to the phone.
--
-- Transfers audio to the phone.
--
-- ObjC selector: @- transferAudioToPhone@
transferAudioToPhone :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
transferAudioToPhone ioBluetoothHandsFreeDevice  =
  sendMsg ioBluetoothHandsFreeDevice (mkSelector "transferAudioToPhone") retVoid []

-- | sendSMS:aNumber:aMessage
--
-- Send an SMS to a number.
--
-- Sends an SMS to aNumber with content aMessage. Currently this does not handle long SMS (>160 characters) or unicode messages.
--
-- ObjC selector: @- sendSMS:message:@
sendSMS_message :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString aNumber, IsNSString aMessage) => ioBluetoothHandsFreeDevice -> aNumber -> aMessage -> IO ()
sendSMS_message ioBluetoothHandsFreeDevice  aNumber aMessage =
withObjCPtr aNumber $ \raw_aNumber ->
  withObjCPtr aMessage $ \raw_aMessage ->
      sendMsg ioBluetoothHandsFreeDevice (mkSelector "sendSMS:message:") retVoid [argPtr (castPtr raw_aNumber :: Ptr ()), argPtr (castPtr raw_aMessage :: Ptr ())]

-- | sendATCommand:atCommand
--
-- Send an AT command to the hands free gateway.
--
-- Sends an AT command to the hands free gateway with a timeout of 10 seconds and handled by the built-in response handling.				See sendAtCommand:timeout:selector:target for more details.
--
-- @atCommand@ — AT command to send
--
-- ObjC selector: @- sendATCommand:@
sendATCommand :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString atCommand) => ioBluetoothHandsFreeDevice -> atCommand -> IO ()
sendATCommand ioBluetoothHandsFreeDevice  atCommand =
withObjCPtr atCommand $ \raw_atCommand ->
    sendMsg ioBluetoothHandsFreeDevice (mkSelector "sendATCommand:") retVoid [argPtr (castPtr raw_atCommand :: Ptr ())]

-- | sendATCommand:atCommand:timeout:selector:target
--
-- Send an AT command to the hands free gateway.
--
-- Sends an AT command to the hands free gateway with timeout. 				On command complete (OK, ERROR, TIMEOUT response or after timeout seconds) will perform selector on target.
--
-- @atCommand@ — AT command to send
--
-- ObjC selector: @- sendATCommand:timeout:selector:target:@
sendATCommand_timeout_selector_target :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString atCommand) => ioBluetoothHandsFreeDevice -> atCommand -> CFloat -> Selector -> RawId -> IO ()
sendATCommand_timeout_selector_target ioBluetoothHandsFreeDevice  atCommand timeout selector target =
withObjCPtr atCommand $ \raw_atCommand ->
    sendMsg ioBluetoothHandsFreeDevice (mkSelector "sendATCommand:timeout:selector:target:") retVoid [argPtr (castPtr raw_atCommand :: Ptr ()), argCFloat (fromIntegral timeout), argPtr (unSelector selector), argPtr (castPtr (unRawId target) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:delegate:@
initWithDevice_delegateSelector :: Selector
initWithDevice_delegateSelector = mkSelector "initWithDevice:delegate:"

-- | @Selector@ for @dialNumber:@
dialNumberSelector :: Selector
dialNumberSelector = mkSelector "dialNumber:"

-- | @Selector@ for @memoryDial:@
memoryDialSelector :: Selector
memoryDialSelector = mkSelector "memoryDial:"

-- | @Selector@ for @redial@
redialSelector :: Selector
redialSelector = mkSelector "redial"

-- | @Selector@ for @endCall@
endCallSelector :: Selector
endCallSelector = mkSelector "endCall"

-- | @Selector@ for @acceptCall@
acceptCallSelector :: Selector
acceptCallSelector = mkSelector "acceptCall"

-- | @Selector@ for @acceptCallOnPhone@
acceptCallOnPhoneSelector :: Selector
acceptCallOnPhoneSelector = mkSelector "acceptCallOnPhone"

-- | @Selector@ for @sendDTMF:@
sendDTMFSelector :: Selector
sendDTMFSelector = mkSelector "sendDTMF:"

-- | @Selector@ for @subscriberNumber@
subscriberNumberSelector :: Selector
subscriberNumberSelector = mkSelector "subscriberNumber"

-- | @Selector@ for @currentCallList@
currentCallListSelector :: Selector
currentCallListSelector = mkSelector "currentCallList"

-- | @Selector@ for @releaseHeldCalls@
releaseHeldCallsSelector :: Selector
releaseHeldCallsSelector = mkSelector "releaseHeldCalls"

-- | @Selector@ for @releaseActiveCalls@
releaseActiveCallsSelector :: Selector
releaseActiveCallsSelector = mkSelector "releaseActiveCalls"

-- | @Selector@ for @releaseCall:@
releaseCallSelector :: Selector
releaseCallSelector = mkSelector "releaseCall:"

-- | @Selector@ for @holdCall@
holdCallSelector :: Selector
holdCallSelector = mkSelector "holdCall"

-- | @Selector@ for @placeAllOthersOnHold:@
placeAllOthersOnHoldSelector :: Selector
placeAllOthersOnHoldSelector = mkSelector "placeAllOthersOnHold:"

-- | @Selector@ for @addHeldCall@
addHeldCallSelector :: Selector
addHeldCallSelector = mkSelector "addHeldCall"

-- | @Selector@ for @callTransfer@
callTransferSelector :: Selector
callTransferSelector = mkSelector "callTransfer"

-- | @Selector@ for @transferAudioToComputer@
transferAudioToComputerSelector :: Selector
transferAudioToComputerSelector = mkSelector "transferAudioToComputer"

-- | @Selector@ for @transferAudioToPhone@
transferAudioToPhoneSelector :: Selector
transferAudioToPhoneSelector = mkSelector "transferAudioToPhone"

-- | @Selector@ for @sendSMS:message:@
sendSMS_messageSelector :: Selector
sendSMS_messageSelector = mkSelector "sendSMS:message:"

-- | @Selector@ for @sendATCommand:@
sendATCommandSelector :: Selector
sendATCommandSelector = mkSelector "sendATCommand:"

-- | @Selector@ for @sendATCommand:timeout:selector:target:@
sendATCommand_timeout_selector_targetSelector :: Selector
sendATCommand_timeout_selector_targetSelector = mkSelector "sendATCommand:timeout:selector:target:"

