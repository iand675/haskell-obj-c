{-# LANGUAGE DataKinds #-}
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
  , acceptCallOnPhoneSelector
  , acceptCallSelector
  , addHeldCallSelector
  , callTransferSelector
  , currentCallListSelector
  , dialNumberSelector
  , endCallSelector
  , holdCallSelector
  , initWithDevice_delegateSelector
  , memoryDialSelector
  , placeAllOthersOnHoldSelector
  , redialSelector
  , releaseActiveCallsSelector
  , releaseCallSelector
  , releaseHeldCallsSelector
  , sendATCommandSelector
  , sendATCommand_timeout_selector_targetSelector
  , sendDTMFSelector
  , sendSMS_messageSelector
  , subscriberNumberSelector
  , transferAudioToComputerSelector
  , transferAudioToPhoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDevice_delegate ioBluetoothHandsFreeDevice device delegate =
  sendOwnedMessage ioBluetoothHandsFreeDevice initWithDevice_delegateSelector (toIOBluetoothDevice device) delegate

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
dialNumber ioBluetoothHandsFreeDevice aNumber =
  sendMessage ioBluetoothHandsFreeDevice dialNumberSelector (toNSString aNumber)

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
memoryDial ioBluetoothHandsFreeDevice memoryLocation =
  sendMessage ioBluetoothHandsFreeDevice memoryDialSelector memoryLocation

-- | redial
--
-- Redial a number
--
-- Redials the previous number stored by the hands free gateway.
--
-- ObjC selector: @- redial@
redial :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
redial ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice redialSelector

-- | endCall
--
-- Hang up a call or reject an incoming call
--
-- Hangs up the current call, or rejects an incoming call.
--
-- ObjC selector: @- endCall@
endCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
endCall ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice endCallSelector

-- | acceptCall
--
-- Accept an incoming call
--
-- Accepts an incoming call.
--
-- ObjC selector: @- acceptCall@
acceptCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
acceptCall ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice acceptCallSelector

-- | acceptCallOnPhone
--
-- Accept an incoming call on the phone
--
-- Accepts an incoming call and then quickly transfer audio to the phone.
--
-- ObjC selector: @- acceptCallOnPhone@
acceptCallOnPhone :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
acceptCallOnPhone ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice acceptCallOnPhoneSelector

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
sendDTMF ioBluetoothHandsFreeDevice character =
  sendMessage ioBluetoothHandsFreeDevice sendDTMFSelector (toNSString character)

-- | subscriberNumber
--
-- Get the subscriber number(s)
--
-- Gets the subscriber number(s) stored on the gateway. Each subscriber number is returned on the delegate method handsFree:subscriberNumber.				There is no guarantee that the gateway will have a subscriber number.
--
-- ObjC selector: @- subscriberNumber@
subscriberNumber :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
subscriberNumber ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice subscriberNumberSelector

-- | currentCallList
--
-- Get the current call list
--
-- Gets the current call list (active, held, and setup in process). Each call is returned on the delegate method handsFree:currentCall.
--
-- ObjC selector: @- currentCallList@
currentCallList :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
currentCallList ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice currentCallListSelector

-- | releaseHeldCalls
--
-- Release all held calls
--
-- Releases all held calls or sets User Determined User Busy for a waiting call.
--
-- ObjC selector: @- releaseHeldCalls@
releaseHeldCalls :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
releaseHeldCalls ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice releaseHeldCallsSelector

-- | releaseActiveCalls
--
-- Release all active calls
--
-- Releases all active calls (if any exist) and accepts the other (held or waiting) call.
--
-- ObjC selector: @- releaseActiveCalls@
releaseActiveCalls :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
releaseActiveCalls ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice releaseActiveCallsSelector

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
releaseCall ioBluetoothHandsFreeDevice index =
  sendMessage ioBluetoothHandsFreeDevice releaseCallSelector index

-- | holdCall
--
-- Place all active calls on hold
--
-- Places all active calls (if any exist) on hold and accepts the other (held or waiting) call.
--
-- ObjC selector: @- holdCall@
holdCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
holdCall ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice holdCallSelector

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
placeAllOthersOnHold ioBluetoothHandsFreeDevice index =
  sendMessage ioBluetoothHandsFreeDevice placeAllOthersOnHoldSelector index

-- | addHeldCall
--
-- Add a held call to the current conversation
--
-- Adds a held call to the current conversation.
--
-- ObjC selector: @- addHeldCall@
addHeldCall :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
addHeldCall ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice addHeldCallSelector

-- | callTransfer
--
-- Release all active calls
--
-- Releases all active calls (if any exist) and accepts the other (held or waiting) call.
--
-- ObjC selector: @- callTransfer@
callTransfer :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
callTransfer ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice callTransferSelector

-- | transferAudioToComputer
--
-- Transfer audio source to the computer.
--
-- Transfers audio to the computer.
--
-- ObjC selector: @- transferAudioToComputer@
transferAudioToComputer :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
transferAudioToComputer ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice transferAudioToComputerSelector

-- | transferAudioToPhone
--
-- Transfer audio source to the phone.
--
-- Transfers audio to the phone.
--
-- ObjC selector: @- transferAudioToPhone@
transferAudioToPhone :: IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice => ioBluetoothHandsFreeDevice -> IO ()
transferAudioToPhone ioBluetoothHandsFreeDevice =
  sendMessage ioBluetoothHandsFreeDevice transferAudioToPhoneSelector

-- | sendSMS:aNumber:aMessage
--
-- Send an SMS to a number.
--
-- Sends an SMS to aNumber with content aMessage. Currently this does not handle long SMS (>160 characters) or unicode messages.
--
-- ObjC selector: @- sendSMS:message:@
sendSMS_message :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString aNumber, IsNSString aMessage) => ioBluetoothHandsFreeDevice -> aNumber -> aMessage -> IO ()
sendSMS_message ioBluetoothHandsFreeDevice aNumber aMessage =
  sendMessage ioBluetoothHandsFreeDevice sendSMS_messageSelector (toNSString aNumber) (toNSString aMessage)

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
sendATCommand ioBluetoothHandsFreeDevice atCommand =
  sendMessage ioBluetoothHandsFreeDevice sendATCommandSelector (toNSString atCommand)

-- | sendATCommand:atCommand:timeout:selector:target
--
-- Send an AT command to the hands free gateway.
--
-- Sends an AT command to the hands free gateway with timeout. 				On command complete (OK, ERROR, TIMEOUT response or after timeout seconds) will perform selector on target.
--
-- @atCommand@ — AT command to send
--
-- ObjC selector: @- sendATCommand:timeout:selector:target:@
sendATCommand_timeout_selector_target :: (IsIOBluetoothHandsFreeDevice ioBluetoothHandsFreeDevice, IsNSString atCommand) => ioBluetoothHandsFreeDevice -> atCommand -> CFloat -> Sel -> RawId -> IO ()
sendATCommand_timeout_selector_target ioBluetoothHandsFreeDevice atCommand timeout selector target =
  sendMessage ioBluetoothHandsFreeDevice sendATCommand_timeout_selector_targetSelector (toNSString atCommand) timeout selector target

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:delegate:@
initWithDevice_delegateSelector :: Selector '[Id IOBluetoothDevice, RawId] (Id IOBluetoothHandsFreeDevice)
initWithDevice_delegateSelector = mkSelector "initWithDevice:delegate:"

-- | @Selector@ for @dialNumber:@
dialNumberSelector :: Selector '[Id NSString] ()
dialNumberSelector = mkSelector "dialNumber:"

-- | @Selector@ for @memoryDial:@
memoryDialSelector :: Selector '[CInt] ()
memoryDialSelector = mkSelector "memoryDial:"

-- | @Selector@ for @redial@
redialSelector :: Selector '[] ()
redialSelector = mkSelector "redial"

-- | @Selector@ for @endCall@
endCallSelector :: Selector '[] ()
endCallSelector = mkSelector "endCall"

-- | @Selector@ for @acceptCall@
acceptCallSelector :: Selector '[] ()
acceptCallSelector = mkSelector "acceptCall"

-- | @Selector@ for @acceptCallOnPhone@
acceptCallOnPhoneSelector :: Selector '[] ()
acceptCallOnPhoneSelector = mkSelector "acceptCallOnPhone"

-- | @Selector@ for @sendDTMF:@
sendDTMFSelector :: Selector '[Id NSString] ()
sendDTMFSelector = mkSelector "sendDTMF:"

-- | @Selector@ for @subscriberNumber@
subscriberNumberSelector :: Selector '[] ()
subscriberNumberSelector = mkSelector "subscriberNumber"

-- | @Selector@ for @currentCallList@
currentCallListSelector :: Selector '[] ()
currentCallListSelector = mkSelector "currentCallList"

-- | @Selector@ for @releaseHeldCalls@
releaseHeldCallsSelector :: Selector '[] ()
releaseHeldCallsSelector = mkSelector "releaseHeldCalls"

-- | @Selector@ for @releaseActiveCalls@
releaseActiveCallsSelector :: Selector '[] ()
releaseActiveCallsSelector = mkSelector "releaseActiveCalls"

-- | @Selector@ for @releaseCall:@
releaseCallSelector :: Selector '[CInt] ()
releaseCallSelector = mkSelector "releaseCall:"

-- | @Selector@ for @holdCall@
holdCallSelector :: Selector '[] ()
holdCallSelector = mkSelector "holdCall"

-- | @Selector@ for @placeAllOthersOnHold:@
placeAllOthersOnHoldSelector :: Selector '[CInt] ()
placeAllOthersOnHoldSelector = mkSelector "placeAllOthersOnHold:"

-- | @Selector@ for @addHeldCall@
addHeldCallSelector :: Selector '[] ()
addHeldCallSelector = mkSelector "addHeldCall"

-- | @Selector@ for @callTransfer@
callTransferSelector :: Selector '[] ()
callTransferSelector = mkSelector "callTransfer"

-- | @Selector@ for @transferAudioToComputer@
transferAudioToComputerSelector :: Selector '[] ()
transferAudioToComputerSelector = mkSelector "transferAudioToComputer"

-- | @Selector@ for @transferAudioToPhone@
transferAudioToPhoneSelector :: Selector '[] ()
transferAudioToPhoneSelector = mkSelector "transferAudioToPhone"

-- | @Selector@ for @sendSMS:message:@
sendSMS_messageSelector :: Selector '[Id NSString, Id NSString] ()
sendSMS_messageSelector = mkSelector "sendSMS:message:"

-- | @Selector@ for @sendATCommand:@
sendATCommandSelector :: Selector '[Id NSString] ()
sendATCommandSelector = mkSelector "sendATCommand:"

-- | @Selector@ for @sendATCommand:timeout:selector:target:@
sendATCommand_timeout_selector_targetSelector :: Selector '[Id NSString, CFloat, Sel, RawId] ()
sendATCommand_timeout_selector_targetSelector = mkSelector "sendATCommand:timeout:selector:target:"

