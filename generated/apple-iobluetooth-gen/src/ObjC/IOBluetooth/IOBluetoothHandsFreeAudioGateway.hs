{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | APIs for managing a hands free audio gateway
--
-- (c) 2010 by Apple Inc. All rights reserved.
--
-- Generated bindings for @IOBluetoothHandsFreeAudioGateway@.
module ObjC.IOBluetooth.IOBluetoothHandsFreeAudioGateway
  ( IOBluetoothHandsFreeAudioGateway
  , IsIOBluetoothHandsFreeAudioGateway(..)
  , initWithDevice_delegate
  , createIndicator_min_max_currentValue
  , processATCommand
  , sendOKResponse
  , sendResponse
  , sendResponse_withOK
  , createIndicator_min_max_currentValueSelector
  , initWithDevice_delegateSelector
  , processATCommandSelector
  , sendOKResponseSelector
  , sendResponseSelector
  , sendResponse_withOKSelector


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
-- Create a new IOBluetoothHandsFreeAudioGateway to act as a hands free gateway
--
-- This will register a listener for incoming connections.
--
-- @device@ — An IOBluetoothDevice
--
-- @inDelegate@ — An object to act as delegate
--
-- Returns: A newly created IOBluetoothHandsFreeAudioGateway object on success, nil on failure
--
-- ObjC selector: @- initWithDevice:delegate:@
initWithDevice_delegate :: (IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway, IsIOBluetoothDevice device) => ioBluetoothHandsFreeAudioGateway -> device -> RawId -> IO (Id IOBluetoothHandsFreeAudioGateway)
initWithDevice_delegate ioBluetoothHandsFreeAudioGateway device inDelegate =
  sendOwnedMessage ioBluetoothHandsFreeAudioGateway initWithDevice_delegateSelector (toIOBluetoothDevice device) inDelegate

-- | createIndicator:indicatorName:min:max:currentValue
--
-- Create an indicator
--
-- Creates an indicator with min and max values and sets the current value. The current value must be valid.
--
-- @indicatorName@ — See  �Hands free indicator constants," for standard indicator names.
--
-- @minValue@ — Minimum value allowed for the indicator
--
-- @maxValue@ — Maximum value allowed for the indicator
--
-- @currentValue@ — The current indicator value. Must be within the min and max values passed in or the indicator will not be created.
--
-- ObjC selector: @- createIndicator:min:max:currentValue:@
createIndicator_min_max_currentValue :: (IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway, IsNSString indicatorName) => ioBluetoothHandsFreeAudioGateway -> indicatorName -> CInt -> CInt -> CInt -> IO ()
createIndicator_min_max_currentValue ioBluetoothHandsFreeAudioGateway indicatorName minValue maxValue currentValue =
  sendMessage ioBluetoothHandsFreeAudioGateway createIndicator_min_max_currentValueSelector (toNSString indicatorName) minValue maxValue currentValue

-- | processATCommand:atCommand
--
-- Handles AT commands sent from the hands free device
--
-- Implement this in a subclass if you wish to respond to additional AT commands or to change the default response.
--
-- @atCommand@ — The at command from the hands free device
--
-- ObjC selector: @- processATCommand:@
processATCommand :: (IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway, IsNSString atCommand) => ioBluetoothHandsFreeAudioGateway -> atCommand -> IO ()
processATCommand ioBluetoothHandsFreeAudioGateway atCommand =
  sendMessage ioBluetoothHandsFreeAudioGateway processATCommandSelector (toNSString atCommand)

-- | sendOKResponse
--
-- Sends an OK response
--
-- Use this to respond OK.
--
-- ObjC selector: @- sendOKResponse@
sendOKResponse :: IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway => ioBluetoothHandsFreeAudioGateway -> IO ()
sendOKResponse ioBluetoothHandsFreeAudioGateway =
  sendMessage ioBluetoothHandsFreeAudioGateway sendOKResponseSelector

-- | sendResponse:response
--
-- Sends a response to the hands free device
--
-- Use this to send a response followed by an OK. Equivalent to [sendResponse:response withOK:YES].
--
-- @response@ — The response to send to the hands free device
--
-- ObjC selector: @- sendResponse:@
sendResponse :: (IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway, IsNSString response) => ioBluetoothHandsFreeAudioGateway -> response -> IO ()
sendResponse ioBluetoothHandsFreeAudioGateway response =
  sendMessage ioBluetoothHandsFreeAudioGateway sendResponseSelector (toNSString response)

-- | sendResponse:response:withOK
--
-- Sends a response to the hands free device
--
-- Use this to send a response and optionally followed by an OK.
--
-- @response@ — The response to send to the hands free device
--
-- @withOK@ — If yes, an OK response will also be sent.
--
-- ObjC selector: @- sendResponse:withOK:@
sendResponse_withOK :: (IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway, IsNSString response) => ioBluetoothHandsFreeAudioGateway -> response -> Bool -> IO ()
sendResponse_withOK ioBluetoothHandsFreeAudioGateway response withOK =
  sendMessage ioBluetoothHandsFreeAudioGateway sendResponse_withOKSelector (toNSString response) withOK

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:delegate:@
initWithDevice_delegateSelector :: Selector '[Id IOBluetoothDevice, RawId] (Id IOBluetoothHandsFreeAudioGateway)
initWithDevice_delegateSelector = mkSelector "initWithDevice:delegate:"

-- | @Selector@ for @createIndicator:min:max:currentValue:@
createIndicator_min_max_currentValueSelector :: Selector '[Id NSString, CInt, CInt, CInt] ()
createIndicator_min_max_currentValueSelector = mkSelector "createIndicator:min:max:currentValue:"

-- | @Selector@ for @processATCommand:@
processATCommandSelector :: Selector '[Id NSString] ()
processATCommandSelector = mkSelector "processATCommand:"

-- | @Selector@ for @sendOKResponse@
sendOKResponseSelector :: Selector '[] ()
sendOKResponseSelector = mkSelector "sendOKResponse"

-- | @Selector@ for @sendResponse:@
sendResponseSelector :: Selector '[Id NSString] ()
sendResponseSelector = mkSelector "sendResponse:"

-- | @Selector@ for @sendResponse:withOK:@
sendResponse_withOKSelector :: Selector '[Id NSString, Bool] ()
sendResponse_withOKSelector = mkSelector "sendResponse:withOK:"

