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
  , initWithDevice_delegateSelector
  , createIndicator_min_max_currentValueSelector
  , processATCommandSelector
  , sendOKResponseSelector
  , sendResponseSelector
  , sendResponse_withOKSelector


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
initWithDevice_delegate ioBluetoothHandsFreeAudioGateway  device inDelegate =
withObjCPtr device $ \raw_device ->
    sendMsg ioBluetoothHandsFreeAudioGateway (mkSelector "initWithDevice:delegate:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr (unRawId inDelegate) :: Ptr ())] >>= ownedObject . castPtr

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
createIndicator_min_max_currentValue ioBluetoothHandsFreeAudioGateway  indicatorName minValue maxValue currentValue =
withObjCPtr indicatorName $ \raw_indicatorName ->
    sendMsg ioBluetoothHandsFreeAudioGateway (mkSelector "createIndicator:min:max:currentValue:") retVoid [argPtr (castPtr raw_indicatorName :: Ptr ()), argCInt (fromIntegral minValue), argCInt (fromIntegral maxValue), argCInt (fromIntegral currentValue)]

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
processATCommand ioBluetoothHandsFreeAudioGateway  atCommand =
withObjCPtr atCommand $ \raw_atCommand ->
    sendMsg ioBluetoothHandsFreeAudioGateway (mkSelector "processATCommand:") retVoid [argPtr (castPtr raw_atCommand :: Ptr ())]

-- | sendOKResponse
--
-- Sends an OK response
--
-- Use this to respond OK.
--
-- ObjC selector: @- sendOKResponse@
sendOKResponse :: IsIOBluetoothHandsFreeAudioGateway ioBluetoothHandsFreeAudioGateway => ioBluetoothHandsFreeAudioGateway -> IO ()
sendOKResponse ioBluetoothHandsFreeAudioGateway  =
  sendMsg ioBluetoothHandsFreeAudioGateway (mkSelector "sendOKResponse") retVoid []

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
sendResponse ioBluetoothHandsFreeAudioGateway  response =
withObjCPtr response $ \raw_response ->
    sendMsg ioBluetoothHandsFreeAudioGateway (mkSelector "sendResponse:") retVoid [argPtr (castPtr raw_response :: Ptr ())]

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
sendResponse_withOK ioBluetoothHandsFreeAudioGateway  response withOK =
withObjCPtr response $ \raw_response ->
    sendMsg ioBluetoothHandsFreeAudioGateway (mkSelector "sendResponse:withOK:") retVoid [argPtr (castPtr raw_response :: Ptr ()), argCULong (if withOK then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:delegate:@
initWithDevice_delegateSelector :: Selector
initWithDevice_delegateSelector = mkSelector "initWithDevice:delegate:"

-- | @Selector@ for @createIndicator:min:max:currentValue:@
createIndicator_min_max_currentValueSelector :: Selector
createIndicator_min_max_currentValueSelector = mkSelector "createIndicator:min:max:currentValue:"

-- | @Selector@ for @processATCommand:@
processATCommandSelector :: Selector
processATCommandSelector = mkSelector "processATCommand:"

-- | @Selector@ for @sendOKResponse@
sendOKResponseSelector :: Selector
sendOKResponseSelector = mkSelector "sendOKResponse"

-- | @Selector@ for @sendResponse:@
sendResponseSelector :: Selector
sendResponseSelector = mkSelector "sendResponse:"

-- | @Selector@ for @sendResponse:withOK:@
sendResponse_withOKSelector :: Selector
sendResponse_withOKSelector = mkSelector "sendResponse:withOK:"

