{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDIUMPMutableEndpoint
--
-- A mutable MIDIUMPEndpoint object.
--
-- It is not necessary to create a MIDIUMPEndpoint or other MIDI endpoint in order to				use UMP natively. Any standard MIDI endpoint created with a specified MIDIProtocolID				is assumed to use all 16 UMP groups for the same unspecified function and to neither				transmit nor receive jitter-reduction timestamps.
--
-- This API is not realtime-safe, all interaction with the mutable endpoint should be done on the				main thread.
--
-- Generated bindings for @MIDIUMPMutableEndpoint@.
module ObjC.CoreMIDI.MIDIUMPMutableEndpoint
  ( MIDIUMPMutableEndpoint
  , IsMIDIUMPMutableEndpoint(..)
  , initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallback
  , setName_error
  , registerFunctionBlocks_markAsStatic_error
  , setEnabled_error
  , mutableFunctionBlocks
  , setMutableFunctionBlocks
  , isEnabled
  , initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallbackSelector
  , isEnabledSelector
  , mutableFunctionBlocksSelector
  , registerFunctionBlocks_markAsStatic_errorSelector
  , setEnabled_errorSelector
  , setMutableFunctionBlocksSelector
  , setName_errorSelector

  -- * Enum types
  , MIDIProtocolID(MIDIProtocolID)
  , pattern KMIDIProtocol_1_0
  , pattern KMIDIProtocol_2_0

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.CoreMIDI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithName:deviceInfo:productInstanceID:MIDIProtocol:destinationCallback
--
-- Initializer for creating a new MIDIUMPEndpoint.
--
-- @name@ — The UMP endpoint name.
--
-- @deviceInfo@ — The MIDI 2 device ID info for the UMP endpoint.
--
-- @productInstanceID@ — The product instance ID, up to 42 characters.
--
-- @MIDIProtocol@ — The MIDI protocol.
--
-- @destinationCallback@ — The receive callback used to create the UMP endpoint's MIDI									destination associated, which can be used to observe or process									incoming MIDI traffic.
--
-- This operation will fail if the device ID information is malformed or if virtual MIDI endpoint creation				is not allowed (for example, on iOS, if your app doesn't list 'audio' in UIBackgroundModes).
--
-- ObjC selector: @- initWithName:deviceInfo:productInstanceID:MIDIProtocol:destinationCallback:@
initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallback :: (IsMIDIUMPMutableEndpoint midiumpMutableEndpoint, IsNSString name, IsMIDI2DeviceInfo deviceInfo, IsNSString productInstanceID) => midiumpMutableEndpoint -> name -> deviceInfo -> productInstanceID -> MIDIProtocolID -> Ptr () -> IO (Id MIDIUMPMutableEndpoint)
initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallback midiumpMutableEndpoint name deviceInfo productInstanceID midiProtocol destinationCallback =
  sendOwnedMessage midiumpMutableEndpoint initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallbackSelector (toNSString name) (toMIDI2DeviceInfo deviceInfo) (toNSString productInstanceID) midiProtocol destinationCallback

-- | setName:error:
--
-- Set the endpoints name.
--
-- @name@ — A string representing the name of the endpoint.
--
-- @error@ — The out-error used if an error occurs.
--
-- Returns: YES for success. NO in the event of a failure, in which case the error is returned in error.
--
-- This operation will fail if the name could not be set.
--
-- ObjC selector: @- setName:error:@
setName_error :: (IsMIDIUMPMutableEndpoint midiumpMutableEndpoint, IsNSString name, IsNSError error_) => midiumpMutableEndpoint -> name -> error_ -> IO Bool
setName_error midiumpMutableEndpoint name error_ =
  sendMessage midiumpMutableEndpoint setName_errorSelector (toNSString name) (toNSError error_)

-- | registerFunctionBlocks:markAsStatic:error:
--
-- Register or replace Function Blocks for a disabled client-created MIDIUMPEndpoint.
--
-- @functionBlocks@ — A list of client-created Function Blocks to register.
--
-- @markAsStatic@ — Whether the Function Block configuration may be updated.
--
-- @error@ — The out-error used if an error occurs.
--
-- Returns: YES for success. NO in the event of a failure, in which case the error is returned in error.
--
-- This operation will fail if the array contains any disabled Function Blocks but the				MIDIUMPEndpoint Function Block configuration is static.				Returns YES if the Function Block configuration was set successfully.
--
-- ObjC selector: @- registerFunctionBlocks:markAsStatic:error:@
registerFunctionBlocks_markAsStatic_error :: (IsMIDIUMPMutableEndpoint midiumpMutableEndpoint, IsNSArray functionBlocks, IsNSError error_) => midiumpMutableEndpoint -> functionBlocks -> Bool -> error_ -> IO Bool
registerFunctionBlocks_markAsStatic_error midiumpMutableEndpoint functionBlocks markAsStatic error_ =
  sendMessage midiumpMutableEndpoint registerFunctionBlocks_markAsStatic_errorSelector (toNSArray functionBlocks) markAsStatic (toNSError error_)

-- | setEnabled:error:
--
-- Enable a mutable UMP endpoint in the system-wide UMP endpoint cache.
--
-- @isEnabled@ — The enable state of the UMP endpoint.
--
-- @error@ — The out-error used if an error occurred.
--
-- Returns: YES for success. NO in the event of a failure, in which case the error is returned in error.
--
-- A MIDIUMPMutableEndpoint must be cache enabled before it is visible via API.				Note that Function Blocks may only be registered to uncached MIDIUMPMutableEndpoint				objects.
--
-- ObjC selector: @- setEnabled:error:@
setEnabled_error :: (IsMIDIUMPMutableEndpoint midiumpMutableEndpoint, IsNSError error_) => midiumpMutableEndpoint -> Bool -> error_ -> IO Bool
setEnabled_error midiumpMutableEndpoint isEnabled error_ =
  sendMessage midiumpMutableEndpoint setEnabled_errorSelector isEnabled (toNSError error_)

-- | mutableFunctionBlocks
--
-- The Function Blocks associated with the UMP endpoint, if any.
--
-- ObjC selector: @- mutableFunctionBlocks@
mutableFunctionBlocks :: IsMIDIUMPMutableEndpoint midiumpMutableEndpoint => midiumpMutableEndpoint -> IO (Id NSArray)
mutableFunctionBlocks midiumpMutableEndpoint =
  sendMessage midiumpMutableEndpoint mutableFunctionBlocksSelector

-- | mutableFunctionBlocks
--
-- The Function Blocks associated with the UMP endpoint, if any.
--
-- ObjC selector: @- setMutableFunctionBlocks:@
setMutableFunctionBlocks :: (IsMIDIUMPMutableEndpoint midiumpMutableEndpoint, IsNSArray value) => midiumpMutableEndpoint -> value -> IO ()
setMutableFunctionBlocks midiumpMutableEndpoint value =
  sendMessage midiumpMutableEndpoint setMutableFunctionBlocksSelector (toNSArray value)

-- | isEnabled
--
-- The enable state of the endpoint.
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsMIDIUMPMutableEndpoint midiumpMutableEndpoint => midiumpMutableEndpoint -> IO Bool
isEnabled midiumpMutableEndpoint =
  sendMessage midiumpMutableEndpoint isEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:deviceInfo:productInstanceID:MIDIProtocol:destinationCallback:@
initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallbackSelector :: Selector '[Id NSString, Id MIDI2DeviceInfo, Id NSString, MIDIProtocolID, Ptr ()] (Id MIDIUMPMutableEndpoint)
initWithName_deviceInfo_productInstanceID_MIDIProtocol_destinationCallbackSelector = mkSelector "initWithName:deviceInfo:productInstanceID:MIDIProtocol:destinationCallback:"

-- | @Selector@ for @setName:error:@
setName_errorSelector :: Selector '[Id NSString, Id NSError] Bool
setName_errorSelector = mkSelector "setName:error:"

-- | @Selector@ for @registerFunctionBlocks:markAsStatic:error:@
registerFunctionBlocks_markAsStatic_errorSelector :: Selector '[Id NSArray, Bool, Id NSError] Bool
registerFunctionBlocks_markAsStatic_errorSelector = mkSelector "registerFunctionBlocks:markAsStatic:error:"

-- | @Selector@ for @setEnabled:error:@
setEnabled_errorSelector :: Selector '[Bool, Id NSError] Bool
setEnabled_errorSelector = mkSelector "setEnabled:error:"

-- | @Selector@ for @mutableFunctionBlocks@
mutableFunctionBlocksSelector :: Selector '[] (Id NSArray)
mutableFunctionBlocksSelector = mkSelector "mutableFunctionBlocks"

-- | @Selector@ for @setMutableFunctionBlocks:@
setMutableFunctionBlocksSelector :: Selector '[Id NSArray] ()
setMutableFunctionBlocksSelector = mkSelector "setMutableFunctionBlocks:"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

