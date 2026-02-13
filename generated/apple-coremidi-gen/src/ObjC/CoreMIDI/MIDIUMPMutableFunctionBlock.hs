{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDIUMPMutableFunctionBlock
--
-- A mutable Function Block object created by the client process.
--
-- A Function Block created with this API may be used in the Function Block configuration				of a client-created MIDIUMPMutableEndpoint.
--
-- This API is not realtime-safe, all interaction with the function block should be done on the				main thread.
--
-- Generated bindings for @MIDIUMPMutableFunctionBlock@.
module ObjC.CoreMIDI.MIDIUMPMutableFunctionBlock
  ( MIDIUMPMutableFunctionBlock
  , IsMIDIUMPMutableFunctionBlock(..)
  , init_
  , initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabled
  , setEnabled_error
  , setName_error
  , reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_error
  , umpEndpoint
  , initSelector
  , initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabledSelector
  , reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_errorSelector
  , setEnabled_errorSelector
  , setName_errorSelector
  , umpEndpointSelector

  -- * Enum types
  , MIDIUMPFunctionBlockDirection(MIDIUMPFunctionBlockDirection)
  , pattern KMIDIUMPFunctionBlockDirectionUnknown
  , pattern KMIDIUMPFunctionBlockDirectionInput
  , pattern KMIDIUMPFunctionBlockDirectionOutput
  , pattern KMIDIUMPFunctionBlockDirectionBidirectional
  , MIDIUMPFunctionBlockMIDI1Info(MIDIUMPFunctionBlockMIDI1Info)
  , pattern KMIDIUMPFunctionBlockMIDI1InfoNotMIDI1
  , pattern KMIDIUMPFunctionBlockMIDI1InfoUnrestrictedBandwidth
  , pattern KMIDIUMPFunctionBlockMIDI1InfoRestrictedBandwidth
  , MIDIUMPFunctionBlockUIHint(MIDIUMPFunctionBlockUIHint)
  , pattern KMIDIUMPFunctionBlockUIHintUnknown
  , pattern KMIDIUMPFunctionBlockUIHintReceiver
  , pattern KMIDIUMPFunctionBlockUIHintSender
  , pattern KMIDIUMPFunctionBlockUIHintSenderReceiver

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

-- | @- init@
init_ :: IsMIDIUMPMutableFunctionBlock midiumpMutableFunctionBlock => midiumpMutableFunctionBlock -> IO (Id MIDIUMPMutableFunctionBlock)
init_ midiumpMutableFunctionBlock =
  sendOwnedMessage midiumpMutableFunctionBlock initSelector

-- | initWithName:direction:firstGroup:totalGroupsSpanned:maxSysEx8Streams:MIDI1Info:UIHint:isEnabled:
--
-- The initializer for constructing a Function Block.
--
-- @name@ — The Function Block name.
--
-- @direction@ — The directionality of the Function Block.
--
-- @firstGroup@ — The first UMP Group supported by the Function Block.
--
-- @totalGroupsSpanned@ — The number of UMP groups spanned by the Function Block.
--
-- @maxSysEx8Streams@ — The maximum number of simultaneous Sysex8 streams.
--
-- @MIDI1Info@ — The MIDI 1.0 speed information for the Function Block.
--
-- @UIHint@ — A UI hint for the Function Block.
--
-- @isEnabled@ — The enable state of the Function Block.
--
-- This operation will fail if virtual MIDI endpoint creation is not allowed				(for example, on iOS, if your app doesn't list 'audio' in UIBackgroundModes).
--
-- ObjC selector: @- initWithName:direction:firstGroup:totalGroupsSpanned:maxSysEx8Streams:MIDI1Info:UIHint:isEnabled:@
initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabled :: (IsMIDIUMPMutableFunctionBlock midiumpMutableFunctionBlock, IsNSString name) => midiumpMutableFunctionBlock -> name -> MIDIUMPFunctionBlockDirection -> CUChar -> CUChar -> CUChar -> MIDIUMPFunctionBlockMIDI1Info -> MIDIUMPFunctionBlockUIHint -> Bool -> IO (Id MIDIUMPMutableFunctionBlock)
initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabled midiumpMutableFunctionBlock name direction firstGroup totalGroupsSpanned maxSysEx8Streams midI1Info uiHint isEnabled =
  sendOwnedMessage midiumpMutableFunctionBlock initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabledSelector (toNSString name) direction firstGroup totalGroupsSpanned maxSysEx8Streams midI1Info uiHint isEnabled

-- | setEnabled:error:
--
-- Set whether this Function Block is enabled or disabled.
--
-- @isEnabled@ — The new state of the Function Block.
--
-- @error@ — The out-error used if an error occurred.
--
-- Returns: YES for success. NO in the event of a failure, in which case the error is returned in error.
--
-- If a Function Block is registered to UMP Endpoint as part of a static configuration,				the state must always be enabled and may not change. If registered to a UMP Endpoint,				changes to the Function Block state are propagated to the system-wide cache.
--
-- ObjC selector: @- setEnabled:error:@
setEnabled_error :: (IsMIDIUMPMutableFunctionBlock midiumpMutableFunctionBlock, IsNSError error_) => midiumpMutableFunctionBlock -> Bool -> error_ -> IO Bool
setEnabled_error midiumpMutableFunctionBlock isEnabled error_ =
  sendMessage midiumpMutableFunctionBlock setEnabled_errorSelector isEnabled (toNSError error_)

-- | setName:error:
--
-- Set the function block name.
--
-- @name@ — A string representing the name of the function block.
--
-- @error@ — The out-error used if an error occurs.
--
-- Returns: YES for success. NO in the event of a failure, in which case the error is returned in error.
--
-- The Function Block name string. Updating the name of a Function Block will cause the				  updated name to be propagated to all local copies of the system-wide cache.
--
-- ObjC selector: @- setName:error:@
setName_error :: (IsMIDIUMPMutableFunctionBlock midiumpMutableFunctionBlock, IsNSString name, IsNSError error_) => midiumpMutableFunctionBlock -> name -> error_ -> IO Bool
setName_error midiumpMutableFunctionBlock name error_ =
  sendMessage midiumpMutableFunctionBlock setName_errorSelector (toNSString name) (toNSError error_)

-- | reconfigureWithFirstGroup:direction:MIDI1Info:UIHint:error
--
-- Reconfigure a Function Block.
--
-- @firstGroup@ — The new first Group to use for the Function Block..
--
-- @direction@ — The direction of the Function Block: input, output, or bidirectional.
--
-- @MIDI1Info@ — MIDI 1.0 speed information.
--
-- @UIHint@ — A hint for UI about the primary usage of this Function Block.
--
-- If a mutable Function Block has not been registered to a CI device or was registered in				a non-static Function Block configuration, the first Group can be changed if the final				Group spanned by the Function Block is valid after the Function Block has been				relocated.				Returns YES if the first Group of the Function Block was changed.
--
-- ObjC selector: @- reconfigureWithFirstGroup:direction:MIDI1Info:UIHint:error:@
reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_error :: (IsMIDIUMPMutableFunctionBlock midiumpMutableFunctionBlock, IsNSError error_) => midiumpMutableFunctionBlock -> CUChar -> MIDIUMPFunctionBlockDirection -> MIDIUMPFunctionBlockMIDI1Info -> MIDIUMPFunctionBlockUIHint -> error_ -> IO Bool
reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_error midiumpMutableFunctionBlock firstGroup direction midI1Info uiHint error_ =
  sendMessage midiumpMutableFunctionBlock reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_errorSelector firstGroup direction midI1Info uiHint (toNSError error_)

-- | UMPEndpoint
--
-- The UMP Endpoint to which this Function Block is registered.
--
-- ObjC selector: @- UMPEndpoint@
umpEndpoint :: IsMIDIUMPMutableFunctionBlock midiumpMutableFunctionBlock => midiumpMutableFunctionBlock -> IO (Id MIDIUMPMutableEndpoint)
umpEndpoint midiumpMutableFunctionBlock =
  sendMessage midiumpMutableFunctionBlock umpEndpointSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDIUMPMutableFunctionBlock)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:direction:firstGroup:totalGroupsSpanned:maxSysEx8Streams:MIDI1Info:UIHint:isEnabled:@
initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabledSelector :: Selector '[Id NSString, MIDIUMPFunctionBlockDirection, CUChar, CUChar, CUChar, MIDIUMPFunctionBlockMIDI1Info, MIDIUMPFunctionBlockUIHint, Bool] (Id MIDIUMPMutableFunctionBlock)
initWithName_direction_firstGroup_totalGroupsSpanned_maxSysEx8Streams_MIDI1Info_UIHint_isEnabledSelector = mkSelector "initWithName:direction:firstGroup:totalGroupsSpanned:maxSysEx8Streams:MIDI1Info:UIHint:isEnabled:"

-- | @Selector@ for @setEnabled:error:@
setEnabled_errorSelector :: Selector '[Bool, Id NSError] Bool
setEnabled_errorSelector = mkSelector "setEnabled:error:"

-- | @Selector@ for @setName:error:@
setName_errorSelector :: Selector '[Id NSString, Id NSError] Bool
setName_errorSelector = mkSelector "setName:error:"

-- | @Selector@ for @reconfigureWithFirstGroup:direction:MIDI1Info:UIHint:error:@
reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_errorSelector :: Selector '[CUChar, MIDIUMPFunctionBlockDirection, MIDIUMPFunctionBlockMIDI1Info, MIDIUMPFunctionBlockUIHint, Id NSError] Bool
reconfigureWithFirstGroup_direction_MIDI1Info_UIHint_errorSelector = mkSelector "reconfigureWithFirstGroup:direction:MIDI1Info:UIHint:error:"

-- | @Selector@ for @UMPEndpoint@
umpEndpointSelector :: Selector '[] (Id MIDIUMPMutableEndpoint)
umpEndpointSelector = mkSelector "UMPEndpoint"

