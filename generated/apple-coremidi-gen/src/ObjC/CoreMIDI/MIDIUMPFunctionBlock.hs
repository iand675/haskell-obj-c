{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDIUMPFunctionBlock
--
-- An object representing a Function Block.
--
-- A Function Block encapsulates one or more UMP groups with a single function, allowing				agents communicating with that UMP Endpoint to route and process UMP traffic				properly. Unless the owning MIDIUMPEndpoint has a static Function Block configuration,				any Function Block metadata may change in response to a configuration change in the				owning UMP endpoint.
--
-- Generated bindings for @MIDIUMPFunctionBlock@.
module ObjC.CoreMIDI.MIDIUMPFunctionBlock
  ( MIDIUMPFunctionBlock
  , IsMIDIUMPFunctionBlock(..)
  , init_
  , name
  , functionBlockID
  , direction
  , firstGroup
  , totalGroupsSpanned
  , maxSysEx8Streams
  , midI1Info
  , uiHint
  , umpEndpoint
  , midiCIDevice
  , isEnabled
  , directionSelector
  , firstGroupSelector
  , functionBlockIDSelector
  , initSelector
  , isEnabledSelector
  , maxSysEx8StreamsSelector
  , midI1InfoSelector
  , midiCIDeviceSelector
  , nameSelector
  , totalGroupsSpannedSelector
  , uiHintSelector
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
init_ :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO (Id MIDIUMPFunctionBlock)
init_ midiumpFunctionBlock =
  sendOwnedMessage midiumpFunctionBlock initSelector

-- | name
--
-- A string containing the Function Block's name.
--
-- ObjC selector: @- name@
name :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO (Id NSString)
name midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock nameSelector

-- | functionBlockID
--
-- The device-unique ID for this Function Block.
--
-- ObjC selector: @- functionBlockID@
functionBlockID :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO CUChar
functionBlockID midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock functionBlockIDSelector

-- | direction
--
-- The direction of the Function Block: input, output, or bidirectional.
--
-- ObjC selector: @- direction@
direction :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO MIDIUMPFunctionBlockDirection
direction midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock directionSelector

-- | firstGroup
--
-- The first Group spanned by this Function Block.
--
-- ObjC selector: @- firstGroup@
firstGroup :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO CUChar
firstGroup midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock firstGroupSelector

-- | totalGroupsSpanned
--
-- The total number of groups spanned by this Function Block.
--
-- ObjC selector: @- totalGroupsSpanned@
totalGroupsSpanned :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO CUChar
totalGroupsSpanned midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock totalGroupsSpannedSelector

-- | maxSysEx8Streams
--
-- The maximum number of simultaneous Sysex8 streams.
--
-- ObjC selector: @- maxSysEx8Streams@
maxSysEx8Streams :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO CUChar
maxSysEx8Streams midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock maxSysEx8StreamsSelector

-- | MIDI1Info
--
-- MIDI 1.0 speed information.
--
-- ObjC selector: @- MIDI1Info@
midI1Info :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO MIDIUMPFunctionBlockMIDI1Info
midI1Info midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock midI1InfoSelector

-- | UIHint
--
-- A hint for UI about the primary usage of this Function Block.
--
-- ObjC selector: @- UIHint@
uiHint :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO MIDIUMPFunctionBlockUIHint
uiHint midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock uiHintSelector

-- | UMPEndpoint
--
-- The UMP Endpoint to which this Function Block is registered.
--
-- If the function block does not belong to an endpoint this property will be nil.
--
-- ObjC selector: @- UMPEndpoint@
umpEndpoint :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO (Id MIDIUMPEndpoint)
umpEndpoint midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock umpEndpointSelector

-- | MIDICIDevice
--
-- Retrieve the MIDICIDevice class interface if the Function Block supports MIDI-CI.
--
-- If a Function Block was created as a MIDICIDevice object, this property will               provide an in-place MIDICIDevice interface that may be used with MIDI-CI API. If the               device was not created as a MIDICIDevice, this method returns nil.
--
-- ObjC selector: @- midiCIDevice@
midiCIDevice :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO (Id MIDICIDevice)
midiCIDevice midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock midiCIDeviceSelector

-- | isEnabled
--
-- The enable state of this Function Block.
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsMIDIUMPFunctionBlock midiumpFunctionBlock => midiumpFunctionBlock -> IO Bool
isEnabled midiumpFunctionBlock =
  sendMessage midiumpFunctionBlock isEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDIUMPFunctionBlock)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @functionBlockID@
functionBlockIDSelector :: Selector '[] CUChar
functionBlockIDSelector = mkSelector "functionBlockID"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] MIDIUMPFunctionBlockDirection
directionSelector = mkSelector "direction"

-- | @Selector@ for @firstGroup@
firstGroupSelector :: Selector '[] CUChar
firstGroupSelector = mkSelector "firstGroup"

-- | @Selector@ for @totalGroupsSpanned@
totalGroupsSpannedSelector :: Selector '[] CUChar
totalGroupsSpannedSelector = mkSelector "totalGroupsSpanned"

-- | @Selector@ for @maxSysEx8Streams@
maxSysEx8StreamsSelector :: Selector '[] CUChar
maxSysEx8StreamsSelector = mkSelector "maxSysEx8Streams"

-- | @Selector@ for @MIDI1Info@
midI1InfoSelector :: Selector '[] MIDIUMPFunctionBlockMIDI1Info
midI1InfoSelector = mkSelector "MIDI1Info"

-- | @Selector@ for @UIHint@
uiHintSelector :: Selector '[] MIDIUMPFunctionBlockUIHint
uiHintSelector = mkSelector "UIHint"

-- | @Selector@ for @UMPEndpoint@
umpEndpointSelector :: Selector '[] (Id MIDIUMPEndpoint)
umpEndpointSelector = mkSelector "UMPEndpoint"

-- | @Selector@ for @midiCIDevice@
midiCIDeviceSelector :: Selector '[] (Id MIDICIDevice)
midiCIDeviceSelector = mkSelector "midiCIDevice"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

