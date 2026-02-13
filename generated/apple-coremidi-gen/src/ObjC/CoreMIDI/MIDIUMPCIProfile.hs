{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDIUMPCIProfile
--
-- An object representing Capability Inquiry Profile on a MIDICIDevice.
--
-- MIDI-CI profiles describe a mapping of MIDI messages to specific sounds and synthesis				behaviors, e.g. General MIDI, a drawbar organ, etc. A MIDI-CI Profile may be a standard				registered Profile or vendor-specific.
--
-- A MIDI-CI Profile ID consists of 5 bytes.				===============================================================								Standard Profile			Vendor-Specific Profile				Profile ID Byte 1:	0x7E Standard Profile		Manufacturer SysEx ID 1 Profile				Profile ID Byte 2:	Profile Bank			Manufacturer SysEx ID 2 Profile				Profile ID Byte 3:	Profile Number			Manufacturer SysEx ID 3 Profile				Profile ID Byte 4:	Profile Version			Manufacturer-specific Info				Profile ID Byte 5:	Profile Level			Manufacturer-specific Info
--
-- MIDI-CI Profiles have the following generalized types:
--
-- Profile Type		Source		Channels		Channel Range				============		==========	===========	========================				Single-channel 		0x00~0x0F 	1			1 channel (1 to 16) of a Group				Group			0x7E			16			All channels of a Group				Function Block		0x7F			16 per Group 	All channels of a Function Block				Multi-channel		0x00~0x0F	2 or more		Profile-specific
--
-- MIDIUMPCIProfile objects may only be registered to a single CI device, and any number				of MIDIUMPCIProfile objects containg the same profile ID may be registered to CI				devices in the MIDI 2.0 subsystem.
--
-- Generated bindings for @MIDIUMPCIProfile@.
module ObjC.CoreMIDI.MIDIUMPCIProfile
  ( MIDIUMPCIProfile
  , IsMIDIUMPCIProfile(..)
  , init_
  , setProfileState_enabledChannelCount_error
  , name
  , profileType
  , groupOffset
  , firstChannel
  , enabledChannelCount
  , totalChannelCount
  , isEnabled
  , enabledChannelCountSelector
  , firstChannelSelector
  , groupOffsetSelector
  , initSelector
  , isEnabledSelector
  , nameSelector
  , profileTypeSelector
  , setProfileState_enabledChannelCount_errorSelector
  , totalChannelCountSelector

  -- * Enum types
  , MIDICIProfileType(MIDICIProfileType)
  , pattern KMIDICIProfileTypeSingleChannel
  , pattern KMIDICIProfileTypeGroup
  , pattern KMIDICIProfileTypeFunctionBlock
  , pattern KMIDICIProfileTypeMultichannel

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
init_ :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO (Id MIDIUMPCIProfile)
init_ midiumpciProfile =
  sendOwnedMessage midiumpciProfile initSelector

-- | setProfileState:enabledChannelCount:error:
--
-- Issue a Set Profile On or Set Profile Off request on this profile using the MIDI server's				MUID.
--
-- @isEnabled@ — YES if setting the Profile to on.
--
-- @enabledChannelCount@ — The requsted number of channels to be enabled when									the Profile is enabled. This field is only used when isOn									is set to YES and the profile can enable a variable number									of channels. Otherwise, it is ignored.
--
-- @error@ — The out-error used if an error occurred.
--
-- The result of this operation, if any, is received via the profile notification				posted by the MIDICIDeviceManager.				Returns YES if the request is valid and the request was dispatched.
--
-- ObjC selector: @- setProfileState:enabledChannelCount:error:@
setProfileState_enabledChannelCount_error :: (IsMIDIUMPCIProfile midiumpciProfile, IsNSError error_) => midiumpciProfile -> Bool -> CUShort -> error_ -> IO Bool
setProfileState_enabledChannelCount_error midiumpciProfile isEnabled enabledChannelCount error_ =
  sendMessage midiumpciProfile setProfileState_enabledChannelCount_errorSelector isEnabled enabledChannelCount (toNSError error_)

-- | name
--
-- The name of the MIDI-CI proifle.
--
-- ObjC selector: @- name@
name :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO (Id NSString)
name midiumpciProfile =
  sendMessage midiumpciProfile nameSelector

-- | profileType
--
-- The type of MIDI-CI Profile, i.e., single-channel, multichannel, Group, or Function Block.
--
-- ObjC selector: @- profileType@
profileType :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO MIDICIProfileType
profileType midiumpciProfile =
  sendMessage midiumpciProfile profileTypeSelector

-- | groupOffset
--
-- For Group profiles defined on Function Blocks, the value to be added to the lowest 			Function Block UMP Group for messaging (e.g., a Group Profile defined on the second 			Group of a Function Block has a groupOffset of 1).
--
-- ObjC selector: @- groupOffset@
groupOffset :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUChar
groupOffset midiumpciProfile =
  sendMessage midiumpciProfile groupOffsetSelector

-- | firstChannel
--
-- The first channel number supported on the Profile.
--
-- ObjC selector: @- firstChannel@
firstChannel :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUChar
firstChannel midiumpciProfile =
  sendMessage midiumpciProfile firstChannelSelector

-- | enabledChannelCount
--
-- The number of channels currently enabled on the Profile. When the profile is disabled,				this value is set to 0.
--
-- ObjC selector: @- enabledChannelCount@
enabledChannelCount :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUShort
enabledChannelCount midiumpciProfile =
  sendMessage midiumpciProfile enabledChannelCountSelector

-- | totalChannelCount
--
-- The total number of channels supported by the Profile.
--
-- ObjC selector: @- totalChannelCount@
totalChannelCount :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUShort
totalChannelCount midiumpciProfile =
  sendMessage midiumpciProfile totalChannelCountSelector

-- | isEnabled
--
-- The enable state of the Profile.
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO Bool
isEnabled midiumpciProfile =
  sendMessage midiumpciProfile isEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDIUMPCIProfile)
initSelector = mkSelector "init"

-- | @Selector@ for @setProfileState:enabledChannelCount:error:@
setProfileState_enabledChannelCount_errorSelector :: Selector '[Bool, CUShort, Id NSError] Bool
setProfileState_enabledChannelCount_errorSelector = mkSelector "setProfileState:enabledChannelCount:error:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @profileType@
profileTypeSelector :: Selector '[] MIDICIProfileType
profileTypeSelector = mkSelector "profileType"

-- | @Selector@ for @groupOffset@
groupOffsetSelector :: Selector '[] CUChar
groupOffsetSelector = mkSelector "groupOffset"

-- | @Selector@ for @firstChannel@
firstChannelSelector :: Selector '[] CUChar
firstChannelSelector = mkSelector "firstChannel"

-- | @Selector@ for @enabledChannelCount@
enabledChannelCountSelector :: Selector '[] CUShort
enabledChannelCountSelector = mkSelector "enabledChannelCount"

-- | @Selector@ for @totalChannelCount@
totalChannelCountSelector :: Selector '[] CUShort
totalChannelCountSelector = mkSelector "totalChannelCount"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

