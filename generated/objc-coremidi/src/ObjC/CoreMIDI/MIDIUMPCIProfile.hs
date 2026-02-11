{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , setProfileState_enabledChannelCount_errorSelector
  , nameSelector
  , profileTypeSelector
  , groupOffsetSelector
  , firstChannelSelector
  , enabledChannelCountSelector
  , totalChannelCountSelector
  , isEnabledSelector

  -- * Enum types
  , MIDICIProfileType(MIDICIProfileType)
  , pattern KMIDICIProfileTypeSingleChannel
  , pattern KMIDICIProfileTypeGroup
  , pattern KMIDICIProfileTypeFunctionBlock
  , pattern KMIDICIProfileTypeMultichannel

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

import ObjC.CoreMIDI.Internal.Classes
import ObjC.CoreMIDI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO (Id MIDIUMPCIProfile)
init_ midiumpciProfile  =
  sendMsg midiumpciProfile (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
setProfileState_enabledChannelCount_error midiumpciProfile  isEnabled enabledChannelCount error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiumpciProfile (mkSelector "setProfileState:enabledChannelCount:error:") retCULong [argCULong (if isEnabled then 1 else 0), argCUInt (fromIntegral enabledChannelCount), argPtr (castPtr raw_error_ :: Ptr ())]

-- | name
--
-- The name of the MIDI-CI proifle.
--
-- ObjC selector: @- name@
name :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO (Id NSString)
name midiumpciProfile  =
  sendMsg midiumpciProfile (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | profileType
--
-- The type of MIDI-CI Profile, i.e., single-channel, multichannel, Group, or Function Block.
--
-- ObjC selector: @- profileType@
profileType :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO MIDICIProfileType
profileType midiumpciProfile  =
  fmap (coerce :: CUChar -> MIDICIProfileType) $ sendMsg midiumpciProfile (mkSelector "profileType") retCUChar []

-- | groupOffset
--
-- For Group profiles defined on Function Blocks, the value to be added to the lowest 			Function Block UMP Group for messaging (e.g., a Group Profile defined on the second 			Group of a Function Block has a groupOffset of 1).
--
-- ObjC selector: @- groupOffset@
groupOffset :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUChar
groupOffset midiumpciProfile  =
  sendMsg midiumpciProfile (mkSelector "groupOffset") retCUChar []

-- | firstChannel
--
-- The first channel number supported on the Profile.
--
-- ObjC selector: @- firstChannel@
firstChannel :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUChar
firstChannel midiumpciProfile  =
  sendMsg midiumpciProfile (mkSelector "firstChannel") retCUChar []

-- | enabledChannelCount
--
-- The number of channels currently enabled on the Profile. When the profile is disabled,				this value is set to 0.
--
-- ObjC selector: @- enabledChannelCount@
enabledChannelCount :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUShort
enabledChannelCount midiumpciProfile  =
  fmap fromIntegral $ sendMsg midiumpciProfile (mkSelector "enabledChannelCount") retCUInt []

-- | totalChannelCount
--
-- The total number of channels supported by the Profile.
--
-- ObjC selector: @- totalChannelCount@
totalChannelCount :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO CUShort
totalChannelCount midiumpciProfile  =
  fmap fromIntegral $ sendMsg midiumpciProfile (mkSelector "totalChannelCount") retCUInt []

-- | isEnabled
--
-- The enable state of the Profile.
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsMIDIUMPCIProfile midiumpciProfile => midiumpciProfile -> IO Bool
isEnabled midiumpciProfile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiumpciProfile (mkSelector "isEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setProfileState:enabledChannelCount:error:@
setProfileState_enabledChannelCount_errorSelector :: Selector
setProfileState_enabledChannelCount_errorSelector = mkSelector "setProfileState:enabledChannelCount:error:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @profileType@
profileTypeSelector :: Selector
profileTypeSelector = mkSelector "profileType"

-- | @Selector@ for @groupOffset@
groupOffsetSelector :: Selector
groupOffsetSelector = mkSelector "groupOffset"

-- | @Selector@ for @firstChannel@
firstChannelSelector :: Selector
firstChannelSelector = mkSelector "firstChannel"

-- | @Selector@ for @enabledChannelCount@
enabledChannelCountSelector :: Selector
enabledChannelCountSelector = mkSelector "enabledChannelCount"

-- | @Selector@ for @totalChannelCount@
totalChannelCountSelector :: Selector
totalChannelCountSelector = mkSelector "totalChannelCount"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector
isEnabledSelector = mkSelector "isEnabled"

