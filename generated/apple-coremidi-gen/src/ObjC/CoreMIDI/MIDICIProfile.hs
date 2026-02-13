{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDICIProfile
--
-- An NSObject representing Capability Inquiry profile. MIDI-CI profiles describe a mapping				of MIDI messages to specific sounds and synthesis behaviors, e.g. General MIDI, a drawbar organ,				etc. A MIDI-CI profile may be a standard registered profile or vendor-specific.
--
-- Standard Profile				Vendor-Specific Profile				Profile ID Byte 1:	0x7E Standard Profile			Manufacturer SysEx ID 1 Profile				Profile ID Byte 2:	Profile Bank				Manufacturer SysEx ID 2 Profile				Profile ID Byte 3:	Profile Number				Manufacturer SysEx ID 3 Profile				Profile ID Byte 4:	Profile Version				Manufacturer-specific Info				Profile ID Byte 5:	Profile Level				Manufacturer-specific Info
--
-- Generated bindings for @MIDICIProfile@.
module ObjC.CoreMIDI.MIDICIProfile
  ( MIDICIProfile
  , IsMIDICIProfile(..)
  , init_
  , initWithData
  , initWithData_name
  , name
  , profileID
  , initSelector
  , initWithDataSelector
  , initWithData_nameSelector
  , nameSelector
  , profileIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDICIProfile midiciProfile => midiciProfile -> IO (Id MIDICIProfile)
init_ midiciProfile =
  sendOwnedMessage midiciProfile initSelector

-- | @- initWithData:@
initWithData :: (IsMIDICIProfile midiciProfile, IsNSData data_) => midiciProfile -> data_ -> IO (Id MIDICIProfile)
initWithData midiciProfile data_ =
  sendOwnedMessage midiciProfile initWithDataSelector (toNSData data_)

-- | @- initWithData:name:@
initWithData_name :: (IsMIDICIProfile midiciProfile, IsNSData data_, IsNSString inName) => midiciProfile -> data_ -> inName -> IO (Id MIDICIProfile)
initWithData_name midiciProfile data_ inName =
  sendOwnedMessage midiciProfile initWithData_nameSelector (toNSData data_) (toNSString inName)

-- | An NSString describing the profile.
--
-- ObjC selector: @- name@
name :: IsMIDICIProfile midiciProfile => midiciProfile -> IO (Id NSString)
name midiciProfile =
  sendMessage midiciProfile nameSelector

-- | The unique 5-byte profile identifier representing the profile.
--
-- ObjC selector: @- profileID@
profileID :: IsMIDICIProfile midiciProfile => midiciProfile -> IO (Id NSData)
profileID midiciProfile =
  sendMessage midiciProfile profileIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDICIProfile)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id MIDICIProfile)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithData:name:@
initWithData_nameSelector :: Selector '[Id NSData, Id NSString] (Id MIDICIProfile)
initWithData_nameSelector = mkSelector "initWithData:name:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @profileID@
profileIDSelector :: Selector '[] (Id NSData)
profileIDSelector = mkSelector "profileID"

