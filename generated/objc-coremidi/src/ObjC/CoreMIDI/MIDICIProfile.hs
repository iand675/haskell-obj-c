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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDICIProfile midiciProfile => midiciProfile -> IO (Id MIDICIProfile)
init_ midiciProfile  =
  sendMsg midiciProfile (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsMIDICIProfile midiciProfile, IsNSData data_) => midiciProfile -> data_ -> IO (Id MIDICIProfile)
initWithData midiciProfile  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg midiciProfile (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:name:@
initWithData_name :: (IsMIDICIProfile midiciProfile, IsNSData data_, IsNSString inName) => midiciProfile -> data_ -> inName -> IO (Id MIDICIProfile)
initWithData_name midiciProfile  data_ inName =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr inName $ \raw_inName ->
      sendMsg midiciProfile (mkSelector "initWithData:name:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_inName :: Ptr ())] >>= ownedObject . castPtr

-- | An NSString describing the profile.
--
-- ObjC selector: @- name@
name :: IsMIDICIProfile midiciProfile => midiciProfile -> IO (Id NSString)
name midiciProfile  =
  sendMsg midiciProfile (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The unique 5-byte profile identifier representing the profile.
--
-- ObjC selector: @- profileID@
profileID :: IsMIDICIProfile midiciProfile => midiciProfile -> IO (Id NSData)
profileID midiciProfile  =
  sendMsg midiciProfile (mkSelector "profileID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithData:name:@
initWithData_nameSelector :: Selector
initWithData_nameSelector = mkSelector "initWithData:name:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @profileID@
profileIDSelector :: Selector
profileIDSelector = mkSelector "profileID"

