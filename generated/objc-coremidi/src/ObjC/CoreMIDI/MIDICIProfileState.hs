{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIProfileState@.
module ObjC.CoreMIDI.MIDICIProfileState
  ( MIDICIProfileState
  , IsMIDICIProfileState(..)
  , init_
  , initWithChannel_enabledProfiles_disabledProfiles
  , initWithEnabledProfiles_disabledProfiles
  , midiChannel
  , enabledProfiles
  , disabledProfiles
  , initSelector
  , initWithChannel_enabledProfiles_disabledProfilesSelector
  , initWithEnabledProfiles_disabledProfilesSelector
  , midiChannelSelector
  , enabledProfilesSelector
  , disabledProfilesSelector


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
init_ :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO (Id MIDICIProfileState)
init_ midiciProfileState  =
  sendMsg midiciProfileState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithChannel:enabledProfiles:disabledProfiles:@
initWithChannel_enabledProfiles_disabledProfiles :: (IsMIDICIProfileState midiciProfileState, IsNSArray enabled, IsNSArray disabled) => midiciProfileState -> CUChar -> enabled -> disabled -> IO (Id MIDICIProfileState)
initWithChannel_enabledProfiles_disabledProfiles midiciProfileState  midiChannelNum enabled disabled =
withObjCPtr enabled $ \raw_enabled ->
  withObjCPtr disabled $ \raw_disabled ->
      sendMsg midiciProfileState (mkSelector "initWithChannel:enabledProfiles:disabledProfiles:") (retPtr retVoid) [argCUChar (fromIntegral midiChannelNum), argPtr (castPtr raw_enabled :: Ptr ()), argPtr (castPtr raw_disabled :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEnabledProfiles:disabledProfiles:@
initWithEnabledProfiles_disabledProfiles :: (IsMIDICIProfileState midiciProfileState, IsNSArray enabled, IsNSArray disabled) => midiciProfileState -> enabled -> disabled -> IO (Id MIDICIProfileState)
initWithEnabledProfiles_disabledProfiles midiciProfileState  enabled disabled =
withObjCPtr enabled $ \raw_enabled ->
  withObjCPtr disabled $ \raw_disabled ->
      sendMsg midiciProfileState (mkSelector "initWithEnabledProfiles:disabledProfiles:") (retPtr retVoid) [argPtr (castPtr raw_enabled :: Ptr ()), argPtr (castPtr raw_disabled :: Ptr ())] >>= ownedObject . castPtr

-- | @- midiChannel@
midiChannel :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO CUChar
midiChannel midiciProfileState  =
  sendMsg midiciProfileState (mkSelector "midiChannel") retCUChar []

-- | @- enabledProfiles@
enabledProfiles :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO (Id NSArray)
enabledProfiles midiciProfileState  =
  sendMsg midiciProfileState (mkSelector "enabledProfiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- disabledProfiles@
disabledProfiles :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO (Id NSArray)
disabledProfiles midiciProfileState  =
  sendMsg midiciProfileState (mkSelector "disabledProfiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithChannel:enabledProfiles:disabledProfiles:@
initWithChannel_enabledProfiles_disabledProfilesSelector :: Selector
initWithChannel_enabledProfiles_disabledProfilesSelector = mkSelector "initWithChannel:enabledProfiles:disabledProfiles:"

-- | @Selector@ for @initWithEnabledProfiles:disabledProfiles:@
initWithEnabledProfiles_disabledProfilesSelector :: Selector
initWithEnabledProfiles_disabledProfilesSelector = mkSelector "initWithEnabledProfiles:disabledProfiles:"

-- | @Selector@ for @midiChannel@
midiChannelSelector :: Selector
midiChannelSelector = mkSelector "midiChannel"

-- | @Selector@ for @enabledProfiles@
enabledProfilesSelector :: Selector
enabledProfilesSelector = mkSelector "enabledProfiles"

-- | @Selector@ for @disabledProfiles@
disabledProfilesSelector :: Selector
disabledProfilesSelector = mkSelector "disabledProfiles"

