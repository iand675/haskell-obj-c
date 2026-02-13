{-# LANGUAGE DataKinds #-}
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
  , disabledProfilesSelector
  , enabledProfilesSelector
  , initSelector
  , initWithChannel_enabledProfiles_disabledProfilesSelector
  , initWithEnabledProfiles_disabledProfilesSelector
  , midiChannelSelector


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
init_ :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO (Id MIDICIProfileState)
init_ midiciProfileState =
  sendOwnedMessage midiciProfileState initSelector

-- | @- initWithChannel:enabledProfiles:disabledProfiles:@
initWithChannel_enabledProfiles_disabledProfiles :: (IsMIDICIProfileState midiciProfileState, IsNSArray enabled, IsNSArray disabled) => midiciProfileState -> CUChar -> enabled -> disabled -> IO (Id MIDICIProfileState)
initWithChannel_enabledProfiles_disabledProfiles midiciProfileState midiChannelNum enabled disabled =
  sendOwnedMessage midiciProfileState initWithChannel_enabledProfiles_disabledProfilesSelector midiChannelNum (toNSArray enabled) (toNSArray disabled)

-- | @- initWithEnabledProfiles:disabledProfiles:@
initWithEnabledProfiles_disabledProfiles :: (IsMIDICIProfileState midiciProfileState, IsNSArray enabled, IsNSArray disabled) => midiciProfileState -> enabled -> disabled -> IO (Id MIDICIProfileState)
initWithEnabledProfiles_disabledProfiles midiciProfileState enabled disabled =
  sendOwnedMessage midiciProfileState initWithEnabledProfiles_disabledProfilesSelector (toNSArray enabled) (toNSArray disabled)

-- | @- midiChannel@
midiChannel :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO CUChar
midiChannel midiciProfileState =
  sendMessage midiciProfileState midiChannelSelector

-- | @- enabledProfiles@
enabledProfiles :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO (Id NSArray)
enabledProfiles midiciProfileState =
  sendMessage midiciProfileState enabledProfilesSelector

-- | @- disabledProfiles@
disabledProfiles :: IsMIDICIProfileState midiciProfileState => midiciProfileState -> IO (Id NSArray)
disabledProfiles midiciProfileState =
  sendMessage midiciProfileState disabledProfilesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDICIProfileState)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithChannel:enabledProfiles:disabledProfiles:@
initWithChannel_enabledProfiles_disabledProfilesSelector :: Selector '[CUChar, Id NSArray, Id NSArray] (Id MIDICIProfileState)
initWithChannel_enabledProfiles_disabledProfilesSelector = mkSelector "initWithChannel:enabledProfiles:disabledProfiles:"

-- | @Selector@ for @initWithEnabledProfiles:disabledProfiles:@
initWithEnabledProfiles_disabledProfilesSelector :: Selector '[Id NSArray, Id NSArray] (Id MIDICIProfileState)
initWithEnabledProfiles_disabledProfilesSelector = mkSelector "initWithEnabledProfiles:disabledProfiles:"

-- | @Selector@ for @midiChannel@
midiChannelSelector :: Selector '[] CUChar
midiChannelSelector = mkSelector "midiChannel"

-- | @Selector@ for @enabledProfiles@
enabledProfilesSelector :: Selector '[] (Id NSArray)
enabledProfilesSelector = mkSelector "enabledProfiles"

-- | @Selector@ for @disabledProfiles@
disabledProfilesSelector :: Selector '[] (Id NSArray)
disabledProfilesSelector = mkSelector "disabledProfiles"

