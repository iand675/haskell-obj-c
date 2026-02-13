{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIResponder@.
module ObjC.CoreMIDI.MIDICIResponder
  ( MIDICIResponder
  , IsMIDICIResponder(..)
  , init_
  , initWithDeviceInfo_profileDelegate_profileStates_supportProperties
  , notifyProfile_onChannel_isEnabled
  , sendProfile_onChannel_profileData
  , start
  , stop
  , initiators
  , profileDelegate
  , deviceInfo
  , deviceInfoSelector
  , initSelector
  , initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector
  , initiatorsSelector
  , notifyProfile_onChannel_isEnabledSelector
  , profileDelegateSelector
  , sendProfile_onChannel_profileDataSelector
  , startSelector
  , stopSelector


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
init_ :: IsMIDICIResponder midiciResponder => midiciResponder -> IO (Id MIDICIResponder)
init_ midiciResponder =
  sendOwnedMessage midiciResponder initSelector

-- | @- initWithDeviceInfo:profileDelegate:profileStates:supportProperties:@
initWithDeviceInfo_profileDelegate_profileStates_supportProperties :: (IsMIDICIResponder midiciResponder, IsMIDICIDeviceInfo deviceInfo) => midiciResponder -> deviceInfo -> RawId -> RawId -> Bool -> IO (Id MIDICIResponder)
initWithDeviceInfo_profileDelegate_profileStates_supportProperties midiciResponder deviceInfo delegate profileList propertiesSupported =
  sendOwnedMessage midiciResponder initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector (toMIDICIDeviceInfo deviceInfo) delegate profileList propertiesSupported

-- | @- notifyProfile:onChannel:isEnabled:@
notifyProfile_onChannel_isEnabled :: (IsMIDICIResponder midiciResponder, IsMIDICIProfile aProfile) => midiciResponder -> aProfile -> CUChar -> Bool -> IO Bool
notifyProfile_onChannel_isEnabled midiciResponder aProfile channel enabledState =
  sendMessage midiciResponder notifyProfile_onChannel_isEnabledSelector (toMIDICIProfile aProfile) channel enabledState

-- | @- sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileData :: (IsMIDICIResponder midiciResponder, IsMIDICIProfile aProfile, IsNSData profileSpecificData) => midiciResponder -> aProfile -> CUChar -> profileSpecificData -> IO Bool
sendProfile_onChannel_profileData midiciResponder aProfile channel profileSpecificData =
  sendMessage midiciResponder sendProfile_onChannel_profileDataSelector (toMIDICIProfile aProfile) channel (toNSData profileSpecificData)

-- | @- start@
start :: IsMIDICIResponder midiciResponder => midiciResponder -> IO Bool
start midiciResponder =
  sendMessage midiciResponder startSelector

-- | @- stop@
stop :: IsMIDICIResponder midiciResponder => midiciResponder -> IO ()
stop midiciResponder =
  sendMessage midiciResponder stopSelector

-- | @- initiators@
initiators :: IsMIDICIResponder midiciResponder => midiciResponder -> IO (Id NSArray)
initiators midiciResponder =
  sendOwnedMessage midiciResponder initiatorsSelector

-- | @- profileDelegate@
profileDelegate :: IsMIDICIResponder midiciResponder => midiciResponder -> IO RawId
profileDelegate midiciResponder =
  sendMessage midiciResponder profileDelegateSelector

-- | @- deviceInfo@
deviceInfo :: IsMIDICIResponder midiciResponder => midiciResponder -> IO (Id MIDICIDeviceInfo)
deviceInfo midiciResponder =
  sendMessage midiciResponder deviceInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDICIResponder)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDeviceInfo:profileDelegate:profileStates:supportProperties:@
initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector :: Selector '[Id MIDICIDeviceInfo, RawId, RawId, Bool] (Id MIDICIResponder)
initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector = mkSelector "initWithDeviceInfo:profileDelegate:profileStates:supportProperties:"

-- | @Selector@ for @notifyProfile:onChannel:isEnabled:@
notifyProfile_onChannel_isEnabledSelector :: Selector '[Id MIDICIProfile, CUChar, Bool] Bool
notifyProfile_onChannel_isEnabledSelector = mkSelector "notifyProfile:onChannel:isEnabled:"

-- | @Selector@ for @sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileDataSelector :: Selector '[Id MIDICIProfile, CUChar, Id NSData] Bool
sendProfile_onChannel_profileDataSelector = mkSelector "sendProfile:onChannel:profileData:"

-- | @Selector@ for @start@
startSelector :: Selector '[] Bool
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @initiators@
initiatorsSelector :: Selector '[] (Id NSArray)
initiatorsSelector = mkSelector "initiators"

-- | @Selector@ for @profileDelegate@
profileDelegateSelector :: Selector '[] RawId
profileDelegateSelector = mkSelector "profileDelegate"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector '[] (Id MIDICIDeviceInfo)
deviceInfoSelector = mkSelector "deviceInfo"

