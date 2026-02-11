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
  , initSelector
  , initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector
  , notifyProfile_onChannel_isEnabledSelector
  , sendProfile_onChannel_profileDataSelector
  , startSelector
  , stopSelector
  , initiatorsSelector
  , profileDelegateSelector
  , deviceInfoSelector


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
init_ :: IsMIDICIResponder midiciResponder => midiciResponder -> IO (Id MIDICIResponder)
init_ midiciResponder  =
    sendMsg midiciResponder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDeviceInfo:profileDelegate:profileStates:supportProperties:@
initWithDeviceInfo_profileDelegate_profileStates_supportProperties :: (IsMIDICIResponder midiciResponder, IsMIDICIDeviceInfo deviceInfo) => midiciResponder -> deviceInfo -> RawId -> RawId -> Bool -> IO (Id MIDICIResponder)
initWithDeviceInfo_profileDelegate_profileStates_supportProperties midiciResponder  deviceInfo delegate profileList propertiesSupported =
  withObjCPtr deviceInfo $ \raw_deviceInfo ->
      sendMsg midiciResponder (mkSelector "initWithDeviceInfo:profileDelegate:profileStates:supportProperties:") (retPtr retVoid) [argPtr (castPtr raw_deviceInfo :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr (unRawId profileList) :: Ptr ()), argCULong (if propertiesSupported then 1 else 0)] >>= ownedObject . castPtr

-- | @- notifyProfile:onChannel:isEnabled:@
notifyProfile_onChannel_isEnabled :: (IsMIDICIResponder midiciResponder, IsMIDICIProfile aProfile) => midiciResponder -> aProfile -> CUChar -> Bool -> IO Bool
notifyProfile_onChannel_isEnabled midiciResponder  aProfile channel enabledState =
  withObjCPtr aProfile $ \raw_aProfile ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciResponder (mkSelector "notifyProfile:onChannel:isEnabled:") retCULong [argPtr (castPtr raw_aProfile :: Ptr ()), argCUChar channel, argCULong (if enabledState then 1 else 0)]

-- | @- sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileData :: (IsMIDICIResponder midiciResponder, IsMIDICIProfile aProfile, IsNSData profileSpecificData) => midiciResponder -> aProfile -> CUChar -> profileSpecificData -> IO Bool
sendProfile_onChannel_profileData midiciResponder  aProfile channel profileSpecificData =
  withObjCPtr aProfile $ \raw_aProfile ->
    withObjCPtr profileSpecificData $ \raw_profileSpecificData ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciResponder (mkSelector "sendProfile:onChannel:profileData:") retCULong [argPtr (castPtr raw_aProfile :: Ptr ()), argCUChar channel, argPtr (castPtr raw_profileSpecificData :: Ptr ())]

-- | @- start@
start :: IsMIDICIResponder midiciResponder => midiciResponder -> IO Bool
start midiciResponder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciResponder (mkSelector "start") retCULong []

-- | @- stop@
stop :: IsMIDICIResponder midiciResponder => midiciResponder -> IO ()
stop midiciResponder  =
    sendMsg midiciResponder (mkSelector "stop") retVoid []

-- | @- initiators@
initiators :: IsMIDICIResponder midiciResponder => midiciResponder -> IO (Id NSArray)
initiators midiciResponder  =
    sendMsg midiciResponder (mkSelector "initiators") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- profileDelegate@
profileDelegate :: IsMIDICIResponder midiciResponder => midiciResponder -> IO RawId
profileDelegate midiciResponder  =
    fmap (RawId . castPtr) $ sendMsg midiciResponder (mkSelector "profileDelegate") (retPtr retVoid) []

-- | @- deviceInfo@
deviceInfo :: IsMIDICIResponder midiciResponder => midiciResponder -> IO (Id MIDICIDeviceInfo)
deviceInfo midiciResponder  =
    sendMsg midiciResponder (mkSelector "deviceInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDeviceInfo:profileDelegate:profileStates:supportProperties:@
initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector :: Selector
initWithDeviceInfo_profileDelegate_profileStates_supportPropertiesSelector = mkSelector "initWithDeviceInfo:profileDelegate:profileStates:supportProperties:"

-- | @Selector@ for @notifyProfile:onChannel:isEnabled:@
notifyProfile_onChannel_isEnabledSelector :: Selector
notifyProfile_onChannel_isEnabledSelector = mkSelector "notifyProfile:onChannel:isEnabled:"

-- | @Selector@ for @sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileDataSelector :: Selector
sendProfile_onChannel_profileDataSelector = mkSelector "sendProfile:onChannel:profileData:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @initiators@
initiatorsSelector :: Selector
initiatorsSelector = mkSelector "initiators"

-- | @Selector@ for @profileDelegate@
profileDelegateSelector :: Selector
profileDelegateSelector = mkSelector "profileDelegate"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector
deviceInfoSelector = mkSelector "deviceInfo"

