{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIResponder@.
module ObjC.CoreMIDI.MIDICIResponder
  ( MIDICIResponder
  , IsMIDICIResponder(..)
  , init_
  , notifyProfile_onChannel_isEnabled
  , sendProfile_onChannel_profileData
  , start
  , stop
  , initiators
  , deviceInfo
  , initSelector
  , notifyProfile_onChannel_isEnabledSelector
  , sendProfile_onChannel_profileDataSelector
  , startSelector
  , stopSelector
  , initiatorsSelector
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

-- | @- notifyProfile:onChannel:isEnabled:@
notifyProfile_onChannel_isEnabled :: (IsMIDICIResponder midiciResponder, IsMIDICIProfile aProfile) => midiciResponder -> aProfile -> CUChar -> Bool -> IO Bool
notifyProfile_onChannel_isEnabled midiciResponder  aProfile channel enabledState =
withObjCPtr aProfile $ \raw_aProfile ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciResponder (mkSelector "notifyProfile:onChannel:isEnabled:") retCULong [argPtr (castPtr raw_aProfile :: Ptr ()), argCUChar (fromIntegral channel), argCULong (if enabledState then 1 else 0)]

-- | @- sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileData :: (IsMIDICIResponder midiciResponder, IsMIDICIProfile aProfile, IsNSData profileSpecificData) => midiciResponder -> aProfile -> CUChar -> profileSpecificData -> IO Bool
sendProfile_onChannel_profileData midiciResponder  aProfile channel profileSpecificData =
withObjCPtr aProfile $ \raw_aProfile ->
  withObjCPtr profileSpecificData $ \raw_profileSpecificData ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciResponder (mkSelector "sendProfile:onChannel:profileData:") retCULong [argPtr (castPtr raw_aProfile :: Ptr ()), argCUChar (fromIntegral channel), argPtr (castPtr raw_profileSpecificData :: Ptr ())]

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

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector
deviceInfoSelector = mkSelector "deviceInfo"

