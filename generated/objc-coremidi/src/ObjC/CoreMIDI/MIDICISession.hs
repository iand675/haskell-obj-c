{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICISession@.
module ObjC.CoreMIDI.MIDICISession
  ( MIDICISession
  , IsMIDICISession(..)
  , init_
  , initWithDiscoveredNode_dataReadyHandler_disconnectHandler
  , profileStateForChannel
  , enableProfile_onChannel_error
  , disableProfile_onChannel_error
  , sendProfile_onChannel_profileData
  , midiDestination
  , supportsProfileCapability
  , supportsPropertyCapability
  , deviceInfo
  , maxSysExSize
  , maxPropertyRequests
  , profileChangedCallback
  , setProfileChangedCallback
  , profileSpecificDataHandler
  , setProfileSpecificDataHandler
  , initSelector
  , initWithDiscoveredNode_dataReadyHandler_disconnectHandlerSelector
  , profileStateForChannelSelector
  , enableProfile_onChannel_errorSelector
  , disableProfile_onChannel_errorSelector
  , sendProfile_onChannel_profileDataSelector
  , midiDestinationSelector
  , supportsProfileCapabilitySelector
  , supportsPropertyCapabilitySelector
  , deviceInfoSelector
  , maxSysExSizeSelector
  , maxPropertyRequestsSelector
  , profileChangedCallbackSelector
  , setProfileChangedCallbackSelector
  , profileSpecificDataHandlerSelector
  , setProfileSpecificDataHandlerSelector


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
init_ :: IsMIDICISession midiciSession => midiciSession -> IO (Id MIDICISession)
init_ midiciSession  =
  sendMsg midiciSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDiscoveredNode:dataReadyHandler:disconnectHandler:@
initWithDiscoveredNode_dataReadyHandler_disconnectHandler :: (IsMIDICISession midiciSession, IsMIDICIDiscoveredNode discoveredNode) => midiciSession -> discoveredNode -> Ptr () -> Ptr () -> IO (Id MIDICISession)
initWithDiscoveredNode_dataReadyHandler_disconnectHandler midiciSession  discoveredNode handler disconnectHandler =
withObjCPtr discoveredNode $ \raw_discoveredNode ->
    sendMsg midiciSession (mkSelector "initWithDiscoveredNode:dataReadyHandler:disconnectHandler:") (retPtr retVoid) [argPtr (castPtr raw_discoveredNode :: Ptr ()), argPtr (castPtr handler :: Ptr ()), argPtr (castPtr disconnectHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- profileStateForChannel:@
profileStateForChannel :: IsMIDICISession midiciSession => midiciSession -> CUChar -> IO (Id MIDICIProfileState)
profileStateForChannel midiciSession  channel =
  sendMsg midiciSession (mkSelector "profileStateForChannel:") (retPtr retVoid) [argCUChar (fromIntegral channel)] >>= retainedObject . castPtr

-- | @- enableProfile:onChannel:error:@
enableProfile_onChannel_error :: (IsMIDICISession midiciSession, IsMIDICIProfile profile, IsNSError outError) => midiciSession -> profile -> CUChar -> outError -> IO Bool
enableProfile_onChannel_error midiciSession  profile channel outError =
withObjCPtr profile $ \raw_profile ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciSession (mkSelector "enableProfile:onChannel:error:") retCULong [argPtr (castPtr raw_profile :: Ptr ()), argCUChar (fromIntegral channel), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- disableProfile:onChannel:error:@
disableProfile_onChannel_error :: (IsMIDICISession midiciSession, IsMIDICIProfile profile, IsNSError outError) => midiciSession -> profile -> CUChar -> outError -> IO Bool
disableProfile_onChannel_error midiciSession  profile channel outError =
withObjCPtr profile $ \raw_profile ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciSession (mkSelector "disableProfile:onChannel:error:") retCULong [argPtr (castPtr raw_profile :: Ptr ()), argCUChar (fromIntegral channel), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileData :: (IsMIDICISession midiciSession, IsMIDICIProfile profile, IsNSData profileSpecificData) => midiciSession -> profile -> CUChar -> profileSpecificData -> IO Bool
sendProfile_onChannel_profileData midiciSession  profile channel profileSpecificData =
withObjCPtr profile $ \raw_profile ->
  withObjCPtr profileSpecificData $ \raw_profileSpecificData ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciSession (mkSelector "sendProfile:onChannel:profileData:") retCULong [argPtr (castPtr raw_profile :: Ptr ()), argCUChar (fromIntegral channel), argPtr (castPtr raw_profileSpecificData :: Ptr ())]

-- | @- midiDestination@
midiDestination :: IsMIDICISession midiciSession => midiciSession -> IO CUInt
midiDestination midiciSession  =
  sendMsg midiciSession (mkSelector "midiDestination") retCUInt []

-- | @- supportsProfileCapability@
supportsProfileCapability :: IsMIDICISession midiciSession => midiciSession -> IO Bool
supportsProfileCapability midiciSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciSession (mkSelector "supportsProfileCapability") retCULong []

-- | @- supportsPropertyCapability@
supportsPropertyCapability :: IsMIDICISession midiciSession => midiciSession -> IO Bool
supportsPropertyCapability midiciSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciSession (mkSelector "supportsPropertyCapability") retCULong []

-- | @- deviceInfo@
deviceInfo :: IsMIDICISession midiciSession => midiciSession -> IO (Id MIDICIDeviceInfo)
deviceInfo midiciSession  =
  sendMsg midiciSession (mkSelector "deviceInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maxSysExSize@
maxSysExSize :: IsMIDICISession midiciSession => midiciSession -> IO (Id NSNumber)
maxSysExSize midiciSession  =
  sendMsg midiciSession (mkSelector "maxSysExSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maxPropertyRequests@
maxPropertyRequests :: IsMIDICISession midiciSession => midiciSession -> IO (Id NSNumber)
maxPropertyRequests midiciSession  =
  sendMsg midiciSession (mkSelector "maxPropertyRequests") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- profileChangedCallback@
profileChangedCallback :: IsMIDICISession midiciSession => midiciSession -> IO (Ptr ())
profileChangedCallback midiciSession  =
  fmap castPtr $ sendMsg midiciSession (mkSelector "profileChangedCallback") (retPtr retVoid) []

-- | @- setProfileChangedCallback:@
setProfileChangedCallback :: IsMIDICISession midiciSession => midiciSession -> Ptr () -> IO ()
setProfileChangedCallback midiciSession  value =
  sendMsg midiciSession (mkSelector "setProfileChangedCallback:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- profileSpecificDataHandler@
profileSpecificDataHandler :: IsMIDICISession midiciSession => midiciSession -> IO (Ptr ())
profileSpecificDataHandler midiciSession  =
  fmap castPtr $ sendMsg midiciSession (mkSelector "profileSpecificDataHandler") (retPtr retVoid) []

-- | @- setProfileSpecificDataHandler:@
setProfileSpecificDataHandler :: IsMIDICISession midiciSession => midiciSession -> Ptr () -> IO ()
setProfileSpecificDataHandler midiciSession  value =
  sendMsg midiciSession (mkSelector "setProfileSpecificDataHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDiscoveredNode:dataReadyHandler:disconnectHandler:@
initWithDiscoveredNode_dataReadyHandler_disconnectHandlerSelector :: Selector
initWithDiscoveredNode_dataReadyHandler_disconnectHandlerSelector = mkSelector "initWithDiscoveredNode:dataReadyHandler:disconnectHandler:"

-- | @Selector@ for @profileStateForChannel:@
profileStateForChannelSelector :: Selector
profileStateForChannelSelector = mkSelector "profileStateForChannel:"

-- | @Selector@ for @enableProfile:onChannel:error:@
enableProfile_onChannel_errorSelector :: Selector
enableProfile_onChannel_errorSelector = mkSelector "enableProfile:onChannel:error:"

-- | @Selector@ for @disableProfile:onChannel:error:@
disableProfile_onChannel_errorSelector :: Selector
disableProfile_onChannel_errorSelector = mkSelector "disableProfile:onChannel:error:"

-- | @Selector@ for @sendProfile:onChannel:profileData:@
sendProfile_onChannel_profileDataSelector :: Selector
sendProfile_onChannel_profileDataSelector = mkSelector "sendProfile:onChannel:profileData:"

-- | @Selector@ for @midiDestination@
midiDestinationSelector :: Selector
midiDestinationSelector = mkSelector "midiDestination"

-- | @Selector@ for @supportsProfileCapability@
supportsProfileCapabilitySelector :: Selector
supportsProfileCapabilitySelector = mkSelector "supportsProfileCapability"

-- | @Selector@ for @supportsPropertyCapability@
supportsPropertyCapabilitySelector :: Selector
supportsPropertyCapabilitySelector = mkSelector "supportsPropertyCapability"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @maxSysExSize@
maxSysExSizeSelector :: Selector
maxSysExSizeSelector = mkSelector "maxSysExSize"

-- | @Selector@ for @maxPropertyRequests@
maxPropertyRequestsSelector :: Selector
maxPropertyRequestsSelector = mkSelector "maxPropertyRequests"

-- | @Selector@ for @profileChangedCallback@
profileChangedCallbackSelector :: Selector
profileChangedCallbackSelector = mkSelector "profileChangedCallback"

-- | @Selector@ for @setProfileChangedCallback:@
setProfileChangedCallbackSelector :: Selector
setProfileChangedCallbackSelector = mkSelector "setProfileChangedCallback:"

-- | @Selector@ for @profileSpecificDataHandler@
profileSpecificDataHandlerSelector :: Selector
profileSpecificDataHandlerSelector = mkSelector "profileSpecificDataHandler"

-- | @Selector@ for @setProfileSpecificDataHandler:@
setProfileSpecificDataHandlerSelector :: Selector
setProfileSpecificDataHandlerSelector = mkSelector "setProfileSpecificDataHandler:"

