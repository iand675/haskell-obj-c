{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221Entity
--
-- AVB17221Entity class represents an entity that has been discovered on the network.
--
-- AVB17221Entity class represents an entity that has been discovered on the network.				AVB17221Entity objects are created by the AVB17221EntityDiscovery object as they are discovered, 				and passed around to the discovery delegates when notifying them of changes in the state of the network.				Changes include an entity being added, removed or rediscovered. Entities register themselves to receive				automatic updates when any of the discovery values change.
--
-- Generated bindings for @AVB17221Entity@.
module ObjC.AudioVideoBridging.AVB17221Entity
  ( AVB17221Entity
  , IsAVB17221Entity(..)
  , localEntity
  , setLocalEntity
  , timeToLive
  , setTimeToLive
  , entityID
  , setEntityID
  , entityModelID
  , setEntityModelID
  , entityCapabilities
  , setEntityCapabilities
  , talkerStreamSources
  , setTalkerStreamSources
  , talkerCapabilities
  , setTalkerCapabilities
  , listenerStreamSinks
  , setListenerStreamSinks
  , listenerCapabilities
  , setListenerCapabilities
  , controllerCapabilities
  , setControllerCapabilities
  , availableIndex
  , setAvailableIndex
  , gPTPGrandmasterID
  , setGPTPGrandmasterID
  , gPTPDomainNumber
  , setGPTPDomainNumber
  , identifyControlIndex
  , setIdentifyControlIndex
  , interfaceIndex
  , setInterfaceIndex
  , associationID
  , setAssociationID
  , currentConfigurationIndex
  , setCurrentConfigurationIndex
  , macAddresses
  , setMacAddresses
  , entityDiscovery
  , setEntityDiscovery
  , localEntitySelector
  , setLocalEntitySelector
  , timeToLiveSelector
  , setTimeToLiveSelector
  , entityIDSelector
  , setEntityIDSelector
  , entityModelIDSelector
  , setEntityModelIDSelector
  , entityCapabilitiesSelector
  , setEntityCapabilitiesSelector
  , talkerStreamSourcesSelector
  , setTalkerStreamSourcesSelector
  , talkerCapabilitiesSelector
  , setTalkerCapabilitiesSelector
  , listenerStreamSinksSelector
  , setListenerStreamSinksSelector
  , listenerCapabilitiesSelector
  , setListenerCapabilitiesSelector
  , controllerCapabilitiesSelector
  , setControllerCapabilitiesSelector
  , availableIndexSelector
  , setAvailableIndexSelector
  , gPTPGrandmasterIDSelector
  , setGPTPGrandmasterIDSelector
  , gPTPDomainNumberSelector
  , setGPTPDomainNumberSelector
  , identifyControlIndexSelector
  , setIdentifyControlIndexSelector
  , interfaceIndexSelector
  , setInterfaceIndexSelector
  , associationIDSelector
  , setAssociationIDSelector
  , currentConfigurationIndexSelector
  , setCurrentConfigurationIndexSelector
  , macAddressesSelector
  , setMacAddressesSelector
  , entityDiscoverySelector
  , setEntityDiscoverySelector

  -- * Enum types
  , AVB17221ADPControllerCapabilities(AVB17221ADPControllerCapabilities)
  , pattern AVB17221ADPControllerCapabilitiesImplemented
  , AVB17221ADPEntityCapabilities(AVB17221ADPEntityCapabilities)
  , pattern AVB17221ADPEntityCapabilitiesDFUMode
  , pattern AVB17221ADPEntityCapabilitiesEFUMode
  , pattern AVB17221ADPEntityCapabilitiesAddressAccessSupported
  , pattern AVB17221ADPEntityCapabilitiesGatewayEntity
  , pattern AVB17221ADPEntityCapabilitiesAEMSupported
  , pattern AVB17221ADPEntityCapabilitiesLegacyAVC
  , pattern AVB17221ADPEntityCapabilitiesAssociationIDSupported
  , pattern AVB17221ADPEntityCapabilitiesAssociationIDValid
  , pattern AVB17221ADPEntityCapabilitiesVendorUniqueSupported
  , pattern AVB17221ADPEntityCapabilitiesClassASupported
  , pattern AVB17221ADPEntityCapabilitiesClassBSupported
  , pattern AVB17221ADPEntityCapabilitiesASSupported
  , pattern AVB17221ADPEntityCapabilitiesGPTPSupported
  , pattern AVB17221ADPEntityCapabilitiesAEMAuthenticationSupported
  , pattern AVB17221ADPEntityCapabilitiesAEMAuthenticationRequired
  , pattern AVB17221ADPEntityCapabilitiesAEMPersistentAcquireSupported
  , pattern AVB17221ADPEntityCapabilitiesAEMIdenitifyControlIndexValid
  , pattern AVB17221ADPEntityCapabilitiesAEMInterfaceIndexValid
  , pattern AVB17221ADPEntityCapabilitiesGeneralControllerIgnore
  , pattern AVB17221ADPEntityCapabilitiesEntityNotReady
  , pattern AVB17221ADPEntityCapabilitiesACMPAcquireWithAEM
  , pattern AVB17221ADPEntityCapabilitiesACMPAuthenticateWithAEM
  , pattern AVB17221ADPEntityCapabilitiesSupportsUDPv4ATDECC
  , pattern AVB17221ADPEntityCapabilitiesSupportsUDPv4Streaming
  , pattern AVB17221ADPEntityCapabilitiesSupportsUDPv6ATDECC
  , pattern AVB17221ADPEntityCapabilitiesSupportsUDPv6Streaming
  , pattern AVB17221ADPEntityCapabilitiesMultiplePTPInstances
  , pattern AVB17221ADPEntityCapabilitiesAEMConfigurationIndexValid
  , AVB17221ADPListenerCapabilities(AVB17221ADPListenerCapabilities)
  , pattern AVB17221ADPListenerCapabilitiesImplemented
  , pattern AVB17221ADPListenerCapabilitiesHasOtherSink
  , pattern AVB17221ADPListenerCapabilitiesHasControlSink
  , pattern AVB17221ADPListenerCapabilitiesHasMediaClockSink
  , pattern AVB17221ADPListenerCapabilitiesHasSMPTESink
  , pattern AVB17221ADPListenerCapabilitiesHasMIDISink
  , pattern AVB17221ADPListenerCapabilitiesHasAudioSink
  , pattern AVB17221ADPListenerCapabilitiesHasVideoSink
  , AVB17221ADPTalkerCapabilities(AVB17221ADPTalkerCapabilities)
  , pattern AVB17221ADPTalkerCapabilitiesImplemented
  , pattern AVB17221ADPTalkerCapabilitiesHasOtherSource
  , pattern AVB17221ADPTalkerCapabilitiesHasControlSource
  , pattern AVB17221ADPTalkerCapabilitiesHasMediaClockSource
  , pattern AVB17221ADPTalkerCapabilitiesHasSMPTESource
  , pattern AVB17221ADPTalkerCapabilitiesHasMIDISource
  , pattern AVB17221ADPTalkerCapabilitiesHasAudioSource
  , pattern AVB17221ADPTalkerCapabilitiesHasVideoSource

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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.AudioVideoBridging.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | localEntity
--
-- YES if the entity is published locally on the machine and NO if the entity has been discovered on the network.
--
-- ObjC selector: @- localEntity@
localEntity :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO Bool
localEntity avB17221Entity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221Entity (mkSelector "localEntity") retCULong []

-- | localEntity
--
-- YES if the entity is published locally on the machine and NO if the entity has been discovered on the network.
--
-- ObjC selector: @- setLocalEntity:@
setLocalEntity :: IsAVB17221Entity avB17221Entity => avB17221Entity -> Bool -> IO ()
setLocalEntity avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setLocalEntity:") retVoid [argCULong (if value then 1 else 0)]

-- | timeToLive
--
-- The number of seconds left until the entity registration times out.
--
-- ObjC selector: @- timeToLive@
timeToLive :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUChar
timeToLive avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "timeToLive") retCUChar []

-- | timeToLive
--
-- The number of seconds left until the entity registration times out.
--
-- ObjC selector: @- setTimeToLive:@
setTimeToLive :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUChar -> IO ()
setTimeToLive avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setTimeToLive:") retVoid [argCUChar (fromIntegral value)]

-- | entityID
--
-- The Unique Identifier (entity_id) of the entity.
--
-- ObjC selector: @- entityID@
entityID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
entityID avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "entityID") retCULong []

-- | entityID
--
-- The Unique Identifier (entity_id) of the entity.
--
-- ObjC selector: @- setEntityID:@
setEntityID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setEntityID avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setEntityID:") retVoid [argCULong (fromIntegral value)]

-- | entityModelID
--
-- The Entity Model Unique Identifier (entity_model_id) of the entity.
--
-- ObjC selector: @- entityModelID@
entityModelID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
entityModelID avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "entityModelID") retCULong []

-- | entityModelID
--
-- The Entity Model Unique Identifier (entity_model_id) of the entity.
--
-- ObjC selector: @- setEntityModelID:@
setEntityModelID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setEntityModelID avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setEntityModelID:") retVoid [argCULong (fromIntegral value)]

-- | entityCapabilities
--
-- The entity_capabilities of the entity.
--
-- ObjC selector: @- entityCapabilities@
entityCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPEntityCapabilities
entityCapabilities avB17221Entity  =
  fmap (coerce :: CUInt -> AVB17221ADPEntityCapabilities) $ sendMsg avB17221Entity (mkSelector "entityCapabilities") retCUInt []

-- | entityCapabilities
--
-- The entity_capabilities of the entity.
--
-- ObjC selector: @- setEntityCapabilities:@
setEntityCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPEntityCapabilities -> IO ()
setEntityCapabilities avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setEntityCapabilities:") retVoid [argCUInt (coerce value)]

-- | talkerStreamSources
--
-- The number of stream sources the entity has.
--
-- ObjC selector: @- talkerStreamSources@
talkerStreamSources :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
talkerStreamSources avB17221Entity  =
  fmap fromIntegral $ sendMsg avB17221Entity (mkSelector "talkerStreamSources") retCUInt []

-- | talkerStreamSources
--
-- The number of stream sources the entity has.
--
-- ObjC selector: @- setTalkerStreamSources:@
setTalkerStreamSources :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setTalkerStreamSources avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setTalkerStreamSources:") retVoid [argCUInt (fromIntegral value)]

-- | talkerCapabilities
--
-- The talker_capabilities of the entity.
--
-- ObjC selector: @- talkerCapabilities@
talkerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPTalkerCapabilities
talkerCapabilities avB17221Entity  =
  fmap (coerce :: CUInt -> AVB17221ADPTalkerCapabilities) $ sendMsg avB17221Entity (mkSelector "talkerCapabilities") retCUInt []

-- | talkerCapabilities
--
-- The talker_capabilities of the entity.
--
-- ObjC selector: @- setTalkerCapabilities:@
setTalkerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPTalkerCapabilities -> IO ()
setTalkerCapabilities avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setTalkerCapabilities:") retVoid [argCUInt (coerce value)]

-- | listenerStreamSinks
--
-- The number of stream sinks the entity has.
--
-- ObjC selector: @- listenerStreamSinks@
listenerStreamSinks :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
listenerStreamSinks avB17221Entity  =
  fmap fromIntegral $ sendMsg avB17221Entity (mkSelector "listenerStreamSinks") retCUInt []

-- | listenerStreamSinks
--
-- The number of stream sinks the entity has.
--
-- ObjC selector: @- setListenerStreamSinks:@
setListenerStreamSinks :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setListenerStreamSinks avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setListenerStreamSinks:") retVoid [argCUInt (fromIntegral value)]

-- | listenerCapabilities
--
-- The listener_capabilities of the entity.
--
-- ObjC selector: @- listenerCapabilities@
listenerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPListenerCapabilities
listenerCapabilities avB17221Entity  =
  fmap (coerce :: CUInt -> AVB17221ADPListenerCapabilities) $ sendMsg avB17221Entity (mkSelector "listenerCapabilities") retCUInt []

-- | listenerCapabilities
--
-- The listener_capabilities of the entity.
--
-- ObjC selector: @- setListenerCapabilities:@
setListenerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPListenerCapabilities -> IO ()
setListenerCapabilities avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setListenerCapabilities:") retVoid [argCUInt (coerce value)]

-- | controllerCapabilities
--
-- The controller_capabilities of the entity.
--
-- ObjC selector: @- controllerCapabilities@
controllerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPControllerCapabilities
controllerCapabilities avB17221Entity  =
  fmap (coerce :: CUInt -> AVB17221ADPControllerCapabilities) $ sendMsg avB17221Entity (mkSelector "controllerCapabilities") retCUInt []

-- | controllerCapabilities
--
-- The controller_capabilities of the entity.
--
-- ObjC selector: @- setControllerCapabilities:@
setControllerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPControllerCapabilities -> IO ()
setControllerCapabilities avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setControllerCapabilities:") retVoid [argCUInt (coerce value)]

-- | availableIndex
--
-- The available_index of the entity.
--
-- ObjC selector: @- availableIndex@
availableIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUInt
availableIndex avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "availableIndex") retCUInt []

-- | availableIndex
--
-- The available_index of the entity.
--
-- ObjC selector: @- setAvailableIndex:@
setAvailableIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUInt -> IO ()
setAvailableIndex avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setAvailableIndex:") retVoid [argCUInt (fromIntegral value)]

-- | gPTPGrandmasterID
--
-- The clock identifier of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- gPTPGrandmasterID@
gPTPGrandmasterID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
gPTPGrandmasterID avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "gPTPGrandmasterID") retCULong []

-- | gPTPGrandmasterID
--
-- The clock identifier of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- setGPTPGrandmasterID:@
setGPTPGrandmasterID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setGPTPGrandmasterID avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setGPTPGrandmasterID:") retVoid [argCULong (fromIntegral value)]

-- | gPTPDomainNumber
--
-- The domain number of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- gPTPDomainNumber@
gPTPDomainNumber :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUChar
gPTPDomainNumber avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "gPTPDomainNumber") retCUChar []

-- | gPTPDomainNumber
--
-- The domain number of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- setGPTPDomainNumber:@
setGPTPDomainNumber :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUChar -> IO ()
setGPTPDomainNumber avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setGPTPDomainNumber:") retVoid [argCUChar (fromIntegral value)]

-- | identifyControlIndex
--
-- The descriptor_index of the CONTROL which implements the IDENTIFY for the entity if supported.
--
-- ObjC selector: @- identifyControlIndex@
identifyControlIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
identifyControlIndex avB17221Entity  =
  fmap fromIntegral $ sendMsg avB17221Entity (mkSelector "identifyControlIndex") retCUInt []

-- | identifyControlIndex
--
-- The descriptor_index of the CONTROL which implements the IDENTIFY for the entity if supported.
--
-- ObjC selector: @- setIdentifyControlIndex:@
setIdentifyControlIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setIdentifyControlIndex avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setIdentifyControlIndex:") retVoid [argCUInt (fromIntegral value)]

-- | interfaceIndex
--
-- The descriptor_index of the AVB_INTERFACE descriptor which is associated with this entity.
--
-- ObjC selector: @- interfaceIndex@
interfaceIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
interfaceIndex avB17221Entity  =
  fmap fromIntegral $ sendMsg avB17221Entity (mkSelector "interfaceIndex") retCUInt []

-- | interfaceIndex
--
-- The descriptor_index of the AVB_INTERFACE descriptor which is associated with this entity.
--
-- ObjC selector: @- setInterfaceIndex:@
setInterfaceIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setInterfaceIndex avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setInterfaceIndex:") retVoid [argCUInt (fromIntegral value)]

-- | associationID
--
-- The association_id of the entity.
--
-- ObjC selector: @- associationID@
associationID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
associationID avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "associationID") retCULong []

-- | associationID
--
-- The association_id of the entity.
--
-- ObjC selector: @- setAssociationID:@
setAssociationID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setAssociationID avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setAssociationID:") retVoid [argCULong (fromIntegral value)]

-- | currentConfigurationIndexIndex
--
-- The descriptor_index of the current CONFIGURATION. This is only valid if entityCapabilities includes AVB17221ADPEntityCapabilitiesAEMConfigurationIndexValid
--
-- ObjC selector: @- currentConfigurationIndex@
currentConfigurationIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
currentConfigurationIndex avB17221Entity  =
  fmap fromIntegral $ sendMsg avB17221Entity (mkSelector "currentConfigurationIndex") retCUInt []

-- | currentConfigurationIndexIndex
--
-- The descriptor_index of the current CONFIGURATION. This is only valid if entityCapabilities includes AVB17221ADPEntityCapabilitiesAEMConfigurationIndexValid
--
-- ObjC selector: @- setCurrentConfigurationIndex:@
setCurrentConfigurationIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setCurrentConfigurationIndex avB17221Entity  value =
  sendMsg avB17221Entity (mkSelector "setCurrentConfigurationIndex:") retVoid [argCUInt (fromIntegral value)]

-- | macAddresses
--
-- An array of AVBMACAddress objects containing the current MAC addresses of the entity.
--
-- ObjC selector: @- macAddresses@
macAddresses :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO (Id NSArray)
macAddresses avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "macAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | macAddresses
--
-- An array of AVBMACAddress objects containing the current MAC addresses of the entity.
--
-- ObjC selector: @- setMacAddresses:@
setMacAddresses :: (IsAVB17221Entity avB17221Entity, IsNSArray value) => avB17221Entity -> value -> IO ()
setMacAddresses avB17221Entity  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221Entity (mkSelector "setMacAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | entityDiscovery
--
-- The AVB17221EntityDiscovery object which discovered the entity.
--
-- ObjC selector: @- entityDiscovery@
entityDiscovery :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO (Id AVB17221EntityDiscovery)
entityDiscovery avB17221Entity  =
  sendMsg avB17221Entity (mkSelector "entityDiscovery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | entityDiscovery
--
-- The AVB17221EntityDiscovery object which discovered the entity.
--
-- ObjC selector: @- setEntityDiscovery:@
setEntityDiscovery :: (IsAVB17221Entity avB17221Entity, IsAVB17221EntityDiscovery value) => avB17221Entity -> value -> IO ()
setEntityDiscovery avB17221Entity  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221Entity (mkSelector "setEntityDiscovery:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localEntity@
localEntitySelector :: Selector
localEntitySelector = mkSelector "localEntity"

-- | @Selector@ for @setLocalEntity:@
setLocalEntitySelector :: Selector
setLocalEntitySelector = mkSelector "setLocalEntity:"

-- | @Selector@ for @timeToLive@
timeToLiveSelector :: Selector
timeToLiveSelector = mkSelector "timeToLive"

-- | @Selector@ for @setTimeToLive:@
setTimeToLiveSelector :: Selector
setTimeToLiveSelector = mkSelector "setTimeToLive:"

-- | @Selector@ for @entityID@
entityIDSelector :: Selector
entityIDSelector = mkSelector "entityID"

-- | @Selector@ for @setEntityID:@
setEntityIDSelector :: Selector
setEntityIDSelector = mkSelector "setEntityID:"

-- | @Selector@ for @entityModelID@
entityModelIDSelector :: Selector
entityModelIDSelector = mkSelector "entityModelID"

-- | @Selector@ for @setEntityModelID:@
setEntityModelIDSelector :: Selector
setEntityModelIDSelector = mkSelector "setEntityModelID:"

-- | @Selector@ for @entityCapabilities@
entityCapabilitiesSelector :: Selector
entityCapabilitiesSelector = mkSelector "entityCapabilities"

-- | @Selector@ for @setEntityCapabilities:@
setEntityCapabilitiesSelector :: Selector
setEntityCapabilitiesSelector = mkSelector "setEntityCapabilities:"

-- | @Selector@ for @talkerStreamSources@
talkerStreamSourcesSelector :: Selector
talkerStreamSourcesSelector = mkSelector "talkerStreamSources"

-- | @Selector@ for @setTalkerStreamSources:@
setTalkerStreamSourcesSelector :: Selector
setTalkerStreamSourcesSelector = mkSelector "setTalkerStreamSources:"

-- | @Selector@ for @talkerCapabilities@
talkerCapabilitiesSelector :: Selector
talkerCapabilitiesSelector = mkSelector "talkerCapabilities"

-- | @Selector@ for @setTalkerCapabilities:@
setTalkerCapabilitiesSelector :: Selector
setTalkerCapabilitiesSelector = mkSelector "setTalkerCapabilities:"

-- | @Selector@ for @listenerStreamSinks@
listenerStreamSinksSelector :: Selector
listenerStreamSinksSelector = mkSelector "listenerStreamSinks"

-- | @Selector@ for @setListenerStreamSinks:@
setListenerStreamSinksSelector :: Selector
setListenerStreamSinksSelector = mkSelector "setListenerStreamSinks:"

-- | @Selector@ for @listenerCapabilities@
listenerCapabilitiesSelector :: Selector
listenerCapabilitiesSelector = mkSelector "listenerCapabilities"

-- | @Selector@ for @setListenerCapabilities:@
setListenerCapabilitiesSelector :: Selector
setListenerCapabilitiesSelector = mkSelector "setListenerCapabilities:"

-- | @Selector@ for @controllerCapabilities@
controllerCapabilitiesSelector :: Selector
controllerCapabilitiesSelector = mkSelector "controllerCapabilities"

-- | @Selector@ for @setControllerCapabilities:@
setControllerCapabilitiesSelector :: Selector
setControllerCapabilitiesSelector = mkSelector "setControllerCapabilities:"

-- | @Selector@ for @availableIndex@
availableIndexSelector :: Selector
availableIndexSelector = mkSelector "availableIndex"

-- | @Selector@ for @setAvailableIndex:@
setAvailableIndexSelector :: Selector
setAvailableIndexSelector = mkSelector "setAvailableIndex:"

-- | @Selector@ for @gPTPGrandmasterID@
gPTPGrandmasterIDSelector :: Selector
gPTPGrandmasterIDSelector = mkSelector "gPTPGrandmasterID"

-- | @Selector@ for @setGPTPGrandmasterID:@
setGPTPGrandmasterIDSelector :: Selector
setGPTPGrandmasterIDSelector = mkSelector "setGPTPGrandmasterID:"

-- | @Selector@ for @gPTPDomainNumber@
gPTPDomainNumberSelector :: Selector
gPTPDomainNumberSelector = mkSelector "gPTPDomainNumber"

-- | @Selector@ for @setGPTPDomainNumber:@
setGPTPDomainNumberSelector :: Selector
setGPTPDomainNumberSelector = mkSelector "setGPTPDomainNumber:"

-- | @Selector@ for @identifyControlIndex@
identifyControlIndexSelector :: Selector
identifyControlIndexSelector = mkSelector "identifyControlIndex"

-- | @Selector@ for @setIdentifyControlIndex:@
setIdentifyControlIndexSelector :: Selector
setIdentifyControlIndexSelector = mkSelector "setIdentifyControlIndex:"

-- | @Selector@ for @interfaceIndex@
interfaceIndexSelector :: Selector
interfaceIndexSelector = mkSelector "interfaceIndex"

-- | @Selector@ for @setInterfaceIndex:@
setInterfaceIndexSelector :: Selector
setInterfaceIndexSelector = mkSelector "setInterfaceIndex:"

-- | @Selector@ for @associationID@
associationIDSelector :: Selector
associationIDSelector = mkSelector "associationID"

-- | @Selector@ for @setAssociationID:@
setAssociationIDSelector :: Selector
setAssociationIDSelector = mkSelector "setAssociationID:"

-- | @Selector@ for @currentConfigurationIndex@
currentConfigurationIndexSelector :: Selector
currentConfigurationIndexSelector = mkSelector "currentConfigurationIndex"

-- | @Selector@ for @setCurrentConfigurationIndex:@
setCurrentConfigurationIndexSelector :: Selector
setCurrentConfigurationIndexSelector = mkSelector "setCurrentConfigurationIndex:"

-- | @Selector@ for @macAddresses@
macAddressesSelector :: Selector
macAddressesSelector = mkSelector "macAddresses"

-- | @Selector@ for @setMacAddresses:@
setMacAddressesSelector :: Selector
setMacAddressesSelector = mkSelector "setMacAddresses:"

-- | @Selector@ for @entityDiscovery@
entityDiscoverySelector :: Selector
entityDiscoverySelector = mkSelector "entityDiscovery"

-- | @Selector@ for @setEntityDiscovery:@
setEntityDiscoverySelector :: Selector
setEntityDiscoverySelector = mkSelector "setEntityDiscovery:"

