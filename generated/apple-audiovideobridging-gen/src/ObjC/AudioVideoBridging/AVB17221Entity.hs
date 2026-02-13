{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , associationIDSelector
  , availableIndexSelector
  , controllerCapabilitiesSelector
  , currentConfigurationIndexSelector
  , entityCapabilitiesSelector
  , entityDiscoverySelector
  , entityIDSelector
  , entityModelIDSelector
  , gPTPDomainNumberSelector
  , gPTPGrandmasterIDSelector
  , identifyControlIndexSelector
  , interfaceIndexSelector
  , listenerCapabilitiesSelector
  , listenerStreamSinksSelector
  , localEntitySelector
  , macAddressesSelector
  , setAssociationIDSelector
  , setAvailableIndexSelector
  , setControllerCapabilitiesSelector
  , setCurrentConfigurationIndexSelector
  , setEntityCapabilitiesSelector
  , setEntityDiscoverySelector
  , setEntityIDSelector
  , setEntityModelIDSelector
  , setGPTPDomainNumberSelector
  , setGPTPGrandmasterIDSelector
  , setIdentifyControlIndexSelector
  , setInterfaceIndexSelector
  , setListenerCapabilitiesSelector
  , setListenerStreamSinksSelector
  , setLocalEntitySelector
  , setMacAddressesSelector
  , setTalkerCapabilitiesSelector
  , setTalkerStreamSourcesSelector
  , setTimeToLiveSelector
  , talkerCapabilitiesSelector
  , talkerStreamSourcesSelector
  , timeToLiveSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
localEntity avB17221Entity =
  sendMessage avB17221Entity localEntitySelector

-- | localEntity
--
-- YES if the entity is published locally on the machine and NO if the entity has been discovered on the network.
--
-- ObjC selector: @- setLocalEntity:@
setLocalEntity :: IsAVB17221Entity avB17221Entity => avB17221Entity -> Bool -> IO ()
setLocalEntity avB17221Entity value =
  sendMessage avB17221Entity setLocalEntitySelector value

-- | timeToLive
--
-- The number of seconds left until the entity registration times out.
--
-- ObjC selector: @- timeToLive@
timeToLive :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUChar
timeToLive avB17221Entity =
  sendMessage avB17221Entity timeToLiveSelector

-- | timeToLive
--
-- The number of seconds left until the entity registration times out.
--
-- ObjC selector: @- setTimeToLive:@
setTimeToLive :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUChar -> IO ()
setTimeToLive avB17221Entity value =
  sendMessage avB17221Entity setTimeToLiveSelector value

-- | entityID
--
-- The Unique Identifier (entity_id) of the entity.
--
-- ObjC selector: @- entityID@
entityID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
entityID avB17221Entity =
  sendMessage avB17221Entity entityIDSelector

-- | entityID
--
-- The Unique Identifier (entity_id) of the entity.
--
-- ObjC selector: @- setEntityID:@
setEntityID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setEntityID avB17221Entity value =
  sendMessage avB17221Entity setEntityIDSelector value

-- | entityModelID
--
-- The Entity Model Unique Identifier (entity_model_id) of the entity.
--
-- ObjC selector: @- entityModelID@
entityModelID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
entityModelID avB17221Entity =
  sendMessage avB17221Entity entityModelIDSelector

-- | entityModelID
--
-- The Entity Model Unique Identifier (entity_model_id) of the entity.
--
-- ObjC selector: @- setEntityModelID:@
setEntityModelID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setEntityModelID avB17221Entity value =
  sendMessage avB17221Entity setEntityModelIDSelector value

-- | entityCapabilities
--
-- The entity_capabilities of the entity.
--
-- ObjC selector: @- entityCapabilities@
entityCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPEntityCapabilities
entityCapabilities avB17221Entity =
  sendMessage avB17221Entity entityCapabilitiesSelector

-- | entityCapabilities
--
-- The entity_capabilities of the entity.
--
-- ObjC selector: @- setEntityCapabilities:@
setEntityCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPEntityCapabilities -> IO ()
setEntityCapabilities avB17221Entity value =
  sendMessage avB17221Entity setEntityCapabilitiesSelector value

-- | talkerStreamSources
--
-- The number of stream sources the entity has.
--
-- ObjC selector: @- talkerStreamSources@
talkerStreamSources :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
talkerStreamSources avB17221Entity =
  sendMessage avB17221Entity talkerStreamSourcesSelector

-- | talkerStreamSources
--
-- The number of stream sources the entity has.
--
-- ObjC selector: @- setTalkerStreamSources:@
setTalkerStreamSources :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setTalkerStreamSources avB17221Entity value =
  sendMessage avB17221Entity setTalkerStreamSourcesSelector value

-- | talkerCapabilities
--
-- The talker_capabilities of the entity.
--
-- ObjC selector: @- talkerCapabilities@
talkerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPTalkerCapabilities
talkerCapabilities avB17221Entity =
  sendMessage avB17221Entity talkerCapabilitiesSelector

-- | talkerCapabilities
--
-- The talker_capabilities of the entity.
--
-- ObjC selector: @- setTalkerCapabilities:@
setTalkerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPTalkerCapabilities -> IO ()
setTalkerCapabilities avB17221Entity value =
  sendMessage avB17221Entity setTalkerCapabilitiesSelector value

-- | listenerStreamSinks
--
-- The number of stream sinks the entity has.
--
-- ObjC selector: @- listenerStreamSinks@
listenerStreamSinks :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
listenerStreamSinks avB17221Entity =
  sendMessage avB17221Entity listenerStreamSinksSelector

-- | listenerStreamSinks
--
-- The number of stream sinks the entity has.
--
-- ObjC selector: @- setListenerStreamSinks:@
setListenerStreamSinks :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setListenerStreamSinks avB17221Entity value =
  sendMessage avB17221Entity setListenerStreamSinksSelector value

-- | listenerCapabilities
--
-- The listener_capabilities of the entity.
--
-- ObjC selector: @- listenerCapabilities@
listenerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPListenerCapabilities
listenerCapabilities avB17221Entity =
  sendMessage avB17221Entity listenerCapabilitiesSelector

-- | listenerCapabilities
--
-- The listener_capabilities of the entity.
--
-- ObjC selector: @- setListenerCapabilities:@
setListenerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPListenerCapabilities -> IO ()
setListenerCapabilities avB17221Entity value =
  sendMessage avB17221Entity setListenerCapabilitiesSelector value

-- | controllerCapabilities
--
-- The controller_capabilities of the entity.
--
-- ObjC selector: @- controllerCapabilities@
controllerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO AVB17221ADPControllerCapabilities
controllerCapabilities avB17221Entity =
  sendMessage avB17221Entity controllerCapabilitiesSelector

-- | controllerCapabilities
--
-- The controller_capabilities of the entity.
--
-- ObjC selector: @- setControllerCapabilities:@
setControllerCapabilities :: IsAVB17221Entity avB17221Entity => avB17221Entity -> AVB17221ADPControllerCapabilities -> IO ()
setControllerCapabilities avB17221Entity value =
  sendMessage avB17221Entity setControllerCapabilitiesSelector value

-- | availableIndex
--
-- The available_index of the entity.
--
-- ObjC selector: @- availableIndex@
availableIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUInt
availableIndex avB17221Entity =
  sendMessage avB17221Entity availableIndexSelector

-- | availableIndex
--
-- The available_index of the entity.
--
-- ObjC selector: @- setAvailableIndex:@
setAvailableIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUInt -> IO ()
setAvailableIndex avB17221Entity value =
  sendMessage avB17221Entity setAvailableIndexSelector value

-- | gPTPGrandmasterID
--
-- The clock identifier of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- gPTPGrandmasterID@
gPTPGrandmasterID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
gPTPGrandmasterID avB17221Entity =
  sendMessage avB17221Entity gPTPGrandmasterIDSelector

-- | gPTPGrandmasterID
--
-- The clock identifier of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- setGPTPGrandmasterID:@
setGPTPGrandmasterID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setGPTPGrandmasterID avB17221Entity value =
  sendMessage avB17221Entity setGPTPGrandmasterIDSelector value

-- | gPTPDomainNumber
--
-- The domain number of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- gPTPDomainNumber@
gPTPDomainNumber :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUChar
gPTPDomainNumber avB17221Entity =
  sendMessage avB17221Entity gPTPDomainNumberSelector

-- | gPTPDomainNumber
--
-- The domain number of the IEEE Std 802.1AS-2011 grandmaster of the entity.
--
-- ObjC selector: @- setGPTPDomainNumber:@
setGPTPDomainNumber :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUChar -> IO ()
setGPTPDomainNumber avB17221Entity value =
  sendMessage avB17221Entity setGPTPDomainNumberSelector value

-- | identifyControlIndex
--
-- The descriptor_index of the CONTROL which implements the IDENTIFY for the entity if supported.
--
-- ObjC selector: @- identifyControlIndex@
identifyControlIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
identifyControlIndex avB17221Entity =
  sendMessage avB17221Entity identifyControlIndexSelector

-- | identifyControlIndex
--
-- The descriptor_index of the CONTROL which implements the IDENTIFY for the entity if supported.
--
-- ObjC selector: @- setIdentifyControlIndex:@
setIdentifyControlIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setIdentifyControlIndex avB17221Entity value =
  sendMessage avB17221Entity setIdentifyControlIndexSelector value

-- | interfaceIndex
--
-- The descriptor_index of the AVB_INTERFACE descriptor which is associated with this entity.
--
-- ObjC selector: @- interfaceIndex@
interfaceIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
interfaceIndex avB17221Entity =
  sendMessage avB17221Entity interfaceIndexSelector

-- | interfaceIndex
--
-- The descriptor_index of the AVB_INTERFACE descriptor which is associated with this entity.
--
-- ObjC selector: @- setInterfaceIndex:@
setInterfaceIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setInterfaceIndex avB17221Entity value =
  sendMessage avB17221Entity setInterfaceIndexSelector value

-- | associationID
--
-- The association_id of the entity.
--
-- ObjC selector: @- associationID@
associationID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CULong
associationID avB17221Entity =
  sendMessage avB17221Entity associationIDSelector

-- | associationID
--
-- The association_id of the entity.
--
-- ObjC selector: @- setAssociationID:@
setAssociationID :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CULong -> IO ()
setAssociationID avB17221Entity value =
  sendMessage avB17221Entity setAssociationIDSelector value

-- | currentConfigurationIndexIndex
--
-- The descriptor_index of the current CONFIGURATION. This is only valid if entityCapabilities includes AVB17221ADPEntityCapabilitiesAEMConfigurationIndexValid
--
-- ObjC selector: @- currentConfigurationIndex@
currentConfigurationIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO CUShort
currentConfigurationIndex avB17221Entity =
  sendMessage avB17221Entity currentConfigurationIndexSelector

-- | currentConfigurationIndexIndex
--
-- The descriptor_index of the current CONFIGURATION. This is only valid if entityCapabilities includes AVB17221ADPEntityCapabilitiesAEMConfigurationIndexValid
--
-- ObjC selector: @- setCurrentConfigurationIndex:@
setCurrentConfigurationIndex :: IsAVB17221Entity avB17221Entity => avB17221Entity -> CUShort -> IO ()
setCurrentConfigurationIndex avB17221Entity value =
  sendMessage avB17221Entity setCurrentConfigurationIndexSelector value

-- | macAddresses
--
-- An array of AVBMACAddress objects containing the current MAC addresses of the entity.
--
-- ObjC selector: @- macAddresses@
macAddresses :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO (Id NSArray)
macAddresses avB17221Entity =
  sendMessage avB17221Entity macAddressesSelector

-- | macAddresses
--
-- An array of AVBMACAddress objects containing the current MAC addresses of the entity.
--
-- ObjC selector: @- setMacAddresses:@
setMacAddresses :: (IsAVB17221Entity avB17221Entity, IsNSArray value) => avB17221Entity -> value -> IO ()
setMacAddresses avB17221Entity value =
  sendMessage avB17221Entity setMacAddressesSelector (toNSArray value)

-- | entityDiscovery
--
-- The AVB17221EntityDiscovery object which discovered the entity.
--
-- ObjC selector: @- entityDiscovery@
entityDiscovery :: IsAVB17221Entity avB17221Entity => avB17221Entity -> IO (Id AVB17221EntityDiscovery)
entityDiscovery avB17221Entity =
  sendMessage avB17221Entity entityDiscoverySelector

-- | entityDiscovery
--
-- The AVB17221EntityDiscovery object which discovered the entity.
--
-- ObjC selector: @- setEntityDiscovery:@
setEntityDiscovery :: (IsAVB17221Entity avB17221Entity, IsAVB17221EntityDiscovery value) => avB17221Entity -> value -> IO ()
setEntityDiscovery avB17221Entity value =
  sendMessage avB17221Entity setEntityDiscoverySelector (toAVB17221EntityDiscovery value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localEntity@
localEntitySelector :: Selector '[] Bool
localEntitySelector = mkSelector "localEntity"

-- | @Selector@ for @setLocalEntity:@
setLocalEntitySelector :: Selector '[Bool] ()
setLocalEntitySelector = mkSelector "setLocalEntity:"

-- | @Selector@ for @timeToLive@
timeToLiveSelector :: Selector '[] CUChar
timeToLiveSelector = mkSelector "timeToLive"

-- | @Selector@ for @setTimeToLive:@
setTimeToLiveSelector :: Selector '[CUChar] ()
setTimeToLiveSelector = mkSelector "setTimeToLive:"

-- | @Selector@ for @entityID@
entityIDSelector :: Selector '[] CULong
entityIDSelector = mkSelector "entityID"

-- | @Selector@ for @setEntityID:@
setEntityIDSelector :: Selector '[CULong] ()
setEntityIDSelector = mkSelector "setEntityID:"

-- | @Selector@ for @entityModelID@
entityModelIDSelector :: Selector '[] CULong
entityModelIDSelector = mkSelector "entityModelID"

-- | @Selector@ for @setEntityModelID:@
setEntityModelIDSelector :: Selector '[CULong] ()
setEntityModelIDSelector = mkSelector "setEntityModelID:"

-- | @Selector@ for @entityCapabilities@
entityCapabilitiesSelector :: Selector '[] AVB17221ADPEntityCapabilities
entityCapabilitiesSelector = mkSelector "entityCapabilities"

-- | @Selector@ for @setEntityCapabilities:@
setEntityCapabilitiesSelector :: Selector '[AVB17221ADPEntityCapabilities] ()
setEntityCapabilitiesSelector = mkSelector "setEntityCapabilities:"

-- | @Selector@ for @talkerStreamSources@
talkerStreamSourcesSelector :: Selector '[] CUShort
talkerStreamSourcesSelector = mkSelector "talkerStreamSources"

-- | @Selector@ for @setTalkerStreamSources:@
setTalkerStreamSourcesSelector :: Selector '[CUShort] ()
setTalkerStreamSourcesSelector = mkSelector "setTalkerStreamSources:"

-- | @Selector@ for @talkerCapabilities@
talkerCapabilitiesSelector :: Selector '[] AVB17221ADPTalkerCapabilities
talkerCapabilitiesSelector = mkSelector "talkerCapabilities"

-- | @Selector@ for @setTalkerCapabilities:@
setTalkerCapabilitiesSelector :: Selector '[AVB17221ADPTalkerCapabilities] ()
setTalkerCapabilitiesSelector = mkSelector "setTalkerCapabilities:"

-- | @Selector@ for @listenerStreamSinks@
listenerStreamSinksSelector :: Selector '[] CUShort
listenerStreamSinksSelector = mkSelector "listenerStreamSinks"

-- | @Selector@ for @setListenerStreamSinks:@
setListenerStreamSinksSelector :: Selector '[CUShort] ()
setListenerStreamSinksSelector = mkSelector "setListenerStreamSinks:"

-- | @Selector@ for @listenerCapabilities@
listenerCapabilitiesSelector :: Selector '[] AVB17221ADPListenerCapabilities
listenerCapabilitiesSelector = mkSelector "listenerCapabilities"

-- | @Selector@ for @setListenerCapabilities:@
setListenerCapabilitiesSelector :: Selector '[AVB17221ADPListenerCapabilities] ()
setListenerCapabilitiesSelector = mkSelector "setListenerCapabilities:"

-- | @Selector@ for @controllerCapabilities@
controllerCapabilitiesSelector :: Selector '[] AVB17221ADPControllerCapabilities
controllerCapabilitiesSelector = mkSelector "controllerCapabilities"

-- | @Selector@ for @setControllerCapabilities:@
setControllerCapabilitiesSelector :: Selector '[AVB17221ADPControllerCapabilities] ()
setControllerCapabilitiesSelector = mkSelector "setControllerCapabilities:"

-- | @Selector@ for @availableIndex@
availableIndexSelector :: Selector '[] CUInt
availableIndexSelector = mkSelector "availableIndex"

-- | @Selector@ for @setAvailableIndex:@
setAvailableIndexSelector :: Selector '[CUInt] ()
setAvailableIndexSelector = mkSelector "setAvailableIndex:"

-- | @Selector@ for @gPTPGrandmasterID@
gPTPGrandmasterIDSelector :: Selector '[] CULong
gPTPGrandmasterIDSelector = mkSelector "gPTPGrandmasterID"

-- | @Selector@ for @setGPTPGrandmasterID:@
setGPTPGrandmasterIDSelector :: Selector '[CULong] ()
setGPTPGrandmasterIDSelector = mkSelector "setGPTPGrandmasterID:"

-- | @Selector@ for @gPTPDomainNumber@
gPTPDomainNumberSelector :: Selector '[] CUChar
gPTPDomainNumberSelector = mkSelector "gPTPDomainNumber"

-- | @Selector@ for @setGPTPDomainNumber:@
setGPTPDomainNumberSelector :: Selector '[CUChar] ()
setGPTPDomainNumberSelector = mkSelector "setGPTPDomainNumber:"

-- | @Selector@ for @identifyControlIndex@
identifyControlIndexSelector :: Selector '[] CUShort
identifyControlIndexSelector = mkSelector "identifyControlIndex"

-- | @Selector@ for @setIdentifyControlIndex:@
setIdentifyControlIndexSelector :: Selector '[CUShort] ()
setIdentifyControlIndexSelector = mkSelector "setIdentifyControlIndex:"

-- | @Selector@ for @interfaceIndex@
interfaceIndexSelector :: Selector '[] CUShort
interfaceIndexSelector = mkSelector "interfaceIndex"

-- | @Selector@ for @setInterfaceIndex:@
setInterfaceIndexSelector :: Selector '[CUShort] ()
setInterfaceIndexSelector = mkSelector "setInterfaceIndex:"

-- | @Selector@ for @associationID@
associationIDSelector :: Selector '[] CULong
associationIDSelector = mkSelector "associationID"

-- | @Selector@ for @setAssociationID:@
setAssociationIDSelector :: Selector '[CULong] ()
setAssociationIDSelector = mkSelector "setAssociationID:"

-- | @Selector@ for @currentConfigurationIndex@
currentConfigurationIndexSelector :: Selector '[] CUShort
currentConfigurationIndexSelector = mkSelector "currentConfigurationIndex"

-- | @Selector@ for @setCurrentConfigurationIndex:@
setCurrentConfigurationIndexSelector :: Selector '[CUShort] ()
setCurrentConfigurationIndexSelector = mkSelector "setCurrentConfigurationIndex:"

-- | @Selector@ for @macAddresses@
macAddressesSelector :: Selector '[] (Id NSArray)
macAddressesSelector = mkSelector "macAddresses"

-- | @Selector@ for @setMacAddresses:@
setMacAddressesSelector :: Selector '[Id NSArray] ()
setMacAddressesSelector = mkSelector "setMacAddresses:"

-- | @Selector@ for @entityDiscovery@
entityDiscoverySelector :: Selector '[] (Id AVB17221EntityDiscovery)
entityDiscoverySelector = mkSelector "entityDiscovery"

-- | @Selector@ for @setEntityDiscovery:@
setEntityDiscoverySelector :: Selector '[Id AVB17221EntityDiscovery] ()
setEntityDiscoverySelector = mkSelector "setEntityDiscovery:"

