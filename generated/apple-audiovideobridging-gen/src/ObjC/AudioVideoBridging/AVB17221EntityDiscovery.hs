{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221EntityDiscovery
--
-- AVB17221EntityDiscovery provides access to the IEEE Std 1722.1™-2013 AVDECC Discovery Protocol (ADP) interface.
--
-- AVB17221EntityDiscovery provides access to the IEEE Std 1722.1™-2013 AVDECC Discovery Protocol (ADP) interface.				It provides a delegate with callbacks whenever an entity is added or removed, 				either locally or remotely. AVB17221EntityDiscovery objects are typically not created 				directly but are created indirectly and accessed via the entityDiscovery property of 				the AVBInterface object.
--
-- The AVBInterface object does not register a delegate with the AVB17221EntityDiscovery object				which is allocated. Immediately after obtaining the entityDiscovery value for the first time,				the discoveryDelegate should be set and the primeIterators method should be called. Until 				primeIterators is called, no entities will be discovered.
--
-- Generated bindings for @AVB17221EntityDiscovery@.
module ObjC.AudioVideoBridging.AVB17221EntityDiscovery
  ( AVB17221EntityDiscovery
  , IsAVB17221EntityDiscovery(..)
  , initWithInterfaceName
  , primeIterators
  , discoverEntities
  , discoverEntity
  , addLocalEntity_error
  , removeLocalEntity_error
  , changeEntityWithEntityID_toNewGPTPGrandmasterID_error
  , interfaceName
  , setInterfaceName
  , interface
  , discoveryDelegate
  , setDiscoveryDelegate
  , initWithInterfaceNameSelector
  , primeIteratorsSelector
  , discoverEntitiesSelector
  , discoverEntitySelector
  , addLocalEntity_errorSelector
  , removeLocalEntity_errorSelector
  , changeEntityWithEntityID_toNewGPTPGrandmasterID_errorSelector
  , interfaceNameSelector
  , setInterfaceNameSelector
  , interfaceSelector
  , discoveryDelegateSelector
  , setDiscoveryDelegateSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithInterfaceName:
--
-- Initializes the receiver with a particular interface name.
--
-- @anInterfaceName@ — The BSD interface name for the interface to perform discovery on.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithInterfaceName:@
initWithInterfaceName :: (IsAVB17221EntityDiscovery avB17221EntityDiscovery, IsNSString anInterfaceName) => avB17221EntityDiscovery -> anInterfaceName -> IO (Id AVB17221EntityDiscovery)
initWithInterfaceName avB17221EntityDiscovery  anInterfaceName =
  withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
      sendMsg avB17221EntityDiscovery (mkSelector "initWithInterfaceName:") (retPtr retVoid) [argPtr (castPtr raw_anInterfaceName :: Ptr ())] >>= ownedObject . castPtr

-- | primeIterators
--
-- Prepares the IOIterators for receiving entity arrival, departure and property change notifications.
--
-- This method primes the iterators by iterating over any already available entities. This may be called once, at any time after object creation,				but if the discoveryDelegate property has not been set, any already discovered entity notifications will be lost.
--
-- ObjC selector: @- primeIterators@
primeIterators :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> IO ()
primeIterators avB17221EntityDiscovery  =
    sendMsg avB17221EntityDiscovery (mkSelector "primeIterators") retVoid []

-- | discoverEntities
--
-- Triggers the IEEE Std 1722.1™-2013 ADP service to perform an ENTITY_DISCOVER for all entities (an entity_id of 0).
--
-- Returns: A boolean indicating if the call succedded.
--
-- ObjC selector: @- discoverEntities@
discoverEntities :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> IO Bool
discoverEntities avB17221EntityDiscovery  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221EntityDiscovery (mkSelector "discoverEntities") retCULong []

-- | discoverEntity:
--
-- Triggers the IEEE Std 1722.1™-2013 ADP service to perform an ENTITY_DISCOVER for a specified entity.
--
-- @entityID@ — The entity_id of the entity to look for.
--
-- Returns: A boolean indicating if the call succedded.
--
-- ObjC selector: @- discoverEntity:@
discoverEntity :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> CULong -> IO Bool
discoverEntity avB17221EntityDiscovery  entityID =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221EntityDiscovery (mkSelector "discoverEntity:") retCULong [argCULong entityID]

-- | addLocalEntity:error:
--
-- Publishes a entity as being available on the interface. The in kernel portion creates an IOAVB17221LocalEntity and maintains the ADP messaging.
--
-- @anEntity@ — The entity to be published.
--
-- @error@ — If the request couldn't be completed, on return it contains an instance of NSError that describes the reason why.
--
-- Returns: A boolean indicating if the entity was added.
--
-- ObjC selector: @- addLocalEntity:error:@
addLocalEntity_error :: (IsAVB17221EntityDiscovery avB17221EntityDiscovery, IsAVB17221Entity anEntity, IsNSError error_) => avB17221EntityDiscovery -> anEntity -> error_ -> IO Bool
addLocalEntity_error avB17221EntityDiscovery  anEntity error_ =
  withObjCPtr anEntity $ \raw_anEntity ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221EntityDiscovery (mkSelector "addLocalEntity:error:") retCULong [argPtr (castPtr raw_anEntity :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeLocalEntity:
--
-- Removes a published local entity with the given GUID.
--
-- @guid@ — The GUID of the local entity to remove.
--
-- @error@ — If the request couldn't be completed, on return it contains an instance of NSError that describes the reason why.
--
-- Returns: A boolean indicating if the entity was removed.
--
-- ObjC selector: @- removeLocalEntity:error:@
removeLocalEntity_error :: (IsAVB17221EntityDiscovery avB17221EntityDiscovery, IsNSError error_) => avB17221EntityDiscovery -> CULong -> error_ -> IO Bool
removeLocalEntity_error avB17221EntityDiscovery  guid error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221EntityDiscovery (mkSelector "removeLocalEntity:error:") retCULong [argCULong guid, argPtr (castPtr raw_error_ :: Ptr ())]

-- | changeEntityWithEntityID:toNewGPTPGrandmasterID:
--
-- Change the gptp_grandmaster_id value of the entity when the grandmaster changes.
--
-- @entityID@ — The entity_id of the entity to change.
--
-- @gPTPGrandmasterID@ — The new IEEE Std 802.1AS grandmaster ID.
--
-- @error@ — If the request couldn't be completed, on return it contains an instance of NSError that describes the reason why.
--
-- Returns: A boolean indicating if the entity was updated.
--
-- ObjC selector: @- changeEntityWithEntityID:toNewGPTPGrandmasterID:error:@
changeEntityWithEntityID_toNewGPTPGrandmasterID_error :: (IsAVB17221EntityDiscovery avB17221EntityDiscovery, IsNSError error_) => avB17221EntityDiscovery -> CULong -> CULong -> error_ -> IO Bool
changeEntityWithEntityID_toNewGPTPGrandmasterID_error avB17221EntityDiscovery  entityID gPTPGrandmasterID error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221EntityDiscovery (mkSelector "changeEntityWithEntityID:toNewGPTPGrandmasterID:error:") retCULong [argCULong entityID, argCULong gPTPGrandmasterID, argPtr (castPtr raw_error_ :: Ptr ())]

-- | interfaceName
--
-- The BSD interface name for the interface that discovery is being performed on.
--
-- ObjC selector: @- interfaceName@
interfaceName :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> IO (Id NSString)
interfaceName avB17221EntityDiscovery  =
    sendMsg avB17221EntityDiscovery (mkSelector "interfaceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | interfaceName
--
-- The BSD interface name for the interface that discovery is being performed on.
--
-- ObjC selector: @- setInterfaceName:@
setInterfaceName :: (IsAVB17221EntityDiscovery avB17221EntityDiscovery, IsNSString value) => avB17221EntityDiscovery -> value -> IO ()
setInterfaceName avB17221EntityDiscovery  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avB17221EntityDiscovery (mkSelector "setInterfaceName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | interface
--
-- The AVBInterface object which owns this object. This may be nil if it was not created by an instance of AVBInterface
--
-- ObjC selector: @- interface@
interface :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> IO (Id AVBInterface)
interface avB17221EntityDiscovery  =
    sendMsg avB17221EntityDiscovery (mkSelector "interface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | discoveryDelegate
--
-- The delegate, implementing the AVB17221EntityDiscoveryDelegate protocol, which will handle entities arriving, departing and changing properties.
--
-- ObjC selector: @- discoveryDelegate@
discoveryDelegate :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> IO RawId
discoveryDelegate avB17221EntityDiscovery  =
    fmap (RawId . castPtr) $ sendMsg avB17221EntityDiscovery (mkSelector "discoveryDelegate") (retPtr retVoid) []

-- | discoveryDelegate
--
-- The delegate, implementing the AVB17221EntityDiscoveryDelegate protocol, which will handle entities arriving, departing and changing properties.
--
-- ObjC selector: @- setDiscoveryDelegate:@
setDiscoveryDelegate :: IsAVB17221EntityDiscovery avB17221EntityDiscovery => avB17221EntityDiscovery -> RawId -> IO ()
setDiscoveryDelegate avB17221EntityDiscovery  value =
    sendMsg avB17221EntityDiscovery (mkSelector "setDiscoveryDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @primeIterators@
primeIteratorsSelector :: Selector
primeIteratorsSelector = mkSelector "primeIterators"

-- | @Selector@ for @discoverEntities@
discoverEntitiesSelector :: Selector
discoverEntitiesSelector = mkSelector "discoverEntities"

-- | @Selector@ for @discoverEntity:@
discoverEntitySelector :: Selector
discoverEntitySelector = mkSelector "discoverEntity:"

-- | @Selector@ for @addLocalEntity:error:@
addLocalEntity_errorSelector :: Selector
addLocalEntity_errorSelector = mkSelector "addLocalEntity:error:"

-- | @Selector@ for @removeLocalEntity:error:@
removeLocalEntity_errorSelector :: Selector
removeLocalEntity_errorSelector = mkSelector "removeLocalEntity:error:"

-- | @Selector@ for @changeEntityWithEntityID:toNewGPTPGrandmasterID:error:@
changeEntityWithEntityID_toNewGPTPGrandmasterID_errorSelector :: Selector
changeEntityWithEntityID_toNewGPTPGrandmasterID_errorSelector = mkSelector "changeEntityWithEntityID:toNewGPTPGrandmasterID:error:"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector
interfaceNameSelector = mkSelector "interfaceName"

-- | @Selector@ for @setInterfaceName:@
setInterfaceNameSelector :: Selector
setInterfaceNameSelector = mkSelector "setInterfaceName:"

-- | @Selector@ for @interface@
interfaceSelector :: Selector
interfaceSelector = mkSelector "interface"

-- | @Selector@ for @discoveryDelegate@
discoveryDelegateSelector :: Selector
discoveryDelegateSelector = mkSelector "discoveryDelegate"

-- | @Selector@ for @setDiscoveryDelegate:@
setDiscoveryDelegateSelector :: Selector
setDiscoveryDelegateSelector = mkSelector "setDiscoveryDelegate:"

