{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A base type that identifies containers and volumes.
--
-- An ``FSEntityIdentifier`` is a UUID to identify a container or volume, optionally with eight bytes of qualifying (differentiating) data. You use the qualifiers in cases in which a file server can receive multiple connections from the same client, which differ by user credentials. In this case, the identifier for each client is the server's base UUID, and a unique qualifier that differs by client.
--
-- > Important: Don't subclass this class.
--
-- Generated bindings for @FSEntityIdentifier@.
module ObjC.FSKit.FSEntityIdentifier
  ( FSEntityIdentifier
  , IsFSEntityIdentifier(..)
  , init_
  , initWithUUID
  , initWithUUID_qualifier
  , initWithUUID_data
  , uuid
  , setUuid
  , qualifier
  , setQualifier
  , initSelector
  , initWithUUIDSelector
  , initWithUUID_dataSelector
  , initWithUUID_qualifierSelector
  , qualifierSelector
  , setQualifierSelector
  , setUuidSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an entity identifier with a random UUID.
--
-- ObjC selector: @- init@
init_ :: IsFSEntityIdentifier fsEntityIdentifier => fsEntityIdentifier -> IO (Id FSEntityIdentifier)
init_ fsEntityIdentifier =
  sendOwnedMessage fsEntityIdentifier initSelector

-- | Creates an entity identifier with the given UUID.
--
-- - Parameter uuid: The UUID to use for this identifier.
--
-- ObjC selector: @- initWithUUID:@
initWithUUID :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID uuid) => fsEntityIdentifier -> uuid -> IO (Id FSEntityIdentifier)
initWithUUID fsEntityIdentifier uuid =
  sendOwnedMessage fsEntityIdentifier initWithUUIDSelector (toNSUUID uuid)

-- | Creates an entity identifier with the given UUID and qualifier data as a 64-bit unsigned integer. - Parameters:   - uuid: The UUID to use for this identifier.   - qualifier: The data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- initWithUUID:qualifier:@
initWithUUID_qualifier :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID uuid) => fsEntityIdentifier -> uuid -> CULong -> IO (Id FSEntityIdentifier)
initWithUUID_qualifier fsEntityIdentifier uuid qualifier =
  sendOwnedMessage fsEntityIdentifier initWithUUID_qualifierSelector (toNSUUID uuid) qualifier

-- | Creates an entity identifier with the given UUID and qualifier data.
--
-- - Parameters:   - uuid: The UUID to use for this identifier.   - qualifierData: The data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- initWithUUID:data:@
initWithUUID_data :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID uuid, IsNSData qualifierData) => fsEntityIdentifier -> uuid -> qualifierData -> IO (Id FSEntityIdentifier)
initWithUUID_data fsEntityIdentifier uuid qualifierData =
  sendOwnedMessage fsEntityIdentifier initWithUUID_dataSelector (toNSUUID uuid) (toNSData qualifierData)

-- | A UUID to uniquely identify this entity.
--
-- ObjC selector: @- uuid@
uuid :: IsFSEntityIdentifier fsEntityIdentifier => fsEntityIdentifier -> IO (Id NSUUID)
uuid fsEntityIdentifier =
  sendMessage fsEntityIdentifier uuidSelector

-- | A UUID to uniquely identify this entity.
--
-- ObjC selector: @- setUuid:@
setUuid :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID value) => fsEntityIdentifier -> value -> IO ()
setUuid fsEntityIdentifier value =
  sendMessage fsEntityIdentifier setUuidSelector (toNSUUID value)

-- | An optional piece of data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- qualifier@
qualifier :: IsFSEntityIdentifier fsEntityIdentifier => fsEntityIdentifier -> IO (Id NSData)
qualifier fsEntityIdentifier =
  sendMessage fsEntityIdentifier qualifierSelector

-- | An optional piece of data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- setQualifier:@
setQualifier :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSData value) => fsEntityIdentifier -> value -> IO ()
setQualifier fsEntityIdentifier value =
  sendMessage fsEntityIdentifier setQualifierSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSEntityIdentifier)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUUID:@
initWithUUIDSelector :: Selector '[Id NSUUID] (Id FSEntityIdentifier)
initWithUUIDSelector = mkSelector "initWithUUID:"

-- | @Selector@ for @initWithUUID:qualifier:@
initWithUUID_qualifierSelector :: Selector '[Id NSUUID, CULong] (Id FSEntityIdentifier)
initWithUUID_qualifierSelector = mkSelector "initWithUUID:qualifier:"

-- | @Selector@ for @initWithUUID:data:@
initWithUUID_dataSelector :: Selector '[Id NSUUID, Id NSData] (Id FSEntityIdentifier)
initWithUUID_dataSelector = mkSelector "initWithUUID:data:"

-- | @Selector@ for @uuid@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @setUuid:@
setUuidSelector :: Selector '[Id NSUUID] ()
setUuidSelector = mkSelector "setUuid:"

-- | @Selector@ for @qualifier@
qualifierSelector :: Selector '[] (Id NSData)
qualifierSelector = mkSelector "qualifier"

-- | @Selector@ for @setQualifier:@
setQualifierSelector :: Selector '[Id NSData] ()
setQualifierSelector = mkSelector "setQualifier:"

