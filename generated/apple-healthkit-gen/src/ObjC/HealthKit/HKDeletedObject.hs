{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDeletedObject
--
-- A class representing an HKObject that was deleted from the HealtKit database.
--
-- Generated bindings for @HKDeletedObject@.
module ObjC.HealthKit.HKDeletedObject
  ( HKDeletedObject
  , IsHKDeletedObject(..)
  , init_
  , uuid
  , metadata
  , initSelector
  , metadataSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKDeletedObject hkDeletedObject => hkDeletedObject -> IO (Id HKDeletedObject)
init_ hkDeletedObject =
  sendOwnedMessage hkDeletedObject initSelector

-- | UUID
--
-- The unique identifier of the HKObject that was deleted from the HealthKit database.
--
-- ObjC selector: @- UUID@
uuid :: IsHKDeletedObject hkDeletedObject => hkDeletedObject -> IO (Id NSUUID)
uuid hkDeletedObject =
  sendMessage hkDeletedObject uuidSelector

-- | metadata
--
-- Extra information describing properties of the receiver.
--
-- Metadata retained from the deleted HKObject.                Available keys: HKMetadataKeySyncIdentifier, HKMetadataKeySyncVersion
--
-- ObjC selector: @- metadata@
metadata :: IsHKDeletedObject hkDeletedObject => hkDeletedObject -> IO (Id NSDictionary)
metadata hkDeletedObject =
  sendMessage hkDeletedObject metadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKDeletedObject)
initSelector = mkSelector "init"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

