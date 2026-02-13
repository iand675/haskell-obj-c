{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreRequest@.
module ObjC.CoreData.NSPersistentStoreRequest
  ( NSPersistentStoreRequest
  , IsNSPersistentStoreRequest(..)
  , affectedStores
  , setAffectedStores
  , requestType
  , affectedStoresSelector
  , requestTypeSelector
  , setAffectedStoresSelector

  -- * Enum types
  , NSPersistentStoreRequestType(NSPersistentStoreRequestType)
  , pattern NSFetchRequestType
  , pattern NSSaveRequestType
  , pattern NSBatchInsertRequestType
  , pattern NSBatchUpdateRequestType
  , pattern NSBatchDeleteRequestType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- affectedStores@
affectedStores :: IsNSPersistentStoreRequest nsPersistentStoreRequest => nsPersistentStoreRequest -> IO (Id NSArray)
affectedStores nsPersistentStoreRequest =
  sendMessage nsPersistentStoreRequest affectedStoresSelector

-- | @- setAffectedStores:@
setAffectedStores :: (IsNSPersistentStoreRequest nsPersistentStoreRequest, IsNSArray value) => nsPersistentStoreRequest -> value -> IO ()
setAffectedStores nsPersistentStoreRequest value =
  sendMessage nsPersistentStoreRequest setAffectedStoresSelector (toNSArray value)

-- | @- requestType@
requestType :: IsNSPersistentStoreRequest nsPersistentStoreRequest => nsPersistentStoreRequest -> IO NSPersistentStoreRequestType
requestType nsPersistentStoreRequest =
  sendMessage nsPersistentStoreRequest requestTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @affectedStores@
affectedStoresSelector :: Selector '[] (Id NSArray)
affectedStoresSelector = mkSelector "affectedStores"

-- | @Selector@ for @setAffectedStores:@
setAffectedStoresSelector :: Selector '[Id NSArray] ()
setAffectedStoresSelector = mkSelector "setAffectedStores:"

-- | @Selector@ for @requestType@
requestTypeSelector :: Selector '[] NSPersistentStoreRequestType
requestTypeSelector = mkSelector "requestType"

