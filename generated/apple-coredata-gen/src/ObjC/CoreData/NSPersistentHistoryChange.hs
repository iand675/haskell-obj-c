{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentHistoryChange@.
module ObjC.CoreData.NSPersistentHistoryChange
  ( NSPersistentHistoryChange
  , IsNSPersistentHistoryChange(..)
  , entityDescriptionWithContext
  , entityDescription
  , fetchRequest
  , changeID
  , changedObjectID
  , changeType
  , tombstone
  , transaction
  , updatedProperties
  , changeIDSelector
  , changeTypeSelector
  , changedObjectIDSelector
  , entityDescriptionSelector
  , entityDescriptionWithContextSelector
  , fetchRequestSelector
  , tombstoneSelector
  , transactionSelector
  , updatedPropertiesSelector

  -- * Enum types
  , NSPersistentHistoryChangeType(NSPersistentHistoryChangeType)
  , pattern NSPersistentHistoryChangeTypeInsert
  , pattern NSPersistentHistoryChangeTypeUpdate
  , pattern NSPersistentHistoryChangeTypeDelete

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

-- | @+ entityDescriptionWithContext:@
entityDescriptionWithContext :: IsNSManagedObjectContext context => context -> IO (Id NSEntityDescription)
entityDescriptionWithContext context =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChange"
    sendClassMessage cls' entityDescriptionWithContextSelector (toNSManagedObjectContext context)

-- | @+ entityDescription@
entityDescription :: IO (Id NSEntityDescription)
entityDescription  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChange"
    sendClassMessage cls' entityDescriptionSelector

-- | @+ fetchRequest@
fetchRequest :: IO (Id NSFetchRequest)
fetchRequest  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChange"
    sendClassMessage cls' fetchRequestSelector

-- | @- changeID@
changeID :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO CLong
changeID nsPersistentHistoryChange =
  sendMessage nsPersistentHistoryChange changeIDSelector

-- | @- changedObjectID@
changedObjectID :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSManagedObjectID)
changedObjectID nsPersistentHistoryChange =
  sendMessage nsPersistentHistoryChange changedObjectIDSelector

-- | @- changeType@
changeType :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO NSPersistentHistoryChangeType
changeType nsPersistentHistoryChange =
  sendMessage nsPersistentHistoryChange changeTypeSelector

-- | @- tombstone@
tombstone :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSDictionary)
tombstone nsPersistentHistoryChange =
  sendMessage nsPersistentHistoryChange tombstoneSelector

-- | @- transaction@
transaction :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSPersistentHistoryTransaction)
transaction nsPersistentHistoryChange =
  sendMessage nsPersistentHistoryChange transactionSelector

-- | @- updatedProperties@
updatedProperties :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSSet)
updatedProperties nsPersistentHistoryChange =
  sendMessage nsPersistentHistoryChange updatedPropertiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entityDescriptionWithContext:@
entityDescriptionWithContextSelector :: Selector '[Id NSManagedObjectContext] (Id NSEntityDescription)
entityDescriptionWithContextSelector = mkSelector "entityDescriptionWithContext:"

-- | @Selector@ for @entityDescription@
entityDescriptionSelector :: Selector '[] (Id NSEntityDescription)
entityDescriptionSelector = mkSelector "entityDescription"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @changeID@
changeIDSelector :: Selector '[] CLong
changeIDSelector = mkSelector "changeID"

-- | @Selector@ for @changedObjectID@
changedObjectIDSelector :: Selector '[] (Id NSManagedObjectID)
changedObjectIDSelector = mkSelector "changedObjectID"

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector '[] NSPersistentHistoryChangeType
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @tombstone@
tombstoneSelector :: Selector '[] (Id NSDictionary)
tombstoneSelector = mkSelector "tombstone"

-- | @Selector@ for @transaction@
transactionSelector :: Selector '[] (Id NSPersistentHistoryTransaction)
transactionSelector = mkSelector "transaction"

-- | @Selector@ for @updatedProperties@
updatedPropertiesSelector :: Selector '[] (Id NSSet)
updatedPropertiesSelector = mkSelector "updatedProperties"

