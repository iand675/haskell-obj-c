{-# LANGUAGE PatternSynonyms #-}
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
  , entityDescriptionWithContextSelector
  , entityDescriptionSelector
  , fetchRequestSelector
  , changeIDSelector
  , changedObjectIDSelector
  , changeTypeSelector
  , tombstoneSelector
  , transactionSelector
  , updatedPropertiesSelector

  -- * Enum types
  , NSPersistentHistoryChangeType(NSPersistentHistoryChangeType)
  , pattern NSPersistentHistoryChangeTypeInsert
  , pattern NSPersistentHistoryChangeTypeUpdate
  , pattern NSPersistentHistoryChangeTypeDelete

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ entityDescriptionWithContext:@
entityDescriptionWithContext :: IsNSManagedObjectContext context => context -> IO (Id NSEntityDescription)
entityDescriptionWithContext context =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChange"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "entityDescriptionWithContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | @+ entityDescription@
entityDescription :: IO (Id NSEntityDescription)
entityDescription  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChange"
    sendClassMsg cls' (mkSelector "entityDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fetchRequest@
fetchRequest :: IO (Id NSFetchRequest)
fetchRequest  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChange"
    sendClassMsg cls' (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changeID@
changeID :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO CLong
changeID nsPersistentHistoryChange  =
  sendMsg nsPersistentHistoryChange (mkSelector "changeID") retCLong []

-- | @- changedObjectID@
changedObjectID :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSManagedObjectID)
changedObjectID nsPersistentHistoryChange  =
  sendMsg nsPersistentHistoryChange (mkSelector "changedObjectID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changeType@
changeType :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO NSPersistentHistoryChangeType
changeType nsPersistentHistoryChange  =
  fmap (coerce :: CLong -> NSPersistentHistoryChangeType) $ sendMsg nsPersistentHistoryChange (mkSelector "changeType") retCLong []

-- | @- tombstone@
tombstone :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSDictionary)
tombstone nsPersistentHistoryChange  =
  sendMsg nsPersistentHistoryChange (mkSelector "tombstone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transaction@
transaction :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSPersistentHistoryTransaction)
transaction nsPersistentHistoryChange  =
  sendMsg nsPersistentHistoryChange (mkSelector "transaction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updatedProperties@
updatedProperties :: IsNSPersistentHistoryChange nsPersistentHistoryChange => nsPersistentHistoryChange -> IO (Id NSSet)
updatedProperties nsPersistentHistoryChange  =
  sendMsg nsPersistentHistoryChange (mkSelector "updatedProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entityDescriptionWithContext:@
entityDescriptionWithContextSelector :: Selector
entityDescriptionWithContextSelector = mkSelector "entityDescriptionWithContext:"

-- | @Selector@ for @entityDescription@
entityDescriptionSelector :: Selector
entityDescriptionSelector = mkSelector "entityDescription"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @changeID@
changeIDSelector :: Selector
changeIDSelector = mkSelector "changeID"

-- | @Selector@ for @changedObjectID@
changedObjectIDSelector :: Selector
changedObjectIDSelector = mkSelector "changedObjectID"

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @tombstone@
tombstoneSelector :: Selector
tombstoneSelector = mkSelector "tombstone"

-- | @Selector@ for @transaction@
transactionSelector :: Selector
transactionSelector = mkSelector "transaction"

-- | @Selector@ for @updatedProperties@
updatedPropertiesSelector :: Selector
updatedPropertiesSelector = mkSelector "updatedProperties"

