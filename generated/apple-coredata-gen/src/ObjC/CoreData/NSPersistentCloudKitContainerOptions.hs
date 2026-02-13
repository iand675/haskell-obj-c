{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSPersistentCloudKitContainerOptions provides customization of how NSPersistentCloudKitContainer aligns a given instance of NSPersistentStoreDescription with a CloudKit database.
--
-- Generated bindings for @NSPersistentCloudKitContainerOptions@.
module ObjC.CoreData.NSPersistentCloudKitContainerOptions
  ( NSPersistentCloudKitContainerOptions
  , IsNSPersistentCloudKitContainerOptions(..)
  , init_
  , initWithContainerIdentifier
  , containerIdentifier
  , databaseScope
  , setDatabaseScope
  , containerIdentifierSelector
  , databaseScopeSelector
  , initSelector
  , initWithContainerIdentifierSelector
  , setDatabaseScopeSelector

  -- * Enum types
  , CKDatabaseScope(CKDatabaseScope)
  , pattern CKDatabaseScopePublic
  , pattern CKDatabaseScopePrivate
  , pattern CKDatabaseScopeShared

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> IO (Id NSPersistentCloudKitContainerOptions)
init_ nsPersistentCloudKitContainerOptions =
  sendOwnedMessage nsPersistentCloudKitContainerOptions initSelector

-- | @- initWithContainerIdentifier:@
initWithContainerIdentifier :: (IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions, IsNSString containerIdentifier) => nsPersistentCloudKitContainerOptions -> containerIdentifier -> IO (Id NSPersistentCloudKitContainerOptions)
initWithContainerIdentifier nsPersistentCloudKitContainerOptions containerIdentifier =
  sendOwnedMessage nsPersistentCloudKitContainerOptions initWithContainerIdentifierSelector (toNSString containerIdentifier)

-- | The container identifier of the CKContainer to use with a given instance of NSPersistentStoreDescription
--
-- ObjC selector: @- containerIdentifier@
containerIdentifier :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> IO (Id NSString)
containerIdentifier nsPersistentCloudKitContainerOptions =
  sendMessage nsPersistentCloudKitContainerOptions containerIdentifierSelector

-- | @- databaseScope@
databaseScope :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> IO CKDatabaseScope
databaseScope nsPersistentCloudKitContainerOptions =
  sendMessage nsPersistentCloudKitContainerOptions databaseScopeSelector

-- | @- setDatabaseScope:@
setDatabaseScope :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> CKDatabaseScope -> IO ()
setDatabaseScope nsPersistentCloudKitContainerOptions value =
  sendMessage nsPersistentCloudKitContainerOptions setDatabaseScopeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPersistentCloudKitContainerOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContainerIdentifier:@
initWithContainerIdentifierSelector :: Selector '[Id NSString] (Id NSPersistentCloudKitContainerOptions)
initWithContainerIdentifierSelector = mkSelector "initWithContainerIdentifier:"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @databaseScope@
databaseScopeSelector :: Selector '[] CKDatabaseScope
databaseScopeSelector = mkSelector "databaseScope"

-- | @Selector@ for @setDatabaseScope:@
setDatabaseScopeSelector :: Selector '[CKDatabaseScope] ()
setDatabaseScopeSelector = mkSelector "setDatabaseScope:"

