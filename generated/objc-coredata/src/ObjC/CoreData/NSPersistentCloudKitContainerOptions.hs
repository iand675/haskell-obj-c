{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithContainerIdentifierSelector
  , containerIdentifierSelector
  , databaseScopeSelector
  , setDatabaseScopeSelector

  -- * Enum types
  , CKDatabaseScope(CKDatabaseScope)
  , pattern CKDatabaseScopePublic
  , pattern CKDatabaseScopePrivate
  , pattern CKDatabaseScopeShared

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
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> IO (Id NSPersistentCloudKitContainerOptions)
init_ nsPersistentCloudKitContainerOptions  =
  sendMsg nsPersistentCloudKitContainerOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithContainerIdentifier:@
initWithContainerIdentifier :: (IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions, IsNSString containerIdentifier) => nsPersistentCloudKitContainerOptions -> containerIdentifier -> IO (Id NSPersistentCloudKitContainerOptions)
initWithContainerIdentifier nsPersistentCloudKitContainerOptions  containerIdentifier =
withObjCPtr containerIdentifier $ \raw_containerIdentifier ->
    sendMsg nsPersistentCloudKitContainerOptions (mkSelector "initWithContainerIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_containerIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | The container identifier of the CKContainer to use with a given instance of NSPersistentStoreDescription
--
-- ObjC selector: @- containerIdentifier@
containerIdentifier :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> IO (Id NSString)
containerIdentifier nsPersistentCloudKitContainerOptions  =
  sendMsg nsPersistentCloudKitContainerOptions (mkSelector "containerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- databaseScope@
databaseScope :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> IO CKDatabaseScope
databaseScope nsPersistentCloudKitContainerOptions  =
  fmap (coerce :: CLong -> CKDatabaseScope) $ sendMsg nsPersistentCloudKitContainerOptions (mkSelector "databaseScope") retCLong []

-- | @- setDatabaseScope:@
setDatabaseScope :: IsNSPersistentCloudKitContainerOptions nsPersistentCloudKitContainerOptions => nsPersistentCloudKitContainerOptions -> CKDatabaseScope -> IO ()
setDatabaseScope nsPersistentCloudKitContainerOptions  value =
  sendMsg nsPersistentCloudKitContainerOptions (mkSelector "setDatabaseScope:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContainerIdentifier:@
initWithContainerIdentifierSelector :: Selector
initWithContainerIdentifierSelector = mkSelector "initWithContainerIdentifier:"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @databaseScope@
databaseScopeSelector :: Selector
databaseScopeSelector = mkSelector "databaseScope"

-- | @Selector@ for @setDatabaseScope:@
setDatabaseScopeSelector :: Selector
setDatabaseScopeSelector = mkSelector "setDatabaseScope:"

