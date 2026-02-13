{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSManagedObjectID@.
module ObjC.CoreData.NSManagedObjectID
  ( NSManagedObjectID
  , IsNSManagedObjectID(..)
  , uriRepresentation
  , entity
  , persistentStore
  , temporaryID
  , entitySelector
  , persistentStoreSelector
  , temporaryIDSelector
  , uriRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- URIRepresentation@
uriRepresentation :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO (Id NSURL)
uriRepresentation nsManagedObjectID =
  sendMessage nsManagedObjectID uriRepresentationSelector

-- | @- entity@
entity :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO (Id NSEntityDescription)
entity nsManagedObjectID =
  sendMessage nsManagedObjectID entitySelector

-- | @- persistentStore@
persistentStore :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO (Id NSPersistentStore)
persistentStore nsManagedObjectID =
  sendMessage nsManagedObjectID persistentStoreSelector

-- | @- temporaryID@
temporaryID :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO Bool
temporaryID nsManagedObjectID =
  sendMessage nsManagedObjectID temporaryIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URIRepresentation@
uriRepresentationSelector :: Selector '[] (Id NSURL)
uriRepresentationSelector = mkSelector "URIRepresentation"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @persistentStore@
persistentStoreSelector :: Selector '[] (Id NSPersistentStore)
persistentStoreSelector = mkSelector "persistentStore"

-- | @Selector@ for @temporaryID@
temporaryIDSelector :: Selector '[] Bool
temporaryIDSelector = mkSelector "temporaryID"

