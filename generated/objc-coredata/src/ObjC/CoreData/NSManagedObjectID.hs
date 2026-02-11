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
  , uriRepresentationSelector
  , entitySelector
  , persistentStoreSelector
  , temporaryIDSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- URIRepresentation@
uriRepresentation :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO (Id NSURL)
uriRepresentation nsManagedObjectID  =
  sendMsg nsManagedObjectID (mkSelector "URIRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- entity@
entity :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO (Id NSEntityDescription)
entity nsManagedObjectID  =
  sendMsg nsManagedObjectID (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- persistentStore@
persistentStore :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO (Id NSPersistentStore)
persistentStore nsManagedObjectID  =
  sendMsg nsManagedObjectID (mkSelector "persistentStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temporaryID@
temporaryID :: IsNSManagedObjectID nsManagedObjectID => nsManagedObjectID -> IO Bool
temporaryID nsManagedObjectID  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectID (mkSelector "temporaryID") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URIRepresentation@
uriRepresentationSelector :: Selector
uriRepresentationSelector = mkSelector "URIRepresentation"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @persistentStore@
persistentStoreSelector :: Selector
persistentStoreSelector = mkSelector "persistentStore"

-- | @Selector@ for @temporaryID@
temporaryIDSelector :: Selector
temporaryIDSelector = mkSelector "temporaryID"

