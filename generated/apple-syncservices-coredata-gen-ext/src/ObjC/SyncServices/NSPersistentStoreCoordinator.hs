{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreCoordinator@.
module ObjC.SyncServices.NSPersistentStoreCoordinator
  ( NSPersistentStoreCoordinator
  , IsNSPersistentStoreCoordinator(..)
  , syncWithClient_inBackground_handler_error
  , setStoresFastSyncDetailsAtURL_forPersistentStore
  , setStoresFastSyncDetailsAtURL_forPersistentStoreSelector
  , syncWithClient_inBackground_handler_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- syncWithClient:inBackground:handler:error:@
syncWithClient_inBackground_handler_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsISyncClient client, IsNSError rError) => nsPersistentStoreCoordinator -> client -> Bool -> RawId -> rError -> IO Bool
syncWithClient_inBackground_handler_error nsPersistentStoreCoordinator client flag syncHandler rError =
  sendMessage nsPersistentStoreCoordinator syncWithClient_inBackground_handler_errorSelector (toISyncClient client) flag syncHandler (toNSError rError)

-- | @- setStoresFastSyncDetailsAtURL:forPersistentStore:@
setStoresFastSyncDetailsAtURL_forPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> url -> store -> IO ()
setStoresFastSyncDetailsAtURL_forPersistentStore nsPersistentStoreCoordinator url store =
  sendMessage nsPersistentStoreCoordinator setStoresFastSyncDetailsAtURL_forPersistentStoreSelector (toNSURL url) (toNSPersistentStore store)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @syncWithClient:inBackground:handler:error:@
syncWithClient_inBackground_handler_errorSelector :: Selector '[Id ISyncClient, Bool, RawId, Id NSError] Bool
syncWithClient_inBackground_handler_errorSelector = mkSelector "syncWithClient:inBackground:handler:error:"

-- | @Selector@ for @setStoresFastSyncDetailsAtURL:forPersistentStore:@
setStoresFastSyncDetailsAtURL_forPersistentStoreSelector :: Selector '[Id NSURL, Id NSPersistentStore] ()
setStoresFastSyncDetailsAtURL_forPersistentStoreSelector = mkSelector "setStoresFastSyncDetailsAtURL:forPersistentStore:"

