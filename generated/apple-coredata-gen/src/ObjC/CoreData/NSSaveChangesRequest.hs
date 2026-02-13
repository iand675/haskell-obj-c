{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSaveChangesRequest@.
module ObjC.CoreData.NSSaveChangesRequest
  ( NSSaveChangesRequest
  , IsNSSaveChangesRequest(..)
  , initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjects
  , insertedObjects
  , updatedObjects
  , deletedObjects
  , lockedObjects
  , deletedObjectsSelector
  , initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector
  , insertedObjectsSelector
  , lockedObjectsSelector
  , updatedObjectsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:@
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjects :: (IsNSSaveChangesRequest nsSaveChangesRequest, IsNSSet insertedObjects, IsNSSet updatedObjects, IsNSSet deletedObjects, IsNSSet lockedObjects) => nsSaveChangesRequest -> insertedObjects -> updatedObjects -> deletedObjects -> lockedObjects -> IO (Id NSSaveChangesRequest)
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjects nsSaveChangesRequest insertedObjects updatedObjects deletedObjects lockedObjects =
  sendOwnedMessage nsSaveChangesRequest initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector (toNSSet insertedObjects) (toNSSet updatedObjects) (toNSSet deletedObjects) (toNSSet lockedObjects)

-- | @- insertedObjects@
insertedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
insertedObjects nsSaveChangesRequest =
  sendMessage nsSaveChangesRequest insertedObjectsSelector

-- | @- updatedObjects@
updatedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
updatedObjects nsSaveChangesRequest =
  sendMessage nsSaveChangesRequest updatedObjectsSelector

-- | @- deletedObjects@
deletedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
deletedObjects nsSaveChangesRequest =
  sendMessage nsSaveChangesRequest deletedObjectsSelector

-- | @- lockedObjects@
lockedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
lockedObjects nsSaveChangesRequest =
  sendMessage nsSaveChangesRequest lockedObjectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:@
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector :: Selector '[Id NSSet, Id NSSet, Id NSSet, Id NSSet] (Id NSSaveChangesRequest)
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector = mkSelector "initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:"

-- | @Selector@ for @insertedObjects@
insertedObjectsSelector :: Selector '[] (Id NSSet)
insertedObjectsSelector = mkSelector "insertedObjects"

-- | @Selector@ for @updatedObjects@
updatedObjectsSelector :: Selector '[] (Id NSSet)
updatedObjectsSelector = mkSelector "updatedObjects"

-- | @Selector@ for @deletedObjects@
deletedObjectsSelector :: Selector '[] (Id NSSet)
deletedObjectsSelector = mkSelector "deletedObjects"

-- | @Selector@ for @lockedObjects@
lockedObjectsSelector :: Selector '[] (Id NSSet)
lockedObjectsSelector = mkSelector "lockedObjects"

