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
  , initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector
  , insertedObjectsSelector
  , updatedObjectsSelector
  , deletedObjectsSelector
  , lockedObjectsSelector


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

-- | @- initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:@
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjects :: (IsNSSaveChangesRequest nsSaveChangesRequest, IsNSSet insertedObjects, IsNSSet updatedObjects, IsNSSet deletedObjects, IsNSSet lockedObjects) => nsSaveChangesRequest -> insertedObjects -> updatedObjects -> deletedObjects -> lockedObjects -> IO (Id NSSaveChangesRequest)
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjects nsSaveChangesRequest  insertedObjects updatedObjects deletedObjects lockedObjects =
withObjCPtr insertedObjects $ \raw_insertedObjects ->
  withObjCPtr updatedObjects $ \raw_updatedObjects ->
    withObjCPtr deletedObjects $ \raw_deletedObjects ->
      withObjCPtr lockedObjects $ \raw_lockedObjects ->
          sendMsg nsSaveChangesRequest (mkSelector "initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:") (retPtr retVoid) [argPtr (castPtr raw_insertedObjects :: Ptr ()), argPtr (castPtr raw_updatedObjects :: Ptr ()), argPtr (castPtr raw_deletedObjects :: Ptr ()), argPtr (castPtr raw_lockedObjects :: Ptr ())] >>= ownedObject . castPtr

-- | @- insertedObjects@
insertedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
insertedObjects nsSaveChangesRequest  =
  sendMsg nsSaveChangesRequest (mkSelector "insertedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updatedObjects@
updatedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
updatedObjects nsSaveChangesRequest  =
  sendMsg nsSaveChangesRequest (mkSelector "updatedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deletedObjects@
deletedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
deletedObjects nsSaveChangesRequest  =
  sendMsg nsSaveChangesRequest (mkSelector "deletedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lockedObjects@
lockedObjects :: IsNSSaveChangesRequest nsSaveChangesRequest => nsSaveChangesRequest -> IO (Id NSSet)
lockedObjects nsSaveChangesRequest  =
  sendMsg nsSaveChangesRequest (mkSelector "lockedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:@
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector :: Selector
initWithInsertedObjects_updatedObjects_deletedObjects_lockedObjectsSelector = mkSelector "initWithInsertedObjects:updatedObjects:deletedObjects:lockedObjects:"

-- | @Selector@ for @insertedObjects@
insertedObjectsSelector :: Selector
insertedObjectsSelector = mkSelector "insertedObjects"

-- | @Selector@ for @updatedObjects@
updatedObjectsSelector :: Selector
updatedObjectsSelector = mkSelector "updatedObjects"

-- | @Selector@ for @deletedObjects@
deletedObjectsSelector :: Selector
deletedObjectsSelector = mkSelector "deletedObjects"

-- | @Selector@ for @lockedObjects@
lockedObjectsSelector :: Selector
lockedObjectsSelector = mkSelector "lockedObjects"

