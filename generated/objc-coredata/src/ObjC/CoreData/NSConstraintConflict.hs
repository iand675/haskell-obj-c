{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSConstraintConflict@.
module ObjC.CoreData.NSConstraintConflict
  ( NSConstraintConflict
  , IsNSConstraintConflict(..)
  , initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshots
  , constraint
  , constraintValues
  , databaseObject
  , databaseSnapshot
  , conflictingObjects
  , conflictingSnapshots
  , initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector
  , constraintSelector
  , constraintValuesSelector
  , databaseObjectSelector
  , databaseSnapshotSelector
  , conflictingObjectsSelector
  , conflictingSnapshotsSelector


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

-- | @- initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:@
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshots :: (IsNSConstraintConflict nsConstraintConflict, IsNSArray contraint, IsNSManagedObject databaseObject, IsNSDictionary databaseSnapshot, IsNSArray conflictingObjects, IsNSArray conflictingSnapshots) => nsConstraintConflict -> contraint -> databaseObject -> databaseSnapshot -> conflictingObjects -> conflictingSnapshots -> IO (Id NSConstraintConflict)
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshots nsConstraintConflict  contraint databaseObject databaseSnapshot conflictingObjects conflictingSnapshots =
withObjCPtr contraint $ \raw_contraint ->
  withObjCPtr databaseObject $ \raw_databaseObject ->
    withObjCPtr databaseSnapshot $ \raw_databaseSnapshot ->
      withObjCPtr conflictingObjects $ \raw_conflictingObjects ->
        withObjCPtr conflictingSnapshots $ \raw_conflictingSnapshots ->
            sendMsg nsConstraintConflict (mkSelector "initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:") (retPtr retVoid) [argPtr (castPtr raw_contraint :: Ptr ()), argPtr (castPtr raw_databaseObject :: Ptr ()), argPtr (castPtr raw_databaseSnapshot :: Ptr ()), argPtr (castPtr raw_conflictingObjects :: Ptr ()), argPtr (castPtr raw_conflictingSnapshots :: Ptr ())] >>= ownedObject . castPtr

-- | @- constraint@
constraint :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSArray)
constraint nsConstraintConflict  =
  sendMsg nsConstraintConflict (mkSelector "constraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- constraintValues@
constraintValues :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSDictionary)
constraintValues nsConstraintConflict  =
  sendMsg nsConstraintConflict (mkSelector "constraintValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- databaseObject@
databaseObject :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSManagedObject)
databaseObject nsConstraintConflict  =
  sendMsg nsConstraintConflict (mkSelector "databaseObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- databaseSnapshot@
databaseSnapshot :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSDictionary)
databaseSnapshot nsConstraintConflict  =
  sendMsg nsConstraintConflict (mkSelector "databaseSnapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conflictingObjects@
conflictingObjects :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSArray)
conflictingObjects nsConstraintConflict  =
  sendMsg nsConstraintConflict (mkSelector "conflictingObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conflictingSnapshots@
conflictingSnapshots :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSArray)
conflictingSnapshots nsConstraintConflict  =
  sendMsg nsConstraintConflict (mkSelector "conflictingSnapshots") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:@
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector :: Selector
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector = mkSelector "initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:"

-- | @Selector@ for @constraint@
constraintSelector :: Selector
constraintSelector = mkSelector "constraint"

-- | @Selector@ for @constraintValues@
constraintValuesSelector :: Selector
constraintValuesSelector = mkSelector "constraintValues"

-- | @Selector@ for @databaseObject@
databaseObjectSelector :: Selector
databaseObjectSelector = mkSelector "databaseObject"

-- | @Selector@ for @databaseSnapshot@
databaseSnapshotSelector :: Selector
databaseSnapshotSelector = mkSelector "databaseSnapshot"

-- | @Selector@ for @conflictingObjects@
conflictingObjectsSelector :: Selector
conflictingObjectsSelector = mkSelector "conflictingObjects"

-- | @Selector@ for @conflictingSnapshots@
conflictingSnapshotsSelector :: Selector
conflictingSnapshotsSelector = mkSelector "conflictingSnapshots"

