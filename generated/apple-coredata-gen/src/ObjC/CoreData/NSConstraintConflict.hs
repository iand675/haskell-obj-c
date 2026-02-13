{-# LANGUAGE DataKinds #-}
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
  , conflictingObjectsSelector
  , conflictingSnapshotsSelector
  , constraintSelector
  , constraintValuesSelector
  , databaseObjectSelector
  , databaseSnapshotSelector
  , initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:@
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshots :: (IsNSConstraintConflict nsConstraintConflict, IsNSArray contraint, IsNSManagedObject databaseObject, IsNSDictionary databaseSnapshot, IsNSArray conflictingObjects, IsNSArray conflictingSnapshots) => nsConstraintConflict -> contraint -> databaseObject -> databaseSnapshot -> conflictingObjects -> conflictingSnapshots -> IO (Id NSConstraintConflict)
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshots nsConstraintConflict contraint databaseObject databaseSnapshot conflictingObjects conflictingSnapshots =
  sendOwnedMessage nsConstraintConflict initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector (toNSArray contraint) (toNSManagedObject databaseObject) (toNSDictionary databaseSnapshot) (toNSArray conflictingObjects) (toNSArray conflictingSnapshots)

-- | @- constraint@
constraint :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSArray)
constraint nsConstraintConflict =
  sendMessage nsConstraintConflict constraintSelector

-- | @- constraintValues@
constraintValues :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSDictionary)
constraintValues nsConstraintConflict =
  sendMessage nsConstraintConflict constraintValuesSelector

-- | @- databaseObject@
databaseObject :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSManagedObject)
databaseObject nsConstraintConflict =
  sendMessage nsConstraintConflict databaseObjectSelector

-- | @- databaseSnapshot@
databaseSnapshot :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSDictionary)
databaseSnapshot nsConstraintConflict =
  sendMessage nsConstraintConflict databaseSnapshotSelector

-- | @- conflictingObjects@
conflictingObjects :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSArray)
conflictingObjects nsConstraintConflict =
  sendMessage nsConstraintConflict conflictingObjectsSelector

-- | @- conflictingSnapshots@
conflictingSnapshots :: IsNSConstraintConflict nsConstraintConflict => nsConstraintConflict -> IO (Id NSArray)
conflictingSnapshots nsConstraintConflict =
  sendMessage nsConstraintConflict conflictingSnapshotsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:@
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector :: Selector '[Id NSArray, Id NSManagedObject, Id NSDictionary, Id NSArray, Id NSArray] (Id NSConstraintConflict)
initWithConstraint_databaseObject_databaseSnapshot_conflictingObjects_conflictingSnapshotsSelector = mkSelector "initWithConstraint:databaseObject:databaseSnapshot:conflictingObjects:conflictingSnapshots:"

-- | @Selector@ for @constraint@
constraintSelector :: Selector '[] (Id NSArray)
constraintSelector = mkSelector "constraint"

-- | @Selector@ for @constraintValues@
constraintValuesSelector :: Selector '[] (Id NSDictionary)
constraintValuesSelector = mkSelector "constraintValues"

-- | @Selector@ for @databaseObject@
databaseObjectSelector :: Selector '[] (Id NSManagedObject)
databaseObjectSelector = mkSelector "databaseObject"

-- | @Selector@ for @databaseSnapshot@
databaseSnapshotSelector :: Selector '[] (Id NSDictionary)
databaseSnapshotSelector = mkSelector "databaseSnapshot"

-- | @Selector@ for @conflictingObjects@
conflictingObjectsSelector :: Selector '[] (Id NSArray)
conflictingObjectsSelector = mkSelector "conflictingObjects"

-- | @Selector@ for @conflictingSnapshots@
conflictingSnapshotsSelector :: Selector '[] (Id NSArray)
conflictingSnapshotsSelector = mkSelector "conflictingSnapshots"

