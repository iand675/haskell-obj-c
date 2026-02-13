{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMergeConflict@.
module ObjC.CoreData.NSMergeConflict
  ( NSMergeConflict
  , IsNSMergeConflict(..)
  , initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot
  , init_
  , sourceObject
  , objectSnapshot
  , cachedSnapshot
  , persistedSnapshot
  , newVersionNumber
  , oldVersionNumber
  , cachedSnapshotSelector
  , initSelector
  , initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector
  , newVersionNumberSelector
  , objectSnapshotSelector
  , oldVersionNumberSelector
  , persistedSnapshotSelector
  , sourceObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:@
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot :: (IsNSMergeConflict nsMergeConflict, IsNSManagedObject srcObject, IsNSDictionary cachesnap, IsNSDictionary persnap) => nsMergeConflict -> srcObject -> CULong -> CULong -> cachesnap -> persnap -> IO (Id NSMergeConflict)
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot nsMergeConflict srcObject newvers oldvers cachesnap persnap =
  sendOwnedMessage nsMergeConflict initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector (toNSManagedObject srcObject) newvers oldvers (toNSDictionary cachesnap) (toNSDictionary persnap)

-- | @- init@
init_ :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSMergeConflict)
init_ nsMergeConflict =
  sendOwnedMessage nsMergeConflict initSelector

-- | @- sourceObject@
sourceObject :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSManagedObject)
sourceObject nsMergeConflict =
  sendMessage nsMergeConflict sourceObjectSelector

-- | @- objectSnapshot@
objectSnapshot :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSDictionary)
objectSnapshot nsMergeConflict =
  sendMessage nsMergeConflict objectSnapshotSelector

-- | @- cachedSnapshot@
cachedSnapshot :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSDictionary)
cachedSnapshot nsMergeConflict =
  sendMessage nsMergeConflict cachedSnapshotSelector

-- | @- persistedSnapshot@
persistedSnapshot :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSDictionary)
persistedSnapshot nsMergeConflict =
  sendMessage nsMergeConflict persistedSnapshotSelector

-- | @- newVersionNumber@
newVersionNumber :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO CULong
newVersionNumber nsMergeConflict =
  sendOwnedMessage nsMergeConflict newVersionNumberSelector

-- | @- oldVersionNumber@
oldVersionNumber :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO CULong
oldVersionNumber nsMergeConflict =
  sendMessage nsMergeConflict oldVersionNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:@
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector :: Selector '[Id NSManagedObject, CULong, CULong, Id NSDictionary, Id NSDictionary] (Id NSMergeConflict)
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector = mkSelector "initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMergeConflict)
initSelector = mkSelector "init"

-- | @Selector@ for @sourceObject@
sourceObjectSelector :: Selector '[] (Id NSManagedObject)
sourceObjectSelector = mkSelector "sourceObject"

-- | @Selector@ for @objectSnapshot@
objectSnapshotSelector :: Selector '[] (Id NSDictionary)
objectSnapshotSelector = mkSelector "objectSnapshot"

-- | @Selector@ for @cachedSnapshot@
cachedSnapshotSelector :: Selector '[] (Id NSDictionary)
cachedSnapshotSelector = mkSelector "cachedSnapshot"

-- | @Selector@ for @persistedSnapshot@
persistedSnapshotSelector :: Selector '[] (Id NSDictionary)
persistedSnapshotSelector = mkSelector "persistedSnapshot"

-- | @Selector@ for @newVersionNumber@
newVersionNumberSelector :: Selector '[] CULong
newVersionNumberSelector = mkSelector "newVersionNumber"

-- | @Selector@ for @oldVersionNumber@
oldVersionNumberSelector :: Selector '[] CULong
oldVersionNumberSelector = mkSelector "oldVersionNumber"

