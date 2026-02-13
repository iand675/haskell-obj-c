{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOrderedCollectionDifference@.
module ObjC.Foundation.NSOrderedCollectionDifference
  ( NSOrderedCollectionDifference
  , IsNSOrderedCollectionDifference(..)
  , initWithChanges
  , initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChanges
  , initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects
  , differenceByTransformingChangesWithBlock
  , inverseDifference
  , insertions
  , removals
  , hasChanges
  , differenceByTransformingChangesWithBlockSelector
  , hasChangesSelector
  , initWithChangesSelector
  , initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector
  , initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector
  , insertionsSelector
  , inverseDifferenceSelector
  , removalsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | Creates a new difference representing the changes in the parameter.
--
-- For clients interested in the difference between two collections, the collection's differenceFrom method should be used instead.
--
-- To guarantee that instances are unambiguous and safe for compatible base states, this method requires that its parameter conform to the following requirements:
--
-- 1) All insertion offsets are unique 2) All removal offsets are unique 3) All associated indexes match a change with the opposite parity.
--
-- ObjC selector: @- initWithChanges:@
initWithChanges :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSArray changes) => nsOrderedCollectionDifference -> changes -> IO (Id NSOrderedCollectionDifference)
initWithChanges nsOrderedCollectionDifference changes =
  sendOwnedMessage nsOrderedCollectionDifference initWithChangesSelector (toNSArray changes)

-- | @- initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChanges :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSIndexSet inserts, IsNSArray insertedObjects, IsNSIndexSet removes, IsNSArray removedObjects, IsNSArray changes) => nsOrderedCollectionDifference -> inserts -> insertedObjects -> removes -> removedObjects -> changes -> IO (Id NSOrderedCollectionDifference)
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChanges nsOrderedCollectionDifference inserts insertedObjects removes removedObjects changes =
  sendOwnedMessage nsOrderedCollectionDifference initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector (toNSIndexSet inserts) (toNSArray insertedObjects) (toNSIndexSet removes) (toNSArray removedObjects) (toNSArray changes)

-- | @- initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSIndexSet inserts, IsNSArray insertedObjects, IsNSIndexSet removes, IsNSArray removedObjects) => nsOrderedCollectionDifference -> inserts -> insertedObjects -> removes -> removedObjects -> IO (Id NSOrderedCollectionDifference)
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects nsOrderedCollectionDifference inserts insertedObjects removes removedObjects =
  sendOwnedMessage nsOrderedCollectionDifference initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector (toNSIndexSet inserts) (toNSArray insertedObjects) (toNSIndexSet removes) (toNSArray removedObjects)

-- | @- differenceByTransformingChangesWithBlock:@
differenceByTransformingChangesWithBlock :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSOrderedCollectionChange block) => nsOrderedCollectionDifference -> block -> IO (Id NSOrderedCollectionDifference)
differenceByTransformingChangesWithBlock nsOrderedCollectionDifference block =
  sendMessage nsOrderedCollectionDifference differenceByTransformingChangesWithBlockSelector (toNSOrderedCollectionChange block)

-- | @- inverseDifference@
inverseDifference :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO (Id NSOrderedCollectionDifference)
inverseDifference nsOrderedCollectionDifference =
  sendMessage nsOrderedCollectionDifference inverseDifferenceSelector

-- | @- insertions@
insertions :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO (Id NSArray)
insertions nsOrderedCollectionDifference =
  sendMessage nsOrderedCollectionDifference insertionsSelector

-- | @- removals@
removals :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO (Id NSArray)
removals nsOrderedCollectionDifference =
  sendMessage nsOrderedCollectionDifference removalsSelector

-- | @- hasChanges@
hasChanges :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO Bool
hasChanges nsOrderedCollectionDifference =
  sendMessage nsOrderedCollectionDifference hasChangesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChanges:@
initWithChangesSelector :: Selector '[Id NSArray] (Id NSOrderedCollectionDifference)
initWithChangesSelector = mkSelector "initWithChanges:"

-- | @Selector@ for @initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector :: Selector '[Id NSIndexSet, Id NSArray, Id NSIndexSet, Id NSArray, Id NSArray] (Id NSOrderedCollectionDifference)
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector = mkSelector "initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:"

-- | @Selector@ for @initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector :: Selector '[Id NSIndexSet, Id NSArray, Id NSIndexSet, Id NSArray] (Id NSOrderedCollectionDifference)
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector = mkSelector "initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:"

-- | @Selector@ for @differenceByTransformingChangesWithBlock:@
differenceByTransformingChangesWithBlockSelector :: Selector '[Id NSOrderedCollectionChange] (Id NSOrderedCollectionDifference)
differenceByTransformingChangesWithBlockSelector = mkSelector "differenceByTransformingChangesWithBlock:"

-- | @Selector@ for @inverseDifference@
inverseDifferenceSelector :: Selector '[] (Id NSOrderedCollectionDifference)
inverseDifferenceSelector = mkSelector "inverseDifference"

-- | @Selector@ for @insertions@
insertionsSelector :: Selector '[] (Id NSArray)
insertionsSelector = mkSelector "insertions"

-- | @Selector@ for @removals@
removalsSelector :: Selector '[] (Id NSArray)
removalsSelector = mkSelector "removals"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector '[] Bool
hasChangesSelector = mkSelector "hasChanges"

