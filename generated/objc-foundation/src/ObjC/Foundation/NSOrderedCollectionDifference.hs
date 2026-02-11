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
  , initWithChangesSelector
  , initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector
  , initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector
  , differenceByTransformingChangesWithBlockSelector
  , inverseDifferenceSelector
  , insertionsSelector
  , removalsSelector
  , hasChangesSelector


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
initWithChanges nsOrderedCollectionDifference  changes =
withObjCPtr changes $ \raw_changes ->
    sendMsg nsOrderedCollectionDifference (mkSelector "initWithChanges:") (retPtr retVoid) [argPtr (castPtr raw_changes :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChanges :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSIndexSet inserts, IsNSArray insertedObjects, IsNSIndexSet removes, IsNSArray removedObjects, IsNSArray changes) => nsOrderedCollectionDifference -> inserts -> insertedObjects -> removes -> removedObjects -> changes -> IO (Id NSOrderedCollectionDifference)
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChanges nsOrderedCollectionDifference  inserts insertedObjects removes removedObjects changes =
withObjCPtr inserts $ \raw_inserts ->
  withObjCPtr insertedObjects $ \raw_insertedObjects ->
    withObjCPtr removes $ \raw_removes ->
      withObjCPtr removedObjects $ \raw_removedObjects ->
        withObjCPtr changes $ \raw_changes ->
            sendMsg nsOrderedCollectionDifference (mkSelector "initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:") (retPtr retVoid) [argPtr (castPtr raw_inserts :: Ptr ()), argPtr (castPtr raw_insertedObjects :: Ptr ()), argPtr (castPtr raw_removes :: Ptr ()), argPtr (castPtr raw_removedObjects :: Ptr ()), argPtr (castPtr raw_changes :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSIndexSet inserts, IsNSArray insertedObjects, IsNSIndexSet removes, IsNSArray removedObjects) => nsOrderedCollectionDifference -> inserts -> insertedObjects -> removes -> removedObjects -> IO (Id NSOrderedCollectionDifference)
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects nsOrderedCollectionDifference  inserts insertedObjects removes removedObjects =
withObjCPtr inserts $ \raw_inserts ->
  withObjCPtr insertedObjects $ \raw_insertedObjects ->
    withObjCPtr removes $ \raw_removes ->
      withObjCPtr removedObjects $ \raw_removedObjects ->
          sendMsg nsOrderedCollectionDifference (mkSelector "initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:") (retPtr retVoid) [argPtr (castPtr raw_inserts :: Ptr ()), argPtr (castPtr raw_insertedObjects :: Ptr ()), argPtr (castPtr raw_removes :: Ptr ()), argPtr (castPtr raw_removedObjects :: Ptr ())] >>= ownedObject . castPtr

-- | @- differenceByTransformingChangesWithBlock:@
differenceByTransformingChangesWithBlock :: (IsNSOrderedCollectionDifference nsOrderedCollectionDifference, IsNSOrderedCollectionChange block) => nsOrderedCollectionDifference -> block -> IO (Id NSOrderedCollectionDifference)
differenceByTransformingChangesWithBlock nsOrderedCollectionDifference  block =
withObjCPtr block $ \raw_block ->
    sendMsg nsOrderedCollectionDifference (mkSelector "differenceByTransformingChangesWithBlock:") (retPtr retVoid) [argPtr (castPtr raw_block :: Ptr ())] >>= retainedObject . castPtr

-- | @- inverseDifference@
inverseDifference :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO (Id NSOrderedCollectionDifference)
inverseDifference nsOrderedCollectionDifference  =
  sendMsg nsOrderedCollectionDifference (mkSelector "inverseDifference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- insertions@
insertions :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO (Id NSArray)
insertions nsOrderedCollectionDifference  =
  sendMsg nsOrderedCollectionDifference (mkSelector "insertions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- removals@
removals :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO (Id NSArray)
removals nsOrderedCollectionDifference  =
  sendMsg nsOrderedCollectionDifference (mkSelector "removals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasChanges@
hasChanges :: IsNSOrderedCollectionDifference nsOrderedCollectionDifference => nsOrderedCollectionDifference -> IO Bool
hasChanges nsOrderedCollectionDifference  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedCollectionDifference (mkSelector "hasChanges") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChanges:@
initWithChangesSelector :: Selector
initWithChangesSelector = mkSelector "initWithChanges:"

-- | @Selector@ for @initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector :: Selector
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjects_additionalChangesSelector = mkSelector "initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:additionalChanges:"

-- | @Selector@ for @initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:@
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector :: Selector
initWithInsertIndexes_insertedObjects_removeIndexes_removedObjectsSelector = mkSelector "initWithInsertIndexes:insertedObjects:removeIndexes:removedObjects:"

-- | @Selector@ for @differenceByTransformingChangesWithBlock:@
differenceByTransformingChangesWithBlockSelector :: Selector
differenceByTransformingChangesWithBlockSelector = mkSelector "differenceByTransformingChangesWithBlock:"

-- | @Selector@ for @inverseDifference@
inverseDifferenceSelector :: Selector
inverseDifferenceSelector = mkSelector "inverseDifference"

-- | @Selector@ for @insertions@
insertionsSelector :: Selector
insertionsSelector = mkSelector "insertions"

-- | @Selector@ for @removals@
removalsSelector :: Selector
removalsSelector = mkSelector "removals"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector
hasChangesSelector = mkSelector "hasChanges"

