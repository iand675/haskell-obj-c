{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSArrayController@.
module ObjC.AppKit.NSArrayController
  ( NSArrayController
  , IsNSArrayController(..)
  , rearrangeObjects
  , didChangeArrangementCriteria
  , arrangeObjects
  , setSelectionIndexes
  , setSelectionIndex
  , addSelectionIndexes
  , removeSelectionIndexes
  , setSelectedObjects
  , addSelectedObjects
  , removeSelectedObjects
  , add
  , remove
  , insert
  , selectNext
  , selectPrevious
  , addObject
  , addObjects
  , insertObject_atArrangedObjectIndex
  , insertObjects_atArrangedObjectIndexes
  , removeObjectAtArrangedObjectIndex
  , removeObjectsAtArrangedObjectIndexes
  , removeObject
  , removeObjects
  , automaticallyRearrangesObjects
  , setAutomaticallyRearrangesObjects
  , automaticRearrangementKeyPaths
  , sortDescriptors
  , setSortDescriptors
  , filterPredicate
  , setFilterPredicate
  , clearsFilterPredicateOnInsertion
  , setClearsFilterPredicateOnInsertion
  , arrangedObjects
  , avoidsEmptySelection
  , setAvoidsEmptySelection
  , preservesSelection
  , setPreservesSelection
  , selectsInsertedObjects
  , setSelectsInsertedObjects
  , alwaysUsesMultipleValuesMarker
  , setAlwaysUsesMultipleValuesMarker
  , selectionIndexes
  , selectionIndex
  , selectedObjects
  , canInsert
  , canSelectNext
  , canSelectPrevious
  , rearrangeObjectsSelector
  , didChangeArrangementCriteriaSelector
  , arrangeObjectsSelector
  , setSelectionIndexesSelector
  , setSelectionIndexSelector
  , addSelectionIndexesSelector
  , removeSelectionIndexesSelector
  , setSelectedObjectsSelector
  , addSelectedObjectsSelector
  , removeSelectedObjectsSelector
  , addSelector
  , removeSelector
  , insertSelector
  , selectNextSelector
  , selectPreviousSelector
  , addObjectSelector
  , addObjectsSelector
  , insertObject_atArrangedObjectIndexSelector
  , insertObjects_atArrangedObjectIndexesSelector
  , removeObjectAtArrangedObjectIndexSelector
  , removeObjectsAtArrangedObjectIndexesSelector
  , removeObjectSelector
  , removeObjectsSelector
  , automaticallyRearrangesObjectsSelector
  , setAutomaticallyRearrangesObjectsSelector
  , automaticRearrangementKeyPathsSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector
  , filterPredicateSelector
  , setFilterPredicateSelector
  , clearsFilterPredicateOnInsertionSelector
  , setClearsFilterPredicateOnInsertionSelector
  , arrangedObjectsSelector
  , avoidsEmptySelectionSelector
  , setAvoidsEmptySelectionSelector
  , preservesSelectionSelector
  , setPreservesSelectionSelector
  , selectsInsertedObjectsSelector
  , setSelectsInsertedObjectsSelector
  , alwaysUsesMultipleValuesMarkerSelector
  , setAlwaysUsesMultipleValuesMarkerSelector
  , selectionIndexesSelector
  , selectionIndexSelector
  , selectedObjectsSelector
  , canInsertSelector
  , canSelectNextSelector
  , canSelectPreviousSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rearrangeObjects@
rearrangeObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO ()
rearrangeObjects nsArrayController  =
    sendMsg nsArrayController (mkSelector "rearrangeObjects") retVoid []

-- | @- didChangeArrangementCriteria@
didChangeArrangementCriteria :: IsNSArrayController nsArrayController => nsArrayController -> IO ()
didChangeArrangementCriteria nsArrayController  =
    sendMsg nsArrayController (mkSelector "didChangeArrangementCriteria") retVoid []

-- | @- arrangeObjects:@
arrangeObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO (Id NSArray)
arrangeObjects nsArrayController  objects =
  withObjCPtr objects $ \raw_objects ->
      sendMsg nsArrayController (mkSelector "arrangeObjects:") (retPtr retVoid) [argPtr (castPtr raw_objects :: Ptr ())] >>= retainedObject . castPtr

-- | @- setSelectionIndexes:@
setSelectionIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO Bool
setSelectionIndexes nsArrayController  indexes =
  withObjCPtr indexes $ \raw_indexes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "setSelectionIndexes:") retCULong [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- setSelectionIndex:@
setSelectionIndex :: IsNSArrayController nsArrayController => nsArrayController -> CULong -> IO Bool
setSelectionIndex nsArrayController  index =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "setSelectionIndex:") retCULong [argCULong index]

-- | @- addSelectionIndexes:@
addSelectionIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO Bool
addSelectionIndexes nsArrayController  indexes =
  withObjCPtr indexes $ \raw_indexes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "addSelectionIndexes:") retCULong [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeSelectionIndexes:@
removeSelectionIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO Bool
removeSelectionIndexes nsArrayController  indexes =
  withObjCPtr indexes $ \raw_indexes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "removeSelectionIndexes:") retCULong [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- setSelectedObjects:@
setSelectedObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO Bool
setSelectedObjects nsArrayController  objects =
  withObjCPtr objects $ \raw_objects ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "setSelectedObjects:") retCULong [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- addSelectedObjects:@
addSelectedObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO Bool
addSelectedObjects nsArrayController  objects =
  withObjCPtr objects $ \raw_objects ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "addSelectedObjects:") retCULong [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- removeSelectedObjects:@
removeSelectedObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO Bool
removeSelectedObjects nsArrayController  objects =
  withObjCPtr objects $ \raw_objects ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "removeSelectedObjects:") retCULong [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- add:@
add :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
add nsArrayController  sender =
    sendMsg nsArrayController (mkSelector "add:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- remove:@
remove :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
remove nsArrayController  sender =
    sendMsg nsArrayController (mkSelector "remove:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- insert:@
insert :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
insert nsArrayController  sender =
    sendMsg nsArrayController (mkSelector "insert:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectNext:@
selectNext :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
selectNext nsArrayController  sender =
    sendMsg nsArrayController (mkSelector "selectNext:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectPrevious:@
selectPrevious :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
selectPrevious nsArrayController  sender =
    sendMsg nsArrayController (mkSelector "selectPrevious:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- addObject:@
addObject :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
addObject nsArrayController  object =
    sendMsg nsArrayController (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- addObjects:@
addObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO ()
addObjects nsArrayController  objects =
  withObjCPtr objects $ \raw_objects ->
      sendMsg nsArrayController (mkSelector "addObjects:") retVoid [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- insertObject:atArrangedObjectIndex:@
insertObject_atArrangedObjectIndex :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> CULong -> IO ()
insertObject_atArrangedObjectIndex nsArrayController  object index =
    sendMsg nsArrayController (mkSelector "insertObject:atArrangedObjectIndex:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argCULong index]

-- | @- insertObjects:atArrangedObjectIndexes:@
insertObjects_atArrangedObjectIndexes :: (IsNSArrayController nsArrayController, IsNSArray objects, IsNSIndexSet indexes) => nsArrayController -> objects -> indexes -> IO ()
insertObjects_atArrangedObjectIndexes nsArrayController  objects indexes =
  withObjCPtr objects $ \raw_objects ->
    withObjCPtr indexes $ \raw_indexes ->
        sendMsg nsArrayController (mkSelector "insertObjects:atArrangedObjectIndexes:") retVoid [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeObjectAtArrangedObjectIndex:@
removeObjectAtArrangedObjectIndex :: IsNSArrayController nsArrayController => nsArrayController -> CULong -> IO ()
removeObjectAtArrangedObjectIndex nsArrayController  index =
    sendMsg nsArrayController (mkSelector "removeObjectAtArrangedObjectIndex:") retVoid [argCULong index]

-- | @- removeObjectsAtArrangedObjectIndexes:@
removeObjectsAtArrangedObjectIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO ()
removeObjectsAtArrangedObjectIndexes nsArrayController  indexes =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsArrayController (mkSelector "removeObjectsAtArrangedObjectIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeObject:@
removeObject :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
removeObject nsArrayController  object =
    sendMsg nsArrayController (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeObjects:@
removeObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO ()
removeObjects nsArrayController  objects =
  withObjCPtr objects $ \raw_objects ->
      sendMsg nsArrayController (mkSelector "removeObjects:") retVoid [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- automaticallyRearrangesObjects@
automaticallyRearrangesObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
automaticallyRearrangesObjects nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "automaticallyRearrangesObjects") retCULong []

-- | @- setAutomaticallyRearrangesObjects:@
setAutomaticallyRearrangesObjects :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setAutomaticallyRearrangesObjects nsArrayController  value =
    sendMsg nsArrayController (mkSelector "setAutomaticallyRearrangesObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticRearrangementKeyPaths@
automaticRearrangementKeyPaths :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSArray)
automaticRearrangementKeyPaths nsArrayController  =
    sendMsg nsArrayController (mkSelector "automaticRearrangementKeyPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sortDescriptors@
sortDescriptors :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSArray)
sortDescriptors nsArrayController  =
    sendMsg nsArrayController (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSArrayController nsArrayController, IsNSArray value) => nsArrayController -> value -> IO ()
setSortDescriptors nsArrayController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsArrayController (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- filterPredicate@
filterPredicate :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSPredicate)
filterPredicate nsArrayController  =
    sendMsg nsArrayController (mkSelector "filterPredicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilterPredicate:@
setFilterPredicate :: (IsNSArrayController nsArrayController, IsNSPredicate value) => nsArrayController -> value -> IO ()
setFilterPredicate nsArrayController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsArrayController (mkSelector "setFilterPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clearsFilterPredicateOnInsertion@
clearsFilterPredicateOnInsertion :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
clearsFilterPredicateOnInsertion nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "clearsFilterPredicateOnInsertion") retCULong []

-- | @- setClearsFilterPredicateOnInsertion:@
setClearsFilterPredicateOnInsertion :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setClearsFilterPredicateOnInsertion nsArrayController  value =
    sendMsg nsArrayController (mkSelector "setClearsFilterPredicateOnInsertion:") retVoid [argCULong (if value then 1 else 0)]

-- | @- arrangedObjects@
arrangedObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO RawId
arrangedObjects nsArrayController  =
    fmap (RawId . castPtr) $ sendMsg nsArrayController (mkSelector "arrangedObjects") (retPtr retVoid) []

-- | @- avoidsEmptySelection@
avoidsEmptySelection :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
avoidsEmptySelection nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "avoidsEmptySelection") retCULong []

-- | @- setAvoidsEmptySelection:@
setAvoidsEmptySelection :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setAvoidsEmptySelection nsArrayController  value =
    sendMsg nsArrayController (mkSelector "setAvoidsEmptySelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preservesSelection@
preservesSelection :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
preservesSelection nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "preservesSelection") retCULong []

-- | @- setPreservesSelection:@
setPreservesSelection :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setPreservesSelection nsArrayController  value =
    sendMsg nsArrayController (mkSelector "setPreservesSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectsInsertedObjects@
selectsInsertedObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
selectsInsertedObjects nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "selectsInsertedObjects") retCULong []

-- | @- setSelectsInsertedObjects:@
setSelectsInsertedObjects :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setSelectsInsertedObjects nsArrayController  value =
    sendMsg nsArrayController (mkSelector "setSelectsInsertedObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarker :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
alwaysUsesMultipleValuesMarker nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "alwaysUsesMultipleValuesMarker") retCULong []

-- | @- setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarker :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setAlwaysUsesMultipleValuesMarker nsArrayController  value =
    sendMsg nsArrayController (mkSelector "setAlwaysUsesMultipleValuesMarker:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectionIndexes@
selectionIndexes :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSIndexSet)
selectionIndexes nsArrayController  =
    sendMsg nsArrayController (mkSelector "selectionIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectionIndex@
selectionIndex :: IsNSArrayController nsArrayController => nsArrayController -> IO CULong
selectionIndex nsArrayController  =
    sendMsg nsArrayController (mkSelector "selectionIndex") retCULong []

-- | @- selectedObjects@
selectedObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSArray)
selectedObjects nsArrayController  =
    sendMsg nsArrayController (mkSelector "selectedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- canInsert@
canInsert :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
canInsert nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "canInsert") retCULong []

-- | @- canSelectNext@
canSelectNext :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
canSelectNext nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "canSelectNext") retCULong []

-- | @- canSelectPrevious@
canSelectPrevious :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
canSelectPrevious nsArrayController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArrayController (mkSelector "canSelectPrevious") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rearrangeObjects@
rearrangeObjectsSelector :: Selector
rearrangeObjectsSelector = mkSelector "rearrangeObjects"

-- | @Selector@ for @didChangeArrangementCriteria@
didChangeArrangementCriteriaSelector :: Selector
didChangeArrangementCriteriaSelector = mkSelector "didChangeArrangementCriteria"

-- | @Selector@ for @arrangeObjects:@
arrangeObjectsSelector :: Selector
arrangeObjectsSelector = mkSelector "arrangeObjects:"

-- | @Selector@ for @setSelectionIndexes:@
setSelectionIndexesSelector :: Selector
setSelectionIndexesSelector = mkSelector "setSelectionIndexes:"

-- | @Selector@ for @setSelectionIndex:@
setSelectionIndexSelector :: Selector
setSelectionIndexSelector = mkSelector "setSelectionIndex:"

-- | @Selector@ for @addSelectionIndexes:@
addSelectionIndexesSelector :: Selector
addSelectionIndexesSelector = mkSelector "addSelectionIndexes:"

-- | @Selector@ for @removeSelectionIndexes:@
removeSelectionIndexesSelector :: Selector
removeSelectionIndexesSelector = mkSelector "removeSelectionIndexes:"

-- | @Selector@ for @setSelectedObjects:@
setSelectedObjectsSelector :: Selector
setSelectedObjectsSelector = mkSelector "setSelectedObjects:"

-- | @Selector@ for @addSelectedObjects:@
addSelectedObjectsSelector :: Selector
addSelectedObjectsSelector = mkSelector "addSelectedObjects:"

-- | @Selector@ for @removeSelectedObjects:@
removeSelectedObjectsSelector :: Selector
removeSelectedObjectsSelector = mkSelector "removeSelectedObjects:"

-- | @Selector@ for @add:@
addSelector :: Selector
addSelector = mkSelector "add:"

-- | @Selector@ for @remove:@
removeSelector :: Selector
removeSelector = mkSelector "remove:"

-- | @Selector@ for @insert:@
insertSelector :: Selector
insertSelector = mkSelector "insert:"

-- | @Selector@ for @selectNext:@
selectNextSelector :: Selector
selectNextSelector = mkSelector "selectNext:"

-- | @Selector@ for @selectPrevious:@
selectPreviousSelector :: Selector
selectPreviousSelector = mkSelector "selectPrevious:"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @addObjects:@
addObjectsSelector :: Selector
addObjectsSelector = mkSelector "addObjects:"

-- | @Selector@ for @insertObject:atArrangedObjectIndex:@
insertObject_atArrangedObjectIndexSelector :: Selector
insertObject_atArrangedObjectIndexSelector = mkSelector "insertObject:atArrangedObjectIndex:"

-- | @Selector@ for @insertObjects:atArrangedObjectIndexes:@
insertObjects_atArrangedObjectIndexesSelector :: Selector
insertObjects_atArrangedObjectIndexesSelector = mkSelector "insertObjects:atArrangedObjectIndexes:"

-- | @Selector@ for @removeObjectAtArrangedObjectIndex:@
removeObjectAtArrangedObjectIndexSelector :: Selector
removeObjectAtArrangedObjectIndexSelector = mkSelector "removeObjectAtArrangedObjectIndex:"

-- | @Selector@ for @removeObjectsAtArrangedObjectIndexes:@
removeObjectsAtArrangedObjectIndexesSelector :: Selector
removeObjectsAtArrangedObjectIndexesSelector = mkSelector "removeObjectsAtArrangedObjectIndexes:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeObjects:@
removeObjectsSelector :: Selector
removeObjectsSelector = mkSelector "removeObjects:"

-- | @Selector@ for @automaticallyRearrangesObjects@
automaticallyRearrangesObjectsSelector :: Selector
automaticallyRearrangesObjectsSelector = mkSelector "automaticallyRearrangesObjects"

-- | @Selector@ for @setAutomaticallyRearrangesObjects:@
setAutomaticallyRearrangesObjectsSelector :: Selector
setAutomaticallyRearrangesObjectsSelector = mkSelector "setAutomaticallyRearrangesObjects:"

-- | @Selector@ for @automaticRearrangementKeyPaths@
automaticRearrangementKeyPathsSelector :: Selector
automaticRearrangementKeyPathsSelector = mkSelector "automaticRearrangementKeyPaths"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @filterPredicate@
filterPredicateSelector :: Selector
filterPredicateSelector = mkSelector "filterPredicate"

-- | @Selector@ for @setFilterPredicate:@
setFilterPredicateSelector :: Selector
setFilterPredicateSelector = mkSelector "setFilterPredicate:"

-- | @Selector@ for @clearsFilterPredicateOnInsertion@
clearsFilterPredicateOnInsertionSelector :: Selector
clearsFilterPredicateOnInsertionSelector = mkSelector "clearsFilterPredicateOnInsertion"

-- | @Selector@ for @setClearsFilterPredicateOnInsertion:@
setClearsFilterPredicateOnInsertionSelector :: Selector
setClearsFilterPredicateOnInsertionSelector = mkSelector "setClearsFilterPredicateOnInsertion:"

-- | @Selector@ for @arrangedObjects@
arrangedObjectsSelector :: Selector
arrangedObjectsSelector = mkSelector "arrangedObjects"

-- | @Selector@ for @avoidsEmptySelection@
avoidsEmptySelectionSelector :: Selector
avoidsEmptySelectionSelector = mkSelector "avoidsEmptySelection"

-- | @Selector@ for @setAvoidsEmptySelection:@
setAvoidsEmptySelectionSelector :: Selector
setAvoidsEmptySelectionSelector = mkSelector "setAvoidsEmptySelection:"

-- | @Selector@ for @preservesSelection@
preservesSelectionSelector :: Selector
preservesSelectionSelector = mkSelector "preservesSelection"

-- | @Selector@ for @setPreservesSelection:@
setPreservesSelectionSelector :: Selector
setPreservesSelectionSelector = mkSelector "setPreservesSelection:"

-- | @Selector@ for @selectsInsertedObjects@
selectsInsertedObjectsSelector :: Selector
selectsInsertedObjectsSelector = mkSelector "selectsInsertedObjects"

-- | @Selector@ for @setSelectsInsertedObjects:@
setSelectsInsertedObjectsSelector :: Selector
setSelectsInsertedObjectsSelector = mkSelector "setSelectsInsertedObjects:"

-- | @Selector@ for @alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarkerSelector :: Selector
alwaysUsesMultipleValuesMarkerSelector = mkSelector "alwaysUsesMultipleValuesMarker"

-- | @Selector@ for @setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarkerSelector :: Selector
setAlwaysUsesMultipleValuesMarkerSelector = mkSelector "setAlwaysUsesMultipleValuesMarker:"

-- | @Selector@ for @selectionIndexes@
selectionIndexesSelector :: Selector
selectionIndexesSelector = mkSelector "selectionIndexes"

-- | @Selector@ for @selectionIndex@
selectionIndexSelector :: Selector
selectionIndexSelector = mkSelector "selectionIndex"

-- | @Selector@ for @selectedObjects@
selectedObjectsSelector :: Selector
selectedObjectsSelector = mkSelector "selectedObjects"

-- | @Selector@ for @canInsert@
canInsertSelector :: Selector
canInsertSelector = mkSelector "canInsert"

-- | @Selector@ for @canSelectNext@
canSelectNextSelector :: Selector
canSelectNextSelector = mkSelector "canSelectNext"

-- | @Selector@ for @canSelectPrevious@
canSelectPreviousSelector :: Selector
canSelectPreviousSelector = mkSelector "canSelectPrevious"

