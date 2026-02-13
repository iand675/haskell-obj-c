{-# LANGUAGE DataKinds #-}
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
  , addObjectSelector
  , addObjectsSelector
  , addSelectedObjectsSelector
  , addSelectionIndexesSelector
  , addSelector
  , alwaysUsesMultipleValuesMarkerSelector
  , arrangeObjectsSelector
  , arrangedObjectsSelector
  , automaticRearrangementKeyPathsSelector
  , automaticallyRearrangesObjectsSelector
  , avoidsEmptySelectionSelector
  , canInsertSelector
  , canSelectNextSelector
  , canSelectPreviousSelector
  , clearsFilterPredicateOnInsertionSelector
  , didChangeArrangementCriteriaSelector
  , filterPredicateSelector
  , insertObject_atArrangedObjectIndexSelector
  , insertObjects_atArrangedObjectIndexesSelector
  , insertSelector
  , preservesSelectionSelector
  , rearrangeObjectsSelector
  , removeObjectAtArrangedObjectIndexSelector
  , removeObjectSelector
  , removeObjectsAtArrangedObjectIndexesSelector
  , removeObjectsSelector
  , removeSelectedObjectsSelector
  , removeSelectionIndexesSelector
  , removeSelector
  , selectNextSelector
  , selectPreviousSelector
  , selectedObjectsSelector
  , selectionIndexSelector
  , selectionIndexesSelector
  , selectsInsertedObjectsSelector
  , setAlwaysUsesMultipleValuesMarkerSelector
  , setAutomaticallyRearrangesObjectsSelector
  , setAvoidsEmptySelectionSelector
  , setClearsFilterPredicateOnInsertionSelector
  , setFilterPredicateSelector
  , setPreservesSelectionSelector
  , setSelectedObjectsSelector
  , setSelectionIndexSelector
  , setSelectionIndexesSelector
  , setSelectsInsertedObjectsSelector
  , setSortDescriptorsSelector
  , sortDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rearrangeObjects@
rearrangeObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO ()
rearrangeObjects nsArrayController =
  sendMessage nsArrayController rearrangeObjectsSelector

-- | @- didChangeArrangementCriteria@
didChangeArrangementCriteria :: IsNSArrayController nsArrayController => nsArrayController -> IO ()
didChangeArrangementCriteria nsArrayController =
  sendMessage nsArrayController didChangeArrangementCriteriaSelector

-- | @- arrangeObjects:@
arrangeObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO (Id NSArray)
arrangeObjects nsArrayController objects =
  sendMessage nsArrayController arrangeObjectsSelector (toNSArray objects)

-- | @- setSelectionIndexes:@
setSelectionIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO Bool
setSelectionIndexes nsArrayController indexes =
  sendMessage nsArrayController setSelectionIndexesSelector (toNSIndexSet indexes)

-- | @- setSelectionIndex:@
setSelectionIndex :: IsNSArrayController nsArrayController => nsArrayController -> CULong -> IO Bool
setSelectionIndex nsArrayController index =
  sendMessage nsArrayController setSelectionIndexSelector index

-- | @- addSelectionIndexes:@
addSelectionIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO Bool
addSelectionIndexes nsArrayController indexes =
  sendMessage nsArrayController addSelectionIndexesSelector (toNSIndexSet indexes)

-- | @- removeSelectionIndexes:@
removeSelectionIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO Bool
removeSelectionIndexes nsArrayController indexes =
  sendMessage nsArrayController removeSelectionIndexesSelector (toNSIndexSet indexes)

-- | @- setSelectedObjects:@
setSelectedObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO Bool
setSelectedObjects nsArrayController objects =
  sendMessage nsArrayController setSelectedObjectsSelector (toNSArray objects)

-- | @- addSelectedObjects:@
addSelectedObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO Bool
addSelectedObjects nsArrayController objects =
  sendMessage nsArrayController addSelectedObjectsSelector (toNSArray objects)

-- | @- removeSelectedObjects:@
removeSelectedObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO Bool
removeSelectedObjects nsArrayController objects =
  sendMessage nsArrayController removeSelectedObjectsSelector (toNSArray objects)

-- | @- add:@
add :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
add nsArrayController sender =
  sendMessage nsArrayController addSelector sender

-- | @- remove:@
remove :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
remove nsArrayController sender =
  sendMessage nsArrayController removeSelector sender

-- | @- insert:@
insert :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
insert nsArrayController sender =
  sendMessage nsArrayController insertSelector sender

-- | @- selectNext:@
selectNext :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
selectNext nsArrayController sender =
  sendMessage nsArrayController selectNextSelector sender

-- | @- selectPrevious:@
selectPrevious :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
selectPrevious nsArrayController sender =
  sendMessage nsArrayController selectPreviousSelector sender

-- | @- addObject:@
addObject :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
addObject nsArrayController object =
  sendMessage nsArrayController addObjectSelector object

-- | @- addObjects:@
addObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO ()
addObjects nsArrayController objects =
  sendMessage nsArrayController addObjectsSelector (toNSArray objects)

-- | @- insertObject:atArrangedObjectIndex:@
insertObject_atArrangedObjectIndex :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> CULong -> IO ()
insertObject_atArrangedObjectIndex nsArrayController object index =
  sendMessage nsArrayController insertObject_atArrangedObjectIndexSelector object index

-- | @- insertObjects:atArrangedObjectIndexes:@
insertObjects_atArrangedObjectIndexes :: (IsNSArrayController nsArrayController, IsNSArray objects, IsNSIndexSet indexes) => nsArrayController -> objects -> indexes -> IO ()
insertObjects_atArrangedObjectIndexes nsArrayController objects indexes =
  sendMessage nsArrayController insertObjects_atArrangedObjectIndexesSelector (toNSArray objects) (toNSIndexSet indexes)

-- | @- removeObjectAtArrangedObjectIndex:@
removeObjectAtArrangedObjectIndex :: IsNSArrayController nsArrayController => nsArrayController -> CULong -> IO ()
removeObjectAtArrangedObjectIndex nsArrayController index =
  sendMessage nsArrayController removeObjectAtArrangedObjectIndexSelector index

-- | @- removeObjectsAtArrangedObjectIndexes:@
removeObjectsAtArrangedObjectIndexes :: (IsNSArrayController nsArrayController, IsNSIndexSet indexes) => nsArrayController -> indexes -> IO ()
removeObjectsAtArrangedObjectIndexes nsArrayController indexes =
  sendMessage nsArrayController removeObjectsAtArrangedObjectIndexesSelector (toNSIndexSet indexes)

-- | @- removeObject:@
removeObject :: IsNSArrayController nsArrayController => nsArrayController -> RawId -> IO ()
removeObject nsArrayController object =
  sendMessage nsArrayController removeObjectSelector object

-- | @- removeObjects:@
removeObjects :: (IsNSArrayController nsArrayController, IsNSArray objects) => nsArrayController -> objects -> IO ()
removeObjects nsArrayController objects =
  sendMessage nsArrayController removeObjectsSelector (toNSArray objects)

-- | @- automaticallyRearrangesObjects@
automaticallyRearrangesObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
automaticallyRearrangesObjects nsArrayController =
  sendMessage nsArrayController automaticallyRearrangesObjectsSelector

-- | @- setAutomaticallyRearrangesObjects:@
setAutomaticallyRearrangesObjects :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setAutomaticallyRearrangesObjects nsArrayController value =
  sendMessage nsArrayController setAutomaticallyRearrangesObjectsSelector value

-- | @- automaticRearrangementKeyPaths@
automaticRearrangementKeyPaths :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSArray)
automaticRearrangementKeyPaths nsArrayController =
  sendMessage nsArrayController automaticRearrangementKeyPathsSelector

-- | @- sortDescriptors@
sortDescriptors :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSArray)
sortDescriptors nsArrayController =
  sendMessage nsArrayController sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSArrayController nsArrayController, IsNSArray value) => nsArrayController -> value -> IO ()
setSortDescriptors nsArrayController value =
  sendMessage nsArrayController setSortDescriptorsSelector (toNSArray value)

-- | @- filterPredicate@
filterPredicate :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSPredicate)
filterPredicate nsArrayController =
  sendMessage nsArrayController filterPredicateSelector

-- | @- setFilterPredicate:@
setFilterPredicate :: (IsNSArrayController nsArrayController, IsNSPredicate value) => nsArrayController -> value -> IO ()
setFilterPredicate nsArrayController value =
  sendMessage nsArrayController setFilterPredicateSelector (toNSPredicate value)

-- | @- clearsFilterPredicateOnInsertion@
clearsFilterPredicateOnInsertion :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
clearsFilterPredicateOnInsertion nsArrayController =
  sendMessage nsArrayController clearsFilterPredicateOnInsertionSelector

-- | @- setClearsFilterPredicateOnInsertion:@
setClearsFilterPredicateOnInsertion :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setClearsFilterPredicateOnInsertion nsArrayController value =
  sendMessage nsArrayController setClearsFilterPredicateOnInsertionSelector value

-- | @- arrangedObjects@
arrangedObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO RawId
arrangedObjects nsArrayController =
  sendMessage nsArrayController arrangedObjectsSelector

-- | @- avoidsEmptySelection@
avoidsEmptySelection :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
avoidsEmptySelection nsArrayController =
  sendMessage nsArrayController avoidsEmptySelectionSelector

-- | @- setAvoidsEmptySelection:@
setAvoidsEmptySelection :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setAvoidsEmptySelection nsArrayController value =
  sendMessage nsArrayController setAvoidsEmptySelectionSelector value

-- | @- preservesSelection@
preservesSelection :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
preservesSelection nsArrayController =
  sendMessage nsArrayController preservesSelectionSelector

-- | @- setPreservesSelection:@
setPreservesSelection :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setPreservesSelection nsArrayController value =
  sendMessage nsArrayController setPreservesSelectionSelector value

-- | @- selectsInsertedObjects@
selectsInsertedObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
selectsInsertedObjects nsArrayController =
  sendMessage nsArrayController selectsInsertedObjectsSelector

-- | @- setSelectsInsertedObjects:@
setSelectsInsertedObjects :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setSelectsInsertedObjects nsArrayController value =
  sendMessage nsArrayController setSelectsInsertedObjectsSelector value

-- | @- alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarker :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
alwaysUsesMultipleValuesMarker nsArrayController =
  sendMessage nsArrayController alwaysUsesMultipleValuesMarkerSelector

-- | @- setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarker :: IsNSArrayController nsArrayController => nsArrayController -> Bool -> IO ()
setAlwaysUsesMultipleValuesMarker nsArrayController value =
  sendMessage nsArrayController setAlwaysUsesMultipleValuesMarkerSelector value

-- | @- selectionIndexes@
selectionIndexes :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSIndexSet)
selectionIndexes nsArrayController =
  sendMessage nsArrayController selectionIndexesSelector

-- | @- selectionIndex@
selectionIndex :: IsNSArrayController nsArrayController => nsArrayController -> IO CULong
selectionIndex nsArrayController =
  sendMessage nsArrayController selectionIndexSelector

-- | @- selectedObjects@
selectedObjects :: IsNSArrayController nsArrayController => nsArrayController -> IO (Id NSArray)
selectedObjects nsArrayController =
  sendMessage nsArrayController selectedObjectsSelector

-- | @- canInsert@
canInsert :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
canInsert nsArrayController =
  sendMessage nsArrayController canInsertSelector

-- | @- canSelectNext@
canSelectNext :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
canSelectNext nsArrayController =
  sendMessage nsArrayController canSelectNextSelector

-- | @- canSelectPrevious@
canSelectPrevious :: IsNSArrayController nsArrayController => nsArrayController -> IO Bool
canSelectPrevious nsArrayController =
  sendMessage nsArrayController canSelectPreviousSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rearrangeObjects@
rearrangeObjectsSelector :: Selector '[] ()
rearrangeObjectsSelector = mkSelector "rearrangeObjects"

-- | @Selector@ for @didChangeArrangementCriteria@
didChangeArrangementCriteriaSelector :: Selector '[] ()
didChangeArrangementCriteriaSelector = mkSelector "didChangeArrangementCriteria"

-- | @Selector@ for @arrangeObjects:@
arrangeObjectsSelector :: Selector '[Id NSArray] (Id NSArray)
arrangeObjectsSelector = mkSelector "arrangeObjects:"

-- | @Selector@ for @setSelectionIndexes:@
setSelectionIndexesSelector :: Selector '[Id NSIndexSet] Bool
setSelectionIndexesSelector = mkSelector "setSelectionIndexes:"

-- | @Selector@ for @setSelectionIndex:@
setSelectionIndexSelector :: Selector '[CULong] Bool
setSelectionIndexSelector = mkSelector "setSelectionIndex:"

-- | @Selector@ for @addSelectionIndexes:@
addSelectionIndexesSelector :: Selector '[Id NSIndexSet] Bool
addSelectionIndexesSelector = mkSelector "addSelectionIndexes:"

-- | @Selector@ for @removeSelectionIndexes:@
removeSelectionIndexesSelector :: Selector '[Id NSIndexSet] Bool
removeSelectionIndexesSelector = mkSelector "removeSelectionIndexes:"

-- | @Selector@ for @setSelectedObjects:@
setSelectedObjectsSelector :: Selector '[Id NSArray] Bool
setSelectedObjectsSelector = mkSelector "setSelectedObjects:"

-- | @Selector@ for @addSelectedObjects:@
addSelectedObjectsSelector :: Selector '[Id NSArray] Bool
addSelectedObjectsSelector = mkSelector "addSelectedObjects:"

-- | @Selector@ for @removeSelectedObjects:@
removeSelectedObjectsSelector :: Selector '[Id NSArray] Bool
removeSelectedObjectsSelector = mkSelector "removeSelectedObjects:"

-- | @Selector@ for @add:@
addSelector :: Selector '[RawId] ()
addSelector = mkSelector "add:"

-- | @Selector@ for @remove:@
removeSelector :: Selector '[RawId] ()
removeSelector = mkSelector "remove:"

-- | @Selector@ for @insert:@
insertSelector :: Selector '[RawId] ()
insertSelector = mkSelector "insert:"

-- | @Selector@ for @selectNext:@
selectNextSelector :: Selector '[RawId] ()
selectNextSelector = mkSelector "selectNext:"

-- | @Selector@ for @selectPrevious:@
selectPreviousSelector :: Selector '[RawId] ()
selectPreviousSelector = mkSelector "selectPrevious:"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @addObjects:@
addObjectsSelector :: Selector '[Id NSArray] ()
addObjectsSelector = mkSelector "addObjects:"

-- | @Selector@ for @insertObject:atArrangedObjectIndex:@
insertObject_atArrangedObjectIndexSelector :: Selector '[RawId, CULong] ()
insertObject_atArrangedObjectIndexSelector = mkSelector "insertObject:atArrangedObjectIndex:"

-- | @Selector@ for @insertObjects:atArrangedObjectIndexes:@
insertObjects_atArrangedObjectIndexesSelector :: Selector '[Id NSArray, Id NSIndexSet] ()
insertObjects_atArrangedObjectIndexesSelector = mkSelector "insertObjects:atArrangedObjectIndexes:"

-- | @Selector@ for @removeObjectAtArrangedObjectIndex:@
removeObjectAtArrangedObjectIndexSelector :: Selector '[CULong] ()
removeObjectAtArrangedObjectIndexSelector = mkSelector "removeObjectAtArrangedObjectIndex:"

-- | @Selector@ for @removeObjectsAtArrangedObjectIndexes:@
removeObjectsAtArrangedObjectIndexesSelector :: Selector '[Id NSIndexSet] ()
removeObjectsAtArrangedObjectIndexesSelector = mkSelector "removeObjectsAtArrangedObjectIndexes:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeObjects:@
removeObjectsSelector :: Selector '[Id NSArray] ()
removeObjectsSelector = mkSelector "removeObjects:"

-- | @Selector@ for @automaticallyRearrangesObjects@
automaticallyRearrangesObjectsSelector :: Selector '[] Bool
automaticallyRearrangesObjectsSelector = mkSelector "automaticallyRearrangesObjects"

-- | @Selector@ for @setAutomaticallyRearrangesObjects:@
setAutomaticallyRearrangesObjectsSelector :: Selector '[Bool] ()
setAutomaticallyRearrangesObjectsSelector = mkSelector "setAutomaticallyRearrangesObjects:"

-- | @Selector@ for @automaticRearrangementKeyPaths@
automaticRearrangementKeyPathsSelector :: Selector '[] (Id NSArray)
automaticRearrangementKeyPathsSelector = mkSelector "automaticRearrangementKeyPaths"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector '[Id NSArray] ()
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @filterPredicate@
filterPredicateSelector :: Selector '[] (Id NSPredicate)
filterPredicateSelector = mkSelector "filterPredicate"

-- | @Selector@ for @setFilterPredicate:@
setFilterPredicateSelector :: Selector '[Id NSPredicate] ()
setFilterPredicateSelector = mkSelector "setFilterPredicate:"

-- | @Selector@ for @clearsFilterPredicateOnInsertion@
clearsFilterPredicateOnInsertionSelector :: Selector '[] Bool
clearsFilterPredicateOnInsertionSelector = mkSelector "clearsFilterPredicateOnInsertion"

-- | @Selector@ for @setClearsFilterPredicateOnInsertion:@
setClearsFilterPredicateOnInsertionSelector :: Selector '[Bool] ()
setClearsFilterPredicateOnInsertionSelector = mkSelector "setClearsFilterPredicateOnInsertion:"

-- | @Selector@ for @arrangedObjects@
arrangedObjectsSelector :: Selector '[] RawId
arrangedObjectsSelector = mkSelector "arrangedObjects"

-- | @Selector@ for @avoidsEmptySelection@
avoidsEmptySelectionSelector :: Selector '[] Bool
avoidsEmptySelectionSelector = mkSelector "avoidsEmptySelection"

-- | @Selector@ for @setAvoidsEmptySelection:@
setAvoidsEmptySelectionSelector :: Selector '[Bool] ()
setAvoidsEmptySelectionSelector = mkSelector "setAvoidsEmptySelection:"

-- | @Selector@ for @preservesSelection@
preservesSelectionSelector :: Selector '[] Bool
preservesSelectionSelector = mkSelector "preservesSelection"

-- | @Selector@ for @setPreservesSelection:@
setPreservesSelectionSelector :: Selector '[Bool] ()
setPreservesSelectionSelector = mkSelector "setPreservesSelection:"

-- | @Selector@ for @selectsInsertedObjects@
selectsInsertedObjectsSelector :: Selector '[] Bool
selectsInsertedObjectsSelector = mkSelector "selectsInsertedObjects"

-- | @Selector@ for @setSelectsInsertedObjects:@
setSelectsInsertedObjectsSelector :: Selector '[Bool] ()
setSelectsInsertedObjectsSelector = mkSelector "setSelectsInsertedObjects:"

-- | @Selector@ for @alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarkerSelector :: Selector '[] Bool
alwaysUsesMultipleValuesMarkerSelector = mkSelector "alwaysUsesMultipleValuesMarker"

-- | @Selector@ for @setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarkerSelector :: Selector '[Bool] ()
setAlwaysUsesMultipleValuesMarkerSelector = mkSelector "setAlwaysUsesMultipleValuesMarker:"

-- | @Selector@ for @selectionIndexes@
selectionIndexesSelector :: Selector '[] (Id NSIndexSet)
selectionIndexesSelector = mkSelector "selectionIndexes"

-- | @Selector@ for @selectionIndex@
selectionIndexSelector :: Selector '[] CULong
selectionIndexSelector = mkSelector "selectionIndex"

-- | @Selector@ for @selectedObjects@
selectedObjectsSelector :: Selector '[] (Id NSArray)
selectedObjectsSelector = mkSelector "selectedObjects"

-- | @Selector@ for @canInsert@
canInsertSelector :: Selector '[] Bool
canInsertSelector = mkSelector "canInsert"

-- | @Selector@ for @canSelectNext@
canSelectNextSelector :: Selector '[] Bool
canSelectNextSelector = mkSelector "canSelectNext"

-- | @Selector@ for @canSelectPrevious@
canSelectPreviousSelector :: Selector '[] Bool
canSelectPreviousSelector = mkSelector "canSelectPrevious"

