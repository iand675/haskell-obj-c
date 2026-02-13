{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************       Mutable Ordered Set     ***************
--
-- Generated bindings for @NSMutableOrderedSet@.
module ObjC.Foundation.NSMutableOrderedSet
  ( NSMutableOrderedSet
  , IsNSMutableOrderedSet(..)
  , insertObject_atIndex
  , removeObjectAtIndex
  , replaceObjectAtIndex_withObject
  , initWithCoder
  , init_
  , initWithCapacity
  , filterUsingPredicate
  , sortUsingDescriptors
  , applyDifference
  , orderedSetWithCapacity
  , addObject
  , addObjects_count
  , addObjectsFromArray
  , exchangeObjectAtIndex_withObjectAtIndex
  , moveObjectsAtIndexes_toIndex
  , insertObjects_atIndexes
  , setObject_atIndex
  , setObject_atIndexedSubscript
  , replaceObjectsInRange_withObjects_count
  , replaceObjectsAtIndexes_withObjects
  , removeObjectsInRange
  , removeObjectsAtIndexes
  , removeAllObjects
  , removeObject
  , removeObjectsInArray
  , intersectOrderedSet
  , minusOrderedSet
  , unionOrderedSet
  , intersectSet
  , minusSet
  , unionSet
  , sortUsingComparator
  , sortWithOptions_usingComparator
  , sortRange_options_usingComparator
  , addObjectSelector
  , addObjectsFromArraySelector
  , addObjects_countSelector
  , applyDifferenceSelector
  , exchangeObjectAtIndex_withObjectAtIndexSelector
  , filterUsingPredicateSelector
  , initSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , insertObject_atIndexSelector
  , insertObjects_atIndexesSelector
  , intersectOrderedSetSelector
  , intersectSetSelector
  , minusOrderedSetSelector
  , minusSetSelector
  , moveObjectsAtIndexes_toIndexSelector
  , orderedSetWithCapacitySelector
  , removeAllObjectsSelector
  , removeObjectAtIndexSelector
  , removeObjectSelector
  , removeObjectsAtIndexesSelector
  , removeObjectsInArraySelector
  , removeObjectsInRangeSelector
  , replaceObjectAtIndex_withObjectSelector
  , replaceObjectsAtIndexes_withObjectsSelector
  , replaceObjectsInRange_withObjects_countSelector
  , setObject_atIndexSelector
  , setObject_atIndexedSubscriptSelector
  , sortRange_options_usingComparatorSelector
  , sortUsingComparatorSelector
  , sortUsingDescriptorsSelector
  , sortWithOptions_usingComparatorSelector
  , unionOrderedSetSelector
  , unionSetSelector

  -- * Enum types
  , NSSortOptions(NSSortOptions)
  , pattern NSSortConcurrent
  , pattern NSSortStable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- insertObject:atIndex:@
insertObject_atIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
insertObject_atIndex nsMutableOrderedSet object idx =
  sendMessage nsMutableOrderedSet insertObject_atIndexSelector object idx

-- | @- removeObjectAtIndex:@
removeObjectAtIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> IO ()
removeObjectAtIndex nsMutableOrderedSet idx =
  sendMessage nsMutableOrderedSet removeObjectAtIndexSelector idx

-- | @- replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObject :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> RawId -> IO ()
replaceObjectAtIndex_withObject nsMutableOrderedSet idx object =
  sendMessage nsMutableOrderedSet replaceObjectAtIndex_withObjectSelector idx object

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSCoder coder) => nsMutableOrderedSet -> coder -> IO (Id NSMutableOrderedSet)
initWithCoder nsMutableOrderedSet coder =
  sendOwnedMessage nsMutableOrderedSet initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> IO (Id NSMutableOrderedSet)
init_ nsMutableOrderedSet =
  sendOwnedMessage nsMutableOrderedSet initSelector

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> IO (Id NSMutableOrderedSet)
initWithCapacity nsMutableOrderedSet numItems =
  sendOwnedMessage nsMutableOrderedSet initWithCapacitySelector numItems

-- | @- filterUsingPredicate:@
filterUsingPredicate :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSPredicate p) => nsMutableOrderedSet -> p -> IO ()
filterUsingPredicate nsMutableOrderedSet p =
  sendMessage nsMutableOrderedSet filterUsingPredicateSelector (toNSPredicate p)

-- | @- sortUsingDescriptors:@
sortUsingDescriptors :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray sortDescriptors) => nsMutableOrderedSet -> sortDescriptors -> IO ()
sortUsingDescriptors nsMutableOrderedSet sortDescriptors =
  sendMessage nsMutableOrderedSet sortUsingDescriptorsSelector (toNSArray sortDescriptors)

-- | @- applyDifference:@
applyDifference :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedCollectionDifference difference) => nsMutableOrderedSet -> difference -> IO ()
applyDifference nsMutableOrderedSet difference =
  sendMessage nsMutableOrderedSet applyDifferenceSelector (toNSOrderedCollectionDifference difference)

-- | @+ orderedSetWithCapacity:@
orderedSetWithCapacity :: CULong -> IO (Id NSMutableOrderedSet)
orderedSetWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableOrderedSet"
    sendClassMessage cls' orderedSetWithCapacitySelector numItems

-- | @- addObject:@
addObject :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> IO ()
addObject nsMutableOrderedSet object =
  sendMessage nsMutableOrderedSet addObjectSelector object

-- | @- addObjects:count:@
addObjects_count :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
addObjects_count nsMutableOrderedSet objects count =
  sendMessage nsMutableOrderedSet addObjects_countSelector objects count

-- | @- addObjectsFromArray:@
addObjectsFromArray :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray array) => nsMutableOrderedSet -> array -> IO ()
addObjectsFromArray nsMutableOrderedSet array =
  sendMessage nsMutableOrderedSet addObjectsFromArraySelector (toNSArray array)

-- | @- exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> CULong -> IO ()
exchangeObjectAtIndex_withObjectAtIndex nsMutableOrderedSet idx1 idx2 =
  sendMessage nsMutableOrderedSet exchangeObjectAtIndex_withObjectAtIndexSelector idx1 idx2

-- | @- moveObjectsAtIndexes:toIndex:@
moveObjectsAtIndexes_toIndex :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSIndexSet indexes) => nsMutableOrderedSet -> indexes -> CULong -> IO ()
moveObjectsAtIndexes_toIndex nsMutableOrderedSet indexes idx =
  sendMessage nsMutableOrderedSet moveObjectsAtIndexes_toIndexSelector (toNSIndexSet indexes) idx

-- | @- insertObjects:atIndexes:@
insertObjects_atIndexes :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray objects, IsNSIndexSet indexes) => nsMutableOrderedSet -> objects -> indexes -> IO ()
insertObjects_atIndexes nsMutableOrderedSet objects indexes =
  sendMessage nsMutableOrderedSet insertObjects_atIndexesSelector (toNSArray objects) (toNSIndexSet indexes)

-- | @- setObject:atIndex:@
setObject_atIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
setObject_atIndex nsMutableOrderedSet obj_ idx =
  sendMessage nsMutableOrderedSet setObject_atIndexSelector obj_ idx

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
setObject_atIndexedSubscript nsMutableOrderedSet obj_ idx =
  sendMessage nsMutableOrderedSet setObject_atIndexedSubscriptSelector obj_ idx

-- | @- replaceObjectsInRange:withObjects:count:@
replaceObjectsInRange_withObjects_count :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSRange -> RawId -> CULong -> IO ()
replaceObjectsInRange_withObjects_count nsMutableOrderedSet range objects count =
  sendMessage nsMutableOrderedSet replaceObjectsInRange_withObjects_countSelector range objects count

-- | @- replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjects :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSIndexSet indexes, IsNSArray objects) => nsMutableOrderedSet -> indexes -> objects -> IO ()
replaceObjectsAtIndexes_withObjects nsMutableOrderedSet indexes objects =
  sendMessage nsMutableOrderedSet replaceObjectsAtIndexes_withObjectsSelector (toNSIndexSet indexes) (toNSArray objects)

-- | @- removeObjectsInRange:@
removeObjectsInRange :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSRange -> IO ()
removeObjectsInRange nsMutableOrderedSet range =
  sendMessage nsMutableOrderedSet removeObjectsInRangeSelector range

-- | @- removeObjectsAtIndexes:@
removeObjectsAtIndexes :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSIndexSet indexes) => nsMutableOrderedSet -> indexes -> IO ()
removeObjectsAtIndexes nsMutableOrderedSet indexes =
  sendMessage nsMutableOrderedSet removeObjectsAtIndexesSelector (toNSIndexSet indexes)

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> IO ()
removeAllObjects nsMutableOrderedSet =
  sendMessage nsMutableOrderedSet removeAllObjectsSelector

-- | @- removeObject:@
removeObject :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> IO ()
removeObject nsMutableOrderedSet object =
  sendMessage nsMutableOrderedSet removeObjectSelector object

-- | @- removeObjectsInArray:@
removeObjectsInArray :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray array) => nsMutableOrderedSet -> array -> IO ()
removeObjectsInArray nsMutableOrderedSet array =
  sendMessage nsMutableOrderedSet removeObjectsInArraySelector (toNSArray array)

-- | @- intersectOrderedSet:@
intersectOrderedSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedSet other) => nsMutableOrderedSet -> other -> IO ()
intersectOrderedSet nsMutableOrderedSet other =
  sendMessage nsMutableOrderedSet intersectOrderedSetSelector (toNSOrderedSet other)

-- | @- minusOrderedSet:@
minusOrderedSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedSet other) => nsMutableOrderedSet -> other -> IO ()
minusOrderedSet nsMutableOrderedSet other =
  sendMessage nsMutableOrderedSet minusOrderedSetSelector (toNSOrderedSet other)

-- | @- unionOrderedSet:@
unionOrderedSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedSet other) => nsMutableOrderedSet -> other -> IO ()
unionOrderedSet nsMutableOrderedSet other =
  sendMessage nsMutableOrderedSet unionOrderedSetSelector (toNSOrderedSet other)

-- | @- intersectSet:@
intersectSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSSet other) => nsMutableOrderedSet -> other -> IO ()
intersectSet nsMutableOrderedSet other =
  sendMessage nsMutableOrderedSet intersectSetSelector (toNSSet other)

-- | @- minusSet:@
minusSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSSet other) => nsMutableOrderedSet -> other -> IO ()
minusSet nsMutableOrderedSet other =
  sendMessage nsMutableOrderedSet minusSetSelector (toNSSet other)

-- | @- unionSet:@
unionSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSSet other) => nsMutableOrderedSet -> other -> IO ()
unionSet nsMutableOrderedSet other =
  sendMessage nsMutableOrderedSet unionSetSelector (toNSSet other)

-- | @- sortUsingComparator:@
sortUsingComparator :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> Ptr () -> IO ()
sortUsingComparator nsMutableOrderedSet cmptr =
  sendMessage nsMutableOrderedSet sortUsingComparatorSelector cmptr

-- | @- sortWithOptions:usingComparator:@
sortWithOptions_usingComparator :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSSortOptions -> Ptr () -> IO ()
sortWithOptions_usingComparator nsMutableOrderedSet opts cmptr =
  sendMessage nsMutableOrderedSet sortWithOptions_usingComparatorSelector opts cmptr

-- | @- sortRange:options:usingComparator:@
sortRange_options_usingComparator :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSRange -> NSSortOptions -> Ptr () -> IO ()
sortRange_options_usingComparator nsMutableOrderedSet range opts cmptr =
  sendMessage nsMutableOrderedSet sortRange_options_usingComparatorSelector range opts cmptr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertObject:atIndex:@
insertObject_atIndexSelector :: Selector '[RawId, CULong] ()
insertObject_atIndexSelector = mkSelector "insertObject:atIndex:"

-- | @Selector@ for @removeObjectAtIndex:@
removeObjectAtIndexSelector :: Selector '[CULong] ()
removeObjectAtIndexSelector = mkSelector "removeObjectAtIndex:"

-- | @Selector@ for @replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObjectSelector :: Selector '[CULong, RawId] ()
replaceObjectAtIndex_withObjectSelector = mkSelector "replaceObjectAtIndex:withObject:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMutableOrderedSet)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMutableOrderedSet)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSMutableOrderedSet)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @filterUsingPredicate:@
filterUsingPredicateSelector :: Selector '[Id NSPredicate] ()
filterUsingPredicateSelector = mkSelector "filterUsingPredicate:"

-- | @Selector@ for @sortUsingDescriptors:@
sortUsingDescriptorsSelector :: Selector '[Id NSArray] ()
sortUsingDescriptorsSelector = mkSelector "sortUsingDescriptors:"

-- | @Selector@ for @applyDifference:@
applyDifferenceSelector :: Selector '[Id NSOrderedCollectionDifference] ()
applyDifferenceSelector = mkSelector "applyDifference:"

-- | @Selector@ for @orderedSetWithCapacity:@
orderedSetWithCapacitySelector :: Selector '[CULong] (Id NSMutableOrderedSet)
orderedSetWithCapacitySelector = mkSelector "orderedSetWithCapacity:"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @addObjects:count:@
addObjects_countSelector :: Selector '[RawId, CULong] ()
addObjects_countSelector = mkSelector "addObjects:count:"

-- | @Selector@ for @addObjectsFromArray:@
addObjectsFromArraySelector :: Selector '[Id NSArray] ()
addObjectsFromArraySelector = mkSelector "addObjectsFromArray:"

-- | @Selector@ for @exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndexSelector :: Selector '[CULong, CULong] ()
exchangeObjectAtIndex_withObjectAtIndexSelector = mkSelector "exchangeObjectAtIndex:withObjectAtIndex:"

-- | @Selector@ for @moveObjectsAtIndexes:toIndex:@
moveObjectsAtIndexes_toIndexSelector :: Selector '[Id NSIndexSet, CULong] ()
moveObjectsAtIndexes_toIndexSelector = mkSelector "moveObjectsAtIndexes:toIndex:"

-- | @Selector@ for @insertObjects:atIndexes:@
insertObjects_atIndexesSelector :: Selector '[Id NSArray, Id NSIndexSet] ()
insertObjects_atIndexesSelector = mkSelector "insertObjects:atIndexes:"

-- | @Selector@ for @setObject:atIndex:@
setObject_atIndexSelector :: Selector '[RawId, CULong] ()
setObject_atIndexSelector = mkSelector "setObject:atIndex:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[RawId, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @replaceObjectsInRange:withObjects:count:@
replaceObjectsInRange_withObjects_countSelector :: Selector '[NSRange, RawId, CULong] ()
replaceObjectsInRange_withObjects_countSelector = mkSelector "replaceObjectsInRange:withObjects:count:"

-- | @Selector@ for @replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjectsSelector :: Selector '[Id NSIndexSet, Id NSArray] ()
replaceObjectsAtIndexes_withObjectsSelector = mkSelector "replaceObjectsAtIndexes:withObjects:"

-- | @Selector@ for @removeObjectsInRange:@
removeObjectsInRangeSelector :: Selector '[NSRange] ()
removeObjectsInRangeSelector = mkSelector "removeObjectsInRange:"

-- | @Selector@ for @removeObjectsAtIndexes:@
removeObjectsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
removeObjectsAtIndexesSelector = mkSelector "removeObjectsAtIndexes:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeObjectsInArray:@
removeObjectsInArraySelector :: Selector '[Id NSArray] ()
removeObjectsInArraySelector = mkSelector "removeObjectsInArray:"

-- | @Selector@ for @intersectOrderedSet:@
intersectOrderedSetSelector :: Selector '[Id NSOrderedSet] ()
intersectOrderedSetSelector = mkSelector "intersectOrderedSet:"

-- | @Selector@ for @minusOrderedSet:@
minusOrderedSetSelector :: Selector '[Id NSOrderedSet] ()
minusOrderedSetSelector = mkSelector "minusOrderedSet:"

-- | @Selector@ for @unionOrderedSet:@
unionOrderedSetSelector :: Selector '[Id NSOrderedSet] ()
unionOrderedSetSelector = mkSelector "unionOrderedSet:"

-- | @Selector@ for @intersectSet:@
intersectSetSelector :: Selector '[Id NSSet] ()
intersectSetSelector = mkSelector "intersectSet:"

-- | @Selector@ for @minusSet:@
minusSetSelector :: Selector '[Id NSSet] ()
minusSetSelector = mkSelector "minusSet:"

-- | @Selector@ for @unionSet:@
unionSetSelector :: Selector '[Id NSSet] ()
unionSetSelector = mkSelector "unionSet:"

-- | @Selector@ for @sortUsingComparator:@
sortUsingComparatorSelector :: Selector '[Ptr ()] ()
sortUsingComparatorSelector = mkSelector "sortUsingComparator:"

-- | @Selector@ for @sortWithOptions:usingComparator:@
sortWithOptions_usingComparatorSelector :: Selector '[NSSortOptions, Ptr ()] ()
sortWithOptions_usingComparatorSelector = mkSelector "sortWithOptions:usingComparator:"

-- | @Selector@ for @sortRange:options:usingComparator:@
sortRange_options_usingComparatorSelector :: Selector '[NSRange, NSSortOptions, Ptr ()] ()
sortRange_options_usingComparatorSelector = mkSelector "sortRange:options:usingComparator:"

