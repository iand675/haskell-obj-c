{-# LANGUAGE PatternSynonyms #-}
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
  , insertObject_atIndexSelector
  , removeObjectAtIndexSelector
  , replaceObjectAtIndex_withObjectSelector
  , initWithCoderSelector
  , initSelector
  , initWithCapacitySelector
  , filterUsingPredicateSelector
  , sortUsingDescriptorsSelector
  , applyDifferenceSelector
  , orderedSetWithCapacitySelector
  , addObjectSelector
  , addObjects_countSelector
  , addObjectsFromArraySelector
  , exchangeObjectAtIndex_withObjectAtIndexSelector
  , moveObjectsAtIndexes_toIndexSelector
  , insertObjects_atIndexesSelector
  , setObject_atIndexSelector
  , setObject_atIndexedSubscriptSelector
  , replaceObjectsInRange_withObjects_countSelector
  , replaceObjectsAtIndexes_withObjectsSelector
  , removeObjectsInRangeSelector
  , removeObjectsAtIndexesSelector
  , removeAllObjectsSelector
  , removeObjectSelector
  , removeObjectsInArraySelector
  , intersectOrderedSetSelector
  , minusOrderedSetSelector
  , unionOrderedSetSelector
  , intersectSetSelector
  , minusSetSelector
  , unionSetSelector
  , sortUsingComparatorSelector
  , sortWithOptions_usingComparatorSelector
  , sortRange_options_usingComparatorSelector

  -- * Enum types
  , NSSortOptions(NSSortOptions)
  , pattern NSSortConcurrent
  , pattern NSSortStable

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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- insertObject:atIndex:@
insertObject_atIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
insertObject_atIndex nsMutableOrderedSet  object idx =
  sendMsg nsMutableOrderedSet (mkSelector "insertObject:atIndex:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argCULong (fromIntegral idx)]

-- | @- removeObjectAtIndex:@
removeObjectAtIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> IO ()
removeObjectAtIndex nsMutableOrderedSet  idx =
  sendMsg nsMutableOrderedSet (mkSelector "removeObjectAtIndex:") retVoid [argCULong (fromIntegral idx)]

-- | @- replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObject :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> RawId -> IO ()
replaceObjectAtIndex_withObject nsMutableOrderedSet  idx object =
  sendMsg nsMutableOrderedSet (mkSelector "replaceObjectAtIndex:withObject:") retVoid [argCULong (fromIntegral idx), argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSCoder coder) => nsMutableOrderedSet -> coder -> IO (Id NSMutableOrderedSet)
initWithCoder nsMutableOrderedSet  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsMutableOrderedSet (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> IO (Id NSMutableOrderedSet)
init_ nsMutableOrderedSet  =
  sendMsg nsMutableOrderedSet (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> IO (Id NSMutableOrderedSet)
initWithCapacity nsMutableOrderedSet  numItems =
  sendMsg nsMutableOrderedSet (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= ownedObject . castPtr

-- | @- filterUsingPredicate:@
filterUsingPredicate :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSPredicate p) => nsMutableOrderedSet -> p -> IO ()
filterUsingPredicate nsMutableOrderedSet  p =
withObjCPtr p $ \raw_p ->
    sendMsg nsMutableOrderedSet (mkSelector "filterUsingPredicate:") retVoid [argPtr (castPtr raw_p :: Ptr ())]

-- | @- sortUsingDescriptors:@
sortUsingDescriptors :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray sortDescriptors) => nsMutableOrderedSet -> sortDescriptors -> IO ()
sortUsingDescriptors nsMutableOrderedSet  sortDescriptors =
withObjCPtr sortDescriptors $ \raw_sortDescriptors ->
    sendMsg nsMutableOrderedSet (mkSelector "sortUsingDescriptors:") retVoid [argPtr (castPtr raw_sortDescriptors :: Ptr ())]

-- | @- applyDifference:@
applyDifference :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedCollectionDifference difference) => nsMutableOrderedSet -> difference -> IO ()
applyDifference nsMutableOrderedSet  difference =
withObjCPtr difference $ \raw_difference ->
    sendMsg nsMutableOrderedSet (mkSelector "applyDifference:") retVoid [argPtr (castPtr raw_difference :: Ptr ())]

-- | @+ orderedSetWithCapacity:@
orderedSetWithCapacity :: CULong -> IO (Id NSMutableOrderedSet)
orderedSetWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableOrderedSet"
    sendClassMsg cls' (mkSelector "orderedSetWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= retainedObject . castPtr

-- | @- addObject:@
addObject :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> IO ()
addObject nsMutableOrderedSet  object =
  sendMsg nsMutableOrderedSet (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- addObjects:count:@
addObjects_count :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
addObjects_count nsMutableOrderedSet  objects count =
  sendMsg nsMutableOrderedSet (mkSelector "addObjects:count:") retVoid [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral count)]

-- | @- addObjectsFromArray:@
addObjectsFromArray :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray array) => nsMutableOrderedSet -> array -> IO ()
addObjectsFromArray nsMutableOrderedSet  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsMutableOrderedSet (mkSelector "addObjectsFromArray:") retVoid [argPtr (castPtr raw_array :: Ptr ())]

-- | @- exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> CULong -> CULong -> IO ()
exchangeObjectAtIndex_withObjectAtIndex nsMutableOrderedSet  idx1 idx2 =
  sendMsg nsMutableOrderedSet (mkSelector "exchangeObjectAtIndex:withObjectAtIndex:") retVoid [argCULong (fromIntegral idx1), argCULong (fromIntegral idx2)]

-- | @- moveObjectsAtIndexes:toIndex:@
moveObjectsAtIndexes_toIndex :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSIndexSet indexes) => nsMutableOrderedSet -> indexes -> CULong -> IO ()
moveObjectsAtIndexes_toIndex nsMutableOrderedSet  indexes idx =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsMutableOrderedSet (mkSelector "moveObjectsAtIndexes:toIndex:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (fromIntegral idx)]

-- | @- insertObjects:atIndexes:@
insertObjects_atIndexes :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray objects, IsNSIndexSet indexes) => nsMutableOrderedSet -> objects -> indexes -> IO ()
insertObjects_atIndexes nsMutableOrderedSet  objects indexes =
withObjCPtr objects $ \raw_objects ->
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsMutableOrderedSet (mkSelector "insertObjects:atIndexes:") retVoid [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- setObject:atIndex:@
setObject_atIndex :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
setObject_atIndex nsMutableOrderedSet  obj_ idx =
  sendMsg nsMutableOrderedSet (mkSelector "setObject:atIndex:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argCULong (fromIntegral idx)]

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> CULong -> IO ()
setObject_atIndexedSubscript nsMutableOrderedSet  obj_ idx =
  sendMsg nsMutableOrderedSet (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argCULong (fromIntegral idx)]

-- | @- replaceObjectsInRange:withObjects:count:@
replaceObjectsInRange_withObjects_count :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSRange -> RawId -> CULong -> IO ()
replaceObjectsInRange_withObjects_count nsMutableOrderedSet  range objects count =
  sendMsg nsMutableOrderedSet (mkSelector "replaceObjectsInRange:withObjects:count:") retVoid [argNSRange range, argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral count)]

-- | @- replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjects :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSIndexSet indexes, IsNSArray objects) => nsMutableOrderedSet -> indexes -> objects -> IO ()
replaceObjectsAtIndexes_withObjects nsMutableOrderedSet  indexes objects =
withObjCPtr indexes $ \raw_indexes ->
  withObjCPtr objects $ \raw_objects ->
      sendMsg nsMutableOrderedSet (mkSelector "replaceObjectsAtIndexes:withObjects:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_objects :: Ptr ())]

-- | @- removeObjectsInRange:@
removeObjectsInRange :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSRange -> IO ()
removeObjectsInRange nsMutableOrderedSet  range =
  sendMsg nsMutableOrderedSet (mkSelector "removeObjectsInRange:") retVoid [argNSRange range]

-- | @- removeObjectsAtIndexes:@
removeObjectsAtIndexes :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSIndexSet indexes) => nsMutableOrderedSet -> indexes -> IO ()
removeObjectsAtIndexes nsMutableOrderedSet  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsMutableOrderedSet (mkSelector "removeObjectsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> IO ()
removeAllObjects nsMutableOrderedSet  =
  sendMsg nsMutableOrderedSet (mkSelector "removeAllObjects") retVoid []

-- | @- removeObject:@
removeObject :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> RawId -> IO ()
removeObject nsMutableOrderedSet  object =
  sendMsg nsMutableOrderedSet (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeObjectsInArray:@
removeObjectsInArray :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSArray array) => nsMutableOrderedSet -> array -> IO ()
removeObjectsInArray nsMutableOrderedSet  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsMutableOrderedSet (mkSelector "removeObjectsInArray:") retVoid [argPtr (castPtr raw_array :: Ptr ())]

-- | @- intersectOrderedSet:@
intersectOrderedSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedSet other) => nsMutableOrderedSet -> other -> IO ()
intersectOrderedSet nsMutableOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsMutableOrderedSet (mkSelector "intersectOrderedSet:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- minusOrderedSet:@
minusOrderedSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedSet other) => nsMutableOrderedSet -> other -> IO ()
minusOrderedSet nsMutableOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsMutableOrderedSet (mkSelector "minusOrderedSet:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- unionOrderedSet:@
unionOrderedSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSOrderedSet other) => nsMutableOrderedSet -> other -> IO ()
unionOrderedSet nsMutableOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsMutableOrderedSet (mkSelector "unionOrderedSet:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- intersectSet:@
intersectSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSSet other) => nsMutableOrderedSet -> other -> IO ()
intersectSet nsMutableOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsMutableOrderedSet (mkSelector "intersectSet:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- minusSet:@
minusSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSSet other) => nsMutableOrderedSet -> other -> IO ()
minusSet nsMutableOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsMutableOrderedSet (mkSelector "minusSet:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- unionSet:@
unionSet :: (IsNSMutableOrderedSet nsMutableOrderedSet, IsNSSet other) => nsMutableOrderedSet -> other -> IO ()
unionSet nsMutableOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsMutableOrderedSet (mkSelector "unionSet:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- sortUsingComparator:@
sortUsingComparator :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> Ptr () -> IO ()
sortUsingComparator nsMutableOrderedSet  cmptr =
  sendMsg nsMutableOrderedSet (mkSelector "sortUsingComparator:") retVoid [argPtr (castPtr cmptr :: Ptr ())]

-- | @- sortWithOptions:usingComparator:@
sortWithOptions_usingComparator :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSSortOptions -> Ptr () -> IO ()
sortWithOptions_usingComparator nsMutableOrderedSet  opts cmptr =
  sendMsg nsMutableOrderedSet (mkSelector "sortWithOptions:usingComparator:") retVoid [argCULong (coerce opts), argPtr (castPtr cmptr :: Ptr ())]

-- | @- sortRange:options:usingComparator:@
sortRange_options_usingComparator :: IsNSMutableOrderedSet nsMutableOrderedSet => nsMutableOrderedSet -> NSRange -> NSSortOptions -> Ptr () -> IO ()
sortRange_options_usingComparator nsMutableOrderedSet  range opts cmptr =
  sendMsg nsMutableOrderedSet (mkSelector "sortRange:options:usingComparator:") retVoid [argNSRange range, argCULong (coerce opts), argPtr (castPtr cmptr :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertObject:atIndex:@
insertObject_atIndexSelector :: Selector
insertObject_atIndexSelector = mkSelector "insertObject:atIndex:"

-- | @Selector@ for @removeObjectAtIndex:@
removeObjectAtIndexSelector :: Selector
removeObjectAtIndexSelector = mkSelector "removeObjectAtIndex:"

-- | @Selector@ for @replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObjectSelector :: Selector
replaceObjectAtIndex_withObjectSelector = mkSelector "replaceObjectAtIndex:withObject:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @filterUsingPredicate:@
filterUsingPredicateSelector :: Selector
filterUsingPredicateSelector = mkSelector "filterUsingPredicate:"

-- | @Selector@ for @sortUsingDescriptors:@
sortUsingDescriptorsSelector :: Selector
sortUsingDescriptorsSelector = mkSelector "sortUsingDescriptors:"

-- | @Selector@ for @applyDifference:@
applyDifferenceSelector :: Selector
applyDifferenceSelector = mkSelector "applyDifference:"

-- | @Selector@ for @orderedSetWithCapacity:@
orderedSetWithCapacitySelector :: Selector
orderedSetWithCapacitySelector = mkSelector "orderedSetWithCapacity:"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @addObjects:count:@
addObjects_countSelector :: Selector
addObjects_countSelector = mkSelector "addObjects:count:"

-- | @Selector@ for @addObjectsFromArray:@
addObjectsFromArraySelector :: Selector
addObjectsFromArraySelector = mkSelector "addObjectsFromArray:"

-- | @Selector@ for @exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndexSelector :: Selector
exchangeObjectAtIndex_withObjectAtIndexSelector = mkSelector "exchangeObjectAtIndex:withObjectAtIndex:"

-- | @Selector@ for @moveObjectsAtIndexes:toIndex:@
moveObjectsAtIndexes_toIndexSelector :: Selector
moveObjectsAtIndexes_toIndexSelector = mkSelector "moveObjectsAtIndexes:toIndex:"

-- | @Selector@ for @insertObjects:atIndexes:@
insertObjects_atIndexesSelector :: Selector
insertObjects_atIndexesSelector = mkSelector "insertObjects:atIndexes:"

-- | @Selector@ for @setObject:atIndex:@
setObject_atIndexSelector :: Selector
setObject_atIndexSelector = mkSelector "setObject:atIndex:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @replaceObjectsInRange:withObjects:count:@
replaceObjectsInRange_withObjects_countSelector :: Selector
replaceObjectsInRange_withObjects_countSelector = mkSelector "replaceObjectsInRange:withObjects:count:"

-- | @Selector@ for @replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjectsSelector :: Selector
replaceObjectsAtIndexes_withObjectsSelector = mkSelector "replaceObjectsAtIndexes:withObjects:"

-- | @Selector@ for @removeObjectsInRange:@
removeObjectsInRangeSelector :: Selector
removeObjectsInRangeSelector = mkSelector "removeObjectsInRange:"

-- | @Selector@ for @removeObjectsAtIndexes:@
removeObjectsAtIndexesSelector :: Selector
removeObjectsAtIndexesSelector = mkSelector "removeObjectsAtIndexes:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeObjectsInArray:@
removeObjectsInArraySelector :: Selector
removeObjectsInArraySelector = mkSelector "removeObjectsInArray:"

-- | @Selector@ for @intersectOrderedSet:@
intersectOrderedSetSelector :: Selector
intersectOrderedSetSelector = mkSelector "intersectOrderedSet:"

-- | @Selector@ for @minusOrderedSet:@
minusOrderedSetSelector :: Selector
minusOrderedSetSelector = mkSelector "minusOrderedSet:"

-- | @Selector@ for @unionOrderedSet:@
unionOrderedSetSelector :: Selector
unionOrderedSetSelector = mkSelector "unionOrderedSet:"

-- | @Selector@ for @intersectSet:@
intersectSetSelector :: Selector
intersectSetSelector = mkSelector "intersectSet:"

-- | @Selector@ for @minusSet:@
minusSetSelector :: Selector
minusSetSelector = mkSelector "minusSet:"

-- | @Selector@ for @unionSet:@
unionSetSelector :: Selector
unionSetSelector = mkSelector "unionSet:"

-- | @Selector@ for @sortUsingComparator:@
sortUsingComparatorSelector :: Selector
sortUsingComparatorSelector = mkSelector "sortUsingComparator:"

-- | @Selector@ for @sortWithOptions:usingComparator:@
sortWithOptions_usingComparatorSelector :: Selector
sortWithOptions_usingComparatorSelector = mkSelector "sortWithOptions:usingComparator:"

-- | @Selector@ for @sortRange:options:usingComparator:@
sortRange_options_usingComparatorSelector :: Selector
sortRange_options_usingComparatorSelector = mkSelector "sortRange:options:usingComparator:"

