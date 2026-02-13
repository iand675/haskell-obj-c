{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Mutable Array		***************
--
-- Generated bindings for @NSMutableArray@.
module ObjC.Foundation.NSMutableArray
  ( NSMutableArray
  , IsNSMutableArray(..)
  , addObject
  , insertObject_atIndex
  , removeLastObject
  , removeObjectAtIndex
  , replaceObjectAtIndex_withObject
  , init_
  , initWithCapacity
  , initWithCoder
  , filterUsingPredicate
  , sortUsingDescriptors
  , applyDifference
  , arrayWithCapacity
  , arrayWithContentsOfFile
  , arrayWithContentsOfURL
  , initWithContentsOfFile
  , initWithContentsOfURL
  , addObjectsFromArray
  , exchangeObjectAtIndex_withObjectAtIndex
  , removeAllObjects
  , removeObject_inRange
  , removeObject
  , removeObjectIdenticalTo_inRange
  , removeObjectIdenticalTo
  , removeObjectsFromIndices_numIndices
  , removeObjectsInArray
  , removeObjectsInRange
  , replaceObjectsInRange_withObjectsFromArray_range
  , replaceObjectsInRange_withObjectsFromArray
  , setArray
  , sortUsingFunction_context
  , sortUsingSelector
  , insertObjects_atIndexes
  , removeObjectsAtIndexes
  , replaceObjectsAtIndexes_withObjects
  , setObject_atIndexedSubscript
  , sortUsingComparator
  , sortWithOptions_usingComparator
  , addObjectSelector
  , addObjectsFromArraySelector
  , applyDifferenceSelector
  , arrayWithCapacitySelector
  , arrayWithContentsOfFileSelector
  , arrayWithContentsOfURLSelector
  , exchangeObjectAtIndex_withObjectAtIndexSelector
  , filterUsingPredicateSelector
  , initSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , insertObject_atIndexSelector
  , insertObjects_atIndexesSelector
  , removeAllObjectsSelector
  , removeLastObjectSelector
  , removeObjectAtIndexSelector
  , removeObjectIdenticalToSelector
  , removeObjectIdenticalTo_inRangeSelector
  , removeObjectSelector
  , removeObject_inRangeSelector
  , removeObjectsAtIndexesSelector
  , removeObjectsFromIndices_numIndicesSelector
  , removeObjectsInArraySelector
  , removeObjectsInRangeSelector
  , replaceObjectAtIndex_withObjectSelector
  , replaceObjectsAtIndexes_withObjectsSelector
  , replaceObjectsInRange_withObjectsFromArraySelector
  , replaceObjectsInRange_withObjectsFromArray_rangeSelector
  , setArraySelector
  , setObject_atIndexedSubscriptSelector
  , sortUsingComparatorSelector
  , sortUsingDescriptorsSelector
  , sortUsingFunction_contextSelector
  , sortUsingSelectorSelector
  , sortWithOptions_usingComparatorSelector

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

-- | @- addObject:@
addObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> IO ()
addObject nsMutableArray anObject =
  sendMessage nsMutableArray addObjectSelector anObject

-- | @- insertObject:atIndex:@
insertObject_atIndex :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> CULong -> IO ()
insertObject_atIndex nsMutableArray anObject index =
  sendMessage nsMutableArray insertObject_atIndexSelector anObject index

-- | @- removeLastObject@
removeLastObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> IO ()
removeLastObject nsMutableArray =
  sendMessage nsMutableArray removeLastObjectSelector

-- | @- removeObjectAtIndex:@
removeObjectAtIndex :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> IO ()
removeObjectAtIndex nsMutableArray index =
  sendMessage nsMutableArray removeObjectAtIndexSelector index

-- | @- replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> RawId -> IO ()
replaceObjectAtIndex_withObject nsMutableArray index anObject =
  sendMessage nsMutableArray replaceObjectAtIndex_withObjectSelector index anObject

-- | @- init@
init_ :: IsNSMutableArray nsMutableArray => nsMutableArray -> IO (Id NSMutableArray)
init_ nsMutableArray =
  sendOwnedMessage nsMutableArray initSelector

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> IO (Id NSMutableArray)
initWithCapacity nsMutableArray numItems =
  sendOwnedMessage nsMutableArray initWithCapacitySelector numItems

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableArray nsMutableArray, IsNSCoder coder) => nsMutableArray -> coder -> IO (Id NSMutableArray)
initWithCoder nsMutableArray coder =
  sendOwnedMessage nsMutableArray initWithCoderSelector (toNSCoder coder)

-- | @- filterUsingPredicate:@
filterUsingPredicate :: (IsNSMutableArray nsMutableArray, IsNSPredicate predicate) => nsMutableArray -> predicate -> IO ()
filterUsingPredicate nsMutableArray predicate =
  sendMessage nsMutableArray filterUsingPredicateSelector (toNSPredicate predicate)

-- | @- sortUsingDescriptors:@
sortUsingDescriptors :: (IsNSMutableArray nsMutableArray, IsNSArray sortDescriptors) => nsMutableArray -> sortDescriptors -> IO ()
sortUsingDescriptors nsMutableArray sortDescriptors =
  sendMessage nsMutableArray sortUsingDescriptorsSelector (toNSArray sortDescriptors)

-- | @- applyDifference:@
applyDifference :: (IsNSMutableArray nsMutableArray, IsNSOrderedCollectionDifference difference) => nsMutableArray -> difference -> IO ()
applyDifference nsMutableArray difference =
  sendMessage nsMutableArray applyDifferenceSelector (toNSOrderedCollectionDifference difference)

-- | @+ arrayWithCapacity:@
arrayWithCapacity :: CULong -> IO (Id NSMutableArray)
arrayWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableArray"
    sendClassMessage cls' arrayWithCapacitySelector numItems

-- | @+ arrayWithContentsOfFile:@
arrayWithContentsOfFile :: IsNSString path => path -> IO (Id NSMutableArray)
arrayWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSMutableArray"
    sendClassMessage cls' arrayWithContentsOfFileSelector (toNSString path)

-- | @+ arrayWithContentsOfURL:@
arrayWithContentsOfURL :: IsNSURL url => url -> IO (Id NSMutableArray)
arrayWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSMutableArray"
    sendClassMessage cls' arrayWithContentsOfURLSelector (toNSURL url)

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSMutableArray nsMutableArray, IsNSString path) => nsMutableArray -> path -> IO (Id NSMutableArray)
initWithContentsOfFile nsMutableArray path =
  sendOwnedMessage nsMutableArray initWithContentsOfFileSelector (toNSString path)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSMutableArray nsMutableArray, IsNSURL url) => nsMutableArray -> url -> IO (Id NSMutableArray)
initWithContentsOfURL nsMutableArray url =
  sendOwnedMessage nsMutableArray initWithContentsOfURLSelector (toNSURL url)

-- | @- addObjectsFromArray:@
addObjectsFromArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> otherArray -> IO ()
addObjectsFromArray nsMutableArray otherArray =
  sendMessage nsMutableArray addObjectsFromArraySelector (toNSArray otherArray)

-- | @- exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndex :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> CULong -> IO ()
exchangeObjectAtIndex_withObjectAtIndex nsMutableArray idx1 idx2 =
  sendMessage nsMutableArray exchangeObjectAtIndex_withObjectAtIndexSelector idx1 idx2

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableArray nsMutableArray => nsMutableArray -> IO ()
removeAllObjects nsMutableArray =
  sendMessage nsMutableArray removeAllObjectsSelector

-- | @- removeObject:inRange:@
removeObject_inRange :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> NSRange -> IO ()
removeObject_inRange nsMutableArray anObject range =
  sendMessage nsMutableArray removeObject_inRangeSelector anObject range

-- | @- removeObject:@
removeObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> IO ()
removeObject nsMutableArray anObject =
  sendMessage nsMutableArray removeObjectSelector anObject

-- | @- removeObjectIdenticalTo:inRange:@
removeObjectIdenticalTo_inRange :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> NSRange -> IO ()
removeObjectIdenticalTo_inRange nsMutableArray anObject range =
  sendMessage nsMutableArray removeObjectIdenticalTo_inRangeSelector anObject range

-- | @- removeObjectIdenticalTo:@
removeObjectIdenticalTo :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> IO ()
removeObjectIdenticalTo nsMutableArray anObject =
  sendMessage nsMutableArray removeObjectIdenticalToSelector anObject

-- | @- removeObjectsFromIndices:numIndices:@
removeObjectsFromIndices_numIndices :: IsNSMutableArray nsMutableArray => nsMutableArray -> Ptr CULong -> CULong -> IO ()
removeObjectsFromIndices_numIndices nsMutableArray indices cnt =
  sendMessage nsMutableArray removeObjectsFromIndices_numIndicesSelector indices cnt

-- | @- removeObjectsInArray:@
removeObjectsInArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> otherArray -> IO ()
removeObjectsInArray nsMutableArray otherArray =
  sendMessage nsMutableArray removeObjectsInArraySelector (toNSArray otherArray)

-- | @- removeObjectsInRange:@
removeObjectsInRange :: IsNSMutableArray nsMutableArray => nsMutableArray -> NSRange -> IO ()
removeObjectsInRange nsMutableArray range =
  sendMessage nsMutableArray removeObjectsInRangeSelector range

-- | @- replaceObjectsInRange:withObjectsFromArray:range:@
replaceObjectsInRange_withObjectsFromArray_range :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> NSRange -> otherArray -> NSRange -> IO ()
replaceObjectsInRange_withObjectsFromArray_range nsMutableArray range otherArray otherRange =
  sendMessage nsMutableArray replaceObjectsInRange_withObjectsFromArray_rangeSelector range (toNSArray otherArray) otherRange

-- | @- replaceObjectsInRange:withObjectsFromArray:@
replaceObjectsInRange_withObjectsFromArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> NSRange -> otherArray -> IO ()
replaceObjectsInRange_withObjectsFromArray nsMutableArray range otherArray =
  sendMessage nsMutableArray replaceObjectsInRange_withObjectsFromArraySelector range (toNSArray otherArray)

-- | @- setArray:@
setArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> otherArray -> IO ()
setArray nsMutableArray otherArray =
  sendMessage nsMutableArray setArraySelector (toNSArray otherArray)

-- | @- sortUsingFunction:context:@
sortUsingFunction_context :: IsNSMutableArray nsMutableArray => nsMutableArray -> Ptr () -> Ptr () -> IO ()
sortUsingFunction_context nsMutableArray compare_ context =
  sendMessage nsMutableArray sortUsingFunction_contextSelector compare_ context

-- | @- sortUsingSelector:@
sortUsingSelector :: IsNSMutableArray nsMutableArray => nsMutableArray -> Sel -> IO ()
sortUsingSelector nsMutableArray comparator =
  sendMessage nsMutableArray sortUsingSelectorSelector comparator

-- | @- insertObjects:atIndexes:@
insertObjects_atIndexes :: (IsNSMutableArray nsMutableArray, IsNSArray objects, IsNSIndexSet indexes) => nsMutableArray -> objects -> indexes -> IO ()
insertObjects_atIndexes nsMutableArray objects indexes =
  sendMessage nsMutableArray insertObjects_atIndexesSelector (toNSArray objects) (toNSIndexSet indexes)

-- | @- removeObjectsAtIndexes:@
removeObjectsAtIndexes :: (IsNSMutableArray nsMutableArray, IsNSIndexSet indexes) => nsMutableArray -> indexes -> IO ()
removeObjectsAtIndexes nsMutableArray indexes =
  sendMessage nsMutableArray removeObjectsAtIndexesSelector (toNSIndexSet indexes)

-- | @- replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjects :: (IsNSMutableArray nsMutableArray, IsNSIndexSet indexes, IsNSArray objects) => nsMutableArray -> indexes -> objects -> IO ()
replaceObjectsAtIndexes_withObjects nsMutableArray indexes objects =
  sendMessage nsMutableArray replaceObjectsAtIndexes_withObjectsSelector (toNSIndexSet indexes) (toNSArray objects)

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> CULong -> IO ()
setObject_atIndexedSubscript nsMutableArray obj_ idx =
  sendMessage nsMutableArray setObject_atIndexedSubscriptSelector obj_ idx

-- | @- sortUsingComparator:@
sortUsingComparator :: IsNSMutableArray nsMutableArray => nsMutableArray -> Ptr () -> IO ()
sortUsingComparator nsMutableArray cmptr =
  sendMessage nsMutableArray sortUsingComparatorSelector cmptr

-- | @- sortWithOptions:usingComparator:@
sortWithOptions_usingComparator :: IsNSMutableArray nsMutableArray => nsMutableArray -> NSSortOptions -> Ptr () -> IO ()
sortWithOptions_usingComparator nsMutableArray opts cmptr =
  sendMessage nsMutableArray sortWithOptions_usingComparatorSelector opts cmptr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @insertObject:atIndex:@
insertObject_atIndexSelector :: Selector '[RawId, CULong] ()
insertObject_atIndexSelector = mkSelector "insertObject:atIndex:"

-- | @Selector@ for @removeLastObject@
removeLastObjectSelector :: Selector '[] ()
removeLastObjectSelector = mkSelector "removeLastObject"

-- | @Selector@ for @removeObjectAtIndex:@
removeObjectAtIndexSelector :: Selector '[CULong] ()
removeObjectAtIndexSelector = mkSelector "removeObjectAtIndex:"

-- | @Selector@ for @replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObjectSelector :: Selector '[CULong, RawId] ()
replaceObjectAtIndex_withObjectSelector = mkSelector "replaceObjectAtIndex:withObject:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMutableArray)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSMutableArray)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMutableArray)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filterUsingPredicate:@
filterUsingPredicateSelector :: Selector '[Id NSPredicate] ()
filterUsingPredicateSelector = mkSelector "filterUsingPredicate:"

-- | @Selector@ for @sortUsingDescriptors:@
sortUsingDescriptorsSelector :: Selector '[Id NSArray] ()
sortUsingDescriptorsSelector = mkSelector "sortUsingDescriptors:"

-- | @Selector@ for @applyDifference:@
applyDifferenceSelector :: Selector '[Id NSOrderedCollectionDifference] ()
applyDifferenceSelector = mkSelector "applyDifference:"

-- | @Selector@ for @arrayWithCapacity:@
arrayWithCapacitySelector :: Selector '[CULong] (Id NSMutableArray)
arrayWithCapacitySelector = mkSelector "arrayWithCapacity:"

-- | @Selector@ for @arrayWithContentsOfFile:@
arrayWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSMutableArray)
arrayWithContentsOfFileSelector = mkSelector "arrayWithContentsOfFile:"

-- | @Selector@ for @arrayWithContentsOfURL:@
arrayWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSMutableArray)
arrayWithContentsOfURLSelector = mkSelector "arrayWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSMutableArray)
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSMutableArray)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @addObjectsFromArray:@
addObjectsFromArraySelector :: Selector '[Id NSArray] ()
addObjectsFromArraySelector = mkSelector "addObjectsFromArray:"

-- | @Selector@ for @exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndexSelector :: Selector '[CULong, CULong] ()
exchangeObjectAtIndex_withObjectAtIndexSelector = mkSelector "exchangeObjectAtIndex:withObjectAtIndex:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @removeObject:inRange:@
removeObject_inRangeSelector :: Selector '[RawId, NSRange] ()
removeObject_inRangeSelector = mkSelector "removeObject:inRange:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeObjectIdenticalTo:inRange:@
removeObjectIdenticalTo_inRangeSelector :: Selector '[RawId, NSRange] ()
removeObjectIdenticalTo_inRangeSelector = mkSelector "removeObjectIdenticalTo:inRange:"

-- | @Selector@ for @removeObjectIdenticalTo:@
removeObjectIdenticalToSelector :: Selector '[RawId] ()
removeObjectIdenticalToSelector = mkSelector "removeObjectIdenticalTo:"

-- | @Selector@ for @removeObjectsFromIndices:numIndices:@
removeObjectsFromIndices_numIndicesSelector :: Selector '[Ptr CULong, CULong] ()
removeObjectsFromIndices_numIndicesSelector = mkSelector "removeObjectsFromIndices:numIndices:"

-- | @Selector@ for @removeObjectsInArray:@
removeObjectsInArraySelector :: Selector '[Id NSArray] ()
removeObjectsInArraySelector = mkSelector "removeObjectsInArray:"

-- | @Selector@ for @removeObjectsInRange:@
removeObjectsInRangeSelector :: Selector '[NSRange] ()
removeObjectsInRangeSelector = mkSelector "removeObjectsInRange:"

-- | @Selector@ for @replaceObjectsInRange:withObjectsFromArray:range:@
replaceObjectsInRange_withObjectsFromArray_rangeSelector :: Selector '[NSRange, Id NSArray, NSRange] ()
replaceObjectsInRange_withObjectsFromArray_rangeSelector = mkSelector "replaceObjectsInRange:withObjectsFromArray:range:"

-- | @Selector@ for @replaceObjectsInRange:withObjectsFromArray:@
replaceObjectsInRange_withObjectsFromArraySelector :: Selector '[NSRange, Id NSArray] ()
replaceObjectsInRange_withObjectsFromArraySelector = mkSelector "replaceObjectsInRange:withObjectsFromArray:"

-- | @Selector@ for @setArray:@
setArraySelector :: Selector '[Id NSArray] ()
setArraySelector = mkSelector "setArray:"

-- | @Selector@ for @sortUsingFunction:context:@
sortUsingFunction_contextSelector :: Selector '[Ptr (), Ptr ()] ()
sortUsingFunction_contextSelector = mkSelector "sortUsingFunction:context:"

-- | @Selector@ for @sortUsingSelector:@
sortUsingSelectorSelector :: Selector '[Sel] ()
sortUsingSelectorSelector = mkSelector "sortUsingSelector:"

-- | @Selector@ for @insertObjects:atIndexes:@
insertObjects_atIndexesSelector :: Selector '[Id NSArray, Id NSIndexSet] ()
insertObjects_atIndexesSelector = mkSelector "insertObjects:atIndexes:"

-- | @Selector@ for @removeObjectsAtIndexes:@
removeObjectsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
removeObjectsAtIndexesSelector = mkSelector "removeObjectsAtIndexes:"

-- | @Selector@ for @replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjectsSelector :: Selector '[Id NSIndexSet, Id NSArray] ()
replaceObjectsAtIndexes_withObjectsSelector = mkSelector "replaceObjectsAtIndexes:withObjects:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[RawId, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @sortUsingComparator:@
sortUsingComparatorSelector :: Selector '[Ptr ()] ()
sortUsingComparatorSelector = mkSelector "sortUsingComparator:"

-- | @Selector@ for @sortWithOptions:usingComparator:@
sortWithOptions_usingComparatorSelector :: Selector '[NSSortOptions, Ptr ()] ()
sortWithOptions_usingComparatorSelector = mkSelector "sortWithOptions:usingComparator:"

