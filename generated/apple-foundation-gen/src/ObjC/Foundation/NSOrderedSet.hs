{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************       Immutable Ordered Set   ***************
--
-- Generated bindings for @NSOrderedSet@.
module ObjC.Foundation.NSOrderedSet
  ( NSOrderedSet
  , IsNSOrderedSet(..)
  , objectAtIndex
  , indexOfObject
  , init_
  , initWithObjects_count
  , initWithCoder
  , filteredOrderedSetUsingPredicate
  , sortedArrayUsingDescriptors
  , addObserver_forKeyPath_options_context
  , removeObserver_forKeyPath_context
  , removeObserver_forKeyPath
  , valueForKey
  , setValue_forKey
  , differenceFromOrderedSet_withOptions_usingEquivalenceTest
  , differenceFromOrderedSet_withOptions
  , differenceFromOrderedSet
  , orderedSetByApplyingDifference
  , orderedSet
  , orderedSetWithObject
  , orderedSetWithObjects_count
  , orderedSetWithObjects
  , orderedSetWithOrderedSet
  , orderedSetWithOrderedSet_range_copyItems
  , orderedSetWithArray
  , orderedSetWithArray_range_copyItems
  , orderedSetWithSet
  , orderedSetWithSet_copyItems
  , initWithObject
  , initWithObjects
  , initWithOrderedSet
  , initWithOrderedSet_copyItems
  , initWithOrderedSet_range_copyItems
  , initWithArray
  , initWithArray_copyItems
  , initWithArray_range_copyItems
  , initWithSet
  , initWithSet_copyItems
  , getObjects_range
  , objectsAtIndexes
  , isEqualToOrderedSet
  , containsObject
  , intersectsOrderedSet
  , intersectsSet
  , isSubsetOfOrderedSet
  , isSubsetOfSet
  , objectAtIndexedSubscript
  , objectEnumerator
  , reverseObjectEnumerator
  , enumerateObjectsUsingBlock
  , enumerateObjectsWithOptions_usingBlock
  , enumerateObjectsAtIndexes_options_usingBlock
  , indexOfObjectPassingTest
  , indexOfObjectWithOptions_passingTest
  , indexOfObjectAtIndexes_options_passingTest
  , indexesOfObjectsPassingTest
  , indexesOfObjectsWithOptions_passingTest
  , indexesOfObjectsAtIndexes_options_passingTest
  , indexOfObject_inSortedRange_options_usingComparator
  , sortedArrayUsingComparator
  , sortedArrayWithOptions_usingComparator
  , descriptionWithLocale
  , descriptionWithLocale_indent
  , count
  , firstObject
  , lastObject
  , reversedOrderedSet
  , array
  , set
  , description
  , addObserver_forKeyPath_options_contextSelector
  , arraySelector
  , containsObjectSelector
  , countSelector
  , descriptionSelector
  , descriptionWithLocaleSelector
  , descriptionWithLocale_indentSelector
  , differenceFromOrderedSetSelector
  , differenceFromOrderedSet_withOptionsSelector
  , differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector
  , enumerateObjectsAtIndexes_options_usingBlockSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , filteredOrderedSetUsingPredicateSelector
  , firstObjectSelector
  , getObjects_rangeSelector
  , indexOfObjectAtIndexes_options_passingTestSelector
  , indexOfObjectPassingTestSelector
  , indexOfObjectSelector
  , indexOfObjectWithOptions_passingTestSelector
  , indexOfObject_inSortedRange_options_usingComparatorSelector
  , indexesOfObjectsAtIndexes_options_passingTestSelector
  , indexesOfObjectsPassingTestSelector
  , indexesOfObjectsWithOptions_passingTestSelector
  , initSelector
  , initWithArraySelector
  , initWithArray_copyItemsSelector
  , initWithArray_range_copyItemsSelector
  , initWithCoderSelector
  , initWithObjectSelector
  , initWithObjectsSelector
  , initWithObjects_countSelector
  , initWithOrderedSetSelector
  , initWithOrderedSet_copyItemsSelector
  , initWithOrderedSet_range_copyItemsSelector
  , initWithSetSelector
  , initWithSet_copyItemsSelector
  , intersectsOrderedSetSelector
  , intersectsSetSelector
  , isEqualToOrderedSetSelector
  , isSubsetOfOrderedSetSelector
  , isSubsetOfSetSelector
  , lastObjectSelector
  , objectAtIndexSelector
  , objectAtIndexedSubscriptSelector
  , objectEnumeratorSelector
  , objectsAtIndexesSelector
  , orderedSetByApplyingDifferenceSelector
  , orderedSetSelector
  , orderedSetWithArraySelector
  , orderedSetWithArray_range_copyItemsSelector
  , orderedSetWithObjectSelector
  , orderedSetWithObjectsSelector
  , orderedSetWithObjects_countSelector
  , orderedSetWithOrderedSetSelector
  , orderedSetWithOrderedSet_range_copyItemsSelector
  , orderedSetWithSetSelector
  , orderedSetWithSet_copyItemsSelector
  , removeObserver_forKeyPathSelector
  , removeObserver_forKeyPath_contextSelector
  , reverseObjectEnumeratorSelector
  , reversedOrderedSetSelector
  , setSelector
  , setValue_forKeySelector
  , sortedArrayUsingComparatorSelector
  , sortedArrayUsingDescriptorsSelector
  , sortedArrayWithOptions_usingComparatorSelector
  , valueForKeySelector

  -- * Enum types
  , NSBinarySearchingOptions(NSBinarySearchingOptions)
  , pattern NSBinarySearchingFirstEqual
  , pattern NSBinarySearchingLastEqual
  , pattern NSBinarySearchingInsertionIndex
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse
  , NSKeyValueObservingOptions(NSKeyValueObservingOptions)
  , pattern NSKeyValueObservingOptionNew
  , pattern NSKeyValueObservingOptionOld
  , pattern NSKeyValueObservingOptionInitial
  , pattern NSKeyValueObservingOptionPrior
  , NSOrderedCollectionDifferenceCalculationOptions(NSOrderedCollectionDifferenceCalculationOptions)
  , pattern NSOrderedCollectionDifferenceCalculationOmitInsertedObjects
  , pattern NSOrderedCollectionDifferenceCalculationOmitRemovedObjects
  , pattern NSOrderedCollectionDifferenceCalculationInferMoves
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

-- | @- objectAtIndex:@
objectAtIndex :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> CULong -> IO RawId
objectAtIndex nsOrderedSet idx =
  sendMessage nsOrderedSet objectAtIndexSelector idx

-- | @- indexOfObject:@
indexOfObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO CULong
indexOfObject nsOrderedSet object =
  sendMessage nsOrderedSet indexOfObjectSelector object

-- | @- init@
init_ :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSOrderedSet)
init_ nsOrderedSet =
  sendOwnedMessage nsOrderedSet initSelector

-- | @- initWithObjects:count:@
initWithObjects_count :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> CULong -> IO (Id NSOrderedSet)
initWithObjects_count nsOrderedSet objects cnt =
  sendOwnedMessage nsOrderedSet initWithObjects_countSelector objects cnt

-- | @- initWithCoder:@
initWithCoder :: (IsNSOrderedSet nsOrderedSet, IsNSCoder coder) => nsOrderedSet -> coder -> IO (Id NSOrderedSet)
initWithCoder nsOrderedSet coder =
  sendOwnedMessage nsOrderedSet initWithCoderSelector (toNSCoder coder)

-- | @- filteredOrderedSetUsingPredicate:@
filteredOrderedSetUsingPredicate :: (IsNSOrderedSet nsOrderedSet, IsNSPredicate p) => nsOrderedSet -> p -> IO (Id NSOrderedSet)
filteredOrderedSetUsingPredicate nsOrderedSet p =
  sendMessage nsOrderedSet filteredOrderedSetUsingPredicateSelector (toNSPredicate p)

-- | @- sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptors :: (IsNSOrderedSet nsOrderedSet, IsNSArray sortDescriptors) => nsOrderedSet -> sortDescriptors -> IO (Id NSArray)
sortedArrayUsingDescriptors nsOrderedSet sortDescriptors =
  sendMessage nsOrderedSet sortedArrayUsingDescriptorsSelector (toNSArray sortDescriptors)

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSOrderedSet nsOrderedSet, IsNSObject observer, IsNSString keyPath) => nsOrderedSet -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsOrderedSet observer keyPath options context =
  sendMessage nsOrderedSet addObserver_forKeyPath_options_contextSelector (toNSObject observer) (toNSString keyPath) options context

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSOrderedSet nsOrderedSet, IsNSObject observer, IsNSString keyPath) => nsOrderedSet -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsOrderedSet observer keyPath context =
  sendMessage nsOrderedSet removeObserver_forKeyPath_contextSelector (toNSObject observer) (toNSString keyPath) context

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSOrderedSet nsOrderedSet, IsNSObject observer, IsNSString keyPath) => nsOrderedSet -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsOrderedSet observer keyPath =
  sendMessage nsOrderedSet removeObserver_forKeyPathSelector (toNSObject observer) (toNSString keyPath)

-- | @- valueForKey:@
valueForKey :: (IsNSOrderedSet nsOrderedSet, IsNSString key) => nsOrderedSet -> key -> IO RawId
valueForKey nsOrderedSet key =
  sendMessage nsOrderedSet valueForKeySelector (toNSString key)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSOrderedSet nsOrderedSet, IsNSString key) => nsOrderedSet -> RawId -> key -> IO ()
setValue_forKey nsOrderedSet value key =
  sendMessage nsOrderedSet setValue_forKeySelector value (toNSString key)

-- | @- differenceFromOrderedSet:withOptions:usingEquivalenceTest:@
differenceFromOrderedSet_withOptions_usingEquivalenceTest :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> NSOrderedCollectionDifferenceCalculationOptions -> Ptr () -> IO (Id NSOrderedCollectionDifference)
differenceFromOrderedSet_withOptions_usingEquivalenceTest nsOrderedSet other options block =
  sendMessage nsOrderedSet differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector (toNSOrderedSet other) options block

-- | @- differenceFromOrderedSet:withOptions:@
differenceFromOrderedSet_withOptions :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> NSOrderedCollectionDifferenceCalculationOptions -> IO (Id NSOrderedCollectionDifference)
differenceFromOrderedSet_withOptions nsOrderedSet other options =
  sendMessage nsOrderedSet differenceFromOrderedSet_withOptionsSelector (toNSOrderedSet other) options

-- | @- differenceFromOrderedSet:@
differenceFromOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO (Id NSOrderedCollectionDifference)
differenceFromOrderedSet nsOrderedSet other =
  sendMessage nsOrderedSet differenceFromOrderedSetSelector (toNSOrderedSet other)

-- | @- orderedSetByApplyingDifference:@
orderedSetByApplyingDifference :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedCollectionDifference difference) => nsOrderedSet -> difference -> IO (Id NSOrderedSet)
orderedSetByApplyingDifference nsOrderedSet difference =
  sendMessage nsOrderedSet orderedSetByApplyingDifferenceSelector (toNSOrderedCollectionDifference difference)

-- | @+ orderedSet@
orderedSet :: IO (Id NSOrderedSet)
orderedSet  =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetSelector

-- | @+ orderedSetWithObject:@
orderedSetWithObject :: RawId -> IO (Id NSOrderedSet)
orderedSetWithObject object =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithObjectSelector object

-- | @+ orderedSetWithObjects:count:@
orderedSetWithObjects_count :: RawId -> CULong -> IO (Id NSOrderedSet)
orderedSetWithObjects_count objects cnt =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithObjects_countSelector objects cnt

-- | @+ orderedSetWithObjects:@
orderedSetWithObjects :: RawId -> IO (Id NSOrderedSet)
orderedSetWithObjects firstObj =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithObjectsSelector firstObj

-- | @+ orderedSetWithOrderedSet:@
orderedSetWithOrderedSet :: IsNSOrderedSet set => set -> IO (Id NSOrderedSet)
orderedSetWithOrderedSet set =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithOrderedSetSelector (toNSOrderedSet set)

-- | @+ orderedSetWithOrderedSet:range:copyItems:@
orderedSetWithOrderedSet_range_copyItems :: IsNSOrderedSet set => set -> NSRange -> Bool -> IO (Id NSOrderedSet)
orderedSetWithOrderedSet_range_copyItems set range flag =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithOrderedSet_range_copyItemsSelector (toNSOrderedSet set) range flag

-- | @+ orderedSetWithArray:@
orderedSetWithArray :: IsNSArray array => array -> IO (Id NSOrderedSet)
orderedSetWithArray array =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithArraySelector (toNSArray array)

-- | @+ orderedSetWithArray:range:copyItems:@
orderedSetWithArray_range_copyItems :: IsNSArray array => array -> NSRange -> Bool -> IO (Id NSOrderedSet)
orderedSetWithArray_range_copyItems array range flag =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithArray_range_copyItemsSelector (toNSArray array) range flag

-- | @+ orderedSetWithSet:@
orderedSetWithSet :: IsNSSet set => set -> IO (Id NSOrderedSet)
orderedSetWithSet set =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithSetSelector (toNSSet set)

-- | @+ orderedSetWithSet:copyItems:@
orderedSetWithSet_copyItems :: IsNSSet set => set -> Bool -> IO (Id NSOrderedSet)
orderedSetWithSet_copyItems set flag =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMessage cls' orderedSetWithSet_copyItemsSelector (toNSSet set) flag

-- | @- initWithObject:@
initWithObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO (Id NSOrderedSet)
initWithObject nsOrderedSet object =
  sendOwnedMessage nsOrderedSet initWithObjectSelector object

-- | @- initWithObjects:@
initWithObjects :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO (Id NSOrderedSet)
initWithObjects nsOrderedSet firstObj =
  sendOwnedMessage nsOrderedSet initWithObjectsSelector firstObj

-- | @- initWithOrderedSet:@
initWithOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet set) => nsOrderedSet -> set -> IO (Id NSOrderedSet)
initWithOrderedSet nsOrderedSet set =
  sendOwnedMessage nsOrderedSet initWithOrderedSetSelector (toNSOrderedSet set)

-- | @- initWithOrderedSet:copyItems:@
initWithOrderedSet_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet set) => nsOrderedSet -> set -> Bool -> IO (Id NSOrderedSet)
initWithOrderedSet_copyItems nsOrderedSet set flag =
  sendOwnedMessage nsOrderedSet initWithOrderedSet_copyItemsSelector (toNSOrderedSet set) flag

-- | @- initWithOrderedSet:range:copyItems:@
initWithOrderedSet_range_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet set) => nsOrderedSet -> set -> NSRange -> Bool -> IO (Id NSOrderedSet)
initWithOrderedSet_range_copyItems nsOrderedSet set range flag =
  sendOwnedMessage nsOrderedSet initWithOrderedSet_range_copyItemsSelector (toNSOrderedSet set) range flag

-- | @- initWithArray:@
initWithArray :: (IsNSOrderedSet nsOrderedSet, IsNSArray array) => nsOrderedSet -> array -> IO (Id NSOrderedSet)
initWithArray nsOrderedSet array =
  sendOwnedMessage nsOrderedSet initWithArraySelector (toNSArray array)

-- | @- initWithArray:copyItems:@
initWithArray_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSArray set) => nsOrderedSet -> set -> Bool -> IO (Id NSOrderedSet)
initWithArray_copyItems nsOrderedSet set flag =
  sendOwnedMessage nsOrderedSet initWithArray_copyItemsSelector (toNSArray set) flag

-- | @- initWithArray:range:copyItems:@
initWithArray_range_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSArray set) => nsOrderedSet -> set -> NSRange -> Bool -> IO (Id NSOrderedSet)
initWithArray_range_copyItems nsOrderedSet set range flag =
  sendOwnedMessage nsOrderedSet initWithArray_range_copyItemsSelector (toNSArray set) range flag

-- | @- initWithSet:@
initWithSet :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> IO (Id NSOrderedSet)
initWithSet nsOrderedSet set =
  sendOwnedMessage nsOrderedSet initWithSetSelector (toNSSet set)

-- | @- initWithSet:copyItems:@
initWithSet_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> Bool -> IO (Id NSOrderedSet)
initWithSet_copyItems nsOrderedSet set flag =
  sendOwnedMessage nsOrderedSet initWithSet_copyItemsSelector (toNSSet set) flag

-- | @- getObjects:range:@
getObjects_range :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> NSRange -> IO ()
getObjects_range nsOrderedSet objects range =
  sendMessage nsOrderedSet getObjects_rangeSelector objects range

-- | @- objectsAtIndexes:@
objectsAtIndexes :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet indexes) => nsOrderedSet -> indexes -> IO (Id NSArray)
objectsAtIndexes nsOrderedSet indexes =
  sendMessage nsOrderedSet objectsAtIndexesSelector (toNSIndexSet indexes)

-- | @- isEqualToOrderedSet:@
isEqualToOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO Bool
isEqualToOrderedSet nsOrderedSet other =
  sendMessage nsOrderedSet isEqualToOrderedSetSelector (toNSOrderedSet other)

-- | @- containsObject:@
containsObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO Bool
containsObject nsOrderedSet object =
  sendMessage nsOrderedSet containsObjectSelector object

-- | @- intersectsOrderedSet:@
intersectsOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO Bool
intersectsOrderedSet nsOrderedSet other =
  sendMessage nsOrderedSet intersectsOrderedSetSelector (toNSOrderedSet other)

-- | @- intersectsSet:@
intersectsSet :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> IO Bool
intersectsSet nsOrderedSet set =
  sendMessage nsOrderedSet intersectsSetSelector (toNSSet set)

-- | @- isSubsetOfOrderedSet:@
isSubsetOfOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO Bool
isSubsetOfOrderedSet nsOrderedSet other =
  sendMessage nsOrderedSet isSubsetOfOrderedSetSelector (toNSOrderedSet other)

-- | @- isSubsetOfSet:@
isSubsetOfSet :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> IO Bool
isSubsetOfSet nsOrderedSet set =
  sendMessage nsOrderedSet isSubsetOfSetSelector (toNSSet set)

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> CULong -> IO RawId
objectAtIndexedSubscript nsOrderedSet idx =
  sendMessage nsOrderedSet objectAtIndexedSubscriptSelector idx

-- | @- objectEnumerator@
objectEnumerator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSEnumerator)
objectEnumerator nsOrderedSet =
  sendMessage nsOrderedSet objectEnumeratorSelector

-- | @- reverseObjectEnumerator@
reverseObjectEnumerator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSEnumerator)
reverseObjectEnumerator nsOrderedSet =
  sendMessage nsOrderedSet reverseObjectEnumeratorSelector

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO ()
enumerateObjectsUsingBlock nsOrderedSet block =
  sendMessage nsOrderedSet enumerateObjectsUsingBlockSelector block

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock nsOrderedSet opts block =
  sendMessage nsOrderedSet enumerateObjectsWithOptions_usingBlockSelector opts block

-- | @- enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlock :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet s) => nsOrderedSet -> s -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsAtIndexes_options_usingBlock nsOrderedSet s opts block =
  sendMessage nsOrderedSet enumerateObjectsAtIndexes_options_usingBlockSelector (toNSIndexSet s) opts block

-- | @- indexOfObjectPassingTest:@
indexOfObjectPassingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO CULong
indexOfObjectPassingTest nsOrderedSet predicate =
  sendMessage nsOrderedSet indexOfObjectPassingTestSelector predicate

-- | @- indexOfObjectWithOptions:passingTest:@
indexOfObjectWithOptions_passingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectWithOptions_passingTest nsOrderedSet opts predicate =
  sendMessage nsOrderedSet indexOfObjectWithOptions_passingTestSelector opts predicate

-- | @- indexOfObjectAtIndexes:options:passingTest:@
indexOfObjectAtIndexes_options_passingTest :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet s) => nsOrderedSet -> s -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectAtIndexes_options_passingTest nsOrderedSet s opts predicate =
  sendMessage nsOrderedSet indexOfObjectAtIndexes_options_passingTestSelector (toNSIndexSet s) opts predicate

-- | @- indexesOfObjectsPassingTest:@
indexesOfObjectsPassingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsPassingTest nsOrderedSet predicate =
  sendMessage nsOrderedSet indexesOfObjectsPassingTestSelector predicate

-- | @- indexesOfObjectsWithOptions:passingTest:@
indexesOfObjectsWithOptions_passingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsWithOptions_passingTest nsOrderedSet opts predicate =
  sendMessage nsOrderedSet indexesOfObjectsWithOptions_passingTestSelector opts predicate

-- | @- indexesOfObjectsAtIndexes:options:passingTest:@
indexesOfObjectsAtIndexes_options_passingTest :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet s) => nsOrderedSet -> s -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsAtIndexes_options_passingTest nsOrderedSet s opts predicate =
  sendMessage nsOrderedSet indexesOfObjectsAtIndexes_options_passingTestSelector (toNSIndexSet s) opts predicate

-- | @- indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> NSRange -> NSBinarySearchingOptions -> Ptr () -> IO CULong
indexOfObject_inSortedRange_options_usingComparator nsOrderedSet object range opts cmp =
  sendMessage nsOrderedSet indexOfObject_inSortedRange_options_usingComparatorSelector object range opts cmp

-- | @- sortedArrayUsingComparator:@
sortedArrayUsingComparator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO (Id NSArray)
sortedArrayUsingComparator nsOrderedSet cmptr =
  sendMessage nsOrderedSet sortedArrayUsingComparatorSelector cmptr

-- | @- sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSSortOptions -> Ptr () -> IO (Id NSArray)
sortedArrayWithOptions_usingComparator nsOrderedSet opts cmptr =
  sendMessage nsOrderedSet sortedArrayWithOptions_usingComparatorSelector opts cmptr

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO (Id NSString)
descriptionWithLocale nsOrderedSet locale =
  sendMessage nsOrderedSet descriptionWithLocaleSelector locale

-- | @- descriptionWithLocale:indent:@
descriptionWithLocale_indent :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> CULong -> IO (Id NSString)
descriptionWithLocale_indent nsOrderedSet locale level =
  sendMessage nsOrderedSet descriptionWithLocale_indentSelector locale level

-- | @- count@
count :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO CULong
count nsOrderedSet =
  sendMessage nsOrderedSet countSelector

-- | @- firstObject@
firstObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO RawId
firstObject nsOrderedSet =
  sendMessage nsOrderedSet firstObjectSelector

-- | @- lastObject@
lastObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO RawId
lastObject nsOrderedSet =
  sendMessage nsOrderedSet lastObjectSelector

-- | @- reversedOrderedSet@
reversedOrderedSet :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSOrderedSet)
reversedOrderedSet nsOrderedSet =
  sendMessage nsOrderedSet reversedOrderedSetSelector

-- | @- array@
array :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSArray)
array nsOrderedSet =
  sendMessage nsOrderedSet arraySelector

-- | @- set@
set :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSSet)
set nsOrderedSet =
  sendMessage nsOrderedSet setSelector

-- | @- description@
description :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSString)
description nsOrderedSet =
  sendMessage nsOrderedSet descriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector '[CULong] RawId
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @indexOfObject:@
indexOfObjectSelector :: Selector '[RawId] CULong
indexOfObjectSelector = mkSelector "indexOfObject:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSOrderedSet)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:count:@
initWithObjects_countSelector :: Selector '[RawId, CULong] (Id NSOrderedSet)
initWithObjects_countSelector = mkSelector "initWithObjects:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSOrderedSet)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filteredOrderedSetUsingPredicate:@
filteredOrderedSetUsingPredicateSelector :: Selector '[Id NSPredicate] (Id NSOrderedSet)
filteredOrderedSetUsingPredicateSelector = mkSelector "filteredOrderedSetUsingPredicate:"

-- | @Selector@ for @sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptorsSelector :: Selector '[Id NSArray] (Id NSArray)
sortedArrayUsingDescriptorsSelector = mkSelector "sortedArrayUsingDescriptors:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector '[Id NSObject, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_contextSelector :: Selector '[Id NSObject, Id NSString, Ptr ()] ()
removeObserver_forKeyPath_contextSelector = mkSelector "removeObserver:forKeyPath:context:"

-- | @Selector@ for @removeObserver:forKeyPath:@
removeObserver_forKeyPathSelector :: Selector '[Id NSObject, Id NSString] ()
removeObserver_forKeyPathSelector = mkSelector "removeObserver:forKeyPath:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @differenceFromOrderedSet:withOptions:usingEquivalenceTest:@
differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector :: Selector '[Id NSOrderedSet, NSOrderedCollectionDifferenceCalculationOptions, Ptr ()] (Id NSOrderedCollectionDifference)
differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector = mkSelector "differenceFromOrderedSet:withOptions:usingEquivalenceTest:"

-- | @Selector@ for @differenceFromOrderedSet:withOptions:@
differenceFromOrderedSet_withOptionsSelector :: Selector '[Id NSOrderedSet, NSOrderedCollectionDifferenceCalculationOptions] (Id NSOrderedCollectionDifference)
differenceFromOrderedSet_withOptionsSelector = mkSelector "differenceFromOrderedSet:withOptions:"

-- | @Selector@ for @differenceFromOrderedSet:@
differenceFromOrderedSetSelector :: Selector '[Id NSOrderedSet] (Id NSOrderedCollectionDifference)
differenceFromOrderedSetSelector = mkSelector "differenceFromOrderedSet:"

-- | @Selector@ for @orderedSetByApplyingDifference:@
orderedSetByApplyingDifferenceSelector :: Selector '[Id NSOrderedCollectionDifference] (Id NSOrderedSet)
orderedSetByApplyingDifferenceSelector = mkSelector "orderedSetByApplyingDifference:"

-- | @Selector@ for @orderedSet@
orderedSetSelector :: Selector '[] (Id NSOrderedSet)
orderedSetSelector = mkSelector "orderedSet"

-- | @Selector@ for @orderedSetWithObject:@
orderedSetWithObjectSelector :: Selector '[RawId] (Id NSOrderedSet)
orderedSetWithObjectSelector = mkSelector "orderedSetWithObject:"

-- | @Selector@ for @orderedSetWithObjects:count:@
orderedSetWithObjects_countSelector :: Selector '[RawId, CULong] (Id NSOrderedSet)
orderedSetWithObjects_countSelector = mkSelector "orderedSetWithObjects:count:"

-- | @Selector@ for @orderedSetWithObjects:@
orderedSetWithObjectsSelector :: Selector '[RawId] (Id NSOrderedSet)
orderedSetWithObjectsSelector = mkSelector "orderedSetWithObjects:"

-- | @Selector@ for @orderedSetWithOrderedSet:@
orderedSetWithOrderedSetSelector :: Selector '[Id NSOrderedSet] (Id NSOrderedSet)
orderedSetWithOrderedSetSelector = mkSelector "orderedSetWithOrderedSet:"

-- | @Selector@ for @orderedSetWithOrderedSet:range:copyItems:@
orderedSetWithOrderedSet_range_copyItemsSelector :: Selector '[Id NSOrderedSet, NSRange, Bool] (Id NSOrderedSet)
orderedSetWithOrderedSet_range_copyItemsSelector = mkSelector "orderedSetWithOrderedSet:range:copyItems:"

-- | @Selector@ for @orderedSetWithArray:@
orderedSetWithArraySelector :: Selector '[Id NSArray] (Id NSOrderedSet)
orderedSetWithArraySelector = mkSelector "orderedSetWithArray:"

-- | @Selector@ for @orderedSetWithArray:range:copyItems:@
orderedSetWithArray_range_copyItemsSelector :: Selector '[Id NSArray, NSRange, Bool] (Id NSOrderedSet)
orderedSetWithArray_range_copyItemsSelector = mkSelector "orderedSetWithArray:range:copyItems:"

-- | @Selector@ for @orderedSetWithSet:@
orderedSetWithSetSelector :: Selector '[Id NSSet] (Id NSOrderedSet)
orderedSetWithSetSelector = mkSelector "orderedSetWithSet:"

-- | @Selector@ for @orderedSetWithSet:copyItems:@
orderedSetWithSet_copyItemsSelector :: Selector '[Id NSSet, Bool] (Id NSOrderedSet)
orderedSetWithSet_copyItemsSelector = mkSelector "orderedSetWithSet:copyItems:"

-- | @Selector@ for @initWithObject:@
initWithObjectSelector :: Selector '[RawId] (Id NSOrderedSet)
initWithObjectSelector = mkSelector "initWithObject:"

-- | @Selector@ for @initWithObjects:@
initWithObjectsSelector :: Selector '[RawId] (Id NSOrderedSet)
initWithObjectsSelector = mkSelector "initWithObjects:"

-- | @Selector@ for @initWithOrderedSet:@
initWithOrderedSetSelector :: Selector '[Id NSOrderedSet] (Id NSOrderedSet)
initWithOrderedSetSelector = mkSelector "initWithOrderedSet:"

-- | @Selector@ for @initWithOrderedSet:copyItems:@
initWithOrderedSet_copyItemsSelector :: Selector '[Id NSOrderedSet, Bool] (Id NSOrderedSet)
initWithOrderedSet_copyItemsSelector = mkSelector "initWithOrderedSet:copyItems:"

-- | @Selector@ for @initWithOrderedSet:range:copyItems:@
initWithOrderedSet_range_copyItemsSelector :: Selector '[Id NSOrderedSet, NSRange, Bool] (Id NSOrderedSet)
initWithOrderedSet_range_copyItemsSelector = mkSelector "initWithOrderedSet:range:copyItems:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector '[Id NSArray] (Id NSOrderedSet)
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @initWithArray:copyItems:@
initWithArray_copyItemsSelector :: Selector '[Id NSArray, Bool] (Id NSOrderedSet)
initWithArray_copyItemsSelector = mkSelector "initWithArray:copyItems:"

-- | @Selector@ for @initWithArray:range:copyItems:@
initWithArray_range_copyItemsSelector :: Selector '[Id NSArray, NSRange, Bool] (Id NSOrderedSet)
initWithArray_range_copyItemsSelector = mkSelector "initWithArray:range:copyItems:"

-- | @Selector@ for @initWithSet:@
initWithSetSelector :: Selector '[Id NSSet] (Id NSOrderedSet)
initWithSetSelector = mkSelector "initWithSet:"

-- | @Selector@ for @initWithSet:copyItems:@
initWithSet_copyItemsSelector :: Selector '[Id NSSet, Bool] (Id NSOrderedSet)
initWithSet_copyItemsSelector = mkSelector "initWithSet:copyItems:"

-- | @Selector@ for @getObjects:range:@
getObjects_rangeSelector :: Selector '[RawId, NSRange] ()
getObjects_rangeSelector = mkSelector "getObjects:range:"

-- | @Selector@ for @objectsAtIndexes:@
objectsAtIndexesSelector :: Selector '[Id NSIndexSet] (Id NSArray)
objectsAtIndexesSelector = mkSelector "objectsAtIndexes:"

-- | @Selector@ for @isEqualToOrderedSet:@
isEqualToOrderedSetSelector :: Selector '[Id NSOrderedSet] Bool
isEqualToOrderedSetSelector = mkSelector "isEqualToOrderedSet:"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector '[RawId] Bool
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @intersectsOrderedSet:@
intersectsOrderedSetSelector :: Selector '[Id NSOrderedSet] Bool
intersectsOrderedSetSelector = mkSelector "intersectsOrderedSet:"

-- | @Selector@ for @intersectsSet:@
intersectsSetSelector :: Selector '[Id NSSet] Bool
intersectsSetSelector = mkSelector "intersectsSet:"

-- | @Selector@ for @isSubsetOfOrderedSet:@
isSubsetOfOrderedSetSelector :: Selector '[Id NSOrderedSet] Bool
isSubsetOfOrderedSetSelector = mkSelector "isSubsetOfOrderedSet:"

-- | @Selector@ for @isSubsetOfSet:@
isSubsetOfSetSelector :: Selector '[Id NSSet] Bool
isSubsetOfSetSelector = mkSelector "isSubsetOfSet:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] RawId
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @reverseObjectEnumerator@
reverseObjectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
reverseObjectEnumeratorSelector = mkSelector "reverseObjectEnumerator"

-- | @Selector@ for @enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateObjectsUsingBlockSelector = mkSelector "enumerateObjectsUsingBlock:"

-- | @Selector@ for @enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateObjectsWithOptions_usingBlockSelector = mkSelector "enumerateObjectsWithOptions:usingBlock:"

-- | @Selector@ for @enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlockSelector :: Selector '[Id NSIndexSet, NSEnumerationOptions, Ptr ()] ()
enumerateObjectsAtIndexes_options_usingBlockSelector = mkSelector "enumerateObjectsAtIndexes:options:usingBlock:"

-- | @Selector@ for @indexOfObjectPassingTest:@
indexOfObjectPassingTestSelector :: Selector '[Ptr ()] CULong
indexOfObjectPassingTestSelector = mkSelector "indexOfObjectPassingTest:"

-- | @Selector@ for @indexOfObjectWithOptions:passingTest:@
indexOfObjectWithOptions_passingTestSelector :: Selector '[NSEnumerationOptions, Ptr ()] CULong
indexOfObjectWithOptions_passingTestSelector = mkSelector "indexOfObjectWithOptions:passingTest:"

-- | @Selector@ for @indexOfObjectAtIndexes:options:passingTest:@
indexOfObjectAtIndexes_options_passingTestSelector :: Selector '[Id NSIndexSet, NSEnumerationOptions, Ptr ()] CULong
indexOfObjectAtIndexes_options_passingTestSelector = mkSelector "indexOfObjectAtIndexes:options:passingTest:"

-- | @Selector@ for @indexesOfObjectsPassingTest:@
indexesOfObjectsPassingTestSelector :: Selector '[Ptr ()] (Id NSIndexSet)
indexesOfObjectsPassingTestSelector = mkSelector "indexesOfObjectsPassingTest:"

-- | @Selector@ for @indexesOfObjectsWithOptions:passingTest:@
indexesOfObjectsWithOptions_passingTestSelector :: Selector '[NSEnumerationOptions, Ptr ()] (Id NSIndexSet)
indexesOfObjectsWithOptions_passingTestSelector = mkSelector "indexesOfObjectsWithOptions:passingTest:"

-- | @Selector@ for @indexesOfObjectsAtIndexes:options:passingTest:@
indexesOfObjectsAtIndexes_options_passingTestSelector :: Selector '[Id NSIndexSet, NSEnumerationOptions, Ptr ()] (Id NSIndexSet)
indexesOfObjectsAtIndexes_options_passingTestSelector = mkSelector "indexesOfObjectsAtIndexes:options:passingTest:"

-- | @Selector@ for @indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparatorSelector :: Selector '[RawId, NSRange, NSBinarySearchingOptions, Ptr ()] CULong
indexOfObject_inSortedRange_options_usingComparatorSelector = mkSelector "indexOfObject:inSortedRange:options:usingComparator:"

-- | @Selector@ for @sortedArrayUsingComparator:@
sortedArrayUsingComparatorSelector :: Selector '[Ptr ()] (Id NSArray)
sortedArrayUsingComparatorSelector = mkSelector "sortedArrayUsingComparator:"

-- | @Selector@ for @sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparatorSelector :: Selector '[NSSortOptions, Ptr ()] (Id NSArray)
sortedArrayWithOptions_usingComparatorSelector = mkSelector "sortedArrayWithOptions:usingComparator:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @descriptionWithLocale:indent:@
descriptionWithLocale_indentSelector :: Selector '[RawId, CULong] (Id NSString)
descriptionWithLocale_indentSelector = mkSelector "descriptionWithLocale:indent:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @firstObject@
firstObjectSelector :: Selector '[] RawId
firstObjectSelector = mkSelector "firstObject"

-- | @Selector@ for @lastObject@
lastObjectSelector :: Selector '[] RawId
lastObjectSelector = mkSelector "lastObject"

-- | @Selector@ for @reversedOrderedSet@
reversedOrderedSetSelector :: Selector '[] (Id NSOrderedSet)
reversedOrderedSetSelector = mkSelector "reversedOrderedSet"

-- | @Selector@ for @array@
arraySelector :: Selector '[] (Id NSArray)
arraySelector = mkSelector "array"

-- | @Selector@ for @set@
setSelector :: Selector '[] (Id NSSet)
setSelector = mkSelector "set"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

