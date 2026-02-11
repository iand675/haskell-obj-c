{-# LANGUAGE PatternSynonyms #-}
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
  , objectAtIndexSelector
  , indexOfObjectSelector
  , initSelector
  , initWithObjects_countSelector
  , initWithCoderSelector
  , filteredOrderedSetUsingPredicateSelector
  , sortedArrayUsingDescriptorsSelector
  , addObserver_forKeyPath_options_contextSelector
  , removeObserver_forKeyPath_contextSelector
  , removeObserver_forKeyPathSelector
  , valueForKeySelector
  , setValue_forKeySelector
  , differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector
  , differenceFromOrderedSet_withOptionsSelector
  , differenceFromOrderedSetSelector
  , orderedSetByApplyingDifferenceSelector
  , orderedSetSelector
  , orderedSetWithObjectSelector
  , orderedSetWithObjects_countSelector
  , orderedSetWithObjectsSelector
  , orderedSetWithOrderedSetSelector
  , orderedSetWithOrderedSet_range_copyItemsSelector
  , orderedSetWithArraySelector
  , orderedSetWithArray_range_copyItemsSelector
  , orderedSetWithSetSelector
  , orderedSetWithSet_copyItemsSelector
  , initWithObjectSelector
  , initWithObjectsSelector
  , initWithOrderedSetSelector
  , initWithOrderedSet_copyItemsSelector
  , initWithOrderedSet_range_copyItemsSelector
  , initWithArraySelector
  , initWithArray_copyItemsSelector
  , initWithArray_range_copyItemsSelector
  , initWithSetSelector
  , initWithSet_copyItemsSelector
  , getObjects_rangeSelector
  , objectsAtIndexesSelector
  , isEqualToOrderedSetSelector
  , containsObjectSelector
  , intersectsOrderedSetSelector
  , intersectsSetSelector
  , isSubsetOfOrderedSetSelector
  , isSubsetOfSetSelector
  , objectAtIndexedSubscriptSelector
  , objectEnumeratorSelector
  , reverseObjectEnumeratorSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , enumerateObjectsAtIndexes_options_usingBlockSelector
  , indexOfObjectPassingTestSelector
  , indexOfObjectWithOptions_passingTestSelector
  , indexOfObjectAtIndexes_options_passingTestSelector
  , indexesOfObjectsPassingTestSelector
  , indexesOfObjectsWithOptions_passingTestSelector
  , indexesOfObjectsAtIndexes_options_passingTestSelector
  , indexOfObject_inSortedRange_options_usingComparatorSelector
  , sortedArrayUsingComparatorSelector
  , sortedArrayWithOptions_usingComparatorSelector
  , descriptionWithLocaleSelector
  , descriptionWithLocale_indentSelector
  , countSelector
  , firstObjectSelector
  , lastObjectSelector
  , reversedOrderedSetSelector
  , arraySelector
  , setSelector
  , descriptionSelector

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

-- | @- objectAtIndex:@
objectAtIndex :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> CULong -> IO RawId
objectAtIndex nsOrderedSet  idx =
  fmap (RawId . castPtr) $ sendMsg nsOrderedSet (mkSelector "objectAtIndex:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- indexOfObject:@
indexOfObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO CULong
indexOfObject nsOrderedSet  object =
  sendMsg nsOrderedSet (mkSelector "indexOfObject:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- init@
init_ :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSOrderedSet)
init_ nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithObjects:count:@
initWithObjects_count :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> CULong -> IO (Id NSOrderedSet)
initWithObjects_count nsOrderedSet  objects cnt =
  sendMsg nsOrderedSet (mkSelector "initWithObjects:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral cnt)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSOrderedSet nsOrderedSet, IsNSCoder coder) => nsOrderedSet -> coder -> IO (Id NSOrderedSet)
initWithCoder nsOrderedSet  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsOrderedSet (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- filteredOrderedSetUsingPredicate:@
filteredOrderedSetUsingPredicate :: (IsNSOrderedSet nsOrderedSet, IsNSPredicate p) => nsOrderedSet -> p -> IO (Id NSOrderedSet)
filteredOrderedSetUsingPredicate nsOrderedSet  p =
withObjCPtr p $ \raw_p ->
    sendMsg nsOrderedSet (mkSelector "filteredOrderedSetUsingPredicate:") (retPtr retVoid) [argPtr (castPtr raw_p :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptors :: (IsNSOrderedSet nsOrderedSet, IsNSArray sortDescriptors) => nsOrderedSet -> sortDescriptors -> IO (Id NSArray)
sortedArrayUsingDescriptors nsOrderedSet  sortDescriptors =
withObjCPtr sortDescriptors $ \raw_sortDescriptors ->
    sendMsg nsOrderedSet (mkSelector "sortedArrayUsingDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_sortDescriptors :: Ptr ())] >>= retainedObject . castPtr

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSOrderedSet nsOrderedSet, IsNSObject observer, IsNSString keyPath) => nsOrderedSet -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsOrderedSet  observer keyPath options context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsOrderedSet (mkSelector "addObserver:forKeyPath:options:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argCULong (coerce options), argPtr context]

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSOrderedSet nsOrderedSet, IsNSObject observer, IsNSString keyPath) => nsOrderedSet -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsOrderedSet  observer keyPath context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsOrderedSet (mkSelector "removeObserver:forKeyPath:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr context]

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSOrderedSet nsOrderedSet, IsNSObject observer, IsNSString keyPath) => nsOrderedSet -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsOrderedSet  observer keyPath =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsOrderedSet (mkSelector "removeObserver:forKeyPath:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- valueForKey:@
valueForKey :: (IsNSOrderedSet nsOrderedSet, IsNSString key) => nsOrderedSet -> key -> IO RawId
valueForKey nsOrderedSet  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsOrderedSet (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSOrderedSet nsOrderedSet, IsNSString key) => nsOrderedSet -> RawId -> key -> IO ()
setValue_forKey nsOrderedSet  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsOrderedSet (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- differenceFromOrderedSet:withOptions:usingEquivalenceTest:@
differenceFromOrderedSet_withOptions_usingEquivalenceTest :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> NSOrderedCollectionDifferenceCalculationOptions -> Ptr () -> IO (Id NSOrderedCollectionDifference)
differenceFromOrderedSet_withOptions_usingEquivalenceTest nsOrderedSet  other options block =
withObjCPtr other $ \raw_other ->
    sendMsg nsOrderedSet (mkSelector "differenceFromOrderedSet:withOptions:usingEquivalenceTest:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ()), argCULong (coerce options), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | @- differenceFromOrderedSet:withOptions:@
differenceFromOrderedSet_withOptions :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> NSOrderedCollectionDifferenceCalculationOptions -> IO (Id NSOrderedCollectionDifference)
differenceFromOrderedSet_withOptions nsOrderedSet  other options =
withObjCPtr other $ \raw_other ->
    sendMsg nsOrderedSet (mkSelector "differenceFromOrderedSet:withOptions:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- differenceFromOrderedSet:@
differenceFromOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO (Id NSOrderedCollectionDifference)
differenceFromOrderedSet nsOrderedSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsOrderedSet (mkSelector "differenceFromOrderedSet:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ())] >>= retainedObject . castPtr

-- | @- orderedSetByApplyingDifference:@
orderedSetByApplyingDifference :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedCollectionDifference difference) => nsOrderedSet -> difference -> IO (Id NSOrderedSet)
orderedSetByApplyingDifference nsOrderedSet  difference =
withObjCPtr difference $ \raw_difference ->
    sendMsg nsOrderedSet (mkSelector "orderedSetByApplyingDifference:") (retPtr retVoid) [argPtr (castPtr raw_difference :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedSet@
orderedSet :: IO (Id NSOrderedSet)
orderedSet  =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMsg cls' (mkSelector "orderedSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ orderedSetWithObject:@
orderedSetWithObject :: RawId -> IO (Id NSOrderedSet)
orderedSetWithObject object =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMsg cls' (mkSelector "orderedSetWithObject:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedSetWithObjects:count:@
orderedSetWithObjects_count :: RawId -> CULong -> IO (Id NSOrderedSet)
orderedSetWithObjects_count objects cnt =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMsg cls' (mkSelector "orderedSetWithObjects:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral cnt)] >>= retainedObject . castPtr

-- | @+ orderedSetWithObjects:@
orderedSetWithObjects :: RawId -> IO (Id NSOrderedSet)
orderedSetWithObjects firstObj =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    sendClassMsg cls' (mkSelector "orderedSetWithObjects:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObj) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedSetWithOrderedSet:@
orderedSetWithOrderedSet :: IsNSOrderedSet set => set -> IO (Id NSOrderedSet)
orderedSetWithOrderedSet set =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    withObjCPtr set $ \raw_set ->
      sendClassMsg cls' (mkSelector "orderedSetWithOrderedSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedSetWithOrderedSet:range:copyItems:@
orderedSetWithOrderedSet_range_copyItems :: IsNSOrderedSet set => set -> NSRange -> Bool -> IO (Id NSOrderedSet)
orderedSetWithOrderedSet_range_copyItems set range flag =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    withObjCPtr set $ \raw_set ->
      sendClassMsg cls' (mkSelector "orderedSetWithOrderedSet:range:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argNSRange range, argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @+ orderedSetWithArray:@
orderedSetWithArray :: IsNSArray array => array -> IO (Id NSOrderedSet)
orderedSetWithArray array =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    withObjCPtr array $ \raw_array ->
      sendClassMsg cls' (mkSelector "orderedSetWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedSetWithArray:range:copyItems:@
orderedSetWithArray_range_copyItems :: IsNSArray array => array -> NSRange -> Bool -> IO (Id NSOrderedSet)
orderedSetWithArray_range_copyItems array range flag =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    withObjCPtr array $ \raw_array ->
      sendClassMsg cls' (mkSelector "orderedSetWithArray:range:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ()), argNSRange range, argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @+ orderedSetWithSet:@
orderedSetWithSet :: IsNSSet set => set -> IO (Id NSOrderedSet)
orderedSetWithSet set =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    withObjCPtr set $ \raw_set ->
      sendClassMsg cls' (mkSelector "orderedSetWithSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedSetWithSet:copyItems:@
orderedSetWithSet_copyItems :: IsNSSet set => set -> Bool -> IO (Id NSOrderedSet)
orderedSetWithSet_copyItems set flag =
  do
    cls' <- getRequiredClass "NSOrderedSet"
    withObjCPtr set $ \raw_set ->
      sendClassMsg cls' (mkSelector "orderedSetWithSet:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @- initWithObject:@
initWithObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO (Id NSOrderedSet)
initWithObject nsOrderedSet  object =
  sendMsg nsOrderedSet (mkSelector "initWithObject:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithObjects:@
initWithObjects :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO (Id NSOrderedSet)
initWithObjects nsOrderedSet  firstObj =
  sendMsg nsOrderedSet (mkSelector "initWithObjects:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObj) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithOrderedSet:@
initWithOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet set) => nsOrderedSet -> set -> IO (Id NSOrderedSet)
initWithOrderedSet nsOrderedSet  set =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithOrderedSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithOrderedSet:copyItems:@
initWithOrderedSet_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet set) => nsOrderedSet -> set -> Bool -> IO (Id NSOrderedSet)
initWithOrderedSet_copyItems nsOrderedSet  set flag =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithOrderedSet:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithOrderedSet:range:copyItems:@
initWithOrderedSet_range_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet set) => nsOrderedSet -> set -> NSRange -> Bool -> IO (Id NSOrderedSet)
initWithOrderedSet_range_copyItems nsOrderedSet  set range flag =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithOrderedSet:range:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argNSRange range, argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithArray:@
initWithArray :: (IsNSOrderedSet nsOrderedSet, IsNSArray array) => nsOrderedSet -> array -> IO (Id NSOrderedSet)
initWithArray nsOrderedSet  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsOrderedSet (mkSelector "initWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithArray:copyItems:@
initWithArray_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSArray set) => nsOrderedSet -> set -> Bool -> IO (Id NSOrderedSet)
initWithArray_copyItems nsOrderedSet  set flag =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithArray:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithArray:range:copyItems:@
initWithArray_range_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSArray set) => nsOrderedSet -> set -> NSRange -> Bool -> IO (Id NSOrderedSet)
initWithArray_range_copyItems nsOrderedSet  set range flag =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithArray:range:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argNSRange range, argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithSet:@
initWithSet :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> IO (Id NSOrderedSet)
initWithSet nsOrderedSet  set =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSet:copyItems:@
initWithSet_copyItems :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> Bool -> IO (Id NSOrderedSet)
initWithSet_copyItems nsOrderedSet  set flag =
withObjCPtr set $ \raw_set ->
    sendMsg nsOrderedSet (mkSelector "initWithSet:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- getObjects:range:@
getObjects_range :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> NSRange -> IO ()
getObjects_range nsOrderedSet  objects range =
  sendMsg nsOrderedSet (mkSelector "getObjects:range:") retVoid [argPtr (castPtr (unRawId objects) :: Ptr ()), argNSRange range]

-- | @- objectsAtIndexes:@
objectsAtIndexes :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet indexes) => nsOrderedSet -> indexes -> IO (Id NSArray)
objectsAtIndexes nsOrderedSet  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsOrderedSet (mkSelector "objectsAtIndexes:") (retPtr retVoid) [argPtr (castPtr raw_indexes :: Ptr ())] >>= retainedObject . castPtr

-- | @- isEqualToOrderedSet:@
isEqualToOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO Bool
isEqualToOrderedSet nsOrderedSet  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedSet (mkSelector "isEqualToOrderedSet:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- containsObject:@
containsObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO Bool
containsObject nsOrderedSet  object =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedSet (mkSelector "containsObject:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- intersectsOrderedSet:@
intersectsOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO Bool
intersectsOrderedSet nsOrderedSet  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedSet (mkSelector "intersectsOrderedSet:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- intersectsSet:@
intersectsSet :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> IO Bool
intersectsSet nsOrderedSet  set =
withObjCPtr set $ \raw_set ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedSet (mkSelector "intersectsSet:") retCULong [argPtr (castPtr raw_set :: Ptr ())]

-- | @- isSubsetOfOrderedSet:@
isSubsetOfOrderedSet :: (IsNSOrderedSet nsOrderedSet, IsNSOrderedSet other) => nsOrderedSet -> other -> IO Bool
isSubsetOfOrderedSet nsOrderedSet  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedSet (mkSelector "isSubsetOfOrderedSet:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- isSubsetOfSet:@
isSubsetOfSet :: (IsNSOrderedSet nsOrderedSet, IsNSSet set) => nsOrderedSet -> set -> IO Bool
isSubsetOfSet nsOrderedSet  set =
withObjCPtr set $ \raw_set ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOrderedSet (mkSelector "isSubsetOfSet:") retCULong [argPtr (castPtr raw_set :: Ptr ())]

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> CULong -> IO RawId
objectAtIndexedSubscript nsOrderedSet  idx =
  fmap (RawId . castPtr) $ sendMsg nsOrderedSet (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- objectEnumerator@
objectEnumerator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSEnumerator)
objectEnumerator nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reverseObjectEnumerator@
reverseObjectEnumerator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSEnumerator)
reverseObjectEnumerator nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "reverseObjectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO ()
enumerateObjectsUsingBlock nsOrderedSet  block =
  sendMsg nsOrderedSet (mkSelector "enumerateObjectsUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock nsOrderedSet  opts block =
  sendMsg nsOrderedSet (mkSelector "enumerateObjectsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlock :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet s) => nsOrderedSet -> s -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsAtIndexes_options_usingBlock nsOrderedSet  s opts block =
withObjCPtr s $ \raw_s ->
    sendMsg nsOrderedSet (mkSelector "enumerateObjectsAtIndexes:options:usingBlock:") retVoid [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- indexOfObjectPassingTest:@
indexOfObjectPassingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO CULong
indexOfObjectPassingTest nsOrderedSet  predicate =
  sendMsg nsOrderedSet (mkSelector "indexOfObjectPassingTest:") retCULong [argPtr (castPtr predicate :: Ptr ())]

-- | @- indexOfObjectWithOptions:passingTest:@
indexOfObjectWithOptions_passingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectWithOptions_passingTest nsOrderedSet  opts predicate =
  sendMsg nsOrderedSet (mkSelector "indexOfObjectWithOptions:passingTest:") retCULong [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())]

-- | @- indexOfObjectAtIndexes:options:passingTest:@
indexOfObjectAtIndexes_options_passingTest :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet s) => nsOrderedSet -> s -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectAtIndexes_options_passingTest nsOrderedSet  s opts predicate =
withObjCPtr s $ \raw_s ->
    sendMsg nsOrderedSet (mkSelector "indexOfObjectAtIndexes:options:passingTest:") retCULong [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())]

-- | @- indexesOfObjectsPassingTest:@
indexesOfObjectsPassingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsPassingTest nsOrderedSet  predicate =
  sendMsg nsOrderedSet (mkSelector "indexesOfObjectsPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexesOfObjectsWithOptions:passingTest:@
indexesOfObjectsWithOptions_passingTest :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsWithOptions_passingTest nsOrderedSet  opts predicate =
  sendMsg nsOrderedSet (mkSelector "indexesOfObjectsWithOptions:passingTest:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexesOfObjectsAtIndexes:options:passingTest:@
indexesOfObjectsAtIndexes_options_passingTest :: (IsNSOrderedSet nsOrderedSet, IsNSIndexSet s) => nsOrderedSet -> s -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsAtIndexes_options_passingTest nsOrderedSet  s opts predicate =
withObjCPtr s $ \raw_s ->
    sendMsg nsOrderedSet (mkSelector "indexesOfObjectsAtIndexes:options:passingTest:") (retPtr retVoid) [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> NSRange -> NSBinarySearchingOptions -> Ptr () -> IO CULong
indexOfObject_inSortedRange_options_usingComparator nsOrderedSet  object range opts cmp =
  sendMsg nsOrderedSet (mkSelector "indexOfObject:inSortedRange:options:usingComparator:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ()), argNSRange range, argCULong (coerce opts), argPtr (castPtr cmp :: Ptr ())]

-- | @- sortedArrayUsingComparator:@
sortedArrayUsingComparator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> Ptr () -> IO (Id NSArray)
sortedArrayUsingComparator nsOrderedSet  cmptr =
  sendMsg nsOrderedSet (mkSelector "sortedArrayUsingComparator:") (retPtr retVoid) [argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparator :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> NSSortOptions -> Ptr () -> IO (Id NSArray)
sortedArrayWithOptions_usingComparator nsOrderedSet  opts cmptr =
  sendMsg nsOrderedSet (mkSelector "sortedArrayWithOptions:usingComparator:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> IO (Id NSString)
descriptionWithLocale nsOrderedSet  locale =
  sendMsg nsOrderedSet (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithLocale:indent:@
descriptionWithLocale_indent :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> RawId -> CULong -> IO (Id NSString)
descriptionWithLocale_indent nsOrderedSet  locale level =
  sendMsg nsOrderedSet (mkSelector "descriptionWithLocale:indent:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ()), argCULong (fromIntegral level)] >>= retainedObject . castPtr

-- | @- count@
count :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO CULong
count nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "count") retCULong []

-- | @- firstObject@
firstObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO RawId
firstObject nsOrderedSet  =
  fmap (RawId . castPtr) $ sendMsg nsOrderedSet (mkSelector "firstObject") (retPtr retVoid) []

-- | @- lastObject@
lastObject :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO RawId
lastObject nsOrderedSet  =
  fmap (RawId . castPtr) $ sendMsg nsOrderedSet (mkSelector "lastObject") (retPtr retVoid) []

-- | @- reversedOrderedSet@
reversedOrderedSet :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSOrderedSet)
reversedOrderedSet nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "reversedOrderedSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- array@
array :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSArray)
array nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "array") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- set@
set :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSSet)
set nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "set") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- description@
description :: IsNSOrderedSet nsOrderedSet => nsOrderedSet -> IO (Id NSString)
description nsOrderedSet  =
  sendMsg nsOrderedSet (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @indexOfObject:@
indexOfObjectSelector :: Selector
indexOfObjectSelector = mkSelector "indexOfObject:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:count:@
initWithObjects_countSelector :: Selector
initWithObjects_countSelector = mkSelector "initWithObjects:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filteredOrderedSetUsingPredicate:@
filteredOrderedSetUsingPredicateSelector :: Selector
filteredOrderedSetUsingPredicateSelector = mkSelector "filteredOrderedSetUsingPredicate:"

-- | @Selector@ for @sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptorsSelector :: Selector
sortedArrayUsingDescriptorsSelector = mkSelector "sortedArrayUsingDescriptors:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_contextSelector :: Selector
removeObserver_forKeyPath_contextSelector = mkSelector "removeObserver:forKeyPath:context:"

-- | @Selector@ for @removeObserver:forKeyPath:@
removeObserver_forKeyPathSelector :: Selector
removeObserver_forKeyPathSelector = mkSelector "removeObserver:forKeyPath:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @differenceFromOrderedSet:withOptions:usingEquivalenceTest:@
differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector :: Selector
differenceFromOrderedSet_withOptions_usingEquivalenceTestSelector = mkSelector "differenceFromOrderedSet:withOptions:usingEquivalenceTest:"

-- | @Selector@ for @differenceFromOrderedSet:withOptions:@
differenceFromOrderedSet_withOptionsSelector :: Selector
differenceFromOrderedSet_withOptionsSelector = mkSelector "differenceFromOrderedSet:withOptions:"

-- | @Selector@ for @differenceFromOrderedSet:@
differenceFromOrderedSetSelector :: Selector
differenceFromOrderedSetSelector = mkSelector "differenceFromOrderedSet:"

-- | @Selector@ for @orderedSetByApplyingDifference:@
orderedSetByApplyingDifferenceSelector :: Selector
orderedSetByApplyingDifferenceSelector = mkSelector "orderedSetByApplyingDifference:"

-- | @Selector@ for @orderedSet@
orderedSetSelector :: Selector
orderedSetSelector = mkSelector "orderedSet"

-- | @Selector@ for @orderedSetWithObject:@
orderedSetWithObjectSelector :: Selector
orderedSetWithObjectSelector = mkSelector "orderedSetWithObject:"

-- | @Selector@ for @orderedSetWithObjects:count:@
orderedSetWithObjects_countSelector :: Selector
orderedSetWithObjects_countSelector = mkSelector "orderedSetWithObjects:count:"

-- | @Selector@ for @orderedSetWithObjects:@
orderedSetWithObjectsSelector :: Selector
orderedSetWithObjectsSelector = mkSelector "orderedSetWithObjects:"

-- | @Selector@ for @orderedSetWithOrderedSet:@
orderedSetWithOrderedSetSelector :: Selector
orderedSetWithOrderedSetSelector = mkSelector "orderedSetWithOrderedSet:"

-- | @Selector@ for @orderedSetWithOrderedSet:range:copyItems:@
orderedSetWithOrderedSet_range_copyItemsSelector :: Selector
orderedSetWithOrderedSet_range_copyItemsSelector = mkSelector "orderedSetWithOrderedSet:range:copyItems:"

-- | @Selector@ for @orderedSetWithArray:@
orderedSetWithArraySelector :: Selector
orderedSetWithArraySelector = mkSelector "orderedSetWithArray:"

-- | @Selector@ for @orderedSetWithArray:range:copyItems:@
orderedSetWithArray_range_copyItemsSelector :: Selector
orderedSetWithArray_range_copyItemsSelector = mkSelector "orderedSetWithArray:range:copyItems:"

-- | @Selector@ for @orderedSetWithSet:@
orderedSetWithSetSelector :: Selector
orderedSetWithSetSelector = mkSelector "orderedSetWithSet:"

-- | @Selector@ for @orderedSetWithSet:copyItems:@
orderedSetWithSet_copyItemsSelector :: Selector
orderedSetWithSet_copyItemsSelector = mkSelector "orderedSetWithSet:copyItems:"

-- | @Selector@ for @initWithObject:@
initWithObjectSelector :: Selector
initWithObjectSelector = mkSelector "initWithObject:"

-- | @Selector@ for @initWithObjects:@
initWithObjectsSelector :: Selector
initWithObjectsSelector = mkSelector "initWithObjects:"

-- | @Selector@ for @initWithOrderedSet:@
initWithOrderedSetSelector :: Selector
initWithOrderedSetSelector = mkSelector "initWithOrderedSet:"

-- | @Selector@ for @initWithOrderedSet:copyItems:@
initWithOrderedSet_copyItemsSelector :: Selector
initWithOrderedSet_copyItemsSelector = mkSelector "initWithOrderedSet:copyItems:"

-- | @Selector@ for @initWithOrderedSet:range:copyItems:@
initWithOrderedSet_range_copyItemsSelector :: Selector
initWithOrderedSet_range_copyItemsSelector = mkSelector "initWithOrderedSet:range:copyItems:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @initWithArray:copyItems:@
initWithArray_copyItemsSelector :: Selector
initWithArray_copyItemsSelector = mkSelector "initWithArray:copyItems:"

-- | @Selector@ for @initWithArray:range:copyItems:@
initWithArray_range_copyItemsSelector :: Selector
initWithArray_range_copyItemsSelector = mkSelector "initWithArray:range:copyItems:"

-- | @Selector@ for @initWithSet:@
initWithSetSelector :: Selector
initWithSetSelector = mkSelector "initWithSet:"

-- | @Selector@ for @initWithSet:copyItems:@
initWithSet_copyItemsSelector :: Selector
initWithSet_copyItemsSelector = mkSelector "initWithSet:copyItems:"

-- | @Selector@ for @getObjects:range:@
getObjects_rangeSelector :: Selector
getObjects_rangeSelector = mkSelector "getObjects:range:"

-- | @Selector@ for @objectsAtIndexes:@
objectsAtIndexesSelector :: Selector
objectsAtIndexesSelector = mkSelector "objectsAtIndexes:"

-- | @Selector@ for @isEqualToOrderedSet:@
isEqualToOrderedSetSelector :: Selector
isEqualToOrderedSetSelector = mkSelector "isEqualToOrderedSet:"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @intersectsOrderedSet:@
intersectsOrderedSetSelector :: Selector
intersectsOrderedSetSelector = mkSelector "intersectsOrderedSet:"

-- | @Selector@ for @intersectsSet:@
intersectsSetSelector :: Selector
intersectsSetSelector = mkSelector "intersectsSet:"

-- | @Selector@ for @isSubsetOfOrderedSet:@
isSubsetOfOrderedSetSelector :: Selector
isSubsetOfOrderedSetSelector = mkSelector "isSubsetOfOrderedSet:"

-- | @Selector@ for @isSubsetOfSet:@
isSubsetOfSetSelector :: Selector
isSubsetOfSetSelector = mkSelector "isSubsetOfSet:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @reverseObjectEnumerator@
reverseObjectEnumeratorSelector :: Selector
reverseObjectEnumeratorSelector = mkSelector "reverseObjectEnumerator"

-- | @Selector@ for @enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlockSelector :: Selector
enumerateObjectsUsingBlockSelector = mkSelector "enumerateObjectsUsingBlock:"

-- | @Selector@ for @enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlockSelector :: Selector
enumerateObjectsWithOptions_usingBlockSelector = mkSelector "enumerateObjectsWithOptions:usingBlock:"

-- | @Selector@ for @enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlockSelector :: Selector
enumerateObjectsAtIndexes_options_usingBlockSelector = mkSelector "enumerateObjectsAtIndexes:options:usingBlock:"

-- | @Selector@ for @indexOfObjectPassingTest:@
indexOfObjectPassingTestSelector :: Selector
indexOfObjectPassingTestSelector = mkSelector "indexOfObjectPassingTest:"

-- | @Selector@ for @indexOfObjectWithOptions:passingTest:@
indexOfObjectWithOptions_passingTestSelector :: Selector
indexOfObjectWithOptions_passingTestSelector = mkSelector "indexOfObjectWithOptions:passingTest:"

-- | @Selector@ for @indexOfObjectAtIndexes:options:passingTest:@
indexOfObjectAtIndexes_options_passingTestSelector :: Selector
indexOfObjectAtIndexes_options_passingTestSelector = mkSelector "indexOfObjectAtIndexes:options:passingTest:"

-- | @Selector@ for @indexesOfObjectsPassingTest:@
indexesOfObjectsPassingTestSelector :: Selector
indexesOfObjectsPassingTestSelector = mkSelector "indexesOfObjectsPassingTest:"

-- | @Selector@ for @indexesOfObjectsWithOptions:passingTest:@
indexesOfObjectsWithOptions_passingTestSelector :: Selector
indexesOfObjectsWithOptions_passingTestSelector = mkSelector "indexesOfObjectsWithOptions:passingTest:"

-- | @Selector@ for @indexesOfObjectsAtIndexes:options:passingTest:@
indexesOfObjectsAtIndexes_options_passingTestSelector :: Selector
indexesOfObjectsAtIndexes_options_passingTestSelector = mkSelector "indexesOfObjectsAtIndexes:options:passingTest:"

-- | @Selector@ for @indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparatorSelector :: Selector
indexOfObject_inSortedRange_options_usingComparatorSelector = mkSelector "indexOfObject:inSortedRange:options:usingComparator:"

-- | @Selector@ for @sortedArrayUsingComparator:@
sortedArrayUsingComparatorSelector :: Selector
sortedArrayUsingComparatorSelector = mkSelector "sortedArrayUsingComparator:"

-- | @Selector@ for @sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparatorSelector :: Selector
sortedArrayWithOptions_usingComparatorSelector = mkSelector "sortedArrayWithOptions:usingComparator:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @descriptionWithLocale:indent:@
descriptionWithLocale_indentSelector :: Selector
descriptionWithLocale_indentSelector = mkSelector "descriptionWithLocale:indent:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @firstObject@
firstObjectSelector :: Selector
firstObjectSelector = mkSelector "firstObject"

-- | @Selector@ for @lastObject@
lastObjectSelector :: Selector
lastObjectSelector = mkSelector "lastObject"

-- | @Selector@ for @reversedOrderedSet@
reversedOrderedSetSelector :: Selector
reversedOrderedSetSelector = mkSelector "reversedOrderedSet"

-- | @Selector@ for @array@
arraySelector :: Selector
arraySelector = mkSelector "array"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

