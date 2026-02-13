{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Array		***************
--
-- Generated bindings for @NSArray@.
module ObjC.Foundation.NSArray
  ( NSArray
  , IsNSArray(..)
  , objectAtIndex
  , init_
  , initWithObjects_count
  , initWithCoder
  , filteredArrayUsingPredicate
  , sortedArrayUsingDescriptors
  , addObserver_toObjectsAtIndexes_forKeyPath_options_context
  , removeObserver_fromObjectsAtIndexes_forKeyPath_context
  , removeObserver_fromObjectsAtIndexes_forKeyPath
  , addObserver_forKeyPath_options_context
  , removeObserver_forKeyPath_context
  , removeObserver_forKeyPath
  , valueForKey
  , setValue_forKey
  , pathsMatchingExtensions
  , getObjects
  , arrayWithContentsOfFile
  , arrayWithContentsOfURL
  , initWithContentsOfFile
  , initWithContentsOfURL
  , writeToFile_atomically
  , writeToURL_atomically
  , differenceFromArray_withOptions_usingEquivalenceTest
  , differenceFromArray_withOptions
  , differenceFromArray
  , arrayByApplyingDifference
  , array
  , arrayWithObject
  , arrayWithObjects_count
  , arrayWithObjects
  , arrayWithArray
  , initWithObjects
  , initWithArray
  , initWithArray_copyItems
  , initWithContentsOfURL_error
  , arrayWithContentsOfURL_error
  , arrayByAddingObject
  , arrayByAddingObjectsFromArray
  , componentsJoinedByString
  , containsObject
  , descriptionWithLocale
  , descriptionWithLocale_indent
  , firstObjectCommonWithArray
  , getObjects_range
  , indexOfObject
  , indexOfObject_inRange
  , indexOfObjectIdenticalTo
  , indexOfObjectIdenticalTo_inRange
  , isEqualToArray
  , objectEnumerator
  , reverseObjectEnumerator
  , sortedArrayUsingFunction_context
  , sortedArrayUsingFunction_context_hint
  , sortedArrayUsingSelector
  , subarrayWithRange
  , writeToURL_error
  , makeObjectsPerformSelector
  , makeObjectsPerformSelector_withObject
  , objectsAtIndexes
  , objectAtIndexedSubscript
  , enumerateObjectsUsingBlock
  , enumerateObjectsWithOptions_usingBlock
  , enumerateObjectsAtIndexes_options_usingBlock
  , indexOfObjectPassingTest
  , indexOfObjectWithOptions_passingTest
  , indexOfObjectAtIndexes_options_passingTest
  , indexesOfObjectsPassingTest
  , indexesOfObjectsWithOptions_passingTest
  , indexesOfObjectsAtIndexes_options_passingTest
  , sortedArrayUsingComparator
  , sortedArrayWithOptions_usingComparator
  , indexOfObject_inSortedRange_options_usingComparator
  , count
  , description
  , firstObject
  , lastObject
  , sortedArrayHint
  , addObserver_forKeyPath_options_contextSelector
  , addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector
  , arrayByAddingObjectSelector
  , arrayByAddingObjectsFromArraySelector
  , arrayByApplyingDifferenceSelector
  , arraySelector
  , arrayWithArraySelector
  , arrayWithContentsOfFileSelector
  , arrayWithContentsOfURLSelector
  , arrayWithContentsOfURL_errorSelector
  , arrayWithObjectSelector
  , arrayWithObjectsSelector
  , arrayWithObjects_countSelector
  , componentsJoinedByStringSelector
  , containsObjectSelector
  , countSelector
  , descriptionSelector
  , descriptionWithLocaleSelector
  , descriptionWithLocale_indentSelector
  , differenceFromArraySelector
  , differenceFromArray_withOptionsSelector
  , differenceFromArray_withOptions_usingEquivalenceTestSelector
  , enumerateObjectsAtIndexes_options_usingBlockSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , filteredArrayUsingPredicateSelector
  , firstObjectCommonWithArraySelector
  , firstObjectSelector
  , getObjectsSelector
  , getObjects_rangeSelector
  , indexOfObjectAtIndexes_options_passingTestSelector
  , indexOfObjectIdenticalToSelector
  , indexOfObjectIdenticalTo_inRangeSelector
  , indexOfObjectPassingTestSelector
  , indexOfObjectSelector
  , indexOfObjectWithOptions_passingTestSelector
  , indexOfObject_inRangeSelector
  , indexOfObject_inSortedRange_options_usingComparatorSelector
  , indexesOfObjectsAtIndexes_options_passingTestSelector
  , indexesOfObjectsPassingTestSelector
  , indexesOfObjectsWithOptions_passingTestSelector
  , initSelector
  , initWithArraySelector
  , initWithArray_copyItemsSelector
  , initWithCoderSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , initWithContentsOfURL_errorSelector
  , initWithObjectsSelector
  , initWithObjects_countSelector
  , isEqualToArraySelector
  , lastObjectSelector
  , makeObjectsPerformSelectorSelector
  , makeObjectsPerformSelector_withObjectSelector
  , objectAtIndexSelector
  , objectAtIndexedSubscriptSelector
  , objectEnumeratorSelector
  , objectsAtIndexesSelector
  , pathsMatchingExtensionsSelector
  , removeObserver_forKeyPathSelector
  , removeObserver_forKeyPath_contextSelector
  , removeObserver_fromObjectsAtIndexes_forKeyPathSelector
  , removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector
  , reverseObjectEnumeratorSelector
  , setValue_forKeySelector
  , sortedArrayHintSelector
  , sortedArrayUsingComparatorSelector
  , sortedArrayUsingDescriptorsSelector
  , sortedArrayUsingFunction_contextSelector
  , sortedArrayUsingFunction_context_hintSelector
  , sortedArrayUsingSelectorSelector
  , sortedArrayWithOptions_usingComparatorSelector
  , subarrayWithRangeSelector
  , valueForKeySelector
  , writeToFile_atomicallySelector
  , writeToURL_atomicallySelector
  , writeToURL_errorSelector

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
objectAtIndex :: IsNSArray nsArray => nsArray -> CULong -> IO RawId
objectAtIndex nsArray index =
  sendMessage nsArray objectAtIndexSelector index

-- | @- init@
init_ :: IsNSArray nsArray => nsArray -> IO (Id NSArray)
init_ nsArray =
  sendOwnedMessage nsArray initSelector

-- | @- initWithObjects:count:@
initWithObjects_count :: IsNSArray nsArray => nsArray -> RawId -> CULong -> IO (Id NSArray)
initWithObjects_count nsArray objects cnt =
  sendOwnedMessage nsArray initWithObjects_countSelector objects cnt

-- | @- initWithCoder:@
initWithCoder :: (IsNSArray nsArray, IsNSCoder coder) => nsArray -> coder -> IO (Id NSArray)
initWithCoder nsArray coder =
  sendOwnedMessage nsArray initWithCoderSelector (toNSCoder coder)

-- | @- filteredArrayUsingPredicate:@
filteredArrayUsingPredicate :: (IsNSArray nsArray, IsNSPredicate predicate) => nsArray -> predicate -> IO (Id NSArray)
filteredArrayUsingPredicate nsArray predicate =
  sendMessage nsArray filteredArrayUsingPredicateSelector (toNSPredicate predicate)

-- | @- sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptors :: (IsNSArray nsArray, IsNSArray sortDescriptors) => nsArray -> sortDescriptors -> IO (Id NSArray)
sortedArrayUsingDescriptors nsArray sortDescriptors =
  sendMessage nsArray sortedArrayUsingDescriptorsSelector (toNSArray sortDescriptors)

-- | @- addObserver:toObjectsAtIndexes:forKeyPath:options:context:@
addObserver_toObjectsAtIndexes_forKeyPath_options_context :: (IsNSArray nsArray, IsNSObject observer, IsNSIndexSet indexes, IsNSString keyPath) => nsArray -> observer -> indexes -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_toObjectsAtIndexes_forKeyPath_options_context nsArray observer indexes keyPath options context =
  sendMessage nsArray addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector (toNSObject observer) (toNSIndexSet indexes) (toNSString keyPath) options context

-- | @- removeObserver:fromObjectsAtIndexes:forKeyPath:context:@
removeObserver_fromObjectsAtIndexes_forKeyPath_context :: (IsNSArray nsArray, IsNSObject observer, IsNSIndexSet indexes, IsNSString keyPath) => nsArray -> observer -> indexes -> keyPath -> Ptr () -> IO ()
removeObserver_fromObjectsAtIndexes_forKeyPath_context nsArray observer indexes keyPath context =
  sendMessage nsArray removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector (toNSObject observer) (toNSIndexSet indexes) (toNSString keyPath) context

-- | @- removeObserver:fromObjectsAtIndexes:forKeyPath:@
removeObserver_fromObjectsAtIndexes_forKeyPath :: (IsNSArray nsArray, IsNSObject observer, IsNSIndexSet indexes, IsNSString keyPath) => nsArray -> observer -> indexes -> keyPath -> IO ()
removeObserver_fromObjectsAtIndexes_forKeyPath nsArray observer indexes keyPath =
  sendMessage nsArray removeObserver_fromObjectsAtIndexes_forKeyPathSelector (toNSObject observer) (toNSIndexSet indexes) (toNSString keyPath)

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSArray nsArray, IsNSObject observer, IsNSString keyPath) => nsArray -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsArray observer keyPath options context =
  sendMessage nsArray addObserver_forKeyPath_options_contextSelector (toNSObject observer) (toNSString keyPath) options context

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSArray nsArray, IsNSObject observer, IsNSString keyPath) => nsArray -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsArray observer keyPath context =
  sendMessage nsArray removeObserver_forKeyPath_contextSelector (toNSObject observer) (toNSString keyPath) context

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSArray nsArray, IsNSObject observer, IsNSString keyPath) => nsArray -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsArray observer keyPath =
  sendMessage nsArray removeObserver_forKeyPathSelector (toNSObject observer) (toNSString keyPath)

-- | @- valueForKey:@
valueForKey :: (IsNSArray nsArray, IsNSString key) => nsArray -> key -> IO RawId
valueForKey nsArray key =
  sendMessage nsArray valueForKeySelector (toNSString key)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSArray nsArray, IsNSString key) => nsArray -> RawId -> key -> IO ()
setValue_forKey nsArray value key =
  sendMessage nsArray setValue_forKeySelector value (toNSString key)

-- | @- pathsMatchingExtensions:@
pathsMatchingExtensions :: (IsNSArray nsArray, IsNSArray filterTypes) => nsArray -> filterTypes -> IO (Id NSArray)
pathsMatchingExtensions nsArray filterTypes =
  sendMessage nsArray pathsMatchingExtensionsSelector (toNSArray filterTypes)

-- | @- getObjects:@
getObjects :: IsNSArray nsArray => nsArray -> RawId -> IO ()
getObjects nsArray objects =
  sendMessage nsArray getObjectsSelector objects

-- | @+ arrayWithContentsOfFile:@
arrayWithContentsOfFile :: IsNSString path => path -> IO (Id NSArray)
arrayWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithContentsOfFileSelector (toNSString path)

-- | @+ arrayWithContentsOfURL:@
arrayWithContentsOfURL :: IsNSURL url => url -> IO (Id NSArray)
arrayWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithContentsOfURLSelector (toNSURL url)

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSArray nsArray, IsNSString path) => nsArray -> path -> IO (Id NSArray)
initWithContentsOfFile nsArray path =
  sendOwnedMessage nsArray initWithContentsOfFileSelector (toNSString path)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSArray nsArray, IsNSURL url) => nsArray -> url -> IO (Id NSArray)
initWithContentsOfURL nsArray url =
  sendOwnedMessage nsArray initWithContentsOfURLSelector (toNSURL url)

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSArray nsArray, IsNSString path) => nsArray -> path -> Bool -> IO Bool
writeToFile_atomically nsArray path useAuxiliaryFile =
  sendMessage nsArray writeToFile_atomicallySelector (toNSString path) useAuxiliaryFile

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSArray nsArray, IsNSURL url) => nsArray -> url -> Bool -> IO Bool
writeToURL_atomically nsArray url atomically =
  sendMessage nsArray writeToURL_atomicallySelector (toNSURL url) atomically

-- | @- differenceFromArray:withOptions:usingEquivalenceTest:@
differenceFromArray_withOptions_usingEquivalenceTest :: (IsNSArray nsArray, IsNSArray other) => nsArray -> other -> NSOrderedCollectionDifferenceCalculationOptions -> Ptr () -> IO (Id NSOrderedCollectionDifference)
differenceFromArray_withOptions_usingEquivalenceTest nsArray other options block =
  sendMessage nsArray differenceFromArray_withOptions_usingEquivalenceTestSelector (toNSArray other) options block

-- | @- differenceFromArray:withOptions:@
differenceFromArray_withOptions :: (IsNSArray nsArray, IsNSArray other) => nsArray -> other -> NSOrderedCollectionDifferenceCalculationOptions -> IO (Id NSOrderedCollectionDifference)
differenceFromArray_withOptions nsArray other options =
  sendMessage nsArray differenceFromArray_withOptionsSelector (toNSArray other) options

-- | @- differenceFromArray:@
differenceFromArray :: (IsNSArray nsArray, IsNSArray other) => nsArray -> other -> IO (Id NSOrderedCollectionDifference)
differenceFromArray nsArray other =
  sendMessage nsArray differenceFromArraySelector (toNSArray other)

-- | @- arrayByApplyingDifference:@
arrayByApplyingDifference :: (IsNSArray nsArray, IsNSOrderedCollectionDifference difference) => nsArray -> difference -> IO (Id NSArray)
arrayByApplyingDifference nsArray difference =
  sendMessage nsArray arrayByApplyingDifferenceSelector (toNSOrderedCollectionDifference difference)

-- | @+ array@
array :: IO (Id NSArray)
array  =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arraySelector

-- | @+ arrayWithObject:@
arrayWithObject :: RawId -> IO (Id NSArray)
arrayWithObject anObject =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithObjectSelector anObject

-- | @+ arrayWithObjects:count:@
arrayWithObjects_count :: RawId -> CULong -> IO (Id NSArray)
arrayWithObjects_count objects cnt =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithObjects_countSelector objects cnt

-- | @+ arrayWithObjects:@
arrayWithObjects :: RawId -> IO (Id NSArray)
arrayWithObjects firstObj =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithObjectsSelector firstObj

-- | @+ arrayWithArray:@
arrayWithArray :: IsNSArray array => array -> IO (Id NSArray)
arrayWithArray array =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithArraySelector (toNSArray array)

-- | @- initWithObjects:@
initWithObjects :: IsNSArray nsArray => nsArray -> RawId -> IO (Id NSArray)
initWithObjects nsArray firstObj =
  sendOwnedMessage nsArray initWithObjectsSelector firstObj

-- | @- initWithArray:@
initWithArray :: (IsNSArray nsArray, IsNSArray array) => nsArray -> array -> IO (Id NSArray)
initWithArray nsArray array =
  sendOwnedMessage nsArray initWithArraySelector (toNSArray array)

-- | @- initWithArray:copyItems:@
initWithArray_copyItems :: (IsNSArray nsArray, IsNSArray array) => nsArray -> array -> Bool -> IO (Id NSArray)
initWithArray_copyItems nsArray array flag =
  sendOwnedMessage nsArray initWithArray_copyItemsSelector (toNSArray array) flag

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNSArray nsArray, IsNSURL url, IsNSError error_) => nsArray -> url -> error_ -> IO (Id NSArray)
initWithContentsOfURL_error nsArray url error_ =
  sendOwnedMessage nsArray initWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @+ arrayWithContentsOfURL:error:@
arrayWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSArray)
arrayWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMessage cls' arrayWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- arrayByAddingObject:@
arrayByAddingObject :: IsNSArray nsArray => nsArray -> RawId -> IO (Id NSArray)
arrayByAddingObject nsArray anObject =
  sendMessage nsArray arrayByAddingObjectSelector anObject

-- | @- arrayByAddingObjectsFromArray:@
arrayByAddingObjectsFromArray :: (IsNSArray nsArray, IsNSArray otherArray) => nsArray -> otherArray -> IO (Id NSArray)
arrayByAddingObjectsFromArray nsArray otherArray =
  sendMessage nsArray arrayByAddingObjectsFromArraySelector (toNSArray otherArray)

-- | @- componentsJoinedByString:@
componentsJoinedByString :: (IsNSArray nsArray, IsNSString separator) => nsArray -> separator -> IO (Id NSString)
componentsJoinedByString nsArray separator =
  sendMessage nsArray componentsJoinedByStringSelector (toNSString separator)

-- | @- containsObject:@
containsObject :: IsNSArray nsArray => nsArray -> RawId -> IO Bool
containsObject nsArray anObject =
  sendMessage nsArray containsObjectSelector anObject

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSArray nsArray => nsArray -> RawId -> IO (Id NSString)
descriptionWithLocale nsArray locale =
  sendMessage nsArray descriptionWithLocaleSelector locale

-- | @- descriptionWithLocale:indent:@
descriptionWithLocale_indent :: IsNSArray nsArray => nsArray -> RawId -> CULong -> IO (Id NSString)
descriptionWithLocale_indent nsArray locale level =
  sendMessage nsArray descriptionWithLocale_indentSelector locale level

-- | @- firstObjectCommonWithArray:@
firstObjectCommonWithArray :: (IsNSArray nsArray, IsNSArray otherArray) => nsArray -> otherArray -> IO RawId
firstObjectCommonWithArray nsArray otherArray =
  sendMessage nsArray firstObjectCommonWithArraySelector (toNSArray otherArray)

-- | @- getObjects:range:@
getObjects_range :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> IO ()
getObjects_range nsArray objects range =
  sendMessage nsArray getObjects_rangeSelector objects range

-- | @- indexOfObject:@
indexOfObject :: IsNSArray nsArray => nsArray -> RawId -> IO CULong
indexOfObject nsArray anObject =
  sendMessage nsArray indexOfObjectSelector anObject

-- | @- indexOfObject:inRange:@
indexOfObject_inRange :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> IO CULong
indexOfObject_inRange nsArray anObject range =
  sendMessage nsArray indexOfObject_inRangeSelector anObject range

-- | @- indexOfObjectIdenticalTo:@
indexOfObjectIdenticalTo :: IsNSArray nsArray => nsArray -> RawId -> IO CULong
indexOfObjectIdenticalTo nsArray anObject =
  sendMessage nsArray indexOfObjectIdenticalToSelector anObject

-- | @- indexOfObjectIdenticalTo:inRange:@
indexOfObjectIdenticalTo_inRange :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> IO CULong
indexOfObjectIdenticalTo_inRange nsArray anObject range =
  sendMessage nsArray indexOfObjectIdenticalTo_inRangeSelector anObject range

-- | @- isEqualToArray:@
isEqualToArray :: (IsNSArray nsArray, IsNSArray otherArray) => nsArray -> otherArray -> IO Bool
isEqualToArray nsArray otherArray =
  sendMessage nsArray isEqualToArraySelector (toNSArray otherArray)

-- | @- objectEnumerator@
objectEnumerator :: IsNSArray nsArray => nsArray -> IO (Id NSEnumerator)
objectEnumerator nsArray =
  sendMessage nsArray objectEnumeratorSelector

-- | @- reverseObjectEnumerator@
reverseObjectEnumerator :: IsNSArray nsArray => nsArray -> IO (Id NSEnumerator)
reverseObjectEnumerator nsArray =
  sendMessage nsArray reverseObjectEnumeratorSelector

-- | @- sortedArrayUsingFunction:context:@
sortedArrayUsingFunction_context :: IsNSArray nsArray => nsArray -> Ptr () -> Ptr () -> IO (Id NSArray)
sortedArrayUsingFunction_context nsArray comparator context =
  sendMessage nsArray sortedArrayUsingFunction_contextSelector comparator context

-- | @- sortedArrayUsingFunction:context:hint:@
sortedArrayUsingFunction_context_hint :: (IsNSArray nsArray, IsNSData hint) => nsArray -> Ptr () -> Ptr () -> hint -> IO (Id NSArray)
sortedArrayUsingFunction_context_hint nsArray comparator context hint =
  sendMessage nsArray sortedArrayUsingFunction_context_hintSelector comparator context (toNSData hint)

-- | @- sortedArrayUsingSelector:@
sortedArrayUsingSelector :: IsNSArray nsArray => nsArray -> Sel -> IO (Id NSArray)
sortedArrayUsingSelector nsArray comparator =
  sendMessage nsArray sortedArrayUsingSelectorSelector comparator

-- | @- subarrayWithRange:@
subarrayWithRange :: IsNSArray nsArray => nsArray -> NSRange -> IO (Id NSArray)
subarrayWithRange nsArray range =
  sendMessage nsArray subarrayWithRangeSelector range

-- | @- writeToURL:error:@
writeToURL_error :: (IsNSArray nsArray, IsNSURL url, IsNSError error_) => nsArray -> url -> error_ -> IO Bool
writeToURL_error nsArray url error_ =
  sendMessage nsArray writeToURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- makeObjectsPerformSelector:@
makeObjectsPerformSelector :: IsNSArray nsArray => nsArray -> Sel -> IO ()
makeObjectsPerformSelector nsArray aSelector =
  sendMessage nsArray makeObjectsPerformSelectorSelector aSelector

-- | @- makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObject :: IsNSArray nsArray => nsArray -> Sel -> RawId -> IO ()
makeObjectsPerformSelector_withObject nsArray aSelector argument =
  sendMessage nsArray makeObjectsPerformSelector_withObjectSelector aSelector argument

-- | @- objectsAtIndexes:@
objectsAtIndexes :: (IsNSArray nsArray, IsNSIndexSet indexes) => nsArray -> indexes -> IO (Id NSArray)
objectsAtIndexes nsArray indexes =
  sendMessage nsArray objectsAtIndexesSelector (toNSIndexSet indexes)

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsNSArray nsArray => nsArray -> CULong -> IO RawId
objectAtIndexedSubscript nsArray idx =
  sendMessage nsArray objectAtIndexedSubscriptSelector idx

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsNSArray nsArray => nsArray -> Ptr () -> IO ()
enumerateObjectsUsingBlock nsArray block =
  sendMessage nsArray enumerateObjectsUsingBlockSelector block

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsNSArray nsArray => nsArray -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock nsArray opts block =
  sendMessage nsArray enumerateObjectsWithOptions_usingBlockSelector opts block

-- | @- enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlock :: (IsNSArray nsArray, IsNSIndexSet s) => nsArray -> s -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsAtIndexes_options_usingBlock nsArray s opts block =
  sendMessage nsArray enumerateObjectsAtIndexes_options_usingBlockSelector (toNSIndexSet s) opts block

-- | @- indexOfObjectPassingTest:@
indexOfObjectPassingTest :: IsNSArray nsArray => nsArray -> Ptr () -> IO CULong
indexOfObjectPassingTest nsArray predicate =
  sendMessage nsArray indexOfObjectPassingTestSelector predicate

-- | @- indexOfObjectWithOptions:passingTest:@
indexOfObjectWithOptions_passingTest :: IsNSArray nsArray => nsArray -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectWithOptions_passingTest nsArray opts predicate =
  sendMessage nsArray indexOfObjectWithOptions_passingTestSelector opts predicate

-- | @- indexOfObjectAtIndexes:options:passingTest:@
indexOfObjectAtIndexes_options_passingTest :: (IsNSArray nsArray, IsNSIndexSet s) => nsArray -> s -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectAtIndexes_options_passingTest nsArray s opts predicate =
  sendMessage nsArray indexOfObjectAtIndexes_options_passingTestSelector (toNSIndexSet s) opts predicate

-- | @- indexesOfObjectsPassingTest:@
indexesOfObjectsPassingTest :: IsNSArray nsArray => nsArray -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsPassingTest nsArray predicate =
  sendMessage nsArray indexesOfObjectsPassingTestSelector predicate

-- | @- indexesOfObjectsWithOptions:passingTest:@
indexesOfObjectsWithOptions_passingTest :: IsNSArray nsArray => nsArray -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsWithOptions_passingTest nsArray opts predicate =
  sendMessage nsArray indexesOfObjectsWithOptions_passingTestSelector opts predicate

-- | @- indexesOfObjectsAtIndexes:options:passingTest:@
indexesOfObjectsAtIndexes_options_passingTest :: (IsNSArray nsArray, IsNSIndexSet s) => nsArray -> s -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsAtIndexes_options_passingTest nsArray s opts predicate =
  sendMessage nsArray indexesOfObjectsAtIndexes_options_passingTestSelector (toNSIndexSet s) opts predicate

-- | @- sortedArrayUsingComparator:@
sortedArrayUsingComparator :: IsNSArray nsArray => nsArray -> Ptr () -> IO (Id NSArray)
sortedArrayUsingComparator nsArray cmptr =
  sendMessage nsArray sortedArrayUsingComparatorSelector cmptr

-- | @- sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparator :: IsNSArray nsArray => nsArray -> NSSortOptions -> Ptr () -> IO (Id NSArray)
sortedArrayWithOptions_usingComparator nsArray opts cmptr =
  sendMessage nsArray sortedArrayWithOptions_usingComparatorSelector opts cmptr

-- | @- indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparator :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> NSBinarySearchingOptions -> Ptr () -> IO CULong
indexOfObject_inSortedRange_options_usingComparator nsArray obj_ r opts cmp =
  sendMessage nsArray indexOfObject_inSortedRange_options_usingComparatorSelector obj_ r opts cmp

-- | @- count@
count :: IsNSArray nsArray => nsArray -> IO CULong
count nsArray =
  sendMessage nsArray countSelector

-- | @- description@
description :: IsNSArray nsArray => nsArray -> IO (Id NSString)
description nsArray =
  sendMessage nsArray descriptionSelector

-- | @- firstObject@
firstObject :: IsNSArray nsArray => nsArray -> IO RawId
firstObject nsArray =
  sendMessage nsArray firstObjectSelector

-- | @- lastObject@
lastObject :: IsNSArray nsArray => nsArray -> IO RawId
lastObject nsArray =
  sendMessage nsArray lastObjectSelector

-- | @- sortedArrayHint@
sortedArrayHint :: IsNSArray nsArray => nsArray -> IO (Id NSData)
sortedArrayHint nsArray =
  sendMessage nsArray sortedArrayHintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector '[CULong] RawId
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSArray)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:count:@
initWithObjects_countSelector :: Selector '[RawId, CULong] (Id NSArray)
initWithObjects_countSelector = mkSelector "initWithObjects:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSArray)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filteredArrayUsingPredicate:@
filteredArrayUsingPredicateSelector :: Selector '[Id NSPredicate] (Id NSArray)
filteredArrayUsingPredicateSelector = mkSelector "filteredArrayUsingPredicate:"

-- | @Selector@ for @sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptorsSelector :: Selector '[Id NSArray] (Id NSArray)
sortedArrayUsingDescriptorsSelector = mkSelector "sortedArrayUsingDescriptors:"

-- | @Selector@ for @addObserver:toObjectsAtIndexes:forKeyPath:options:context:@
addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector :: Selector '[Id NSObject, Id NSIndexSet, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector = mkSelector "addObserver:toObjectsAtIndexes:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:fromObjectsAtIndexes:forKeyPath:context:@
removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector :: Selector '[Id NSObject, Id NSIndexSet, Id NSString, Ptr ()] ()
removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector = mkSelector "removeObserver:fromObjectsAtIndexes:forKeyPath:context:"

-- | @Selector@ for @removeObserver:fromObjectsAtIndexes:forKeyPath:@
removeObserver_fromObjectsAtIndexes_forKeyPathSelector :: Selector '[Id NSObject, Id NSIndexSet, Id NSString] ()
removeObserver_fromObjectsAtIndexes_forKeyPathSelector = mkSelector "removeObserver:fromObjectsAtIndexes:forKeyPath:"

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

-- | @Selector@ for @pathsMatchingExtensions:@
pathsMatchingExtensionsSelector :: Selector '[Id NSArray] (Id NSArray)
pathsMatchingExtensionsSelector = mkSelector "pathsMatchingExtensions:"

-- | @Selector@ for @getObjects:@
getObjectsSelector :: Selector '[RawId] ()
getObjectsSelector = mkSelector "getObjects:"

-- | @Selector@ for @arrayWithContentsOfFile:@
arrayWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSArray)
arrayWithContentsOfFileSelector = mkSelector "arrayWithContentsOfFile:"

-- | @Selector@ for @arrayWithContentsOfURL:@
arrayWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSArray)
arrayWithContentsOfURLSelector = mkSelector "arrayWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSArray)
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSArray)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector '[Id NSString, Bool] Bool
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector '[Id NSURL, Bool] Bool
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @differenceFromArray:withOptions:usingEquivalenceTest:@
differenceFromArray_withOptions_usingEquivalenceTestSelector :: Selector '[Id NSArray, NSOrderedCollectionDifferenceCalculationOptions, Ptr ()] (Id NSOrderedCollectionDifference)
differenceFromArray_withOptions_usingEquivalenceTestSelector = mkSelector "differenceFromArray:withOptions:usingEquivalenceTest:"

-- | @Selector@ for @differenceFromArray:withOptions:@
differenceFromArray_withOptionsSelector :: Selector '[Id NSArray, NSOrderedCollectionDifferenceCalculationOptions] (Id NSOrderedCollectionDifference)
differenceFromArray_withOptionsSelector = mkSelector "differenceFromArray:withOptions:"

-- | @Selector@ for @differenceFromArray:@
differenceFromArraySelector :: Selector '[Id NSArray] (Id NSOrderedCollectionDifference)
differenceFromArraySelector = mkSelector "differenceFromArray:"

-- | @Selector@ for @arrayByApplyingDifference:@
arrayByApplyingDifferenceSelector :: Selector '[Id NSOrderedCollectionDifference] (Id NSArray)
arrayByApplyingDifferenceSelector = mkSelector "arrayByApplyingDifference:"

-- | @Selector@ for @array@
arraySelector :: Selector '[] (Id NSArray)
arraySelector = mkSelector "array"

-- | @Selector@ for @arrayWithObject:@
arrayWithObjectSelector :: Selector '[RawId] (Id NSArray)
arrayWithObjectSelector = mkSelector "arrayWithObject:"

-- | @Selector@ for @arrayWithObjects:count:@
arrayWithObjects_countSelector :: Selector '[RawId, CULong] (Id NSArray)
arrayWithObjects_countSelector = mkSelector "arrayWithObjects:count:"

-- | @Selector@ for @arrayWithObjects:@
arrayWithObjectsSelector :: Selector '[RawId] (Id NSArray)
arrayWithObjectsSelector = mkSelector "arrayWithObjects:"

-- | @Selector@ for @arrayWithArray:@
arrayWithArraySelector :: Selector '[Id NSArray] (Id NSArray)
arrayWithArraySelector = mkSelector "arrayWithArray:"

-- | @Selector@ for @initWithObjects:@
initWithObjectsSelector :: Selector '[RawId] (Id NSArray)
initWithObjectsSelector = mkSelector "initWithObjects:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector '[Id NSArray] (Id NSArray)
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @initWithArray:copyItems:@
initWithArray_copyItemsSelector :: Selector '[Id NSArray, Bool] (Id NSArray)
initWithArray_copyItemsSelector = mkSelector "initWithArray:copyItems:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSArray)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @arrayWithContentsOfURL:error:@
arrayWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSArray)
arrayWithContentsOfURL_errorSelector = mkSelector "arrayWithContentsOfURL:error:"

-- | @Selector@ for @arrayByAddingObject:@
arrayByAddingObjectSelector :: Selector '[RawId] (Id NSArray)
arrayByAddingObjectSelector = mkSelector "arrayByAddingObject:"

-- | @Selector@ for @arrayByAddingObjectsFromArray:@
arrayByAddingObjectsFromArraySelector :: Selector '[Id NSArray] (Id NSArray)
arrayByAddingObjectsFromArraySelector = mkSelector "arrayByAddingObjectsFromArray:"

-- | @Selector@ for @componentsJoinedByString:@
componentsJoinedByStringSelector :: Selector '[Id NSString] (Id NSString)
componentsJoinedByStringSelector = mkSelector "componentsJoinedByString:"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector '[RawId] Bool
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @descriptionWithLocale:indent:@
descriptionWithLocale_indentSelector :: Selector '[RawId, CULong] (Id NSString)
descriptionWithLocale_indentSelector = mkSelector "descriptionWithLocale:indent:"

-- | @Selector@ for @firstObjectCommonWithArray:@
firstObjectCommonWithArraySelector :: Selector '[Id NSArray] RawId
firstObjectCommonWithArraySelector = mkSelector "firstObjectCommonWithArray:"

-- | @Selector@ for @getObjects:range:@
getObjects_rangeSelector :: Selector '[RawId, NSRange] ()
getObjects_rangeSelector = mkSelector "getObjects:range:"

-- | @Selector@ for @indexOfObject:@
indexOfObjectSelector :: Selector '[RawId] CULong
indexOfObjectSelector = mkSelector "indexOfObject:"

-- | @Selector@ for @indexOfObject:inRange:@
indexOfObject_inRangeSelector :: Selector '[RawId, NSRange] CULong
indexOfObject_inRangeSelector = mkSelector "indexOfObject:inRange:"

-- | @Selector@ for @indexOfObjectIdenticalTo:@
indexOfObjectIdenticalToSelector :: Selector '[RawId] CULong
indexOfObjectIdenticalToSelector = mkSelector "indexOfObjectIdenticalTo:"

-- | @Selector@ for @indexOfObjectIdenticalTo:inRange:@
indexOfObjectIdenticalTo_inRangeSelector :: Selector '[RawId, NSRange] CULong
indexOfObjectIdenticalTo_inRangeSelector = mkSelector "indexOfObjectIdenticalTo:inRange:"

-- | @Selector@ for @isEqualToArray:@
isEqualToArraySelector :: Selector '[Id NSArray] Bool
isEqualToArraySelector = mkSelector "isEqualToArray:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @reverseObjectEnumerator@
reverseObjectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
reverseObjectEnumeratorSelector = mkSelector "reverseObjectEnumerator"

-- | @Selector@ for @sortedArrayUsingFunction:context:@
sortedArrayUsingFunction_contextSelector :: Selector '[Ptr (), Ptr ()] (Id NSArray)
sortedArrayUsingFunction_contextSelector = mkSelector "sortedArrayUsingFunction:context:"

-- | @Selector@ for @sortedArrayUsingFunction:context:hint:@
sortedArrayUsingFunction_context_hintSelector :: Selector '[Ptr (), Ptr (), Id NSData] (Id NSArray)
sortedArrayUsingFunction_context_hintSelector = mkSelector "sortedArrayUsingFunction:context:hint:"

-- | @Selector@ for @sortedArrayUsingSelector:@
sortedArrayUsingSelectorSelector :: Selector '[Sel] (Id NSArray)
sortedArrayUsingSelectorSelector = mkSelector "sortedArrayUsingSelector:"

-- | @Selector@ for @subarrayWithRange:@
subarrayWithRangeSelector :: Selector '[NSRange] (Id NSArray)
subarrayWithRangeSelector = mkSelector "subarrayWithRange:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @makeObjectsPerformSelector:@
makeObjectsPerformSelectorSelector :: Selector '[Sel] ()
makeObjectsPerformSelectorSelector = mkSelector "makeObjectsPerformSelector:"

-- | @Selector@ for @makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObjectSelector :: Selector '[Sel, RawId] ()
makeObjectsPerformSelector_withObjectSelector = mkSelector "makeObjectsPerformSelector:withObject:"

-- | @Selector@ for @objectsAtIndexes:@
objectsAtIndexesSelector :: Selector '[Id NSIndexSet] (Id NSArray)
objectsAtIndexesSelector = mkSelector "objectsAtIndexes:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] RawId
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

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

-- | @Selector@ for @sortedArrayUsingComparator:@
sortedArrayUsingComparatorSelector :: Selector '[Ptr ()] (Id NSArray)
sortedArrayUsingComparatorSelector = mkSelector "sortedArrayUsingComparator:"

-- | @Selector@ for @sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparatorSelector :: Selector '[NSSortOptions, Ptr ()] (Id NSArray)
sortedArrayWithOptions_usingComparatorSelector = mkSelector "sortedArrayWithOptions:usingComparator:"

-- | @Selector@ for @indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparatorSelector :: Selector '[RawId, NSRange, NSBinarySearchingOptions, Ptr ()] CULong
indexOfObject_inSortedRange_options_usingComparatorSelector = mkSelector "indexOfObject:inSortedRange:options:usingComparator:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @firstObject@
firstObjectSelector :: Selector '[] RawId
firstObjectSelector = mkSelector "firstObject"

-- | @Selector@ for @lastObject@
lastObjectSelector :: Selector '[] RawId
lastObjectSelector = mkSelector "lastObject"

-- | @Selector@ for @sortedArrayHint@
sortedArrayHintSelector :: Selector '[] (Id NSData)
sortedArrayHintSelector = mkSelector "sortedArrayHint"

