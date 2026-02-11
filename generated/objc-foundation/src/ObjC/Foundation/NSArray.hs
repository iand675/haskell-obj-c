{-# LANGUAGE PatternSynonyms #-}
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
  , objectAtIndexSelector
  , initSelector
  , initWithObjects_countSelector
  , initWithCoderSelector
  , filteredArrayUsingPredicateSelector
  , sortedArrayUsingDescriptorsSelector
  , addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector
  , removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector
  , removeObserver_fromObjectsAtIndexes_forKeyPathSelector
  , addObserver_forKeyPath_options_contextSelector
  , removeObserver_forKeyPath_contextSelector
  , removeObserver_forKeyPathSelector
  , valueForKeySelector
  , setValue_forKeySelector
  , pathsMatchingExtensionsSelector
  , getObjectsSelector
  , arrayWithContentsOfFileSelector
  , arrayWithContentsOfURLSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , writeToFile_atomicallySelector
  , writeToURL_atomicallySelector
  , differenceFromArray_withOptions_usingEquivalenceTestSelector
  , differenceFromArray_withOptionsSelector
  , differenceFromArraySelector
  , arrayByApplyingDifferenceSelector
  , arraySelector
  , arrayWithObjectSelector
  , arrayWithObjects_countSelector
  , arrayWithObjectsSelector
  , arrayWithArraySelector
  , initWithObjectsSelector
  , initWithArraySelector
  , initWithArray_copyItemsSelector
  , initWithContentsOfURL_errorSelector
  , arrayWithContentsOfURL_errorSelector
  , arrayByAddingObjectSelector
  , arrayByAddingObjectsFromArraySelector
  , componentsJoinedByStringSelector
  , containsObjectSelector
  , descriptionWithLocaleSelector
  , descriptionWithLocale_indentSelector
  , firstObjectCommonWithArraySelector
  , getObjects_rangeSelector
  , indexOfObjectSelector
  , indexOfObject_inRangeSelector
  , indexOfObjectIdenticalToSelector
  , indexOfObjectIdenticalTo_inRangeSelector
  , isEqualToArraySelector
  , objectEnumeratorSelector
  , reverseObjectEnumeratorSelector
  , sortedArrayUsingFunction_contextSelector
  , sortedArrayUsingFunction_context_hintSelector
  , sortedArrayUsingSelectorSelector
  , subarrayWithRangeSelector
  , writeToURL_errorSelector
  , makeObjectsPerformSelectorSelector
  , makeObjectsPerformSelector_withObjectSelector
  , objectsAtIndexesSelector
  , objectAtIndexedSubscriptSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , enumerateObjectsAtIndexes_options_usingBlockSelector
  , indexOfObjectPassingTestSelector
  , indexOfObjectWithOptions_passingTestSelector
  , indexOfObjectAtIndexes_options_passingTestSelector
  , indexesOfObjectsPassingTestSelector
  , indexesOfObjectsWithOptions_passingTestSelector
  , indexesOfObjectsAtIndexes_options_passingTestSelector
  , sortedArrayUsingComparatorSelector
  , sortedArrayWithOptions_usingComparatorSelector
  , indexOfObject_inSortedRange_options_usingComparatorSelector
  , countSelector
  , descriptionSelector
  , firstObjectSelector
  , lastObjectSelector
  , sortedArrayHintSelector

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
objectAtIndex :: IsNSArray nsArray => nsArray -> CULong -> IO RawId
objectAtIndex nsArray  index =
  fmap (RawId . castPtr) $ sendMsg nsArray (mkSelector "objectAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- init@
init_ :: IsNSArray nsArray => nsArray -> IO (Id NSArray)
init_ nsArray  =
  sendMsg nsArray (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithObjects:count:@
initWithObjects_count :: IsNSArray nsArray => nsArray -> RawId -> CULong -> IO (Id NSArray)
initWithObjects_count nsArray  objects cnt =
  sendMsg nsArray (mkSelector "initWithObjects:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral cnt)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSArray nsArray, IsNSCoder coder) => nsArray -> coder -> IO (Id NSArray)
initWithCoder nsArray  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsArray (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- filteredArrayUsingPredicate:@
filteredArrayUsingPredicate :: (IsNSArray nsArray, IsNSPredicate predicate) => nsArray -> predicate -> IO (Id NSArray)
filteredArrayUsingPredicate nsArray  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsArray (mkSelector "filteredArrayUsingPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptors :: (IsNSArray nsArray, IsNSArray sortDescriptors) => nsArray -> sortDescriptors -> IO (Id NSArray)
sortedArrayUsingDescriptors nsArray  sortDescriptors =
withObjCPtr sortDescriptors $ \raw_sortDescriptors ->
    sendMsg nsArray (mkSelector "sortedArrayUsingDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_sortDescriptors :: Ptr ())] >>= retainedObject . castPtr

-- | @- addObserver:toObjectsAtIndexes:forKeyPath:options:context:@
addObserver_toObjectsAtIndexes_forKeyPath_options_context :: (IsNSArray nsArray, IsNSObject observer, IsNSIndexSet indexes, IsNSString keyPath) => nsArray -> observer -> indexes -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_toObjectsAtIndexes_forKeyPath_options_context nsArray  observer indexes keyPath options context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr indexes $ \raw_indexes ->
    withObjCPtr keyPath $ \raw_keyPath ->
        sendMsg nsArray (mkSelector "addObserver:toObjectsAtIndexes:forKeyPath:options:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argCULong (coerce options), argPtr context]

-- | @- removeObserver:fromObjectsAtIndexes:forKeyPath:context:@
removeObserver_fromObjectsAtIndexes_forKeyPath_context :: (IsNSArray nsArray, IsNSObject observer, IsNSIndexSet indexes, IsNSString keyPath) => nsArray -> observer -> indexes -> keyPath -> Ptr () -> IO ()
removeObserver_fromObjectsAtIndexes_forKeyPath_context nsArray  observer indexes keyPath context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr indexes $ \raw_indexes ->
    withObjCPtr keyPath $ \raw_keyPath ->
        sendMsg nsArray (mkSelector "removeObserver:fromObjectsAtIndexes:forKeyPath:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr context]

-- | @- removeObserver:fromObjectsAtIndexes:forKeyPath:@
removeObserver_fromObjectsAtIndexes_forKeyPath :: (IsNSArray nsArray, IsNSObject observer, IsNSIndexSet indexes, IsNSString keyPath) => nsArray -> observer -> indexes -> keyPath -> IO ()
removeObserver_fromObjectsAtIndexes_forKeyPath nsArray  observer indexes keyPath =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr indexes $ \raw_indexes ->
    withObjCPtr keyPath $ \raw_keyPath ->
        sendMsg nsArray (mkSelector "removeObserver:fromObjectsAtIndexes:forKeyPath:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSArray nsArray, IsNSObject observer, IsNSString keyPath) => nsArray -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsArray  observer keyPath options context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsArray (mkSelector "addObserver:forKeyPath:options:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argCULong (coerce options), argPtr context]

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSArray nsArray, IsNSObject observer, IsNSString keyPath) => nsArray -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsArray  observer keyPath context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsArray (mkSelector "removeObserver:forKeyPath:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr context]

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSArray nsArray, IsNSObject observer, IsNSString keyPath) => nsArray -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsArray  observer keyPath =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsArray (mkSelector "removeObserver:forKeyPath:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- valueForKey:@
valueForKey :: (IsNSArray nsArray, IsNSString key) => nsArray -> key -> IO RawId
valueForKey nsArray  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsArray (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSArray nsArray, IsNSString key) => nsArray -> RawId -> key -> IO ()
setValue_forKey nsArray  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsArray (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- pathsMatchingExtensions:@
pathsMatchingExtensions :: (IsNSArray nsArray, IsNSArray filterTypes) => nsArray -> filterTypes -> IO (Id NSArray)
pathsMatchingExtensions nsArray  filterTypes =
withObjCPtr filterTypes $ \raw_filterTypes ->
    sendMsg nsArray (mkSelector "pathsMatchingExtensions:") (retPtr retVoid) [argPtr (castPtr raw_filterTypes :: Ptr ())] >>= retainedObject . castPtr

-- | @- getObjects:@
getObjects :: IsNSArray nsArray => nsArray -> RawId -> IO ()
getObjects nsArray  objects =
  sendMsg nsArray (mkSelector "getObjects:") retVoid [argPtr (castPtr (unRawId objects) :: Ptr ())]

-- | @+ arrayWithContentsOfFile:@
arrayWithContentsOfFile :: IsNSString path => path -> IO (Id NSArray)
arrayWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSArray"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "arrayWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ arrayWithContentsOfURL:@
arrayWithContentsOfURL :: IsNSURL url => url -> IO (Id NSArray)
arrayWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSArray"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "arrayWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSArray nsArray, IsNSString path) => nsArray -> path -> IO (Id NSArray)
initWithContentsOfFile nsArray  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsArray (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSArray nsArray, IsNSURL url) => nsArray -> url -> IO (Id NSArray)
initWithContentsOfURL nsArray  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsArray (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSArray nsArray, IsNSString path) => nsArray -> path -> Bool -> IO Bool
writeToFile_atomically nsArray  path useAuxiliaryFile =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArray (mkSelector "writeToFile:atomically:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if useAuxiliaryFile then 1 else 0)]

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSArray nsArray, IsNSURL url) => nsArray -> url -> Bool -> IO Bool
writeToURL_atomically nsArray  url atomically =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArray (mkSelector "writeToURL:atomically:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (if atomically then 1 else 0)]

-- | @- differenceFromArray:withOptions:usingEquivalenceTest:@
differenceFromArray_withOptions_usingEquivalenceTest :: (IsNSArray nsArray, IsNSArray other) => nsArray -> other -> NSOrderedCollectionDifferenceCalculationOptions -> Ptr () -> IO (Id NSOrderedCollectionDifference)
differenceFromArray_withOptions_usingEquivalenceTest nsArray  other options block =
withObjCPtr other $ \raw_other ->
    sendMsg nsArray (mkSelector "differenceFromArray:withOptions:usingEquivalenceTest:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ()), argCULong (coerce options), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | @- differenceFromArray:withOptions:@
differenceFromArray_withOptions :: (IsNSArray nsArray, IsNSArray other) => nsArray -> other -> NSOrderedCollectionDifferenceCalculationOptions -> IO (Id NSOrderedCollectionDifference)
differenceFromArray_withOptions nsArray  other options =
withObjCPtr other $ \raw_other ->
    sendMsg nsArray (mkSelector "differenceFromArray:withOptions:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- differenceFromArray:@
differenceFromArray :: (IsNSArray nsArray, IsNSArray other) => nsArray -> other -> IO (Id NSOrderedCollectionDifference)
differenceFromArray nsArray  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsArray (mkSelector "differenceFromArray:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ())] >>= retainedObject . castPtr

-- | @- arrayByApplyingDifference:@
arrayByApplyingDifference :: (IsNSArray nsArray, IsNSOrderedCollectionDifference difference) => nsArray -> difference -> IO (Id NSArray)
arrayByApplyingDifference nsArray  difference =
withObjCPtr difference $ \raw_difference ->
    sendMsg nsArray (mkSelector "arrayByApplyingDifference:") (retPtr retVoid) [argPtr (castPtr raw_difference :: Ptr ())] >>= retainedObject . castPtr

-- | @+ array@
array :: IO (Id NSArray)
array  =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMsg cls' (mkSelector "array") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ arrayWithObject:@
arrayWithObject :: RawId -> IO (Id NSArray)
arrayWithObject anObject =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMsg cls' (mkSelector "arrayWithObject:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ arrayWithObjects:count:@
arrayWithObjects_count :: RawId -> CULong -> IO (Id NSArray)
arrayWithObjects_count objects cnt =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMsg cls' (mkSelector "arrayWithObjects:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral cnt)] >>= retainedObject . castPtr

-- | @+ arrayWithObjects:@
arrayWithObjects :: RawId -> IO (Id NSArray)
arrayWithObjects firstObj =
  do
    cls' <- getRequiredClass "NSArray"
    sendClassMsg cls' (mkSelector "arrayWithObjects:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObj) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ arrayWithArray:@
arrayWithArray :: IsNSArray array => array -> IO (Id NSArray)
arrayWithArray array =
  do
    cls' <- getRequiredClass "NSArray"
    withObjCPtr array $ \raw_array ->
      sendClassMsg cls' (mkSelector "arrayWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithObjects:@
initWithObjects :: IsNSArray nsArray => nsArray -> RawId -> IO (Id NSArray)
initWithObjects nsArray  firstObj =
  sendMsg nsArray (mkSelector "initWithObjects:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObj) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithArray:@
initWithArray :: (IsNSArray nsArray, IsNSArray array) => nsArray -> array -> IO (Id NSArray)
initWithArray nsArray  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsArray (mkSelector "initWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithArray:copyItems:@
initWithArray_copyItems :: (IsNSArray nsArray, IsNSArray array) => nsArray -> array -> Bool -> IO (Id NSArray)
initWithArray_copyItems nsArray  array flag =
withObjCPtr array $ \raw_array ->
    sendMsg nsArray (mkSelector "initWithArray:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ()), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNSArray nsArray, IsNSURL url, IsNSError error_) => nsArray -> url -> error_ -> IO (Id NSArray)
initWithContentsOfURL_error nsArray  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsArray (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ arrayWithContentsOfURL:error:@
arrayWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSArray)
arrayWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NSArray"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "arrayWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- arrayByAddingObject:@
arrayByAddingObject :: IsNSArray nsArray => nsArray -> RawId -> IO (Id NSArray)
arrayByAddingObject nsArray  anObject =
  sendMsg nsArray (mkSelector "arrayByAddingObject:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @- arrayByAddingObjectsFromArray:@
arrayByAddingObjectsFromArray :: (IsNSArray nsArray, IsNSArray otherArray) => nsArray -> otherArray -> IO (Id NSArray)
arrayByAddingObjectsFromArray nsArray  otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    sendMsg nsArray (mkSelector "arrayByAddingObjectsFromArray:") (retPtr retVoid) [argPtr (castPtr raw_otherArray :: Ptr ())] >>= retainedObject . castPtr

-- | @- componentsJoinedByString:@
componentsJoinedByString :: (IsNSArray nsArray, IsNSString separator) => nsArray -> separator -> IO (Id NSString)
componentsJoinedByString nsArray  separator =
withObjCPtr separator $ \raw_separator ->
    sendMsg nsArray (mkSelector "componentsJoinedByString:") (retPtr retVoid) [argPtr (castPtr raw_separator :: Ptr ())] >>= retainedObject . castPtr

-- | @- containsObject:@
containsObject :: IsNSArray nsArray => nsArray -> RawId -> IO Bool
containsObject nsArray  anObject =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArray (mkSelector "containsObject:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSArray nsArray => nsArray -> RawId -> IO (Id NSString)
descriptionWithLocale nsArray  locale =
  sendMsg nsArray (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithLocale:indent:@
descriptionWithLocale_indent :: IsNSArray nsArray => nsArray -> RawId -> CULong -> IO (Id NSString)
descriptionWithLocale_indent nsArray  locale level =
  sendMsg nsArray (mkSelector "descriptionWithLocale:indent:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ()), argCULong (fromIntegral level)] >>= retainedObject . castPtr

-- | @- firstObjectCommonWithArray:@
firstObjectCommonWithArray :: (IsNSArray nsArray, IsNSArray otherArray) => nsArray -> otherArray -> IO RawId
firstObjectCommonWithArray nsArray  otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    fmap (RawId . castPtr) $ sendMsg nsArray (mkSelector "firstObjectCommonWithArray:") (retPtr retVoid) [argPtr (castPtr raw_otherArray :: Ptr ())]

-- | @- getObjects:range:@
getObjects_range :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> IO ()
getObjects_range nsArray  objects range =
  sendMsg nsArray (mkSelector "getObjects:range:") retVoid [argPtr (castPtr (unRawId objects) :: Ptr ()), argNSRange range]

-- | @- indexOfObject:@
indexOfObject :: IsNSArray nsArray => nsArray -> RawId -> IO CULong
indexOfObject nsArray  anObject =
  sendMsg nsArray (mkSelector "indexOfObject:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- indexOfObject:inRange:@
indexOfObject_inRange :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> IO CULong
indexOfObject_inRange nsArray  anObject range =
  sendMsg nsArray (mkSelector "indexOfObject:inRange:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ()), argNSRange range]

-- | @- indexOfObjectIdenticalTo:@
indexOfObjectIdenticalTo :: IsNSArray nsArray => nsArray -> RawId -> IO CULong
indexOfObjectIdenticalTo nsArray  anObject =
  sendMsg nsArray (mkSelector "indexOfObjectIdenticalTo:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- indexOfObjectIdenticalTo:inRange:@
indexOfObjectIdenticalTo_inRange :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> IO CULong
indexOfObjectIdenticalTo_inRange nsArray  anObject range =
  sendMsg nsArray (mkSelector "indexOfObjectIdenticalTo:inRange:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ()), argNSRange range]

-- | @- isEqualToArray:@
isEqualToArray :: (IsNSArray nsArray, IsNSArray otherArray) => nsArray -> otherArray -> IO Bool
isEqualToArray nsArray  otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArray (mkSelector "isEqualToArray:") retCULong [argPtr (castPtr raw_otherArray :: Ptr ())]

-- | @- objectEnumerator@
objectEnumerator :: IsNSArray nsArray => nsArray -> IO (Id NSEnumerator)
objectEnumerator nsArray  =
  sendMsg nsArray (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reverseObjectEnumerator@
reverseObjectEnumerator :: IsNSArray nsArray => nsArray -> IO (Id NSEnumerator)
reverseObjectEnumerator nsArray  =
  sendMsg nsArray (mkSelector "reverseObjectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sortedArrayUsingFunction:context:@
sortedArrayUsingFunction_context :: IsNSArray nsArray => nsArray -> Ptr () -> Ptr () -> IO (Id NSArray)
sortedArrayUsingFunction_context nsArray  comparator context =
  sendMsg nsArray (mkSelector "sortedArrayUsingFunction:context:") (retPtr retVoid) [argPtr comparator, argPtr context] >>= retainedObject . castPtr

-- | @- sortedArrayUsingFunction:context:hint:@
sortedArrayUsingFunction_context_hint :: (IsNSArray nsArray, IsNSData hint) => nsArray -> Ptr () -> Ptr () -> hint -> IO (Id NSArray)
sortedArrayUsingFunction_context_hint nsArray  comparator context hint =
withObjCPtr hint $ \raw_hint ->
    sendMsg nsArray (mkSelector "sortedArrayUsingFunction:context:hint:") (retPtr retVoid) [argPtr comparator, argPtr context, argPtr (castPtr raw_hint :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayUsingSelector:@
sortedArrayUsingSelector :: IsNSArray nsArray => nsArray -> Selector -> IO (Id NSArray)
sortedArrayUsingSelector nsArray  comparator =
  sendMsg nsArray (mkSelector "sortedArrayUsingSelector:") (retPtr retVoid) [argPtr (unSelector comparator)] >>= retainedObject . castPtr

-- | @- subarrayWithRange:@
subarrayWithRange :: IsNSArray nsArray => nsArray -> NSRange -> IO (Id NSArray)
subarrayWithRange nsArray  range =
  sendMsg nsArray (mkSelector "subarrayWithRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- writeToURL:error:@
writeToURL_error :: (IsNSArray nsArray, IsNSURL url, IsNSError error_) => nsArray -> url -> error_ -> IO Bool
writeToURL_error nsArray  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsArray (mkSelector "writeToURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- makeObjectsPerformSelector:@
makeObjectsPerformSelector :: IsNSArray nsArray => nsArray -> Selector -> IO ()
makeObjectsPerformSelector nsArray  aSelector =
  sendMsg nsArray (mkSelector "makeObjectsPerformSelector:") retVoid [argPtr (unSelector aSelector)]

-- | @- makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObject :: IsNSArray nsArray => nsArray -> Selector -> RawId -> IO ()
makeObjectsPerformSelector_withObject nsArray  aSelector argument =
  sendMsg nsArray (mkSelector "makeObjectsPerformSelector:withObject:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId argument) :: Ptr ())]

-- | @- objectsAtIndexes:@
objectsAtIndexes :: (IsNSArray nsArray, IsNSIndexSet indexes) => nsArray -> indexes -> IO (Id NSArray)
objectsAtIndexes nsArray  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsArray (mkSelector "objectsAtIndexes:") (retPtr retVoid) [argPtr (castPtr raw_indexes :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsNSArray nsArray => nsArray -> CULong -> IO RawId
objectAtIndexedSubscript nsArray  idx =
  fmap (RawId . castPtr) $ sendMsg nsArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsNSArray nsArray => nsArray -> Ptr () -> IO ()
enumerateObjectsUsingBlock nsArray  block =
  sendMsg nsArray (mkSelector "enumerateObjectsUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsNSArray nsArray => nsArray -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock nsArray  opts block =
  sendMsg nsArray (mkSelector "enumerateObjectsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlock :: (IsNSArray nsArray, IsNSIndexSet s) => nsArray -> s -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsAtIndexes_options_usingBlock nsArray  s opts block =
withObjCPtr s $ \raw_s ->
    sendMsg nsArray (mkSelector "enumerateObjectsAtIndexes:options:usingBlock:") retVoid [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- indexOfObjectPassingTest:@
indexOfObjectPassingTest :: IsNSArray nsArray => nsArray -> Ptr () -> IO CULong
indexOfObjectPassingTest nsArray  predicate =
  sendMsg nsArray (mkSelector "indexOfObjectPassingTest:") retCULong [argPtr (castPtr predicate :: Ptr ())]

-- | @- indexOfObjectWithOptions:passingTest:@
indexOfObjectWithOptions_passingTest :: IsNSArray nsArray => nsArray -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectWithOptions_passingTest nsArray  opts predicate =
  sendMsg nsArray (mkSelector "indexOfObjectWithOptions:passingTest:") retCULong [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())]

-- | @- indexOfObjectAtIndexes:options:passingTest:@
indexOfObjectAtIndexes_options_passingTest :: (IsNSArray nsArray, IsNSIndexSet s) => nsArray -> s -> NSEnumerationOptions -> Ptr () -> IO CULong
indexOfObjectAtIndexes_options_passingTest nsArray  s opts predicate =
withObjCPtr s $ \raw_s ->
    sendMsg nsArray (mkSelector "indexOfObjectAtIndexes:options:passingTest:") retCULong [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())]

-- | @- indexesOfObjectsPassingTest:@
indexesOfObjectsPassingTest :: IsNSArray nsArray => nsArray -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsPassingTest nsArray  predicate =
  sendMsg nsArray (mkSelector "indexesOfObjectsPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexesOfObjectsWithOptions:passingTest:@
indexesOfObjectsWithOptions_passingTest :: IsNSArray nsArray => nsArray -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsWithOptions_passingTest nsArray  opts predicate =
  sendMsg nsArray (mkSelector "indexesOfObjectsWithOptions:passingTest:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexesOfObjectsAtIndexes:options:passingTest:@
indexesOfObjectsAtIndexes_options_passingTest :: (IsNSArray nsArray, IsNSIndexSet s) => nsArray -> s -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesOfObjectsAtIndexes_options_passingTest nsArray  s opts predicate =
withObjCPtr s $ \raw_s ->
    sendMsg nsArray (mkSelector "indexesOfObjectsAtIndexes:options:passingTest:") (retPtr retVoid) [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayUsingComparator:@
sortedArrayUsingComparator :: IsNSArray nsArray => nsArray -> Ptr () -> IO (Id NSArray)
sortedArrayUsingComparator nsArray  cmptr =
  sendMsg nsArray (mkSelector "sortedArrayUsingComparator:") (retPtr retVoid) [argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparator :: IsNSArray nsArray => nsArray -> NSSortOptions -> Ptr () -> IO (Id NSArray)
sortedArrayWithOptions_usingComparator nsArray  opts cmptr =
  sendMsg nsArray (mkSelector "sortedArrayWithOptions:usingComparator:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparator :: IsNSArray nsArray => nsArray -> RawId -> NSRange -> NSBinarySearchingOptions -> Ptr () -> IO CULong
indexOfObject_inSortedRange_options_usingComparator nsArray  obj_ r opts cmp =
  sendMsg nsArray (mkSelector "indexOfObject:inSortedRange:options:usingComparator:") retCULong [argPtr (castPtr (unRawId obj_) :: Ptr ()), argNSRange r, argCULong (coerce opts), argPtr (castPtr cmp :: Ptr ())]

-- | @- count@
count :: IsNSArray nsArray => nsArray -> IO CULong
count nsArray  =
  sendMsg nsArray (mkSelector "count") retCULong []

-- | @- description@
description :: IsNSArray nsArray => nsArray -> IO (Id NSString)
description nsArray  =
  sendMsg nsArray (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- firstObject@
firstObject :: IsNSArray nsArray => nsArray -> IO RawId
firstObject nsArray  =
  fmap (RawId . castPtr) $ sendMsg nsArray (mkSelector "firstObject") (retPtr retVoid) []

-- | @- lastObject@
lastObject :: IsNSArray nsArray => nsArray -> IO RawId
lastObject nsArray  =
  fmap (RawId . castPtr) $ sendMsg nsArray (mkSelector "lastObject") (retPtr retVoid) []

-- | @- sortedArrayHint@
sortedArrayHint :: IsNSArray nsArray => nsArray -> IO (Id NSData)
sortedArrayHint nsArray  =
  sendMsg nsArray (mkSelector "sortedArrayHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:count:@
initWithObjects_countSelector :: Selector
initWithObjects_countSelector = mkSelector "initWithObjects:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filteredArrayUsingPredicate:@
filteredArrayUsingPredicateSelector :: Selector
filteredArrayUsingPredicateSelector = mkSelector "filteredArrayUsingPredicate:"

-- | @Selector@ for @sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptorsSelector :: Selector
sortedArrayUsingDescriptorsSelector = mkSelector "sortedArrayUsingDescriptors:"

-- | @Selector@ for @addObserver:toObjectsAtIndexes:forKeyPath:options:context:@
addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector :: Selector
addObserver_toObjectsAtIndexes_forKeyPath_options_contextSelector = mkSelector "addObserver:toObjectsAtIndexes:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:fromObjectsAtIndexes:forKeyPath:context:@
removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector :: Selector
removeObserver_fromObjectsAtIndexes_forKeyPath_contextSelector = mkSelector "removeObserver:fromObjectsAtIndexes:forKeyPath:context:"

-- | @Selector@ for @removeObserver:fromObjectsAtIndexes:forKeyPath:@
removeObserver_fromObjectsAtIndexes_forKeyPathSelector :: Selector
removeObserver_fromObjectsAtIndexes_forKeyPathSelector = mkSelector "removeObserver:fromObjectsAtIndexes:forKeyPath:"

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

-- | @Selector@ for @pathsMatchingExtensions:@
pathsMatchingExtensionsSelector :: Selector
pathsMatchingExtensionsSelector = mkSelector "pathsMatchingExtensions:"

-- | @Selector@ for @getObjects:@
getObjectsSelector :: Selector
getObjectsSelector = mkSelector "getObjects:"

-- | @Selector@ for @arrayWithContentsOfFile:@
arrayWithContentsOfFileSelector :: Selector
arrayWithContentsOfFileSelector = mkSelector "arrayWithContentsOfFile:"

-- | @Selector@ for @arrayWithContentsOfURL:@
arrayWithContentsOfURLSelector :: Selector
arrayWithContentsOfURLSelector = mkSelector "arrayWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @differenceFromArray:withOptions:usingEquivalenceTest:@
differenceFromArray_withOptions_usingEquivalenceTestSelector :: Selector
differenceFromArray_withOptions_usingEquivalenceTestSelector = mkSelector "differenceFromArray:withOptions:usingEquivalenceTest:"

-- | @Selector@ for @differenceFromArray:withOptions:@
differenceFromArray_withOptionsSelector :: Selector
differenceFromArray_withOptionsSelector = mkSelector "differenceFromArray:withOptions:"

-- | @Selector@ for @differenceFromArray:@
differenceFromArraySelector :: Selector
differenceFromArraySelector = mkSelector "differenceFromArray:"

-- | @Selector@ for @arrayByApplyingDifference:@
arrayByApplyingDifferenceSelector :: Selector
arrayByApplyingDifferenceSelector = mkSelector "arrayByApplyingDifference:"

-- | @Selector@ for @array@
arraySelector :: Selector
arraySelector = mkSelector "array"

-- | @Selector@ for @arrayWithObject:@
arrayWithObjectSelector :: Selector
arrayWithObjectSelector = mkSelector "arrayWithObject:"

-- | @Selector@ for @arrayWithObjects:count:@
arrayWithObjects_countSelector :: Selector
arrayWithObjects_countSelector = mkSelector "arrayWithObjects:count:"

-- | @Selector@ for @arrayWithObjects:@
arrayWithObjectsSelector :: Selector
arrayWithObjectsSelector = mkSelector "arrayWithObjects:"

-- | @Selector@ for @arrayWithArray:@
arrayWithArraySelector :: Selector
arrayWithArraySelector = mkSelector "arrayWithArray:"

-- | @Selector@ for @initWithObjects:@
initWithObjectsSelector :: Selector
initWithObjectsSelector = mkSelector "initWithObjects:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @initWithArray:copyItems:@
initWithArray_copyItemsSelector :: Selector
initWithArray_copyItemsSelector = mkSelector "initWithArray:copyItems:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @arrayWithContentsOfURL:error:@
arrayWithContentsOfURL_errorSelector :: Selector
arrayWithContentsOfURL_errorSelector = mkSelector "arrayWithContentsOfURL:error:"

-- | @Selector@ for @arrayByAddingObject:@
arrayByAddingObjectSelector :: Selector
arrayByAddingObjectSelector = mkSelector "arrayByAddingObject:"

-- | @Selector@ for @arrayByAddingObjectsFromArray:@
arrayByAddingObjectsFromArraySelector :: Selector
arrayByAddingObjectsFromArraySelector = mkSelector "arrayByAddingObjectsFromArray:"

-- | @Selector@ for @componentsJoinedByString:@
componentsJoinedByStringSelector :: Selector
componentsJoinedByStringSelector = mkSelector "componentsJoinedByString:"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @descriptionWithLocale:indent:@
descriptionWithLocale_indentSelector :: Selector
descriptionWithLocale_indentSelector = mkSelector "descriptionWithLocale:indent:"

-- | @Selector@ for @firstObjectCommonWithArray:@
firstObjectCommonWithArraySelector :: Selector
firstObjectCommonWithArraySelector = mkSelector "firstObjectCommonWithArray:"

-- | @Selector@ for @getObjects:range:@
getObjects_rangeSelector :: Selector
getObjects_rangeSelector = mkSelector "getObjects:range:"

-- | @Selector@ for @indexOfObject:@
indexOfObjectSelector :: Selector
indexOfObjectSelector = mkSelector "indexOfObject:"

-- | @Selector@ for @indexOfObject:inRange:@
indexOfObject_inRangeSelector :: Selector
indexOfObject_inRangeSelector = mkSelector "indexOfObject:inRange:"

-- | @Selector@ for @indexOfObjectIdenticalTo:@
indexOfObjectIdenticalToSelector :: Selector
indexOfObjectIdenticalToSelector = mkSelector "indexOfObjectIdenticalTo:"

-- | @Selector@ for @indexOfObjectIdenticalTo:inRange:@
indexOfObjectIdenticalTo_inRangeSelector :: Selector
indexOfObjectIdenticalTo_inRangeSelector = mkSelector "indexOfObjectIdenticalTo:inRange:"

-- | @Selector@ for @isEqualToArray:@
isEqualToArraySelector :: Selector
isEqualToArraySelector = mkSelector "isEqualToArray:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @reverseObjectEnumerator@
reverseObjectEnumeratorSelector :: Selector
reverseObjectEnumeratorSelector = mkSelector "reverseObjectEnumerator"

-- | @Selector@ for @sortedArrayUsingFunction:context:@
sortedArrayUsingFunction_contextSelector :: Selector
sortedArrayUsingFunction_contextSelector = mkSelector "sortedArrayUsingFunction:context:"

-- | @Selector@ for @sortedArrayUsingFunction:context:hint:@
sortedArrayUsingFunction_context_hintSelector :: Selector
sortedArrayUsingFunction_context_hintSelector = mkSelector "sortedArrayUsingFunction:context:hint:"

-- | @Selector@ for @sortedArrayUsingSelector:@
sortedArrayUsingSelectorSelector :: Selector
sortedArrayUsingSelectorSelector = mkSelector "sortedArrayUsingSelector:"

-- | @Selector@ for @subarrayWithRange:@
subarrayWithRangeSelector :: Selector
subarrayWithRangeSelector = mkSelector "subarrayWithRange:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @makeObjectsPerformSelector:@
makeObjectsPerformSelectorSelector :: Selector
makeObjectsPerformSelectorSelector = mkSelector "makeObjectsPerformSelector:"

-- | @Selector@ for @makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObjectSelector :: Selector
makeObjectsPerformSelector_withObjectSelector = mkSelector "makeObjectsPerformSelector:withObject:"

-- | @Selector@ for @objectsAtIndexes:@
objectsAtIndexesSelector :: Selector
objectsAtIndexesSelector = mkSelector "objectsAtIndexes:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

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

-- | @Selector@ for @sortedArrayUsingComparator:@
sortedArrayUsingComparatorSelector :: Selector
sortedArrayUsingComparatorSelector = mkSelector "sortedArrayUsingComparator:"

-- | @Selector@ for @sortedArrayWithOptions:usingComparator:@
sortedArrayWithOptions_usingComparatorSelector :: Selector
sortedArrayWithOptions_usingComparatorSelector = mkSelector "sortedArrayWithOptions:usingComparator:"

-- | @Selector@ for @indexOfObject:inSortedRange:options:usingComparator:@
indexOfObject_inSortedRange_options_usingComparatorSelector :: Selector
indexOfObject_inSortedRange_options_usingComparatorSelector = mkSelector "indexOfObject:inSortedRange:options:usingComparator:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @firstObject@
firstObjectSelector :: Selector
firstObjectSelector = mkSelector "firstObject"

-- | @Selector@ for @lastObject@
lastObjectSelector :: Selector
lastObjectSelector = mkSelector "lastObject"

-- | @Selector@ for @sortedArrayHint@
sortedArrayHintSelector :: Selector
sortedArrayHintSelector = mkSelector "sortedArrayHint"

