{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIndexSet@.
module ObjC.Foundation.NSIndexSet
  ( NSIndexSet
  , IsNSIndexSet(..)
  , indexSet
  , indexSetWithIndex
  , indexSetWithIndexesInRange
  , initWithIndexesInRange
  , initWithIndexSet
  , initWithIndex
  , isEqualToIndexSet
  , indexGreaterThanIndex
  , indexLessThanIndex
  , indexGreaterThanOrEqualToIndex
  , indexLessThanOrEqualToIndex
  , getIndexes_maxCount_inIndexRange
  , countOfIndexesInRange
  , containsIndex
  , containsIndexesInRange
  , containsIndexes
  , intersectsIndexesInRange
  , enumerateIndexesUsingBlock
  , enumerateIndexesWithOptions_usingBlock
  , enumerateIndexesInRange_options_usingBlock
  , indexPassingTest
  , indexWithOptions_passingTest
  , indexInRange_options_passingTest
  , indexesPassingTest
  , indexesWithOptions_passingTest
  , indexesInRange_options_passingTest
  , enumerateRangesUsingBlock
  , enumerateRangesWithOptions_usingBlock
  , enumerateRangesInRange_options_usingBlock
  , count
  , firstIndex
  , lastIndex
  , containsIndexSelector
  , containsIndexesInRangeSelector
  , containsIndexesSelector
  , countOfIndexesInRangeSelector
  , countSelector
  , enumerateIndexesInRange_options_usingBlockSelector
  , enumerateIndexesUsingBlockSelector
  , enumerateIndexesWithOptions_usingBlockSelector
  , enumerateRangesInRange_options_usingBlockSelector
  , enumerateRangesUsingBlockSelector
  , enumerateRangesWithOptions_usingBlockSelector
  , firstIndexSelector
  , getIndexes_maxCount_inIndexRangeSelector
  , indexGreaterThanIndexSelector
  , indexGreaterThanOrEqualToIndexSelector
  , indexInRange_options_passingTestSelector
  , indexLessThanIndexSelector
  , indexLessThanOrEqualToIndexSelector
  , indexPassingTestSelector
  , indexSetSelector
  , indexSetWithIndexSelector
  , indexSetWithIndexesInRangeSelector
  , indexWithOptions_passingTestSelector
  , indexesInRange_options_passingTestSelector
  , indexesPassingTestSelector
  , indexesWithOptions_passingTestSelector
  , initWithIndexSelector
  , initWithIndexSetSelector
  , initWithIndexesInRangeSelector
  , intersectsIndexesInRangeSelector
  , isEqualToIndexSetSelector
  , lastIndexSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse

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

-- | @+ indexSet@
indexSet :: IO (Id NSIndexSet)
indexSet  =
  do
    cls' <- getRequiredClass "NSIndexSet"
    sendClassMessage cls' indexSetSelector

-- | @+ indexSetWithIndex:@
indexSetWithIndex :: CULong -> IO (Id NSIndexSet)
indexSetWithIndex value =
  do
    cls' <- getRequiredClass "NSIndexSet"
    sendClassMessage cls' indexSetWithIndexSelector value

-- | @+ indexSetWithIndexesInRange:@
indexSetWithIndexesInRange :: NSRange -> IO (Id NSIndexSet)
indexSetWithIndexesInRange range =
  do
    cls' <- getRequiredClass "NSIndexSet"
    sendClassMessage cls' indexSetWithIndexesInRangeSelector range

-- | @- initWithIndexesInRange:@
initWithIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO (Id NSIndexSet)
initWithIndexesInRange nsIndexSet range =
  sendOwnedMessage nsIndexSet initWithIndexesInRangeSelector range

-- | @- initWithIndexSet:@
initWithIndexSet :: (IsNSIndexSet nsIndexSet, IsNSIndexSet indexSet) => nsIndexSet -> indexSet -> IO (Id NSIndexSet)
initWithIndexSet nsIndexSet indexSet =
  sendOwnedMessage nsIndexSet initWithIndexSetSelector (toNSIndexSet indexSet)

-- | @- initWithIndex:@
initWithIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO (Id NSIndexSet)
initWithIndex nsIndexSet value =
  sendOwnedMessage nsIndexSet initWithIndexSelector value

-- | @- isEqualToIndexSet:@
isEqualToIndexSet :: (IsNSIndexSet nsIndexSet, IsNSIndexSet indexSet) => nsIndexSet -> indexSet -> IO Bool
isEqualToIndexSet nsIndexSet indexSet =
  sendMessage nsIndexSet isEqualToIndexSetSelector (toNSIndexSet indexSet)

-- | @- indexGreaterThanIndex:@
indexGreaterThanIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexGreaterThanIndex nsIndexSet value =
  sendMessage nsIndexSet indexGreaterThanIndexSelector value

-- | @- indexLessThanIndex:@
indexLessThanIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexLessThanIndex nsIndexSet value =
  sendMessage nsIndexSet indexLessThanIndexSelector value

-- | @- indexGreaterThanOrEqualToIndex:@
indexGreaterThanOrEqualToIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexGreaterThanOrEqualToIndex nsIndexSet value =
  sendMessage nsIndexSet indexGreaterThanOrEqualToIndexSelector value

-- | @- indexLessThanOrEqualToIndex:@
indexLessThanOrEqualToIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexLessThanOrEqualToIndex nsIndexSet value =
  sendMessage nsIndexSet indexLessThanOrEqualToIndexSelector value

-- | @- getIndexes:maxCount:inIndexRange:@
getIndexes_maxCount_inIndexRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr CULong -> CULong -> Ptr NSRange -> IO CULong
getIndexes_maxCount_inIndexRange nsIndexSet indexBuffer bufferSize range =
  sendMessage nsIndexSet getIndexes_maxCount_inIndexRangeSelector indexBuffer bufferSize range

-- | @- countOfIndexesInRange:@
countOfIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO CULong
countOfIndexesInRange nsIndexSet range =
  sendMessage nsIndexSet countOfIndexesInRangeSelector range

-- | @- containsIndex:@
containsIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO Bool
containsIndex nsIndexSet value =
  sendMessage nsIndexSet containsIndexSelector value

-- | @- containsIndexesInRange:@
containsIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO Bool
containsIndexesInRange nsIndexSet range =
  sendMessage nsIndexSet containsIndexesInRangeSelector range

-- | @- containsIndexes:@
containsIndexes :: (IsNSIndexSet nsIndexSet, IsNSIndexSet indexSet) => nsIndexSet -> indexSet -> IO Bool
containsIndexes nsIndexSet indexSet =
  sendMessage nsIndexSet containsIndexesSelector (toNSIndexSet indexSet)

-- | @- intersectsIndexesInRange:@
intersectsIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO Bool
intersectsIndexesInRange nsIndexSet range =
  sendMessage nsIndexSet intersectsIndexesInRangeSelector range

-- | @- enumerateIndexesUsingBlock:@
enumerateIndexesUsingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO ()
enumerateIndexesUsingBlock nsIndexSet block =
  sendMessage nsIndexSet enumerateIndexesUsingBlockSelector block

-- | @- enumerateIndexesWithOptions:usingBlock:@
enumerateIndexesWithOptions_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateIndexesWithOptions_usingBlock nsIndexSet opts block =
  sendMessage nsIndexSet enumerateIndexesWithOptions_usingBlockSelector opts block

-- | @- enumerateIndexesInRange:options:usingBlock:@
enumerateIndexesInRange_options_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateIndexesInRange_options_usingBlock nsIndexSet range opts block =
  sendMessage nsIndexSet enumerateIndexesInRange_options_usingBlockSelector range opts block

-- | @- indexPassingTest:@
indexPassingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO CULong
indexPassingTest nsIndexSet predicate =
  sendMessage nsIndexSet indexPassingTestSelector predicate

-- | @- indexWithOptions:passingTest:@
indexWithOptions_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO CULong
indexWithOptions_passingTest nsIndexSet opts predicate =
  sendMessage nsIndexSet indexWithOptions_passingTestSelector opts predicate

-- | @- indexInRange:options:passingTest:@
indexInRange_options_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO CULong
indexInRange_options_passingTest nsIndexSet range opts predicate =
  sendMessage nsIndexSet indexInRange_options_passingTestSelector range opts predicate

-- | @- indexesPassingTest:@
indexesPassingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO (Id NSIndexSet)
indexesPassingTest nsIndexSet predicate =
  sendMessage nsIndexSet indexesPassingTestSelector predicate

-- | @- indexesWithOptions:passingTest:@
indexesWithOptions_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesWithOptions_passingTest nsIndexSet opts predicate =
  sendMessage nsIndexSet indexesWithOptions_passingTestSelector opts predicate

-- | @- indexesInRange:options:passingTest:@
indexesInRange_options_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesInRange_options_passingTest nsIndexSet range opts predicate =
  sendMessage nsIndexSet indexesInRange_options_passingTestSelector range opts predicate

-- | @- enumerateRangesUsingBlock:@
enumerateRangesUsingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO ()
enumerateRangesUsingBlock nsIndexSet block =
  sendMessage nsIndexSet enumerateRangesUsingBlockSelector block

-- | @- enumerateRangesWithOptions:usingBlock:@
enumerateRangesWithOptions_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateRangesWithOptions_usingBlock nsIndexSet opts block =
  sendMessage nsIndexSet enumerateRangesWithOptions_usingBlockSelector opts block

-- | @- enumerateRangesInRange:options:usingBlock:@
enumerateRangesInRange_options_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateRangesInRange_options_usingBlock nsIndexSet range opts block =
  sendMessage nsIndexSet enumerateRangesInRange_options_usingBlockSelector range opts block

-- | @- count@
count :: IsNSIndexSet nsIndexSet => nsIndexSet -> IO CULong
count nsIndexSet =
  sendMessage nsIndexSet countSelector

-- | @- firstIndex@
firstIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> IO CULong
firstIndex nsIndexSet =
  sendMessage nsIndexSet firstIndexSelector

-- | @- lastIndex@
lastIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> IO CULong
lastIndex nsIndexSet =
  sendMessage nsIndexSet lastIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexSet@
indexSetSelector :: Selector '[] (Id NSIndexSet)
indexSetSelector = mkSelector "indexSet"

-- | @Selector@ for @indexSetWithIndex:@
indexSetWithIndexSelector :: Selector '[CULong] (Id NSIndexSet)
indexSetWithIndexSelector = mkSelector "indexSetWithIndex:"

-- | @Selector@ for @indexSetWithIndexesInRange:@
indexSetWithIndexesInRangeSelector :: Selector '[NSRange] (Id NSIndexSet)
indexSetWithIndexesInRangeSelector = mkSelector "indexSetWithIndexesInRange:"

-- | @Selector@ for @initWithIndexesInRange:@
initWithIndexesInRangeSelector :: Selector '[NSRange] (Id NSIndexSet)
initWithIndexesInRangeSelector = mkSelector "initWithIndexesInRange:"

-- | @Selector@ for @initWithIndexSet:@
initWithIndexSetSelector :: Selector '[Id NSIndexSet] (Id NSIndexSet)
initWithIndexSetSelector = mkSelector "initWithIndexSet:"

-- | @Selector@ for @initWithIndex:@
initWithIndexSelector :: Selector '[CULong] (Id NSIndexSet)
initWithIndexSelector = mkSelector "initWithIndex:"

-- | @Selector@ for @isEqualToIndexSet:@
isEqualToIndexSetSelector :: Selector '[Id NSIndexSet] Bool
isEqualToIndexSetSelector = mkSelector "isEqualToIndexSet:"

-- | @Selector@ for @indexGreaterThanIndex:@
indexGreaterThanIndexSelector :: Selector '[CULong] CULong
indexGreaterThanIndexSelector = mkSelector "indexGreaterThanIndex:"

-- | @Selector@ for @indexLessThanIndex:@
indexLessThanIndexSelector :: Selector '[CULong] CULong
indexLessThanIndexSelector = mkSelector "indexLessThanIndex:"

-- | @Selector@ for @indexGreaterThanOrEqualToIndex:@
indexGreaterThanOrEqualToIndexSelector :: Selector '[CULong] CULong
indexGreaterThanOrEqualToIndexSelector = mkSelector "indexGreaterThanOrEqualToIndex:"

-- | @Selector@ for @indexLessThanOrEqualToIndex:@
indexLessThanOrEqualToIndexSelector :: Selector '[CULong] CULong
indexLessThanOrEqualToIndexSelector = mkSelector "indexLessThanOrEqualToIndex:"

-- | @Selector@ for @getIndexes:maxCount:inIndexRange:@
getIndexes_maxCount_inIndexRangeSelector :: Selector '[Ptr CULong, CULong, Ptr NSRange] CULong
getIndexes_maxCount_inIndexRangeSelector = mkSelector "getIndexes:maxCount:inIndexRange:"

-- | @Selector@ for @countOfIndexesInRange:@
countOfIndexesInRangeSelector :: Selector '[NSRange] CULong
countOfIndexesInRangeSelector = mkSelector "countOfIndexesInRange:"

-- | @Selector@ for @containsIndex:@
containsIndexSelector :: Selector '[CULong] Bool
containsIndexSelector = mkSelector "containsIndex:"

-- | @Selector@ for @containsIndexesInRange:@
containsIndexesInRangeSelector :: Selector '[NSRange] Bool
containsIndexesInRangeSelector = mkSelector "containsIndexesInRange:"

-- | @Selector@ for @containsIndexes:@
containsIndexesSelector :: Selector '[Id NSIndexSet] Bool
containsIndexesSelector = mkSelector "containsIndexes:"

-- | @Selector@ for @intersectsIndexesInRange:@
intersectsIndexesInRangeSelector :: Selector '[NSRange] Bool
intersectsIndexesInRangeSelector = mkSelector "intersectsIndexesInRange:"

-- | @Selector@ for @enumerateIndexesUsingBlock:@
enumerateIndexesUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateIndexesUsingBlockSelector = mkSelector "enumerateIndexesUsingBlock:"

-- | @Selector@ for @enumerateIndexesWithOptions:usingBlock:@
enumerateIndexesWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateIndexesWithOptions_usingBlockSelector = mkSelector "enumerateIndexesWithOptions:usingBlock:"

-- | @Selector@ for @enumerateIndexesInRange:options:usingBlock:@
enumerateIndexesInRange_options_usingBlockSelector :: Selector '[NSRange, NSEnumerationOptions, Ptr ()] ()
enumerateIndexesInRange_options_usingBlockSelector = mkSelector "enumerateIndexesInRange:options:usingBlock:"

-- | @Selector@ for @indexPassingTest:@
indexPassingTestSelector :: Selector '[Ptr ()] CULong
indexPassingTestSelector = mkSelector "indexPassingTest:"

-- | @Selector@ for @indexWithOptions:passingTest:@
indexWithOptions_passingTestSelector :: Selector '[NSEnumerationOptions, Ptr ()] CULong
indexWithOptions_passingTestSelector = mkSelector "indexWithOptions:passingTest:"

-- | @Selector@ for @indexInRange:options:passingTest:@
indexInRange_options_passingTestSelector :: Selector '[NSRange, NSEnumerationOptions, Ptr ()] CULong
indexInRange_options_passingTestSelector = mkSelector "indexInRange:options:passingTest:"

-- | @Selector@ for @indexesPassingTest:@
indexesPassingTestSelector :: Selector '[Ptr ()] (Id NSIndexSet)
indexesPassingTestSelector = mkSelector "indexesPassingTest:"

-- | @Selector@ for @indexesWithOptions:passingTest:@
indexesWithOptions_passingTestSelector :: Selector '[NSEnumerationOptions, Ptr ()] (Id NSIndexSet)
indexesWithOptions_passingTestSelector = mkSelector "indexesWithOptions:passingTest:"

-- | @Selector@ for @indexesInRange:options:passingTest:@
indexesInRange_options_passingTestSelector :: Selector '[NSRange, NSEnumerationOptions, Ptr ()] (Id NSIndexSet)
indexesInRange_options_passingTestSelector = mkSelector "indexesInRange:options:passingTest:"

-- | @Selector@ for @enumerateRangesUsingBlock:@
enumerateRangesUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateRangesUsingBlockSelector = mkSelector "enumerateRangesUsingBlock:"

-- | @Selector@ for @enumerateRangesWithOptions:usingBlock:@
enumerateRangesWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateRangesWithOptions_usingBlockSelector = mkSelector "enumerateRangesWithOptions:usingBlock:"

-- | @Selector@ for @enumerateRangesInRange:options:usingBlock:@
enumerateRangesInRange_options_usingBlockSelector :: Selector '[NSRange, NSEnumerationOptions, Ptr ()] ()
enumerateRangesInRange_options_usingBlockSelector = mkSelector "enumerateRangesInRange:options:usingBlock:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @firstIndex@
firstIndexSelector :: Selector '[] CULong
firstIndexSelector = mkSelector "firstIndex"

-- | @Selector@ for @lastIndex@
lastIndexSelector :: Selector '[] CULong
lastIndexSelector = mkSelector "lastIndex"

