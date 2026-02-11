{-# LANGUAGE PatternSynonyms #-}
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
  , indexSetSelector
  , indexSetWithIndexSelector
  , indexSetWithIndexesInRangeSelector
  , initWithIndexesInRangeSelector
  , initWithIndexSetSelector
  , initWithIndexSelector
  , isEqualToIndexSetSelector
  , indexGreaterThanIndexSelector
  , indexLessThanIndexSelector
  , indexGreaterThanOrEqualToIndexSelector
  , indexLessThanOrEqualToIndexSelector
  , getIndexes_maxCount_inIndexRangeSelector
  , countOfIndexesInRangeSelector
  , containsIndexSelector
  , containsIndexesInRangeSelector
  , containsIndexesSelector
  , intersectsIndexesInRangeSelector
  , enumerateIndexesUsingBlockSelector
  , enumerateIndexesWithOptions_usingBlockSelector
  , enumerateIndexesInRange_options_usingBlockSelector
  , indexPassingTestSelector
  , indexWithOptions_passingTestSelector
  , indexInRange_options_passingTestSelector
  , indexesPassingTestSelector
  , indexesWithOptions_passingTestSelector
  , indexesInRange_options_passingTestSelector
  , enumerateRangesUsingBlockSelector
  , enumerateRangesWithOptions_usingBlockSelector
  , enumerateRangesInRange_options_usingBlockSelector
  , countSelector
  , firstIndexSelector
  , lastIndexSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse

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

-- | @+ indexSet@
indexSet :: IO (Id NSIndexSet)
indexSet  =
  do
    cls' <- getRequiredClass "NSIndexSet"
    sendClassMsg cls' (mkSelector "indexSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ indexSetWithIndex:@
indexSetWithIndex :: CULong -> IO (Id NSIndexSet)
indexSetWithIndex value =
  do
    cls' <- getRequiredClass "NSIndexSet"
    sendClassMsg cls' (mkSelector "indexSetWithIndex:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ indexSetWithIndexesInRange:@
indexSetWithIndexesInRange :: NSRange -> IO (Id NSIndexSet)
indexSetWithIndexesInRange range =
  do
    cls' <- getRequiredClass "NSIndexSet"
    sendClassMsg cls' (mkSelector "indexSetWithIndexesInRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- initWithIndexesInRange:@
initWithIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO (Id NSIndexSet)
initWithIndexesInRange nsIndexSet  range =
  sendMsg nsIndexSet (mkSelector "initWithIndexesInRange:") (retPtr retVoid) [argNSRange range] >>= ownedObject . castPtr

-- | @- initWithIndexSet:@
initWithIndexSet :: (IsNSIndexSet nsIndexSet, IsNSIndexSet indexSet) => nsIndexSet -> indexSet -> IO (Id NSIndexSet)
initWithIndexSet nsIndexSet  indexSet =
withObjCPtr indexSet $ \raw_indexSet ->
    sendMsg nsIndexSet (mkSelector "initWithIndexSet:") (retPtr retVoid) [argPtr (castPtr raw_indexSet :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIndex:@
initWithIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO (Id NSIndexSet)
initWithIndex nsIndexSet  value =
  sendMsg nsIndexSet (mkSelector "initWithIndex:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- isEqualToIndexSet:@
isEqualToIndexSet :: (IsNSIndexSet nsIndexSet, IsNSIndexSet indexSet) => nsIndexSet -> indexSet -> IO Bool
isEqualToIndexSet nsIndexSet  indexSet =
withObjCPtr indexSet $ \raw_indexSet ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsIndexSet (mkSelector "isEqualToIndexSet:") retCULong [argPtr (castPtr raw_indexSet :: Ptr ())]

-- | @- indexGreaterThanIndex:@
indexGreaterThanIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexGreaterThanIndex nsIndexSet  value =
  sendMsg nsIndexSet (mkSelector "indexGreaterThanIndex:") retCULong [argCULong (fromIntegral value)]

-- | @- indexLessThanIndex:@
indexLessThanIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexLessThanIndex nsIndexSet  value =
  sendMsg nsIndexSet (mkSelector "indexLessThanIndex:") retCULong [argCULong (fromIntegral value)]

-- | @- indexGreaterThanOrEqualToIndex:@
indexGreaterThanOrEqualToIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexGreaterThanOrEqualToIndex nsIndexSet  value =
  sendMsg nsIndexSet (mkSelector "indexGreaterThanOrEqualToIndex:") retCULong [argCULong (fromIntegral value)]

-- | @- indexLessThanOrEqualToIndex:@
indexLessThanOrEqualToIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO CULong
indexLessThanOrEqualToIndex nsIndexSet  value =
  sendMsg nsIndexSet (mkSelector "indexLessThanOrEqualToIndex:") retCULong [argCULong (fromIntegral value)]

-- | @- getIndexes:maxCount:inIndexRange:@
getIndexes_maxCount_inIndexRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr CULong -> CULong -> Ptr NSRange -> IO CULong
getIndexes_maxCount_inIndexRange nsIndexSet  indexBuffer bufferSize range =
  sendMsg nsIndexSet (mkSelector "getIndexes:maxCount:inIndexRange:") retCULong [argPtr indexBuffer, argCULong (fromIntegral bufferSize), argPtr range]

-- | @- countOfIndexesInRange:@
countOfIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO CULong
countOfIndexesInRange nsIndexSet  range =
  sendMsg nsIndexSet (mkSelector "countOfIndexesInRange:") retCULong [argNSRange range]

-- | @- containsIndex:@
containsIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> CULong -> IO Bool
containsIndex nsIndexSet  value =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsIndexSet (mkSelector "containsIndex:") retCULong [argCULong (fromIntegral value)]

-- | @- containsIndexesInRange:@
containsIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO Bool
containsIndexesInRange nsIndexSet  range =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsIndexSet (mkSelector "containsIndexesInRange:") retCULong [argNSRange range]

-- | @- containsIndexes:@
containsIndexes :: (IsNSIndexSet nsIndexSet, IsNSIndexSet indexSet) => nsIndexSet -> indexSet -> IO Bool
containsIndexes nsIndexSet  indexSet =
withObjCPtr indexSet $ \raw_indexSet ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsIndexSet (mkSelector "containsIndexes:") retCULong [argPtr (castPtr raw_indexSet :: Ptr ())]

-- | @- intersectsIndexesInRange:@
intersectsIndexesInRange :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> IO Bool
intersectsIndexesInRange nsIndexSet  range =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsIndexSet (mkSelector "intersectsIndexesInRange:") retCULong [argNSRange range]

-- | @- enumerateIndexesUsingBlock:@
enumerateIndexesUsingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO ()
enumerateIndexesUsingBlock nsIndexSet  block =
  sendMsg nsIndexSet (mkSelector "enumerateIndexesUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateIndexesWithOptions:usingBlock:@
enumerateIndexesWithOptions_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateIndexesWithOptions_usingBlock nsIndexSet  opts block =
  sendMsg nsIndexSet (mkSelector "enumerateIndexesWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateIndexesInRange:options:usingBlock:@
enumerateIndexesInRange_options_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateIndexesInRange_options_usingBlock nsIndexSet  range opts block =
  sendMsg nsIndexSet (mkSelector "enumerateIndexesInRange:options:usingBlock:") retVoid [argNSRange range, argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- indexPassingTest:@
indexPassingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO CULong
indexPassingTest nsIndexSet  predicate =
  sendMsg nsIndexSet (mkSelector "indexPassingTest:") retCULong [argPtr (castPtr predicate :: Ptr ())]

-- | @- indexWithOptions:passingTest:@
indexWithOptions_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO CULong
indexWithOptions_passingTest nsIndexSet  opts predicate =
  sendMsg nsIndexSet (mkSelector "indexWithOptions:passingTest:") retCULong [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())]

-- | @- indexInRange:options:passingTest:@
indexInRange_options_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO CULong
indexInRange_options_passingTest nsIndexSet  range opts predicate =
  sendMsg nsIndexSet (mkSelector "indexInRange:options:passingTest:") retCULong [argNSRange range, argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())]

-- | @- indexesPassingTest:@
indexesPassingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO (Id NSIndexSet)
indexesPassingTest nsIndexSet  predicate =
  sendMsg nsIndexSet (mkSelector "indexesPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexesWithOptions:passingTest:@
indexesWithOptions_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesWithOptions_passingTest nsIndexSet  opts predicate =
  sendMsg nsIndexSet (mkSelector "indexesWithOptions:passingTest:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexesInRange:options:passingTest:@
indexesInRange_options_passingTest :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO (Id NSIndexSet)
indexesInRange_options_passingTest nsIndexSet  range opts predicate =
  sendMsg nsIndexSet (mkSelector "indexesInRange:options:passingTest:") (retPtr retVoid) [argNSRange range, argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateRangesUsingBlock:@
enumerateRangesUsingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> Ptr () -> IO ()
enumerateRangesUsingBlock nsIndexSet  block =
  sendMsg nsIndexSet (mkSelector "enumerateRangesUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateRangesWithOptions:usingBlock:@
enumerateRangesWithOptions_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateRangesWithOptions_usingBlock nsIndexSet  opts block =
  sendMsg nsIndexSet (mkSelector "enumerateRangesWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateRangesInRange:options:usingBlock:@
enumerateRangesInRange_options_usingBlock :: IsNSIndexSet nsIndexSet => nsIndexSet -> NSRange -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateRangesInRange_options_usingBlock nsIndexSet  range opts block =
  sendMsg nsIndexSet (mkSelector "enumerateRangesInRange:options:usingBlock:") retVoid [argNSRange range, argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- count@
count :: IsNSIndexSet nsIndexSet => nsIndexSet -> IO CULong
count nsIndexSet  =
  sendMsg nsIndexSet (mkSelector "count") retCULong []

-- | @- firstIndex@
firstIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> IO CULong
firstIndex nsIndexSet  =
  sendMsg nsIndexSet (mkSelector "firstIndex") retCULong []

-- | @- lastIndex@
lastIndex :: IsNSIndexSet nsIndexSet => nsIndexSet -> IO CULong
lastIndex nsIndexSet  =
  sendMsg nsIndexSet (mkSelector "lastIndex") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexSet@
indexSetSelector :: Selector
indexSetSelector = mkSelector "indexSet"

-- | @Selector@ for @indexSetWithIndex:@
indexSetWithIndexSelector :: Selector
indexSetWithIndexSelector = mkSelector "indexSetWithIndex:"

-- | @Selector@ for @indexSetWithIndexesInRange:@
indexSetWithIndexesInRangeSelector :: Selector
indexSetWithIndexesInRangeSelector = mkSelector "indexSetWithIndexesInRange:"

-- | @Selector@ for @initWithIndexesInRange:@
initWithIndexesInRangeSelector :: Selector
initWithIndexesInRangeSelector = mkSelector "initWithIndexesInRange:"

-- | @Selector@ for @initWithIndexSet:@
initWithIndexSetSelector :: Selector
initWithIndexSetSelector = mkSelector "initWithIndexSet:"

-- | @Selector@ for @initWithIndex:@
initWithIndexSelector :: Selector
initWithIndexSelector = mkSelector "initWithIndex:"

-- | @Selector@ for @isEqualToIndexSet:@
isEqualToIndexSetSelector :: Selector
isEqualToIndexSetSelector = mkSelector "isEqualToIndexSet:"

-- | @Selector@ for @indexGreaterThanIndex:@
indexGreaterThanIndexSelector :: Selector
indexGreaterThanIndexSelector = mkSelector "indexGreaterThanIndex:"

-- | @Selector@ for @indexLessThanIndex:@
indexLessThanIndexSelector :: Selector
indexLessThanIndexSelector = mkSelector "indexLessThanIndex:"

-- | @Selector@ for @indexGreaterThanOrEqualToIndex:@
indexGreaterThanOrEqualToIndexSelector :: Selector
indexGreaterThanOrEqualToIndexSelector = mkSelector "indexGreaterThanOrEqualToIndex:"

-- | @Selector@ for @indexLessThanOrEqualToIndex:@
indexLessThanOrEqualToIndexSelector :: Selector
indexLessThanOrEqualToIndexSelector = mkSelector "indexLessThanOrEqualToIndex:"

-- | @Selector@ for @getIndexes:maxCount:inIndexRange:@
getIndexes_maxCount_inIndexRangeSelector :: Selector
getIndexes_maxCount_inIndexRangeSelector = mkSelector "getIndexes:maxCount:inIndexRange:"

-- | @Selector@ for @countOfIndexesInRange:@
countOfIndexesInRangeSelector :: Selector
countOfIndexesInRangeSelector = mkSelector "countOfIndexesInRange:"

-- | @Selector@ for @containsIndex:@
containsIndexSelector :: Selector
containsIndexSelector = mkSelector "containsIndex:"

-- | @Selector@ for @containsIndexesInRange:@
containsIndexesInRangeSelector :: Selector
containsIndexesInRangeSelector = mkSelector "containsIndexesInRange:"

-- | @Selector@ for @containsIndexes:@
containsIndexesSelector :: Selector
containsIndexesSelector = mkSelector "containsIndexes:"

-- | @Selector@ for @intersectsIndexesInRange:@
intersectsIndexesInRangeSelector :: Selector
intersectsIndexesInRangeSelector = mkSelector "intersectsIndexesInRange:"

-- | @Selector@ for @enumerateIndexesUsingBlock:@
enumerateIndexesUsingBlockSelector :: Selector
enumerateIndexesUsingBlockSelector = mkSelector "enumerateIndexesUsingBlock:"

-- | @Selector@ for @enumerateIndexesWithOptions:usingBlock:@
enumerateIndexesWithOptions_usingBlockSelector :: Selector
enumerateIndexesWithOptions_usingBlockSelector = mkSelector "enumerateIndexesWithOptions:usingBlock:"

-- | @Selector@ for @enumerateIndexesInRange:options:usingBlock:@
enumerateIndexesInRange_options_usingBlockSelector :: Selector
enumerateIndexesInRange_options_usingBlockSelector = mkSelector "enumerateIndexesInRange:options:usingBlock:"

-- | @Selector@ for @indexPassingTest:@
indexPassingTestSelector :: Selector
indexPassingTestSelector = mkSelector "indexPassingTest:"

-- | @Selector@ for @indexWithOptions:passingTest:@
indexWithOptions_passingTestSelector :: Selector
indexWithOptions_passingTestSelector = mkSelector "indexWithOptions:passingTest:"

-- | @Selector@ for @indexInRange:options:passingTest:@
indexInRange_options_passingTestSelector :: Selector
indexInRange_options_passingTestSelector = mkSelector "indexInRange:options:passingTest:"

-- | @Selector@ for @indexesPassingTest:@
indexesPassingTestSelector :: Selector
indexesPassingTestSelector = mkSelector "indexesPassingTest:"

-- | @Selector@ for @indexesWithOptions:passingTest:@
indexesWithOptions_passingTestSelector :: Selector
indexesWithOptions_passingTestSelector = mkSelector "indexesWithOptions:passingTest:"

-- | @Selector@ for @indexesInRange:options:passingTest:@
indexesInRange_options_passingTestSelector :: Selector
indexesInRange_options_passingTestSelector = mkSelector "indexesInRange:options:passingTest:"

-- | @Selector@ for @enumerateRangesUsingBlock:@
enumerateRangesUsingBlockSelector :: Selector
enumerateRangesUsingBlockSelector = mkSelector "enumerateRangesUsingBlock:"

-- | @Selector@ for @enumerateRangesWithOptions:usingBlock:@
enumerateRangesWithOptions_usingBlockSelector :: Selector
enumerateRangesWithOptions_usingBlockSelector = mkSelector "enumerateRangesWithOptions:usingBlock:"

-- | @Selector@ for @enumerateRangesInRange:options:usingBlock:@
enumerateRangesInRange_options_usingBlockSelector :: Selector
enumerateRangesInRange_options_usingBlockSelector = mkSelector "enumerateRangesInRange:options:usingBlock:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @firstIndex@
firstIndexSelector :: Selector
firstIndexSelector = mkSelector "firstIndex"

-- | @Selector@ for @lastIndex@
lastIndexSelector :: Selector
lastIndexSelector = mkSelector "lastIndex"

