{-# LANGUAGE PatternSynonyms #-}
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
  , insertObject_atIndexSelector
  , removeLastObjectSelector
  , removeObjectAtIndexSelector
  , replaceObjectAtIndex_withObjectSelector
  , initSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , filterUsingPredicateSelector
  , sortUsingDescriptorsSelector
  , applyDifferenceSelector
  , arrayWithCapacitySelector
  , arrayWithContentsOfFileSelector
  , arrayWithContentsOfURLSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , addObjectsFromArraySelector
  , exchangeObjectAtIndex_withObjectAtIndexSelector
  , removeAllObjectsSelector
  , removeObject_inRangeSelector
  , removeObjectSelector
  , removeObjectIdenticalTo_inRangeSelector
  , removeObjectIdenticalToSelector
  , removeObjectsFromIndices_numIndicesSelector
  , removeObjectsInArraySelector
  , removeObjectsInRangeSelector
  , replaceObjectsInRange_withObjectsFromArray_rangeSelector
  , replaceObjectsInRange_withObjectsFromArraySelector
  , setArraySelector
  , sortUsingFunction_contextSelector
  , sortUsingSelectorSelector
  , insertObjects_atIndexesSelector
  , removeObjectsAtIndexesSelector
  , replaceObjectsAtIndexes_withObjectsSelector
  , setObject_atIndexedSubscriptSelector
  , sortUsingComparatorSelector
  , sortWithOptions_usingComparatorSelector

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

-- | @- addObject:@
addObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> IO ()
addObject nsMutableArray  anObject =
  sendMsg nsMutableArray (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- insertObject:atIndex:@
insertObject_atIndex :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> CULong -> IO ()
insertObject_atIndex nsMutableArray  anObject index =
  sendMsg nsMutableArray (mkSelector "insertObject:atIndex:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argCULong (fromIntegral index)]

-- | @- removeLastObject@
removeLastObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> IO ()
removeLastObject nsMutableArray  =
  sendMsg nsMutableArray (mkSelector "removeLastObject") retVoid []

-- | @- removeObjectAtIndex:@
removeObjectAtIndex :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> IO ()
removeObjectAtIndex nsMutableArray  index =
  sendMsg nsMutableArray (mkSelector "removeObjectAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | @- replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> RawId -> IO ()
replaceObjectAtIndex_withObject nsMutableArray  index anObject =
  sendMsg nsMutableArray (mkSelector "replaceObjectAtIndex:withObject:") retVoid [argCULong (fromIntegral index), argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- init@
init_ :: IsNSMutableArray nsMutableArray => nsMutableArray -> IO (Id NSMutableArray)
init_ nsMutableArray  =
  sendMsg nsMutableArray (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> IO (Id NSMutableArray)
initWithCapacity nsMutableArray  numItems =
  sendMsg nsMutableArray (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableArray nsMutableArray, IsNSCoder coder) => nsMutableArray -> coder -> IO (Id NSMutableArray)
initWithCoder nsMutableArray  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsMutableArray (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- filterUsingPredicate:@
filterUsingPredicate :: (IsNSMutableArray nsMutableArray, IsNSPredicate predicate) => nsMutableArray -> predicate -> IO ()
filterUsingPredicate nsMutableArray  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsMutableArray (mkSelector "filterUsingPredicate:") retVoid [argPtr (castPtr raw_predicate :: Ptr ())]

-- | @- sortUsingDescriptors:@
sortUsingDescriptors :: (IsNSMutableArray nsMutableArray, IsNSArray sortDescriptors) => nsMutableArray -> sortDescriptors -> IO ()
sortUsingDescriptors nsMutableArray  sortDescriptors =
withObjCPtr sortDescriptors $ \raw_sortDescriptors ->
    sendMsg nsMutableArray (mkSelector "sortUsingDescriptors:") retVoid [argPtr (castPtr raw_sortDescriptors :: Ptr ())]

-- | @- applyDifference:@
applyDifference :: (IsNSMutableArray nsMutableArray, IsNSOrderedCollectionDifference difference) => nsMutableArray -> difference -> IO ()
applyDifference nsMutableArray  difference =
withObjCPtr difference $ \raw_difference ->
    sendMsg nsMutableArray (mkSelector "applyDifference:") retVoid [argPtr (castPtr raw_difference :: Ptr ())]

-- | @+ arrayWithCapacity:@
arrayWithCapacity :: CULong -> IO (Id NSMutableArray)
arrayWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableArray"
    sendClassMsg cls' (mkSelector "arrayWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= retainedObject . castPtr

-- | @+ arrayWithContentsOfFile:@
arrayWithContentsOfFile :: IsNSString path => path -> IO (Id NSMutableArray)
arrayWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSMutableArray"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "arrayWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ arrayWithContentsOfURL:@
arrayWithContentsOfURL :: IsNSURL url => url -> IO (Id NSMutableArray)
arrayWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSMutableArray"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "arrayWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSMutableArray nsMutableArray, IsNSString path) => nsMutableArray -> path -> IO (Id NSMutableArray)
initWithContentsOfFile nsMutableArray  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsMutableArray (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSMutableArray nsMutableArray, IsNSURL url) => nsMutableArray -> url -> IO (Id NSMutableArray)
initWithContentsOfURL nsMutableArray  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsMutableArray (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- addObjectsFromArray:@
addObjectsFromArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> otherArray -> IO ()
addObjectsFromArray nsMutableArray  otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    sendMsg nsMutableArray (mkSelector "addObjectsFromArray:") retVoid [argPtr (castPtr raw_otherArray :: Ptr ())]

-- | @- exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndex :: IsNSMutableArray nsMutableArray => nsMutableArray -> CULong -> CULong -> IO ()
exchangeObjectAtIndex_withObjectAtIndex nsMutableArray  idx1 idx2 =
  sendMsg nsMutableArray (mkSelector "exchangeObjectAtIndex:withObjectAtIndex:") retVoid [argCULong (fromIntegral idx1), argCULong (fromIntegral idx2)]

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableArray nsMutableArray => nsMutableArray -> IO ()
removeAllObjects nsMutableArray  =
  sendMsg nsMutableArray (mkSelector "removeAllObjects") retVoid []

-- | @- removeObject:inRange:@
removeObject_inRange :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> NSRange -> IO ()
removeObject_inRange nsMutableArray  anObject range =
  sendMsg nsMutableArray (mkSelector "removeObject:inRange:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argNSRange range]

-- | @- removeObject:@
removeObject :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> IO ()
removeObject nsMutableArray  anObject =
  sendMsg nsMutableArray (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- removeObjectIdenticalTo:inRange:@
removeObjectIdenticalTo_inRange :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> NSRange -> IO ()
removeObjectIdenticalTo_inRange nsMutableArray  anObject range =
  sendMsg nsMutableArray (mkSelector "removeObjectIdenticalTo:inRange:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argNSRange range]

-- | @- removeObjectIdenticalTo:@
removeObjectIdenticalTo :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> IO ()
removeObjectIdenticalTo nsMutableArray  anObject =
  sendMsg nsMutableArray (mkSelector "removeObjectIdenticalTo:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- removeObjectsFromIndices:numIndices:@
removeObjectsFromIndices_numIndices :: IsNSMutableArray nsMutableArray => nsMutableArray -> Ptr CULong -> CULong -> IO ()
removeObjectsFromIndices_numIndices nsMutableArray  indices cnt =
  sendMsg nsMutableArray (mkSelector "removeObjectsFromIndices:numIndices:") retVoid [argPtr indices, argCULong (fromIntegral cnt)]

-- | @- removeObjectsInArray:@
removeObjectsInArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> otherArray -> IO ()
removeObjectsInArray nsMutableArray  otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    sendMsg nsMutableArray (mkSelector "removeObjectsInArray:") retVoid [argPtr (castPtr raw_otherArray :: Ptr ())]

-- | @- removeObjectsInRange:@
removeObjectsInRange :: IsNSMutableArray nsMutableArray => nsMutableArray -> NSRange -> IO ()
removeObjectsInRange nsMutableArray  range =
  sendMsg nsMutableArray (mkSelector "removeObjectsInRange:") retVoid [argNSRange range]

-- | @- replaceObjectsInRange:withObjectsFromArray:range:@
replaceObjectsInRange_withObjectsFromArray_range :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> NSRange -> otherArray -> NSRange -> IO ()
replaceObjectsInRange_withObjectsFromArray_range nsMutableArray  range otherArray otherRange =
withObjCPtr otherArray $ \raw_otherArray ->
    sendMsg nsMutableArray (mkSelector "replaceObjectsInRange:withObjectsFromArray:range:") retVoid [argNSRange range, argPtr (castPtr raw_otherArray :: Ptr ()), argNSRange otherRange]

-- | @- replaceObjectsInRange:withObjectsFromArray:@
replaceObjectsInRange_withObjectsFromArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> NSRange -> otherArray -> IO ()
replaceObjectsInRange_withObjectsFromArray nsMutableArray  range otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    sendMsg nsMutableArray (mkSelector "replaceObjectsInRange:withObjectsFromArray:") retVoid [argNSRange range, argPtr (castPtr raw_otherArray :: Ptr ())]

-- | @- setArray:@
setArray :: (IsNSMutableArray nsMutableArray, IsNSArray otherArray) => nsMutableArray -> otherArray -> IO ()
setArray nsMutableArray  otherArray =
withObjCPtr otherArray $ \raw_otherArray ->
    sendMsg nsMutableArray (mkSelector "setArray:") retVoid [argPtr (castPtr raw_otherArray :: Ptr ())]

-- | @- sortUsingFunction:context:@
sortUsingFunction_context :: IsNSMutableArray nsMutableArray => nsMutableArray -> Ptr () -> Ptr () -> IO ()
sortUsingFunction_context nsMutableArray  compare_ context =
  sendMsg nsMutableArray (mkSelector "sortUsingFunction:context:") retVoid [argPtr compare_, argPtr context]

-- | @- sortUsingSelector:@
sortUsingSelector :: IsNSMutableArray nsMutableArray => nsMutableArray -> Selector -> IO ()
sortUsingSelector nsMutableArray  comparator =
  sendMsg nsMutableArray (mkSelector "sortUsingSelector:") retVoid [argPtr (unSelector comparator)]

-- | @- insertObjects:atIndexes:@
insertObjects_atIndexes :: (IsNSMutableArray nsMutableArray, IsNSArray objects, IsNSIndexSet indexes) => nsMutableArray -> objects -> indexes -> IO ()
insertObjects_atIndexes nsMutableArray  objects indexes =
withObjCPtr objects $ \raw_objects ->
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsMutableArray (mkSelector "insertObjects:atIndexes:") retVoid [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- removeObjectsAtIndexes:@
removeObjectsAtIndexes :: (IsNSMutableArray nsMutableArray, IsNSIndexSet indexes) => nsMutableArray -> indexes -> IO ()
removeObjectsAtIndexes nsMutableArray  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsMutableArray (mkSelector "removeObjectsAtIndexes:") retVoid [argPtr (castPtr raw_indexes :: Ptr ())]

-- | @- replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjects :: (IsNSMutableArray nsMutableArray, IsNSIndexSet indexes, IsNSArray objects) => nsMutableArray -> indexes -> objects -> IO ()
replaceObjectsAtIndexes_withObjects nsMutableArray  indexes objects =
withObjCPtr indexes $ \raw_indexes ->
  withObjCPtr objects $ \raw_objects ->
      sendMsg nsMutableArray (mkSelector "replaceObjectsAtIndexes:withObjects:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_objects :: Ptr ())]

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: IsNSMutableArray nsMutableArray => nsMutableArray -> RawId -> CULong -> IO ()
setObject_atIndexedSubscript nsMutableArray  obj_ idx =
  sendMsg nsMutableArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argCULong (fromIntegral idx)]

-- | @- sortUsingComparator:@
sortUsingComparator :: IsNSMutableArray nsMutableArray => nsMutableArray -> Ptr () -> IO ()
sortUsingComparator nsMutableArray  cmptr =
  sendMsg nsMutableArray (mkSelector "sortUsingComparator:") retVoid [argPtr (castPtr cmptr :: Ptr ())]

-- | @- sortWithOptions:usingComparator:@
sortWithOptions_usingComparator :: IsNSMutableArray nsMutableArray => nsMutableArray -> NSSortOptions -> Ptr () -> IO ()
sortWithOptions_usingComparator nsMutableArray  opts cmptr =
  sendMsg nsMutableArray (mkSelector "sortWithOptions:usingComparator:") retVoid [argCULong (coerce opts), argPtr (castPtr cmptr :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @insertObject:atIndex:@
insertObject_atIndexSelector :: Selector
insertObject_atIndexSelector = mkSelector "insertObject:atIndex:"

-- | @Selector@ for @removeLastObject@
removeLastObjectSelector :: Selector
removeLastObjectSelector = mkSelector "removeLastObject"

-- | @Selector@ for @removeObjectAtIndex:@
removeObjectAtIndexSelector :: Selector
removeObjectAtIndexSelector = mkSelector "removeObjectAtIndex:"

-- | @Selector@ for @replaceObjectAtIndex:withObject:@
replaceObjectAtIndex_withObjectSelector :: Selector
replaceObjectAtIndex_withObjectSelector = mkSelector "replaceObjectAtIndex:withObject:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filterUsingPredicate:@
filterUsingPredicateSelector :: Selector
filterUsingPredicateSelector = mkSelector "filterUsingPredicate:"

-- | @Selector@ for @sortUsingDescriptors:@
sortUsingDescriptorsSelector :: Selector
sortUsingDescriptorsSelector = mkSelector "sortUsingDescriptors:"

-- | @Selector@ for @applyDifference:@
applyDifferenceSelector :: Selector
applyDifferenceSelector = mkSelector "applyDifference:"

-- | @Selector@ for @arrayWithCapacity:@
arrayWithCapacitySelector :: Selector
arrayWithCapacitySelector = mkSelector "arrayWithCapacity:"

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

-- | @Selector@ for @addObjectsFromArray:@
addObjectsFromArraySelector :: Selector
addObjectsFromArraySelector = mkSelector "addObjectsFromArray:"

-- | @Selector@ for @exchangeObjectAtIndex:withObjectAtIndex:@
exchangeObjectAtIndex_withObjectAtIndexSelector :: Selector
exchangeObjectAtIndex_withObjectAtIndexSelector = mkSelector "exchangeObjectAtIndex:withObjectAtIndex:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @removeObject:inRange:@
removeObject_inRangeSelector :: Selector
removeObject_inRangeSelector = mkSelector "removeObject:inRange:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeObjectIdenticalTo:inRange:@
removeObjectIdenticalTo_inRangeSelector :: Selector
removeObjectIdenticalTo_inRangeSelector = mkSelector "removeObjectIdenticalTo:inRange:"

-- | @Selector@ for @removeObjectIdenticalTo:@
removeObjectIdenticalToSelector :: Selector
removeObjectIdenticalToSelector = mkSelector "removeObjectIdenticalTo:"

-- | @Selector@ for @removeObjectsFromIndices:numIndices:@
removeObjectsFromIndices_numIndicesSelector :: Selector
removeObjectsFromIndices_numIndicesSelector = mkSelector "removeObjectsFromIndices:numIndices:"

-- | @Selector@ for @removeObjectsInArray:@
removeObjectsInArraySelector :: Selector
removeObjectsInArraySelector = mkSelector "removeObjectsInArray:"

-- | @Selector@ for @removeObjectsInRange:@
removeObjectsInRangeSelector :: Selector
removeObjectsInRangeSelector = mkSelector "removeObjectsInRange:"

-- | @Selector@ for @replaceObjectsInRange:withObjectsFromArray:range:@
replaceObjectsInRange_withObjectsFromArray_rangeSelector :: Selector
replaceObjectsInRange_withObjectsFromArray_rangeSelector = mkSelector "replaceObjectsInRange:withObjectsFromArray:range:"

-- | @Selector@ for @replaceObjectsInRange:withObjectsFromArray:@
replaceObjectsInRange_withObjectsFromArraySelector :: Selector
replaceObjectsInRange_withObjectsFromArraySelector = mkSelector "replaceObjectsInRange:withObjectsFromArray:"

-- | @Selector@ for @setArray:@
setArraySelector :: Selector
setArraySelector = mkSelector "setArray:"

-- | @Selector@ for @sortUsingFunction:context:@
sortUsingFunction_contextSelector :: Selector
sortUsingFunction_contextSelector = mkSelector "sortUsingFunction:context:"

-- | @Selector@ for @sortUsingSelector:@
sortUsingSelectorSelector :: Selector
sortUsingSelectorSelector = mkSelector "sortUsingSelector:"

-- | @Selector@ for @insertObjects:atIndexes:@
insertObjects_atIndexesSelector :: Selector
insertObjects_atIndexesSelector = mkSelector "insertObjects:atIndexes:"

-- | @Selector@ for @removeObjectsAtIndexes:@
removeObjectsAtIndexesSelector :: Selector
removeObjectsAtIndexesSelector = mkSelector "removeObjectsAtIndexes:"

-- | @Selector@ for @replaceObjectsAtIndexes:withObjects:@
replaceObjectsAtIndexes_withObjectsSelector :: Selector
replaceObjectsAtIndexes_withObjectsSelector = mkSelector "replaceObjectsAtIndexes:withObjects:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @sortUsingComparator:@
sortUsingComparatorSelector :: Selector
sortUsingComparatorSelector = mkSelector "sortUsingComparator:"

-- | @Selector@ for @sortWithOptions:usingComparator:@
sortWithOptions_usingComparatorSelector :: Selector
sortWithOptions_usingComparatorSelector = mkSelector "sortWithOptions:usingComparator:"

