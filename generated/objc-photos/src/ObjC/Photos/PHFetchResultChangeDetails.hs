{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHFetchResultChangeDetails@.
module ObjC.Photos.PHFetchResultChangeDetails
  ( PHFetchResultChangeDetails
  , IsPHFetchResultChangeDetails(..)
  , enumerateMovesWithBlock
  , changeDetailsFromFetchResult_toFetchResult_changedObjects
  , fetchResultBeforeChanges
  , fetchResultAfterChanges
  , hasIncrementalChanges
  , removedIndexes
  , removedObjects
  , insertedIndexes
  , insertedObjects
  , changedIndexes
  , changedObjects
  , hasMoves
  , enumerateMovesWithBlockSelector
  , changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector
  , fetchResultBeforeChangesSelector
  , fetchResultAfterChangesSelector
  , hasIncrementalChangesSelector
  , removedIndexesSelector
  , removedObjectsSelector
  , insertedIndexesSelector
  , insertedObjectsSelector
  , changedIndexesSelector
  , changedObjectsSelector
  , hasMovesSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enumerateMovesWithBlock:@
enumerateMovesWithBlock :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> Ptr () -> IO ()
enumerateMovesWithBlock phFetchResultChangeDetails  handler =
  sendMsg phFetchResultChangeDetails (mkSelector "enumerateMovesWithBlock:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @+ changeDetailsFromFetchResult:toFetchResult:changedObjects:@
changeDetailsFromFetchResult_toFetchResult_changedObjects :: (IsPHFetchResult fromResult, IsPHFetchResult toResult, IsNSArray changedObjects) => fromResult -> toResult -> changedObjects -> IO (Id PHFetchResultChangeDetails)
changeDetailsFromFetchResult_toFetchResult_changedObjects fromResult toResult changedObjects =
  do
    cls' <- getRequiredClass "PHFetchResultChangeDetails"
    withObjCPtr fromResult $ \raw_fromResult ->
      withObjCPtr toResult $ \raw_toResult ->
        withObjCPtr changedObjects $ \raw_changedObjects ->
          sendClassMsg cls' (mkSelector "changeDetailsFromFetchResult:toFetchResult:changedObjects:") (retPtr retVoid) [argPtr (castPtr raw_fromResult :: Ptr ()), argPtr (castPtr raw_toResult :: Ptr ()), argPtr (castPtr raw_changedObjects :: Ptr ())] >>= retainedObject . castPtr

-- | @- fetchResultBeforeChanges@
fetchResultBeforeChanges :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id PHFetchResult)
fetchResultBeforeChanges phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "fetchResultBeforeChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchResultAfterChanges@
fetchResultAfterChanges :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id PHFetchResult)
fetchResultAfterChanges phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "fetchResultAfterChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasIncrementalChanges@
hasIncrementalChanges :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO Bool
hasIncrementalChanges phFetchResultChangeDetails  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phFetchResultChangeDetails (mkSelector "hasIncrementalChanges") retCULong []

-- | @- removedIndexes@
removedIndexes :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSIndexSet)
removedIndexes phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "removedIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- removedObjects@
removedObjects :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSArray)
removedObjects phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "removedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- insertedIndexes@
insertedIndexes :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSIndexSet)
insertedIndexes phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "insertedIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- insertedObjects@
insertedObjects :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSArray)
insertedObjects phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "insertedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changedIndexes@
changedIndexes :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSIndexSet)
changedIndexes phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "changedIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changedObjects@
changedObjects :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSArray)
changedObjects phFetchResultChangeDetails  =
  sendMsg phFetchResultChangeDetails (mkSelector "changedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasMoves@
hasMoves :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO Bool
hasMoves phFetchResultChangeDetails  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phFetchResultChangeDetails (mkSelector "hasMoves") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enumerateMovesWithBlock:@
enumerateMovesWithBlockSelector :: Selector
enumerateMovesWithBlockSelector = mkSelector "enumerateMovesWithBlock:"

-- | @Selector@ for @changeDetailsFromFetchResult:toFetchResult:changedObjects:@
changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector :: Selector
changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector = mkSelector "changeDetailsFromFetchResult:toFetchResult:changedObjects:"

-- | @Selector@ for @fetchResultBeforeChanges@
fetchResultBeforeChangesSelector :: Selector
fetchResultBeforeChangesSelector = mkSelector "fetchResultBeforeChanges"

-- | @Selector@ for @fetchResultAfterChanges@
fetchResultAfterChangesSelector :: Selector
fetchResultAfterChangesSelector = mkSelector "fetchResultAfterChanges"

-- | @Selector@ for @hasIncrementalChanges@
hasIncrementalChangesSelector :: Selector
hasIncrementalChangesSelector = mkSelector "hasIncrementalChanges"

-- | @Selector@ for @removedIndexes@
removedIndexesSelector :: Selector
removedIndexesSelector = mkSelector "removedIndexes"

-- | @Selector@ for @removedObjects@
removedObjectsSelector :: Selector
removedObjectsSelector = mkSelector "removedObjects"

-- | @Selector@ for @insertedIndexes@
insertedIndexesSelector :: Selector
insertedIndexesSelector = mkSelector "insertedIndexes"

-- | @Selector@ for @insertedObjects@
insertedObjectsSelector :: Selector
insertedObjectsSelector = mkSelector "insertedObjects"

-- | @Selector@ for @changedIndexes@
changedIndexesSelector :: Selector
changedIndexesSelector = mkSelector "changedIndexes"

-- | @Selector@ for @changedObjects@
changedObjectsSelector :: Selector
changedObjectsSelector = mkSelector "changedObjects"

-- | @Selector@ for @hasMoves@
hasMovesSelector :: Selector
hasMovesSelector = mkSelector "hasMoves"

