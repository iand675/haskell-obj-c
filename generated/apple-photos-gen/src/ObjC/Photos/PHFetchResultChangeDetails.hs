{-# LANGUAGE DataKinds #-}
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
  , changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector
  , changedIndexesSelector
  , changedObjectsSelector
  , enumerateMovesWithBlockSelector
  , fetchResultAfterChangesSelector
  , fetchResultBeforeChangesSelector
  , hasIncrementalChangesSelector
  , hasMovesSelector
  , insertedIndexesSelector
  , insertedObjectsSelector
  , removedIndexesSelector
  , removedObjectsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enumerateMovesWithBlock:@
enumerateMovesWithBlock :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> Ptr () -> IO ()
enumerateMovesWithBlock phFetchResultChangeDetails handler =
  sendMessage phFetchResultChangeDetails enumerateMovesWithBlockSelector handler

-- | @+ changeDetailsFromFetchResult:toFetchResult:changedObjects:@
changeDetailsFromFetchResult_toFetchResult_changedObjects :: (IsPHFetchResult fromResult, IsPHFetchResult toResult, IsNSArray changedObjects) => fromResult -> toResult -> changedObjects -> IO (Id PHFetchResultChangeDetails)
changeDetailsFromFetchResult_toFetchResult_changedObjects fromResult toResult changedObjects =
  do
    cls' <- getRequiredClass "PHFetchResultChangeDetails"
    sendClassMessage cls' changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector (toPHFetchResult fromResult) (toPHFetchResult toResult) (toNSArray changedObjects)

-- | @- fetchResultBeforeChanges@
fetchResultBeforeChanges :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id PHFetchResult)
fetchResultBeforeChanges phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails fetchResultBeforeChangesSelector

-- | @- fetchResultAfterChanges@
fetchResultAfterChanges :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id PHFetchResult)
fetchResultAfterChanges phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails fetchResultAfterChangesSelector

-- | @- hasIncrementalChanges@
hasIncrementalChanges :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO Bool
hasIncrementalChanges phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails hasIncrementalChangesSelector

-- | @- removedIndexes@
removedIndexes :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSIndexSet)
removedIndexes phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails removedIndexesSelector

-- | @- removedObjects@
removedObjects :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSArray)
removedObjects phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails removedObjectsSelector

-- | @- insertedIndexes@
insertedIndexes :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSIndexSet)
insertedIndexes phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails insertedIndexesSelector

-- | @- insertedObjects@
insertedObjects :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSArray)
insertedObjects phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails insertedObjectsSelector

-- | @- changedIndexes@
changedIndexes :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSIndexSet)
changedIndexes phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails changedIndexesSelector

-- | @- changedObjects@
changedObjects :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO (Id NSArray)
changedObjects phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails changedObjectsSelector

-- | @- hasMoves@
hasMoves :: IsPHFetchResultChangeDetails phFetchResultChangeDetails => phFetchResultChangeDetails -> IO Bool
hasMoves phFetchResultChangeDetails =
  sendMessage phFetchResultChangeDetails hasMovesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enumerateMovesWithBlock:@
enumerateMovesWithBlockSelector :: Selector '[Ptr ()] ()
enumerateMovesWithBlockSelector = mkSelector "enumerateMovesWithBlock:"

-- | @Selector@ for @changeDetailsFromFetchResult:toFetchResult:changedObjects:@
changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector :: Selector '[Id PHFetchResult, Id PHFetchResult, Id NSArray] (Id PHFetchResultChangeDetails)
changeDetailsFromFetchResult_toFetchResult_changedObjectsSelector = mkSelector "changeDetailsFromFetchResult:toFetchResult:changedObjects:"

-- | @Selector@ for @fetchResultBeforeChanges@
fetchResultBeforeChangesSelector :: Selector '[] (Id PHFetchResult)
fetchResultBeforeChangesSelector = mkSelector "fetchResultBeforeChanges"

-- | @Selector@ for @fetchResultAfterChanges@
fetchResultAfterChangesSelector :: Selector '[] (Id PHFetchResult)
fetchResultAfterChangesSelector = mkSelector "fetchResultAfterChanges"

-- | @Selector@ for @hasIncrementalChanges@
hasIncrementalChangesSelector :: Selector '[] Bool
hasIncrementalChangesSelector = mkSelector "hasIncrementalChanges"

-- | @Selector@ for @removedIndexes@
removedIndexesSelector :: Selector '[] (Id NSIndexSet)
removedIndexesSelector = mkSelector "removedIndexes"

-- | @Selector@ for @removedObjects@
removedObjectsSelector :: Selector '[] (Id NSArray)
removedObjectsSelector = mkSelector "removedObjects"

-- | @Selector@ for @insertedIndexes@
insertedIndexesSelector :: Selector '[] (Id NSIndexSet)
insertedIndexesSelector = mkSelector "insertedIndexes"

-- | @Selector@ for @insertedObjects@
insertedObjectsSelector :: Selector '[] (Id NSArray)
insertedObjectsSelector = mkSelector "insertedObjects"

-- | @Selector@ for @changedIndexes@
changedIndexesSelector :: Selector '[] (Id NSIndexSet)
changedIndexesSelector = mkSelector "changedIndexes"

-- | @Selector@ for @changedObjects@
changedObjectsSelector :: Selector '[] (Id NSArray)
changedObjectsSelector = mkSelector "changedObjects"

-- | @Selector@ for @hasMoves@
hasMovesSelector :: Selector '[] Bool
hasMovesSelector = mkSelector "hasMoves"

