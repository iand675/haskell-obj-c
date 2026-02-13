{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKFetchRecordZonesOperation@.
module ObjC.CloudKit.CKFetchRecordZonesOperation
  ( CKFetchRecordZonesOperation
  , IsCKFetchRecordZonesOperation(..)
  , fetchAllRecordZonesOperation
  , init_
  , initWithRecordZoneIDs
  , recordZoneIDs
  , setRecordZoneIDs
  , perRecordZoneCompletionBlock
  , setPerRecordZoneCompletionBlock
  , fetchAllRecordZonesOperationSelector
  , initSelector
  , initWithRecordZoneIDsSelector
  , perRecordZoneCompletionBlockSelector
  , recordZoneIDsSelector
  , setPerRecordZoneCompletionBlockSelector
  , setRecordZoneIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fetchAllRecordZonesOperation@
fetchAllRecordZonesOperation :: IO (Id CKFetchRecordZonesOperation)
fetchAllRecordZonesOperation  =
  do
    cls' <- getRequiredClass "CKFetchRecordZonesOperation"
    sendClassMessage cls' fetchAllRecordZonesOperationSelector

-- | @- init@
init_ :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> IO (Id CKFetchRecordZonesOperation)
init_ ckFetchRecordZonesOperation =
  sendOwnedMessage ckFetchRecordZonesOperation initSelector

-- | @- initWithRecordZoneIDs:@
initWithRecordZoneIDs :: (IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation, IsNSArray zoneIDs) => ckFetchRecordZonesOperation -> zoneIDs -> IO (Id CKFetchRecordZonesOperation)
initWithRecordZoneIDs ckFetchRecordZonesOperation zoneIDs =
  sendOwnedMessage ckFetchRecordZonesOperation initWithRecordZoneIDsSelector (toNSArray zoneIDs)

-- | @- recordZoneIDs@
recordZoneIDs :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> IO (Id NSArray)
recordZoneIDs ckFetchRecordZonesOperation =
  sendMessage ckFetchRecordZonesOperation recordZoneIDsSelector

-- | @- setRecordZoneIDs:@
setRecordZoneIDs :: (IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation, IsNSArray value) => ckFetchRecordZonesOperation -> value -> IO ()
setRecordZoneIDs ckFetchRecordZonesOperation value =
  sendMessage ckFetchRecordZonesOperation setRecordZoneIDsSelector (toNSArray value)

-- | Called on success or failure for each record zone.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordZoneCompletionBlock@
perRecordZoneCompletionBlock :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> IO (Ptr ())
perRecordZoneCompletionBlock ckFetchRecordZonesOperation =
  sendMessage ckFetchRecordZonesOperation perRecordZoneCompletionBlockSelector

-- | Called on success or failure for each record zone.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordZoneCompletionBlock:@
setPerRecordZoneCompletionBlock :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> Ptr () -> IO ()
setPerRecordZoneCompletionBlock ckFetchRecordZonesOperation value =
  sendMessage ckFetchRecordZonesOperation setPerRecordZoneCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAllRecordZonesOperation@
fetchAllRecordZonesOperationSelector :: Selector '[] (Id CKFetchRecordZonesOperation)
fetchAllRecordZonesOperationSelector = mkSelector "fetchAllRecordZonesOperation"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchRecordZonesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZoneIDs:@
initWithRecordZoneIDsSelector :: Selector '[Id NSArray] (Id CKFetchRecordZonesOperation)
initWithRecordZoneIDsSelector = mkSelector "initWithRecordZoneIDs:"

-- | @Selector@ for @recordZoneIDs@
recordZoneIDsSelector :: Selector '[] (Id NSArray)
recordZoneIDsSelector = mkSelector "recordZoneIDs"

-- | @Selector@ for @setRecordZoneIDs:@
setRecordZoneIDsSelector :: Selector '[Id NSArray] ()
setRecordZoneIDsSelector = mkSelector "setRecordZoneIDs:"

-- | @Selector@ for @perRecordZoneCompletionBlock@
perRecordZoneCompletionBlockSelector :: Selector '[] (Ptr ())
perRecordZoneCompletionBlockSelector = mkSelector "perRecordZoneCompletionBlock"

-- | @Selector@ for @setPerRecordZoneCompletionBlock:@
setPerRecordZoneCompletionBlockSelector :: Selector '[Ptr ()] ()
setPerRecordZoneCompletionBlockSelector = mkSelector "setPerRecordZoneCompletionBlock:"

