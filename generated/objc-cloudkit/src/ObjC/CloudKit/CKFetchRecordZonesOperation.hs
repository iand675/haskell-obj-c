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
  , recordZoneIDsSelector
  , setRecordZoneIDsSelector
  , perRecordZoneCompletionBlockSelector
  , setPerRecordZoneCompletionBlockSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fetchAllRecordZonesOperation@
fetchAllRecordZonesOperation :: IO (Id CKFetchRecordZonesOperation)
fetchAllRecordZonesOperation  =
  do
    cls' <- getRequiredClass "CKFetchRecordZonesOperation"
    sendClassMsg cls' (mkSelector "fetchAllRecordZonesOperation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> IO (Id CKFetchRecordZonesOperation)
init_ ckFetchRecordZonesOperation  =
  sendMsg ckFetchRecordZonesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordZoneIDs:@
initWithRecordZoneIDs :: (IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation, IsNSArray zoneIDs) => ckFetchRecordZonesOperation -> zoneIDs -> IO (Id CKFetchRecordZonesOperation)
initWithRecordZoneIDs ckFetchRecordZonesOperation  zoneIDs =
withObjCPtr zoneIDs $ \raw_zoneIDs ->
    sendMsg ckFetchRecordZonesOperation (mkSelector "initWithRecordZoneIDs:") (retPtr retVoid) [argPtr (castPtr raw_zoneIDs :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordZoneIDs@
recordZoneIDs :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> IO (Id NSArray)
recordZoneIDs ckFetchRecordZonesOperation  =
  sendMsg ckFetchRecordZonesOperation (mkSelector "recordZoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordZoneIDs:@
setRecordZoneIDs :: (IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation, IsNSArray value) => ckFetchRecordZonesOperation -> value -> IO ()
setRecordZoneIDs ckFetchRecordZonesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZonesOperation (mkSelector "setRecordZoneIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called on success or failure for each record zone.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordZoneCompletionBlock@
perRecordZoneCompletionBlock :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> IO (Ptr ())
perRecordZoneCompletionBlock ckFetchRecordZonesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZonesOperation (mkSelector "perRecordZoneCompletionBlock") (retPtr retVoid) []

-- | Called on success or failure for each record zone.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordZoneCompletionBlock:@
setPerRecordZoneCompletionBlock :: IsCKFetchRecordZonesOperation ckFetchRecordZonesOperation => ckFetchRecordZonesOperation -> Ptr () -> IO ()
setPerRecordZoneCompletionBlock ckFetchRecordZonesOperation  value =
  sendMsg ckFetchRecordZonesOperation (mkSelector "setPerRecordZoneCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAllRecordZonesOperation@
fetchAllRecordZonesOperationSelector :: Selector
fetchAllRecordZonesOperationSelector = mkSelector "fetchAllRecordZonesOperation"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZoneIDs:@
initWithRecordZoneIDsSelector :: Selector
initWithRecordZoneIDsSelector = mkSelector "initWithRecordZoneIDs:"

-- | @Selector@ for @recordZoneIDs@
recordZoneIDsSelector :: Selector
recordZoneIDsSelector = mkSelector "recordZoneIDs"

-- | @Selector@ for @setRecordZoneIDs:@
setRecordZoneIDsSelector :: Selector
setRecordZoneIDsSelector = mkSelector "setRecordZoneIDs:"

-- | @Selector@ for @perRecordZoneCompletionBlock@
perRecordZoneCompletionBlockSelector :: Selector
perRecordZoneCompletionBlockSelector = mkSelector "perRecordZoneCompletionBlock"

-- | @Selector@ for @setPerRecordZoneCompletionBlock:@
setPerRecordZoneCompletionBlockSelector :: Selector
setPerRecordZoneCompletionBlockSelector = mkSelector "setPerRecordZoneCompletionBlock:"

