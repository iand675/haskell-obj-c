{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKFetchSubscriptionsOperation@.
module ObjC.CloudKit.CKFetchSubscriptionsOperation
  ( CKFetchSubscriptionsOperation
  , IsCKFetchSubscriptionsOperation(..)
  , fetchAllSubscriptionsOperation
  , init_
  , initWithSubscriptionIDs
  , subscriptionIDs
  , setSubscriptionIDs
  , perSubscriptionCompletionBlock
  , setPerSubscriptionCompletionBlock
  , fetchAllSubscriptionsOperationSelector
  , initSelector
  , initWithSubscriptionIDsSelector
  , perSubscriptionCompletionBlockSelector
  , setPerSubscriptionCompletionBlockSelector
  , setSubscriptionIDsSelector
  , subscriptionIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fetchAllSubscriptionsOperation@
fetchAllSubscriptionsOperation :: IO (Id CKFetchSubscriptionsOperation)
fetchAllSubscriptionsOperation  =
  do
    cls' <- getRequiredClass "CKFetchSubscriptionsOperation"
    sendClassMessage cls' fetchAllSubscriptionsOperationSelector

-- | @- init@
init_ :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> IO (Id CKFetchSubscriptionsOperation)
init_ ckFetchSubscriptionsOperation =
  sendOwnedMessage ckFetchSubscriptionsOperation initSelector

-- | @- initWithSubscriptionIDs:@
initWithSubscriptionIDs :: (IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation, IsNSArray subscriptionIDs) => ckFetchSubscriptionsOperation -> subscriptionIDs -> IO (Id CKFetchSubscriptionsOperation)
initWithSubscriptionIDs ckFetchSubscriptionsOperation subscriptionIDs =
  sendOwnedMessage ckFetchSubscriptionsOperation initWithSubscriptionIDsSelector (toNSArray subscriptionIDs)

-- | @- subscriptionIDs@
subscriptionIDs :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> IO (Id NSArray)
subscriptionIDs ckFetchSubscriptionsOperation =
  sendMessage ckFetchSubscriptionsOperation subscriptionIDsSelector

-- | @- setSubscriptionIDs:@
setSubscriptionIDs :: (IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation, IsNSArray value) => ckFetchSubscriptionsOperation -> value -> IO ()
setSubscriptionIDs ckFetchSubscriptionsOperation value =
  sendMessage ckFetchSubscriptionsOperation setSubscriptionIDsSelector (toNSArray value)

-- | Called on success or failure for each subscriptionID.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perSubscriptionCompletionBlock@
perSubscriptionCompletionBlock :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> IO (Ptr ())
perSubscriptionCompletionBlock ckFetchSubscriptionsOperation =
  sendMessage ckFetchSubscriptionsOperation perSubscriptionCompletionBlockSelector

-- | Called on success or failure for each subscriptionID.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerSubscriptionCompletionBlock:@
setPerSubscriptionCompletionBlock :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> Ptr () -> IO ()
setPerSubscriptionCompletionBlock ckFetchSubscriptionsOperation value =
  sendMessage ckFetchSubscriptionsOperation setPerSubscriptionCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAllSubscriptionsOperation@
fetchAllSubscriptionsOperationSelector :: Selector '[] (Id CKFetchSubscriptionsOperation)
fetchAllSubscriptionsOperationSelector = mkSelector "fetchAllSubscriptionsOperation"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchSubscriptionsOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSubscriptionIDs:@
initWithSubscriptionIDsSelector :: Selector '[Id NSArray] (Id CKFetchSubscriptionsOperation)
initWithSubscriptionIDsSelector = mkSelector "initWithSubscriptionIDs:"

-- | @Selector@ for @subscriptionIDs@
subscriptionIDsSelector :: Selector '[] (Id NSArray)
subscriptionIDsSelector = mkSelector "subscriptionIDs"

-- | @Selector@ for @setSubscriptionIDs:@
setSubscriptionIDsSelector :: Selector '[Id NSArray] ()
setSubscriptionIDsSelector = mkSelector "setSubscriptionIDs:"

-- | @Selector@ for @perSubscriptionCompletionBlock@
perSubscriptionCompletionBlockSelector :: Selector '[] (Ptr ())
perSubscriptionCompletionBlockSelector = mkSelector "perSubscriptionCompletionBlock"

-- | @Selector@ for @setPerSubscriptionCompletionBlock:@
setPerSubscriptionCompletionBlockSelector :: Selector '[Ptr ()] ()
setPerSubscriptionCompletionBlockSelector = mkSelector "setPerSubscriptionCompletionBlock:"

