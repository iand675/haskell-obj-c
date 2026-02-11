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
  , subscriptionIDsSelector
  , setSubscriptionIDsSelector
  , perSubscriptionCompletionBlockSelector
  , setPerSubscriptionCompletionBlockSelector


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

-- | @+ fetchAllSubscriptionsOperation@
fetchAllSubscriptionsOperation :: IO (Id CKFetchSubscriptionsOperation)
fetchAllSubscriptionsOperation  =
  do
    cls' <- getRequiredClass "CKFetchSubscriptionsOperation"
    sendClassMsg cls' (mkSelector "fetchAllSubscriptionsOperation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> IO (Id CKFetchSubscriptionsOperation)
init_ ckFetchSubscriptionsOperation  =
  sendMsg ckFetchSubscriptionsOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSubscriptionIDs:@
initWithSubscriptionIDs :: (IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation, IsNSArray subscriptionIDs) => ckFetchSubscriptionsOperation -> subscriptionIDs -> IO (Id CKFetchSubscriptionsOperation)
initWithSubscriptionIDs ckFetchSubscriptionsOperation  subscriptionIDs =
withObjCPtr subscriptionIDs $ \raw_subscriptionIDs ->
    sendMsg ckFetchSubscriptionsOperation (mkSelector "initWithSubscriptionIDs:") (retPtr retVoid) [argPtr (castPtr raw_subscriptionIDs :: Ptr ())] >>= ownedObject . castPtr

-- | @- subscriptionIDs@
subscriptionIDs :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> IO (Id NSArray)
subscriptionIDs ckFetchSubscriptionsOperation  =
  sendMsg ckFetchSubscriptionsOperation (mkSelector "subscriptionIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubscriptionIDs:@
setSubscriptionIDs :: (IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation, IsNSArray value) => ckFetchSubscriptionsOperation -> value -> IO ()
setSubscriptionIDs ckFetchSubscriptionsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchSubscriptionsOperation (mkSelector "setSubscriptionIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called on success or failure for each subscriptionID.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perSubscriptionCompletionBlock@
perSubscriptionCompletionBlock :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> IO (Ptr ())
perSubscriptionCompletionBlock ckFetchSubscriptionsOperation  =
  fmap castPtr $ sendMsg ckFetchSubscriptionsOperation (mkSelector "perSubscriptionCompletionBlock") (retPtr retVoid) []

-- | Called on success or failure for each subscriptionID.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerSubscriptionCompletionBlock:@
setPerSubscriptionCompletionBlock :: IsCKFetchSubscriptionsOperation ckFetchSubscriptionsOperation => ckFetchSubscriptionsOperation -> Ptr () -> IO ()
setPerSubscriptionCompletionBlock ckFetchSubscriptionsOperation  value =
  sendMsg ckFetchSubscriptionsOperation (mkSelector "setPerSubscriptionCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAllSubscriptionsOperation@
fetchAllSubscriptionsOperationSelector :: Selector
fetchAllSubscriptionsOperationSelector = mkSelector "fetchAllSubscriptionsOperation"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSubscriptionIDs:@
initWithSubscriptionIDsSelector :: Selector
initWithSubscriptionIDsSelector = mkSelector "initWithSubscriptionIDs:"

-- | @Selector@ for @subscriptionIDs@
subscriptionIDsSelector :: Selector
subscriptionIDsSelector = mkSelector "subscriptionIDs"

-- | @Selector@ for @setSubscriptionIDs:@
setSubscriptionIDsSelector :: Selector
setSubscriptionIDsSelector = mkSelector "setSubscriptionIDs:"

-- | @Selector@ for @perSubscriptionCompletionBlock@
perSubscriptionCompletionBlockSelector :: Selector
perSubscriptionCompletionBlockSelector = mkSelector "perSubscriptionCompletionBlock"

-- | @Selector@ for @setPerSubscriptionCompletionBlock:@
setPerSubscriptionCompletionBlockSelector :: Selector
setPerSubscriptionCompletionBlockSelector = mkSelector "setPerSubscriptionCompletionBlock:"

