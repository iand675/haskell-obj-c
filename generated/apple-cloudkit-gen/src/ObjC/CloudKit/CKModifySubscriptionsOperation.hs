{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKModifySubscriptionsOperation@.
module ObjC.CloudKit.CKModifySubscriptionsOperation
  ( CKModifySubscriptionsOperation
  , IsCKModifySubscriptionsOperation(..)
  , init_
  , initWithSubscriptionsToSave_subscriptionIDsToDelete
  , subscriptionsToSave
  , setSubscriptionsToSave
  , subscriptionIDsToDelete
  , setSubscriptionIDsToDelete
  , perSubscriptionSaveBlock
  , setPerSubscriptionSaveBlock
  , perSubscriptionDeleteBlock
  , setPerSubscriptionDeleteBlock
  , initSelector
  , initWithSubscriptionsToSave_subscriptionIDsToDeleteSelector
  , perSubscriptionDeleteBlockSelector
  , perSubscriptionSaveBlockSelector
  , setPerSubscriptionDeleteBlockSelector
  , setPerSubscriptionSaveBlockSelector
  , setSubscriptionIDsToDeleteSelector
  , setSubscriptionsToSaveSelector
  , subscriptionIDsToDeleteSelector
  , subscriptionsToSaveSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Id CKModifySubscriptionsOperation)
init_ ckModifySubscriptionsOperation =
  sendOwnedMessage ckModifySubscriptionsOperation initSelector

-- | @- initWithSubscriptionsToSave:subscriptionIDsToDelete:@
initWithSubscriptionsToSave_subscriptionIDsToDelete :: (IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation, IsNSArray subscriptionsToSave, IsNSArray subscriptionIDsToDelete) => ckModifySubscriptionsOperation -> subscriptionsToSave -> subscriptionIDsToDelete -> IO (Id CKModifySubscriptionsOperation)
initWithSubscriptionsToSave_subscriptionIDsToDelete ckModifySubscriptionsOperation subscriptionsToSave subscriptionIDsToDelete =
  sendOwnedMessage ckModifySubscriptionsOperation initWithSubscriptionsToSave_subscriptionIDsToDeleteSelector (toNSArray subscriptionsToSave) (toNSArray subscriptionIDsToDelete)

-- | @- subscriptionsToSave@
subscriptionsToSave :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Id NSArray)
subscriptionsToSave ckModifySubscriptionsOperation =
  sendMessage ckModifySubscriptionsOperation subscriptionsToSaveSelector

-- | @- setSubscriptionsToSave:@
setSubscriptionsToSave :: (IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation, IsNSArray value) => ckModifySubscriptionsOperation -> value -> IO ()
setSubscriptionsToSave ckModifySubscriptionsOperation value =
  sendMessage ckModifySubscriptionsOperation setSubscriptionsToSaveSelector (toNSArray value)

-- | @- subscriptionIDsToDelete@
subscriptionIDsToDelete :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Id NSArray)
subscriptionIDsToDelete ckModifySubscriptionsOperation =
  sendMessage ckModifySubscriptionsOperation subscriptionIDsToDeleteSelector

-- | @- setSubscriptionIDsToDelete:@
setSubscriptionIDsToDelete :: (IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation, IsNSArray value) => ckModifySubscriptionsOperation -> value -> IO ()
setSubscriptionIDsToDelete ckModifySubscriptionsOperation value =
  sendMessage ckModifySubscriptionsOperation setSubscriptionIDsToDeleteSelector (toNSArray value)

-- | Called on success or failure of a subscription save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perSubscriptionSaveBlock@
perSubscriptionSaveBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Ptr ())
perSubscriptionSaveBlock ckModifySubscriptionsOperation =
  sendMessage ckModifySubscriptionsOperation perSubscriptionSaveBlockSelector

-- | Called on success or failure of a subscription save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerSubscriptionSaveBlock:@
setPerSubscriptionSaveBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> Ptr () -> IO ()
setPerSubscriptionSaveBlock ckModifySubscriptionsOperation value =
  sendMessage ckModifySubscriptionsOperation setPerSubscriptionSaveBlockSelector value

-- | Called on success or failure of a subscription deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perSubscriptionDeleteBlock@
perSubscriptionDeleteBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Ptr ())
perSubscriptionDeleteBlock ckModifySubscriptionsOperation =
  sendMessage ckModifySubscriptionsOperation perSubscriptionDeleteBlockSelector

-- | Called on success or failure of a subscription deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerSubscriptionDeleteBlock:@
setPerSubscriptionDeleteBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> Ptr () -> IO ()
setPerSubscriptionDeleteBlock ckModifySubscriptionsOperation value =
  sendMessage ckModifySubscriptionsOperation setPerSubscriptionDeleteBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKModifySubscriptionsOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSubscriptionsToSave:subscriptionIDsToDelete:@
initWithSubscriptionsToSave_subscriptionIDsToDeleteSelector :: Selector '[Id NSArray, Id NSArray] (Id CKModifySubscriptionsOperation)
initWithSubscriptionsToSave_subscriptionIDsToDeleteSelector = mkSelector "initWithSubscriptionsToSave:subscriptionIDsToDelete:"

-- | @Selector@ for @subscriptionsToSave@
subscriptionsToSaveSelector :: Selector '[] (Id NSArray)
subscriptionsToSaveSelector = mkSelector "subscriptionsToSave"

-- | @Selector@ for @setSubscriptionsToSave:@
setSubscriptionsToSaveSelector :: Selector '[Id NSArray] ()
setSubscriptionsToSaveSelector = mkSelector "setSubscriptionsToSave:"

-- | @Selector@ for @subscriptionIDsToDelete@
subscriptionIDsToDeleteSelector :: Selector '[] (Id NSArray)
subscriptionIDsToDeleteSelector = mkSelector "subscriptionIDsToDelete"

-- | @Selector@ for @setSubscriptionIDsToDelete:@
setSubscriptionIDsToDeleteSelector :: Selector '[Id NSArray] ()
setSubscriptionIDsToDeleteSelector = mkSelector "setSubscriptionIDsToDelete:"

-- | @Selector@ for @perSubscriptionSaveBlock@
perSubscriptionSaveBlockSelector :: Selector '[] (Ptr ())
perSubscriptionSaveBlockSelector = mkSelector "perSubscriptionSaveBlock"

-- | @Selector@ for @setPerSubscriptionSaveBlock:@
setPerSubscriptionSaveBlockSelector :: Selector '[Ptr ()] ()
setPerSubscriptionSaveBlockSelector = mkSelector "setPerSubscriptionSaveBlock:"

-- | @Selector@ for @perSubscriptionDeleteBlock@
perSubscriptionDeleteBlockSelector :: Selector '[] (Ptr ())
perSubscriptionDeleteBlockSelector = mkSelector "perSubscriptionDeleteBlock"

-- | @Selector@ for @setPerSubscriptionDeleteBlock:@
setPerSubscriptionDeleteBlockSelector :: Selector '[Ptr ()] ()
setPerSubscriptionDeleteBlockSelector = mkSelector "setPerSubscriptionDeleteBlock:"

