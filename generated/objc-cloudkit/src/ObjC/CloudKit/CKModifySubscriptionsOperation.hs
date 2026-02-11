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
  , subscriptionsToSaveSelector
  , setSubscriptionsToSaveSelector
  , subscriptionIDsToDeleteSelector
  , setSubscriptionIDsToDeleteSelector
  , perSubscriptionSaveBlockSelector
  , setPerSubscriptionSaveBlockSelector
  , perSubscriptionDeleteBlockSelector
  , setPerSubscriptionDeleteBlockSelector


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

-- | @- init@
init_ :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Id CKModifySubscriptionsOperation)
init_ ckModifySubscriptionsOperation  =
  sendMsg ckModifySubscriptionsOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSubscriptionsToSave:subscriptionIDsToDelete:@
initWithSubscriptionsToSave_subscriptionIDsToDelete :: (IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation, IsNSArray subscriptionsToSave, IsNSArray subscriptionIDsToDelete) => ckModifySubscriptionsOperation -> subscriptionsToSave -> subscriptionIDsToDelete -> IO (Id CKModifySubscriptionsOperation)
initWithSubscriptionsToSave_subscriptionIDsToDelete ckModifySubscriptionsOperation  subscriptionsToSave subscriptionIDsToDelete =
withObjCPtr subscriptionsToSave $ \raw_subscriptionsToSave ->
  withObjCPtr subscriptionIDsToDelete $ \raw_subscriptionIDsToDelete ->
      sendMsg ckModifySubscriptionsOperation (mkSelector "initWithSubscriptionsToSave:subscriptionIDsToDelete:") (retPtr retVoid) [argPtr (castPtr raw_subscriptionsToSave :: Ptr ()), argPtr (castPtr raw_subscriptionIDsToDelete :: Ptr ())] >>= ownedObject . castPtr

-- | @- subscriptionsToSave@
subscriptionsToSave :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Id NSArray)
subscriptionsToSave ckModifySubscriptionsOperation  =
  sendMsg ckModifySubscriptionsOperation (mkSelector "subscriptionsToSave") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubscriptionsToSave:@
setSubscriptionsToSave :: (IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation, IsNSArray value) => ckModifySubscriptionsOperation -> value -> IO ()
setSubscriptionsToSave ckModifySubscriptionsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifySubscriptionsOperation (mkSelector "setSubscriptionsToSave:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subscriptionIDsToDelete@
subscriptionIDsToDelete :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Id NSArray)
subscriptionIDsToDelete ckModifySubscriptionsOperation  =
  sendMsg ckModifySubscriptionsOperation (mkSelector "subscriptionIDsToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubscriptionIDsToDelete:@
setSubscriptionIDsToDelete :: (IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation, IsNSArray value) => ckModifySubscriptionsOperation -> value -> IO ()
setSubscriptionIDsToDelete ckModifySubscriptionsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifySubscriptionsOperation (mkSelector "setSubscriptionIDsToDelete:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called on success or failure of a subscription save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perSubscriptionSaveBlock@
perSubscriptionSaveBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Ptr ())
perSubscriptionSaveBlock ckModifySubscriptionsOperation  =
  fmap castPtr $ sendMsg ckModifySubscriptionsOperation (mkSelector "perSubscriptionSaveBlock") (retPtr retVoid) []

-- | Called on success or failure of a subscription save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerSubscriptionSaveBlock:@
setPerSubscriptionSaveBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> Ptr () -> IO ()
setPerSubscriptionSaveBlock ckModifySubscriptionsOperation  value =
  sendMsg ckModifySubscriptionsOperation (mkSelector "setPerSubscriptionSaveBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure of a subscription deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perSubscriptionDeleteBlock@
perSubscriptionDeleteBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> IO (Ptr ())
perSubscriptionDeleteBlock ckModifySubscriptionsOperation  =
  fmap castPtr $ sendMsg ckModifySubscriptionsOperation (mkSelector "perSubscriptionDeleteBlock") (retPtr retVoid) []

-- | Called on success or failure of a subscription deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerSubscriptionDeleteBlock:@
setPerSubscriptionDeleteBlock :: IsCKModifySubscriptionsOperation ckModifySubscriptionsOperation => ckModifySubscriptionsOperation -> Ptr () -> IO ()
setPerSubscriptionDeleteBlock ckModifySubscriptionsOperation  value =
  sendMsg ckModifySubscriptionsOperation (mkSelector "setPerSubscriptionDeleteBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSubscriptionsToSave:subscriptionIDsToDelete:@
initWithSubscriptionsToSave_subscriptionIDsToDeleteSelector :: Selector
initWithSubscriptionsToSave_subscriptionIDsToDeleteSelector = mkSelector "initWithSubscriptionsToSave:subscriptionIDsToDelete:"

-- | @Selector@ for @subscriptionsToSave@
subscriptionsToSaveSelector :: Selector
subscriptionsToSaveSelector = mkSelector "subscriptionsToSave"

-- | @Selector@ for @setSubscriptionsToSave:@
setSubscriptionsToSaveSelector :: Selector
setSubscriptionsToSaveSelector = mkSelector "setSubscriptionsToSave:"

-- | @Selector@ for @subscriptionIDsToDelete@
subscriptionIDsToDeleteSelector :: Selector
subscriptionIDsToDeleteSelector = mkSelector "subscriptionIDsToDelete"

-- | @Selector@ for @setSubscriptionIDsToDelete:@
setSubscriptionIDsToDeleteSelector :: Selector
setSubscriptionIDsToDeleteSelector = mkSelector "setSubscriptionIDsToDelete:"

-- | @Selector@ for @perSubscriptionSaveBlock@
perSubscriptionSaveBlockSelector :: Selector
perSubscriptionSaveBlockSelector = mkSelector "perSubscriptionSaveBlock"

-- | @Selector@ for @setPerSubscriptionSaveBlock:@
setPerSubscriptionSaveBlockSelector :: Selector
setPerSubscriptionSaveBlockSelector = mkSelector "setPerSubscriptionSaveBlock:"

-- | @Selector@ for @perSubscriptionDeleteBlock@
perSubscriptionDeleteBlockSelector :: Selector
perSubscriptionDeleteBlockSelector = mkSelector "perSubscriptionDeleteBlock"

-- | @Selector@ for @setPerSubscriptionDeleteBlock:@
setPerSubscriptionDeleteBlockSelector :: Selector
setPerSubscriptionDeleteBlockSelector = mkSelector "setPerSubscriptionDeleteBlock:"

