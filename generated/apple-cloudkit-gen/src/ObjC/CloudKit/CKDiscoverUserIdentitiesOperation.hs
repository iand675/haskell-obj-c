{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKDiscoverUserIdentitiesOperation@.
module ObjC.CloudKit.CKDiscoverUserIdentitiesOperation
  ( CKDiscoverUserIdentitiesOperation
  , IsCKDiscoverUserIdentitiesOperation(..)
  , init_
  , initWithUserIdentityLookupInfos
  , userIdentityLookupInfos
  , setUserIdentityLookupInfos
  , userIdentityDiscoveredBlock
  , setUserIdentityDiscoveredBlock
  , discoverUserIdentitiesCompletionBlock
  , setDiscoverUserIdentitiesCompletionBlock
  , discoverUserIdentitiesCompletionBlockSelector
  , initSelector
  , initWithUserIdentityLookupInfosSelector
  , setDiscoverUserIdentitiesCompletionBlockSelector
  , setUserIdentityDiscoveredBlockSelector
  , setUserIdentityLookupInfosSelector
  , userIdentityDiscoveredBlockSelector
  , userIdentityLookupInfosSelector


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
init_ :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Id CKDiscoverUserIdentitiesOperation)
init_ ckDiscoverUserIdentitiesOperation =
  sendOwnedMessage ckDiscoverUserIdentitiesOperation initSelector

-- | @- initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfos :: (IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation, IsNSArray userIdentityLookupInfos) => ckDiscoverUserIdentitiesOperation -> userIdentityLookupInfos -> IO (Id CKDiscoverUserIdentitiesOperation)
initWithUserIdentityLookupInfos ckDiscoverUserIdentitiesOperation userIdentityLookupInfos =
  sendOwnedMessage ckDiscoverUserIdentitiesOperation initWithUserIdentityLookupInfosSelector (toNSArray userIdentityLookupInfos)

-- | @- userIdentityLookupInfos@
userIdentityLookupInfos :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Id NSArray)
userIdentityLookupInfos ckDiscoverUserIdentitiesOperation =
  sendMessage ckDiscoverUserIdentitiesOperation userIdentityLookupInfosSelector

-- | @- setUserIdentityLookupInfos:@
setUserIdentityLookupInfos :: (IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation, IsNSArray value) => ckDiscoverUserIdentitiesOperation -> value -> IO ()
setUserIdentityLookupInfos ckDiscoverUserIdentitiesOperation value =
  sendMessage ckDiscoverUserIdentitiesOperation setUserIdentityLookupInfosSelector (toNSArray value)

-- | Called once for each user identity lookup info that was successfully discovered on the server
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- userIdentityDiscoveredBlock@
userIdentityDiscoveredBlock :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Ptr ())
userIdentityDiscoveredBlock ckDiscoverUserIdentitiesOperation =
  sendMessage ckDiscoverUserIdentitiesOperation userIdentityDiscoveredBlockSelector

-- | Called once for each user identity lookup info that was successfully discovered on the server
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlock :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> Ptr () -> IO ()
setUserIdentityDiscoveredBlock ckDiscoverUserIdentitiesOperation value =
  sendMessage ckDiscoverUserIdentitiesOperation setUserIdentityDiscoveredBlockSelector value

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- discoverUserIdentitiesCompletionBlock@
discoverUserIdentitiesCompletionBlock :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Ptr ())
discoverUserIdentitiesCompletionBlock ckDiscoverUserIdentitiesOperation =
  sendMessage ckDiscoverUserIdentitiesOperation discoverUserIdentitiesCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setDiscoverUserIdentitiesCompletionBlock:@
setDiscoverUserIdentitiesCompletionBlock :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> Ptr () -> IO ()
setDiscoverUserIdentitiesCompletionBlock ckDiscoverUserIdentitiesOperation value =
  sendMessage ckDiscoverUserIdentitiesOperation setDiscoverUserIdentitiesCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKDiscoverUserIdentitiesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfosSelector :: Selector '[Id NSArray] (Id CKDiscoverUserIdentitiesOperation)
initWithUserIdentityLookupInfosSelector = mkSelector "initWithUserIdentityLookupInfos:"

-- | @Selector@ for @userIdentityLookupInfos@
userIdentityLookupInfosSelector :: Selector '[] (Id NSArray)
userIdentityLookupInfosSelector = mkSelector "userIdentityLookupInfos"

-- | @Selector@ for @setUserIdentityLookupInfos:@
setUserIdentityLookupInfosSelector :: Selector '[Id NSArray] ()
setUserIdentityLookupInfosSelector = mkSelector "setUserIdentityLookupInfos:"

-- | @Selector@ for @userIdentityDiscoveredBlock@
userIdentityDiscoveredBlockSelector :: Selector '[] (Ptr ())
userIdentityDiscoveredBlockSelector = mkSelector "userIdentityDiscoveredBlock"

-- | @Selector@ for @setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlockSelector :: Selector '[Ptr ()] ()
setUserIdentityDiscoveredBlockSelector = mkSelector "setUserIdentityDiscoveredBlock:"

-- | @Selector@ for @discoverUserIdentitiesCompletionBlock@
discoverUserIdentitiesCompletionBlockSelector :: Selector '[] (Ptr ())
discoverUserIdentitiesCompletionBlockSelector = mkSelector "discoverUserIdentitiesCompletionBlock"

-- | @Selector@ for @setDiscoverUserIdentitiesCompletionBlock:@
setDiscoverUserIdentitiesCompletionBlockSelector :: Selector '[Ptr ()] ()
setDiscoverUserIdentitiesCompletionBlockSelector = mkSelector "setDiscoverUserIdentitiesCompletionBlock:"

