{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKDiscoverAllUserIdentitiesOperation
--
-- Finds all discoverable users in the device's contacts database. No Contacts access dialog will be displayed.
--
-- This operation scales linearly with the number of email addresses and phone numbers in the device's address book.  It may take some time to complete.
--
-- Generated bindings for @CKDiscoverAllUserIdentitiesOperation@.
module ObjC.CloudKit.CKDiscoverAllUserIdentitiesOperation
  ( CKDiscoverAllUserIdentitiesOperation
  , IsCKDiscoverAllUserIdentitiesOperation(..)
  , init_
  , userIdentityDiscoveredBlock
  , setUserIdentityDiscoveredBlock
  , discoverAllUserIdentitiesCompletionBlock
  , setDiscoverAllUserIdentitiesCompletionBlock
  , discoverAllUserIdentitiesCompletionBlockSelector
  , initSelector
  , setDiscoverAllUserIdentitiesCompletionBlockSelector
  , setUserIdentityDiscoveredBlockSelector
  , userIdentityDiscoveredBlockSelector


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
init_ :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> IO (Id CKDiscoverAllUserIdentitiesOperation)
init_ ckDiscoverAllUserIdentitiesOperation =
  sendOwnedMessage ckDiscoverAllUserIdentitiesOperation initSelector

-- | Called once for each successfully-discovered user identity from the device's address book.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- userIdentityDiscoveredBlock@
userIdentityDiscoveredBlock :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> IO (Ptr ())
userIdentityDiscoveredBlock ckDiscoverAllUserIdentitiesOperation =
  sendMessage ckDiscoverAllUserIdentitiesOperation userIdentityDiscoveredBlockSelector

-- | Called once for each successfully-discovered user identity from the device's address book.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlock :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> Ptr () -> IO ()
setUserIdentityDiscoveredBlock ckDiscoverAllUserIdentitiesOperation value =
  sendMessage ckDiscoverAllUserIdentitiesOperation setUserIdentityDiscoveredBlockSelector value

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- discoverAllUserIdentitiesCompletionBlock@
discoverAllUserIdentitiesCompletionBlock :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> IO (Ptr ())
discoverAllUserIdentitiesCompletionBlock ckDiscoverAllUserIdentitiesOperation =
  sendMessage ckDiscoverAllUserIdentitiesOperation discoverAllUserIdentitiesCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setDiscoverAllUserIdentitiesCompletionBlock:@
setDiscoverAllUserIdentitiesCompletionBlock :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> Ptr () -> IO ()
setDiscoverAllUserIdentitiesCompletionBlock ckDiscoverAllUserIdentitiesOperation value =
  sendMessage ckDiscoverAllUserIdentitiesOperation setDiscoverAllUserIdentitiesCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKDiscoverAllUserIdentitiesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @userIdentityDiscoveredBlock@
userIdentityDiscoveredBlockSelector :: Selector '[] (Ptr ())
userIdentityDiscoveredBlockSelector = mkSelector "userIdentityDiscoveredBlock"

-- | @Selector@ for @setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlockSelector :: Selector '[Ptr ()] ()
setUserIdentityDiscoveredBlockSelector = mkSelector "setUserIdentityDiscoveredBlock:"

-- | @Selector@ for @discoverAllUserIdentitiesCompletionBlock@
discoverAllUserIdentitiesCompletionBlockSelector :: Selector '[] (Ptr ())
discoverAllUserIdentitiesCompletionBlockSelector = mkSelector "discoverAllUserIdentitiesCompletionBlock"

-- | @Selector@ for @setDiscoverAllUserIdentitiesCompletionBlock:@
setDiscoverAllUserIdentitiesCompletionBlockSelector :: Selector '[Ptr ()] ()
setDiscoverAllUserIdentitiesCompletionBlockSelector = mkSelector "setDiscoverAllUserIdentitiesCompletionBlock:"

