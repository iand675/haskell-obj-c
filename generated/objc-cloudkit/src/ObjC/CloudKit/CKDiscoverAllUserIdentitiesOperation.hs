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
  , initSelector
  , userIdentityDiscoveredBlockSelector
  , setUserIdentityDiscoveredBlockSelector
  , discoverAllUserIdentitiesCompletionBlockSelector
  , setDiscoverAllUserIdentitiesCompletionBlockSelector


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
init_ :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> IO (Id CKDiscoverAllUserIdentitiesOperation)
init_ ckDiscoverAllUserIdentitiesOperation  =
  sendMsg ckDiscoverAllUserIdentitiesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Called once for each successfully-discovered user identity from the device's address book.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- userIdentityDiscoveredBlock@
userIdentityDiscoveredBlock :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> IO (Ptr ())
userIdentityDiscoveredBlock ckDiscoverAllUserIdentitiesOperation  =
  fmap castPtr $ sendMsg ckDiscoverAllUserIdentitiesOperation (mkSelector "userIdentityDiscoveredBlock") (retPtr retVoid) []

-- | Called once for each successfully-discovered user identity from the device's address book.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlock :: IsCKDiscoverAllUserIdentitiesOperation ckDiscoverAllUserIdentitiesOperation => ckDiscoverAllUserIdentitiesOperation -> Ptr () -> IO ()
setUserIdentityDiscoveredBlock ckDiscoverAllUserIdentitiesOperation  value =
  sendMsg ckDiscoverAllUserIdentitiesOperation (mkSelector "setUserIdentityDiscoveredBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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
discoverAllUserIdentitiesCompletionBlock ckDiscoverAllUserIdentitiesOperation  =
  fmap castPtr $ sendMsg ckDiscoverAllUserIdentitiesOperation (mkSelector "discoverAllUserIdentitiesCompletionBlock") (retPtr retVoid) []

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
setDiscoverAllUserIdentitiesCompletionBlock ckDiscoverAllUserIdentitiesOperation  value =
  sendMsg ckDiscoverAllUserIdentitiesOperation (mkSelector "setDiscoverAllUserIdentitiesCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @userIdentityDiscoveredBlock@
userIdentityDiscoveredBlockSelector :: Selector
userIdentityDiscoveredBlockSelector = mkSelector "userIdentityDiscoveredBlock"

-- | @Selector@ for @setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlockSelector :: Selector
setUserIdentityDiscoveredBlockSelector = mkSelector "setUserIdentityDiscoveredBlock:"

-- | @Selector@ for @discoverAllUserIdentitiesCompletionBlock@
discoverAllUserIdentitiesCompletionBlockSelector :: Selector
discoverAllUserIdentitiesCompletionBlockSelector = mkSelector "discoverAllUserIdentitiesCompletionBlock"

-- | @Selector@ for @setDiscoverAllUserIdentitiesCompletionBlock:@
setDiscoverAllUserIdentitiesCompletionBlockSelector :: Selector
setDiscoverAllUserIdentitiesCompletionBlockSelector = mkSelector "setDiscoverAllUserIdentitiesCompletionBlock:"

