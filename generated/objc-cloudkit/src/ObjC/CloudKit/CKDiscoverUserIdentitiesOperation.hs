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
  , initSelector
  , initWithUserIdentityLookupInfosSelector
  , userIdentityLookupInfosSelector
  , setUserIdentityLookupInfosSelector
  , userIdentityDiscoveredBlockSelector
  , setUserIdentityDiscoveredBlockSelector
  , discoverUserIdentitiesCompletionBlockSelector
  , setDiscoverUserIdentitiesCompletionBlockSelector


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
init_ :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Id CKDiscoverUserIdentitiesOperation)
init_ ckDiscoverUserIdentitiesOperation  =
  sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfos :: (IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation, IsNSArray userIdentityLookupInfos) => ckDiscoverUserIdentitiesOperation -> userIdentityLookupInfos -> IO (Id CKDiscoverUserIdentitiesOperation)
initWithUserIdentityLookupInfos ckDiscoverUserIdentitiesOperation  userIdentityLookupInfos =
withObjCPtr userIdentityLookupInfos $ \raw_userIdentityLookupInfos ->
    sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "initWithUserIdentityLookupInfos:") (retPtr retVoid) [argPtr (castPtr raw_userIdentityLookupInfos :: Ptr ())] >>= ownedObject . castPtr

-- | @- userIdentityLookupInfos@
userIdentityLookupInfos :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Id NSArray)
userIdentityLookupInfos ckDiscoverUserIdentitiesOperation  =
  sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "userIdentityLookupInfos") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIdentityLookupInfos:@
setUserIdentityLookupInfos :: (IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation, IsNSArray value) => ckDiscoverUserIdentitiesOperation -> value -> IO ()
setUserIdentityLookupInfos ckDiscoverUserIdentitiesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "setUserIdentityLookupInfos:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called once for each user identity lookup info that was successfully discovered on the server
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- userIdentityDiscoveredBlock@
userIdentityDiscoveredBlock :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> IO (Ptr ())
userIdentityDiscoveredBlock ckDiscoverUserIdentitiesOperation  =
  fmap castPtr $ sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "userIdentityDiscoveredBlock") (retPtr retVoid) []

-- | Called once for each user identity lookup info that was successfully discovered on the server
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlock :: IsCKDiscoverUserIdentitiesOperation ckDiscoverUserIdentitiesOperation => ckDiscoverUserIdentitiesOperation -> Ptr () -> IO ()
setUserIdentityDiscoveredBlock ckDiscoverUserIdentitiesOperation  value =
  sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "setUserIdentityDiscoveredBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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
discoverUserIdentitiesCompletionBlock ckDiscoverUserIdentitiesOperation  =
  fmap castPtr $ sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "discoverUserIdentitiesCompletionBlock") (retPtr retVoid) []

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
setDiscoverUserIdentitiesCompletionBlock ckDiscoverUserIdentitiesOperation  value =
  sendMsg ckDiscoverUserIdentitiesOperation (mkSelector "setDiscoverUserIdentitiesCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfosSelector :: Selector
initWithUserIdentityLookupInfosSelector = mkSelector "initWithUserIdentityLookupInfos:"

-- | @Selector@ for @userIdentityLookupInfos@
userIdentityLookupInfosSelector :: Selector
userIdentityLookupInfosSelector = mkSelector "userIdentityLookupInfos"

-- | @Selector@ for @setUserIdentityLookupInfos:@
setUserIdentityLookupInfosSelector :: Selector
setUserIdentityLookupInfosSelector = mkSelector "setUserIdentityLookupInfos:"

-- | @Selector@ for @userIdentityDiscoveredBlock@
userIdentityDiscoveredBlockSelector :: Selector
userIdentityDiscoveredBlockSelector = mkSelector "userIdentityDiscoveredBlock"

-- | @Selector@ for @setUserIdentityDiscoveredBlock:@
setUserIdentityDiscoveredBlockSelector :: Selector
setUserIdentityDiscoveredBlockSelector = mkSelector "setUserIdentityDiscoveredBlock:"

-- | @Selector@ for @discoverUserIdentitiesCompletionBlock@
discoverUserIdentitiesCompletionBlockSelector :: Selector
discoverUserIdentitiesCompletionBlockSelector = mkSelector "discoverUserIdentitiesCompletionBlock"

-- | @Selector@ for @setDiscoverUserIdentitiesCompletionBlock:@
setDiscoverUserIdentitiesCompletionBlockSelector :: Selector
setDiscoverUserIdentitiesCompletionBlockSelector = mkSelector "setDiscoverUserIdentitiesCompletionBlock:"

