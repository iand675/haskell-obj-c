{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKFetchShareParticipantsOperation@.
module ObjC.CloudKit.CKFetchShareParticipantsOperation
  ( CKFetchShareParticipantsOperation
  , IsCKFetchShareParticipantsOperation(..)
  , init_
  , initWithUserIdentityLookupInfos
  , userIdentityLookupInfos
  , setUserIdentityLookupInfos
  , shareParticipantFetchedBlock
  , setShareParticipantFetchedBlock
  , perShareParticipantCompletionBlock
  , setPerShareParticipantCompletionBlock
  , fetchShareParticipantsCompletionBlock
  , setFetchShareParticipantsCompletionBlock
  , initSelector
  , initWithUserIdentityLookupInfosSelector
  , userIdentityLookupInfosSelector
  , setUserIdentityLookupInfosSelector
  , shareParticipantFetchedBlockSelector
  , setShareParticipantFetchedBlockSelector
  , perShareParticipantCompletionBlockSelector
  , setPerShareParticipantCompletionBlockSelector
  , fetchShareParticipantsCompletionBlockSelector
  , setFetchShareParticipantsCompletionBlockSelector


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
init_ :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Id CKFetchShareParticipantsOperation)
init_ ckFetchShareParticipantsOperation  =
  sendMsg ckFetchShareParticipantsOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfos :: (IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation, IsNSArray userIdentityLookupInfos) => ckFetchShareParticipantsOperation -> userIdentityLookupInfos -> IO (Id CKFetchShareParticipantsOperation)
initWithUserIdentityLookupInfos ckFetchShareParticipantsOperation  userIdentityLookupInfos =
withObjCPtr userIdentityLookupInfos $ \raw_userIdentityLookupInfos ->
    sendMsg ckFetchShareParticipantsOperation (mkSelector "initWithUserIdentityLookupInfos:") (retPtr retVoid) [argPtr (castPtr raw_userIdentityLookupInfos :: Ptr ())] >>= ownedObject . castPtr

-- | @- userIdentityLookupInfos@
userIdentityLookupInfos :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Id NSArray)
userIdentityLookupInfos ckFetchShareParticipantsOperation  =
  sendMsg ckFetchShareParticipantsOperation (mkSelector "userIdentityLookupInfos") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIdentityLookupInfos:@
setUserIdentityLookupInfos :: (IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation, IsNSArray value) => ckFetchShareParticipantsOperation -> value -> IO ()
setUserIdentityLookupInfos ckFetchShareParticipantsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchShareParticipantsOperation (mkSelector "setUserIdentityLookupInfos:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called once for each share participant created from a submitted user identity lookup info.
--
-- If the replacement callback @perShareParticipantCompletionBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- shareParticipantFetchedBlock@
shareParticipantFetchedBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Ptr ())
shareParticipantFetchedBlock ckFetchShareParticipantsOperation  =
  fmap castPtr $ sendMsg ckFetchShareParticipantsOperation (mkSelector "shareParticipantFetchedBlock") (retPtr retVoid) []

-- | Called once for each share participant created from a submitted user identity lookup info.
--
-- If the replacement callback @perShareParticipantCompletionBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setShareParticipantFetchedBlock:@
setShareParticipantFetchedBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> Ptr () -> IO ()
setShareParticipantFetchedBlock ckFetchShareParticipantsOperation  value =
  sendMsg ckFetchShareParticipantsOperation (mkSelector "setShareParticipantFetchedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called once for each lookup info.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perShareParticipantCompletionBlock@
perShareParticipantCompletionBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Ptr ())
perShareParticipantCompletionBlock ckFetchShareParticipantsOperation  =
  fmap castPtr $ sendMsg ckFetchShareParticipantsOperation (mkSelector "perShareParticipantCompletionBlock") (retPtr retVoid) []

-- | Called once for each lookup info.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerShareParticipantCompletionBlock:@
setPerShareParticipantCompletionBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> Ptr () -> IO ()
setPerShareParticipantCompletionBlock ckFetchShareParticipantsOperation  value =
  sendMsg ckFetchShareParticipantsOperation (mkSelector "setPerShareParticipantCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of lookup infos to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @perShareParticipantCompletionBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchShareParticipantsCompletionBlock@
fetchShareParticipantsCompletionBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Ptr ())
fetchShareParticipantsCompletionBlock ckFetchShareParticipantsOperation  =
  fmap castPtr $ sendMsg ckFetchShareParticipantsOperation (mkSelector "fetchShareParticipantsCompletionBlock") (retPtr retVoid) []

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of lookup infos to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @perShareParticipantCompletionBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchShareParticipantsCompletionBlock:@
setFetchShareParticipantsCompletionBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> Ptr () -> IO ()
setFetchShareParticipantsCompletionBlock ckFetchShareParticipantsOperation  value =
  sendMsg ckFetchShareParticipantsOperation (mkSelector "setFetchShareParticipantsCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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

-- | @Selector@ for @shareParticipantFetchedBlock@
shareParticipantFetchedBlockSelector :: Selector
shareParticipantFetchedBlockSelector = mkSelector "shareParticipantFetchedBlock"

-- | @Selector@ for @setShareParticipantFetchedBlock:@
setShareParticipantFetchedBlockSelector :: Selector
setShareParticipantFetchedBlockSelector = mkSelector "setShareParticipantFetchedBlock:"

-- | @Selector@ for @perShareParticipantCompletionBlock@
perShareParticipantCompletionBlockSelector :: Selector
perShareParticipantCompletionBlockSelector = mkSelector "perShareParticipantCompletionBlock"

-- | @Selector@ for @setPerShareParticipantCompletionBlock:@
setPerShareParticipantCompletionBlockSelector :: Selector
setPerShareParticipantCompletionBlockSelector = mkSelector "setPerShareParticipantCompletionBlock:"

-- | @Selector@ for @fetchShareParticipantsCompletionBlock@
fetchShareParticipantsCompletionBlockSelector :: Selector
fetchShareParticipantsCompletionBlockSelector = mkSelector "fetchShareParticipantsCompletionBlock"

-- | @Selector@ for @setFetchShareParticipantsCompletionBlock:@
setFetchShareParticipantsCompletionBlockSelector :: Selector
setFetchShareParticipantsCompletionBlockSelector = mkSelector "setFetchShareParticipantsCompletionBlock:"

