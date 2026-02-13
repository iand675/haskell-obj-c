{-# LANGUAGE DataKinds #-}
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
  , fetchShareParticipantsCompletionBlockSelector
  , initSelector
  , initWithUserIdentityLookupInfosSelector
  , perShareParticipantCompletionBlockSelector
  , setFetchShareParticipantsCompletionBlockSelector
  , setPerShareParticipantCompletionBlockSelector
  , setShareParticipantFetchedBlockSelector
  , setUserIdentityLookupInfosSelector
  , shareParticipantFetchedBlockSelector
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
init_ :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Id CKFetchShareParticipantsOperation)
init_ ckFetchShareParticipantsOperation =
  sendOwnedMessage ckFetchShareParticipantsOperation initSelector

-- | @- initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfos :: (IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation, IsNSArray userIdentityLookupInfos) => ckFetchShareParticipantsOperation -> userIdentityLookupInfos -> IO (Id CKFetchShareParticipantsOperation)
initWithUserIdentityLookupInfos ckFetchShareParticipantsOperation userIdentityLookupInfos =
  sendOwnedMessage ckFetchShareParticipantsOperation initWithUserIdentityLookupInfosSelector (toNSArray userIdentityLookupInfos)

-- | @- userIdentityLookupInfos@
userIdentityLookupInfos :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Id NSArray)
userIdentityLookupInfos ckFetchShareParticipantsOperation =
  sendMessage ckFetchShareParticipantsOperation userIdentityLookupInfosSelector

-- | @- setUserIdentityLookupInfos:@
setUserIdentityLookupInfos :: (IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation, IsNSArray value) => ckFetchShareParticipantsOperation -> value -> IO ()
setUserIdentityLookupInfos ckFetchShareParticipantsOperation value =
  sendMessage ckFetchShareParticipantsOperation setUserIdentityLookupInfosSelector (toNSArray value)

-- | Called once for each share participant created from a submitted user identity lookup info.
--
-- If the replacement callback @perShareParticipantCompletionBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- shareParticipantFetchedBlock@
shareParticipantFetchedBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Ptr ())
shareParticipantFetchedBlock ckFetchShareParticipantsOperation =
  sendMessage ckFetchShareParticipantsOperation shareParticipantFetchedBlockSelector

-- | Called once for each share participant created from a submitted user identity lookup info.
--
-- If the replacement callback @perShareParticipantCompletionBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setShareParticipantFetchedBlock:@
setShareParticipantFetchedBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> Ptr () -> IO ()
setShareParticipantFetchedBlock ckFetchShareParticipantsOperation value =
  sendMessage ckFetchShareParticipantsOperation setShareParticipantFetchedBlockSelector value

-- | Called once for each lookup info.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perShareParticipantCompletionBlock@
perShareParticipantCompletionBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> IO (Ptr ())
perShareParticipantCompletionBlock ckFetchShareParticipantsOperation =
  sendMessage ckFetchShareParticipantsOperation perShareParticipantCompletionBlockSelector

-- | Called once for each lookup info.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerShareParticipantCompletionBlock:@
setPerShareParticipantCompletionBlock :: IsCKFetchShareParticipantsOperation ckFetchShareParticipantsOperation => ckFetchShareParticipantsOperation -> Ptr () -> IO ()
setPerShareParticipantCompletionBlock ckFetchShareParticipantsOperation value =
  sendMessage ckFetchShareParticipantsOperation setPerShareParticipantCompletionBlockSelector value

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
fetchShareParticipantsCompletionBlock ckFetchShareParticipantsOperation =
  sendMessage ckFetchShareParticipantsOperation fetchShareParticipantsCompletionBlockSelector

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
setFetchShareParticipantsCompletionBlock ckFetchShareParticipantsOperation value =
  sendMessage ckFetchShareParticipantsOperation setFetchShareParticipantsCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchShareParticipantsOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUserIdentityLookupInfos:@
initWithUserIdentityLookupInfosSelector :: Selector '[Id NSArray] (Id CKFetchShareParticipantsOperation)
initWithUserIdentityLookupInfosSelector = mkSelector "initWithUserIdentityLookupInfos:"

-- | @Selector@ for @userIdentityLookupInfos@
userIdentityLookupInfosSelector :: Selector '[] (Id NSArray)
userIdentityLookupInfosSelector = mkSelector "userIdentityLookupInfos"

-- | @Selector@ for @setUserIdentityLookupInfos:@
setUserIdentityLookupInfosSelector :: Selector '[Id NSArray] ()
setUserIdentityLookupInfosSelector = mkSelector "setUserIdentityLookupInfos:"

-- | @Selector@ for @shareParticipantFetchedBlock@
shareParticipantFetchedBlockSelector :: Selector '[] (Ptr ())
shareParticipantFetchedBlockSelector = mkSelector "shareParticipantFetchedBlock"

-- | @Selector@ for @setShareParticipantFetchedBlock:@
setShareParticipantFetchedBlockSelector :: Selector '[Ptr ()] ()
setShareParticipantFetchedBlockSelector = mkSelector "setShareParticipantFetchedBlock:"

-- | @Selector@ for @perShareParticipantCompletionBlock@
perShareParticipantCompletionBlockSelector :: Selector '[] (Ptr ())
perShareParticipantCompletionBlockSelector = mkSelector "perShareParticipantCompletionBlock"

-- | @Selector@ for @setPerShareParticipantCompletionBlock:@
setPerShareParticipantCompletionBlockSelector :: Selector '[Ptr ()] ()
setPerShareParticipantCompletionBlockSelector = mkSelector "setPerShareParticipantCompletionBlock:"

-- | @Selector@ for @fetchShareParticipantsCompletionBlock@
fetchShareParticipantsCompletionBlockSelector :: Selector '[] (Ptr ())
fetchShareParticipantsCompletionBlockSelector = mkSelector "fetchShareParticipantsCompletionBlock"

-- | @Selector@ for @setFetchShareParticipantsCompletionBlock:@
setFetchShareParticipantsCompletionBlockSelector :: Selector '[Ptr ()] ()
setFetchShareParticipantsCompletionBlockSelector = mkSelector "setFetchShareParticipantsCompletionBlock:"

