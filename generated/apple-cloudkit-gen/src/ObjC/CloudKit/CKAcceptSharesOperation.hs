{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKAcceptSharesOperation@.
module ObjC.CloudKit.CKAcceptSharesOperation
  ( CKAcceptSharesOperation
  , IsCKAcceptSharesOperation(..)
  , init_
  , initWithShareMetadatas
  , shareMetadatas
  , setShareMetadatas
  , perShareCompletionBlock
  , setPerShareCompletionBlock
  , acceptSharesCompletionBlock
  , setAcceptSharesCompletionBlock
  , acceptSharesCompletionBlockSelector
  , initSelector
  , initWithShareMetadatasSelector
  , perShareCompletionBlockSelector
  , setAcceptSharesCompletionBlockSelector
  , setPerShareCompletionBlockSelector
  , setShareMetadatasSelector
  , shareMetadatasSelector


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
init_ :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Id CKAcceptSharesOperation)
init_ ckAcceptSharesOperation =
  sendOwnedMessage ckAcceptSharesOperation initSelector

-- | @- initWithShareMetadatas:@
initWithShareMetadatas :: (IsCKAcceptSharesOperation ckAcceptSharesOperation, IsNSArray shareMetadatas) => ckAcceptSharesOperation -> shareMetadatas -> IO (Id CKAcceptSharesOperation)
initWithShareMetadatas ckAcceptSharesOperation shareMetadatas =
  sendOwnedMessage ckAcceptSharesOperation initWithShareMetadatasSelector (toNSArray shareMetadatas)

-- | @- shareMetadatas@
shareMetadatas :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Id NSArray)
shareMetadatas ckAcceptSharesOperation =
  sendMessage ckAcceptSharesOperation shareMetadatasSelector

-- | @- setShareMetadatas:@
setShareMetadatas :: (IsCKAcceptSharesOperation ckAcceptSharesOperation, IsNSArray value) => ckAcceptSharesOperation -> value -> IO ()
setShareMetadatas ckAcceptSharesOperation value =
  sendMessage ckAcceptSharesOperation setShareMetadatasSelector (toNSArray value)

-- | Called once for each share metadata that the server processed
--
-- If error is nil then the share was successfully accepted.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perShareCompletionBlock@
perShareCompletionBlock :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Ptr ())
perShareCompletionBlock ckAcceptSharesOperation =
  sendMessage ckAcceptSharesOperation perShareCompletionBlockSelector

-- | Called once for each share metadata that the server processed
--
-- If error is nil then the share was successfully accepted.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerShareCompletionBlock:@
setPerShareCompletionBlock :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> Ptr () -> IO ()
setPerShareCompletionBlock ckAcceptSharesOperation value =
  sendMessage ckAcceptSharesOperation setPerShareCompletionBlockSelector value

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of shareURLs to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @perShareCompletionBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- acceptSharesCompletionBlock@
acceptSharesCompletionBlock :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Ptr ())
acceptSharesCompletionBlock ckAcceptSharesOperation =
  sendMessage ckAcceptSharesOperation acceptSharesCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of shareURLs to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @perShareCompletionBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setAcceptSharesCompletionBlock:@
setAcceptSharesCompletionBlock :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> Ptr () -> IO ()
setAcceptSharesCompletionBlock ckAcceptSharesOperation value =
  sendMessage ckAcceptSharesOperation setAcceptSharesCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKAcceptSharesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithShareMetadatas:@
initWithShareMetadatasSelector :: Selector '[Id NSArray] (Id CKAcceptSharesOperation)
initWithShareMetadatasSelector = mkSelector "initWithShareMetadatas:"

-- | @Selector@ for @shareMetadatas@
shareMetadatasSelector :: Selector '[] (Id NSArray)
shareMetadatasSelector = mkSelector "shareMetadatas"

-- | @Selector@ for @setShareMetadatas:@
setShareMetadatasSelector :: Selector '[Id NSArray] ()
setShareMetadatasSelector = mkSelector "setShareMetadatas:"

-- | @Selector@ for @perShareCompletionBlock@
perShareCompletionBlockSelector :: Selector '[] (Ptr ())
perShareCompletionBlockSelector = mkSelector "perShareCompletionBlock"

-- | @Selector@ for @setPerShareCompletionBlock:@
setPerShareCompletionBlockSelector :: Selector '[Ptr ()] ()
setPerShareCompletionBlockSelector = mkSelector "setPerShareCompletionBlock:"

-- | @Selector@ for @acceptSharesCompletionBlock@
acceptSharesCompletionBlockSelector :: Selector '[] (Ptr ())
acceptSharesCompletionBlockSelector = mkSelector "acceptSharesCompletionBlock"

-- | @Selector@ for @setAcceptSharesCompletionBlock:@
setAcceptSharesCompletionBlockSelector :: Selector '[Ptr ()] ()
setAcceptSharesCompletionBlockSelector = mkSelector "setAcceptSharesCompletionBlock:"

