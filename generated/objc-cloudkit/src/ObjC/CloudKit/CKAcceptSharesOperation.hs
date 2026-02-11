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
  , initSelector
  , initWithShareMetadatasSelector
  , shareMetadatasSelector
  , setShareMetadatasSelector
  , perShareCompletionBlockSelector
  , setPerShareCompletionBlockSelector
  , acceptSharesCompletionBlockSelector
  , setAcceptSharesCompletionBlockSelector


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
init_ :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Id CKAcceptSharesOperation)
init_ ckAcceptSharesOperation  =
  sendMsg ckAcceptSharesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithShareMetadatas:@
initWithShareMetadatas :: (IsCKAcceptSharesOperation ckAcceptSharesOperation, IsNSArray shareMetadatas) => ckAcceptSharesOperation -> shareMetadatas -> IO (Id CKAcceptSharesOperation)
initWithShareMetadatas ckAcceptSharesOperation  shareMetadatas =
withObjCPtr shareMetadatas $ \raw_shareMetadatas ->
    sendMsg ckAcceptSharesOperation (mkSelector "initWithShareMetadatas:") (retPtr retVoid) [argPtr (castPtr raw_shareMetadatas :: Ptr ())] >>= ownedObject . castPtr

-- | @- shareMetadatas@
shareMetadatas :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Id NSArray)
shareMetadatas ckAcceptSharesOperation  =
  sendMsg ckAcceptSharesOperation (mkSelector "shareMetadatas") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShareMetadatas:@
setShareMetadatas :: (IsCKAcceptSharesOperation ckAcceptSharesOperation, IsNSArray value) => ckAcceptSharesOperation -> value -> IO ()
setShareMetadatas ckAcceptSharesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckAcceptSharesOperation (mkSelector "setShareMetadatas:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called once for each share metadata that the server processed
--
-- If error is nil then the share was successfully accepted.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perShareCompletionBlock@
perShareCompletionBlock :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> IO (Ptr ())
perShareCompletionBlock ckAcceptSharesOperation  =
  fmap castPtr $ sendMsg ckAcceptSharesOperation (mkSelector "perShareCompletionBlock") (retPtr retVoid) []

-- | Called once for each share metadata that the server processed
--
-- If error is nil then the share was successfully accepted.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerShareCompletionBlock:@
setPerShareCompletionBlock :: IsCKAcceptSharesOperation ckAcceptSharesOperation => ckAcceptSharesOperation -> Ptr () -> IO ()
setPerShareCompletionBlock ckAcceptSharesOperation  value =
  sendMsg ckAcceptSharesOperation (mkSelector "setPerShareCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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
acceptSharesCompletionBlock ckAcceptSharesOperation  =
  fmap castPtr $ sendMsg ckAcceptSharesOperation (mkSelector "acceptSharesCompletionBlock") (retPtr retVoid) []

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
setAcceptSharesCompletionBlock ckAcceptSharesOperation  value =
  sendMsg ckAcceptSharesOperation (mkSelector "setAcceptSharesCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithShareMetadatas:@
initWithShareMetadatasSelector :: Selector
initWithShareMetadatasSelector = mkSelector "initWithShareMetadatas:"

-- | @Selector@ for @shareMetadatas@
shareMetadatasSelector :: Selector
shareMetadatasSelector = mkSelector "shareMetadatas"

-- | @Selector@ for @setShareMetadatas:@
setShareMetadatasSelector :: Selector
setShareMetadatasSelector = mkSelector "setShareMetadatas:"

-- | @Selector@ for @perShareCompletionBlock@
perShareCompletionBlockSelector :: Selector
perShareCompletionBlockSelector = mkSelector "perShareCompletionBlock"

-- | @Selector@ for @setPerShareCompletionBlock:@
setPerShareCompletionBlockSelector :: Selector
setPerShareCompletionBlockSelector = mkSelector "setPerShareCompletionBlock:"

-- | @Selector@ for @acceptSharesCompletionBlock@
acceptSharesCompletionBlockSelector :: Selector
acceptSharesCompletionBlockSelector = mkSelector "acceptSharesCompletionBlock"

-- | @Selector@ for @setAcceptSharesCompletionBlock:@
setAcceptSharesCompletionBlockSelector :: Selector
setAcceptSharesCompletionBlockSelector = mkSelector "setAcceptSharesCompletionBlock:"

