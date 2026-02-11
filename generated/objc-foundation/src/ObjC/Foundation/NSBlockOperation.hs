{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBlockOperation@.
module ObjC.Foundation.NSBlockOperation
  ( NSBlockOperation
  , IsNSBlockOperation(..)
  , blockOperationWithBlock
  , addExecutionBlock
  , executionBlocks
  , blockOperationWithBlockSelector
  , addExecutionBlockSelector
  , executionBlocksSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ blockOperationWithBlock:@
blockOperationWithBlock :: Ptr () -> IO (Id NSBlockOperation)
blockOperationWithBlock block =
  do
    cls' <- getRequiredClass "NSBlockOperation"
    sendClassMsg cls' (mkSelector "blockOperationWithBlock:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | @- addExecutionBlock:@
addExecutionBlock :: IsNSBlockOperation nsBlockOperation => nsBlockOperation -> Ptr () -> IO ()
addExecutionBlock nsBlockOperation  block =
  sendMsg nsBlockOperation (mkSelector "addExecutionBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- executionBlocks@
executionBlocks :: IsNSBlockOperation nsBlockOperation => nsBlockOperation -> IO (Id NSArray)
executionBlocks nsBlockOperation  =
  sendMsg nsBlockOperation (mkSelector "executionBlocks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @blockOperationWithBlock:@
blockOperationWithBlockSelector :: Selector
blockOperationWithBlockSelector = mkSelector "blockOperationWithBlock:"

-- | @Selector@ for @addExecutionBlock:@
addExecutionBlockSelector :: Selector
addExecutionBlockSelector = mkSelector "addExecutionBlock:"

-- | @Selector@ for @executionBlocks@
executionBlocksSelector :: Selector
executionBlocksSelector = mkSelector "executionBlocks"

