{-# LANGUAGE DataKinds #-}
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
  , addExecutionBlockSelector
  , blockOperationWithBlockSelector
  , executionBlocksSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ blockOperationWithBlock:@
blockOperationWithBlock :: Ptr () -> IO (Id NSBlockOperation)
blockOperationWithBlock block =
  do
    cls' <- getRequiredClass "NSBlockOperation"
    sendClassMessage cls' blockOperationWithBlockSelector block

-- | @- addExecutionBlock:@
addExecutionBlock :: IsNSBlockOperation nsBlockOperation => nsBlockOperation -> Ptr () -> IO ()
addExecutionBlock nsBlockOperation block =
  sendMessage nsBlockOperation addExecutionBlockSelector block

-- | @- executionBlocks@
executionBlocks :: IsNSBlockOperation nsBlockOperation => nsBlockOperation -> IO (Id NSArray)
executionBlocks nsBlockOperation =
  sendMessage nsBlockOperation executionBlocksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @blockOperationWithBlock:@
blockOperationWithBlockSelector :: Selector '[Ptr ()] (Id NSBlockOperation)
blockOperationWithBlockSelector = mkSelector "blockOperationWithBlock:"

-- | @Selector@ for @addExecutionBlock:@
addExecutionBlockSelector :: Selector '[Ptr ()] ()
addExecutionBlockSelector = mkSelector "addExecutionBlock:"

-- | @Selector@ for @executionBlocks@
executionBlocksSelector :: Selector '[] (Id NSArray)
executionBlocksSelector = mkSelector "executionBlocks"

