{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents options to configure a commit operation on a command queue.
--
-- You pass these options as a parameter when you call ``MTL4CommandQueue/commit:count:options:``.
--
-- - Note Instances of this class are not thread-safe. If your app modifies a shared commit options instance from multiple threads simultaneously, you are responsible for providing external synchronization.
--
-- Generated bindings for @MTL4CommitOptions@.
module ObjC.Metal.MTL4CommitOptions
  ( MTL4CommitOptions
  , IsMTL4CommitOptions(..)
  , addFeedbackHandler
  , addFeedbackHandlerSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Registers a commit feedback handler that Metal calls with feedback data when available.
--
-- - Parameter block: ``MTL4CommitFeedbackHandler`` that Metal invokes.
--
-- ObjC selector: @- addFeedbackHandler:@
addFeedbackHandler :: IsMTL4CommitOptions mtL4CommitOptions => mtL4CommitOptions -> Ptr () -> IO ()
addFeedbackHandler mtL4CommitOptions  block =
  sendMsg mtL4CommitOptions (mkSelector "addFeedbackHandler:") retVoid [argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addFeedbackHandler:@
addFeedbackHandlerSelector :: Selector
addFeedbackHandlerSelector = mkSelector "addFeedbackHandler:"

