{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a MessageFilter extension request's context.
--
-- Generated bindings for @ILMessageFilterExtensionContext@.
module ObjC.IdentityLookup.ILMessageFilterExtensionContext
  ( ILMessageFilterExtensionContext
  , IsILMessageFilterExtensionContext(..)
  , deferQueryRequestToNetworkWithCompletion
  , deferQueryRequestToNetworkWithCompletionSelector


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

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Defer the current query request to the app extension's associated network service and receive a network response asynchronously.
--
-- This causes the system to perform an HTTPS network request to a URL specified in the app extension's Info.plist, and the response to that HTTPS request (or an error) is returned asynchronously. See documentation for details regarding how this HTTPS request is formatted, restrictions on the URL, etc.
--
-- @completion@ — Completion block containing either the network response to the HTTPS request or an error.
--
-- ObjC selector: @- deferQueryRequestToNetworkWithCompletion:@
deferQueryRequestToNetworkWithCompletion :: IsILMessageFilterExtensionContext ilMessageFilterExtensionContext => ilMessageFilterExtensionContext -> Ptr () -> IO ()
deferQueryRequestToNetworkWithCompletion ilMessageFilterExtensionContext  completion =
  sendMsg ilMessageFilterExtensionContext (mkSelector "deferQueryRequestToNetworkWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deferQueryRequestToNetworkWithCompletion:@
deferQueryRequestToNetworkWithCompletionSelector :: Selector
deferQueryRequestToNetworkWithCompletionSelector = mkSelector "deferQueryRequestToNetworkWithCompletion:"

