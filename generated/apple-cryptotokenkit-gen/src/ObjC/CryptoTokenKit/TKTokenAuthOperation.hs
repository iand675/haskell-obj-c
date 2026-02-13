{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Context of a pending authentication operation.
--
-- Generated bindings for @TKTokenAuthOperation@.
module ObjC.CryptoTokenKit.TKTokenAuthOperation
  ( TKTokenAuthOperation
  , IsTKTokenAuthOperation(..)
  , finishWithError
  , finishWithErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Handler triggered by the system in order to let the token finalize the authentication operation.
--
-- @error@ — Error details (see TKError.h).
--
-- Returns: Finalization status.
--
-- ObjC selector: @- finishWithError:@
finishWithError :: (IsTKTokenAuthOperation tkTokenAuthOperation, IsNSError error_) => tkTokenAuthOperation -> error_ -> IO Bool
finishWithError tkTokenAuthOperation error_ =
  sendMessage tkTokenAuthOperation finishWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finishWithError:@
finishWithErrorSelector :: Selector '[Id NSError] Bool
finishWithErrorSelector = mkSelector "finishWithError:"

