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
finishWithError tkTokenAuthOperation  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenAuthOperation (mkSelector "finishWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finishWithError:@
finishWithErrorSelector :: Selector
finishWithErrorSelector = mkSelector "finishWithError:"

