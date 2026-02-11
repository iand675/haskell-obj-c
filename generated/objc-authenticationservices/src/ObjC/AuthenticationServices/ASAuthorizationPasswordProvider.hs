{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPasswordProvider@.
module ObjC.AuthenticationServices.ASAuthorizationPasswordProvider
  ( ASAuthorizationPasswordProvider
  , IsASAuthorizationPasswordProvider(..)
  , createRequest
  , createRequestSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createRequest@
createRequest :: IsASAuthorizationPasswordProvider asAuthorizationPasswordProvider => asAuthorizationPasswordProvider -> IO (Id ASAuthorizationPasswordRequest)
createRequest asAuthorizationPasswordProvider  =
  sendMsg asAuthorizationPasswordProvider (mkSelector "createRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRequest@
createRequestSelector :: Selector
createRequestSelector = mkSelector "createRequest"

