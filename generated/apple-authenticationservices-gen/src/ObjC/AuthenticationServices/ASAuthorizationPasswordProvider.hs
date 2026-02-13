{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createRequest@
createRequest :: IsASAuthorizationPasswordProvider asAuthorizationPasswordProvider => asAuthorizationPasswordProvider -> IO (Id ASAuthorizationPasswordRequest)
createRequest asAuthorizationPasswordProvider =
  sendMessage asAuthorizationPasswordProvider createRequestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRequest@
createRequestSelector :: Selector '[] (Id ASAuthorizationPasswordRequest)
createRequestSelector = mkSelector "createRequest"

