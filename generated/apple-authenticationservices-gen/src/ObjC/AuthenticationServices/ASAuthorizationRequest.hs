{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationRequest@.
module ObjC.AuthenticationServices.ASAuthorizationRequest
  ( ASAuthorizationRequest
  , IsASAuthorizationRequest(..)
  , new
  , init_
  , provider
  , initSelector
  , newSelector
  , providerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationRequest asAuthorizationRequest => asAuthorizationRequest -> IO (Id ASAuthorizationRequest)
init_ asAuthorizationRequest =
  sendOwnedMessage asAuthorizationRequest initSelector

-- | The provider object that is being used to service this request
--
-- ObjC selector: @- provider@
provider :: IsASAuthorizationRequest asAuthorizationRequest => asAuthorizationRequest -> IO RawId
provider asAuthorizationRequest =
  sendMessage asAuthorizationRequest providerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @provider@
providerSelector :: Selector '[] RawId
providerSelector = mkSelector "provider"

