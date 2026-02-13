{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorization@.
module ObjC.AuthenticationServices.ASAuthorization
  ( ASAuthorization
  , IsASAuthorization(..)
  , new
  , init_
  , provider
  , credential
  , credentialSelector
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
new :: IO (Id ASAuthorization)
new  =
  do
    cls' <- getRequiredClass "ASAuthorization"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorization asAuthorization => asAuthorization -> IO (Id ASAuthorization)
init_ asAuthorization =
  sendOwnedMessage asAuthorization initSelector

-- | Provider which was used to generate this authorization response.
--
-- ObjC selector: @- provider@
provider :: IsASAuthorization asAuthorization => asAuthorization -> IO RawId
provider asAuthorization =
  sendMessage asAuthorization providerSelector

-- | The credential that was returned by the authorization provider. Authorization provider type should be used to determine how to introspect the credential.
--
-- ObjC selector: @- credential@
credential :: IsASAuthorization asAuthorization => asAuthorization -> IO RawId
credential asAuthorization =
  sendMessage asAuthorization credentialSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorization)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorization)
initSelector = mkSelector "init"

-- | @Selector@ for @provider@
providerSelector :: Selector '[] RawId
providerSelector = mkSelector "provider"

-- | @Selector@ for @credential@
credentialSelector :: Selector '[] RawId
credentialSelector = mkSelector "credential"

