{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialParameters@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialParameters
  ( ASAuthorizationPublicKeyCredentialParameters
  , IsASAuthorizationPublicKeyCredentialParameters(..)
  , initWithAlgorithm
  , new
  , init_
  , algorithm
  , algorithmSelector
  , initSelector
  , initWithAlgorithmSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAlgorithm:@
initWithAlgorithm :: IsASAuthorizationPublicKeyCredentialParameters asAuthorizationPublicKeyCredentialParameters => asAuthorizationPublicKeyCredentialParameters -> CLong -> IO (Id ASAuthorizationPublicKeyCredentialParameters)
initWithAlgorithm asAuthorizationPublicKeyCredentialParameters algorithm =
  sendOwnedMessage asAuthorizationPublicKeyCredentialParameters initWithAlgorithmSelector algorithm

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialParameters)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialParameters"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialParameters asAuthorizationPublicKeyCredentialParameters => asAuthorizationPublicKeyCredentialParameters -> IO (Id ASAuthorizationPublicKeyCredentialParameters)
init_ asAuthorizationPublicKeyCredentialParameters =
  sendOwnedMessage asAuthorizationPublicKeyCredentialParameters initSelector

-- | A COSE algorithm indentifier.
--
-- ObjC selector: @- algorithm@
algorithm :: IsASAuthorizationPublicKeyCredentialParameters asAuthorizationPublicKeyCredentialParameters => asAuthorizationPublicKeyCredentialParameters -> IO CLong
algorithm asAuthorizationPublicKeyCredentialParameters =
  sendMessage asAuthorizationPublicKeyCredentialParameters algorithmSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAlgorithm:@
initWithAlgorithmSelector :: Selector '[CLong] (Id ASAuthorizationPublicKeyCredentialParameters)
initWithAlgorithmSelector = mkSelector "initWithAlgorithm:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @algorithm@
algorithmSelector :: Selector '[] CLong
algorithmSelector = mkSelector "algorithm"

