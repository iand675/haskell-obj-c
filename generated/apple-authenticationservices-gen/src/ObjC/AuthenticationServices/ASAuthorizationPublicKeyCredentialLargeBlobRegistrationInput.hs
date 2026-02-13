{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput
  ( ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput
  , IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput(..)
  , initWithSupportRequirement
  , new
  , init_
  , supportRequirement
  , setSupportRequirement
  , initSelector
  , initWithSupportRequirementSelector
  , newSelector
  , setSupportRequirementSelector
  , supportRequirementSelector

  -- * Enum types
  , ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement(ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement)
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirementRequired
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirementPreferred

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithSupportRequirement:@
initWithSupportRequirement :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
initWithSupportRequirement asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput requirement =
  sendOwnedMessage asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput initWithSupportRequirementSelector requirement

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
init_ asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput =
  sendOwnedMessage asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput initSelector

-- | @- supportRequirement@
supportRequirement :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> IO ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement
supportRequirement asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput supportRequirementSelector

-- | @- setSupportRequirement:@
setSupportRequirement :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement -> IO ()
setSupportRequirement asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput value =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput setSupportRequirementSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSupportRequirement:@
initWithSupportRequirementSelector :: Selector '[ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
initWithSupportRequirementSelector = mkSelector "initWithSupportRequirement:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
initSelector = mkSelector "init"

-- | @Selector@ for @supportRequirement@
supportRequirementSelector :: Selector '[] ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement
supportRequirementSelector = mkSelector "supportRequirement"

-- | @Selector@ for @setSupportRequirement:@
setSupportRequirementSelector :: Selector '[ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement] ()
setSupportRequirementSelector = mkSelector "setSupportRequirement:"

