{-# LANGUAGE PatternSynonyms #-}
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
  , initWithSupportRequirementSelector
  , newSelector
  , initSelector
  , supportRequirementSelector
  , setSupportRequirementSelector

  -- * Enum types
  , ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement(ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement)
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirementRequired
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirementPreferred

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
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithSupportRequirement:@
initWithSupportRequirement :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
initWithSupportRequirement asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput  requirement =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput (mkSelector "initWithSupportRequirement:") (retPtr retVoid) [argCLong (coerce requirement)] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
init_ asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput  =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- supportRequirement@
supportRequirement :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> IO ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement
supportRequirement asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput  =
  fmap (coerce :: CLong -> ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement) $ sendMsg asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput (mkSelector "supportRequirement") retCLong []

-- | @- setSupportRequirement:@
setSupportRequirement :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput -> ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement -> IO ()
setSupportRequirement asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput  value =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobRegistrationInput (mkSelector "setSupportRequirement:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSupportRequirement:@
initWithSupportRequirementSelector :: Selector
initWithSupportRequirementSelector = mkSelector "initWithSupportRequirement:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @supportRequirement@
supportRequirementSelector :: Selector
supportRequirementSelector = mkSelector "supportRequirement"

-- | @Selector@ for @setSupportRequirement:@
setSupportRequirementSelector :: Selector
setSupportRequirementSelector = mkSelector "setSupportRequirement:"

