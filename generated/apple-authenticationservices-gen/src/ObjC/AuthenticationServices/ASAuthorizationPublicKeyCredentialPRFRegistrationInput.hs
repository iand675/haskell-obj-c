{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFRegistrationInput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFRegistrationInput
  ( ASAuthorizationPublicKeyCredentialPRFRegistrationInput
  , IsASAuthorizationPublicKeyCredentialPRFRegistrationInput(..)
  , checkForSupport
  , initWithInputValues
  , shouldCheckForSupport
  , inputValues
  , checkForSupportSelector
  , initWithInputValuesSelector
  , inputValuesSelector
  , shouldCheckForSupportSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ checkForSupport@
checkForSupport :: IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
checkForSupport  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialPRFRegistrationInput"
    sendClassMessage cls' checkForSupportSelector

-- | @- initWithInputValues:@
initWithInputValues :: (IsASAuthorizationPublicKeyCredentialPRFRegistrationInput asAuthorizationPublicKeyCredentialPRFRegistrationInput, IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues inputValues) => asAuthorizationPublicKeyCredentialPRFRegistrationInput -> inputValues -> IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
initWithInputValues asAuthorizationPublicKeyCredentialPRFRegistrationInput inputValues =
  sendOwnedMessage asAuthorizationPublicKeyCredentialPRFRegistrationInput initWithInputValuesSelector (toASAuthorizationPublicKeyCredentialPRFAssertionInputValues inputValues)

-- | @- shouldCheckForSupport@
shouldCheckForSupport :: IsASAuthorizationPublicKeyCredentialPRFRegistrationInput asAuthorizationPublicKeyCredentialPRFRegistrationInput => asAuthorizationPublicKeyCredentialPRFRegistrationInput -> IO Bool
shouldCheckForSupport asAuthorizationPublicKeyCredentialPRFRegistrationInput =
  sendMessage asAuthorizationPublicKeyCredentialPRFRegistrationInput shouldCheckForSupportSelector

-- | @- inputValues@
inputValues :: IsASAuthorizationPublicKeyCredentialPRFRegistrationInput asAuthorizationPublicKeyCredentialPRFRegistrationInput => asAuthorizationPublicKeyCredentialPRFRegistrationInput -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
inputValues asAuthorizationPublicKeyCredentialPRFRegistrationInput =
  sendMessage asAuthorizationPublicKeyCredentialPRFRegistrationInput inputValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkForSupport@
checkForSupportSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
checkForSupportSelector = mkSelector "checkForSupport"

-- | @Selector@ for @initWithInputValues:@
initWithInputValuesSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues] (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
initWithInputValuesSelector = mkSelector "initWithInputValues:"

-- | @Selector@ for @shouldCheckForSupport@
shouldCheckForSupportSelector :: Selector '[] Bool
shouldCheckForSupportSelector = mkSelector "shouldCheckForSupport"

-- | @Selector@ for @inputValues@
inputValuesSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
inputValuesSelector = mkSelector "inputValues"

