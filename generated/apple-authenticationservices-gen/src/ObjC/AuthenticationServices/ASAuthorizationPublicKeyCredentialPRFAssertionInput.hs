{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFAssertionInput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFAssertionInput
  ( ASAuthorizationPublicKeyCredentialPRFAssertionInput
  , IsASAuthorizationPublicKeyCredentialPRFAssertionInput(..)
  , initWithInputValues_perCredentialInputValues
  , inputValues
  , perCredentialInputValues
  , initWithInputValues_perCredentialInputValuesSelector
  , inputValuesSelector
  , perCredentialInputValuesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithInputValues:perCredentialInputValues:@
initWithInputValues_perCredentialInputValues :: (IsASAuthorizationPublicKeyCredentialPRFAssertionInput asAuthorizationPublicKeyCredentialPRFAssertionInput, IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues inputValues, IsNSDictionary perCredentialInputValues) => asAuthorizationPublicKeyCredentialPRFAssertionInput -> inputValues -> perCredentialInputValues -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput)
initWithInputValues_perCredentialInputValues asAuthorizationPublicKeyCredentialPRFAssertionInput inputValues perCredentialInputValues =
  sendOwnedMessage asAuthorizationPublicKeyCredentialPRFAssertionInput initWithInputValues_perCredentialInputValuesSelector (toASAuthorizationPublicKeyCredentialPRFAssertionInputValues inputValues) (toNSDictionary perCredentialInputValues)

-- | @- inputValues@
inputValues :: IsASAuthorizationPublicKeyCredentialPRFAssertionInput asAuthorizationPublicKeyCredentialPRFAssertionInput => asAuthorizationPublicKeyCredentialPRFAssertionInput -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
inputValues asAuthorizationPublicKeyCredentialPRFAssertionInput =
  sendMessage asAuthorizationPublicKeyCredentialPRFAssertionInput inputValuesSelector

-- | @- perCredentialInputValues@
perCredentialInputValues :: IsASAuthorizationPublicKeyCredentialPRFAssertionInput asAuthorizationPublicKeyCredentialPRFAssertionInput => asAuthorizationPublicKeyCredentialPRFAssertionInput -> IO (Id NSDictionary)
perCredentialInputValues asAuthorizationPublicKeyCredentialPRFAssertionInput =
  sendMessage asAuthorizationPublicKeyCredentialPRFAssertionInput perCredentialInputValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInputValues:perCredentialInputValues:@
initWithInputValues_perCredentialInputValuesSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues, Id NSDictionary] (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput)
initWithInputValues_perCredentialInputValuesSelector = mkSelector "initWithInputValues:perCredentialInputValues:"

-- | @Selector@ for @inputValues@
inputValuesSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
inputValuesSelector = mkSelector "inputValues"

-- | @Selector@ for @perCredentialInputValues@
perCredentialInputValuesSelector :: Selector '[] (Id NSDictionary)
perCredentialInputValuesSelector = mkSelector "perCredentialInputValues"

