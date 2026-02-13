{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFAssertionInputValues@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFAssertionInputValues
  ( ASAuthorizationPublicKeyCredentialPRFAssertionInputValues
  , IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues(..)
  , initWithSaltInput1_saltInput2
  , saltInput1
  , saltInput2
  , initWithSaltInput1_saltInput2Selector
  , saltInput1Selector
  , saltInput2Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSaltInput1:saltInput2:@
initWithSaltInput1_saltInput2 :: (IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues asAuthorizationPublicKeyCredentialPRFAssertionInputValues, IsNSData saltInput1, IsNSData saltInput2) => asAuthorizationPublicKeyCredentialPRFAssertionInputValues -> saltInput1 -> saltInput2 -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
initWithSaltInput1_saltInput2 asAuthorizationPublicKeyCredentialPRFAssertionInputValues saltInput1 saltInput2 =
  sendOwnedMessage asAuthorizationPublicKeyCredentialPRFAssertionInputValues initWithSaltInput1_saltInput2Selector (toNSData saltInput1) (toNSData saltInput2)

-- | @- saltInput1@
saltInput1 :: IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues asAuthorizationPublicKeyCredentialPRFAssertionInputValues => asAuthorizationPublicKeyCredentialPRFAssertionInputValues -> IO (Id NSData)
saltInput1 asAuthorizationPublicKeyCredentialPRFAssertionInputValues =
  sendMessage asAuthorizationPublicKeyCredentialPRFAssertionInputValues saltInput1Selector

-- | @- saltInput2@
saltInput2 :: IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues asAuthorizationPublicKeyCredentialPRFAssertionInputValues => asAuthorizationPublicKeyCredentialPRFAssertionInputValues -> IO (Id NSData)
saltInput2 asAuthorizationPublicKeyCredentialPRFAssertionInputValues =
  sendMessage asAuthorizationPublicKeyCredentialPRFAssertionInputValues saltInput2Selector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSaltInput1:saltInput2:@
initWithSaltInput1_saltInput2Selector :: Selector '[Id NSData, Id NSData] (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
initWithSaltInput1_saltInput2Selector = mkSelector "initWithSaltInput1:saltInput2:"

-- | @Selector@ for @saltInput1@
saltInput1Selector :: Selector '[] (Id NSData)
saltInput1Selector = mkSelector "saltInput1"

-- | @Selector@ for @saltInput2@
saltInput2Selector :: Selector '[] (Id NSData)
saltInput2Selector = mkSelector "saltInput2"

