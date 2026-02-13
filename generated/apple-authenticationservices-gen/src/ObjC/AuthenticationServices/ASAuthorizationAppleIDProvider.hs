{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationAppleIDProvider@.
module ObjC.AuthenticationServices.ASAuthorizationAppleIDProvider
  ( ASAuthorizationAppleIDProvider
  , IsASAuthorizationAppleIDProvider(..)
  , createRequest
  , getCredentialStateForUserID_completion
  , createRequestSelector
  , getCredentialStateForUserID_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This method initializes and returns an instance of
--
-- See: ASAuthorizationAppleIDRequest to be serviced by
--
-- See: ASAuthorizationController.
--
-- ObjC selector: @- createRequest@
createRequest :: IsASAuthorizationAppleIDProvider asAuthorizationAppleIDProvider => asAuthorizationAppleIDProvider -> IO (Id ASAuthorizationAppleIDRequest)
createRequest asAuthorizationAppleIDProvider =
  sendMessage asAuthorizationAppleIDProvider createRequestSelector

-- | This method can be used to get the current state of an opaque user ID previously given.
--
-- @userID@ — Opaque user identifier that will be checked for state.
--
-- @completion@ — A completion block that will return one of 3 possible states
--
-- See: ASAuthorizationAppleIDProviderCredentialState.
--
-- Note: If credentialState is
--
-- See: ASAuthorizationAppleIDProviderCredentialNotFound, an error will also be passed in the completion block.
--
-- ObjC selector: @- getCredentialStateForUserID:completion:@
getCredentialStateForUserID_completion :: (IsASAuthorizationAppleIDProvider asAuthorizationAppleIDProvider, IsNSString userID) => asAuthorizationAppleIDProvider -> userID -> Ptr () -> IO ()
getCredentialStateForUserID_completion asAuthorizationAppleIDProvider userID completion =
  sendMessage asAuthorizationAppleIDProvider getCredentialStateForUserID_completionSelector (toNSString userID) completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRequest@
createRequestSelector :: Selector '[] (Id ASAuthorizationAppleIDRequest)
createRequestSelector = mkSelector "createRequest"

-- | @Selector@ for @getCredentialStateForUserID:completion:@
getCredentialStateForUserID_completionSelector :: Selector '[Id NSString, Ptr ()] ()
getCredentialStateForUserID_completionSelector = mkSelector "getCredentialStateForUserID:completion:"

