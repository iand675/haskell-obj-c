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
import ObjC.Foundation.Internal.Classes

-- | This method initializes and returns an instance of
--
-- See: ASAuthorizationAppleIDRequest to be serviced by
--
-- See: ASAuthorizationController.
--
-- ObjC selector: @- createRequest@
createRequest :: IsASAuthorizationAppleIDProvider asAuthorizationAppleIDProvider => asAuthorizationAppleIDProvider -> IO (Id ASAuthorizationAppleIDRequest)
createRequest asAuthorizationAppleIDProvider  =
  sendMsg asAuthorizationAppleIDProvider (mkSelector "createRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

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
getCredentialStateForUserID_completion asAuthorizationAppleIDProvider  userID completion =
withObjCPtr userID $ \raw_userID ->
    sendMsg asAuthorizationAppleIDProvider (mkSelector "getCredentialStateForUserID:completion:") retVoid [argPtr (castPtr raw_userID :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRequest@
createRequestSelector :: Selector
createRequestSelector = mkSelector "createRequest"

-- | @Selector@ for @getCredentialStateForUserID:completion:@
getCredentialStateForUserID_completionSelector :: Selector
getCredentialStateForUserID_completionSelector = mkSelector "getCredentialStateForUserID:completion:"

