{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationAppleIDRequest@.
module ObjC.AuthenticationServices.ASAuthorizationAppleIDRequest
  ( ASAuthorizationAppleIDRequest
  , IsASAuthorizationAppleIDRequest(..)
  , user
  , setUser
  , setUserSelector
  , userSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | If you have been previously vended a 'user' value through ASAuthorization response, you may set it here to provide additional context to identity provider.
--
-- See: ASAuthorizationAppleIDCredential doc for the description of this property in context of response.
--
-- ObjC selector: @- user@
user :: IsASAuthorizationAppleIDRequest asAuthorizationAppleIDRequest => asAuthorizationAppleIDRequest -> IO (Id NSString)
user asAuthorizationAppleIDRequest =
  sendMessage asAuthorizationAppleIDRequest userSelector

-- | If you have been previously vended a 'user' value through ASAuthorization response, you may set it here to provide additional context to identity provider.
--
-- See: ASAuthorizationAppleIDCredential doc for the description of this property in context of response.
--
-- ObjC selector: @- setUser:@
setUser :: (IsASAuthorizationAppleIDRequest asAuthorizationAppleIDRequest, IsNSString value) => asAuthorizationAppleIDRequest -> value -> IO ()
setUser asAuthorizationAppleIDRequest value =
  sendMessage asAuthorizationAppleIDRequest setUserSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @setUser:@
setUserSelector :: Selector '[Id NSString] ()
setUserSelector = mkSelector "setUser:"

