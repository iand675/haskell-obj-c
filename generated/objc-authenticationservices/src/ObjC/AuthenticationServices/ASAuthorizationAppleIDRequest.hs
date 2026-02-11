{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationAppleIDRequest@.
module ObjC.AuthenticationServices.ASAuthorizationAppleIDRequest
  ( ASAuthorizationAppleIDRequest
  , IsASAuthorizationAppleIDRequest(..)
  , user
  , setUser
  , userSelector
  , setUserSelector


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

-- | If you have been previously vended a 'user' value through ASAuthorization response, you may set it here to provide additional context to identity provider.
--
-- See: ASAuthorizationAppleIDCredential doc for the description of this property in context of response.
--
-- ObjC selector: @- user@
user :: IsASAuthorizationAppleIDRequest asAuthorizationAppleIDRequest => asAuthorizationAppleIDRequest -> IO (Id NSString)
user asAuthorizationAppleIDRequest  =
  sendMsg asAuthorizationAppleIDRequest (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If you have been previously vended a 'user' value through ASAuthorization response, you may set it here to provide additional context to identity provider.
--
-- See: ASAuthorizationAppleIDCredential doc for the description of this property in context of response.
--
-- ObjC selector: @- setUser:@
setUser :: (IsASAuthorizationAppleIDRequest asAuthorizationAppleIDRequest, IsNSString value) => asAuthorizationAppleIDRequest -> value -> IO ()
setUser asAuthorizationAppleIDRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationAppleIDRequest (mkSelector "setUser:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @setUser:@
setUserSelector :: Selector
setUserSelector = mkSelector "setUser:"

