{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASPasswordCredential@.
module ObjC.AuthenticationServices.ASPasswordCredential
  ( ASPasswordCredential
  , IsASPasswordCredential(..)
  , initWithUser_password
  , credentialWithUser_password
  , user
  , password
  , initWithUser_passwordSelector
  , credentialWithUser_passwordSelector
  , userSelector
  , passwordSelector


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

-- | Initializes an ASPasswordCredential object.
--
-- @user@ — the user.
--
-- @password@ — the password.
--
-- ObjC selector: @- initWithUser:password:@
initWithUser_password :: (IsASPasswordCredential asPasswordCredential, IsNSString user, IsNSString password) => asPasswordCredential -> user -> password -> IO (Id ASPasswordCredential)
initWithUser_password asPasswordCredential  user password =
withObjCPtr user $ \raw_user ->
  withObjCPtr password $ \raw_password ->
      sendMsg asPasswordCredential (mkSelector "initWithUser:password:") (retPtr retVoid) [argPtr (castPtr raw_user :: Ptr ()), argPtr (castPtr raw_password :: Ptr ())] >>= ownedObject . castPtr

-- | Creates and initializes a new ASPasswordCredential object.
--
-- @user@ — the user.
--
-- @password@ — the password.
--
-- ObjC selector: @+ credentialWithUser:password:@
credentialWithUser_password :: (IsNSString user, IsNSString password) => user -> password -> IO (Id ASPasswordCredential)
credentialWithUser_password user password =
  do
    cls' <- getRequiredClass "ASPasswordCredential"
    withObjCPtr user $ \raw_user ->
      withObjCPtr password $ \raw_password ->
        sendClassMsg cls' (mkSelector "credentialWithUser:password:") (retPtr retVoid) [argPtr (castPtr raw_user :: Ptr ()), argPtr (castPtr raw_password :: Ptr ())] >>= retainedObject . castPtr

-- | The user name of this credential.
--
-- Returns: The user string.
--
-- ObjC selector: @- user@
user :: IsASPasswordCredential asPasswordCredential => asPasswordCredential -> IO (Id NSString)
user asPasswordCredential  =
  sendMsg asPasswordCredential (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The password of this credential.
--
-- Returns: The password string.
--
-- ObjC selector: @- password@
password :: IsASPasswordCredential asPasswordCredential => asPasswordCredential -> IO (Id NSString)
password asPasswordCredential  =
  sendMsg asPasswordCredential (mkSelector "password") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUser:password:@
initWithUser_passwordSelector :: Selector
initWithUser_passwordSelector = mkSelector "initWithUser:password:"

-- | @Selector@ for @credentialWithUser:password:@
credentialWithUser_passwordSelector :: Selector
credentialWithUser_passwordSelector = mkSelector "credentialWithUser:password:"

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @password@
passwordSelector :: Selector
passwordSelector = mkSelector "password"

