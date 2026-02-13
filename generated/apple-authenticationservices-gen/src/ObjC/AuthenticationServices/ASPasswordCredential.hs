{-# LANGUAGE DataKinds #-}
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
  , credentialWithUser_passwordSelector
  , initWithUser_passwordSelector
  , passwordSelector
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

-- | Initializes an ASPasswordCredential object.
--
-- @user@ — the user.
--
-- @password@ — the password.
--
-- ObjC selector: @- initWithUser:password:@
initWithUser_password :: (IsASPasswordCredential asPasswordCredential, IsNSString user, IsNSString password) => asPasswordCredential -> user -> password -> IO (Id ASPasswordCredential)
initWithUser_password asPasswordCredential user password =
  sendOwnedMessage asPasswordCredential initWithUser_passwordSelector (toNSString user) (toNSString password)

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
    sendClassMessage cls' credentialWithUser_passwordSelector (toNSString user) (toNSString password)

-- | The user name of this credential.
--
-- Returns: The user string.
--
-- ObjC selector: @- user@
user :: IsASPasswordCredential asPasswordCredential => asPasswordCredential -> IO (Id NSString)
user asPasswordCredential =
  sendMessage asPasswordCredential userSelector

-- | The password of this credential.
--
-- Returns: The password string.
--
-- ObjC selector: @- password@
password :: IsASPasswordCredential asPasswordCredential => asPasswordCredential -> IO (Id NSString)
password asPasswordCredential =
  sendMessage asPasswordCredential passwordSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUser:password:@
initWithUser_passwordSelector :: Selector '[Id NSString, Id NSString] (Id ASPasswordCredential)
initWithUser_passwordSelector = mkSelector "initWithUser:password:"

-- | @Selector@ for @credentialWithUser:password:@
credentialWithUser_passwordSelector :: Selector '[Id NSString, Id NSString] (Id ASPasswordCredential)
credentialWithUser_passwordSelector = mkSelector "credentialWithUser:password:"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

