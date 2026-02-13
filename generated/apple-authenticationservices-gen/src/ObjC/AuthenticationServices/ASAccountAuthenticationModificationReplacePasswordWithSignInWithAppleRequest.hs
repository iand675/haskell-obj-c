{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest@.
module ObjC.AuthenticationServices.ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest
  ( ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest
  , IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest(..)
  , initWithUser_serviceIdentifier_userInfo
  , user
  , serviceIdentifier
  , userInfo
  , initWithUser_serviceIdentifier_userInfoSelector
  , serviceIdentifierSelector
  , userInfoSelector
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

-- | Creates a request for a Sign in with Apple upgrade invoked within the extension's containing app.
--
-- @user@ — The username for the account to upgrade.
--
-- @serviceIdentifier@ — The service identifier of the credential the user wishes to upgrade.
--
-- @userInfo@ — A dictionary the app can use to pass information to the extension, most likely to help with authorizing the upgrade.
--
-- In this flow, when the extension is invoked from within the containing app, the extension will receive an empty password for the credential to upgrade. It should check that it is authorized to perform the upgrade. The authorization check should ideally be done with information in userInfo, but may involve communicating with a backend server or using a shared data container between the app and extension.
--
-- ObjC selector: @- initWithUser:serviceIdentifier:userInfo:@
initWithUser_serviceIdentifier_userInfo :: (IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest, IsNSString user, IsASCredentialServiceIdentifier serviceIdentifier, IsNSDictionary userInfo) => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> user -> serviceIdentifier -> userInfo -> IO (Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest)
initWithUser_serviceIdentifier_userInfo asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest user serviceIdentifier userInfo =
  sendOwnedMessage asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest initWithUser_serviceIdentifier_userInfoSelector (toNSString user) (toASCredentialServiceIdentifier serviceIdentifier) (toNSDictionary userInfo)

-- | @- user@
user :: IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> IO (Id NSString)
user asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest =
  sendMessage asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest userSelector

-- | @- serviceIdentifier@
serviceIdentifier :: IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest =
  sendMessage asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest serviceIdentifierSelector

-- | @- userInfo@
userInfo :: IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> IO (Id NSDictionary)
userInfo asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest =
  sendMessage asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest userInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUser:serviceIdentifier:userInfo:@
initWithUser_serviceIdentifier_userInfoSelector :: Selector '[Id NSString, Id ASCredentialServiceIdentifier, Id NSDictionary] (Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest)
initWithUser_serviceIdentifier_userInfoSelector = mkSelector "initWithUser:serviceIdentifier:userInfo:"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector '[] (Id ASCredentialServiceIdentifier)
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

