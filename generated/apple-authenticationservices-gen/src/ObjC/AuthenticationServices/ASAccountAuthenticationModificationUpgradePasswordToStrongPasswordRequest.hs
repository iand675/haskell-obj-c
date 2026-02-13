{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest@.
module ObjC.AuthenticationServices.ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest
  ( ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest
  , IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest(..)
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

-- | Creates a request for a strong password upgrade invoked within the extension's containing app.
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
initWithUser_serviceIdentifier_userInfo :: (IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest, IsNSString user, IsASCredentialServiceIdentifier serviceIdentifier, IsNSDictionary userInfo) => asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest -> user -> serviceIdentifier -> userInfo -> IO (Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest)
initWithUser_serviceIdentifier_userInfo asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest user serviceIdentifier userInfo =
  sendOwnedMessage asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest initWithUser_serviceIdentifier_userInfoSelector (toNSString user) (toASCredentialServiceIdentifier serviceIdentifier) (toNSDictionary userInfo)

-- | @- user@
user :: IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest => asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest -> IO (Id NSString)
user asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest =
  sendMessage asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest userSelector

-- | @- serviceIdentifier@
serviceIdentifier :: IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest => asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest =
  sendMessage asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest serviceIdentifierSelector

-- | @- userInfo@
userInfo :: IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest => asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest -> IO (Id NSDictionary)
userInfo asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest =
  sendMessage asAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest userInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUser:serviceIdentifier:userInfo:@
initWithUser_serviceIdentifier_userInfoSelector :: Selector '[Id NSString, Id ASCredentialServiceIdentifier, Id NSDictionary] (Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest)
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

