{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccountAuthenticationModificationViewController@.
module ObjC.AuthenticationServices.ASAccountAuthenticationModificationViewController
  ( ASAccountAuthenticationModificationViewController
  , IsASAccountAuthenticationModificationViewController(..)
  , convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfo
  , prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfo
  , changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfo
  , prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfo
  , cancelRequest
  , extensionContext
  , cancelRequestSelector
  , changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfoSelector
  , convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfoSelector
  , extensionContextSelector
  , prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfoSelector
  , prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This method will be called when a user initiates a Sign in with Apple upgrade request.
--
-- @serviceIdentifier@ — The service identifier of the credential the user wishes to upgrade.
--
-- @existingCredential@ — The current password-based credential of the account for the upgrade.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the app wants to pass to the extension, most likely to help authorize the upgrade.
--
-- This method will be called to begin a Sign in with Apple upgrade request and will not present any extension UI. If extension UI needs to be shown, this initial request should be canceled with the ASExtensionErrorCodeUserCanceled error code. The existingCredential parameter will have an empty password for in-app upgrades. The extension should check if a user is already logged in by checking a data container shared with its containing app.
--
-- ObjC selector: @- convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier:existingCredential:userInfo:@
convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfo :: (IsASAccountAuthenticationModificationViewController asAccountAuthenticationModificationViewController, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential existingCredential, IsNSDictionary userInfo) => asAccountAuthenticationModificationViewController -> serviceIdentifier -> existingCredential -> userInfo -> IO ()
convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfo asAccountAuthenticationModificationViewController serviceIdentifier existingCredential userInfo =
  sendMessage asAccountAuthenticationModificationViewController convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfoSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential existingCredential) (toNSDictionary userInfo)

-- | This method will be called when a Sign in with Apple upgrade request requires user interaction.
--
-- @serviceIdentifier@ — The service identifier of the credential the user wishes to upgrade.
--
-- @existingCredential@ — The current password-based credential of the account for the upgrade.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the app wants to pass to the extension, most likely to help authorize the upgrade.
--
-- This method will be called when the initial request is canceled with the ASExtensionErrorCodeUserCanceled error code. The implementation of this method should do any work necessary to prepare to present the extension UI. Once it finishes, the system will present the extension UI. The existingCredential parameter will have an empty password for in-app upgrades. The extension should check if a user is already logged in by checking state in a data container shared with its containing app.
--
-- ObjC selector: @- prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier:existingCredential:userInfo:@
prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfo :: (IsASAccountAuthenticationModificationViewController asAccountAuthenticationModificationViewController, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential existingCredential, IsNSDictionary userInfo) => asAccountAuthenticationModificationViewController -> serviceIdentifier -> existingCredential -> userInfo -> IO ()
prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfo asAccountAuthenticationModificationViewController serviceIdentifier existingCredential userInfo =
  sendMessage asAccountAuthenticationModificationViewController prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfoSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential existingCredential) (toNSDictionary userInfo)

-- | This method will be called when a user initiates a strong password upgrade request.
--
-- @serviceIdentifier@ — The service identifier of the credential the user wishes to upgrade.
--
-- @existingCredential@ — The current password-based credential of the account for the upgrade.
--
-- @newPassword@ — A strong password generated by the system to be used for the upgrade.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the app wants to pass to the extension, most likely to help authorize the upgrade.
--
-- Password rules for generating the newPassword parameter can be specified with the ASAccountAuthenticationModificationSupportsUpgradeToSignInWithApple key in the extension's Info.plist. This method will be called to begin a strong password upgrade request and will not present any extension UI. If extension UI needs to be shown, this initial request should be canceled with the ASExtensionErrorCodeUserCanceled error code. The existingCredential parameter will have an empty password for in-app upgrades. The extension should check if a user is already logged in by checking a data container shared with its containing app.
--
-- ObjC selector: @- changePasswordWithoutUserInteractionForServiceIdentifier:existingCredential:newPassword:userInfo:@
changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfo :: (IsASAccountAuthenticationModificationViewController asAccountAuthenticationModificationViewController, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential existingCredential, IsNSString newPassword, IsNSDictionary userInfo) => asAccountAuthenticationModificationViewController -> serviceIdentifier -> existingCredential -> newPassword -> userInfo -> IO ()
changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfo asAccountAuthenticationModificationViewController serviceIdentifier existingCredential newPassword userInfo =
  sendMessage asAccountAuthenticationModificationViewController changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfoSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential existingCredential) (toNSString newPassword) (toNSDictionary userInfo)

-- | This method will be called when a strong password upgrade request requires user interaction.
--
-- @serviceIdentifier@ — The service identifier of the credential the user wishes to upgrade.
--
-- @existingCredential@ — The current password-based credential of the account for the upgrade.
--
-- @newPassword@ — A strong password generated by the system to be used for the upgrade.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the app wants to pass to the extension, most likely to help authorize the upgrade.
--
-- This method will be called when the initial request is canceled with the ASExtensionErrorCodeUserCanceled error code. The implementation of this method should do any work necessary to prepare to present the extension UI. Once it finishes, the system will present the extension UI. Password rules for generating the newPassword parameter can be specified with the ASAccountAuthenticationModificationSupportsUpgradeToSignInWithApple key in the extension's Info.plist. The existingCredential parameter will have an empty password for in-app upgrades. The extension should check if a user is already logged in by checking state in a data container shared with its containing app.
--
-- ObjC selector: @- prepareInterfaceToChangePasswordForServiceIdentifier:existingCredential:newPassword:userInfo:@
prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfo :: (IsASAccountAuthenticationModificationViewController asAccountAuthenticationModificationViewController, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential existingCredential, IsNSString newPassword, IsNSDictionary userInfo) => asAccountAuthenticationModificationViewController -> serviceIdentifier -> existingCredential -> newPassword -> userInfo -> IO ()
prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfo asAccountAuthenticationModificationViewController serviceIdentifier existingCredential newPassword userInfo =
  sendMessage asAccountAuthenticationModificationViewController prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfoSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential existingCredential) (toNSString newPassword) (toNSDictionary userInfo)

-- | This method will be called when the user taps the system-provided "Cancel" button shown above extension UI.
--
-- Extension UI will be dismissed as soon as this method is called. The default implementation simply cancels the request with the ASExtensionErrorCodeUserCanceled error code. Override this method to do any cleanup work before the request is canceled. The implementation of this method should still cancel the request with the ASExtensionErrorCodeUserCanceled error code once the cleanup work is completed. If the request is not canceled in 10 seconds, the system will cancel the request.
--
-- ObjC selector: @- cancelRequest@
cancelRequest :: IsASAccountAuthenticationModificationViewController asAccountAuthenticationModificationViewController => asAccountAuthenticationModificationViewController -> IO ()
cancelRequest asAccountAuthenticationModificationViewController =
  sendMessage asAccountAuthenticationModificationViewController cancelRequestSelector

-- | @- extensionContext@
extensionContext :: IsASAccountAuthenticationModificationViewController asAccountAuthenticationModificationViewController => asAccountAuthenticationModificationViewController -> IO (Id ASAccountAuthenticationModificationExtensionContext)
extensionContext asAccountAuthenticationModificationViewController =
  sendMessage asAccountAuthenticationModificationViewController extensionContextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier:existingCredential:userInfo:@
convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfoSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSDictionary] ()
convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier_existingCredential_userInfoSelector = mkSelector "convertAccountToSignInWithAppleWithoutUserInteractionForServiceIdentifier:existingCredential:userInfo:"

-- | @Selector@ for @prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier:existingCredential:userInfo:@
prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfoSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSDictionary] ()
prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier_existingCredential_userInfoSelector = mkSelector "prepareInterfaceToConvertAccountToSignInWithAppleForServiceIdentifier:existingCredential:userInfo:"

-- | @Selector@ for @changePasswordWithoutUserInteractionForServiceIdentifier:existingCredential:newPassword:userInfo:@
changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfoSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSString, Id NSDictionary] ()
changePasswordWithoutUserInteractionForServiceIdentifier_existingCredential_newPassword_userInfoSelector = mkSelector "changePasswordWithoutUserInteractionForServiceIdentifier:existingCredential:newPassword:userInfo:"

-- | @Selector@ for @prepareInterfaceToChangePasswordForServiceIdentifier:existingCredential:newPassword:userInfo:@
prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfoSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSString, Id NSDictionary] ()
prepareInterfaceToChangePasswordForServiceIdentifier_existingCredential_newPassword_userInfoSelector = mkSelector "prepareInterfaceToChangePasswordForServiceIdentifier:existingCredential:newPassword:userInfo:"

-- | @Selector@ for @cancelRequest@
cancelRequestSelector :: Selector '[] ()
cancelRequestSelector = mkSelector "cancelRequest"

-- | @Selector@ for @extensionContext@
extensionContextSelector :: Selector '[] (Id ASAccountAuthenticationModificationExtensionContext)
extensionContextSelector = mkSelector "extensionContext"

