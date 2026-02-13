{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccountAuthenticationModificationExtensionContext@.
module ObjC.AuthenticationServices.ASAccountAuthenticationModificationExtensionContext
  ( ASAccountAuthenticationModificationExtensionContext
  , IsASAccountAuthenticationModificationExtensionContext(..)
  , getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandler
  , completeUpgradeToSignInWithAppleWithUserInfo
  , completeChangePasswordRequestWithUpdatedCredential_userInfo
  , cancelRequestWithError
  , cancelRequestWithErrorSelector
  , completeChangePasswordRequestWithUpdatedCredential_userInfoSelector
  , completeUpgradeToSignInWithAppleWithUserInfoSelector
  , getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Retrieves a Sign in with Apple credential from the system.
--
-- @state@ — Can be specified to enhance security. State is returned in the ASAuthorizationAppleIDCredential.
--
-- @nonce@ — Can be specified to enhance security. The returned identityToken on the ASAuthorizationAppleIDCredential can be exchanged with the server for the nonce.
--
-- Calling this method will cause the system Sign in with Apple upgrade sheet to appear. If extension UI is showing when this method is called, the extension UI will be dismissed before the sheet is presented.
--
-- ObjC selector: @- getSignInWithAppleUpgradeAuthorizationWithState:nonce:completionHandler:@
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandler :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsNSString state, IsNSString nonce) => asAccountAuthenticationModificationExtensionContext -> state -> nonce -> Ptr () -> IO ()
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandler asAccountAuthenticationModificationExtensionContext state nonce completionHandler =
  sendMessage asAccountAuthenticationModificationExtensionContext getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector (toNSString state) (toNSString nonce) completionHandler

-- | Confirms successful completion of a Sign in with Apple upgrade.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the extension wants to pass back to the app.
--
-- Once a Sign in with Apple upgrade is completed, the system will delete the password-based credential from the Keychain, if it is saved there.
--
-- ObjC selector: @- completeUpgradeToSignInWithAppleWithUserInfo:@
completeUpgradeToSignInWithAppleWithUserInfo :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsNSDictionary userInfo) => asAccountAuthenticationModificationExtensionContext -> userInfo -> IO ()
completeUpgradeToSignInWithAppleWithUserInfo asAccountAuthenticationModificationExtensionContext userInfo =
  sendMessage asAccountAuthenticationModificationExtensionContext completeUpgradeToSignInWithAppleWithUserInfoSelector (toNSDictionary userInfo)

-- | Confirms successful completion of a strong password upgrade.
--
-- @updatedCredential@ — contains the account username and new password.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the extension wants to pass back to the app.
--
-- ObjC selector: @- completeChangePasswordRequestWithUpdatedCredential:userInfo:@
completeChangePasswordRequestWithUpdatedCredential_userInfo :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsASPasswordCredential updatedCredential, IsNSDictionary userInfo) => asAccountAuthenticationModificationExtensionContext -> updatedCredential -> userInfo -> IO ()
completeChangePasswordRequestWithUpdatedCredential_userInfo asAccountAuthenticationModificationExtensionContext updatedCredential userInfo =
  sendMessage asAccountAuthenticationModificationExtensionContext completeChangePasswordRequestWithUpdatedCredential_userInfoSelector (toASPasswordCredential updatedCredential) (toNSDictionary userInfo)

-- | Used to either ask for user interaction in a request or to fail a request.
--
-- ObjC selector: @- cancelRequestWithError:@
cancelRequestWithError :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsNSError error_) => asAccountAuthenticationModificationExtensionContext -> error_ -> IO ()
cancelRequestWithError asAccountAuthenticationModificationExtensionContext error_ =
  sendMessage asAccountAuthenticationModificationExtensionContext cancelRequestWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSignInWithAppleUpgradeAuthorizationWithState:nonce:completionHandler:@
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Ptr ()] ()
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector = mkSelector "getSignInWithAppleUpgradeAuthorizationWithState:nonce:completionHandler:"

-- | @Selector@ for @completeUpgradeToSignInWithAppleWithUserInfo:@
completeUpgradeToSignInWithAppleWithUserInfoSelector :: Selector '[Id NSDictionary] ()
completeUpgradeToSignInWithAppleWithUserInfoSelector = mkSelector "completeUpgradeToSignInWithAppleWithUserInfo:"

-- | @Selector@ for @completeChangePasswordRequestWithUpdatedCredential:userInfo:@
completeChangePasswordRequestWithUpdatedCredential_userInfoSelector :: Selector '[Id ASPasswordCredential, Id NSDictionary] ()
completeChangePasswordRequestWithUpdatedCredential_userInfoSelector = mkSelector "completeChangePasswordRequestWithUpdatedCredential:userInfo:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector '[Id NSError] ()
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

