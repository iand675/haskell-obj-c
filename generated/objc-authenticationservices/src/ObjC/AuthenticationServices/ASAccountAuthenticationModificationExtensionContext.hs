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
  , getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector
  , completeUpgradeToSignInWithAppleWithUserInfoSelector
  , completeChangePasswordRequestWithUpdatedCredential_userInfoSelector
  , cancelRequestWithErrorSelector


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
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandler asAccountAuthenticationModificationExtensionContext  state nonce completionHandler =
withObjCPtr state $ \raw_state ->
  withObjCPtr nonce $ \raw_nonce ->
      sendMsg asAccountAuthenticationModificationExtensionContext (mkSelector "getSignInWithAppleUpgradeAuthorizationWithState:nonce:completionHandler:") retVoid [argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr raw_nonce :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Confirms successful completion of a Sign in with Apple upgrade.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the extension wants to pass back to the app.
--
-- Once a Sign in with Apple upgrade is completed, the system will delete the password-based credential from the Keychain, if it is saved there.
--
-- ObjC selector: @- completeUpgradeToSignInWithAppleWithUserInfo:@
completeUpgradeToSignInWithAppleWithUserInfo :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsNSDictionary userInfo) => asAccountAuthenticationModificationExtensionContext -> userInfo -> IO ()
completeUpgradeToSignInWithAppleWithUserInfo asAccountAuthenticationModificationExtensionContext  userInfo =
withObjCPtr userInfo $ \raw_userInfo ->
    sendMsg asAccountAuthenticationModificationExtensionContext (mkSelector "completeUpgradeToSignInWithAppleWithUserInfo:") retVoid [argPtr (castPtr raw_userInfo :: Ptr ())]

-- | Confirms successful completion of a strong password upgrade.
--
-- @updatedCredential@ — contains the account username and new password.
--
-- @userInfo@ — For upgrades invoked within the extension's containing app, any information the extension wants to pass back to the app.
--
-- ObjC selector: @- completeChangePasswordRequestWithUpdatedCredential:userInfo:@
completeChangePasswordRequestWithUpdatedCredential_userInfo :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsASPasswordCredential updatedCredential, IsNSDictionary userInfo) => asAccountAuthenticationModificationExtensionContext -> updatedCredential -> userInfo -> IO ()
completeChangePasswordRequestWithUpdatedCredential_userInfo asAccountAuthenticationModificationExtensionContext  updatedCredential userInfo =
withObjCPtr updatedCredential $ \raw_updatedCredential ->
  withObjCPtr userInfo $ \raw_userInfo ->
      sendMsg asAccountAuthenticationModificationExtensionContext (mkSelector "completeChangePasswordRequestWithUpdatedCredential:userInfo:") retVoid [argPtr (castPtr raw_updatedCredential :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ())]

-- | Used to either ask for user interaction in a request or to fail a request.
--
-- ObjC selector: @- cancelRequestWithError:@
cancelRequestWithError :: (IsASAccountAuthenticationModificationExtensionContext asAccountAuthenticationModificationExtensionContext, IsNSError error_) => asAccountAuthenticationModificationExtensionContext -> error_ -> IO ()
cancelRequestWithError asAccountAuthenticationModificationExtensionContext  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg asAccountAuthenticationModificationExtensionContext (mkSelector "cancelRequestWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSignInWithAppleUpgradeAuthorizationWithState:nonce:completionHandler:@
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector :: Selector
getSignInWithAppleUpgradeAuthorizationWithState_nonce_completionHandlerSelector = mkSelector "getSignInWithAppleUpgradeAuthorizationWithState:nonce:completionHandler:"

-- | @Selector@ for @completeUpgradeToSignInWithAppleWithUserInfo:@
completeUpgradeToSignInWithAppleWithUserInfoSelector :: Selector
completeUpgradeToSignInWithAppleWithUserInfoSelector = mkSelector "completeUpgradeToSignInWithAppleWithUserInfo:"

-- | @Selector@ for @completeChangePasswordRequestWithUpdatedCredential:userInfo:@
completeChangePasswordRequestWithUpdatedCredential_userInfoSelector :: Selector
completeChangePasswordRequestWithUpdatedCredential_userInfoSelector = mkSelector "completeChangePasswordRequestWithUpdatedCredential:userInfo:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

