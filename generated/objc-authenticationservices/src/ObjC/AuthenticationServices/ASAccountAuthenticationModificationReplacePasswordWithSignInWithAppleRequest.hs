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
  , userSelector
  , serviceIdentifierSelector
  , userInfoSelector


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
initWithUser_serviceIdentifier_userInfo asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest  user serviceIdentifier userInfo =
withObjCPtr user $ \raw_user ->
  withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
    withObjCPtr userInfo $ \raw_userInfo ->
        sendMsg asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest (mkSelector "initWithUser:serviceIdentifier:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_user :: Ptr ()), argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- user@
user :: IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> IO (Id NSString)
user asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest  =
  sendMsg asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serviceIdentifier@
serviceIdentifier :: IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest  =
  sendMsg asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest (mkSelector "serviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userInfo@
userInfo :: IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest => asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest -> IO (Id NSDictionary)
userInfo asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest  =
  sendMsg asAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUser:serviceIdentifier:userInfo:@
initWithUser_serviceIdentifier_userInfoSelector :: Selector
initWithUser_serviceIdentifier_userInfoSelector = mkSelector "initWithUser:serviceIdentifier:userInfo:"

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

