{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationProviderExtensionUserLoginConfiguration@.
module ObjC.AuthenticationServices.ASAuthorizationProviderExtensionUserLoginConfiguration
  ( ASAuthorizationProviderExtensionUserLoginConfiguration
  , IsASAuthorizationProviderExtensionUserLoginConfiguration(..)
  , init_
  , new
  , initWithLoginUserName
  , setCustomAssertionRequestHeaderClaims_returningError
  , setCustomAssertionRequestBodyClaims_returningError
  , setCustomLoginRequestHeaderClaims_returningError
  , setCustomLoginRequestBodyClaims_returningError
  , loginUserName
  , setLoginUserName
  , initSelector
  , newSelector
  , initWithLoginUserNameSelector
  , setCustomAssertionRequestHeaderClaims_returningErrorSelector
  , setCustomAssertionRequestBodyClaims_returningErrorSelector
  , setCustomLoginRequestHeaderClaims_returningErrorSelector
  , setCustomLoginRequestBodyClaims_returningErrorSelector
  , loginUserNameSelector
  , setLoginUserNameSelector


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

-- | @- init@
init_ :: IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration => asAuthorizationProviderExtensionUserLoginConfiguration -> IO (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
init_ asAuthorizationProviderExtensionUserLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationProviderExtensionUserLoginConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates an instance with the required values.
--
-- @loginUserName@ — The login user name to use.
--
-- ObjC selector: @- initWithLoginUserName:@
initWithLoginUserName :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSString loginUserName) => asAuthorizationProviderExtensionUserLoginConfiguration -> loginUserName -> IO (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
initWithLoginUserName asAuthorizationProviderExtensionUserLoginConfiguration  loginUserName =
withObjCPtr loginUserName $ \raw_loginUserName ->
    sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "initWithLoginUserName:") (retPtr retVoid) [argPtr (castPtr raw_loginUserName :: Ptr ())] >>= ownedObject . castPtr

-- | Sets custom claims to be added to the embedded assertion request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomAssertionRequestHeaderClaims:returningError:@
setCustomAssertionRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionUserLoginConfiguration -> claims -> error_ -> IO Bool
setCustomAssertionRequestHeaderClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "setCustomAssertionRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the embedded assertion request body.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomAssertionRequestBodyClaims:returningError:@
setCustomAssertionRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionUserLoginConfiguration -> claims -> error_ -> IO Bool
setCustomAssertionRequestBodyClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "setCustomAssertionRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the login request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomLoginRequestHeaderClaims:returningError:@
setCustomLoginRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionUserLoginConfiguration -> claims -> error_ -> IO Bool
setCustomLoginRequestHeaderClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "setCustomLoginRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the login request body.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomLoginRequestBodyClaims:returningError:@
setCustomLoginRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionUserLoginConfiguration -> claims -> error_ -> IO Bool
setCustomLoginRequestBodyClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "setCustomLoginRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | The user name to use when authenticating with the identity provider.
--
-- ObjC selector: @- loginUserName@
loginUserName :: IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration => asAuthorizationProviderExtensionUserLoginConfiguration -> IO (Id NSString)
loginUserName asAuthorizationProviderExtensionUserLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "loginUserName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user name to use when authenticating with the identity provider.
--
-- ObjC selector: @- setLoginUserName:@
setLoginUserName :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionUserLoginConfiguration -> value -> IO ()
setLoginUserName asAuthorizationProviderExtensionUserLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionUserLoginConfiguration (mkSelector "setLoginUserName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithLoginUserName:@
initWithLoginUserNameSelector :: Selector
initWithLoginUserNameSelector = mkSelector "initWithLoginUserName:"

-- | @Selector@ for @setCustomAssertionRequestHeaderClaims:returningError:@
setCustomAssertionRequestHeaderClaims_returningErrorSelector :: Selector
setCustomAssertionRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomAssertionRequestBodyClaims:returningError:@
setCustomAssertionRequestBodyClaims_returningErrorSelector :: Selector
setCustomAssertionRequestBodyClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestHeaderClaims:returningError:@
setCustomLoginRequestHeaderClaims_returningErrorSelector :: Selector
setCustomLoginRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomLoginRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestBodyClaims:returningError:@
setCustomLoginRequestBodyClaims_returningErrorSelector :: Selector
setCustomLoginRequestBodyClaims_returningErrorSelector = mkSelector "setCustomLoginRequestBodyClaims:returningError:"

-- | @Selector@ for @loginUserName@
loginUserNameSelector :: Selector
loginUserNameSelector = mkSelector "loginUserName"

-- | @Selector@ for @setLoginUserName:@
setLoginUserNameSelector :: Selector
setLoginUserNameSelector = mkSelector "setLoginUserName:"

