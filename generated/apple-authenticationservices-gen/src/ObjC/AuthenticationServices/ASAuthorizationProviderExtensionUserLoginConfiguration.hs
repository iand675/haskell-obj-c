{-# LANGUAGE DataKinds #-}
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
  , initWithLoginUserNameSelector
  , loginUserNameSelector
  , newSelector
  , setCustomAssertionRequestBodyClaims_returningErrorSelector
  , setCustomAssertionRequestHeaderClaims_returningErrorSelector
  , setCustomLoginRequestBodyClaims_returningErrorSelector
  , setCustomLoginRequestHeaderClaims_returningErrorSelector
  , setLoginUserNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration => asAuthorizationProviderExtensionUserLoginConfiguration -> IO (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
init_ asAuthorizationProviderExtensionUserLoginConfiguration =
  sendOwnedMessage asAuthorizationProviderExtensionUserLoginConfiguration initSelector

-- | @+ new@
new :: IO (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationProviderExtensionUserLoginConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Creates an instance with the required values.
--
-- @loginUserName@ — The login user name to use.
--
-- ObjC selector: @- initWithLoginUserName:@
initWithLoginUserName :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSString loginUserName) => asAuthorizationProviderExtensionUserLoginConfiguration -> loginUserName -> IO (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
initWithLoginUserName asAuthorizationProviderExtensionUserLoginConfiguration loginUserName =
  sendOwnedMessage asAuthorizationProviderExtensionUserLoginConfiguration initWithLoginUserNameSelector (toNSString loginUserName)

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
setCustomAssertionRequestHeaderClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionUserLoginConfiguration setCustomAssertionRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomAssertionRequestBodyClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionUserLoginConfiguration setCustomAssertionRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomLoginRequestHeaderClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionUserLoginConfiguration setCustomLoginRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomLoginRequestBodyClaims_returningError asAuthorizationProviderExtensionUserLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionUserLoginConfiguration setCustomLoginRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

-- | The user name to use when authenticating with the identity provider.
--
-- ObjC selector: @- loginUserName@
loginUserName :: IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration => asAuthorizationProviderExtensionUserLoginConfiguration -> IO (Id NSString)
loginUserName asAuthorizationProviderExtensionUserLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionUserLoginConfiguration loginUserNameSelector

-- | The user name to use when authenticating with the identity provider.
--
-- ObjC selector: @- setLoginUserName:@
setLoginUserName :: (IsASAuthorizationProviderExtensionUserLoginConfiguration asAuthorizationProviderExtensionUserLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionUserLoginConfiguration -> value -> IO ()
setLoginUserName asAuthorizationProviderExtensionUserLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionUserLoginConfiguration setLoginUserNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithLoginUserName:@
initWithLoginUserNameSelector :: Selector '[Id NSString] (Id ASAuthorizationProviderExtensionUserLoginConfiguration)
initWithLoginUserNameSelector = mkSelector "initWithLoginUserName:"

-- | @Selector@ for @setCustomAssertionRequestHeaderClaims:returningError:@
setCustomAssertionRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomAssertionRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomAssertionRequestBodyClaims:returningError:@
setCustomAssertionRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomAssertionRequestBodyClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestHeaderClaims:returningError:@
setCustomLoginRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomLoginRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomLoginRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestBodyClaims:returningError:@
setCustomLoginRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomLoginRequestBodyClaims_returningErrorSelector = mkSelector "setCustomLoginRequestBodyClaims:returningError:"

-- | @Selector@ for @loginUserName@
loginUserNameSelector :: Selector '[] (Id NSString)
loginUserNameSelector = mkSelector "loginUserName"

-- | @Selector@ for @setLoginUserName:@
setLoginUserNameSelector :: Selector '[Id NSString] ()
setLoginUserNameSelector = mkSelector "setLoginUserName:"

