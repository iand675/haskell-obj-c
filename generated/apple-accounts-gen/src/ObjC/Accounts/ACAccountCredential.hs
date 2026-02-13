{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ACAccountCredential@.
module ObjC.Accounts.ACAccountCredential
  ( ACAccountCredential
  , IsACAccountCredential(..)
  , initWithOAuthToken_tokenSecret
  , initWithOAuth2Token_refreshToken_expiryDate
  , oauthToken
  , setOauthToken
  , initWithOAuth2Token_refreshToken_expiryDateSelector
  , initWithOAuthToken_tokenSecretSelector
  , oauthTokenSelector
  , setOauthTokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOAuthToken:tokenSecret:@
initWithOAuthToken_tokenSecret :: (IsACAccountCredential acAccountCredential, IsNSString token, IsNSString secret) => acAccountCredential -> token -> secret -> IO (Id ACAccountCredential)
initWithOAuthToken_tokenSecret acAccountCredential token secret =
  sendOwnedMessage acAccountCredential initWithOAuthToken_tokenSecretSelector (toNSString token) (toNSString secret)

-- | @- initWithOAuth2Token:refreshToken:expiryDate:@
initWithOAuth2Token_refreshToken_expiryDate :: (IsACAccountCredential acAccountCredential, IsNSString token, IsNSString refreshToken, IsNSDate expiryDate) => acAccountCredential -> token -> refreshToken -> expiryDate -> IO (Id ACAccountCredential)
initWithOAuth2Token_refreshToken_expiryDate acAccountCredential token refreshToken expiryDate =
  sendOwnedMessage acAccountCredential initWithOAuth2Token_refreshToken_expiryDateSelector (toNSString token) (toNSString refreshToken) (toNSDate expiryDate)

-- | @- oauthToken@
oauthToken :: IsACAccountCredential acAccountCredential => acAccountCredential -> IO (Id NSString)
oauthToken acAccountCredential =
  sendMessage acAccountCredential oauthTokenSelector

-- | @- setOauthToken:@
setOauthToken :: (IsACAccountCredential acAccountCredential, IsNSString value) => acAccountCredential -> value -> IO ()
setOauthToken acAccountCredential value =
  sendMessage acAccountCredential setOauthTokenSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOAuthToken:tokenSecret:@
initWithOAuthToken_tokenSecretSelector :: Selector '[Id NSString, Id NSString] (Id ACAccountCredential)
initWithOAuthToken_tokenSecretSelector = mkSelector "initWithOAuthToken:tokenSecret:"

-- | @Selector@ for @initWithOAuth2Token:refreshToken:expiryDate:@
initWithOAuth2Token_refreshToken_expiryDateSelector :: Selector '[Id NSString, Id NSString, Id NSDate] (Id ACAccountCredential)
initWithOAuth2Token_refreshToken_expiryDateSelector = mkSelector "initWithOAuth2Token:refreshToken:expiryDate:"

-- | @Selector@ for @oauthToken@
oauthTokenSelector :: Selector '[] (Id NSString)
oauthTokenSelector = mkSelector "oauthToken"

-- | @Selector@ for @setOauthToken:@
setOauthTokenSelector :: Selector '[Id NSString] ()
setOauthTokenSelector = mkSelector "setOauthToken:"

