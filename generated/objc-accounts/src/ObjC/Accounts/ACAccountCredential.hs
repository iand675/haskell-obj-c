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
  , initWithOAuthToken_tokenSecretSelector
  , initWithOAuth2Token_refreshToken_expiryDateSelector
  , oauthTokenSelector
  , setOauthTokenSelector


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

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOAuthToken:tokenSecret:@
initWithOAuthToken_tokenSecret :: (IsACAccountCredential acAccountCredential, IsNSString token, IsNSString secret) => acAccountCredential -> token -> secret -> IO (Id ACAccountCredential)
initWithOAuthToken_tokenSecret acAccountCredential  token secret =
withObjCPtr token $ \raw_token ->
  withObjCPtr secret $ \raw_secret ->
      sendMsg acAccountCredential (mkSelector "initWithOAuthToken:tokenSecret:") (retPtr retVoid) [argPtr (castPtr raw_token :: Ptr ()), argPtr (castPtr raw_secret :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithOAuth2Token:refreshToken:expiryDate:@
initWithOAuth2Token_refreshToken_expiryDate :: (IsACAccountCredential acAccountCredential, IsNSString token, IsNSString refreshToken, IsNSDate expiryDate) => acAccountCredential -> token -> refreshToken -> expiryDate -> IO (Id ACAccountCredential)
initWithOAuth2Token_refreshToken_expiryDate acAccountCredential  token refreshToken expiryDate =
withObjCPtr token $ \raw_token ->
  withObjCPtr refreshToken $ \raw_refreshToken ->
    withObjCPtr expiryDate $ \raw_expiryDate ->
        sendMsg acAccountCredential (mkSelector "initWithOAuth2Token:refreshToken:expiryDate:") (retPtr retVoid) [argPtr (castPtr raw_token :: Ptr ()), argPtr (castPtr raw_refreshToken :: Ptr ()), argPtr (castPtr raw_expiryDate :: Ptr ())] >>= ownedObject . castPtr

-- | @- oauthToken@
oauthToken :: IsACAccountCredential acAccountCredential => acAccountCredential -> IO (Id NSString)
oauthToken acAccountCredential  =
  sendMsg acAccountCredential (mkSelector "oauthToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOauthToken:@
setOauthToken :: (IsACAccountCredential acAccountCredential, IsNSString value) => acAccountCredential -> value -> IO ()
setOauthToken acAccountCredential  value =
withObjCPtr value $ \raw_value ->
    sendMsg acAccountCredential (mkSelector "setOauthToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOAuthToken:tokenSecret:@
initWithOAuthToken_tokenSecretSelector :: Selector
initWithOAuthToken_tokenSecretSelector = mkSelector "initWithOAuthToken:tokenSecret:"

-- | @Selector@ for @initWithOAuth2Token:refreshToken:expiryDate:@
initWithOAuth2Token_refreshToken_expiryDateSelector :: Selector
initWithOAuth2Token_refreshToken_expiryDateSelector = mkSelector "initWithOAuth2Token:refreshToken:expiryDate:"

-- | @Selector@ for @oauthToken@
oauthTokenSelector :: Selector
oauthTokenSelector = mkSelector "oauthToken"

-- | @Selector@ for @setOauthToken:@
setOauthTokenSelector :: Selector
setOauthTokenSelector = mkSelector "setOauthToken:"

