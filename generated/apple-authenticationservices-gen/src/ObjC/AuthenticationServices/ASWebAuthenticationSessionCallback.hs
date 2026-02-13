{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object used to evaluate navigation events in an authentication session. When the session navigates to a matching URL, it will pass the URL to the session completion handler.
--
-- Generated bindings for @ASWebAuthenticationSessionCallback@.
module ObjC.AuthenticationServices.ASWebAuthenticationSessionCallback
  ( ASWebAuthenticationSessionCallback
  , IsASWebAuthenticationSessionCallback(..)
  , callbackWithCustomScheme
  , callbackWithHTTPSHost_path
  , init_
  , new
  , matchesURL
  , callbackWithCustomSchemeSelector
  , callbackWithHTTPSHost_pathSelector
  , initSelector
  , matchesURLSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a callback object that matches against URLs with the given custom scheme.
--
-- @customScheme@ — The custom scheme that the app expects in the callback URL.
--
-- ObjC selector: @+ callbackWithCustomScheme:@
callbackWithCustomScheme :: IsNSString customScheme => customScheme -> IO (Id ASWebAuthenticationSessionCallback)
callbackWithCustomScheme customScheme =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionCallback"
    sendClassMessage cls' callbackWithCustomSchemeSelector (toNSString customScheme)

-- | Creates a callback object that matches against HTTPS URLs with the given host and path.
--
-- @host@ — The host that the app expects in the callback URL. The host must be associated with the app using associated web credentials domains.
--
-- @path@ — The path that the app expects in the callback URL.
--
-- ObjC selector: @+ callbackWithHTTPSHost:path:@
callbackWithHTTPSHost_path :: (IsNSString host, IsNSString path) => host -> path -> IO (Id ASWebAuthenticationSessionCallback)
callbackWithHTTPSHost_path host path =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionCallback"
    sendClassMessage cls' callbackWithHTTPSHost_pathSelector (toNSString host) (toNSString path)

-- | @- init@
init_ :: IsASWebAuthenticationSessionCallback asWebAuthenticationSessionCallback => asWebAuthenticationSessionCallback -> IO (Id ASWebAuthenticationSessionCallback)
init_ asWebAuthenticationSessionCallback =
  sendOwnedMessage asWebAuthenticationSessionCallback initSelector

-- | @+ new@
new :: IO (Id ASWebAuthenticationSessionCallback)
new  =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionCallback"
    sendOwnedClassMessage cls' newSelector

-- | Check whether a given main-frame navigation URL matches the callback expected by the client app. Handles all URL-based callback strategies, including custom schemes and HTTPS navigations. This is mainly meant for web browsers adopting the ASWebAuthenticationWebBrowser API, but may also be useful for other apps for debugging purposes.
--
-- @url@ — The URL to check.
--
-- ObjC selector: @- matchesURL:@
matchesURL :: (IsASWebAuthenticationSessionCallback asWebAuthenticationSessionCallback, IsNSURL url) => asWebAuthenticationSessionCallback -> url -> IO Bool
matchesURL asWebAuthenticationSessionCallback url =
  sendMessage asWebAuthenticationSessionCallback matchesURLSelector (toNSURL url)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callbackWithCustomScheme:@
callbackWithCustomSchemeSelector :: Selector '[Id NSString] (Id ASWebAuthenticationSessionCallback)
callbackWithCustomSchemeSelector = mkSelector "callbackWithCustomScheme:"

-- | @Selector@ for @callbackWithHTTPSHost:path:@
callbackWithHTTPSHost_pathSelector :: Selector '[Id NSString, Id NSString] (Id ASWebAuthenticationSessionCallback)
callbackWithHTTPSHost_pathSelector = mkSelector "callbackWithHTTPSHost:path:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASWebAuthenticationSessionCallback)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASWebAuthenticationSessionCallback)
newSelector = mkSelector "new"

-- | @Selector@ for @matchesURL:@
matchesURLSelector :: Selector '[Id NSURL] Bool
matchesURLSelector = mkSelector "matchesURL:"

