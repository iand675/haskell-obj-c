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
  , newSelector
  , matchesURLSelector


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

-- | Creates a callback object that matches against URLs with the given custom scheme.
--
-- @customScheme@ — The custom scheme that the app expects in the callback URL.
--
-- ObjC selector: @+ callbackWithCustomScheme:@
callbackWithCustomScheme :: IsNSString customScheme => customScheme -> IO (Id ASWebAuthenticationSessionCallback)
callbackWithCustomScheme customScheme =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionCallback"
    withObjCPtr customScheme $ \raw_customScheme ->
      sendClassMsg cls' (mkSelector "callbackWithCustomScheme:") (retPtr retVoid) [argPtr (castPtr raw_customScheme :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr host $ \raw_host ->
      withObjCPtr path $ \raw_path ->
        sendClassMsg cls' (mkSelector "callbackWithHTTPSHost:path:") (retPtr retVoid) [argPtr (castPtr raw_host :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsASWebAuthenticationSessionCallback asWebAuthenticationSessionCallback => asWebAuthenticationSessionCallback -> IO (Id ASWebAuthenticationSessionCallback)
init_ asWebAuthenticationSessionCallback  =
  sendMsg asWebAuthenticationSessionCallback (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASWebAuthenticationSessionCallback)
new  =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionCallback"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Check whether a given main-frame navigation URL matches the callback expected by the client app. Handles all URL-based callback strategies, including custom schemes and HTTPS navigations. This is mainly meant for web browsers adopting the ASWebAuthenticationWebBrowser API, but may also be useful for other apps for debugging purposes.
--
-- @url@ — The URL to check.
--
-- ObjC selector: @- matchesURL:@
matchesURL :: (IsASWebAuthenticationSessionCallback asWebAuthenticationSessionCallback, IsNSURL url) => asWebAuthenticationSessionCallback -> url -> IO Bool
matchesURL asWebAuthenticationSessionCallback  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg asWebAuthenticationSessionCallback (mkSelector "matchesURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callbackWithCustomScheme:@
callbackWithCustomSchemeSelector :: Selector
callbackWithCustomSchemeSelector = mkSelector "callbackWithCustomScheme:"

-- | @Selector@ for @callbackWithHTTPSHost:path:@
callbackWithHTTPSHost_pathSelector :: Selector
callbackWithHTTPSHost_pathSelector = mkSelector "callbackWithHTTPSHost:path:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @matchesURL:@
matchesURLSelector :: Selector
matchesURLSelector = mkSelector "matchesURL:"

