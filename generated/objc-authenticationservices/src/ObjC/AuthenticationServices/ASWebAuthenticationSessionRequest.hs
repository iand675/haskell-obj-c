{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASWebAuthenticationSessionRequest@.
module ObjC.AuthenticationServices.ASWebAuthenticationSessionRequest
  ( ASWebAuthenticationSessionRequest
  , IsASWebAuthenticationSessionRequest(..)
  , new
  , init_
  , cancelWithError
  , completeWithCallbackURL
  , uuid
  , url
  , callbackURLScheme
  , shouldUseEphemeralSession
  , additionalHeaderFields
  , callback
  , newSelector
  , initSelector
  , cancelWithErrorSelector
  , completeWithCallbackURLSelector
  , uuidSelector
  , urlSelector
  , callbackURLSchemeSelector
  , shouldUseEphemeralSessionSelector
  , additionalHeaderFieldsSelector
  , callbackSelector


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

-- | @+ new@
new :: IO (Id ASWebAuthenticationSessionRequest)
new  =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO (Id ASWebAuthenticationSessionRequest)
init_ asWebAuthenticationSessionRequest  =
  sendMsg asWebAuthenticationSessionRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- cancelWithError:@
cancelWithError :: (IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest, IsNSError error_) => asWebAuthenticationSessionRequest -> error_ -> IO ()
cancelWithError asWebAuthenticationSessionRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg asWebAuthenticationSessionRequest (mkSelector "cancelWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- completeWithCallbackURL:@
completeWithCallbackURL :: (IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest, IsNSURL url) => asWebAuthenticationSessionRequest -> url -> IO ()
completeWithCallbackURL asWebAuthenticationSessionRequest  url =
withObjCPtr url $ \raw_url ->
    sendMsg asWebAuthenticationSessionRequest (mkSelector "completeWithCallbackURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | @- UUID@
uuid :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO (Id NSUUID)
uuid asWebAuthenticationSessionRequest  =
  sendMsg asWebAuthenticationSessionRequest (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO (Id NSURL)
url asWebAuthenticationSessionRequest  =
  sendMsg asWebAuthenticationSessionRequest (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callbackURLScheme@
callbackURLScheme :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO (Id NSString)
callbackURLScheme asWebAuthenticationSessionRequest  =
  sendMsg asWebAuthenticationSessionRequest (mkSelector "callbackURLScheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldUseEphemeralSession@
shouldUseEphemeralSession :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO Bool
shouldUseEphemeralSession asWebAuthenticationSessionRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asWebAuthenticationSessionRequest (mkSelector "shouldUseEphemeralSession") retCULong []

-- | Additional headers to be sent when loading the initial URL. These should _only_ apply to the initial page, and should not overwrite any headers normally sent by the browser. Add @AdditionalHeaderFieldsAreSupported: true@ to @ASWebAuthenticationSessionWebBrowserSupportCapabilities@ in your browser's Info.plist file to indicate support for this.
--
-- ObjC selector: @- additionalHeaderFields@
additionalHeaderFields :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO (Id NSDictionary)
additionalHeaderFields asWebAuthenticationSessionRequest  =
  sendMsg asWebAuthenticationSessionRequest (mkSelector "additionalHeaderFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The callback to listen for to complete this request. Check all main-frame navigations loaded during the request with this callback. It is used to handle all callback types, including custom schemes and HTTPS navigations. When a match is found, invoke @-completeWithCallbackURL:@ with that URL. Add @CallbackURLMatchingIsSupported: true@ to @ASWebAuthenticationSessionWebBrowserSupportCapabilities@ in your browser's Info.plist file to indicate support for this.
--
-- ObjC selector: @- callback@
callback :: IsASWebAuthenticationSessionRequest asWebAuthenticationSessionRequest => asWebAuthenticationSessionRequest -> IO (Id ASWebAuthenticationSessionCallback)
callback asWebAuthenticationSessionRequest  =
  sendMsg asWebAuthenticationSessionRequest (mkSelector "callback") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @cancelWithError:@
cancelWithErrorSelector :: Selector
cancelWithErrorSelector = mkSelector "cancelWithError:"

-- | @Selector@ for @completeWithCallbackURL:@
completeWithCallbackURLSelector :: Selector
completeWithCallbackURLSelector = mkSelector "completeWithCallbackURL:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @callbackURLScheme@
callbackURLSchemeSelector :: Selector
callbackURLSchemeSelector = mkSelector "callbackURLScheme"

-- | @Selector@ for @shouldUseEphemeralSession@
shouldUseEphemeralSessionSelector :: Selector
shouldUseEphemeralSessionSelector = mkSelector "shouldUseEphemeralSession"

-- | @Selector@ for @additionalHeaderFields@
additionalHeaderFieldsSelector :: Selector
additionalHeaderFieldsSelector = mkSelector "additionalHeaderFields"

-- | @Selector@ for @callback@
callbackSelector :: Selector
callbackSelector = mkSelector "callback"

