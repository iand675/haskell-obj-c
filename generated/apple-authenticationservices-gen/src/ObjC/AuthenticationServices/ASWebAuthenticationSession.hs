{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ASWebAuthenticationSession
--
-- An ASWebAuthenticationSession object can be used to authenticate a user with a web service, even if the web service is run by a third party. ASWebAuthenticationSession puts the user in control of whether they want to use their existing logged-in session from Safari. The app provides a URL that points to the authentication webpage. The page will be loaded in a secure view controller. From the webpage, the user can authenticate herself and grant access to the app. On completion, the service will send a callback URL with an authentication token, and this URL will be passed to the app by ASWebAuthenticationSessionCompletionHandler.
--
-- The callback URL usually has a custom URL scheme. For the app to receive the callback URL, it needs to either register the custom URL scheme in its Info.plist, or set the scheme to callbackURLScheme argument in the initializer.
--
-- If the user has already logged into the web service in Safari or other apps via ASWebAuthenticationSession, it is possible to share the existing login information. An alert will be presented to get the user's consent for sharing their existing login information. If the user cancels the alert, the session will be canceled, and the completion handler will be called with the error code ASWebAuthenticationSessionErrorCodeCanceledLogin.
--
-- If the user taps Cancel when showing the login webpage for the web service, the session will be canceled, and the completion handler will be called with the error code ASWebAuthenticationSessionErrorCodeCanceledLogin.
--
-- The app can cancel the session by calling -[ASWebAuthenticationSession cancel]. This will also dismiss the view controller that is showing the web service's login page.
--
-- Generated bindings for @ASWebAuthenticationSession@.
module ObjC.AuthenticationServices.ASWebAuthenticationSession
  ( ASWebAuthenticationSession
  , IsASWebAuthenticationSession(..)
  , initWithURL_callbackURLScheme_completionHandler
  , initWithURL_callback_completionHandler
  , start
  , cancel
  , new
  , init_
  , presentationContextProvider
  , setPresentationContextProvider
  , prefersEphemeralWebBrowserSession
  , setPrefersEphemeralWebBrowserSession
  , additionalHeaderFields
  , setAdditionalHeaderFields
  , canStart
  , initWithURL_callbackURLScheme_completionHandlerSelector
  , initWithURL_callback_completionHandlerSelector
  , startSelector
  , cancelSelector
  , newSelector
  , initSelector
  , presentationContextProviderSelector
  , setPresentationContextProviderSelector
  , prefersEphemeralWebBrowserSessionSelector
  , setPrefersEphemeralWebBrowserSessionSelector
  , additionalHeaderFieldsSelector
  , setAdditionalHeaderFieldsSelector
  , canStartSelector


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

-- | Returns an ASWebAuthenticationSession object.
--
-- @URL@ — the initial URL pointing to the authentication webpage. Only supports URLs with http:// or https:// schemes.
--
-- @callbackURLScheme@ — the custom URL scheme that the app expects in the callback URL.
--
-- @completionHandler@ — the completion handler which is called when the session is completed successfully or canceled by user.
--
-- ObjC selector: @- initWithURL:callbackURLScheme:completionHandler:@
initWithURL_callbackURLScheme_completionHandler :: (IsASWebAuthenticationSession asWebAuthenticationSession, IsNSURL url, IsNSString callbackURLScheme) => asWebAuthenticationSession -> url -> callbackURLScheme -> Ptr () -> IO (Id ASWebAuthenticationSession)
initWithURL_callbackURLScheme_completionHandler asWebAuthenticationSession  url callbackURLScheme completionHandler =
  withObjCPtr url $ \raw_url ->
    withObjCPtr callbackURLScheme $ \raw_callbackURLScheme ->
        sendMsg asWebAuthenticationSession (mkSelector "initWithURL:callbackURLScheme:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_callbackURLScheme :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithURL:callback:completionHandler:@
initWithURL_callback_completionHandler :: (IsASWebAuthenticationSession asWebAuthenticationSession, IsNSURL url, IsASWebAuthenticationSessionCallback callback) => asWebAuthenticationSession -> url -> callback -> Ptr () -> IO (Id ASWebAuthenticationSession)
initWithURL_callback_completionHandler asWebAuthenticationSession  url callback completionHandler =
  withObjCPtr url $ \raw_url ->
    withObjCPtr callback $ \raw_callback ->
        sendMsg asWebAuthenticationSession (mkSelector "initWithURL:callback:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_callback :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Starts the ASWebAuthenticationSession instance after it is instantiated.
--
-- start can only be called once for an ASWebAuthenticationSession instance. This also means calling start on a canceled session will fail.
--
-- Returns: Returns YES if the session starts successfully.
--
-- ObjC selector: @- start@
start :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO Bool
start asWebAuthenticationSession  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg asWebAuthenticationSession (mkSelector "start") retCULong []

-- | Cancel an ASWebAuthenticationSession. If the view controller is already presented to load the webpage for authentication, it will be dismissed. Calling cancel on an already canceled session will have no effect.
--
-- ObjC selector: @- cancel@
cancel :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO ()
cancel asWebAuthenticationSession  =
    sendMsg asWebAuthenticationSession (mkSelector "cancel") retVoid []

-- | @+ new@
new :: IO (Id ASWebAuthenticationSession)
new  =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO (Id ASWebAuthenticationSession)
init_ asWebAuthenticationSession  =
    sendMsg asWebAuthenticationSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Provides context to target where in an application's UI the authorization view should be shown. A provider must be set prior to calling -start, otherwise the authorization view cannot be displayed. If deploying to iOS prior to 13.0, the desired window is inferred by the application's key window.
--
-- ObjC selector: @- presentationContextProvider@
presentationContextProvider :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO RawId
presentationContextProvider asWebAuthenticationSession  =
    fmap (RawId . castPtr) $ sendMsg asWebAuthenticationSession (mkSelector "presentationContextProvider") (retPtr retVoid) []

-- | Provides context to target where in an application's UI the authorization view should be shown. A provider must be set prior to calling -start, otherwise the authorization view cannot be displayed. If deploying to iOS prior to 13.0, the desired window is inferred by the application's key window.
--
-- ObjC selector: @- setPresentationContextProvider:@
setPresentationContextProvider :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> RawId -> IO ()
setPresentationContextProvider asWebAuthenticationSession  value =
    sendMsg asWebAuthenticationSession (mkSelector "setPresentationContextProvider:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Indicates whether this session should ask the browser for an ephemeral session.
--
-- Ephemeral web browser sessions do not not share cookies or other browsing data with a user's normal browser session. This value is NO by default. Setting this property after calling -[ASWebAuthenticationSession start] has no effect.
--
-- ObjC selector: @- prefersEphemeralWebBrowserSession@
prefersEphemeralWebBrowserSession :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO Bool
prefersEphemeralWebBrowserSession asWebAuthenticationSession  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg asWebAuthenticationSession (mkSelector "prefersEphemeralWebBrowserSession") retCULong []

-- | Indicates whether this session should ask the browser for an ephemeral session.
--
-- Ephemeral web browser sessions do not not share cookies or other browsing data with a user's normal browser session. This value is NO by default. Setting this property after calling -[ASWebAuthenticationSession start] has no effect.
--
-- ObjC selector: @- setPrefersEphemeralWebBrowserSession:@
setPrefersEphemeralWebBrowserSession :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> Bool -> IO ()
setPrefersEphemeralWebBrowserSession asWebAuthenticationSession  value =
    sendMsg asWebAuthenticationSession (mkSelector "setPrefersEphemeralWebBrowserSession:") retVoid [argCULong (if value then 1 else 0)]

-- | Any additional header fields to be set when loading the initial URL. All header field names must start with the "X-" prefix.
--
-- ObjC selector: @- additionalHeaderFields@
additionalHeaderFields :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO (Id NSDictionary)
additionalHeaderFields asWebAuthenticationSession  =
    sendMsg asWebAuthenticationSession (mkSelector "additionalHeaderFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any additional header fields to be set when loading the initial URL. All header field names must start with the "X-" prefix.
--
-- ObjC selector: @- setAdditionalHeaderFields:@
setAdditionalHeaderFields :: (IsASWebAuthenticationSession asWebAuthenticationSession, IsNSDictionary value) => asWebAuthenticationSession -> value -> IO ()
setAdditionalHeaderFields asWebAuthenticationSession  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asWebAuthenticationSession (mkSelector "setAdditionalHeaderFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Returns whether the session can be successfully started. This property returns the same value as calling -start, but without the side effect of actually starting the session.
--
-- ObjC selector: @- canStart@
canStart :: IsASWebAuthenticationSession asWebAuthenticationSession => asWebAuthenticationSession -> IO Bool
canStart asWebAuthenticationSession  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg asWebAuthenticationSession (mkSelector "canStart") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:callbackURLScheme:completionHandler:@
initWithURL_callbackURLScheme_completionHandlerSelector :: Selector
initWithURL_callbackURLScheme_completionHandlerSelector = mkSelector "initWithURL:callbackURLScheme:completionHandler:"

-- | @Selector@ for @initWithURL:callback:completionHandler:@
initWithURL_callback_completionHandlerSelector :: Selector
initWithURL_callback_completionHandlerSelector = mkSelector "initWithURL:callback:completionHandler:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @presentationContextProvider@
presentationContextProviderSelector :: Selector
presentationContextProviderSelector = mkSelector "presentationContextProvider"

-- | @Selector@ for @setPresentationContextProvider:@
setPresentationContextProviderSelector :: Selector
setPresentationContextProviderSelector = mkSelector "setPresentationContextProvider:"

-- | @Selector@ for @prefersEphemeralWebBrowserSession@
prefersEphemeralWebBrowserSessionSelector :: Selector
prefersEphemeralWebBrowserSessionSelector = mkSelector "prefersEphemeralWebBrowserSession"

-- | @Selector@ for @setPrefersEphemeralWebBrowserSession:@
setPrefersEphemeralWebBrowserSessionSelector :: Selector
setPrefersEphemeralWebBrowserSessionSelector = mkSelector "setPrefersEphemeralWebBrowserSession:"

-- | @Selector@ for @additionalHeaderFields@
additionalHeaderFieldsSelector :: Selector
additionalHeaderFieldsSelector = mkSelector "additionalHeaderFields"

-- | @Selector@ for @setAdditionalHeaderFields:@
setAdditionalHeaderFieldsSelector :: Selector
setAdditionalHeaderFieldsSelector = mkSelector "setAdditionalHeaderFields:"

-- | @Selector@ for @canStart@
canStartSelector :: Selector
canStartSelector = mkSelector "canStart"

