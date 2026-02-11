{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKWebViewConfiguration object is a collection of properties with which to initialize a web view.
--
-- Contains properties used to configure a
--
-- WKWebView
--
-- .
--
-- Generated bindings for @WKWebViewConfiguration@.
module ObjC.WebKit.WKWebViewConfiguration
  ( WKWebViewConfiguration
  , IsWKWebViewConfiguration(..)
  , setURLSchemeHandler_forURLScheme
  , urlSchemeHandlerForURLScheme
  , processPool
  , setProcessPool
  , preferences
  , setPreferences
  , userContentController
  , setUserContentController
  , webExtensionController
  , setWebExtensionController
  , websiteDataStore
  , setWebsiteDataStore
  , suppressesIncrementalRendering
  , setSuppressesIncrementalRendering
  , applicationNameForUserAgent
  , setApplicationNameForUserAgent
  , allowsAirPlayForMediaPlayback
  , setAllowsAirPlayForMediaPlayback
  , showsSystemScreenTimeBlockingView
  , setShowsSystemScreenTimeBlockingView
  , upgradeKnownHostsToHTTPS
  , setUpgradeKnownHostsToHTTPS
  , mediaTypesRequiringUserActionForPlayback
  , setMediaTypesRequiringUserActionForPlayback
  , defaultWebpagePreferences
  , setDefaultWebpagePreferences
  , limitsNavigationsToAppBoundDomains
  , setLimitsNavigationsToAppBoundDomains
  , allowsInlinePredictions
  , setAllowsInlinePredictions
  , userInterfaceDirectionPolicy
  , setUserInterfaceDirectionPolicy
  , supportsAdaptiveImageGlyph
  , setSupportsAdaptiveImageGlyph
  , writingToolsBehavior
  , setWritingToolsBehavior
  , setURLSchemeHandler_forURLSchemeSelector
  , urlSchemeHandlerForURLSchemeSelector
  , processPoolSelector
  , setProcessPoolSelector
  , preferencesSelector
  , setPreferencesSelector
  , userContentControllerSelector
  , setUserContentControllerSelector
  , webExtensionControllerSelector
  , setWebExtensionControllerSelector
  , websiteDataStoreSelector
  , setWebsiteDataStoreSelector
  , suppressesIncrementalRenderingSelector
  , setSuppressesIncrementalRenderingSelector
  , applicationNameForUserAgentSelector
  , setApplicationNameForUserAgentSelector
  , allowsAirPlayForMediaPlaybackSelector
  , setAllowsAirPlayForMediaPlaybackSelector
  , showsSystemScreenTimeBlockingViewSelector
  , setShowsSystemScreenTimeBlockingViewSelector
  , upgradeKnownHostsToHTTPSSelector
  , setUpgradeKnownHostsToHTTPSSelector
  , mediaTypesRequiringUserActionForPlaybackSelector
  , setMediaTypesRequiringUserActionForPlaybackSelector
  , defaultWebpagePreferencesSelector
  , setDefaultWebpagePreferencesSelector
  , limitsNavigationsToAppBoundDomainsSelector
  , setLimitsNavigationsToAppBoundDomainsSelector
  , allowsInlinePredictionsSelector
  , setAllowsInlinePredictionsSelector
  , userInterfaceDirectionPolicySelector
  , setUserInterfaceDirectionPolicySelector
  , supportsAdaptiveImageGlyphSelector
  , setSupportsAdaptiveImageGlyphSelector
  , writingToolsBehaviorSelector
  , setWritingToolsBehaviorSelector

  -- * Enum types
  , NSWritingToolsBehavior(NSWritingToolsBehavior)
  , pattern NSWritingToolsBehaviorNone
  , pattern NSWritingToolsBehaviorDefault
  , pattern NSWritingToolsBehaviorComplete
  , pattern NSWritingToolsBehaviorLimited
  , WKAudiovisualMediaTypes(WKAudiovisualMediaTypes)
  , pattern WKAudiovisualMediaTypeNone
  , pattern WKAudiovisualMediaTypeAudio
  , pattern WKAudiovisualMediaTypeVideo
  , pattern WKAudiovisualMediaTypeAll
  , WKUserInterfaceDirectionPolicy(WKUserInterfaceDirectionPolicy)
  , pattern WKUserInterfaceDirectionPolicyContent
  , pattern WKUserInterfaceDirectionPolicySystem

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

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- setURLSchemeHandler:forURLScheme:@
setURLSchemeHandler_forURLScheme :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsNSString urlScheme) => wkWebViewConfiguration -> RawId -> urlScheme -> IO ()
setURLSchemeHandler_forURLScheme wkWebViewConfiguration  urlSchemeHandler urlScheme =
withObjCPtr urlScheme $ \raw_urlScheme ->
    sendMsg wkWebViewConfiguration (mkSelector "setURLSchemeHandler:forURLScheme:") retVoid [argPtr (castPtr (unRawId urlSchemeHandler) :: Ptr ()), argPtr (castPtr raw_urlScheme :: Ptr ())]

-- | @- urlSchemeHandlerForURLScheme:@
urlSchemeHandlerForURLScheme :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsNSString urlScheme) => wkWebViewConfiguration -> urlScheme -> IO RawId
urlSchemeHandlerForURLScheme wkWebViewConfiguration  urlScheme =
withObjCPtr urlScheme $ \raw_urlScheme ->
    fmap (RawId . castPtr) $ sendMsg wkWebViewConfiguration (mkSelector "urlSchemeHandlerForURLScheme:") (retPtr retVoid) [argPtr (castPtr raw_urlScheme :: Ptr ())]

-- | The process pool from which to obtain the view's web content process.
--
-- When a web view is initialized, a new web content process will be created for it from the specified pool, or an existing process in that pool will be used.
--
-- ObjC selector: @- processPool@
processPool :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id WKProcessPool)
processPool wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "processPool") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The process pool from which to obtain the view's web content process.
--
-- When a web view is initialized, a new web content process will be created for it from the specified pool, or an existing process in that pool will be used.
--
-- ObjC selector: @- setProcessPool:@
setProcessPool :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsWKProcessPool value) => wkWebViewConfiguration -> value -> IO ()
setProcessPool wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setProcessPool:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The preference settings to be used by the web view.
--
-- ObjC selector: @- preferences@
preferences :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id WKPreferences)
preferences wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "preferences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The preference settings to be used by the web view.
--
-- ObjC selector: @- setPreferences:@
setPreferences :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsWKPreferences value) => wkWebViewConfiguration -> value -> IO ()
setPreferences wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setPreferences:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The user content controller to associate with the web view.
--
-- ObjC selector: @- userContentController@
userContentController :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id WKUserContentController)
userContentController wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "userContentController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user content controller to associate with the web view.
--
-- ObjC selector: @- setUserContentController:@
setUserContentController :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsWKUserContentController value) => wkWebViewConfiguration -> value -> IO ()
setUserContentController wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setUserContentController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The web extension controller to associate with the web view.
--
-- ObjC selector: @- webExtensionController@
webExtensionController :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id WKWebExtensionController)
webExtensionController wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "webExtensionController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The web extension controller to associate with the web view.
--
-- ObjC selector: @- setWebExtensionController:@
setWebExtensionController :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsWKWebExtensionController value) => wkWebViewConfiguration -> value -> IO ()
setWebExtensionController wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setWebExtensionController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The website data store to be used by the web view.
--
-- ObjC selector: @- websiteDataStore@
websiteDataStore :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id WKWebsiteDataStore)
websiteDataStore wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "websiteDataStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The website data store to be used by the web view.
--
-- ObjC selector: @- setWebsiteDataStore:@
setWebsiteDataStore :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsWKWebsiteDataStore value) => wkWebViewConfiguration -> value -> IO ()
setWebsiteDataStore wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setWebsiteDataStore:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating whether the web view suppresses content rendering until it is fully loaded into memory.
--
-- The default value is NO.
--
-- ObjC selector: @- suppressesIncrementalRendering@
suppressesIncrementalRendering :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
suppressesIncrementalRendering wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "suppressesIncrementalRendering") retCULong []

-- | A Boolean value indicating whether the web view suppresses content rendering until it is fully loaded into memory.
--
-- The default value is NO.
--
-- ObjC selector: @- setSuppressesIncrementalRendering:@
setSuppressesIncrementalRendering :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setSuppressesIncrementalRendering wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setSuppressesIncrementalRendering:") retVoid [argCULong (if value then 1 else 0)]

-- | The name of the application as used in the user agent string.
--
-- ObjC selector: @- applicationNameForUserAgent@
applicationNameForUserAgent :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id NSString)
applicationNameForUserAgent wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "applicationNameForUserAgent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the application as used in the user agent string.
--
-- ObjC selector: @- setApplicationNameForUserAgent:@
setApplicationNameForUserAgent :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsNSString value) => wkWebViewConfiguration -> value -> IO ()
setApplicationNameForUserAgent wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setApplicationNameForUserAgent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating whether AirPlay is allowed.
--
-- The default value is YES.
--
-- ObjC selector: @- allowsAirPlayForMediaPlayback@
allowsAirPlayForMediaPlayback :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
allowsAirPlayForMediaPlayback wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "allowsAirPlayForMediaPlayback") retCULong []

-- | A Boolean value indicating whether AirPlay is allowed.
--
-- The default value is YES.
--
-- ObjC selector: @- setAllowsAirPlayForMediaPlayback:@
setAllowsAirPlayForMediaPlayback :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setAllowsAirPlayForMediaPlayback wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setAllowsAirPlayForMediaPlayback:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether the System Screen Time blocking view should be shown.
--
-- The default value is YES.
--
-- ObjC selector: @- showsSystemScreenTimeBlockingView@
showsSystemScreenTimeBlockingView :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
showsSystemScreenTimeBlockingView wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "showsSystemScreenTimeBlockingView") retCULong []

-- | A Boolean value indicating whether the System Screen Time blocking view should be shown.
--
-- The default value is YES.
--
-- ObjC selector: @- setShowsSystemScreenTimeBlockingView:@
setShowsSystemScreenTimeBlockingView :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setShowsSystemScreenTimeBlockingView wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setShowsSystemScreenTimeBlockingView:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether HTTP requests to servers known to support HTTPS should be automatically upgraded to HTTPS requests.
--
-- The default value is YES.
--
-- ObjC selector: @- upgradeKnownHostsToHTTPS@
upgradeKnownHostsToHTTPS :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
upgradeKnownHostsToHTTPS wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "upgradeKnownHostsToHTTPS") retCULong []

-- | A Boolean value indicating whether HTTP requests to servers known to support HTTPS should be automatically upgraded to HTTPS requests.
--
-- The default value is YES.
--
-- ObjC selector: @- setUpgradeKnownHostsToHTTPS:@
setUpgradeKnownHostsToHTTPS :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setUpgradeKnownHostsToHTTPS wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setUpgradeKnownHostsToHTTPS:") retVoid [argCULong (if value then 1 else 0)]

-- | @- mediaTypesRequiringUserActionForPlayback@
mediaTypesRequiringUserActionForPlayback :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO WKAudiovisualMediaTypes
mediaTypesRequiringUserActionForPlayback wkWebViewConfiguration  =
  fmap (coerce :: CULong -> WKAudiovisualMediaTypes) $ sendMsg wkWebViewConfiguration (mkSelector "mediaTypesRequiringUserActionForPlayback") retCULong []

-- | @- setMediaTypesRequiringUserActionForPlayback:@
setMediaTypesRequiringUserActionForPlayback :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> WKAudiovisualMediaTypes -> IO ()
setMediaTypesRequiringUserActionForPlayback wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setMediaTypesRequiringUserActionForPlayback:") retVoid [argCULong (coerce value)]

-- | The set of default webpage preferences to use when loading and rendering content.
--
-- These default webpage preferences are additionally passed to the navigation delegate in -webView:decidePolicyForNavigationAction:preferences:decisionHandler:.
--
-- ObjC selector: @- defaultWebpagePreferences@
defaultWebpagePreferences :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO (Id WKWebpagePreferences)
defaultWebpagePreferences wkWebViewConfiguration  =
  sendMsg wkWebViewConfiguration (mkSelector "defaultWebpagePreferences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of default webpage preferences to use when loading and rendering content.
--
-- These default webpage preferences are additionally passed to the navigation delegate in -webView:decidePolicyForNavigationAction:preferences:decisionHandler:.
--
-- ObjC selector: @- setDefaultWebpagePreferences:@
setDefaultWebpagePreferences :: (IsWKWebViewConfiguration wkWebViewConfiguration, IsWKWebpagePreferences value) => wkWebViewConfiguration -> value -> IO ()
setDefaultWebpagePreferences wkWebViewConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebViewConfiguration (mkSelector "setDefaultWebpagePreferences:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- limitsNavigationsToAppBoundDomains@
limitsNavigationsToAppBoundDomains :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
limitsNavigationsToAppBoundDomains wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "limitsNavigationsToAppBoundDomains") retCULong []

-- | @- setLimitsNavigationsToAppBoundDomains:@
setLimitsNavigationsToAppBoundDomains :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setLimitsNavigationsToAppBoundDomains wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setLimitsNavigationsToAppBoundDomains:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether inline predictions are allowed.
--
-- The default value is @NO@. If false, inline predictionsare disabled regardless of the system setting. If true, they are enabled basedon the system setting.
--
-- ObjC selector: @- allowsInlinePredictions@
allowsInlinePredictions :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
allowsInlinePredictions wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "allowsInlinePredictions") retCULong []

-- | A Boolean value indicating whether inline predictions are allowed.
--
-- The default value is @NO@. If false, inline predictionsare disabled regardless of the system setting. If true, they are enabled basedon the system setting.
--
-- ObjC selector: @- setAllowsInlinePredictions:@
setAllowsInlinePredictions :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setAllowsInlinePredictions wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setAllowsInlinePredictions:") retVoid [argCULong (if value then 1 else 0)]

-- | The directionality of user interface elements.
--
-- Possible values are described in WKUserInterfaceDirectionPolicy. The default value is WKUserInterfaceDirectionPolicyContent.
--
-- ObjC selector: @- userInterfaceDirectionPolicy@
userInterfaceDirectionPolicy :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO WKUserInterfaceDirectionPolicy
userInterfaceDirectionPolicy wkWebViewConfiguration  =
  fmap (coerce :: CLong -> WKUserInterfaceDirectionPolicy) $ sendMsg wkWebViewConfiguration (mkSelector "userInterfaceDirectionPolicy") retCLong []

-- | The directionality of user interface elements.
--
-- Possible values are described in WKUserInterfaceDirectionPolicy. The default value is WKUserInterfaceDirectionPolicyContent.
--
-- ObjC selector: @- setUserInterfaceDirectionPolicy:@
setUserInterfaceDirectionPolicy :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> WKUserInterfaceDirectionPolicy -> IO ()
setUserInterfaceDirectionPolicy wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setUserInterfaceDirectionPolicy:") retVoid [argCLong (coerce value)]

-- | A Boolean value indicating whether insertion of adaptive image glyphs is allowed.
--
-- The default value is @NO@. If @NO@, adaptive image glyphs are inserted as regular    images. If @YES@, they are inserted with the full adaptive sizing behavior.
--
-- ObjC selector: @- supportsAdaptiveImageGlyph@
supportsAdaptiveImageGlyph :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO Bool
supportsAdaptiveImageGlyph wkWebViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebViewConfiguration (mkSelector "supportsAdaptiveImageGlyph") retCULong []

-- | A Boolean value indicating whether insertion of adaptive image glyphs is allowed.
--
-- The default value is @NO@. If @NO@, adaptive image glyphs are inserted as regular    images. If @YES@, they are inserted with the full adaptive sizing behavior.
--
-- ObjC selector: @- setSupportsAdaptiveImageGlyph:@
setSupportsAdaptiveImageGlyph :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> Bool -> IO ()
setSupportsAdaptiveImageGlyph wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setSupportsAdaptiveImageGlyph:") retVoid [argCULong (if value then 1 else 0)]

-- | The preferred behavior of Writing Tools.
--
-- The default behavior is equivalent to @NSWritingToolsBehaviorLimited@.
--
-- ObjC selector: @- writingToolsBehavior@
writingToolsBehavior :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> IO NSWritingToolsBehavior
writingToolsBehavior wkWebViewConfiguration  =
  fmap (coerce :: CLong -> NSWritingToolsBehavior) $ sendMsg wkWebViewConfiguration (mkSelector "writingToolsBehavior") retCLong []

-- | The preferred behavior of Writing Tools.
--
-- The default behavior is equivalent to @NSWritingToolsBehaviorLimited@.
--
-- ObjC selector: @- setWritingToolsBehavior:@
setWritingToolsBehavior :: IsWKWebViewConfiguration wkWebViewConfiguration => wkWebViewConfiguration -> NSWritingToolsBehavior -> IO ()
setWritingToolsBehavior wkWebViewConfiguration  value =
  sendMsg wkWebViewConfiguration (mkSelector "setWritingToolsBehavior:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setURLSchemeHandler:forURLScheme:@
setURLSchemeHandler_forURLSchemeSelector :: Selector
setURLSchemeHandler_forURLSchemeSelector = mkSelector "setURLSchemeHandler:forURLScheme:"

-- | @Selector@ for @urlSchemeHandlerForURLScheme:@
urlSchemeHandlerForURLSchemeSelector :: Selector
urlSchemeHandlerForURLSchemeSelector = mkSelector "urlSchemeHandlerForURLScheme:"

-- | @Selector@ for @processPool@
processPoolSelector :: Selector
processPoolSelector = mkSelector "processPool"

-- | @Selector@ for @setProcessPool:@
setProcessPoolSelector :: Selector
setProcessPoolSelector = mkSelector "setProcessPool:"

-- | @Selector@ for @preferences@
preferencesSelector :: Selector
preferencesSelector = mkSelector "preferences"

-- | @Selector@ for @setPreferences:@
setPreferencesSelector :: Selector
setPreferencesSelector = mkSelector "setPreferences:"

-- | @Selector@ for @userContentController@
userContentControllerSelector :: Selector
userContentControllerSelector = mkSelector "userContentController"

-- | @Selector@ for @setUserContentController:@
setUserContentControllerSelector :: Selector
setUserContentControllerSelector = mkSelector "setUserContentController:"

-- | @Selector@ for @webExtensionController@
webExtensionControllerSelector :: Selector
webExtensionControllerSelector = mkSelector "webExtensionController"

-- | @Selector@ for @setWebExtensionController:@
setWebExtensionControllerSelector :: Selector
setWebExtensionControllerSelector = mkSelector "setWebExtensionController:"

-- | @Selector@ for @websiteDataStore@
websiteDataStoreSelector :: Selector
websiteDataStoreSelector = mkSelector "websiteDataStore"

-- | @Selector@ for @setWebsiteDataStore:@
setWebsiteDataStoreSelector :: Selector
setWebsiteDataStoreSelector = mkSelector "setWebsiteDataStore:"

-- | @Selector@ for @suppressesIncrementalRendering@
suppressesIncrementalRenderingSelector :: Selector
suppressesIncrementalRenderingSelector = mkSelector "suppressesIncrementalRendering"

-- | @Selector@ for @setSuppressesIncrementalRendering:@
setSuppressesIncrementalRenderingSelector :: Selector
setSuppressesIncrementalRenderingSelector = mkSelector "setSuppressesIncrementalRendering:"

-- | @Selector@ for @applicationNameForUserAgent@
applicationNameForUserAgentSelector :: Selector
applicationNameForUserAgentSelector = mkSelector "applicationNameForUserAgent"

-- | @Selector@ for @setApplicationNameForUserAgent:@
setApplicationNameForUserAgentSelector :: Selector
setApplicationNameForUserAgentSelector = mkSelector "setApplicationNameForUserAgent:"

-- | @Selector@ for @allowsAirPlayForMediaPlayback@
allowsAirPlayForMediaPlaybackSelector :: Selector
allowsAirPlayForMediaPlaybackSelector = mkSelector "allowsAirPlayForMediaPlayback"

-- | @Selector@ for @setAllowsAirPlayForMediaPlayback:@
setAllowsAirPlayForMediaPlaybackSelector :: Selector
setAllowsAirPlayForMediaPlaybackSelector = mkSelector "setAllowsAirPlayForMediaPlayback:"

-- | @Selector@ for @showsSystemScreenTimeBlockingView@
showsSystemScreenTimeBlockingViewSelector :: Selector
showsSystemScreenTimeBlockingViewSelector = mkSelector "showsSystemScreenTimeBlockingView"

-- | @Selector@ for @setShowsSystemScreenTimeBlockingView:@
setShowsSystemScreenTimeBlockingViewSelector :: Selector
setShowsSystemScreenTimeBlockingViewSelector = mkSelector "setShowsSystemScreenTimeBlockingView:"

-- | @Selector@ for @upgradeKnownHostsToHTTPS@
upgradeKnownHostsToHTTPSSelector :: Selector
upgradeKnownHostsToHTTPSSelector = mkSelector "upgradeKnownHostsToHTTPS"

-- | @Selector@ for @setUpgradeKnownHostsToHTTPS:@
setUpgradeKnownHostsToHTTPSSelector :: Selector
setUpgradeKnownHostsToHTTPSSelector = mkSelector "setUpgradeKnownHostsToHTTPS:"

-- | @Selector@ for @mediaTypesRequiringUserActionForPlayback@
mediaTypesRequiringUserActionForPlaybackSelector :: Selector
mediaTypesRequiringUserActionForPlaybackSelector = mkSelector "mediaTypesRequiringUserActionForPlayback"

-- | @Selector@ for @setMediaTypesRequiringUserActionForPlayback:@
setMediaTypesRequiringUserActionForPlaybackSelector :: Selector
setMediaTypesRequiringUserActionForPlaybackSelector = mkSelector "setMediaTypesRequiringUserActionForPlayback:"

-- | @Selector@ for @defaultWebpagePreferences@
defaultWebpagePreferencesSelector :: Selector
defaultWebpagePreferencesSelector = mkSelector "defaultWebpagePreferences"

-- | @Selector@ for @setDefaultWebpagePreferences:@
setDefaultWebpagePreferencesSelector :: Selector
setDefaultWebpagePreferencesSelector = mkSelector "setDefaultWebpagePreferences:"

-- | @Selector@ for @limitsNavigationsToAppBoundDomains@
limitsNavigationsToAppBoundDomainsSelector :: Selector
limitsNavigationsToAppBoundDomainsSelector = mkSelector "limitsNavigationsToAppBoundDomains"

-- | @Selector@ for @setLimitsNavigationsToAppBoundDomains:@
setLimitsNavigationsToAppBoundDomainsSelector :: Selector
setLimitsNavigationsToAppBoundDomainsSelector = mkSelector "setLimitsNavigationsToAppBoundDomains:"

-- | @Selector@ for @allowsInlinePredictions@
allowsInlinePredictionsSelector :: Selector
allowsInlinePredictionsSelector = mkSelector "allowsInlinePredictions"

-- | @Selector@ for @setAllowsInlinePredictions:@
setAllowsInlinePredictionsSelector :: Selector
setAllowsInlinePredictionsSelector = mkSelector "setAllowsInlinePredictions:"

-- | @Selector@ for @userInterfaceDirectionPolicy@
userInterfaceDirectionPolicySelector :: Selector
userInterfaceDirectionPolicySelector = mkSelector "userInterfaceDirectionPolicy"

-- | @Selector@ for @setUserInterfaceDirectionPolicy:@
setUserInterfaceDirectionPolicySelector :: Selector
setUserInterfaceDirectionPolicySelector = mkSelector "setUserInterfaceDirectionPolicy:"

-- | @Selector@ for @supportsAdaptiveImageGlyph@
supportsAdaptiveImageGlyphSelector :: Selector
supportsAdaptiveImageGlyphSelector = mkSelector "supportsAdaptiveImageGlyph"

-- | @Selector@ for @setSupportsAdaptiveImageGlyph:@
setSupportsAdaptiveImageGlyphSelector :: Selector
setSupportsAdaptiveImageGlyphSelector = mkSelector "setSupportsAdaptiveImageGlyph:"

-- | @Selector@ for @writingToolsBehavior@
writingToolsBehaviorSelector :: Selector
writingToolsBehaviorSelector = mkSelector "writingToolsBehavior"

-- | @Selector@ for @setWritingToolsBehavior:@
setWritingToolsBehaviorSelector :: Selector
setWritingToolsBehaviorSelector = mkSelector "setWritingToolsBehavior:"

