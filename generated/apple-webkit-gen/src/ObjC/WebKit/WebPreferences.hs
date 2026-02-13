{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebPreferences
--
-- Generated bindings for @WebPreferences@.
module ObjC.WebKit.WebPreferences
  ( WebPreferences
  , IsWebPreferences(..)
  , standardPreferences
  , initWithIdentifier
  , identifier
  , standardFontFamily
  , setStandardFontFamily
  , fixedFontFamily
  , setFixedFontFamily
  , serifFontFamily
  , setSerifFontFamily
  , sansSerifFontFamily
  , setSansSerifFontFamily
  , cursiveFontFamily
  , setCursiveFontFamily
  , fantasyFontFamily
  , setFantasyFontFamily
  , defaultFontSize
  , setDefaultFontSize
  , defaultFixedFontSize
  , setDefaultFixedFontSize
  , minimumFontSize
  , setMinimumFontSize
  , minimumLogicalFontSize
  , setMinimumLogicalFontSize
  , defaultTextEncodingName
  , setDefaultTextEncodingName
  , userStyleSheetEnabled
  , setUserStyleSheetEnabled
  , userStyleSheetLocation
  , setUserStyleSheetLocation
  , javaEnabled
  , setJavaEnabled
  , javaScriptEnabled
  , setJavaScriptEnabled
  , javaScriptCanOpenWindowsAutomatically
  , setJavaScriptCanOpenWindowsAutomatically
  , plugInsEnabled
  , setPlugInsEnabled
  , allowsAnimatedImages
  , setAllowsAnimatedImages
  , allowsAnimatedImageLooping
  , setAllowsAnimatedImageLooping
  , loadsImagesAutomatically
  , setLoadsImagesAutomatically
  , autosaves
  , setAutosaves
  , shouldPrintBackgrounds
  , setShouldPrintBackgrounds
  , privateBrowsingEnabled
  , setPrivateBrowsingEnabled
  , tabsToLinks
  , setTabsToLinks
  , usesPageCache
  , setUsesPageCache
  , cacheModel
  , setCacheModel
  , suppressesIncrementalRendering
  , setSuppressesIncrementalRendering
  , allowsAirPlayForMediaPlayback
  , setAllowsAirPlayForMediaPlayback
  , allowsAirPlayForMediaPlaybackSelector
  , allowsAnimatedImageLoopingSelector
  , allowsAnimatedImagesSelector
  , autosavesSelector
  , cacheModelSelector
  , cursiveFontFamilySelector
  , defaultFixedFontSizeSelector
  , defaultFontSizeSelector
  , defaultTextEncodingNameSelector
  , fantasyFontFamilySelector
  , fixedFontFamilySelector
  , identifierSelector
  , initWithIdentifierSelector
  , javaEnabledSelector
  , javaScriptCanOpenWindowsAutomaticallySelector
  , javaScriptEnabledSelector
  , loadsImagesAutomaticallySelector
  , minimumFontSizeSelector
  , minimumLogicalFontSizeSelector
  , plugInsEnabledSelector
  , privateBrowsingEnabledSelector
  , sansSerifFontFamilySelector
  , serifFontFamilySelector
  , setAllowsAirPlayForMediaPlaybackSelector
  , setAllowsAnimatedImageLoopingSelector
  , setAllowsAnimatedImagesSelector
  , setAutosavesSelector
  , setCacheModelSelector
  , setCursiveFontFamilySelector
  , setDefaultFixedFontSizeSelector
  , setDefaultFontSizeSelector
  , setDefaultTextEncodingNameSelector
  , setFantasyFontFamilySelector
  , setFixedFontFamilySelector
  , setJavaEnabledSelector
  , setJavaScriptCanOpenWindowsAutomaticallySelector
  , setJavaScriptEnabledSelector
  , setLoadsImagesAutomaticallySelector
  , setMinimumFontSizeSelector
  , setMinimumLogicalFontSizeSelector
  , setPlugInsEnabledSelector
  , setPrivateBrowsingEnabledSelector
  , setSansSerifFontFamilySelector
  , setSerifFontFamilySelector
  , setShouldPrintBackgroundsSelector
  , setStandardFontFamilySelector
  , setSuppressesIncrementalRenderingSelector
  , setTabsToLinksSelector
  , setUserStyleSheetEnabledSelector
  , setUserStyleSheetLocationSelector
  , setUsesPageCacheSelector
  , shouldPrintBackgroundsSelector
  , standardFontFamilySelector
  , standardPreferencesSelector
  , suppressesIncrementalRenderingSelector
  , tabsToLinksSelector
  , userStyleSheetEnabledSelector
  , userStyleSheetLocationSelector
  , usesPageCacheSelector

  -- * Enum types
  , WebCacheModel(WebCacheModel)
  , pattern WebCacheModelDocumentViewer
  , pattern WebCacheModelDocumentBrowser
  , pattern WebCacheModelPrimaryWebBrowser

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | standardPreferences
--
-- ObjC selector: @+ standardPreferences@
standardPreferences :: IO (Id WebPreferences)
standardPreferences  =
  do
    cls' <- getRequiredClass "WebPreferences"
    sendClassMessage cls' standardPreferencesSelector

-- | initWithIdentifier:
--
-- @anIdentifier@ — A string used to identify the WebPreferences.
--
-- WebViews can share instances of WebPreferences by using an instance of WebPreferences with    the same identifier.  Typically, instance are not created directly.  Instead you set the preferences    identifier on a WebView.  The identifier is used as a prefix that is added to the user defaults keys    for the WebPreferences.
--
-- Returns: Returns a new instance of WebPreferences or a previously allocated instance with the same identifier.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsWebPreferences webPreferences, IsNSString anIdentifier) => webPreferences -> anIdentifier -> IO (Id WebPreferences)
initWithIdentifier webPreferences anIdentifier =
  sendOwnedMessage webPreferences initWithIdentifierSelector (toNSString anIdentifier)

-- | identifier
--
-- Returns: Returns the identifier for this WebPreferences.
--
-- ObjC selector: @- identifier@
identifier :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
identifier webPreferences =
  sendMessage webPreferences identifierSelector

-- | standardFontFamily
--
-- ObjC selector: @- standardFontFamily@
standardFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
standardFontFamily webPreferences =
  sendMessage webPreferences standardFontFamilySelector

-- | standardFontFamily
--
-- ObjC selector: @- setStandardFontFamily:@
setStandardFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setStandardFontFamily webPreferences value =
  sendMessage webPreferences setStandardFontFamilySelector (toNSString value)

-- | fixedFontFamily
--
-- ObjC selector: @- fixedFontFamily@
fixedFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
fixedFontFamily webPreferences =
  sendMessage webPreferences fixedFontFamilySelector

-- | fixedFontFamily
--
-- ObjC selector: @- setFixedFontFamily:@
setFixedFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setFixedFontFamily webPreferences value =
  sendMessage webPreferences setFixedFontFamilySelector (toNSString value)

-- | serifFontFamily
--
-- ObjC selector: @- serifFontFamily@
serifFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
serifFontFamily webPreferences =
  sendMessage webPreferences serifFontFamilySelector

-- | serifFontFamily
--
-- ObjC selector: @- setSerifFontFamily:@
setSerifFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setSerifFontFamily webPreferences value =
  sendMessage webPreferences setSerifFontFamilySelector (toNSString value)

-- | sansSerifFontFamily
--
-- ObjC selector: @- sansSerifFontFamily@
sansSerifFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
sansSerifFontFamily webPreferences =
  sendMessage webPreferences sansSerifFontFamilySelector

-- | sansSerifFontFamily
--
-- ObjC selector: @- setSansSerifFontFamily:@
setSansSerifFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setSansSerifFontFamily webPreferences value =
  sendMessage webPreferences setSansSerifFontFamilySelector (toNSString value)

-- | cursiveFontFamily
--
-- ObjC selector: @- cursiveFontFamily@
cursiveFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
cursiveFontFamily webPreferences =
  sendMessage webPreferences cursiveFontFamilySelector

-- | cursiveFontFamily
--
-- ObjC selector: @- setCursiveFontFamily:@
setCursiveFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setCursiveFontFamily webPreferences value =
  sendMessage webPreferences setCursiveFontFamilySelector (toNSString value)

-- | fantasyFontFamily
--
-- ObjC selector: @- fantasyFontFamily@
fantasyFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
fantasyFontFamily webPreferences =
  sendMessage webPreferences fantasyFontFamilySelector

-- | fantasyFontFamily
--
-- ObjC selector: @- setFantasyFontFamily:@
setFantasyFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setFantasyFontFamily webPreferences value =
  sendMessage webPreferences setFantasyFontFamilySelector (toNSString value)

-- | defaultFontSize
--
-- ObjC selector: @- defaultFontSize@
defaultFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
defaultFontSize webPreferences =
  sendMessage webPreferences defaultFontSizeSelector

-- | defaultFontSize
--
-- ObjC selector: @- setDefaultFontSize:@
setDefaultFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setDefaultFontSize webPreferences value =
  sendMessage webPreferences setDefaultFontSizeSelector value

-- | defaultFixedFontSize
--
-- ObjC selector: @- defaultFixedFontSize@
defaultFixedFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
defaultFixedFontSize webPreferences =
  sendMessage webPreferences defaultFixedFontSizeSelector

-- | defaultFixedFontSize
--
-- ObjC selector: @- setDefaultFixedFontSize:@
setDefaultFixedFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setDefaultFixedFontSize webPreferences value =
  sendMessage webPreferences setDefaultFixedFontSizeSelector value

-- | minimumFontSize
--
-- ObjC selector: @- minimumFontSize@
minimumFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
minimumFontSize webPreferences =
  sendMessage webPreferences minimumFontSizeSelector

-- | minimumFontSize
--
-- ObjC selector: @- setMinimumFontSize:@
setMinimumFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setMinimumFontSize webPreferences value =
  sendMessage webPreferences setMinimumFontSizeSelector value

-- | minimumLogicalFontSize
--
-- ObjC selector: @- minimumLogicalFontSize@
minimumLogicalFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
minimumLogicalFontSize webPreferences =
  sendMessage webPreferences minimumLogicalFontSizeSelector

-- | minimumLogicalFontSize
--
-- ObjC selector: @- setMinimumLogicalFontSize:@
setMinimumLogicalFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setMinimumLogicalFontSize webPreferences value =
  sendMessage webPreferences setMinimumLogicalFontSizeSelector value

-- | defaultTextEncodingName
--
-- ObjC selector: @- defaultTextEncodingName@
defaultTextEncodingName :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
defaultTextEncodingName webPreferences =
  sendMessage webPreferences defaultTextEncodingNameSelector

-- | defaultTextEncodingName
--
-- ObjC selector: @- setDefaultTextEncodingName:@
setDefaultTextEncodingName :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setDefaultTextEncodingName webPreferences value =
  sendMessage webPreferences setDefaultTextEncodingNameSelector (toNSString value)

-- | userStyleSheetEnabled
--
-- ObjC selector: @- userStyleSheetEnabled@
userStyleSheetEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
userStyleSheetEnabled webPreferences =
  sendMessage webPreferences userStyleSheetEnabledSelector

-- | userStyleSheetEnabled
--
-- ObjC selector: @- setUserStyleSheetEnabled:@
setUserStyleSheetEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setUserStyleSheetEnabled webPreferences value =
  sendMessage webPreferences setUserStyleSheetEnabledSelector value

-- | userStyleSheetLocation
--
-- The location of the user style sheet.
--
-- ObjC selector: @- userStyleSheetLocation@
userStyleSheetLocation :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSURL)
userStyleSheetLocation webPreferences =
  sendMessage webPreferences userStyleSheetLocationSelector

-- | userStyleSheetLocation
--
-- The location of the user style sheet.
--
-- ObjC selector: @- setUserStyleSheetLocation:@
setUserStyleSheetLocation :: (IsWebPreferences webPreferences, IsNSURL value) => webPreferences -> value -> IO ()
setUserStyleSheetLocation webPreferences value =
  sendMessage webPreferences setUserStyleSheetLocationSelector (toNSURL value)

-- | javaEnabled
--
-- Deprecated function that does nothing and always returns false.
--
-- ObjC selector: @- javaEnabled@
javaEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
javaEnabled webPreferences =
  sendMessage webPreferences javaEnabledSelector

-- | javaEnabled
--
-- Deprecated function that does nothing and always returns false.
--
-- ObjC selector: @- setJavaEnabled:@
setJavaEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setJavaEnabled webPreferences value =
  sendMessage webPreferences setJavaEnabledSelector value

-- | javaScriptEnabled
--
-- ObjC selector: @- javaScriptEnabled@
javaScriptEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
javaScriptEnabled webPreferences =
  sendMessage webPreferences javaScriptEnabledSelector

-- | javaScriptEnabled
--
-- ObjC selector: @- setJavaScriptEnabled:@
setJavaScriptEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setJavaScriptEnabled webPreferences value =
  sendMessage webPreferences setJavaScriptEnabledSelector value

-- | javaScriptCanOpenWindowsAutomatically
--
-- ObjC selector: @- javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomatically :: IsWebPreferences webPreferences => webPreferences -> IO Bool
javaScriptCanOpenWindowsAutomatically webPreferences =
  sendMessage webPreferences javaScriptCanOpenWindowsAutomaticallySelector

-- | javaScriptCanOpenWindowsAutomatically
--
-- ObjC selector: @- setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomatically :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setJavaScriptCanOpenWindowsAutomatically webPreferences value =
  sendMessage webPreferences setJavaScriptCanOpenWindowsAutomaticallySelector value

-- | plugInsEnabled
--
-- ObjC selector: @- plugInsEnabled@
plugInsEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
plugInsEnabled webPreferences =
  sendMessage webPreferences plugInsEnabledSelector

-- | plugInsEnabled
--
-- ObjC selector: @- setPlugInsEnabled:@
setPlugInsEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setPlugInsEnabled webPreferences value =
  sendMessage webPreferences setPlugInsEnabledSelector value

-- | allowsAnimatedImages
--
-- ObjC selector: @- allowsAnimatedImages@
allowsAnimatedImages :: IsWebPreferences webPreferences => webPreferences -> IO Bool
allowsAnimatedImages webPreferences =
  sendMessage webPreferences allowsAnimatedImagesSelector

-- | allowsAnimatedImages
--
-- ObjC selector: @- setAllowsAnimatedImages:@
setAllowsAnimatedImages :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAllowsAnimatedImages webPreferences value =
  sendMessage webPreferences setAllowsAnimatedImagesSelector value

-- | allowsAnimatedImageLooping
--
-- ObjC selector: @- allowsAnimatedImageLooping@
allowsAnimatedImageLooping :: IsWebPreferences webPreferences => webPreferences -> IO Bool
allowsAnimatedImageLooping webPreferences =
  sendMessage webPreferences allowsAnimatedImageLoopingSelector

-- | allowsAnimatedImageLooping
--
-- ObjC selector: @- setAllowsAnimatedImageLooping:@
setAllowsAnimatedImageLooping :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAllowsAnimatedImageLooping webPreferences value =
  sendMessage webPreferences setAllowsAnimatedImageLoopingSelector value

-- | willLoadImagesAutomatically
--
-- ObjC selector: @- loadsImagesAutomatically@
loadsImagesAutomatically :: IsWebPreferences webPreferences => webPreferences -> IO Bool
loadsImagesAutomatically webPreferences =
  sendMessage webPreferences loadsImagesAutomaticallySelector

-- | willLoadImagesAutomatically
--
-- ObjC selector: @- setLoadsImagesAutomatically:@
setLoadsImagesAutomatically :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setLoadsImagesAutomatically webPreferences value =
  sendMessage webPreferences setLoadsImagesAutomaticallySelector value

-- | autosaves
--
-- If autosaves is YES the settings represented by    WebPreferences will be stored in the user defaults database.
--
-- ObjC selector: @- autosaves@
autosaves :: IsWebPreferences webPreferences => webPreferences -> IO Bool
autosaves webPreferences =
  sendMessage webPreferences autosavesSelector

-- | autosaves
--
-- If autosaves is YES the settings represented by    WebPreferences will be stored in the user defaults database.
--
-- ObjC selector: @- setAutosaves:@
setAutosaves :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAutosaves webPreferences value =
  sendMessage webPreferences setAutosavesSelector value

-- | shouldPrintBackgrounds
--
-- ObjC selector: @- shouldPrintBackgrounds@
shouldPrintBackgrounds :: IsWebPreferences webPreferences => webPreferences -> IO Bool
shouldPrintBackgrounds webPreferences =
  sendMessage webPreferences shouldPrintBackgroundsSelector

-- | shouldPrintBackgrounds
--
-- ObjC selector: @- setShouldPrintBackgrounds:@
setShouldPrintBackgrounds :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setShouldPrintBackgrounds webPreferences value =
  sendMessage webPreferences setShouldPrintBackgroundsSelector value

-- | privateBrowsingEnabled:
--
-- If private browsing is enabled, WebKit will not store information    about sites the user visits.
--
-- ObjC selector: @- privateBrowsingEnabled@
privateBrowsingEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
privateBrowsingEnabled webPreferences =
  sendMessage webPreferences privateBrowsingEnabledSelector

-- | privateBrowsingEnabled:
--
-- If private browsing is enabled, WebKit will not store information    about sites the user visits.
--
-- ObjC selector: @- setPrivateBrowsingEnabled:@
setPrivateBrowsingEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setPrivateBrowsingEnabled webPreferences value =
  sendMessage webPreferences setPrivateBrowsingEnabledSelector value

-- | tabsToLinks
--
-- If tabsToLinks is YES, the tab key will focus links and form controls.    The option key temporarily reverses this preference.
--
-- ObjC selector: @- tabsToLinks@
tabsToLinks :: IsWebPreferences webPreferences => webPreferences -> IO Bool
tabsToLinks webPreferences =
  sendMessage webPreferences tabsToLinksSelector

-- | tabsToLinks
--
-- If tabsToLinks is YES, the tab key will focus links and form controls.    The option key temporarily reverses this preference.
--
-- ObjC selector: @- setTabsToLinks:@
setTabsToLinks :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setTabsToLinks webPreferences value =
  sendMessage webPreferences setTabsToLinksSelector value

-- | usesPageCache
--
-- Whether the receiver's associated WebViews use the shared    page cache.
--
-- Pages are cached as they are added to a WebBackForwardList, and    removed from the cache as they are removed from a WebBackForwardList. Because     the page cache is global, caching a page in one WebBackForwardList may cause    a page in another WebBackForwardList to be evicted from the cache.
--
-- ObjC selector: @- usesPageCache@
usesPageCache :: IsWebPreferences webPreferences => webPreferences -> IO Bool
usesPageCache webPreferences =
  sendMessage webPreferences usesPageCacheSelector

-- | usesPageCache
--
-- Whether the receiver's associated WebViews use the shared    page cache.
--
-- Pages are cached as they are added to a WebBackForwardList, and    removed from the cache as they are removed from a WebBackForwardList. Because     the page cache is global, caching a page in one WebBackForwardList may cause    a page in another WebBackForwardList to be evicted from the cache.
--
-- ObjC selector: @- setUsesPageCache:@
setUsesPageCache :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setUsesPageCache webPreferences value =
  sendMessage webPreferences setUsesPageCacheSelector value

-- | cacheModel
--
-- Specifies a usage model for a WebView, which WebKit will use to    determine its caching behavior. If necessary, WebKit    will prune its caches to match cacheModel when set.
--
-- Research indicates that users tend to browse within clusters of    documents that hold resources in common, and to revisit previously visited    documents. WebKit and the frameworks below it include built-in caches that take    advantage of these patterns, substantially improving document load speed in    browsing situations. The WebKit cache model controls the behaviors of all of    these caches, including NSURLCache and the various WebCore caches.
--
-- Applications with a browsing interface can improve document load speed    substantially by specifying WebCacheModelDocumentBrowser. Applications without    a browsing interface can reduce memory usage substantially by specifying    WebCacheModelDocumentViewer.
--
-- If cacheModel is not set, WebKit will select a cache model automatically.
--
-- ObjC selector: @- cacheModel@
cacheModel :: IsWebPreferences webPreferences => webPreferences -> IO WebCacheModel
cacheModel webPreferences =
  sendMessage webPreferences cacheModelSelector

-- | cacheModel
--
-- Specifies a usage model for a WebView, which WebKit will use to    determine its caching behavior. If necessary, WebKit    will prune its caches to match cacheModel when set.
--
-- Research indicates that users tend to browse within clusters of    documents that hold resources in common, and to revisit previously visited    documents. WebKit and the frameworks below it include built-in caches that take    advantage of these patterns, substantially improving document load speed in    browsing situations. The WebKit cache model controls the behaviors of all of    these caches, including NSURLCache and the various WebCore caches.
--
-- Applications with a browsing interface can improve document load speed    substantially by specifying WebCacheModelDocumentBrowser. Applications without    a browsing interface can reduce memory usage substantially by specifying    WebCacheModelDocumentViewer.
--
-- If cacheModel is not set, WebKit will select a cache model automatically.
--
-- ObjC selector: @- setCacheModel:@
setCacheModel :: IsWebPreferences webPreferences => webPreferences -> WebCacheModel -> IO ()
setCacheModel webPreferences value =
  sendMessage webPreferences setCacheModelSelector value

-- | suppressesIncrementalRendering
--
-- ObjC selector: @- suppressesIncrementalRendering@
suppressesIncrementalRendering :: IsWebPreferences webPreferences => webPreferences -> IO Bool
suppressesIncrementalRendering webPreferences =
  sendMessage webPreferences suppressesIncrementalRenderingSelector

-- | suppressesIncrementalRendering
--
-- ObjC selector: @- setSuppressesIncrementalRendering:@
setSuppressesIncrementalRendering :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setSuppressesIncrementalRendering webPreferences value =
  sendMessage webPreferences setSuppressesIncrementalRenderingSelector value

-- | allowsAirPlayForMediaPlayback
--
-- ObjC selector: @- allowsAirPlayForMediaPlayback@
allowsAirPlayForMediaPlayback :: IsWebPreferences webPreferences => webPreferences -> IO Bool
allowsAirPlayForMediaPlayback webPreferences =
  sendMessage webPreferences allowsAirPlayForMediaPlaybackSelector

-- | allowsAirPlayForMediaPlayback
--
-- ObjC selector: @- setAllowsAirPlayForMediaPlayback:@
setAllowsAirPlayForMediaPlayback :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAllowsAirPlayForMediaPlayback webPreferences value =
  sendMessage webPreferences setAllowsAirPlayForMediaPlaybackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @standardPreferences@
standardPreferencesSelector :: Selector '[] (Id WebPreferences)
standardPreferencesSelector = mkSelector "standardPreferences"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id WebPreferences)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @standardFontFamily@
standardFontFamilySelector :: Selector '[] (Id NSString)
standardFontFamilySelector = mkSelector "standardFontFamily"

-- | @Selector@ for @setStandardFontFamily:@
setStandardFontFamilySelector :: Selector '[Id NSString] ()
setStandardFontFamilySelector = mkSelector "setStandardFontFamily:"

-- | @Selector@ for @fixedFontFamily@
fixedFontFamilySelector :: Selector '[] (Id NSString)
fixedFontFamilySelector = mkSelector "fixedFontFamily"

-- | @Selector@ for @setFixedFontFamily:@
setFixedFontFamilySelector :: Selector '[Id NSString] ()
setFixedFontFamilySelector = mkSelector "setFixedFontFamily:"

-- | @Selector@ for @serifFontFamily@
serifFontFamilySelector :: Selector '[] (Id NSString)
serifFontFamilySelector = mkSelector "serifFontFamily"

-- | @Selector@ for @setSerifFontFamily:@
setSerifFontFamilySelector :: Selector '[Id NSString] ()
setSerifFontFamilySelector = mkSelector "setSerifFontFamily:"

-- | @Selector@ for @sansSerifFontFamily@
sansSerifFontFamilySelector :: Selector '[] (Id NSString)
sansSerifFontFamilySelector = mkSelector "sansSerifFontFamily"

-- | @Selector@ for @setSansSerifFontFamily:@
setSansSerifFontFamilySelector :: Selector '[Id NSString] ()
setSansSerifFontFamilySelector = mkSelector "setSansSerifFontFamily:"

-- | @Selector@ for @cursiveFontFamily@
cursiveFontFamilySelector :: Selector '[] (Id NSString)
cursiveFontFamilySelector = mkSelector "cursiveFontFamily"

-- | @Selector@ for @setCursiveFontFamily:@
setCursiveFontFamilySelector :: Selector '[Id NSString] ()
setCursiveFontFamilySelector = mkSelector "setCursiveFontFamily:"

-- | @Selector@ for @fantasyFontFamily@
fantasyFontFamilySelector :: Selector '[] (Id NSString)
fantasyFontFamilySelector = mkSelector "fantasyFontFamily"

-- | @Selector@ for @setFantasyFontFamily:@
setFantasyFontFamilySelector :: Selector '[Id NSString] ()
setFantasyFontFamilySelector = mkSelector "setFantasyFontFamily:"

-- | @Selector@ for @defaultFontSize@
defaultFontSizeSelector :: Selector '[] CInt
defaultFontSizeSelector = mkSelector "defaultFontSize"

-- | @Selector@ for @setDefaultFontSize:@
setDefaultFontSizeSelector :: Selector '[CInt] ()
setDefaultFontSizeSelector = mkSelector "setDefaultFontSize:"

-- | @Selector@ for @defaultFixedFontSize@
defaultFixedFontSizeSelector :: Selector '[] CInt
defaultFixedFontSizeSelector = mkSelector "defaultFixedFontSize"

-- | @Selector@ for @setDefaultFixedFontSize:@
setDefaultFixedFontSizeSelector :: Selector '[CInt] ()
setDefaultFixedFontSizeSelector = mkSelector "setDefaultFixedFontSize:"

-- | @Selector@ for @minimumFontSize@
minimumFontSizeSelector :: Selector '[] CInt
minimumFontSizeSelector = mkSelector "minimumFontSize"

-- | @Selector@ for @setMinimumFontSize:@
setMinimumFontSizeSelector :: Selector '[CInt] ()
setMinimumFontSizeSelector = mkSelector "setMinimumFontSize:"

-- | @Selector@ for @minimumLogicalFontSize@
minimumLogicalFontSizeSelector :: Selector '[] CInt
minimumLogicalFontSizeSelector = mkSelector "minimumLogicalFontSize"

-- | @Selector@ for @setMinimumLogicalFontSize:@
setMinimumLogicalFontSizeSelector :: Selector '[CInt] ()
setMinimumLogicalFontSizeSelector = mkSelector "setMinimumLogicalFontSize:"

-- | @Selector@ for @defaultTextEncodingName@
defaultTextEncodingNameSelector :: Selector '[] (Id NSString)
defaultTextEncodingNameSelector = mkSelector "defaultTextEncodingName"

-- | @Selector@ for @setDefaultTextEncodingName:@
setDefaultTextEncodingNameSelector :: Selector '[Id NSString] ()
setDefaultTextEncodingNameSelector = mkSelector "setDefaultTextEncodingName:"

-- | @Selector@ for @userStyleSheetEnabled@
userStyleSheetEnabledSelector :: Selector '[] Bool
userStyleSheetEnabledSelector = mkSelector "userStyleSheetEnabled"

-- | @Selector@ for @setUserStyleSheetEnabled:@
setUserStyleSheetEnabledSelector :: Selector '[Bool] ()
setUserStyleSheetEnabledSelector = mkSelector "setUserStyleSheetEnabled:"

-- | @Selector@ for @userStyleSheetLocation@
userStyleSheetLocationSelector :: Selector '[] (Id NSURL)
userStyleSheetLocationSelector = mkSelector "userStyleSheetLocation"

-- | @Selector@ for @setUserStyleSheetLocation:@
setUserStyleSheetLocationSelector :: Selector '[Id NSURL] ()
setUserStyleSheetLocationSelector = mkSelector "setUserStyleSheetLocation:"

-- | @Selector@ for @javaEnabled@
javaEnabledSelector :: Selector '[] Bool
javaEnabledSelector = mkSelector "javaEnabled"

-- | @Selector@ for @setJavaEnabled:@
setJavaEnabledSelector :: Selector '[Bool] ()
setJavaEnabledSelector = mkSelector "setJavaEnabled:"

-- | @Selector@ for @javaScriptEnabled@
javaScriptEnabledSelector :: Selector '[] Bool
javaScriptEnabledSelector = mkSelector "javaScriptEnabled"

-- | @Selector@ for @setJavaScriptEnabled:@
setJavaScriptEnabledSelector :: Selector '[Bool] ()
setJavaScriptEnabledSelector = mkSelector "setJavaScriptEnabled:"

-- | @Selector@ for @javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomaticallySelector :: Selector '[] Bool
javaScriptCanOpenWindowsAutomaticallySelector = mkSelector "javaScriptCanOpenWindowsAutomatically"

-- | @Selector@ for @setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomaticallySelector :: Selector '[Bool] ()
setJavaScriptCanOpenWindowsAutomaticallySelector = mkSelector "setJavaScriptCanOpenWindowsAutomatically:"

-- | @Selector@ for @plugInsEnabled@
plugInsEnabledSelector :: Selector '[] Bool
plugInsEnabledSelector = mkSelector "plugInsEnabled"

-- | @Selector@ for @setPlugInsEnabled:@
setPlugInsEnabledSelector :: Selector '[Bool] ()
setPlugInsEnabledSelector = mkSelector "setPlugInsEnabled:"

-- | @Selector@ for @allowsAnimatedImages@
allowsAnimatedImagesSelector :: Selector '[] Bool
allowsAnimatedImagesSelector = mkSelector "allowsAnimatedImages"

-- | @Selector@ for @setAllowsAnimatedImages:@
setAllowsAnimatedImagesSelector :: Selector '[Bool] ()
setAllowsAnimatedImagesSelector = mkSelector "setAllowsAnimatedImages:"

-- | @Selector@ for @allowsAnimatedImageLooping@
allowsAnimatedImageLoopingSelector :: Selector '[] Bool
allowsAnimatedImageLoopingSelector = mkSelector "allowsAnimatedImageLooping"

-- | @Selector@ for @setAllowsAnimatedImageLooping:@
setAllowsAnimatedImageLoopingSelector :: Selector '[Bool] ()
setAllowsAnimatedImageLoopingSelector = mkSelector "setAllowsAnimatedImageLooping:"

-- | @Selector@ for @loadsImagesAutomatically@
loadsImagesAutomaticallySelector :: Selector '[] Bool
loadsImagesAutomaticallySelector = mkSelector "loadsImagesAutomatically"

-- | @Selector@ for @setLoadsImagesAutomatically:@
setLoadsImagesAutomaticallySelector :: Selector '[Bool] ()
setLoadsImagesAutomaticallySelector = mkSelector "setLoadsImagesAutomatically:"

-- | @Selector@ for @autosaves@
autosavesSelector :: Selector '[] Bool
autosavesSelector = mkSelector "autosaves"

-- | @Selector@ for @setAutosaves:@
setAutosavesSelector :: Selector '[Bool] ()
setAutosavesSelector = mkSelector "setAutosaves:"

-- | @Selector@ for @shouldPrintBackgrounds@
shouldPrintBackgroundsSelector :: Selector '[] Bool
shouldPrintBackgroundsSelector = mkSelector "shouldPrintBackgrounds"

-- | @Selector@ for @setShouldPrintBackgrounds:@
setShouldPrintBackgroundsSelector :: Selector '[Bool] ()
setShouldPrintBackgroundsSelector = mkSelector "setShouldPrintBackgrounds:"

-- | @Selector@ for @privateBrowsingEnabled@
privateBrowsingEnabledSelector :: Selector '[] Bool
privateBrowsingEnabledSelector = mkSelector "privateBrowsingEnabled"

-- | @Selector@ for @setPrivateBrowsingEnabled:@
setPrivateBrowsingEnabledSelector :: Selector '[Bool] ()
setPrivateBrowsingEnabledSelector = mkSelector "setPrivateBrowsingEnabled:"

-- | @Selector@ for @tabsToLinks@
tabsToLinksSelector :: Selector '[] Bool
tabsToLinksSelector = mkSelector "tabsToLinks"

-- | @Selector@ for @setTabsToLinks:@
setTabsToLinksSelector :: Selector '[Bool] ()
setTabsToLinksSelector = mkSelector "setTabsToLinks:"

-- | @Selector@ for @usesPageCache@
usesPageCacheSelector :: Selector '[] Bool
usesPageCacheSelector = mkSelector "usesPageCache"

-- | @Selector@ for @setUsesPageCache:@
setUsesPageCacheSelector :: Selector '[Bool] ()
setUsesPageCacheSelector = mkSelector "setUsesPageCache:"

-- | @Selector@ for @cacheModel@
cacheModelSelector :: Selector '[] WebCacheModel
cacheModelSelector = mkSelector "cacheModel"

-- | @Selector@ for @setCacheModel:@
setCacheModelSelector :: Selector '[WebCacheModel] ()
setCacheModelSelector = mkSelector "setCacheModel:"

-- | @Selector@ for @suppressesIncrementalRendering@
suppressesIncrementalRenderingSelector :: Selector '[] Bool
suppressesIncrementalRenderingSelector = mkSelector "suppressesIncrementalRendering"

-- | @Selector@ for @setSuppressesIncrementalRendering:@
setSuppressesIncrementalRenderingSelector :: Selector '[Bool] ()
setSuppressesIncrementalRenderingSelector = mkSelector "setSuppressesIncrementalRendering:"

-- | @Selector@ for @allowsAirPlayForMediaPlayback@
allowsAirPlayForMediaPlaybackSelector :: Selector '[] Bool
allowsAirPlayForMediaPlaybackSelector = mkSelector "allowsAirPlayForMediaPlayback"

-- | @Selector@ for @setAllowsAirPlayForMediaPlayback:@
setAllowsAirPlayForMediaPlaybackSelector :: Selector '[Bool] ()
setAllowsAirPlayForMediaPlaybackSelector = mkSelector "setAllowsAirPlayForMediaPlayback:"

