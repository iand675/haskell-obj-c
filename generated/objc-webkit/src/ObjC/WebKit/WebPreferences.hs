{-# LANGUAGE PatternSynonyms #-}
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
  , standardPreferencesSelector
  , initWithIdentifierSelector
  , identifierSelector
  , standardFontFamilySelector
  , setStandardFontFamilySelector
  , fixedFontFamilySelector
  , setFixedFontFamilySelector
  , serifFontFamilySelector
  , setSerifFontFamilySelector
  , sansSerifFontFamilySelector
  , setSansSerifFontFamilySelector
  , cursiveFontFamilySelector
  , setCursiveFontFamilySelector
  , fantasyFontFamilySelector
  , setFantasyFontFamilySelector
  , defaultFontSizeSelector
  , setDefaultFontSizeSelector
  , defaultFixedFontSizeSelector
  , setDefaultFixedFontSizeSelector
  , minimumFontSizeSelector
  , setMinimumFontSizeSelector
  , minimumLogicalFontSizeSelector
  , setMinimumLogicalFontSizeSelector
  , defaultTextEncodingNameSelector
  , setDefaultTextEncodingNameSelector
  , userStyleSheetEnabledSelector
  , setUserStyleSheetEnabledSelector
  , userStyleSheetLocationSelector
  , setUserStyleSheetLocationSelector
  , javaEnabledSelector
  , setJavaEnabledSelector
  , javaScriptEnabledSelector
  , setJavaScriptEnabledSelector
  , javaScriptCanOpenWindowsAutomaticallySelector
  , setJavaScriptCanOpenWindowsAutomaticallySelector
  , plugInsEnabledSelector
  , setPlugInsEnabledSelector
  , allowsAnimatedImagesSelector
  , setAllowsAnimatedImagesSelector
  , allowsAnimatedImageLoopingSelector
  , setAllowsAnimatedImageLoopingSelector
  , loadsImagesAutomaticallySelector
  , setLoadsImagesAutomaticallySelector
  , autosavesSelector
  , setAutosavesSelector
  , shouldPrintBackgroundsSelector
  , setShouldPrintBackgroundsSelector
  , privateBrowsingEnabledSelector
  , setPrivateBrowsingEnabledSelector
  , tabsToLinksSelector
  , setTabsToLinksSelector
  , usesPageCacheSelector
  , setUsesPageCacheSelector
  , cacheModelSelector
  , setCacheModelSelector
  , suppressesIncrementalRenderingSelector
  , setSuppressesIncrementalRenderingSelector
  , allowsAirPlayForMediaPlaybackSelector
  , setAllowsAirPlayForMediaPlaybackSelector

  -- * Enum types
  , WebCacheModel(WebCacheModel)
  , pattern WebCacheModelDocumentViewer
  , pattern WebCacheModelDocumentBrowser
  , pattern WebCacheModelPrimaryWebBrowser

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
import ObjC.Foundation.Internal.Classes

-- | standardPreferences
--
-- ObjC selector: @+ standardPreferences@
standardPreferences :: IO (Id WebPreferences)
standardPreferences  =
  do
    cls' <- getRequiredClass "WebPreferences"
    sendClassMsg cls' (mkSelector "standardPreferences") (retPtr retVoid) [] >>= retainedObject . castPtr

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
initWithIdentifier webPreferences  anIdentifier =
withObjCPtr anIdentifier $ \raw_anIdentifier ->
    sendMsg webPreferences (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_anIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | identifier
--
-- Returns: Returns the identifier for this WebPreferences.
--
-- ObjC selector: @- identifier@
identifier :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
identifier webPreferences  =
  sendMsg webPreferences (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | standardFontFamily
--
-- ObjC selector: @- standardFontFamily@
standardFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
standardFontFamily webPreferences  =
  sendMsg webPreferences (mkSelector "standardFontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | standardFontFamily
--
-- ObjC selector: @- setStandardFontFamily:@
setStandardFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setStandardFontFamily webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setStandardFontFamily:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fixedFontFamily
--
-- ObjC selector: @- fixedFontFamily@
fixedFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
fixedFontFamily webPreferences  =
  sendMsg webPreferences (mkSelector "fixedFontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fixedFontFamily
--
-- ObjC selector: @- setFixedFontFamily:@
setFixedFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setFixedFontFamily webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setFixedFontFamily:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | serifFontFamily
--
-- ObjC selector: @- serifFontFamily@
serifFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
serifFontFamily webPreferences  =
  sendMsg webPreferences (mkSelector "serifFontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serifFontFamily
--
-- ObjC selector: @- setSerifFontFamily:@
setSerifFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setSerifFontFamily webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setSerifFontFamily:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sansSerifFontFamily
--
-- ObjC selector: @- sansSerifFontFamily@
sansSerifFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
sansSerifFontFamily webPreferences  =
  sendMsg webPreferences (mkSelector "sansSerifFontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sansSerifFontFamily
--
-- ObjC selector: @- setSansSerifFontFamily:@
setSansSerifFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setSansSerifFontFamily webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setSansSerifFontFamily:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | cursiveFontFamily
--
-- ObjC selector: @- cursiveFontFamily@
cursiveFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
cursiveFontFamily webPreferences  =
  sendMsg webPreferences (mkSelector "cursiveFontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cursiveFontFamily
--
-- ObjC selector: @- setCursiveFontFamily:@
setCursiveFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setCursiveFontFamily webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setCursiveFontFamily:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fantasyFontFamily
--
-- ObjC selector: @- fantasyFontFamily@
fantasyFontFamily :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
fantasyFontFamily webPreferences  =
  sendMsg webPreferences (mkSelector "fantasyFontFamily") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fantasyFontFamily
--
-- ObjC selector: @- setFantasyFontFamily:@
setFantasyFontFamily :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setFantasyFontFamily webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setFantasyFontFamily:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | defaultFontSize
--
-- ObjC selector: @- defaultFontSize@
defaultFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
defaultFontSize webPreferences  =
  sendMsg webPreferences (mkSelector "defaultFontSize") retCInt []

-- | defaultFontSize
--
-- ObjC selector: @- setDefaultFontSize:@
setDefaultFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setDefaultFontSize webPreferences  value =
  sendMsg webPreferences (mkSelector "setDefaultFontSize:") retVoid [argCInt (fromIntegral value)]

-- | defaultFixedFontSize
--
-- ObjC selector: @- defaultFixedFontSize@
defaultFixedFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
defaultFixedFontSize webPreferences  =
  sendMsg webPreferences (mkSelector "defaultFixedFontSize") retCInt []

-- | defaultFixedFontSize
--
-- ObjC selector: @- setDefaultFixedFontSize:@
setDefaultFixedFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setDefaultFixedFontSize webPreferences  value =
  sendMsg webPreferences (mkSelector "setDefaultFixedFontSize:") retVoid [argCInt (fromIntegral value)]

-- | minimumFontSize
--
-- ObjC selector: @- minimumFontSize@
minimumFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
minimumFontSize webPreferences  =
  sendMsg webPreferences (mkSelector "minimumFontSize") retCInt []

-- | minimumFontSize
--
-- ObjC selector: @- setMinimumFontSize:@
setMinimumFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setMinimumFontSize webPreferences  value =
  sendMsg webPreferences (mkSelector "setMinimumFontSize:") retVoid [argCInt (fromIntegral value)]

-- | minimumLogicalFontSize
--
-- ObjC selector: @- minimumLogicalFontSize@
minimumLogicalFontSize :: IsWebPreferences webPreferences => webPreferences -> IO CInt
minimumLogicalFontSize webPreferences  =
  sendMsg webPreferences (mkSelector "minimumLogicalFontSize") retCInt []

-- | minimumLogicalFontSize
--
-- ObjC selector: @- setMinimumLogicalFontSize:@
setMinimumLogicalFontSize :: IsWebPreferences webPreferences => webPreferences -> CInt -> IO ()
setMinimumLogicalFontSize webPreferences  value =
  sendMsg webPreferences (mkSelector "setMinimumLogicalFontSize:") retVoid [argCInt (fromIntegral value)]

-- | defaultTextEncodingName
--
-- ObjC selector: @- defaultTextEncodingName@
defaultTextEncodingName :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSString)
defaultTextEncodingName webPreferences  =
  sendMsg webPreferences (mkSelector "defaultTextEncodingName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultTextEncodingName
--
-- ObjC selector: @- setDefaultTextEncodingName:@
setDefaultTextEncodingName :: (IsWebPreferences webPreferences, IsNSString value) => webPreferences -> value -> IO ()
setDefaultTextEncodingName webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setDefaultTextEncodingName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | userStyleSheetEnabled
--
-- ObjC selector: @- userStyleSheetEnabled@
userStyleSheetEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
userStyleSheetEnabled webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "userStyleSheetEnabled") retCULong []

-- | userStyleSheetEnabled
--
-- ObjC selector: @- setUserStyleSheetEnabled:@
setUserStyleSheetEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setUserStyleSheetEnabled webPreferences  value =
  sendMsg webPreferences (mkSelector "setUserStyleSheetEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | userStyleSheetLocation
--
-- The location of the user style sheet.
--
-- ObjC selector: @- userStyleSheetLocation@
userStyleSheetLocation :: IsWebPreferences webPreferences => webPreferences -> IO (Id NSURL)
userStyleSheetLocation webPreferences  =
  sendMsg webPreferences (mkSelector "userStyleSheetLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | userStyleSheetLocation
--
-- The location of the user style sheet.
--
-- ObjC selector: @- setUserStyleSheetLocation:@
setUserStyleSheetLocation :: (IsWebPreferences webPreferences, IsNSURL value) => webPreferences -> value -> IO ()
setUserStyleSheetLocation webPreferences  value =
withObjCPtr value $ \raw_value ->
    sendMsg webPreferences (mkSelector "setUserStyleSheetLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | javaEnabled
--
-- Deprecated function that does nothing and always returns false.
--
-- ObjC selector: @- javaEnabled@
javaEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
javaEnabled webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "javaEnabled") retCULong []

-- | javaEnabled
--
-- Deprecated function that does nothing and always returns false.
--
-- ObjC selector: @- setJavaEnabled:@
setJavaEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setJavaEnabled webPreferences  value =
  sendMsg webPreferences (mkSelector "setJavaEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | javaScriptEnabled
--
-- ObjC selector: @- javaScriptEnabled@
javaScriptEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
javaScriptEnabled webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "javaScriptEnabled") retCULong []

-- | javaScriptEnabled
--
-- ObjC selector: @- setJavaScriptEnabled:@
setJavaScriptEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setJavaScriptEnabled webPreferences  value =
  sendMsg webPreferences (mkSelector "setJavaScriptEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | javaScriptCanOpenWindowsAutomatically
--
-- ObjC selector: @- javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomatically :: IsWebPreferences webPreferences => webPreferences -> IO Bool
javaScriptCanOpenWindowsAutomatically webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "javaScriptCanOpenWindowsAutomatically") retCULong []

-- | javaScriptCanOpenWindowsAutomatically
--
-- ObjC selector: @- setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomatically :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setJavaScriptCanOpenWindowsAutomatically webPreferences  value =
  sendMsg webPreferences (mkSelector "setJavaScriptCanOpenWindowsAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | plugInsEnabled
--
-- ObjC selector: @- plugInsEnabled@
plugInsEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
plugInsEnabled webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "plugInsEnabled") retCULong []

-- | plugInsEnabled
--
-- ObjC selector: @- setPlugInsEnabled:@
setPlugInsEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setPlugInsEnabled webPreferences  value =
  sendMsg webPreferences (mkSelector "setPlugInsEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | allowsAnimatedImages
--
-- ObjC selector: @- allowsAnimatedImages@
allowsAnimatedImages :: IsWebPreferences webPreferences => webPreferences -> IO Bool
allowsAnimatedImages webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "allowsAnimatedImages") retCULong []

-- | allowsAnimatedImages
--
-- ObjC selector: @- setAllowsAnimatedImages:@
setAllowsAnimatedImages :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAllowsAnimatedImages webPreferences  value =
  sendMsg webPreferences (mkSelector "setAllowsAnimatedImages:") retVoid [argCULong (if value then 1 else 0)]

-- | allowsAnimatedImageLooping
--
-- ObjC selector: @- allowsAnimatedImageLooping@
allowsAnimatedImageLooping :: IsWebPreferences webPreferences => webPreferences -> IO Bool
allowsAnimatedImageLooping webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "allowsAnimatedImageLooping") retCULong []

-- | allowsAnimatedImageLooping
--
-- ObjC selector: @- setAllowsAnimatedImageLooping:@
setAllowsAnimatedImageLooping :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAllowsAnimatedImageLooping webPreferences  value =
  sendMsg webPreferences (mkSelector "setAllowsAnimatedImageLooping:") retVoid [argCULong (if value then 1 else 0)]

-- | willLoadImagesAutomatically
--
-- ObjC selector: @- loadsImagesAutomatically@
loadsImagesAutomatically :: IsWebPreferences webPreferences => webPreferences -> IO Bool
loadsImagesAutomatically webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "loadsImagesAutomatically") retCULong []

-- | willLoadImagesAutomatically
--
-- ObjC selector: @- setLoadsImagesAutomatically:@
setLoadsImagesAutomatically :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setLoadsImagesAutomatically webPreferences  value =
  sendMsg webPreferences (mkSelector "setLoadsImagesAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | autosaves
--
-- If autosaves is YES the settings represented by    WebPreferences will be stored in the user defaults database.
--
-- ObjC selector: @- autosaves@
autosaves :: IsWebPreferences webPreferences => webPreferences -> IO Bool
autosaves webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "autosaves") retCULong []

-- | autosaves
--
-- If autosaves is YES the settings represented by    WebPreferences will be stored in the user defaults database.
--
-- ObjC selector: @- setAutosaves:@
setAutosaves :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAutosaves webPreferences  value =
  sendMsg webPreferences (mkSelector "setAutosaves:") retVoid [argCULong (if value then 1 else 0)]

-- | shouldPrintBackgrounds
--
-- ObjC selector: @- shouldPrintBackgrounds@
shouldPrintBackgrounds :: IsWebPreferences webPreferences => webPreferences -> IO Bool
shouldPrintBackgrounds webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "shouldPrintBackgrounds") retCULong []

-- | shouldPrintBackgrounds
--
-- ObjC selector: @- setShouldPrintBackgrounds:@
setShouldPrintBackgrounds :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setShouldPrintBackgrounds webPreferences  value =
  sendMsg webPreferences (mkSelector "setShouldPrintBackgrounds:") retVoid [argCULong (if value then 1 else 0)]

-- | privateBrowsingEnabled:
--
-- If private browsing is enabled, WebKit will not store information    about sites the user visits.
--
-- ObjC selector: @- privateBrowsingEnabled@
privateBrowsingEnabled :: IsWebPreferences webPreferences => webPreferences -> IO Bool
privateBrowsingEnabled webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "privateBrowsingEnabled") retCULong []

-- | privateBrowsingEnabled:
--
-- If private browsing is enabled, WebKit will not store information    about sites the user visits.
--
-- ObjC selector: @- setPrivateBrowsingEnabled:@
setPrivateBrowsingEnabled :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setPrivateBrowsingEnabled webPreferences  value =
  sendMsg webPreferences (mkSelector "setPrivateBrowsingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | tabsToLinks
--
-- If tabsToLinks is YES, the tab key will focus links and form controls.    The option key temporarily reverses this preference.
--
-- ObjC selector: @- tabsToLinks@
tabsToLinks :: IsWebPreferences webPreferences => webPreferences -> IO Bool
tabsToLinks webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "tabsToLinks") retCULong []

-- | tabsToLinks
--
-- If tabsToLinks is YES, the tab key will focus links and form controls.    The option key temporarily reverses this preference.
--
-- ObjC selector: @- setTabsToLinks:@
setTabsToLinks :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setTabsToLinks webPreferences  value =
  sendMsg webPreferences (mkSelector "setTabsToLinks:") retVoid [argCULong (if value then 1 else 0)]

-- | usesPageCache
--
-- Whether the receiver's associated WebViews use the shared    page cache.
--
-- Pages are cached as they are added to a WebBackForwardList, and    removed from the cache as they are removed from a WebBackForwardList. Because     the page cache is global, caching a page in one WebBackForwardList may cause    a page in another WebBackForwardList to be evicted from the cache.
--
-- ObjC selector: @- usesPageCache@
usesPageCache :: IsWebPreferences webPreferences => webPreferences -> IO Bool
usesPageCache webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "usesPageCache") retCULong []

-- | usesPageCache
--
-- Whether the receiver's associated WebViews use the shared    page cache.
--
-- Pages are cached as they are added to a WebBackForwardList, and    removed from the cache as they are removed from a WebBackForwardList. Because     the page cache is global, caching a page in one WebBackForwardList may cause    a page in another WebBackForwardList to be evicted from the cache.
--
-- ObjC selector: @- setUsesPageCache:@
setUsesPageCache :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setUsesPageCache webPreferences  value =
  sendMsg webPreferences (mkSelector "setUsesPageCache:") retVoid [argCULong (if value then 1 else 0)]

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
cacheModel webPreferences  =
  fmap (coerce :: CULong -> WebCacheModel) $ sendMsg webPreferences (mkSelector "cacheModel") retCULong []

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
setCacheModel webPreferences  value =
  sendMsg webPreferences (mkSelector "setCacheModel:") retVoid [argCULong (coerce value)]

-- | suppressesIncrementalRendering
--
-- ObjC selector: @- suppressesIncrementalRendering@
suppressesIncrementalRendering :: IsWebPreferences webPreferences => webPreferences -> IO Bool
suppressesIncrementalRendering webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "suppressesIncrementalRendering") retCULong []

-- | suppressesIncrementalRendering
--
-- ObjC selector: @- setSuppressesIncrementalRendering:@
setSuppressesIncrementalRendering :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setSuppressesIncrementalRendering webPreferences  value =
  sendMsg webPreferences (mkSelector "setSuppressesIncrementalRendering:") retVoid [argCULong (if value then 1 else 0)]

-- | allowsAirPlayForMediaPlayback
--
-- ObjC selector: @- allowsAirPlayForMediaPlayback@
allowsAirPlayForMediaPlayback :: IsWebPreferences webPreferences => webPreferences -> IO Bool
allowsAirPlayForMediaPlayback webPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webPreferences (mkSelector "allowsAirPlayForMediaPlayback") retCULong []

-- | allowsAirPlayForMediaPlayback
--
-- ObjC selector: @- setAllowsAirPlayForMediaPlayback:@
setAllowsAirPlayForMediaPlayback :: IsWebPreferences webPreferences => webPreferences -> Bool -> IO ()
setAllowsAirPlayForMediaPlayback webPreferences  value =
  sendMsg webPreferences (mkSelector "setAllowsAirPlayForMediaPlayback:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @standardPreferences@
standardPreferencesSelector :: Selector
standardPreferencesSelector = mkSelector "standardPreferences"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @standardFontFamily@
standardFontFamilySelector :: Selector
standardFontFamilySelector = mkSelector "standardFontFamily"

-- | @Selector@ for @setStandardFontFamily:@
setStandardFontFamilySelector :: Selector
setStandardFontFamilySelector = mkSelector "setStandardFontFamily:"

-- | @Selector@ for @fixedFontFamily@
fixedFontFamilySelector :: Selector
fixedFontFamilySelector = mkSelector "fixedFontFamily"

-- | @Selector@ for @setFixedFontFamily:@
setFixedFontFamilySelector :: Selector
setFixedFontFamilySelector = mkSelector "setFixedFontFamily:"

-- | @Selector@ for @serifFontFamily@
serifFontFamilySelector :: Selector
serifFontFamilySelector = mkSelector "serifFontFamily"

-- | @Selector@ for @setSerifFontFamily:@
setSerifFontFamilySelector :: Selector
setSerifFontFamilySelector = mkSelector "setSerifFontFamily:"

-- | @Selector@ for @sansSerifFontFamily@
sansSerifFontFamilySelector :: Selector
sansSerifFontFamilySelector = mkSelector "sansSerifFontFamily"

-- | @Selector@ for @setSansSerifFontFamily:@
setSansSerifFontFamilySelector :: Selector
setSansSerifFontFamilySelector = mkSelector "setSansSerifFontFamily:"

-- | @Selector@ for @cursiveFontFamily@
cursiveFontFamilySelector :: Selector
cursiveFontFamilySelector = mkSelector "cursiveFontFamily"

-- | @Selector@ for @setCursiveFontFamily:@
setCursiveFontFamilySelector :: Selector
setCursiveFontFamilySelector = mkSelector "setCursiveFontFamily:"

-- | @Selector@ for @fantasyFontFamily@
fantasyFontFamilySelector :: Selector
fantasyFontFamilySelector = mkSelector "fantasyFontFamily"

-- | @Selector@ for @setFantasyFontFamily:@
setFantasyFontFamilySelector :: Selector
setFantasyFontFamilySelector = mkSelector "setFantasyFontFamily:"

-- | @Selector@ for @defaultFontSize@
defaultFontSizeSelector :: Selector
defaultFontSizeSelector = mkSelector "defaultFontSize"

-- | @Selector@ for @setDefaultFontSize:@
setDefaultFontSizeSelector :: Selector
setDefaultFontSizeSelector = mkSelector "setDefaultFontSize:"

-- | @Selector@ for @defaultFixedFontSize@
defaultFixedFontSizeSelector :: Selector
defaultFixedFontSizeSelector = mkSelector "defaultFixedFontSize"

-- | @Selector@ for @setDefaultFixedFontSize:@
setDefaultFixedFontSizeSelector :: Selector
setDefaultFixedFontSizeSelector = mkSelector "setDefaultFixedFontSize:"

-- | @Selector@ for @minimumFontSize@
minimumFontSizeSelector :: Selector
minimumFontSizeSelector = mkSelector "minimumFontSize"

-- | @Selector@ for @setMinimumFontSize:@
setMinimumFontSizeSelector :: Selector
setMinimumFontSizeSelector = mkSelector "setMinimumFontSize:"

-- | @Selector@ for @minimumLogicalFontSize@
minimumLogicalFontSizeSelector :: Selector
minimumLogicalFontSizeSelector = mkSelector "minimumLogicalFontSize"

-- | @Selector@ for @setMinimumLogicalFontSize:@
setMinimumLogicalFontSizeSelector :: Selector
setMinimumLogicalFontSizeSelector = mkSelector "setMinimumLogicalFontSize:"

-- | @Selector@ for @defaultTextEncodingName@
defaultTextEncodingNameSelector :: Selector
defaultTextEncodingNameSelector = mkSelector "defaultTextEncodingName"

-- | @Selector@ for @setDefaultTextEncodingName:@
setDefaultTextEncodingNameSelector :: Selector
setDefaultTextEncodingNameSelector = mkSelector "setDefaultTextEncodingName:"

-- | @Selector@ for @userStyleSheetEnabled@
userStyleSheetEnabledSelector :: Selector
userStyleSheetEnabledSelector = mkSelector "userStyleSheetEnabled"

-- | @Selector@ for @setUserStyleSheetEnabled:@
setUserStyleSheetEnabledSelector :: Selector
setUserStyleSheetEnabledSelector = mkSelector "setUserStyleSheetEnabled:"

-- | @Selector@ for @userStyleSheetLocation@
userStyleSheetLocationSelector :: Selector
userStyleSheetLocationSelector = mkSelector "userStyleSheetLocation"

-- | @Selector@ for @setUserStyleSheetLocation:@
setUserStyleSheetLocationSelector :: Selector
setUserStyleSheetLocationSelector = mkSelector "setUserStyleSheetLocation:"

-- | @Selector@ for @javaEnabled@
javaEnabledSelector :: Selector
javaEnabledSelector = mkSelector "javaEnabled"

-- | @Selector@ for @setJavaEnabled:@
setJavaEnabledSelector :: Selector
setJavaEnabledSelector = mkSelector "setJavaEnabled:"

-- | @Selector@ for @javaScriptEnabled@
javaScriptEnabledSelector :: Selector
javaScriptEnabledSelector = mkSelector "javaScriptEnabled"

-- | @Selector@ for @setJavaScriptEnabled:@
setJavaScriptEnabledSelector :: Selector
setJavaScriptEnabledSelector = mkSelector "setJavaScriptEnabled:"

-- | @Selector@ for @javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomaticallySelector :: Selector
javaScriptCanOpenWindowsAutomaticallySelector = mkSelector "javaScriptCanOpenWindowsAutomatically"

-- | @Selector@ for @setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomaticallySelector :: Selector
setJavaScriptCanOpenWindowsAutomaticallySelector = mkSelector "setJavaScriptCanOpenWindowsAutomatically:"

-- | @Selector@ for @plugInsEnabled@
plugInsEnabledSelector :: Selector
plugInsEnabledSelector = mkSelector "plugInsEnabled"

-- | @Selector@ for @setPlugInsEnabled:@
setPlugInsEnabledSelector :: Selector
setPlugInsEnabledSelector = mkSelector "setPlugInsEnabled:"

-- | @Selector@ for @allowsAnimatedImages@
allowsAnimatedImagesSelector :: Selector
allowsAnimatedImagesSelector = mkSelector "allowsAnimatedImages"

-- | @Selector@ for @setAllowsAnimatedImages:@
setAllowsAnimatedImagesSelector :: Selector
setAllowsAnimatedImagesSelector = mkSelector "setAllowsAnimatedImages:"

-- | @Selector@ for @allowsAnimatedImageLooping@
allowsAnimatedImageLoopingSelector :: Selector
allowsAnimatedImageLoopingSelector = mkSelector "allowsAnimatedImageLooping"

-- | @Selector@ for @setAllowsAnimatedImageLooping:@
setAllowsAnimatedImageLoopingSelector :: Selector
setAllowsAnimatedImageLoopingSelector = mkSelector "setAllowsAnimatedImageLooping:"

-- | @Selector@ for @loadsImagesAutomatically@
loadsImagesAutomaticallySelector :: Selector
loadsImagesAutomaticallySelector = mkSelector "loadsImagesAutomatically"

-- | @Selector@ for @setLoadsImagesAutomatically:@
setLoadsImagesAutomaticallySelector :: Selector
setLoadsImagesAutomaticallySelector = mkSelector "setLoadsImagesAutomatically:"

-- | @Selector@ for @autosaves@
autosavesSelector :: Selector
autosavesSelector = mkSelector "autosaves"

-- | @Selector@ for @setAutosaves:@
setAutosavesSelector :: Selector
setAutosavesSelector = mkSelector "setAutosaves:"

-- | @Selector@ for @shouldPrintBackgrounds@
shouldPrintBackgroundsSelector :: Selector
shouldPrintBackgroundsSelector = mkSelector "shouldPrintBackgrounds"

-- | @Selector@ for @setShouldPrintBackgrounds:@
setShouldPrintBackgroundsSelector :: Selector
setShouldPrintBackgroundsSelector = mkSelector "setShouldPrintBackgrounds:"

-- | @Selector@ for @privateBrowsingEnabled@
privateBrowsingEnabledSelector :: Selector
privateBrowsingEnabledSelector = mkSelector "privateBrowsingEnabled"

-- | @Selector@ for @setPrivateBrowsingEnabled:@
setPrivateBrowsingEnabledSelector :: Selector
setPrivateBrowsingEnabledSelector = mkSelector "setPrivateBrowsingEnabled:"

-- | @Selector@ for @tabsToLinks@
tabsToLinksSelector :: Selector
tabsToLinksSelector = mkSelector "tabsToLinks"

-- | @Selector@ for @setTabsToLinks:@
setTabsToLinksSelector :: Selector
setTabsToLinksSelector = mkSelector "setTabsToLinks:"

-- | @Selector@ for @usesPageCache@
usesPageCacheSelector :: Selector
usesPageCacheSelector = mkSelector "usesPageCache"

-- | @Selector@ for @setUsesPageCache:@
setUsesPageCacheSelector :: Selector
setUsesPageCacheSelector = mkSelector "setUsesPageCache:"

-- | @Selector@ for @cacheModel@
cacheModelSelector :: Selector
cacheModelSelector = mkSelector "cacheModel"

-- | @Selector@ for @setCacheModel:@
setCacheModelSelector :: Selector
setCacheModelSelector = mkSelector "setCacheModel:"

-- | @Selector@ for @suppressesIncrementalRendering@
suppressesIncrementalRenderingSelector :: Selector
suppressesIncrementalRenderingSelector = mkSelector "suppressesIncrementalRendering"

-- | @Selector@ for @setSuppressesIncrementalRendering:@
setSuppressesIncrementalRenderingSelector :: Selector
setSuppressesIncrementalRenderingSelector = mkSelector "setSuppressesIncrementalRendering:"

-- | @Selector@ for @allowsAirPlayForMediaPlayback@
allowsAirPlayForMediaPlaybackSelector :: Selector
allowsAirPlayForMediaPlaybackSelector = mkSelector "allowsAirPlayForMediaPlayback"

-- | @Selector@ for @setAllowsAirPlayForMediaPlayback:@
setAllowsAirPlayForMediaPlaybackSelector :: Selector
setAllowsAirPlayForMediaPlaybackSelector = mkSelector "setAllowsAirPlayForMediaPlayback:"

