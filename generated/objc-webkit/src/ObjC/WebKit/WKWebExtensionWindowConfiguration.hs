{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionWindowConfiguration`` object encapsulates configuration options for a window in an extension.
--
-- This class holds various options that influence the behavior and initial state of a window. The app retains the discretion to disregard any or all of these options, or even opt not to create a window.
--
-- Generated bindings for @WKWebExtensionWindowConfiguration@.
module ObjC.WebKit.WKWebExtensionWindowConfiguration
  ( WKWebExtensionWindowConfiguration
  , IsWKWebExtensionWindowConfiguration(..)
  , new
  , init_
  , windowType
  , windowState
  , tabURLs
  , shouldBeFocused
  , shouldBePrivate
  , newSelector
  , initSelector
  , windowTypeSelector
  , windowStateSelector
  , tabURLsSelector
  , shouldBeFocusedSelector
  , shouldBePrivateSelector

  -- * Enum types
  , WKWebExtensionWindowState(WKWebExtensionWindowState)
  , pattern WKWebExtensionWindowStateNormal
  , pattern WKWebExtensionWindowStateMinimized
  , pattern WKWebExtensionWindowStateMaximized
  , pattern WKWebExtensionWindowStateFullscreen
  , WKWebExtensionWindowType(WKWebExtensionWindowType)
  , pattern WKWebExtensionWindowTypeNormal
  , pattern WKWebExtensionWindowTypePopup

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionWindowConfiguration)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionWindowConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO (Id WKWebExtensionWindowConfiguration)
init_ wkWebExtensionWindowConfiguration  =
  sendMsg wkWebExtensionWindowConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates the window type for the window.
--
-- ObjC selector: @- windowType@
windowType :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO WKWebExtensionWindowType
windowType wkWebExtensionWindowConfiguration  =
  fmap (coerce :: CLong -> WKWebExtensionWindowType) $ sendMsg wkWebExtensionWindowConfiguration (mkSelector "windowType") retCLong []

-- | Indicates the window state for the window.
--
-- ObjC selector: @- windowState@
windowState :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO WKWebExtensionWindowState
windowState wkWebExtensionWindowConfiguration  =
  fmap (coerce :: CLong -> WKWebExtensionWindowState) $ sendMsg wkWebExtensionWindowConfiguration (mkSelector "windowState") retCLong []

-- | Indicates the URLs that the window should initially load as tabs.
--
-- If ``tabURLs`` and ``tabs`` are both empty, the app's default "start page" should appear in a tab.
--
-- tabs
--
-- ObjC selector: @- tabURLs@
tabURLs :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO (Id NSArray)
tabURLs wkWebExtensionWindowConfiguration  =
  sendMsg wkWebExtensionWindowConfiguration (mkSelector "tabURLs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the window should be focused.
--
-- ObjC selector: @- shouldBeFocused@
shouldBeFocused :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO Bool
shouldBeFocused wkWebExtensionWindowConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionWindowConfiguration (mkSelector "shouldBeFocused") retCULong []

-- | Indicates whether the window should be private.
--
-- Note: To ensure proper isolation between private and non-private data, web views associated with private data must use a different ``WKUserContentController``. Likewise, to be identified as a private web view and to ensure that cookies and other website data is not shared, private web views must be configured to use a non-persistent ``WKWebsiteDataStore``.
--
-- ObjC selector: @- shouldBePrivate@
shouldBePrivate :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO Bool
shouldBePrivate wkWebExtensionWindowConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionWindowConfiguration (mkSelector "shouldBePrivate") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @windowType@
windowTypeSelector :: Selector
windowTypeSelector = mkSelector "windowType"

-- | @Selector@ for @windowState@
windowStateSelector :: Selector
windowStateSelector = mkSelector "windowState"

-- | @Selector@ for @tabURLs@
tabURLsSelector :: Selector
tabURLsSelector = mkSelector "tabURLs"

-- | @Selector@ for @shouldBeFocused@
shouldBeFocusedSelector :: Selector
shouldBeFocusedSelector = mkSelector "shouldBeFocused"

-- | @Selector@ for @shouldBePrivate@
shouldBePrivateSelector :: Selector
shouldBePrivateSelector = mkSelector "shouldBePrivate"

