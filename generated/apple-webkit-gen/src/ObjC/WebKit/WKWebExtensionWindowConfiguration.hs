{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , tabs
  , shouldBeFocused
  , shouldBePrivate
  , initSelector
  , newSelector
  , shouldBeFocusedSelector
  , shouldBePrivateSelector
  , tabURLsSelector
  , tabsSelector
  , windowStateSelector
  , windowTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO (Id WKWebExtensionWindowConfiguration)
init_ wkWebExtensionWindowConfiguration =
  sendOwnedMessage wkWebExtensionWindowConfiguration initSelector

-- | Indicates the window type for the window.
--
-- ObjC selector: @- windowType@
windowType :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO WKWebExtensionWindowType
windowType wkWebExtensionWindowConfiguration =
  sendMessage wkWebExtensionWindowConfiguration windowTypeSelector

-- | Indicates the window state for the window.
--
-- ObjC selector: @- windowState@
windowState :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO WKWebExtensionWindowState
windowState wkWebExtensionWindowConfiguration =
  sendMessage wkWebExtensionWindowConfiguration windowStateSelector

-- | Indicates the URLs that the window should initially load as tabs.
--
-- If ``tabURLs`` and ``tabs`` are both empty, the app's default "start page" should appear in a tab.
--
-- tabs
--
-- ObjC selector: @- tabURLs@
tabURLs :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO (Id NSArray)
tabURLs wkWebExtensionWindowConfiguration =
  sendMessage wkWebExtensionWindowConfiguration tabURLsSelector

-- | Indicates the existing tabs that should be moved to the window.
--
-- If ``tabs`` and ``tabURLs`` are both empty, the app's default "start page" should appear in a tab.
--
-- tabURLs
--
-- ObjC selector: @- tabs@
tabs :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO (Id NSArray)
tabs wkWebExtensionWindowConfiguration =
  sendMessage wkWebExtensionWindowConfiguration tabsSelector

-- | Indicates whether the window should be focused.
--
-- ObjC selector: @- shouldBeFocused@
shouldBeFocused :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO Bool
shouldBeFocused wkWebExtensionWindowConfiguration =
  sendMessage wkWebExtensionWindowConfiguration shouldBeFocusedSelector

-- | Indicates whether the window should be private.
--
-- Note: To ensure proper isolation between private and non-private data, web views associated with private data must use a different ``WKUserContentController``. Likewise, to be identified as a private web view and to ensure that cookies and other website data is not shared, private web views must be configured to use a non-persistent ``WKWebsiteDataStore``.
--
-- ObjC selector: @- shouldBePrivate@
shouldBePrivate :: IsWKWebExtensionWindowConfiguration wkWebExtensionWindowConfiguration => wkWebExtensionWindowConfiguration -> IO Bool
shouldBePrivate wkWebExtensionWindowConfiguration =
  sendMessage wkWebExtensionWindowConfiguration shouldBePrivateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionWindowConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionWindowConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @windowType@
windowTypeSelector :: Selector '[] WKWebExtensionWindowType
windowTypeSelector = mkSelector "windowType"

-- | @Selector@ for @windowState@
windowStateSelector :: Selector '[] WKWebExtensionWindowState
windowStateSelector = mkSelector "windowState"

-- | @Selector@ for @tabURLs@
tabURLsSelector :: Selector '[] (Id NSArray)
tabURLsSelector = mkSelector "tabURLs"

-- | @Selector@ for @tabs@
tabsSelector :: Selector '[] (Id NSArray)
tabsSelector = mkSelector "tabs"

-- | @Selector@ for @shouldBeFocused@
shouldBeFocusedSelector :: Selector '[] Bool
shouldBeFocusedSelector = mkSelector "shouldBeFocused"

-- | @Selector@ for @shouldBePrivate@
shouldBePrivateSelector :: Selector '[] Bool
shouldBePrivateSelector = mkSelector "shouldBePrivate"

