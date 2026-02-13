{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionTabConfiguration`` object encapsulates configuration options for a tab in an extension.
--
-- This class holds various options that influence the behavior and initial state of a tab. The app retains the discretion to disregard any or all of these options, or even opt not to create a tab.
--
-- Generated bindings for @WKWebExtensionTabConfiguration@.
module ObjC.WebKit.WKWebExtensionTabConfiguration
  ( WKWebExtensionTabConfiguration
  , IsWKWebExtensionTabConfiguration(..)
  , new
  , init_
  , window
  , index
  , parentTab
  , url
  , shouldBeActive
  , shouldAddToSelection
  , shouldBePinned
  , shouldBeMuted
  , shouldReaderModeBeActive
  , indexSelector
  , initSelector
  , newSelector
  , parentTabSelector
  , shouldAddToSelectionSelector
  , shouldBeActiveSelector
  , shouldBeMutedSelector
  , shouldBePinnedSelector
  , shouldReaderModeBeActiveSelector
  , urlSelector
  , windowSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionTabConfiguration)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionTabConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO (Id WKWebExtensionTabConfiguration)
init_ wkWebExtensionTabConfiguration =
  sendOwnedMessage wkWebExtensionTabConfiguration initSelector

-- | Indicates the window where the tab should be opened.
--
-- If this property is @nil@, no window was specified.
--
-- ObjC selector: @- window@
window :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO RawId
window wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration windowSelector

-- | Indicates the position where the tab should be opened within the window.
--
-- ObjC selector: @- index@
index :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO CULong
index wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration indexSelector

-- | Indicates the parent tab with which the tab should be related.
--
-- If this property is @nil@, no parent tab was specified.
--
-- ObjC selector: @- parentTab@
parentTab :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO RawId
parentTab wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration parentTabSelector

-- | Indicates the initial URL for the tab.
--
-- If this property is @nil@, the app's default "start page" should appear in the tab.
--
-- ObjC selector: @- url@
url :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO (Id NSURL)
url wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration urlSelector

-- | Indicates whether the tab should be the active tab.
--
-- If this property is @YES@, the tab should be made active in the window, ensuring it is the frontmost tab. Being active implies the tab is also selected. If this property is @NO@, the tab shouldn't affect the currently active tab.
--
-- ObjC selector: @- shouldBeActive@
shouldBeActive :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldBeActive wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration shouldBeActiveSelector

-- | Indicates whether the tab should be added to the current tab selection.
--
-- If this property is @YES@, the tab should be part of the current selection, but not necessarily become the active tab unless ``shouldBeActive`` is also @YES@. If this property is @NO@, the tab shouldn't be part of the current selection.
--
-- ObjC selector: @- shouldAddToSelection@
shouldAddToSelection :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldAddToSelection wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration shouldAddToSelectionSelector

-- | Indicates whether the tab should be pinned.
--
-- ObjC selector: @- shouldBePinned@
shouldBePinned :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldBePinned wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration shouldBePinnedSelector

-- | Indicates whether the tab should be muted.
--
-- ObjC selector: @- shouldBeMuted@
shouldBeMuted :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldBeMuted wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration shouldBeMutedSelector

-- | Indicates whether reader mode in the tab should be active.
--
-- ObjC selector: @- shouldReaderModeBeActive@
shouldReaderModeBeActive :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldReaderModeBeActive wkWebExtensionTabConfiguration =
  sendMessage wkWebExtensionTabConfiguration shouldReaderModeBeActiveSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionTabConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionTabConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @window@
windowSelector :: Selector '[] RawId
windowSelector = mkSelector "window"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @parentTab@
parentTabSelector :: Selector '[] RawId
parentTabSelector = mkSelector "parentTab"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @shouldBeActive@
shouldBeActiveSelector :: Selector '[] Bool
shouldBeActiveSelector = mkSelector "shouldBeActive"

-- | @Selector@ for @shouldAddToSelection@
shouldAddToSelectionSelector :: Selector '[] Bool
shouldAddToSelectionSelector = mkSelector "shouldAddToSelection"

-- | @Selector@ for @shouldBePinned@
shouldBePinnedSelector :: Selector '[] Bool
shouldBePinnedSelector = mkSelector "shouldBePinned"

-- | @Selector@ for @shouldBeMuted@
shouldBeMutedSelector :: Selector '[] Bool
shouldBeMutedSelector = mkSelector "shouldBeMuted"

-- | @Selector@ for @shouldReaderModeBeActive@
shouldReaderModeBeActiveSelector :: Selector '[] Bool
shouldReaderModeBeActiveSelector = mkSelector "shouldReaderModeBeActive"

