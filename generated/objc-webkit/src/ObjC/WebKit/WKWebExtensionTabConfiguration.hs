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
  , index
  , url
  , shouldBeActive
  , shouldAddToSelection
  , shouldBePinned
  , shouldBeMuted
  , shouldReaderModeBeActive
  , newSelector
  , initSelector
  , indexSelector
  , urlSelector
  , shouldBeActiveSelector
  , shouldAddToSelectionSelector
  , shouldBePinnedSelector
  , shouldBeMutedSelector
  , shouldReaderModeBeActiveSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionTabConfiguration)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionTabConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO (Id WKWebExtensionTabConfiguration)
init_ wkWebExtensionTabConfiguration  =
  sendMsg wkWebExtensionTabConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates the position where the tab should be opened within the window.
--
-- ObjC selector: @- index@
index :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO CULong
index wkWebExtensionTabConfiguration  =
  sendMsg wkWebExtensionTabConfiguration (mkSelector "index") retCULong []

-- | Indicates the initial URL for the tab.
--
-- If this property is @nil@, the app's default "start page" should appear in the tab.
--
-- ObjC selector: @- url@
url :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO (Id NSURL)
url wkWebExtensionTabConfiguration  =
  sendMsg wkWebExtensionTabConfiguration (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the tab should be the active tab.
--
-- If this property is @YES@, the tab should be made active in the window, ensuring it is the frontmost tab. Being active implies the tab is also selected. If this property is @NO@, the tab shouldn't affect the currently active tab.
--
-- ObjC selector: @- shouldBeActive@
shouldBeActive :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldBeActive wkWebExtensionTabConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionTabConfiguration (mkSelector "shouldBeActive") retCULong []

-- | Indicates whether the tab should be added to the current tab selection.
--
-- If this property is @YES@, the tab should be part of the current selection, but not necessarily become the active tab unless ``shouldBeActive`` is also @YES@. If this property is @NO@, the tab shouldn't be part of the current selection.
--
-- ObjC selector: @- shouldAddToSelection@
shouldAddToSelection :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldAddToSelection wkWebExtensionTabConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionTabConfiguration (mkSelector "shouldAddToSelection") retCULong []

-- | Indicates whether the tab should be pinned.
--
-- ObjC selector: @- shouldBePinned@
shouldBePinned :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldBePinned wkWebExtensionTabConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionTabConfiguration (mkSelector "shouldBePinned") retCULong []

-- | Indicates whether the tab should be muted.
--
-- ObjC selector: @- shouldBeMuted@
shouldBeMuted :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldBeMuted wkWebExtensionTabConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionTabConfiguration (mkSelector "shouldBeMuted") retCULong []

-- | Indicates whether reader mode in the tab should be active.
--
-- ObjC selector: @- shouldReaderModeBeActive@
shouldReaderModeBeActive :: IsWKWebExtensionTabConfiguration wkWebExtensionTabConfiguration => wkWebExtensionTabConfiguration -> IO Bool
shouldReaderModeBeActive wkWebExtensionTabConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionTabConfiguration (mkSelector "shouldReaderModeBeActive") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @shouldBeActive@
shouldBeActiveSelector :: Selector
shouldBeActiveSelector = mkSelector "shouldBeActive"

-- | @Selector@ for @shouldAddToSelection@
shouldAddToSelectionSelector :: Selector
shouldAddToSelectionSelector = mkSelector "shouldAddToSelection"

-- | @Selector@ for @shouldBePinned@
shouldBePinnedSelector :: Selector
shouldBePinnedSelector = mkSelector "shouldBePinned"

-- | @Selector@ for @shouldBeMuted@
shouldBeMutedSelector :: Selector
shouldBeMutedSelector = mkSelector "shouldBeMuted"

-- | @Selector@ for @shouldReaderModeBeActive@
shouldReaderModeBeActiveSelector :: Selector
shouldReaderModeBeActiveSelector = mkSelector "shouldReaderModeBeActive"

