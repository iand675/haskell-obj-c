{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTrackingSeparatorToolbarItem@.
module ObjC.AppKit.NSTrackingSeparatorToolbarItem
  ( NSTrackingSeparatorToolbarItem
  , IsNSTrackingSeparatorToolbarItem(..)
  , trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndex
  , splitView
  , setSplitView
  , dividerIndex
  , setDividerIndex
  , dividerIndexSelector
  , setDividerIndexSelector
  , setSplitViewSelector
  , splitViewSelector
  , trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new @NSTrackingSeparatorToolbarItem@ and configures it to attempt to visually align with the specified divider of the @splitView@. The @splitView@ must be in the same window as the toolbar containing this item by the time the toolbar is shown. Only vertical @splitViews@ are supported.
--
-- ObjC selector: @+ trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:@
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndex :: (IsNSString identifier, IsNSSplitView splitView) => identifier -> splitView -> CLong -> IO (Id NSTrackingSeparatorToolbarItem)
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndex identifier splitView dividerIndex =
  do
    cls' <- getRequiredClass "NSTrackingSeparatorToolbarItem"
    sendClassMessage cls' trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector (toNSString identifier) (toNSSplitView splitView) dividerIndex

-- | The @splitView@ must be in the same window as the toolbar containing this item by the time the toolbar is shown. Only vertical @splitViews@ are supported.
--
-- ObjC selector: @- splitView@
splitView :: IsNSTrackingSeparatorToolbarItem nsTrackingSeparatorToolbarItem => nsTrackingSeparatorToolbarItem -> IO (Id NSSplitView)
splitView nsTrackingSeparatorToolbarItem =
  sendMessage nsTrackingSeparatorToolbarItem splitViewSelector

-- | The @splitView@ must be in the same window as the toolbar containing this item by the time the toolbar is shown. Only vertical @splitViews@ are supported.
--
-- ObjC selector: @- setSplitView:@
setSplitView :: (IsNSTrackingSeparatorToolbarItem nsTrackingSeparatorToolbarItem, IsNSSplitView value) => nsTrackingSeparatorToolbarItem -> value -> IO ()
setSplitView nsTrackingSeparatorToolbarItem value =
  sendMessage nsTrackingSeparatorToolbarItem setSplitViewSelector (toNSSplitView value)

-- | The specific divider of the @splitView@ which will be tracked.
--
-- ObjC selector: @- dividerIndex@
dividerIndex :: IsNSTrackingSeparatorToolbarItem nsTrackingSeparatorToolbarItem => nsTrackingSeparatorToolbarItem -> IO CLong
dividerIndex nsTrackingSeparatorToolbarItem =
  sendMessage nsTrackingSeparatorToolbarItem dividerIndexSelector

-- | The specific divider of the @splitView@ which will be tracked.
--
-- ObjC selector: @- setDividerIndex:@
setDividerIndex :: IsNSTrackingSeparatorToolbarItem nsTrackingSeparatorToolbarItem => nsTrackingSeparatorToolbarItem -> CLong -> IO ()
setDividerIndex nsTrackingSeparatorToolbarItem value =
  sendMessage nsTrackingSeparatorToolbarItem setDividerIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:@
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector :: Selector '[Id NSString, Id NSSplitView, CLong] (Id NSTrackingSeparatorToolbarItem)
trackingSeparatorToolbarItemWithIdentifier_splitView_dividerIndexSelector = mkSelector "trackingSeparatorToolbarItemWithIdentifier:splitView:dividerIndex:"

-- | @Selector@ for @splitView@
splitViewSelector :: Selector '[] (Id NSSplitView)
splitViewSelector = mkSelector "splitView"

-- | @Selector@ for @setSplitView:@
setSplitViewSelector :: Selector '[Id NSSplitView] ()
setSplitViewSelector = mkSelector "setSplitView:"

-- | @Selector@ for @dividerIndex@
dividerIndexSelector :: Selector '[] CLong
dividerIndexSelector = mkSelector "dividerIndex"

-- | @Selector@ for @setDividerIndex:@
setDividerIndexSelector :: Selector '[CLong] ()
setDividerIndexSelector = mkSelector "setDividerIndex:"

