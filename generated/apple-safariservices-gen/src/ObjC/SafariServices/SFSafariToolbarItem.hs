{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariToolbarItem@.
module ObjC.SafariServices.SFSafariToolbarItem
  ( SFSafariToolbarItem
  , IsSFSafariToolbarItem(..)
  , new
  , init_
  , setEnabled_withBadgeText
  , setEnabled
  , setBadgeText
  , setImage
  , setLabel
  , showPopover
  , initSelector
  , newSelector
  , setBadgeTextSelector
  , setEnabledSelector
  , setEnabled_withBadgeTextSelector
  , setImageSelector
  , setLabelSelector
  , showPopoverSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFSafariToolbarItem)
new  =
  do
    cls' <- getRequiredClass "SFSafariToolbarItem"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFSafariToolbarItem sfSafariToolbarItem => sfSafariToolbarItem -> IO (Id SFSafariToolbarItem)
init_ sfSafariToolbarItem =
  sendOwnedMessage sfSafariToolbarItem initSelector

-- | Sets the enabled state and the badge text for the toolbar item. The badge text is visible even when the toolbar item is disabled.
--
-- ObjC selector: @- setEnabled:withBadgeText:@
setEnabled_withBadgeText :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSString badgeText) => sfSafariToolbarItem -> Bool -> badgeText -> IO ()
setEnabled_withBadgeText sfSafariToolbarItem enabled badgeText =
  sendMessage sfSafariToolbarItem setEnabled_withBadgeTextSelector enabled (toNSString badgeText)

-- | Sets the enabled state for the toolbar item.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFSafariToolbarItem sfSafariToolbarItem => sfSafariToolbarItem -> Bool -> IO ()
setEnabled sfSafariToolbarItem enabled =
  sendMessage sfSafariToolbarItem setEnabledSelector enabled

-- | Sets the badge text for the toolbar item. The badge text is visible even when the toolbar item is disabled. Setting the text to nil will clear the badge.
--
-- ObjC selector: @- setBadgeText:@
setBadgeText :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSString badgeText) => sfSafariToolbarItem -> badgeText -> IO ()
setBadgeText sfSafariToolbarItem badgeText =
  sendMessage sfSafariToolbarItem setBadgeTextSelector (toNSString badgeText)

-- | Sets the image for the toolbar item. Setting the image to nil will set the default image.
--
-- ObjC selector: @- setImage:@
setImage :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSImage image) => sfSafariToolbarItem -> image -> IO ()
setImage sfSafariToolbarItem image =
  sendMessage sfSafariToolbarItem setImageSelector (toNSImage image)

-- | Sets the label for the toolbar button. Setting the label to nil will set the default label.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSString label) => sfSafariToolbarItem -> label -> IO ()
setLabel sfSafariToolbarItem label =
  sendMessage sfSafariToolbarItem setLabelSelector (toNSString label)

-- | Shows the popover associated with this toolbar button.
--
-- ObjC selector: @- showPopover@
showPopover :: IsSFSafariToolbarItem sfSafariToolbarItem => sfSafariToolbarItem -> IO ()
showPopover sfSafariToolbarItem =
  sendMessage sfSafariToolbarItem showPopoverSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFSafariToolbarItem)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSafariToolbarItem)
initSelector = mkSelector "init"

-- | @Selector@ for @setEnabled:withBadgeText:@
setEnabled_withBadgeTextSelector :: Selector '[Bool, Id NSString] ()
setEnabled_withBadgeTextSelector = mkSelector "setEnabled:withBadgeText:"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @setBadgeText:@
setBadgeTextSelector :: Selector '[Id NSString] ()
setBadgeTextSelector = mkSelector "setBadgeText:"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @showPopover@
showPopoverSelector :: Selector '[] ()
showPopoverSelector = mkSelector "showPopover"

