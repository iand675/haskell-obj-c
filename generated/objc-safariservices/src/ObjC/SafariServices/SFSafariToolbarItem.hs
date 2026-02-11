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
  , newSelector
  , initSelector
  , setEnabled_withBadgeTextSelector
  , setEnabledSelector
  , setBadgeTextSelector
  , setImageSelector
  , setLabelSelector
  , showPopoverSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFSafariToolbarItem)
new  =
  do
    cls' <- getRequiredClass "SFSafariToolbarItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFSafariToolbarItem sfSafariToolbarItem => sfSafariToolbarItem -> IO (Id SFSafariToolbarItem)
init_ sfSafariToolbarItem  =
  sendMsg sfSafariToolbarItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Sets the enabled state and the badge text for the toolbar item. The badge text is visible even when the toolbar item is disabled.
--
-- ObjC selector: @- setEnabled:withBadgeText:@
setEnabled_withBadgeText :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSString badgeText) => sfSafariToolbarItem -> Bool -> badgeText -> IO ()
setEnabled_withBadgeText sfSafariToolbarItem  enabled badgeText =
withObjCPtr badgeText $ \raw_badgeText ->
    sendMsg sfSafariToolbarItem (mkSelector "setEnabled:withBadgeText:") retVoid [argCULong (if enabled then 1 else 0), argPtr (castPtr raw_badgeText :: Ptr ())]

-- | Sets the enabled state for the toolbar item.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFSafariToolbarItem sfSafariToolbarItem => sfSafariToolbarItem -> Bool -> IO ()
setEnabled sfSafariToolbarItem  enabled =
  sendMsg sfSafariToolbarItem (mkSelector "setEnabled:") retVoid [argCULong (if enabled then 1 else 0)]

-- | Sets the badge text for the toolbar item. The badge text is visible even when the toolbar item is disabled. Setting the text to nil will clear the badge.
--
-- ObjC selector: @- setBadgeText:@
setBadgeText :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSString badgeText) => sfSafariToolbarItem -> badgeText -> IO ()
setBadgeText sfSafariToolbarItem  badgeText =
withObjCPtr badgeText $ \raw_badgeText ->
    sendMsg sfSafariToolbarItem (mkSelector "setBadgeText:") retVoid [argPtr (castPtr raw_badgeText :: Ptr ())]

-- | Sets the image for the toolbar item. Setting the image to nil will set the default image.
--
-- ObjC selector: @- setImage:@
setImage :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSImage image) => sfSafariToolbarItem -> image -> IO ()
setImage sfSafariToolbarItem  image =
withObjCPtr image $ \raw_image ->
    sendMsg sfSafariToolbarItem (mkSelector "setImage:") retVoid [argPtr (castPtr raw_image :: Ptr ())]

-- | Sets the label for the toolbar button. Setting the label to nil will set the default label.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsSFSafariToolbarItem sfSafariToolbarItem, IsNSString label) => sfSafariToolbarItem -> label -> IO ()
setLabel sfSafariToolbarItem  label =
withObjCPtr label $ \raw_label ->
    sendMsg sfSafariToolbarItem (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_label :: Ptr ())]

-- | Shows the popover associated with this toolbar button.
--
-- ObjC selector: @- showPopover@
showPopover :: IsSFSafariToolbarItem sfSafariToolbarItem => sfSafariToolbarItem -> IO ()
showPopover sfSafariToolbarItem  =
  sendMsg sfSafariToolbarItem (mkSelector "showPopover") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setEnabled:withBadgeText:@
setEnabled_withBadgeTextSelector :: Selector
setEnabled_withBadgeTextSelector = mkSelector "setEnabled:withBadgeText:"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @setBadgeText:@
setBadgeTextSelector :: Selector
setBadgeTextSelector = mkSelector "setBadgeText:"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @showPopover@
showPopoverSelector :: Selector
showPopoverSelector = mkSelector "showPopover"

