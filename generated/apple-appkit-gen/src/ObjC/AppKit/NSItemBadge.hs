{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @NSItemBadge@ represents a badge that can be attached to an @NSToolbarItem@.
--
-- This badge provides a way to display small visual indicators, such as counts and text labels, within a toolbar item. Badges can be used to highlight important information, such as unread notifications or status indicators.
--
-- Generated bindings for @NSItemBadge@.
module ObjC.AppKit.NSItemBadge
  ( NSItemBadge
  , IsNSItemBadge(..)
  , badgeWithCount
  , badgeWithText
  , indicatorBadge
  , text
  , badgeWithCountSelector
  , badgeWithTextSelector
  , indicatorBadgeSelector
  , textSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a badge displaying a localized numerical count.
--
-- @count@ — The integer value to localize and display in the badge.
--
-- Returns: A new NSItemBadge instance with the localized specified count.
--
-- ObjC selector: @+ badgeWithCount:@
badgeWithCount :: CLong -> IO (Id NSItemBadge)
badgeWithCount count =
  do
    cls' <- getRequiredClass "NSItemBadge"
    sendClassMessage cls' badgeWithCountSelector count

-- | Creates a badge displaying a text.
--
-- @text@ — The text to be displayed inside the badge.
--
-- Returns: A new @NSItemBadge@ instance with the specified text.
--
-- ObjC selector: @+ badgeWithText:@
badgeWithText :: IsNSString text => text -> IO (Id NSItemBadge)
badgeWithText text =
  do
    cls' <- getRequiredClass "NSItemBadge"
    sendClassMessage cls' badgeWithTextSelector (toNSString text)

-- | Creates a badge styled as an indicator. In this context, an indicator is simply a badge without any text.
--
-- Returns: A new @NSItemBadge@ instance styled as an indicator.
--
-- ObjC selector: @+ indicatorBadge@
indicatorBadge :: IO (Id NSItemBadge)
indicatorBadge  =
  do
    cls' <- getRequiredClass "NSItemBadge"
    sendClassMessage cls' indicatorBadgeSelector

-- | The text to be displayed within the badge.
--
-- ObjC selector: @- text@
text :: IsNSItemBadge nsItemBadge => nsItemBadge -> IO (Id NSString)
text nsItemBadge =
  sendMessage nsItemBadge textSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @badgeWithCount:@
badgeWithCountSelector :: Selector '[CLong] (Id NSItemBadge)
badgeWithCountSelector = mkSelector "badgeWithCount:"

-- | @Selector@ for @badgeWithText:@
badgeWithTextSelector :: Selector '[Id NSString] (Id NSItemBadge)
badgeWithTextSelector = mkSelector "badgeWithText:"

-- | @Selector@ for @indicatorBadge@
indicatorBadgeSelector :: Selector '[] (Id NSItemBadge)
indicatorBadgeSelector = mkSelector "indicatorBadge"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

