{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A badge used to provide additional quantitative information specific to the menu item, such as the number of available updates.
--
-- Generated bindings for @NSMenuItemBadge@.
module ObjC.AppKit.NSMenuItemBadge
  ( NSMenuItemBadge
  , IsNSMenuItemBadge(..)
  , updatesWithCount
  , newItemsWithCount
  , alertsWithCount
  , initWithCount_type
  , initWithCount
  , initWithString
  , init_
  , itemCount
  , type_
  , stringValue
  , alertsWithCountSelector
  , initSelector
  , initWithCountSelector
  , initWithCount_typeSelector
  , initWithStringSelector
  , itemCountSelector
  , newItemsWithCountSelector
  , stringValueSelector
  , typeSelector
  , updatesWithCountSelector

  -- * Enum types
  , NSMenuItemBadgeType(NSMenuItemBadgeType)
  , pattern NSMenuItemBadgeTypeNone
  , pattern NSMenuItemBadgeTypeUpdates
  , pattern NSMenuItemBadgeTypeNewItems
  , pattern NSMenuItemBadgeTypeAlerts

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a badge with an integer count and a label representing the number of available updates.
--
-- ObjC selector: @+ updatesWithCount:@
updatesWithCount :: CLong -> IO (Id NSMenuItemBadge)
updatesWithCount itemCount =
  do
    cls' <- getRequiredClass "NSMenuItemBadge"
    sendClassMessage cls' updatesWithCountSelector itemCount

-- | Creates a badge with an integer count and a label representing the number of new items.
--
-- ObjC selector: @+ newItemsWithCount:@
newItemsWithCount :: CLong -> IO (Id NSMenuItemBadge)
newItemsWithCount itemCount =
  do
    cls' <- getRequiredClass "NSMenuItemBadge"
    sendOwnedClassMessage cls' newItemsWithCountSelector itemCount

-- | Creates a badge with an integer count and a label representing the number of alerts.
--
-- ObjC selector: @+ alertsWithCount:@
alertsWithCount :: CLong -> IO (Id NSMenuItemBadge)
alertsWithCount itemCount =
  do
    cls' <- getRequiredClass "NSMenuItemBadge"
    sendClassMessage cls' alertsWithCountSelector itemCount

-- | Initializes the badge with a count and a pre-defined badge type.
--
-- ObjC selector: @- initWithCount:type:@
initWithCount_type :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> CLong -> NSMenuItemBadgeType -> IO (Id NSMenuItemBadge)
initWithCount_type nsMenuItemBadge itemCount type_ =
  sendOwnedMessage nsMenuItemBadge initWithCount_typeSelector itemCount type_

-- | Initializes the badge with an integer count and an empty string.
--
-- ObjC selector: @- initWithCount:@
initWithCount :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> CLong -> IO (Id NSMenuItemBadge)
initWithCount nsMenuItemBadge itemCount =
  sendOwnedMessage nsMenuItemBadge initWithCountSelector itemCount

-- | Initializes the badge with the provided custom string.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsNSMenuItemBadge nsMenuItemBadge, IsNSString string) => nsMenuItemBadge -> string -> IO (Id NSMenuItemBadge)
initWithString nsMenuItemBadge string =
  sendOwnedMessage nsMenuItemBadge initWithStringSelector (toNSString string)

-- | @- init@
init_ :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO (Id NSMenuItemBadge)
init_ nsMenuItemBadge =
  sendOwnedMessage nsMenuItemBadge initSelector

-- | The count of items the badge displays. If a custom string was used to create a badge, the value is 0.
--
-- ObjC selector: @- itemCount@
itemCount :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO CLong
itemCount nsMenuItemBadge =
  sendMessage nsMenuItemBadge itemCountSelector

-- | The type of items the badge displays. If a custom string was used to create a badge, this value is @NSMenuItemBadgeTypeNone.@
--
-- ObjC selector: @- type@
type_ :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO NSMenuItemBadgeType
type_ nsMenuItemBadge =
  sendMessage nsMenuItemBadge typeSelector

-- | The string representation of the badge as it would appear when the badge is displayed.
--
-- ObjC selector: @- stringValue@
stringValue :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO (Id NSString)
stringValue nsMenuItemBadge =
  sendMessage nsMenuItemBadge stringValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatesWithCount:@
updatesWithCountSelector :: Selector '[CLong] (Id NSMenuItemBadge)
updatesWithCountSelector = mkSelector "updatesWithCount:"

-- | @Selector@ for @newItemsWithCount:@
newItemsWithCountSelector :: Selector '[CLong] (Id NSMenuItemBadge)
newItemsWithCountSelector = mkSelector "newItemsWithCount:"

-- | @Selector@ for @alertsWithCount:@
alertsWithCountSelector :: Selector '[CLong] (Id NSMenuItemBadge)
alertsWithCountSelector = mkSelector "alertsWithCount:"

-- | @Selector@ for @initWithCount:type:@
initWithCount_typeSelector :: Selector '[CLong, NSMenuItemBadgeType] (Id NSMenuItemBadge)
initWithCount_typeSelector = mkSelector "initWithCount:type:"

-- | @Selector@ for @initWithCount:@
initWithCountSelector :: Selector '[CLong] (Id NSMenuItemBadge)
initWithCountSelector = mkSelector "initWithCount:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSMenuItemBadge)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMenuItemBadge)
initSelector = mkSelector "init"

-- | @Selector@ for @itemCount@
itemCountSelector :: Selector '[] CLong
itemCountSelector = mkSelector "itemCount"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSMenuItemBadgeType
typeSelector = mkSelector "type"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

