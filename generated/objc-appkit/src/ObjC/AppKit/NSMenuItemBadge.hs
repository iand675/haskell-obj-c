{-# LANGUAGE PatternSynonyms #-}
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
  , updatesWithCountSelector
  , newItemsWithCountSelector
  , alertsWithCountSelector
  , initWithCount_typeSelector
  , initWithCountSelector
  , initWithStringSelector
  , initSelector
  , itemCountSelector
  , typeSelector
  , stringValueSelector

  -- * Enum types
  , NSMenuItemBadgeType(NSMenuItemBadgeType)
  , pattern NSMenuItemBadgeTypeNone
  , pattern NSMenuItemBadgeTypeUpdates
  , pattern NSMenuItemBadgeTypeNewItems
  , pattern NSMenuItemBadgeTypeAlerts

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
    sendClassMsg cls' (mkSelector "updatesWithCount:") (retPtr retVoid) [argCLong (fromIntegral itemCount)] >>= retainedObject . castPtr

-- | Creates a badge with an integer count and a label representing the number of new items.
--
-- ObjC selector: @+ newItemsWithCount:@
newItemsWithCount :: CLong -> IO (Id NSMenuItemBadge)
newItemsWithCount itemCount =
  do
    cls' <- getRequiredClass "NSMenuItemBadge"
    sendClassMsg cls' (mkSelector "newItemsWithCount:") (retPtr retVoid) [argCLong (fromIntegral itemCount)] >>= ownedObject . castPtr

-- | Creates a badge with an integer count and a label representing the number of alerts.
--
-- ObjC selector: @+ alertsWithCount:@
alertsWithCount :: CLong -> IO (Id NSMenuItemBadge)
alertsWithCount itemCount =
  do
    cls' <- getRequiredClass "NSMenuItemBadge"
    sendClassMsg cls' (mkSelector "alertsWithCount:") (retPtr retVoid) [argCLong (fromIntegral itemCount)] >>= retainedObject . castPtr

-- | Initializes the badge with a count and a pre-defined badge type.
--
-- ObjC selector: @- initWithCount:type:@
initWithCount_type :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> CLong -> NSMenuItemBadgeType -> IO (Id NSMenuItemBadge)
initWithCount_type nsMenuItemBadge  itemCount type_ =
  sendMsg nsMenuItemBadge (mkSelector "initWithCount:type:") (retPtr retVoid) [argCLong (fromIntegral itemCount), argCLong (coerce type_)] >>= ownedObject . castPtr

-- | Initializes the badge with an integer count and an empty string.
--
-- ObjC selector: @- initWithCount:@
initWithCount :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> CLong -> IO (Id NSMenuItemBadge)
initWithCount nsMenuItemBadge  itemCount =
  sendMsg nsMenuItemBadge (mkSelector "initWithCount:") (retPtr retVoid) [argCLong (fromIntegral itemCount)] >>= ownedObject . castPtr

-- | Initializes the badge with the provided custom string.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsNSMenuItemBadge nsMenuItemBadge, IsNSString string) => nsMenuItemBadge -> string -> IO (Id NSMenuItemBadge)
initWithString nsMenuItemBadge  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsMenuItemBadge (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO (Id NSMenuItemBadge)
init_ nsMenuItemBadge  =
  sendMsg nsMenuItemBadge (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The count of items the badge displays. If a custom string was used to create a badge, the value is 0.
--
-- ObjC selector: @- itemCount@
itemCount :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO CLong
itemCount nsMenuItemBadge  =
  sendMsg nsMenuItemBadge (mkSelector "itemCount") retCLong []

-- | The type of items the badge displays. If a custom string was used to create a badge, this value is @NSMenuItemBadgeTypeNone.@
--
-- ObjC selector: @- type@
type_ :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO NSMenuItemBadgeType
type_ nsMenuItemBadge  =
  fmap (coerce :: CLong -> NSMenuItemBadgeType) $ sendMsg nsMenuItemBadge (mkSelector "type") retCLong []

-- | The string representation of the badge as it would appear when the badge is displayed.
--
-- ObjC selector: @- stringValue@
stringValue :: IsNSMenuItemBadge nsMenuItemBadge => nsMenuItemBadge -> IO (Id NSString)
stringValue nsMenuItemBadge  =
  sendMsg nsMenuItemBadge (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatesWithCount:@
updatesWithCountSelector :: Selector
updatesWithCountSelector = mkSelector "updatesWithCount:"

-- | @Selector@ for @newItemsWithCount:@
newItemsWithCountSelector :: Selector
newItemsWithCountSelector = mkSelector "newItemsWithCount:"

-- | @Selector@ for @alertsWithCount:@
alertsWithCountSelector :: Selector
alertsWithCountSelector = mkSelector "alertsWithCount:"

-- | @Selector@ for @initWithCount:type:@
initWithCount_typeSelector :: Selector
initWithCount_typeSelector = mkSelector "initWithCount:type:"

-- | @Selector@ for @initWithCount:@
initWithCountSelector :: Selector
initWithCountSelector = mkSelector "initWithCount:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @itemCount@
itemCountSelector :: Selector
itemCountSelector = mkSelector "itemCount"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

