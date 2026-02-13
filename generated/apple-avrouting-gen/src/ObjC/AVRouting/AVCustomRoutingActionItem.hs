{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a custom action item to display in a device route picker.
--
-- Use this class to specify supplemental action items to display in the list of discovered routes. Tapping a custom item dismisses the picker and calls the ``AVCustomRoutingControllerDelegate/customRoutingController:didSelectItem:`` method of ``AVCustomRoutingControllerDelegate``.
--
-- Generated bindings for @AVCustomRoutingActionItem@.
module ObjC.AVRouting.AVCustomRoutingActionItem
  ( AVCustomRoutingActionItem
  , IsAVCustomRoutingActionItem(..)
  , type_
  , setType
  , overrideTitle
  , setOverrideTitle
  , overrideTitleSelector
  , setOverrideTitleSelector
  , setTypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A type with an identifier that matches a value in the app’s configuration.
--
-- Provide a @UTType@ symbol name and description in your app’s @Info.plist@ file.
--
-- ObjC selector: @- type@
type_ :: IsAVCustomRoutingActionItem avCustomRoutingActionItem => avCustomRoutingActionItem -> IO RawId
type_ avCustomRoutingActionItem =
  sendMessage avCustomRoutingActionItem typeSelector

-- | A type with an identifier that matches a value in the app’s configuration.
--
-- Provide a @UTType@ symbol name and description in your app’s @Info.plist@ file.
--
-- ObjC selector: @- setType:@
setType :: IsAVCustomRoutingActionItem avCustomRoutingActionItem => avCustomRoutingActionItem -> RawId -> IO ()
setType avCustomRoutingActionItem value =
  sendMessage avCustomRoutingActionItem setTypeSelector value

-- | A string to use to override the title of the item’s type.
--
-- Use this value to dynamically override the title of the custom item.
--
-- ObjC selector: @- overrideTitle@
overrideTitle :: IsAVCustomRoutingActionItem avCustomRoutingActionItem => avCustomRoutingActionItem -> IO (Id NSString)
overrideTitle avCustomRoutingActionItem =
  sendMessage avCustomRoutingActionItem overrideTitleSelector

-- | A string to use to override the title of the item’s type.
--
-- Use this value to dynamically override the title of the custom item.
--
-- ObjC selector: @- setOverrideTitle:@
setOverrideTitle :: (IsAVCustomRoutingActionItem avCustomRoutingActionItem, IsNSString value) => avCustomRoutingActionItem -> value -> IO ()
setOverrideTitle avCustomRoutingActionItem value =
  sendMessage avCustomRoutingActionItem setOverrideTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] RawId
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[RawId] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @overrideTitle@
overrideTitleSelector :: Selector '[] (Id NSString)
overrideTitleSelector = mkSelector "overrideTitle"

-- | @Selector@ for @setOverrideTitle:@
setOverrideTitleSelector :: Selector '[Id NSString] ()
setOverrideTitleSelector = mkSelector "setOverrideTitle:"

