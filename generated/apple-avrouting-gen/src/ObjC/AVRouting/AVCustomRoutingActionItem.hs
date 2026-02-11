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
  , typeSelector
  , setTypeSelector
  , overrideTitleSelector
  , setOverrideTitleSelector


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

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A type with an identifier that matches a value in the app’s configuration.
--
-- Provide a @UTType@ symbol name and description in your app’s @Info.plist@ file.
--
-- ObjC selector: @- type@
type_ :: IsAVCustomRoutingActionItem avCustomRoutingActionItem => avCustomRoutingActionItem -> IO RawId
type_ avCustomRoutingActionItem  =
    fmap (RawId . castPtr) $ sendMsg avCustomRoutingActionItem (mkSelector "type") (retPtr retVoid) []

-- | A type with an identifier that matches a value in the app’s configuration.
--
-- Provide a @UTType@ symbol name and description in your app’s @Info.plist@ file.
--
-- ObjC selector: @- setType:@
setType :: IsAVCustomRoutingActionItem avCustomRoutingActionItem => avCustomRoutingActionItem -> RawId -> IO ()
setType avCustomRoutingActionItem  value =
    sendMsg avCustomRoutingActionItem (mkSelector "setType:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A string to use to override the title of the item’s type.
--
-- Use this value to dynamically override the title of the custom item.
--
-- ObjC selector: @- overrideTitle@
overrideTitle :: IsAVCustomRoutingActionItem avCustomRoutingActionItem => avCustomRoutingActionItem -> IO (Id NSString)
overrideTitle avCustomRoutingActionItem  =
    sendMsg avCustomRoutingActionItem (mkSelector "overrideTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A string to use to override the title of the item’s type.
--
-- Use this value to dynamically override the title of the custom item.
--
-- ObjC selector: @- setOverrideTitle:@
setOverrideTitle :: (IsAVCustomRoutingActionItem avCustomRoutingActionItem, IsNSString value) => avCustomRoutingActionItem -> value -> IO ()
setOverrideTitle avCustomRoutingActionItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCustomRoutingActionItem (mkSelector "setOverrideTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @overrideTitle@
overrideTitleSelector :: Selector
overrideTitleSelector = mkSelector "overrideTitle"

-- | @Selector@ for @setOverrideTitle:@
setOverrideTitleSelector :: Selector
setOverrideTitleSelector = mkSelector "setOverrideTitle:"

