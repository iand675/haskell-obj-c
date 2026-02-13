{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A picker display item created from customizing a discovered accessory.
--
-- Use this type when your app's picker uses the ``ASPickerDisplaySettings/Options/filterDiscoveryResults`` option. With this option enabled, your discovery session receives ``ASAccessoryEventType/accessoryDiscovered`` events with discovered accessories. To include a discovered accessory in the picker, create an instance of this class, optionally using the Bluetooth properties of the event's ``ASDiscoveredAccessory`` to provide a more specific name or product image. Then send the @ASDiscoveredDisplayItem@ to the picker with the session's ``ASAccessorySession/updatePicker(showing:completionHandler:)`` method.
--
-- Generated bindings for @ASDiscoveredDisplayItem@.
module ObjC.AccessorySetupKit.ASDiscoveredDisplayItem
  ( ASDiscoveredDisplayItem
  , IsASDiscoveredDisplayItem(..)
  , initWithName_productImage_accessory
  , initWithName_productImage_descriptor
  , initWithName_productImage_accessorySelector
  , initWithName_productImage_descriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a discovered picker display item with a name and image to display and a descriptor to match discovered accessories. - Parameters:   - name: The accessory name to display in the picker.   - productImage: An image of the accessory to display in the picker.   - accessory: App filtered accessory to display in the picker.
--
-- ObjC selector: @- initWithName:productImage:accessory:@
initWithName_productImage_accessory :: (IsASDiscoveredDisplayItem asDiscoveredDisplayItem, IsNSString name, IsASDiscoveredAccessory accessory) => asDiscoveredDisplayItem -> name -> RawId -> accessory -> IO (Id ASDiscoveredDisplayItem)
initWithName_productImage_accessory asDiscoveredDisplayItem name productImage accessory =
  sendOwnedMessage asDiscoveredDisplayItem initWithName_productImage_accessorySelector (toNSString name) productImage (toASDiscoveredAccessory accessory)

-- | @- initWithName:productImage:descriptor:@
initWithName_productImage_descriptor :: (IsASDiscoveredDisplayItem asDiscoveredDisplayItem, IsNSString name, IsASDiscoveryDescriptor descriptor) => asDiscoveredDisplayItem -> name -> RawId -> descriptor -> IO (Id ASDiscoveredDisplayItem)
initWithName_productImage_descriptor asDiscoveredDisplayItem name productImage descriptor =
  sendOwnedMessage asDiscoveredDisplayItem initWithName_productImage_descriptorSelector (toNSString name) productImage (toASDiscoveryDescriptor descriptor)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:productImage:accessory:@
initWithName_productImage_accessorySelector :: Selector '[Id NSString, RawId, Id ASDiscoveredAccessory] (Id ASDiscoveredDisplayItem)
initWithName_productImage_accessorySelector = mkSelector "initWithName:productImage:accessory:"

-- | @Selector@ for @initWithName:productImage:descriptor:@
initWithName_productImage_descriptorSelector :: Selector '[Id NSString, RawId, Id ASDiscoveryDescriptor] (Id ASDiscoveredDisplayItem)
initWithName_productImage_descriptorSelector = mkSelector "initWithName:productImage:descriptor:"

