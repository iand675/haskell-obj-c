{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASPickerDisplayItem@.
module ObjC.AccessorySetupKit.ASPickerDisplayItem
  ( ASPickerDisplayItem
  , IsASPickerDisplayItem(..)
  , initWithName_productImage_descriptor
  , init_
  , new
  , name
  , descriptor
  , renameOptions
  , setRenameOptions
  , setupOptions
  , setSetupOptions
  , descriptorSelector
  , initSelector
  , initWithName_productImage_descriptorSelector
  , nameSelector
  , newSelector
  , renameOptionsSelector
  , setRenameOptionsSelector
  , setSetupOptionsSelector
  , setupOptionsSelector

  -- * Enum types
  , ASAccessoryRenameOptions(ASAccessoryRenameOptions)
  , pattern ASAccessoryRenameSSID
  , ASPickerDisplayItemSetupOptions(ASPickerDisplayItemSetupOptions)
  , pattern ASPickerDisplayItemSetupRename
  , pattern ASPickerDisplayItemSetupConfirmAuthorization
  , pattern ASPickerDisplayItemSetupFinishInApp

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a picker display item with a name and image to display and a descriptor to match discovered accessories. - Parameters:   - name: The accessory name to display in the picker.   - productImage: An image of the accessory to display in the picker.   - descriptor: A descriptor that the picker uses to determine which discovered accessories to display.
--
-- ObjC selector: @- initWithName:productImage:descriptor:@
initWithName_productImage_descriptor :: (IsASPickerDisplayItem asPickerDisplayItem, IsNSString name, IsASDiscoveryDescriptor descriptor) => asPickerDisplayItem -> name -> RawId -> descriptor -> IO (Id ASPickerDisplayItem)
initWithName_productImage_descriptor asPickerDisplayItem name productImage descriptor =
  sendOwnedMessage asPickerDisplayItem initWithName_productImage_descriptorSelector (toNSString name) productImage (toASDiscoveryDescriptor descriptor)

-- | @- init@
init_ :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id ASPickerDisplayItem)
init_ asPickerDisplayItem =
  sendOwnedMessage asPickerDisplayItem initSelector

-- | @- new@
new :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id ASPickerDisplayItem)
new asPickerDisplayItem =
  sendOwnedMessage asPickerDisplayItem newSelector

-- | The accessory name to display in the picker.
--
-- ObjC selector: @- name@
name :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id NSString)
name asPickerDisplayItem =
  sendMessage asPickerDisplayItem nameSelector

-- | A descriptor that the picker uses to determine which discovered accessories to display.
--
-- ObjC selector: @- descriptor@
descriptor :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO (Id ASDiscoveryDescriptor)
descriptor asPickerDisplayItem =
  sendMessage asPickerDisplayItem descriptorSelector

-- | Options to allow renaming a matched accessory.
--
-- To permit renaming, include ``SetupOptions-swift.struct/rename`` in the ``setupOptions-c.property``
--
-- ObjC selector: @- renameOptions@
renameOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO ASAccessoryRenameOptions
renameOptions asPickerDisplayItem =
  sendMessage asPickerDisplayItem renameOptionsSelector

-- | Options to allow renaming a matched accessory.
--
-- To permit renaming, include ``SetupOptions-swift.struct/rename`` in the ``setupOptions-c.property``
--
-- ObjC selector: @- setRenameOptions:@
setRenameOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> ASAccessoryRenameOptions -> IO ()
setRenameOptions asPickerDisplayItem value =
  sendMessage asPickerDisplayItem setRenameOptionsSelector value

-- | Custom setup options for the accessory.
--
-- ObjC selector: @- setupOptions@
setupOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> IO ASPickerDisplayItemSetupOptions
setupOptions asPickerDisplayItem =
  sendMessage asPickerDisplayItem setupOptionsSelector

-- | Custom setup options for the accessory.
--
-- ObjC selector: @- setSetupOptions:@
setSetupOptions :: IsASPickerDisplayItem asPickerDisplayItem => asPickerDisplayItem -> ASPickerDisplayItemSetupOptions -> IO ()
setSetupOptions asPickerDisplayItem value =
  sendMessage asPickerDisplayItem setSetupOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:productImage:descriptor:@
initWithName_productImage_descriptorSelector :: Selector '[Id NSString, RawId, Id ASDiscoveryDescriptor] (Id ASPickerDisplayItem)
initWithName_productImage_descriptorSelector = mkSelector "initWithName:productImage:descriptor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPickerDisplayItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASPickerDisplayItem)
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id ASDiscoveryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @renameOptions@
renameOptionsSelector :: Selector '[] ASAccessoryRenameOptions
renameOptionsSelector = mkSelector "renameOptions"

-- | @Selector@ for @setRenameOptions:@
setRenameOptionsSelector :: Selector '[ASAccessoryRenameOptions] ()
setRenameOptionsSelector = mkSelector "setRenameOptions:"

-- | @Selector@ for @setupOptions@
setupOptionsSelector :: Selector '[] ASPickerDisplayItemSetupOptions
setupOptionsSelector = mkSelector "setupOptions"

-- | @Selector@ for @setSetupOptions:@
setSetupOptionsSelector :: Selector '[ASPickerDisplayItemSetupOptions] ()
setSetupOptionsSelector = mkSelector "setSetupOptions:"

