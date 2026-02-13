{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPreviewRepresentingActivityItem@.
module ObjC.AppKit.NSPreviewRepresentingActivityItem
  ( NSPreviewRepresentingActivityItem
  , IsNSPreviewRepresentingActivityItem(..)
  , initWithItem_title_image_icon
  , initWithItem_title_imageProvider_iconProvider
  , init_
  , new
  , initSelector
  , initWithItem_title_imageProvider_iconProviderSelector
  , initWithItem_title_image_iconSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | - Parameters:     - item: The item to share     - title: A title to show in a preview     - image: An image to show in a preview     - icon: An icon to show in a preview
--
-- For more information about the parameters, see NSPreviewRepresentableActivityItem documentation
--
-- ObjC selector: @- initWithItem:title:image:icon:@
initWithItem_title_image_icon :: (IsNSPreviewRepresentingActivityItem nsPreviewRepresentingActivityItem, IsNSString title, IsNSImage image, IsNSImage icon) => nsPreviewRepresentingActivityItem -> RawId -> title -> image -> icon -> IO (Id NSPreviewRepresentingActivityItem)
initWithItem_title_image_icon nsPreviewRepresentingActivityItem item title image icon =
  sendOwnedMessage nsPreviewRepresentingActivityItem initWithItem_title_image_iconSelector item (toNSString title) (toNSImage image) (toNSImage icon)

-- | - Parameters:     - item: The item to share     - title: A title to show in a preview     - imageProvider: An NSItemProvider which provides an image to show in a preview     - iconProvider: An NSItemProvider which provides an icon to show in a preview
--
-- For more information about the parameters, see NSPreviewRepresentableActivityItem documentation
--
-- ObjC selector: @- initWithItem:title:imageProvider:iconProvider:@
initWithItem_title_imageProvider_iconProvider :: (IsNSPreviewRepresentingActivityItem nsPreviewRepresentingActivityItem, IsNSString title, IsNSItemProvider imageProvider, IsNSItemProvider iconProvider) => nsPreviewRepresentingActivityItem -> RawId -> title -> imageProvider -> iconProvider -> IO (Id NSPreviewRepresentingActivityItem)
initWithItem_title_imageProvider_iconProvider nsPreviewRepresentingActivityItem item title imageProvider iconProvider =
  sendOwnedMessage nsPreviewRepresentingActivityItem initWithItem_title_imageProvider_iconProviderSelector item (toNSString title) (toNSItemProvider imageProvider) (toNSItemProvider iconProvider)

-- | @- init@
init_ :: IsNSPreviewRepresentingActivityItem nsPreviewRepresentingActivityItem => nsPreviewRepresentingActivityItem -> IO (Id NSPreviewRepresentingActivityItem)
init_ nsPreviewRepresentingActivityItem =
  sendOwnedMessage nsPreviewRepresentingActivityItem initSelector

-- | @+ new@
new :: IO (Id NSPreviewRepresentingActivityItem)
new  =
  do
    cls' <- getRequiredClass "NSPreviewRepresentingActivityItem"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItem:title:image:icon:@
initWithItem_title_image_iconSelector :: Selector '[RawId, Id NSString, Id NSImage, Id NSImage] (Id NSPreviewRepresentingActivityItem)
initWithItem_title_image_iconSelector = mkSelector "initWithItem:title:image:icon:"

-- | @Selector@ for @initWithItem:title:imageProvider:iconProvider:@
initWithItem_title_imageProvider_iconProviderSelector :: Selector '[RawId, Id NSString, Id NSItemProvider, Id NSItemProvider] (Id NSPreviewRepresentingActivityItem)
initWithItem_title_imageProvider_iconProviderSelector = mkSelector "initWithItem:title:imageProvider:iconProvider:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPreviewRepresentingActivityItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSPreviewRepresentingActivityItem)
newSelector = mkSelector "new"

