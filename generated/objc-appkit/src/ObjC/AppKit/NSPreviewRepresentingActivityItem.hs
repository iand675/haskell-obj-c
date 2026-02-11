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
  , initWithItem_title_image_iconSelector
  , initWithItem_title_imageProvider_iconProviderSelector
  , initSelector
  , newSelector


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
import ObjC.Foundation.Internal.Classes

-- | - Parameters:     - item: The item to share     - title: A title to show in a preview     - image: An image to show in a preview     - icon: An icon to show in a preview
--
-- For more information about the parameters, see NSPreviewRepresentableActivityItem documentation
--
-- ObjC selector: @- initWithItem:title:image:icon:@
initWithItem_title_image_icon :: (IsNSPreviewRepresentingActivityItem nsPreviewRepresentingActivityItem, IsNSString title, IsNSImage image, IsNSImage icon) => nsPreviewRepresentingActivityItem -> RawId -> title -> image -> icon -> IO (Id NSPreviewRepresentingActivityItem)
initWithItem_title_image_icon nsPreviewRepresentingActivityItem  item title image icon =
withObjCPtr title $ \raw_title ->
  withObjCPtr image $ \raw_image ->
    withObjCPtr icon $ \raw_icon ->
        sendMsg nsPreviewRepresentingActivityItem (mkSelector "initWithItem:title:image:icon:") (retPtr retVoid) [argPtr (castPtr (unRawId item) :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_icon :: Ptr ())] >>= ownedObject . castPtr

-- | - Parameters:     - item: The item to share     - title: A title to show in a preview     - imageProvider: An NSItemProvider which provides an image to show in a preview     - iconProvider: An NSItemProvider which provides an icon to show in a preview
--
-- For more information about the parameters, see NSPreviewRepresentableActivityItem documentation
--
-- ObjC selector: @- initWithItem:title:imageProvider:iconProvider:@
initWithItem_title_imageProvider_iconProvider :: (IsNSPreviewRepresentingActivityItem nsPreviewRepresentingActivityItem, IsNSString title, IsNSItemProvider imageProvider, IsNSItemProvider iconProvider) => nsPreviewRepresentingActivityItem -> RawId -> title -> imageProvider -> iconProvider -> IO (Id NSPreviewRepresentingActivityItem)
initWithItem_title_imageProvider_iconProvider nsPreviewRepresentingActivityItem  item title imageProvider iconProvider =
withObjCPtr title $ \raw_title ->
  withObjCPtr imageProvider $ \raw_imageProvider ->
    withObjCPtr iconProvider $ \raw_iconProvider ->
        sendMsg nsPreviewRepresentingActivityItem (mkSelector "initWithItem:title:imageProvider:iconProvider:") (retPtr retVoid) [argPtr (castPtr (unRawId item) :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_imageProvider :: Ptr ()), argPtr (castPtr raw_iconProvider :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSPreviewRepresentingActivityItem nsPreviewRepresentingActivityItem => nsPreviewRepresentingActivityItem -> IO (Id NSPreviewRepresentingActivityItem)
init_ nsPreviewRepresentingActivityItem  =
  sendMsg nsPreviewRepresentingActivityItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSPreviewRepresentingActivityItem)
new  =
  do
    cls' <- getRequiredClass "NSPreviewRepresentingActivityItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItem:title:image:icon:@
initWithItem_title_image_iconSelector :: Selector
initWithItem_title_image_iconSelector = mkSelector "initWithItem:title:image:icon:"

-- | @Selector@ for @initWithItem:title:imageProvider:iconProvider:@
initWithItem_title_imageProvider_iconProviderSelector :: Selector
initWithItem_title_imageProvider_iconProviderSelector = mkSelector "initWithItem:title:imageProvider:iconProvider:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

