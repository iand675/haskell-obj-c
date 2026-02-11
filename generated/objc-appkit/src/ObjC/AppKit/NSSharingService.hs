{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSSharingService can be used to share items to different kinds of local and remote services. Items are objects which respond to the NSPasteboardWriting protocol, like NSURL, NSImage or NSString. If an NSURL is a file URL (point to a video for example), then the content of the file will be shared. If the URL is remote, then the URL itself will be shared.
--
-- Generated bindings for @NSSharingService@.
module ObjC.AppKit.NSSharingService
  ( NSSharingService
  , IsNSSharingService(..)
  , sharingServicesForItems
  , sharingServiceNamed
  , initWithTitle_image_alternateImage_handler
  , init_
  , canPerformWithItems
  , performWithItems
  , title
  , image
  , alternateImage
  , sharingServicesForItemsSelector
  , sharingServiceNamedSelector
  , initWithTitle_image_alternateImage_handlerSelector
  , initSelector
  , canPerformWithItemsSelector
  , performWithItemsSelector
  , titleSelector
  , imageSelector
  , alternateImageSelector


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

-- | Returns a list of NSSharingServices which could share all the provided items together. sharingServicesForItems can be used to build a custom UI, or to populate a contextual NSMenu. The items represent the objects to be shared and must conform to the <NSPasteboardWriting> protocol or be an NSItemProvider or an NSDocument. (e.g. NSString, NSImage, NSURL, etc.)
--
-- ObjC selector: @+ sharingServicesForItems:@
sharingServicesForItems :: IsNSArray items => items -> IO (Id NSArray)
sharingServicesForItems items =
  do
    cls' <- getRequiredClass "NSSharingService"
    withObjCPtr items $ \raw_items ->
      sendClassMsg cls' (mkSelector "sharingServicesForItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an NSSharingService representing one of the built-in services.
--
-- ObjC selector: @+ sharingServiceNamed:@
sharingServiceNamed :: IsNSString serviceName => serviceName -> IO (Id NSSharingService)
sharingServiceNamed serviceName =
  do
    cls' <- getRequiredClass "NSSharingService"
    withObjCPtr serviceName $ \raw_serviceName ->
      sendClassMsg cls' (mkSelector "sharingServiceNamed:") (retPtr retVoid) [argPtr (castPtr raw_serviceName :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a custom NSSharingService object. Custom sharing services can be added to the NSSharingServicePicker with the sharingServicePicker:sharingServicesForItems:proposedSharingServices: delegate method.
--
-- ObjC selector: @- initWithTitle:image:alternateImage:handler:@
initWithTitle_image_alternateImage_handler :: (IsNSSharingService nsSharingService, IsNSString title, IsNSImage image, IsNSImage alternateImage) => nsSharingService -> title -> image -> alternateImage -> Ptr () -> IO (Id NSSharingService)
initWithTitle_image_alternateImage_handler nsSharingService  title image alternateImage block =
withObjCPtr title $ \raw_title ->
  withObjCPtr image $ \raw_image ->
    withObjCPtr alternateImage $ \raw_alternateImage ->
        sendMsg nsSharingService (mkSelector "initWithTitle:image:alternateImage:handler:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_alternateImage :: Ptr ()), argPtr (castPtr block :: Ptr ())] >>= ownedObject . castPtr

-- | Use -initWithTitle:image:alternateImage:handler: instead
--
-- ObjC selector: @- init@
init_ :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSSharingService)
init_ nsSharingService  =
  sendMsg nsSharingService (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns whether a service can do something with all the provided items. This can be used to validate a custom UI such as a dedicated Twitter button. If items is nil, the method will return YES when the service is configured. Therefore you could call it once at launch time with nil items to check whether to display the button or not, and then with real items to enable and disable the button depending on the context or selection.
--
-- The items represent the objects to be shared and must conform to the <NSPasteboardWriting> protocol or be an NSItemProvider or an NSDocument. (e.g. NSString, NSImage, NSURL, etc.)
--
-- ObjC selector: @- canPerformWithItems:@
canPerformWithItems :: (IsNSSharingService nsSharingService, IsNSArray items) => nsSharingService -> items -> IO Bool
canPerformWithItems nsSharingService  items =
withObjCPtr items $ \raw_items ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSharingService (mkSelector "canPerformWithItems:") retCULong [argPtr (castPtr raw_items :: Ptr ())]

-- | Manually performs the service on the provided items. In most cases this will display a sharing window.
--
-- The items represent the objects to be shared and must conform to the <NSPasteboardWriting> protocol or be an NSItemProvider or an NSDocument. (e.g. NSString, NSImage, NSURL, etc.)
--
-- ObjC selector: @- performWithItems:@
performWithItems :: (IsNSSharingService nsSharingService, IsNSArray items) => nsSharingService -> items -> IO ()
performWithItems nsSharingService  items =
withObjCPtr items $ \raw_items ->
    sendMsg nsSharingService (mkSelector "performWithItems:") retVoid [argPtr (castPtr raw_items :: Ptr ())]

-- | @- title@
title :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
title nsSharingService  =
  sendMsg nsSharingService (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- image@
image :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSImage)
image nsSharingService  =
  sendMsg nsSharingService (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alternateImage@
alternateImage :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSImage)
alternateImage nsSharingService  =
  sendMsg nsSharingService (mkSelector "alternateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharingServicesForItems:@
sharingServicesForItemsSelector :: Selector
sharingServicesForItemsSelector = mkSelector "sharingServicesForItems:"

-- | @Selector@ for @sharingServiceNamed:@
sharingServiceNamedSelector :: Selector
sharingServiceNamedSelector = mkSelector "sharingServiceNamed:"

-- | @Selector@ for @initWithTitle:image:alternateImage:handler:@
initWithTitle_image_alternateImage_handlerSelector :: Selector
initWithTitle_image_alternateImage_handlerSelector = mkSelector "initWithTitle:image:alternateImage:handler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @canPerformWithItems:@
canPerformWithItemsSelector :: Selector
canPerformWithItemsSelector = mkSelector "canPerformWithItems:"

-- | @Selector@ for @performWithItems:@
performWithItemsSelector :: Selector
performWithItemsSelector = mkSelector "performWithItems:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector
alternateImageSelector = mkSelector "alternateImage"

