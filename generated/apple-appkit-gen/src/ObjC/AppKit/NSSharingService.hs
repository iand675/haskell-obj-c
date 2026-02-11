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
  , delegate
  , setDelegate
  , title
  , image
  , alternateImage
  , menuItemTitle
  , setMenuItemTitle
  , recipients
  , setRecipients
  , subject
  , setSubject
  , messageBody
  , permanentLink
  , accountName
  , attachmentFileURLs
  , sharingServicesForItemsSelector
  , sharingServiceNamedSelector
  , initWithTitle_image_alternateImage_handlerSelector
  , initSelector
  , canPerformWithItemsSelector
  , performWithItemsSelector
  , delegateSelector
  , setDelegateSelector
  , titleSelector
  , imageSelector
  , alternateImageSelector
  , menuItemTitleSelector
  , setMenuItemTitleSelector
  , recipientsSelector
  , setRecipientsSelector
  , subjectSelector
  , setSubjectSelector
  , messageBodySelector
  , permanentLinkSelector
  , accountNameSelector
  , attachmentFileURLsSelector


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

-- | @- delegate@
delegate :: IsNSSharingService nsSharingService => nsSharingService -> IO RawId
delegate nsSharingService  =
    fmap (RawId . castPtr) $ sendMsg nsSharingService (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSharingService nsSharingService => nsSharingService -> RawId -> IO ()
setDelegate nsSharingService  value =
    sendMsg nsSharingService (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

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

-- | Title of the service in the Share menu. Can be modified.
--
-- ObjC selector: @- menuItemTitle@
menuItemTitle :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
menuItemTitle nsSharingService  =
    sendMsg nsSharingService (mkSelector "menuItemTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title of the service in the Share menu. Can be modified.
--
-- ObjC selector: @- setMenuItemTitle:@
setMenuItemTitle :: (IsNSSharingService nsSharingService, IsNSString value) => nsSharingService -> value -> IO ()
setMenuItemTitle nsSharingService  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSharingService (mkSelector "setMenuItemTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | NSArray of NSString objects representing handles (example: email adresses)
--
-- ObjC selector: @- recipients@
recipients :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSArray)
recipients nsSharingService  =
    sendMsg nsSharingService (mkSelector "recipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | NSArray of NSString objects representing handles (example: email adresses)
--
-- ObjC selector: @- setRecipients:@
setRecipients :: (IsNSSharingService nsSharingService, IsNSArray value) => nsSharingService -> value -> IO ()
setRecipients nsSharingService  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSharingService (mkSelector "setRecipients:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subject@
subject :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
subject nsSharingService  =
    sendMsg nsSharingService (mkSelector "subject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubject:@
setSubject :: (IsNSSharingService nsSharingService, IsNSString value) => nsSharingService -> value -> IO ()
setSubject nsSharingService  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSharingService (mkSelector "setSubject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Message body as string
--
-- ObjC selector: @- messageBody@
messageBody :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
messageBody nsSharingService  =
    sendMsg nsSharingService (mkSelector "messageBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL to access the post on Facebook, Twitter, Sina Weibo, etc. (also known as permalink)
--
-- ObjC selector: @- permanentLink@
permanentLink :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSURL)
permanentLink nsSharingService  =
    sendMsg nsSharingService (mkSelector "permanentLink") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Account name used for sending on Twitter or Sina Weibo
--
-- ObjC selector: @- accountName@
accountName :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
accountName nsSharingService  =
    sendMsg nsSharingService (mkSelector "accountName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | NSArray of NSURL objects representing the files that were shared
--
-- ObjC selector: @- attachmentFileURLs@
attachmentFileURLs :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSArray)
attachmentFileURLs nsSharingService  =
    sendMsg nsSharingService (mkSelector "attachmentFileURLs") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @menuItemTitle@
menuItemTitleSelector :: Selector
menuItemTitleSelector = mkSelector "menuItemTitle"

-- | @Selector@ for @setMenuItemTitle:@
setMenuItemTitleSelector :: Selector
setMenuItemTitleSelector = mkSelector "setMenuItemTitle:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @setRecipients:@
setRecipientsSelector :: Selector
setRecipientsSelector = mkSelector "setRecipients:"

-- | @Selector@ for @subject@
subjectSelector :: Selector
subjectSelector = mkSelector "subject"

-- | @Selector@ for @setSubject:@
setSubjectSelector :: Selector
setSubjectSelector = mkSelector "setSubject:"

-- | @Selector@ for @messageBody@
messageBodySelector :: Selector
messageBodySelector = mkSelector "messageBody"

-- | @Selector@ for @permanentLink@
permanentLinkSelector :: Selector
permanentLinkSelector = mkSelector "permanentLink"

-- | @Selector@ for @accountName@
accountNameSelector :: Selector
accountNameSelector = mkSelector "accountName"

-- | @Selector@ for @attachmentFileURLs@
attachmentFileURLsSelector :: Selector
attachmentFileURLsSelector = mkSelector "attachmentFileURLs"

