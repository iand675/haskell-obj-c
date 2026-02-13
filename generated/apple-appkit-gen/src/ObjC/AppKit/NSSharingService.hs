{-# LANGUAGE DataKinds #-}
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
  , accountNameSelector
  , alternateImageSelector
  , attachmentFileURLsSelector
  , canPerformWithItemsSelector
  , delegateSelector
  , imageSelector
  , initSelector
  , initWithTitle_image_alternateImage_handlerSelector
  , menuItemTitleSelector
  , messageBodySelector
  , performWithItemsSelector
  , permanentLinkSelector
  , recipientsSelector
  , setDelegateSelector
  , setMenuItemTitleSelector
  , setRecipientsSelector
  , setSubjectSelector
  , sharingServiceNamedSelector
  , sharingServicesForItemsSelector
  , subjectSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharingServicesForItemsSelector (toNSArray items)

-- | Returns an NSSharingService representing one of the built-in services.
--
-- ObjC selector: @+ sharingServiceNamed:@
sharingServiceNamed :: IsNSString serviceName => serviceName -> IO (Id NSSharingService)
sharingServiceNamed serviceName =
  do
    cls' <- getRequiredClass "NSSharingService"
    sendClassMessage cls' sharingServiceNamedSelector (toNSString serviceName)

-- | Creates a custom NSSharingService object. Custom sharing services can be added to the NSSharingServicePicker with the sharingServicePicker:sharingServicesForItems:proposedSharingServices: delegate method.
--
-- ObjC selector: @- initWithTitle:image:alternateImage:handler:@
initWithTitle_image_alternateImage_handler :: (IsNSSharingService nsSharingService, IsNSString title, IsNSImage image, IsNSImage alternateImage) => nsSharingService -> title -> image -> alternateImage -> Ptr () -> IO (Id NSSharingService)
initWithTitle_image_alternateImage_handler nsSharingService title image alternateImage block =
  sendOwnedMessage nsSharingService initWithTitle_image_alternateImage_handlerSelector (toNSString title) (toNSImage image) (toNSImage alternateImage) block

-- | Use -initWithTitle:image:alternateImage:handler: instead
--
-- ObjC selector: @- init@
init_ :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSSharingService)
init_ nsSharingService =
  sendOwnedMessage nsSharingService initSelector

-- | Returns whether a service can do something with all the provided items. This can be used to validate a custom UI such as a dedicated Twitter button. If items is nil, the method will return YES when the service is configured. Therefore you could call it once at launch time with nil items to check whether to display the button or not, and then with real items to enable and disable the button depending on the context or selection.
--
-- The items represent the objects to be shared and must conform to the <NSPasteboardWriting> protocol or be an NSItemProvider or an NSDocument. (e.g. NSString, NSImage, NSURL, etc.)
--
-- ObjC selector: @- canPerformWithItems:@
canPerformWithItems :: (IsNSSharingService nsSharingService, IsNSArray items) => nsSharingService -> items -> IO Bool
canPerformWithItems nsSharingService items =
  sendMessage nsSharingService canPerformWithItemsSelector (toNSArray items)

-- | Manually performs the service on the provided items. In most cases this will display a sharing window.
--
-- The items represent the objects to be shared and must conform to the <NSPasteboardWriting> protocol or be an NSItemProvider or an NSDocument. (e.g. NSString, NSImage, NSURL, etc.)
--
-- ObjC selector: @- performWithItems:@
performWithItems :: (IsNSSharingService nsSharingService, IsNSArray items) => nsSharingService -> items -> IO ()
performWithItems nsSharingService items =
  sendMessage nsSharingService performWithItemsSelector (toNSArray items)

-- | @- delegate@
delegate :: IsNSSharingService nsSharingService => nsSharingService -> IO RawId
delegate nsSharingService =
  sendMessage nsSharingService delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSharingService nsSharingService => nsSharingService -> RawId -> IO ()
setDelegate nsSharingService value =
  sendMessage nsSharingService setDelegateSelector value

-- | @- title@
title :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
title nsSharingService =
  sendMessage nsSharingService titleSelector

-- | @- image@
image :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSImage)
image nsSharingService =
  sendMessage nsSharingService imageSelector

-- | @- alternateImage@
alternateImage :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSImage)
alternateImage nsSharingService =
  sendMessage nsSharingService alternateImageSelector

-- | Title of the service in the Share menu. Can be modified.
--
-- ObjC selector: @- menuItemTitle@
menuItemTitle :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
menuItemTitle nsSharingService =
  sendMessage nsSharingService menuItemTitleSelector

-- | Title of the service in the Share menu. Can be modified.
--
-- ObjC selector: @- setMenuItemTitle:@
setMenuItemTitle :: (IsNSSharingService nsSharingService, IsNSString value) => nsSharingService -> value -> IO ()
setMenuItemTitle nsSharingService value =
  sendMessage nsSharingService setMenuItemTitleSelector (toNSString value)

-- | NSArray of NSString objects representing handles (example: email adresses)
--
-- ObjC selector: @- recipients@
recipients :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSArray)
recipients nsSharingService =
  sendMessage nsSharingService recipientsSelector

-- | NSArray of NSString objects representing handles (example: email adresses)
--
-- ObjC selector: @- setRecipients:@
setRecipients :: (IsNSSharingService nsSharingService, IsNSArray value) => nsSharingService -> value -> IO ()
setRecipients nsSharingService value =
  sendMessage nsSharingService setRecipientsSelector (toNSArray value)

-- | @- subject@
subject :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
subject nsSharingService =
  sendMessage nsSharingService subjectSelector

-- | @- setSubject:@
setSubject :: (IsNSSharingService nsSharingService, IsNSString value) => nsSharingService -> value -> IO ()
setSubject nsSharingService value =
  sendMessage nsSharingService setSubjectSelector (toNSString value)

-- | Message body as string
--
-- ObjC selector: @- messageBody@
messageBody :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
messageBody nsSharingService =
  sendMessage nsSharingService messageBodySelector

-- | URL to access the post on Facebook, Twitter, Sina Weibo, etc. (also known as permalink)
--
-- ObjC selector: @- permanentLink@
permanentLink :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSURL)
permanentLink nsSharingService =
  sendMessage nsSharingService permanentLinkSelector

-- | Account name used for sending on Twitter or Sina Weibo
--
-- ObjC selector: @- accountName@
accountName :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSString)
accountName nsSharingService =
  sendMessage nsSharingService accountNameSelector

-- | NSArray of NSURL objects representing the files that were shared
--
-- ObjC selector: @- attachmentFileURLs@
attachmentFileURLs :: IsNSSharingService nsSharingService => nsSharingService -> IO (Id NSArray)
attachmentFileURLs nsSharingService =
  sendMessage nsSharingService attachmentFileURLsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharingServicesForItems:@
sharingServicesForItemsSelector :: Selector '[Id NSArray] (Id NSArray)
sharingServicesForItemsSelector = mkSelector "sharingServicesForItems:"

-- | @Selector@ for @sharingServiceNamed:@
sharingServiceNamedSelector :: Selector '[Id NSString] (Id NSSharingService)
sharingServiceNamedSelector = mkSelector "sharingServiceNamed:"

-- | @Selector@ for @initWithTitle:image:alternateImage:handler:@
initWithTitle_image_alternateImage_handlerSelector :: Selector '[Id NSString, Id NSImage, Id NSImage, Ptr ()] (Id NSSharingService)
initWithTitle_image_alternateImage_handlerSelector = mkSelector "initWithTitle:image:alternateImage:handler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSharingService)
initSelector = mkSelector "init"

-- | @Selector@ for @canPerformWithItems:@
canPerformWithItemsSelector :: Selector '[Id NSArray] Bool
canPerformWithItemsSelector = mkSelector "canPerformWithItems:"

-- | @Selector@ for @performWithItems:@
performWithItemsSelector :: Selector '[Id NSArray] ()
performWithItemsSelector = mkSelector "performWithItems:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector '[] (Id NSImage)
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @menuItemTitle@
menuItemTitleSelector :: Selector '[] (Id NSString)
menuItemTitleSelector = mkSelector "menuItemTitle"

-- | @Selector@ for @setMenuItemTitle:@
setMenuItemTitleSelector :: Selector '[Id NSString] ()
setMenuItemTitleSelector = mkSelector "setMenuItemTitle:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector '[] (Id NSArray)
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @setRecipients:@
setRecipientsSelector :: Selector '[Id NSArray] ()
setRecipientsSelector = mkSelector "setRecipients:"

-- | @Selector@ for @subject@
subjectSelector :: Selector '[] (Id NSString)
subjectSelector = mkSelector "subject"

-- | @Selector@ for @setSubject:@
setSubjectSelector :: Selector '[Id NSString] ()
setSubjectSelector = mkSelector "setSubject:"

-- | @Selector@ for @messageBody@
messageBodySelector :: Selector '[] (Id NSString)
messageBodySelector = mkSelector "messageBody"

-- | @Selector@ for @permanentLink@
permanentLinkSelector :: Selector '[] (Id NSURL)
permanentLinkSelector = mkSelector "permanentLink"

-- | @Selector@ for @accountName@
accountNameSelector :: Selector '[] (Id NSString)
accountNameSelector = mkSelector "accountName"

-- | @Selector@ for @attachmentFileURLs@
attachmentFileURLsSelector :: Selector '[] (Id NSArray)
attachmentFileURLsSelector = mkSelector "attachmentFileURLs"

