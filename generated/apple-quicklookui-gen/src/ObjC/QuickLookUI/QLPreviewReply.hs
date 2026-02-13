{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | To provide a data-based preview, you have to return a QLPreviewReply object.
--
-- Generated bindings for @QLPreviewReply@.
module ObjC.QuickLookUI.QLPreviewReply
  ( QLPreviewReply
  , IsQLPreviewReply(..)
  , initWithFileURL
  , stringEncoding
  , setStringEncoding
  , attachments
  , setAttachments
  , title
  , setTitle
  , attachmentsSelector
  , initWithFileURLSelector
  , setAttachmentsSelector
  , setStringEncodingSelector
  , setTitleSelector
  , stringEncodingSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookUI.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | Use this method to provide a preview by providing a URL to a file of a supported type.
--
-- @fileURL@ — A file URL representing a preview of the previewed URL. Currently supported types include: UTTypeImage, UTTypePDF, UTTypeHTML, UTTypeXML, UTTypePlainText, UTTypeRTF, UTTypeRTFD, UTTypeMovie, UTTypeAudio
--
-- ObjC selector: @- initWithFileURL:@
initWithFileURL :: (IsQLPreviewReply qlPreviewReply, IsNSURL fileURL) => qlPreviewReply -> fileURL -> IO (Id QLPreviewReply)
initWithFileURL qlPreviewReply fileURL =
  sendOwnedMessage qlPreviewReply initWithFileURLSelector (toNSURL fileURL)

-- | String encoding for text or html based previews. Defaults to NSUTF8StringEncoding.
--
-- ObjC selector: @- stringEncoding@
stringEncoding :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> IO CULong
stringEncoding qlPreviewReply =
  sendMessage qlPreviewReply stringEncodingSelector

-- | String encoding for text or html based previews. Defaults to NSUTF8StringEncoding.
--
-- ObjC selector: @- setStringEncoding:@
setStringEncoding :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> CULong -> IO ()
setStringEncoding qlPreviewReply value =
  sendMessage qlPreviewReply setStringEncodingSelector value

-- | Attachments for HTML data previews. The keys of the dictionary are the attachment identifiers (eg foo) that can be referenced with the cid:id URL (eg cid:foo).
--
-- ObjC selector: @- attachments@
attachments :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> IO (Id NSDictionary)
attachments qlPreviewReply =
  sendMessage qlPreviewReply attachmentsSelector

-- | Attachments for HTML data previews. The keys of the dictionary are the attachment identifiers (eg foo) that can be referenced with the cid:id URL (eg cid:foo).
--
-- ObjC selector: @- setAttachments:@
setAttachments :: (IsQLPreviewReply qlPreviewReply, IsNSDictionary value) => qlPreviewReply -> value -> IO ()
setAttachments qlPreviewReply value =
  sendMessage qlPreviewReply setAttachmentsSelector (toNSDictionary value)

-- | Custom display title for the preview. If left as the empty string, QuickLook will use the file name.
--
-- ObjC selector: @- title@
title :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> IO (Id NSString)
title qlPreviewReply =
  sendMessage qlPreviewReply titleSelector

-- | Custom display title for the preview. If left as the empty string, QuickLook will use the file name.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsQLPreviewReply qlPreviewReply, IsNSString value) => qlPreviewReply -> value -> IO ()
setTitle qlPreviewReply value =
  sendMessage qlPreviewReply setTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileURL:@
initWithFileURLSelector :: Selector '[Id NSURL] (Id QLPreviewReply)
initWithFileURLSelector = mkSelector "initWithFileURL:"

-- | @Selector@ for @stringEncoding@
stringEncodingSelector :: Selector '[] CULong
stringEncodingSelector = mkSelector "stringEncoding"

-- | @Selector@ for @setStringEncoding:@
setStringEncodingSelector :: Selector '[CULong] ()
setStringEncodingSelector = mkSelector "setStringEncoding:"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector '[] (Id NSDictionary)
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @setAttachments:@
setAttachmentsSelector :: Selector '[Id NSDictionary] ()
setAttachmentsSelector = mkSelector "setAttachments:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

