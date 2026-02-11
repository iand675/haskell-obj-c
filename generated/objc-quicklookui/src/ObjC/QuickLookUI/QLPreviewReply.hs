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
  , initWithFileURLSelector
  , stringEncodingSelector
  , setStringEncodingSelector
  , attachmentsSelector
  , setAttachmentsSelector
  , titleSelector
  , setTitleSelector


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

import ObjC.QuickLookUI.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | Use this method to provide a preview by providing a URL to a file of a supported type.
--
-- @fileURL@ — A file URL representing a preview of the previewed URL. Currently supported types include: UTTypeImage, UTTypePDF, UTTypeHTML, UTTypeXML, UTTypePlainText, UTTypeRTF, UTTypeRTFD, UTTypeMovie, UTTypeAudio
--
-- ObjC selector: @- initWithFileURL:@
initWithFileURL :: (IsQLPreviewReply qlPreviewReply, IsNSURL fileURL) => qlPreviewReply -> fileURL -> IO (Id QLPreviewReply)
initWithFileURL qlPreviewReply  fileURL =
withObjCPtr fileURL $ \raw_fileURL ->
    sendMsg qlPreviewReply (mkSelector "initWithFileURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= ownedObject . castPtr

-- | String encoding for text or html based previews. Defaults to NSUTF8StringEncoding.
--
-- ObjC selector: @- stringEncoding@
stringEncoding :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> IO CULong
stringEncoding qlPreviewReply  =
  sendMsg qlPreviewReply (mkSelector "stringEncoding") retCULong []

-- | String encoding for text or html based previews. Defaults to NSUTF8StringEncoding.
--
-- ObjC selector: @- setStringEncoding:@
setStringEncoding :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> CULong -> IO ()
setStringEncoding qlPreviewReply  value =
  sendMsg qlPreviewReply (mkSelector "setStringEncoding:") retVoid [argCULong (fromIntegral value)]

-- | Attachments for HTML data previews. The keys of the dictionary are the attachment identifiers (eg foo) that can be referenced with the cid:id URL (eg cid:foo).
--
-- ObjC selector: @- attachments@
attachments :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> IO (Id NSDictionary)
attachments qlPreviewReply  =
  sendMsg qlPreviewReply (mkSelector "attachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Attachments for HTML data previews. The keys of the dictionary are the attachment identifiers (eg foo) that can be referenced with the cid:id URL (eg cid:foo).
--
-- ObjC selector: @- setAttachments:@
setAttachments :: (IsQLPreviewReply qlPreviewReply, IsNSDictionary value) => qlPreviewReply -> value -> IO ()
setAttachments qlPreviewReply  value =
withObjCPtr value $ \raw_value ->
    sendMsg qlPreviewReply (mkSelector "setAttachments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Custom display title for the preview. If left as the empty string, QuickLook will use the file name.
--
-- ObjC selector: @- title@
title :: IsQLPreviewReply qlPreviewReply => qlPreviewReply -> IO (Id NSString)
title qlPreviewReply  =
  sendMsg qlPreviewReply (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Custom display title for the preview. If left as the empty string, QuickLook will use the file name.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsQLPreviewReply qlPreviewReply, IsNSString value) => qlPreviewReply -> value -> IO ()
setTitle qlPreviewReply  value =
withObjCPtr value $ \raw_value ->
    sendMsg qlPreviewReply (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileURL:@
initWithFileURLSelector :: Selector
initWithFileURLSelector = mkSelector "initWithFileURL:"

-- | @Selector@ for @stringEncoding@
stringEncodingSelector :: Selector
stringEncodingSelector = mkSelector "stringEncoding"

-- | @Selector@ for @setStringEncoding:@
setStringEncodingSelector :: Selector
setStringEncodingSelector = mkSelector "setStringEncoding:"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @setAttachments:@
setAttachmentsSelector :: Selector
setAttachmentsSelector = mkSelector "setAttachments:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

