{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | QLPreviewReplyAttachment is used to provide data for attachment in html data-based previews.
--
-- Generated bindings for @QLPreviewReplyAttachment@.
module ObjC.QuickLookUI.QLPreviewReplyAttachment
  ( QLPreviewReplyAttachment
  , IsQLPreviewReplyAttachment(..)
  , initWithData_contentType
  , data_
  , contentType
  , initWithData_contentTypeSelector
  , dataSelector
  , contentTypeSelector


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

-- | Create an attachment for html previews by providing the data and mime type of the attachment.
--
-- @data@ — The data content of an html preview
--
-- @contentType@ — The UTType of the attachment for an html preview
--
-- ObjC selector: @- initWithData:contentType:@
initWithData_contentType :: (IsQLPreviewReplyAttachment qlPreviewReplyAttachment, IsNSData data_, IsUTType contentType) => qlPreviewReplyAttachment -> data_ -> contentType -> IO (Id QLPreviewReplyAttachment)
initWithData_contentType qlPreviewReplyAttachment  data_ contentType =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr contentType $ \raw_contentType ->
      sendMsg qlPreviewReplyAttachment (mkSelector "initWithData:contentType:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ())] >>= ownedObject . castPtr

-- | The data content of an html preview
--
-- ObjC selector: @- data@
data_ :: IsQLPreviewReplyAttachment qlPreviewReplyAttachment => qlPreviewReplyAttachment -> IO (Id NSData)
data_ qlPreviewReplyAttachment  =
  sendMsg qlPreviewReplyAttachment (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The content type of the attachment for an html preview
--
-- ObjC selector: @- contentType@
contentType :: IsQLPreviewReplyAttachment qlPreviewReplyAttachment => qlPreviewReplyAttachment -> IO (Id UTType)
contentType qlPreviewReplyAttachment  =
  sendMsg qlPreviewReplyAttachment (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:contentType:@
initWithData_contentTypeSelector :: Selector
initWithData_contentTypeSelector = mkSelector "initWithData:contentType:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

