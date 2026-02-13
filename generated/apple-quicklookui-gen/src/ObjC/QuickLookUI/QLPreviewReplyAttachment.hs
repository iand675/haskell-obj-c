{-# LANGUAGE DataKinds #-}
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
  , contentTypeSelector
  , dataSelector
  , initWithData_contentTypeSelector


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

-- | Create an attachment for html previews by providing the data and mime type of the attachment.
--
-- @data@ — The data content of an html preview
--
-- @contentType@ — The UTType of the attachment for an html preview
--
-- ObjC selector: @- initWithData:contentType:@
initWithData_contentType :: (IsQLPreviewReplyAttachment qlPreviewReplyAttachment, IsNSData data_, IsUTType contentType) => qlPreviewReplyAttachment -> data_ -> contentType -> IO (Id QLPreviewReplyAttachment)
initWithData_contentType qlPreviewReplyAttachment data_ contentType =
  sendOwnedMessage qlPreviewReplyAttachment initWithData_contentTypeSelector (toNSData data_) (toUTType contentType)

-- | The data content of an html preview
--
-- ObjC selector: @- data@
data_ :: IsQLPreviewReplyAttachment qlPreviewReplyAttachment => qlPreviewReplyAttachment -> IO (Id NSData)
data_ qlPreviewReplyAttachment =
  sendMessage qlPreviewReplyAttachment dataSelector

-- | The content type of the attachment for an html preview
--
-- ObjC selector: @- contentType@
contentType :: IsQLPreviewReplyAttachment qlPreviewReplyAttachment => qlPreviewReplyAttachment -> IO (Id UTType)
contentType qlPreviewReplyAttachment =
  sendMessage qlPreviewReplyAttachment contentTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:contentType:@
initWithData_contentTypeSelector :: Selector '[Id NSData, Id UTType] (Id QLPreviewReplyAttachment)
initWithData_contentTypeSelector = mkSelector "initWithData:contentType:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id UTType)
contentTypeSelector = mkSelector "contentType"

