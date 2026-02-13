{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextAttachment@.
module ObjC.AppKit.NSTextAttachment
  ( NSTextAttachment
  , IsNSTextAttachment(..)
  , initWithData_ofType
  , initWithFileWrapper
  , textAttachmentViewProviderClassForFileType
  , registerTextAttachmentViewProviderClass_forFileType
  , contents
  , setContents
  , fileType
  , setFileType
  , image
  , setImage
  , fileWrapper
  , setFileWrapper
  , attachmentCell
  , setAttachmentCell
  , lineLayoutPadding
  , setLineLayoutPadding
  , allowsTextAttachmentView
  , setAllowsTextAttachmentView
  , usesTextAttachmentView
  , allowsTextAttachmentViewSelector
  , attachmentCellSelector
  , contentsSelector
  , fileTypeSelector
  , fileWrapperSelector
  , imageSelector
  , initWithData_ofTypeSelector
  , initWithFileWrapperSelector
  , lineLayoutPaddingSelector
  , registerTextAttachmentViewProviderClass_forFileTypeSelector
  , setAllowsTextAttachmentViewSelector
  , setAttachmentCellSelector
  , setContentsSelector
  , setFileTypeSelector
  , setFileWrapperSelector
  , setImageSelector
  , setLineLayoutPaddingSelector
  , textAttachmentViewProviderClassForFileTypeSelector
  , usesTextAttachmentViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- initWithData:ofType:@
initWithData_ofType :: (IsNSTextAttachment nsTextAttachment, IsNSData contentData, IsNSString uti) => nsTextAttachment -> contentData -> uti -> IO (Id NSTextAttachment)
initWithData_ofType nsTextAttachment contentData uti =
  sendOwnedMessage nsTextAttachment initWithData_ofTypeSelector (toNSData contentData) (toNSString uti)

-- | @- initWithFileWrapper:@
initWithFileWrapper :: (IsNSTextAttachment nsTextAttachment, IsNSFileWrapper fileWrapper) => nsTextAttachment -> fileWrapper -> IO (Id NSTextAttachment)
initWithFileWrapper nsTextAttachment fileWrapper =
  sendOwnedMessage nsTextAttachment initWithFileWrapperSelector (toNSFileWrapper fileWrapper)

-- | @+ textAttachmentViewProviderClassForFileType:@
textAttachmentViewProviderClassForFileType :: IsNSString fileType => fileType -> IO Class
textAttachmentViewProviderClassForFileType fileType =
  do
    cls' <- getRequiredClass "NSTextAttachment"
    sendClassMessage cls' textAttachmentViewProviderClassForFileTypeSelector (toNSString fileType)

-- | @+ registerTextAttachmentViewProviderClass:forFileType:@
registerTextAttachmentViewProviderClass_forFileType :: IsNSString fileType => Class -> fileType -> IO ()
registerTextAttachmentViewProviderClass_forFileType textAttachmentViewProviderClass fileType =
  do
    cls' <- getRequiredClass "NSTextAttachment"
    sendClassMessage cls' registerTextAttachmentViewProviderClass_forFileTypeSelector textAttachmentViewProviderClass (toNSString fileType)

-- | ************************** Content properties ***************************
--
-- ObjC selector: @- contents@
contents :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSData)
contents nsTextAttachment =
  sendMessage nsTextAttachment contentsSelector

-- | ************************** Content properties ***************************
--
-- ObjC selector: @- setContents:@
setContents :: (IsNSTextAttachment nsTextAttachment, IsNSData value) => nsTextAttachment -> value -> IO ()
setContents nsTextAttachment value =
  sendMessage nsTextAttachment setContentsSelector (toNSData value)

-- | @- fileType@
fileType :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSString)
fileType nsTextAttachment =
  sendMessage nsTextAttachment fileTypeSelector

-- | @- setFileType:@
setFileType :: (IsNSTextAttachment nsTextAttachment, IsNSString value) => nsTextAttachment -> value -> IO ()
setFileType nsTextAttachment value =
  sendMessage nsTextAttachment setFileTypeSelector (toNSString value)

-- | ************************** Rendering/layout properties ***************************
--
-- ObjC selector: @- image@
image :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSImage)
image nsTextAttachment =
  sendMessage nsTextAttachment imageSelector

-- | ************************** Rendering/layout properties ***************************
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSTextAttachment nsTextAttachment, IsNSImage value) => nsTextAttachment -> value -> IO ()
setImage nsTextAttachment value =
  sendMessage nsTextAttachment setImageSelector (toNSImage value)

-- | ************************** Non-image contents properties ***************************
--
-- ObjC selector: @- fileWrapper@
fileWrapper :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSFileWrapper)
fileWrapper nsTextAttachment =
  sendMessage nsTextAttachment fileWrapperSelector

-- | ************************** Non-image contents properties ***************************
--
-- ObjC selector: @- setFileWrapper:@
setFileWrapper :: (IsNSTextAttachment nsTextAttachment, IsNSFileWrapper value) => nsTextAttachment -> value -> IO ()
setFileWrapper nsTextAttachment value =
  sendMessage nsTextAttachment setFileWrapperSelector (toNSFileWrapper value)

-- | @- attachmentCell@
attachmentCell :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO RawId
attachmentCell nsTextAttachment =
  sendMessage nsTextAttachment attachmentCellSelector

-- | @- setAttachmentCell:@
setAttachmentCell :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> RawId -> IO ()
setAttachmentCell nsTextAttachment value =
  sendMessage nsTextAttachment setAttachmentCellSelector value

-- | @- lineLayoutPadding@
lineLayoutPadding :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO CDouble
lineLayoutPadding nsTextAttachment =
  sendMessage nsTextAttachment lineLayoutPaddingSelector

-- | @- setLineLayoutPadding:@
setLineLayoutPadding :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> CDouble -> IO ()
setLineLayoutPadding nsTextAttachment value =
  sendMessage nsTextAttachment setLineLayoutPaddingSelector value

-- | @- allowsTextAttachmentView@
allowsTextAttachmentView :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO Bool
allowsTextAttachmentView nsTextAttachment =
  sendMessage nsTextAttachment allowsTextAttachmentViewSelector

-- | @- setAllowsTextAttachmentView:@
setAllowsTextAttachmentView :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> Bool -> IO ()
setAllowsTextAttachmentView nsTextAttachment value =
  sendMessage nsTextAttachment setAllowsTextAttachmentViewSelector value

-- | @- usesTextAttachmentView@
usesTextAttachmentView :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO Bool
usesTextAttachmentView nsTextAttachment =
  sendMessage nsTextAttachment usesTextAttachmentViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:ofType:@
initWithData_ofTypeSelector :: Selector '[Id NSData, Id NSString] (Id NSTextAttachment)
initWithData_ofTypeSelector = mkSelector "initWithData:ofType:"

-- | @Selector@ for @initWithFileWrapper:@
initWithFileWrapperSelector :: Selector '[Id NSFileWrapper] (Id NSTextAttachment)
initWithFileWrapperSelector = mkSelector "initWithFileWrapper:"

-- | @Selector@ for @textAttachmentViewProviderClassForFileType:@
textAttachmentViewProviderClassForFileTypeSelector :: Selector '[Id NSString] Class
textAttachmentViewProviderClassForFileTypeSelector = mkSelector "textAttachmentViewProviderClassForFileType:"

-- | @Selector@ for @registerTextAttachmentViewProviderClass:forFileType:@
registerTextAttachmentViewProviderClass_forFileTypeSelector :: Selector '[Class, Id NSString] ()
registerTextAttachmentViewProviderClass_forFileTypeSelector = mkSelector "registerTextAttachmentViewProviderClass:forFileType:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] (Id NSData)
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[Id NSData] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector '[] (Id NSString)
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @setFileType:@
setFileTypeSelector :: Selector '[Id NSString] ()
setFileTypeSelector = mkSelector "setFileType:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @fileWrapper@
fileWrapperSelector :: Selector '[] (Id NSFileWrapper)
fileWrapperSelector = mkSelector "fileWrapper"

-- | @Selector@ for @setFileWrapper:@
setFileWrapperSelector :: Selector '[Id NSFileWrapper] ()
setFileWrapperSelector = mkSelector "setFileWrapper:"

-- | @Selector@ for @attachmentCell@
attachmentCellSelector :: Selector '[] RawId
attachmentCellSelector = mkSelector "attachmentCell"

-- | @Selector@ for @setAttachmentCell:@
setAttachmentCellSelector :: Selector '[RawId] ()
setAttachmentCellSelector = mkSelector "setAttachmentCell:"

-- | @Selector@ for @lineLayoutPadding@
lineLayoutPaddingSelector :: Selector '[] CDouble
lineLayoutPaddingSelector = mkSelector "lineLayoutPadding"

-- | @Selector@ for @setLineLayoutPadding:@
setLineLayoutPaddingSelector :: Selector '[CDouble] ()
setLineLayoutPaddingSelector = mkSelector "setLineLayoutPadding:"

-- | @Selector@ for @allowsTextAttachmentView@
allowsTextAttachmentViewSelector :: Selector '[] Bool
allowsTextAttachmentViewSelector = mkSelector "allowsTextAttachmentView"

-- | @Selector@ for @setAllowsTextAttachmentView:@
setAllowsTextAttachmentViewSelector :: Selector '[Bool] ()
setAllowsTextAttachmentViewSelector = mkSelector "setAllowsTextAttachmentView:"

-- | @Selector@ for @usesTextAttachmentView@
usesTextAttachmentViewSelector :: Selector '[] Bool
usesTextAttachmentViewSelector = mkSelector "usesTextAttachmentView"

