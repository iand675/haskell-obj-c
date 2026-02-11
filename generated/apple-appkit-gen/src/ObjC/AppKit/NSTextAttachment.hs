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
  , initWithData_ofTypeSelector
  , initWithFileWrapperSelector
  , textAttachmentViewProviderClassForFileTypeSelector
  , registerTextAttachmentViewProviderClass_forFileTypeSelector
  , contentsSelector
  , setContentsSelector
  , fileTypeSelector
  , setFileTypeSelector
  , imageSelector
  , setImageSelector
  , fileWrapperSelector
  , setFileWrapperSelector
  , attachmentCellSelector
  , setAttachmentCellSelector
  , lineLayoutPaddingSelector
  , setLineLayoutPaddingSelector
  , allowsTextAttachmentViewSelector
  , setAllowsTextAttachmentViewSelector
  , usesTextAttachmentViewSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- initWithData:ofType:@
initWithData_ofType :: (IsNSTextAttachment nsTextAttachment, IsNSData contentData, IsNSString uti) => nsTextAttachment -> contentData -> uti -> IO (Id NSTextAttachment)
initWithData_ofType nsTextAttachment  contentData uti =
  withObjCPtr contentData $ \raw_contentData ->
    withObjCPtr uti $ \raw_uti ->
        sendMsg nsTextAttachment (mkSelector "initWithData:ofType:") (retPtr retVoid) [argPtr (castPtr raw_contentData :: Ptr ()), argPtr (castPtr raw_uti :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFileWrapper:@
initWithFileWrapper :: (IsNSTextAttachment nsTextAttachment, IsNSFileWrapper fileWrapper) => nsTextAttachment -> fileWrapper -> IO (Id NSTextAttachment)
initWithFileWrapper nsTextAttachment  fileWrapper =
  withObjCPtr fileWrapper $ \raw_fileWrapper ->
      sendMsg nsTextAttachment (mkSelector "initWithFileWrapper:") (retPtr retVoid) [argPtr (castPtr raw_fileWrapper :: Ptr ())] >>= ownedObject . castPtr

-- | @+ textAttachmentViewProviderClassForFileType:@
textAttachmentViewProviderClassForFileType :: IsNSString fileType => fileType -> IO Class
textAttachmentViewProviderClassForFileType fileType =
  do
    cls' <- getRequiredClass "NSTextAttachment"
    withObjCPtr fileType $ \raw_fileType ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "textAttachmentViewProviderClassForFileType:") (retPtr retVoid) [argPtr (castPtr raw_fileType :: Ptr ())]

-- | @+ registerTextAttachmentViewProviderClass:forFileType:@
registerTextAttachmentViewProviderClass_forFileType :: IsNSString fileType => Class -> fileType -> IO ()
registerTextAttachmentViewProviderClass_forFileType textAttachmentViewProviderClass fileType =
  do
    cls' <- getRequiredClass "NSTextAttachment"
    withObjCPtr fileType $ \raw_fileType ->
      sendClassMsg cls' (mkSelector "registerTextAttachmentViewProviderClass:forFileType:") retVoid [argPtr (unClass textAttachmentViewProviderClass), argPtr (castPtr raw_fileType :: Ptr ())]

-- | ************************** Content properties ***************************
--
-- ObjC selector: @- contents@
contents :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSData)
contents nsTextAttachment  =
    sendMsg nsTextAttachment (mkSelector "contents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Content properties ***************************
--
-- ObjC selector: @- setContents:@
setContents :: (IsNSTextAttachment nsTextAttachment, IsNSData value) => nsTextAttachment -> value -> IO ()
setContents nsTextAttachment  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextAttachment (mkSelector "setContents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileType@
fileType :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSString)
fileType nsTextAttachment  =
    sendMsg nsTextAttachment (mkSelector "fileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileType:@
setFileType :: (IsNSTextAttachment nsTextAttachment, IsNSString value) => nsTextAttachment -> value -> IO ()
setFileType nsTextAttachment  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextAttachment (mkSelector "setFileType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | ************************** Rendering/layout properties ***************************
--
-- ObjC selector: @- image@
image :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSImage)
image nsTextAttachment  =
    sendMsg nsTextAttachment (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Rendering/layout properties ***************************
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSTextAttachment nsTextAttachment, IsNSImage value) => nsTextAttachment -> value -> IO ()
setImage nsTextAttachment  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextAttachment (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | ************************** Non-image contents properties ***************************
--
-- ObjC selector: @- fileWrapper@
fileWrapper :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO (Id NSFileWrapper)
fileWrapper nsTextAttachment  =
    sendMsg nsTextAttachment (mkSelector "fileWrapper") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Non-image contents properties ***************************
--
-- ObjC selector: @- setFileWrapper:@
setFileWrapper :: (IsNSTextAttachment nsTextAttachment, IsNSFileWrapper value) => nsTextAttachment -> value -> IO ()
setFileWrapper nsTextAttachment  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextAttachment (mkSelector "setFileWrapper:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attachmentCell@
attachmentCell :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO RawId
attachmentCell nsTextAttachment  =
    fmap (RawId . castPtr) $ sendMsg nsTextAttachment (mkSelector "attachmentCell") (retPtr retVoid) []

-- | @- setAttachmentCell:@
setAttachmentCell :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> RawId -> IO ()
setAttachmentCell nsTextAttachment  value =
    sendMsg nsTextAttachment (mkSelector "setAttachmentCell:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- lineLayoutPadding@
lineLayoutPadding :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO CDouble
lineLayoutPadding nsTextAttachment  =
    sendMsg nsTextAttachment (mkSelector "lineLayoutPadding") retCDouble []

-- | @- setLineLayoutPadding:@
setLineLayoutPadding :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> CDouble -> IO ()
setLineLayoutPadding nsTextAttachment  value =
    sendMsg nsTextAttachment (mkSelector "setLineLayoutPadding:") retVoid [argCDouble value]

-- | @- allowsTextAttachmentView@
allowsTextAttachmentView :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO Bool
allowsTextAttachmentView nsTextAttachment  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextAttachment (mkSelector "allowsTextAttachmentView") retCULong []

-- | @- setAllowsTextAttachmentView:@
setAllowsTextAttachmentView :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> Bool -> IO ()
setAllowsTextAttachmentView nsTextAttachment  value =
    sendMsg nsTextAttachment (mkSelector "setAllowsTextAttachmentView:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesTextAttachmentView@
usesTextAttachmentView :: IsNSTextAttachment nsTextAttachment => nsTextAttachment -> IO Bool
usesTextAttachmentView nsTextAttachment  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextAttachment (mkSelector "usesTextAttachmentView") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:ofType:@
initWithData_ofTypeSelector :: Selector
initWithData_ofTypeSelector = mkSelector "initWithData:ofType:"

-- | @Selector@ for @initWithFileWrapper:@
initWithFileWrapperSelector :: Selector
initWithFileWrapperSelector = mkSelector "initWithFileWrapper:"

-- | @Selector@ for @textAttachmentViewProviderClassForFileType:@
textAttachmentViewProviderClassForFileTypeSelector :: Selector
textAttachmentViewProviderClassForFileTypeSelector = mkSelector "textAttachmentViewProviderClassForFileType:"

-- | @Selector@ for @registerTextAttachmentViewProviderClass:forFileType:@
registerTextAttachmentViewProviderClass_forFileTypeSelector :: Selector
registerTextAttachmentViewProviderClass_forFileTypeSelector = mkSelector "registerTextAttachmentViewProviderClass:forFileType:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @setFileType:@
setFileTypeSelector :: Selector
setFileTypeSelector = mkSelector "setFileType:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @fileWrapper@
fileWrapperSelector :: Selector
fileWrapperSelector = mkSelector "fileWrapper"

-- | @Selector@ for @setFileWrapper:@
setFileWrapperSelector :: Selector
setFileWrapperSelector = mkSelector "setFileWrapper:"

-- | @Selector@ for @attachmentCell@
attachmentCellSelector :: Selector
attachmentCellSelector = mkSelector "attachmentCell"

-- | @Selector@ for @setAttachmentCell:@
setAttachmentCellSelector :: Selector
setAttachmentCellSelector = mkSelector "setAttachmentCell:"

-- | @Selector@ for @lineLayoutPadding@
lineLayoutPaddingSelector :: Selector
lineLayoutPaddingSelector = mkSelector "lineLayoutPadding"

-- | @Selector@ for @setLineLayoutPadding:@
setLineLayoutPaddingSelector :: Selector
setLineLayoutPaddingSelector = mkSelector "setLineLayoutPadding:"

-- | @Selector@ for @allowsTextAttachmentView@
allowsTextAttachmentViewSelector :: Selector
allowsTextAttachmentViewSelector = mkSelector "allowsTextAttachmentView"

-- | @Selector@ for @setAllowsTextAttachmentView:@
setAllowsTextAttachmentViewSelector :: Selector
setAllowsTextAttachmentViewSelector = mkSelector "setAllowsTextAttachmentView:"

-- | @Selector@ for @usesTextAttachmentView@
usesTextAttachmentViewSelector :: Selector
usesTextAttachmentViewSelector = mkSelector "usesTextAttachmentView"

