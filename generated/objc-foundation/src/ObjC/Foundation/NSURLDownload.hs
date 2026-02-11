{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLDownload
--
-- A NSURLDownload loads a request and saves the downloaded data to a file. The progress of the download    is reported via the NSURLDownloadDelegate protocol. Note: The word "download" is used to refer to the process    of loading data off a network, decoding the data if necessary and saving the data to a file.
--
-- Generated bindings for @NSURLDownload@.
module ObjC.Foundation.NSURLDownload
  ( NSURLDownload
  , IsNSURLDownload(..)
  , canResumeDownloadDecodedWithEncodingMIMEType
  , initWithRequest_delegate
  , initWithResumeData_delegate_path
  , cancel
  , setDestination_allowOverwrite
  , request
  , resumeData
  , deletesFileUponFailure
  , setDeletesFileUponFailure
  , canResumeDownloadDecodedWithEncodingMIMETypeSelector
  , initWithRequest_delegateSelector
  , initWithResumeData_delegate_pathSelector
  , cancelSelector
  , setDestination_allowOverwriteSelector
  , requestSelector
  , resumeDataSelector
  , deletesFileUponFailureSelector
  , setDeletesFileUponFailureSelector


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

import ObjC.Foundation.Internal.Classes

-- | canResumeDownloadDecodedWithEncodingMIMEType:
--
-- Returns whether or not NSURLDownload can resume a download that was decoded with a given encoding MIME type.
--
-- @MIMEType@ — The encoding MIME type.     canResumeDownloadDecodedWithEncodingMIMEType: returns whether or not NSURLDownload can resume a download    that was decoded with a given encoding MIME type.  NSURLDownload cannot resume a download that was partially decoded    in the gzip format for example. In order to ensure that a download can be later resumed,    canResumeDownloadDecodedWithEncodingMIMEType: should be used when download:shouldDecodeSourceDataOfMIMEType: is called.
--
-- ObjC selector: @+ canResumeDownloadDecodedWithEncodingMIMEType:@
canResumeDownloadDecodedWithEncodingMIMEType :: IsNSString mimeType => mimeType -> IO Bool
canResumeDownloadDecodedWithEncodingMIMEType mimeType =
  do
    cls' <- getRequiredClass "NSURLDownload"
    withObjCPtr mimeType $ \raw_mimeType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canResumeDownloadDecodedWithEncodingMIMEType:") retCULong [argPtr (castPtr raw_mimeType :: Ptr ())]

-- | initWithRequest:delegate:
--
-- Initializes a NSURLDownload object and starts the download.
--
-- @request@ — The request to download. Must not be nil.
--
-- @delegate@ — The delegate of the download.
--
-- Returns: An initialized NSURLDownload object.
--
-- ObjC selector: @- initWithRequest:delegate:@
initWithRequest_delegate :: (IsNSURLDownload nsurlDownload, IsNSURLRequest request) => nsurlDownload -> request -> RawId -> IO (Id NSURLDownload)
initWithRequest_delegate nsurlDownload  request delegate =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlDownload (mkSelector "initWithRequest:delegate:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | initWithResumeData:delegate:path:
--
-- Initializes a NSURLDownload object for resuming a previous download.
--
-- @resumeData@ — The resume data from the previous download.
--
-- @delegate@ — The delegate of the download.
--
-- @path@ — The path of the incomplete downloaded file.
--
-- Returns: An initialized NSURLDownload object.
--
-- ObjC selector: @- initWithResumeData:delegate:path:@
initWithResumeData_delegate_path :: (IsNSURLDownload nsurlDownload, IsNSData resumeData, IsNSString path) => nsurlDownload -> resumeData -> RawId -> path -> IO (Id NSURLDownload)
initWithResumeData_delegate_path nsurlDownload  resumeData delegate path =
withObjCPtr resumeData $ \raw_resumeData ->
  withObjCPtr path $ \raw_path ->
      sendMsg nsurlDownload (mkSelector "initWithResumeData:delegate:path:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | cancel
--
-- Cancels the download and deletes the downloaded file.
--
-- ObjC selector: @- cancel@
cancel :: IsNSURLDownload nsurlDownload => nsurlDownload -> IO ()
cancel nsurlDownload  =
  sendMsg nsurlDownload (mkSelector "cancel") retVoid []

-- | setDestination:allowOverwrite:
--
-- Sets the destination path of the downloaded file.
--
-- @path@ — The destination path of the downloaded file.
--
-- @allowOverwrite@ — Allows a file of the same path to be overwritten.
--
-- This method can be called after the download is created or in response to the    decideDestinationWithSuggestedFilename: delegate method. It should only be called once.    If NO is passed for allowOverwrite and a file of the same path exists, a number will be    appended to the filename to prevent the overwrite. Because of this, use the path    passed with didCreateDestination: to determine the actual path of the downloaded file.
--
-- ObjC selector: @- setDestination:allowOverwrite:@
setDestination_allowOverwrite :: (IsNSURLDownload nsurlDownload, IsNSString path) => nsurlDownload -> path -> Bool -> IO ()
setDestination_allowOverwrite nsurlDownload  path allowOverwrite =
withObjCPtr path $ \raw_path ->
    sendMsg nsurlDownload (mkSelector "setDestination:allowOverwrite:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argCULong (if allowOverwrite then 1 else 0)]

-- | Returns the request of the download.
--
-- Returns: The request of the download.
--
-- ObjC selector: @- request@
request :: IsNSURLDownload nsurlDownload => nsurlDownload -> IO (Id NSURLRequest)
request nsurlDownload  =
  sendMsg nsurlDownload (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the resume data of a download that is incomplete.
--
-- Returns: The resume data.     resumeData returns the resume data of a download that is incomplete. This data represents the necessary    state information that NSURLDownload needs to resume a download. The resume data can later be used when initializing    a download with initWithResumeData:delegate:path:. Non-nil is returned if resuming the download seems possible.    Non-nil is returned if the download was cancelled or ended in error after some but not all data has been received.    The protocol of the download as well as the server must support resuming for non-nil to be returned.    In order to later resume a download, be sure to call setDeletesFileUponFailure: with NO.
--
-- ObjC selector: @- resumeData@
resumeData :: IsNSURLDownload nsurlDownload => nsurlDownload -> IO (Id NSData)
resumeData nsurlDownload  =
  sendMsg nsurlDownload (mkSelector "resumeData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets whether or not the downloaded file should be deleted upon failure.1     To allow the download to be resumed in case the download ends prematurely,    deletesFileUponFailure must be set to NO as soon as possible to prevent the downloaded file    from being deleted. deletesFileUponFailure is YES by default.
--
-- ObjC selector: @- deletesFileUponFailure@
deletesFileUponFailure :: IsNSURLDownload nsurlDownload => nsurlDownload -> IO Bool
deletesFileUponFailure nsurlDownload  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlDownload (mkSelector "deletesFileUponFailure") retCULong []

-- | Sets whether or not the downloaded file should be deleted upon failure.1     To allow the download to be resumed in case the download ends prematurely,    deletesFileUponFailure must be set to NO as soon as possible to prevent the downloaded file    from being deleted. deletesFileUponFailure is YES by default.
--
-- ObjC selector: @- setDeletesFileUponFailure:@
setDeletesFileUponFailure :: IsNSURLDownload nsurlDownload => nsurlDownload -> Bool -> IO ()
setDeletesFileUponFailure nsurlDownload  value =
  sendMsg nsurlDownload (mkSelector "setDeletesFileUponFailure:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canResumeDownloadDecodedWithEncodingMIMEType:@
canResumeDownloadDecodedWithEncodingMIMETypeSelector :: Selector
canResumeDownloadDecodedWithEncodingMIMETypeSelector = mkSelector "canResumeDownloadDecodedWithEncodingMIMEType:"

-- | @Selector@ for @initWithRequest:delegate:@
initWithRequest_delegateSelector :: Selector
initWithRequest_delegateSelector = mkSelector "initWithRequest:delegate:"

-- | @Selector@ for @initWithResumeData:delegate:path:@
initWithResumeData_delegate_pathSelector :: Selector
initWithResumeData_delegate_pathSelector = mkSelector "initWithResumeData:delegate:path:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @setDestination:allowOverwrite:@
setDestination_allowOverwriteSelector :: Selector
setDestination_allowOverwriteSelector = mkSelector "setDestination:allowOverwrite:"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @resumeData@
resumeDataSelector :: Selector
resumeDataSelector = mkSelector "resumeData"

-- | @Selector@ for @deletesFileUponFailure@
deletesFileUponFailureSelector :: Selector
deletesFileUponFailureSelector = mkSelector "deletesFileUponFailure"

-- | @Selector@ for @setDeletesFileUponFailure:@
setDeletesFileUponFailureSelector :: Selector
setDeletesFileUponFailureSelector = mkSelector "setDeletesFileUponFailure:"

