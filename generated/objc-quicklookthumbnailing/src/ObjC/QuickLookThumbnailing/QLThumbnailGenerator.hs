{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QLThumbnailGenerator@.
module ObjC.QuickLookThumbnailing.QLThumbnailGenerator
  ( QLThumbnailGenerator
  , IsQLThumbnailGenerator(..)
  , generateBestRepresentationForRequest_completionHandler
  , generateRepresentationsForRequest_updateHandler
  , cancelRequest
  , saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandler
  , saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandler
  , generateBestRepresentationForRequest_completionHandlerSelector
  , generateRepresentationsForRequest_updateHandlerSelector
  , cancelRequestSelector
  , saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector
  , saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector


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

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @completionHandler@ — Always called when the thumbnail generation is over. The thumbnail passed to this handler is the most representative version of the thumbnail that was successfully generated (if any). If set, the error contains information about the issue that occurred while trying to generate the thumbnail. QLThumbnail error codes can be found in <QuickLookThumbnailing/QLThumbnailErrors.h>.
--
-- ObjC selector: @- generateBestRepresentationForRequest:completionHandler:@
generateBestRepresentationForRequest_completionHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request) => qlThumbnailGenerator -> request -> Ptr () -> IO ()
generateBestRepresentationForRequest_completionHandler qlThumbnailGenerator  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg qlThumbnailGenerator (mkSelector "generateBestRepresentationForRequest:completionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @updateHandler@ — Called for the successive requested representations of a thumbnail. If a representation was not successfully generated, this may be called with a nil representation. If a requested more representative version was successfully generated before a less representative one, this handler will be called only for the more representative version, skipping the less representative one. This handler is guaranteed to be called at least once, for the requested most representative version, whether a representation could be successfully generated or not. If set, the error contains information about the issue that occurred while trying to generate the representation of the given type. QLThumbnail error codes can be found in <QuickLookThumbnailing/QLThumbnailErrors.h>.
--
-- ObjC selector: @- generateRepresentationsForRequest:updateHandler:@
generateRepresentationsForRequest_updateHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request) => qlThumbnailGenerator -> request -> Ptr () -> IO ()
generateRepresentationsForRequest_updateHandler qlThumbnailGenerator  request updateHandler =
withObjCPtr request $ \raw_request ->
    sendMsg qlThumbnailGenerator (mkSelector "generateRepresentationsForRequest:updateHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr updateHandler :: Ptr ())]

-- | Cancels the given QLThumbnailGenerationRequest.
--
-- @request@ — The request that should be cancelled.
--
-- ObjC selector: @- cancelRequest:@
cancelRequest :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request) => qlThumbnailGenerator -> request -> IO ()
cancelRequest qlThumbnailGenerator  request =
withObjCPtr request $ \raw_request ->
    sendMsg qlThumbnailGenerator (mkSelector "cancelRequest:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

-- | Saves a thumbnail for the request on disk at fileURL. The file saved at fileURL has to be deleted when it is not used anymore. This is primarily intended for file provider extensions which need to upload thumbnails and have a small memory limit.
--
-- @contentType@ — An image content type to save the thumbnail as, supported by CGImageDestination, such as UTTypePNG or UTTypeJPEG
--
-- @completionHandler@ — Always called when the thumbnail generation is over. Will contain an error if the thumbnail could not be successfully saved to disk at fileURL.
--
-- ObjC selector: @- saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request, IsNSURL fileURL, IsUTType contentType) => qlThumbnailGenerator -> request -> fileURL -> contentType -> Ptr () -> IO ()
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandler qlThumbnailGenerator  request fileURL contentType completionHandler =
withObjCPtr request $ \raw_request ->
  withObjCPtr fileURL $ \raw_fileURL ->
    withObjCPtr contentType $ \raw_contentType ->
        sendMsg qlThumbnailGenerator (mkSelector "saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Saves a thumbnail for the request on disk at fileURL. The file saved at fileURL has to be deleted when it is not used anymore. This is primarily intended for file provider extensions which need to upload thumbnails and have a small memory limit.
--
-- @contentType@ — An image content type to save the thumbnail as, supported by CGImageDestination, such as kUTTypePNG or kUTTypeJPEG
--
-- @completionHandler@ — Always called when the thumbnail generation is over. Will contain an error if the thumbnail could not be successfully saved to disk at fileURL.
--
-- ObjC selector: @- saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request, IsNSURL fileURL, IsNSString contentType) => qlThumbnailGenerator -> request -> fileURL -> contentType -> Ptr () -> IO ()
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandler qlThumbnailGenerator  request fileURL contentType completionHandler =
withObjCPtr request $ \raw_request ->
  withObjCPtr fileURL $ \raw_fileURL ->
    withObjCPtr contentType $ \raw_contentType ->
        sendMsg qlThumbnailGenerator (mkSelector "saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @generateBestRepresentationForRequest:completionHandler:@
generateBestRepresentationForRequest_completionHandlerSelector :: Selector
generateBestRepresentationForRequest_completionHandlerSelector = mkSelector "generateBestRepresentationForRequest:completionHandler:"

-- | @Selector@ for @generateRepresentationsForRequest:updateHandler:@
generateRepresentationsForRequest_updateHandlerSelector :: Selector
generateRepresentationsForRequest_updateHandlerSelector = mkSelector "generateRepresentationsForRequest:updateHandler:"

-- | @Selector@ for @cancelRequest:@
cancelRequestSelector :: Selector
cancelRequestSelector = mkSelector "cancelRequest:"

-- | @Selector@ for @saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector :: Selector
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector = mkSelector "saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:"

-- | @Selector@ for @saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector :: Selector
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector = mkSelector "saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:"

