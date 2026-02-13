{-# LANGUAGE DataKinds #-}
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
  , sharedGenerator
  , cancelRequestSelector
  , generateBestRepresentationForRequest_completionHandlerSelector
  , generateRepresentationsForRequest_updateHandlerSelector
  , saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector
  , saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector
  , sharedGeneratorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @completionHandler@ — Always called when the thumbnail generation is over. The thumbnail passed to this handler is the most representative version of the thumbnail that was successfully generated (if any). If set, the error contains information about the issue that occurred while trying to generate the thumbnail. QLThumbnail error codes can be found in <QuickLookThumbnailing/QLThumbnailErrors.h>.
--
-- ObjC selector: @- generateBestRepresentationForRequest:completionHandler:@
generateBestRepresentationForRequest_completionHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request) => qlThumbnailGenerator -> request -> Ptr () -> IO ()
generateBestRepresentationForRequest_completionHandler qlThumbnailGenerator request completionHandler =
  sendMessage qlThumbnailGenerator generateBestRepresentationForRequest_completionHandlerSelector (toQLThumbnailGenerationRequest request) completionHandler

-- | @updateHandler@ — Called for the successive requested representations of a thumbnail. If a representation was not successfully generated, this may be called with a nil representation. If a requested more representative version was successfully generated before a less representative one, this handler will be called only for the more representative version, skipping the less representative one. This handler is guaranteed to be called at least once, for the requested most representative version, whether a representation could be successfully generated or not. If set, the error contains information about the issue that occurred while trying to generate the representation of the given type. QLThumbnail error codes can be found in <QuickLookThumbnailing/QLThumbnailErrors.h>.
--
-- ObjC selector: @- generateRepresentationsForRequest:updateHandler:@
generateRepresentationsForRequest_updateHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request) => qlThumbnailGenerator -> request -> Ptr () -> IO ()
generateRepresentationsForRequest_updateHandler qlThumbnailGenerator request updateHandler =
  sendMessage qlThumbnailGenerator generateRepresentationsForRequest_updateHandlerSelector (toQLThumbnailGenerationRequest request) updateHandler

-- | Cancels the given QLThumbnailGenerationRequest.
--
-- @request@ — The request that should be cancelled.
--
-- ObjC selector: @- cancelRequest:@
cancelRequest :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request) => qlThumbnailGenerator -> request -> IO ()
cancelRequest qlThumbnailGenerator request =
  sendMessage qlThumbnailGenerator cancelRequestSelector (toQLThumbnailGenerationRequest request)

-- | Saves a thumbnail for the request on disk at fileURL. The file saved at fileURL has to be deleted when it is not used anymore. This is primarily intended for file provider extensions which need to upload thumbnails and have a small memory limit.
--
-- @contentType@ — An image content type to save the thumbnail as, supported by CGImageDestination, such as UTTypePNG or UTTypeJPEG
--
-- @completionHandler@ — Always called when the thumbnail generation is over. Will contain an error if the thumbnail could not be successfully saved to disk at fileURL.
--
-- ObjC selector: @- saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request, IsNSURL fileURL, IsUTType contentType) => qlThumbnailGenerator -> request -> fileURL -> contentType -> Ptr () -> IO ()
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandler qlThumbnailGenerator request fileURL contentType completionHandler =
  sendMessage qlThumbnailGenerator saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector (toQLThumbnailGenerationRequest request) (toNSURL fileURL) (toUTType contentType) completionHandler

-- | Saves a thumbnail for the request on disk at fileURL. The file saved at fileURL has to be deleted when it is not used anymore. This is primarily intended for file provider extensions which need to upload thumbnails and have a small memory limit.
--
-- @contentType@ — An image content type to save the thumbnail as, supported by CGImageDestination, such as kUTTypePNG or kUTTypeJPEG
--
-- @completionHandler@ — Always called when the thumbnail generation is over. Will contain an error if the thumbnail could not be successfully saved to disk at fileURL.
--
-- ObjC selector: @- saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandler :: (IsQLThumbnailGenerator qlThumbnailGenerator, IsQLThumbnailGenerationRequest request, IsNSURL fileURL, IsNSString contentType) => qlThumbnailGenerator -> request -> fileURL -> contentType -> Ptr () -> IO ()
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandler qlThumbnailGenerator request fileURL contentType completionHandler =
  sendMessage qlThumbnailGenerator saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector (toQLThumbnailGenerationRequest request) (toNSURL fileURL) (toNSString contentType) completionHandler

-- | @+ sharedGenerator@
sharedGenerator :: IO (Id QLThumbnailGenerator)
sharedGenerator  =
  do
    cls' <- getRequiredClass "QLThumbnailGenerator"
    sendClassMessage cls' sharedGeneratorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @generateBestRepresentationForRequest:completionHandler:@
generateBestRepresentationForRequest_completionHandlerSelector :: Selector '[Id QLThumbnailGenerationRequest, Ptr ()] ()
generateBestRepresentationForRequest_completionHandlerSelector = mkSelector "generateBestRepresentationForRequest:completionHandler:"

-- | @Selector@ for @generateRepresentationsForRequest:updateHandler:@
generateRepresentationsForRequest_updateHandlerSelector :: Selector '[Id QLThumbnailGenerationRequest, Ptr ()] ()
generateRepresentationsForRequest_updateHandlerSelector = mkSelector "generateRepresentationsForRequest:updateHandler:"

-- | @Selector@ for @cancelRequest:@
cancelRequestSelector :: Selector '[Id QLThumbnailGenerationRequest] ()
cancelRequestSelector = mkSelector "cancelRequest:"

-- | @Selector@ for @saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector :: Selector '[Id QLThumbnailGenerationRequest, Id NSURL, Id UTType, Ptr ()] ()
saveBestRepresentationForRequest_toFileAtURL_asContentType_completionHandlerSelector = mkSelector "saveBestRepresentationForRequest:toFileAtURL:asContentType:completionHandler:"

-- | @Selector@ for @saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:@
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector :: Selector '[Id QLThumbnailGenerationRequest, Id NSURL, Id NSString, Ptr ()] ()
saveBestRepresentationForRequest_toFileAtURL_withContentType_completionHandlerSelector = mkSelector "saveBestRepresentationForRequest:toFileAtURL:withContentType:completionHandler:"

-- | @Selector@ for @sharedGenerator@
sharedGeneratorSelector :: Selector '[] (Id QLThumbnailGenerator)
sharedGeneratorSelector = mkSelector "sharedGenerator"

