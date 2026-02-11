{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A controller object that is used to perform one or more requests on a video stream.
--
-- VNVideoProcessor handles the video decoding and buffer management, feeding the buffers to the associated requests at the best desired frame rate.
--
-- Generated bindings for @VNVideoProcessor@.
module ObjC.Vision.VNVideoProcessor
  ( VNVideoProcessor
  , IsVNVideoProcessor(..)
  , init_
  , initWithURL
  , addRequest_processingOptions_error
  , addRequest_withProcessingOptions_error
  , removeRequest_error
  , cancel
  , initSelector
  , initWithURLSelector
  , addRequest_processingOptions_errorSelector
  , addRequest_withProcessingOptions_errorSelector
  , removeRequest_errorSelector
  , cancelSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNVideoProcessor vnVideoProcessor => vnVideoProcessor -> IO (Id VNVideoProcessor)
init_ vnVideoProcessor  =
  sendMsg vnVideoProcessor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a VNVideoProcessor to be used for performing requests against a video asset specified by it's URL.
--
-- @videoURL@ — A URL pointing at a video asset on which the requests will be performed. The video format has to be supported by AVFoundation.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsVNVideoProcessor vnVideoProcessor, IsNSURL videoURL) => vnVideoProcessor -> videoURL -> IO (Id VNVideoProcessor)
initWithURL vnVideoProcessor  videoURL =
withObjCPtr videoURL $ \raw_videoURL ->
    sendMsg vnVideoProcessor (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_videoURL :: Ptr ())] >>= ownedObject . castPtr

-- | Add a VNRequest with the specified processing options to be performed on the video.
--
-- This method can be called either before calling -analyzeTimeRange:error: or from within one of the already associated request's completion handlers.
--
-- @request@ — The VNRequest to be added to the processing pipeline. If added from within a completionHandler, it will be processed on the same frame that is currently being processed.
--
-- @processingOptions@ — The options applied to the request's processing of the video.
--
-- @error@ — Returns an error that happened during scheduling of the requests. Check individual requests results and errors for their respective success and failures. This parameter is optional.
--
-- Returns: Returns true if the request added to the processing pipeline.
--
-- Note: The VNRequest must have completion handler set otherwise no results can be returned.
--
-- ObjC selector: @- addRequest:processingOptions:error:@
addRequest_processingOptions_error :: (IsVNVideoProcessor vnVideoProcessor, IsVNRequest request, IsVNVideoProcessorRequestProcessingOptions processingOptions, IsNSError error_) => vnVideoProcessor -> request -> processingOptions -> error_ -> IO Bool
addRequest_processingOptions_error vnVideoProcessor  request processingOptions error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr processingOptions $ \raw_processingOptions ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnVideoProcessor (mkSelector "addRequest:processingOptions:error:") retCULong [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_processingOptions :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- addRequest:withProcessingOptions:error:@
addRequest_withProcessingOptions_error :: (IsVNVideoProcessor vnVideoProcessor, IsVNRequest request, IsNSDictionary processingOptions, IsNSError error_) => vnVideoProcessor -> request -> processingOptions -> error_ -> IO Bool
addRequest_withProcessingOptions_error vnVideoProcessor  request processingOptions error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr processingOptions $ \raw_processingOptions ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnVideoProcessor (mkSelector "addRequest:withProcessingOptions:error:") retCULong [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_processingOptions :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Remove a VNRequest from the video processor, which means it won't be performed anymore.
--
-- This method can be called either before calling -analyzeTimeRange:error: or from within one of the already associated request's completion handlers.
--
-- @request@ — The VNRequest to be removed from the processing pipeline.
--
-- @error@ — Returns an error that happened during processing of the request, such as if the request was not found in the processing queue. This parameter is optional.
--
-- Returns: Returns true if the request was found and removed from the processing pipeline.
--
-- ObjC selector: @- removeRequest:error:@
removeRequest_error :: (IsVNVideoProcessor vnVideoProcessor, IsVNRequest request, IsNSError error_) => vnVideoProcessor -> request -> error_ -> IO Bool
removeRequest_error vnVideoProcessor  request error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnVideoProcessor (mkSelector "removeRequest:error:") retCULong [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Cancel the processing of the video. This can return before the last request has completed.
--
-- ObjC selector: @- cancel@
cancel :: IsVNVideoProcessor vnVideoProcessor => vnVideoProcessor -> IO ()
cancel vnVideoProcessor  =
  sendMsg vnVideoProcessor (mkSelector "cancel") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @addRequest:processingOptions:error:@
addRequest_processingOptions_errorSelector :: Selector
addRequest_processingOptions_errorSelector = mkSelector "addRequest:processingOptions:error:"

-- | @Selector@ for @addRequest:withProcessingOptions:error:@
addRequest_withProcessingOptions_errorSelector :: Selector
addRequest_withProcessingOptions_errorSelector = mkSelector "addRequest:withProcessingOptions:error:"

-- | @Selector@ for @removeRequest:error:@
removeRequest_errorSelector :: Selector
removeRequest_errorSelector = mkSelector "removeRequest:error:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

