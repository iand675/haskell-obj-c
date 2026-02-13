{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVAsynchronousVideoCompositionRequest instance contains the information necessary for a video compositor to render an output pixel buffer. The video compositor must implement the AVVideoCompositing protocol.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAsynchronousVideoCompositionRequest@.
module ObjC.AVFoundation.AVAsynchronousVideoCompositionRequest
  ( AVAsynchronousVideoCompositionRequest
  , IsAVAsynchronousVideoCompositionRequest(..)
  , sourceFrameByTrackID
  , sourceSampleBufferByTrackID
  , sourceTimedMetadataByTrackID
  , finishWithComposedVideoFrame
  , finishWithError
  , finishCancelledRequest
  , sourceTaggedBufferGroupByTrackID
  , finishWithComposedTaggedBufferGroup
  , attachSpatialVideoConfiguration_toPixelBuffer
  , renderContext
  , sourceTrackIDs
  , sourceSampleDataTrackIDs
  , videoCompositionInstruction
  , attachSpatialVideoConfiguration_toPixelBufferSelector
  , finishCancelledRequestSelector
  , finishWithComposedTaggedBufferGroupSelector
  , finishWithComposedVideoFrameSelector
  , finishWithErrorSelector
  , renderContextSelector
  , sourceFrameByTrackIDSelector
  , sourceSampleBufferByTrackIDSelector
  , sourceSampleDataTrackIDsSelector
  , sourceTaggedBufferGroupByTrackIDSelector
  , sourceTimedMetadataByTrackIDSelector
  , sourceTrackIDsSelector
  , videoCompositionInstructionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the source CVPixelBufferRef for the given track ID
--
-- If the track contains tagged buffers, a pixel buffer from one of the tagged buffers will be returned.
--
-- - Parameter trackID: The track ID for the requested source frame
--
-- ObjC selector: @- sourceFrameByTrackID:@
sourceFrameByTrackID :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> CInt -> IO (Ptr ())
sourceFrameByTrackID avAsynchronousVideoCompositionRequest trackID =
  sendMessage avAsynchronousVideoCompositionRequest sourceFrameByTrackIDSelector trackID

-- | Returns the source CMSampleBufferRef for the given track ID
--
-- - Parameter trackID: The track ID for the requested source sample buffer
--
-- ObjC selector: @- sourceSampleBufferByTrackID:@
sourceSampleBufferByTrackID :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> CInt -> IO (Ptr ())
sourceSampleBufferByTrackID avAsynchronousVideoCompositionRequest trackID =
  sendMessage avAsynchronousVideoCompositionRequest sourceSampleBufferByTrackIDSelector trackID

-- | Returns the source AVTimedMetadataGroup * for the given track ID
--
-- - Parameter trackID: The track ID for the requested source timed metadata group.
--
-- ObjC selector: @- sourceTimedMetadataByTrackID:@
sourceTimedMetadataByTrackID :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> CInt -> IO (Id AVTimedMetadataGroup)
sourceTimedMetadataByTrackID avAsynchronousVideoCompositionRequest trackID =
  sendMessage avAsynchronousVideoCompositionRequest sourceTimedMetadataByTrackIDSelector trackID

-- | The method that the custom compositor calls when composition succeeds.
--
-- - Parameter composedVideoFrame: The video frame to finish with. Call finishWithComposedTaggedBufferGroup: instead if outputBufferDescription is non-nil.
--
-- ObjC selector: @- finishWithComposedVideoFrame:@
finishWithComposedVideoFrame :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> Ptr () -> IO ()
finishWithComposedVideoFrame avAsynchronousVideoCompositionRequest composedVideoFrame =
  sendMessage avAsynchronousVideoCompositionRequest finishWithComposedVideoFrameSelector composedVideoFrame

-- | callback the custom compositor should call when composition failed. The error parameter should describe the actual error.
--
-- ObjC selector: @- finishWithError:@
finishWithError :: (IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest, IsNSError error_) => avAsynchronousVideoCompositionRequest -> error_ -> IO ()
finishWithError avAsynchronousVideoCompositionRequest error_ =
  sendMessage avAsynchronousVideoCompositionRequest finishWithErrorSelector (toNSError error_)

-- | callback the custom compositor should call for a request that has been cancelled.
--
-- ObjC selector: @- finishCancelledRequest@
finishCancelledRequest :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> IO ()
finishCancelledRequest avAsynchronousVideoCompositionRequest =
  sendMessage avAsynchronousVideoCompositionRequest finishCancelledRequestSelector

-- | Returns the source CMTaggedBufferGroupRef for the given track ID.
--
-- Returns nil if the video track does not contain tagged buffers. Returns nil if the track does not contain video. This function should only be called when supportsSourceTaggedBuffers is YES.
--
-- - Parameter trackID: The track ID for the requested source tagged buffer group.
--
-- ObjC selector: @- sourceTaggedBufferGroupByTrackID:@
sourceTaggedBufferGroupByTrackID :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> CInt -> IO (Ptr ())
sourceTaggedBufferGroupByTrackID avAsynchronousVideoCompositionRequest trackID =
  sendMessage avAsynchronousVideoCompositionRequest sourceTaggedBufferGroupByTrackIDSelector trackID

-- | The method that the custom compositor calls when composition succeeds.
--
-- - Parameter taggedBufferGroup: The tagged buffer group containing the composed tagged buffers. The tagged buffers must be compatible with the outputBufferDescription specified in the video composition. The outputBufferDescription must not be nil when calling this function. NOTE: If ``AVVideoComposition/spatialConfigurations`` is not empty, then ``attach(spatialVideoConfiguration:to:)`` must be called with one of the spatial configurations. An exception will be thrown otherwise. Also, all pixel buffers must be associated with the same spatial configuration. An exception will be thrown otherwise.
--
-- ObjC selector: @- finishWithComposedTaggedBufferGroup:@
finishWithComposedTaggedBufferGroup :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> Ptr () -> IO ()
finishWithComposedTaggedBufferGroup avAsynchronousVideoCompositionRequest taggedBufferGroup =
  sendMessage avAsynchronousVideoCompositionRequest finishWithComposedTaggedBufferGroupSelector taggedBufferGroup

-- | Associates the pixel buffer with the specified spatial configuration. - Parameters:   - spatialVideoConfiguration: The spatial configuration to associate with the pixel buffer.   - pixelBuffer: The pixel buffer to associate with the spatial configuration. NOTE: The spatial configuration must be one of the spatial configurations specified in the ``AVVideoComposition/spatialConfigurations`` property. An exception will be thrown otherwise. NOTE: All pixel buffers from the custom compositor must be associated with the same spatial configuration. An exception will be thrown otherwise. A spatial configuration with all nil values indicates the video is not spatial. A nil spatial configuration also indicates the video is not spatial. The value can be nil, which indicates the output will not be spatial, but a spatial configuration with all nil values must be in the ``AVVideoComposition/spatialConfigurations`` property or an exception will be thrown.
--
-- ObjC selector: @- attachSpatialVideoConfiguration:toPixelBuffer:@
attachSpatialVideoConfiguration_toPixelBuffer :: (IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest, IsAVSpatialVideoConfiguration spatialVideoConfiguration) => avAsynchronousVideoCompositionRequest -> spatialVideoConfiguration -> Ptr () -> IO ()
attachSpatialVideoConfiguration_toPixelBuffer avAsynchronousVideoCompositionRequest spatialVideoConfiguration pixelBuffer =
  sendMessage avAsynchronousVideoCompositionRequest attachSpatialVideoConfiguration_toPixelBufferSelector (toAVSpatialVideoConfiguration spatialVideoConfiguration) pixelBuffer

-- | The AVVideoCompositionRenderContext making the request
--
-- ObjC selector: @- renderContext@
renderContext :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> IO (Id AVVideoCompositionRenderContext)
renderContext avAsynchronousVideoCompositionRequest =
  sendMessage avAsynchronousVideoCompositionRequest renderContextSelector

-- | Track IDs of all the source video buffers that are available to compose the frame.
--
-- ObjC selector: @- sourceTrackIDs@
sourceTrackIDs :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> IO (Id NSArray)
sourceTrackIDs avAsynchronousVideoCompositionRequest =
  sendMessage avAsynchronousVideoCompositionRequest sourceTrackIDsSelector

-- | Track IDs of all the source sample data buffers that are available to compose the frame.
--
-- ObjC selector: @- sourceSampleDataTrackIDs@
sourceSampleDataTrackIDs :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> IO (Id NSArray)
sourceSampleDataTrackIDs avAsynchronousVideoCompositionRequest =
  sendMessage avAsynchronousVideoCompositionRequest sourceSampleDataTrackIDsSelector

-- | The AVVideoCompositionInstruction to use to compose the frame.
--
-- ObjC selector: @- videoCompositionInstruction@
videoCompositionInstruction :: IsAVAsynchronousVideoCompositionRequest avAsynchronousVideoCompositionRequest => avAsynchronousVideoCompositionRequest -> IO RawId
videoCompositionInstruction avAsynchronousVideoCompositionRequest =
  sendMessage avAsynchronousVideoCompositionRequest videoCompositionInstructionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceFrameByTrackID:@
sourceFrameByTrackIDSelector :: Selector '[CInt] (Ptr ())
sourceFrameByTrackIDSelector = mkSelector "sourceFrameByTrackID:"

-- | @Selector@ for @sourceSampleBufferByTrackID:@
sourceSampleBufferByTrackIDSelector :: Selector '[CInt] (Ptr ())
sourceSampleBufferByTrackIDSelector = mkSelector "sourceSampleBufferByTrackID:"

-- | @Selector@ for @sourceTimedMetadataByTrackID:@
sourceTimedMetadataByTrackIDSelector :: Selector '[CInt] (Id AVTimedMetadataGroup)
sourceTimedMetadataByTrackIDSelector = mkSelector "sourceTimedMetadataByTrackID:"

-- | @Selector@ for @finishWithComposedVideoFrame:@
finishWithComposedVideoFrameSelector :: Selector '[Ptr ()] ()
finishWithComposedVideoFrameSelector = mkSelector "finishWithComposedVideoFrame:"

-- | @Selector@ for @finishWithError:@
finishWithErrorSelector :: Selector '[Id NSError] ()
finishWithErrorSelector = mkSelector "finishWithError:"

-- | @Selector@ for @finishCancelledRequest@
finishCancelledRequestSelector :: Selector '[] ()
finishCancelledRequestSelector = mkSelector "finishCancelledRequest"

-- | @Selector@ for @sourceTaggedBufferGroupByTrackID:@
sourceTaggedBufferGroupByTrackIDSelector :: Selector '[CInt] (Ptr ())
sourceTaggedBufferGroupByTrackIDSelector = mkSelector "sourceTaggedBufferGroupByTrackID:"

-- | @Selector@ for @finishWithComposedTaggedBufferGroup:@
finishWithComposedTaggedBufferGroupSelector :: Selector '[Ptr ()] ()
finishWithComposedTaggedBufferGroupSelector = mkSelector "finishWithComposedTaggedBufferGroup:"

-- | @Selector@ for @attachSpatialVideoConfiguration:toPixelBuffer:@
attachSpatialVideoConfiguration_toPixelBufferSelector :: Selector '[Id AVSpatialVideoConfiguration, Ptr ()] ()
attachSpatialVideoConfiguration_toPixelBufferSelector = mkSelector "attachSpatialVideoConfiguration:toPixelBuffer:"

-- | @Selector@ for @renderContext@
renderContextSelector :: Selector '[] (Id AVVideoCompositionRenderContext)
renderContextSelector = mkSelector "renderContext"

-- | @Selector@ for @sourceTrackIDs@
sourceTrackIDsSelector :: Selector '[] (Id NSArray)
sourceTrackIDsSelector = mkSelector "sourceTrackIDs"

-- | @Selector@ for @sourceSampleDataTrackIDs@
sourceSampleDataTrackIDsSelector :: Selector '[] (Id NSArray)
sourceSampleDataTrackIDsSelector = mkSelector "sourceSampleDataTrackIDs"

-- | @Selector@ for @videoCompositionInstruction@
videoCompositionInstructionSelector :: Selector '[] RawId
videoCompositionInstructionSelector = mkSelector "videoCompositionInstruction"

