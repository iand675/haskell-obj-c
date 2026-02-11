{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVVideoComposition object represents an immutable video composition.
--
-- A video composition describes, for any time in the aggregate time range of its instructions, the number and IDs of video tracks that are to be used in order to produce a composed video frame corresponding to that time. When AVFoundation's built-in video compositor is used, the instructions an AVVideoComposition contain can specify a spatial transformation, an opacity value, and a cropping rectangle for each video source, and these can vary over time via simple linear ramping functions.
--
-- A client can implement their own custom video compositor by implementing the AVVideoCompositing protocol; a custom video compositor is provided with pixel buffers for each of its video sources during playback and other operations and can perform arbitrary graphical operations on them in order to produce visual output.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVVideoComposition@.
module ObjC.AVFoundation.AVVideoComposition
  ( AVVideoComposition
  , IsAVVideoComposition(..)
  , videoCompositionWithPropertiesOfAsset
  , videoCompositionWithPropertiesOfAsset_completionHandler
  , videoCompositionWithVideoComposition
  , videoCompositionWithAsset_applyingCIFiltersWithHandler
  , videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandler
  , sourceTrackIDForFrameTiming
  , renderScale
  , animationTool
  , perFrameHDRDisplayMetadataPolicy
  , videoCompositionWithPropertiesOfAssetSelector
  , videoCompositionWithPropertiesOfAsset_completionHandlerSelector
  , videoCompositionWithVideoCompositionSelector
  , videoCompositionWithAsset_applyingCIFiltersWithHandlerSelector
  , videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandlerSelector
  , sourceTrackIDForFrameTimingSelector
  , renderScaleSelector
  , animationToolSelector
  , perFrameHDRDisplayMetadataPolicySelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a new instance of AVVideoComposition with values and instructions suitable for presenting the video tracks of the specified asset according to its temporal and geometric properties and those of its tracks.
--
-- The returned AVVideoComposition will have instructions that respect the spatial properties and timeRanges of the specified asset's video tracks. It will also have the following values for its properties:
--
-- - If the asset has exactly one video track, the original timing of the source video track will be used. If the asset has more than one video track, and the nominal frame rate of any of video tracks is known, the reciprocal of the greatest known nominalFrameRate will be used as the value of frameDuration. Otherwise, a default framerate of 30fps is used. - If the specified asset is an instance of AVComposition, the renderSize will be set to the naturalSize of the AVComposition; otherwise the renderSize will be set to a value that encompasses all of the asset's video tracks. - A renderScale of 1.0. - A nil animationTool.
--
-- If the specified asset has no video tracks, this method will return an AVVideoComposition instance with an empty collection of instructions.
--
-- - Parameter asset: An instance of AVAsset. Ensure that the duration and tracks properties of the asset are already loaded before invoking this method.
--
-- - Returns: An instance of AVVideoComposition.
--
-- ObjC selector: @+ videoCompositionWithPropertiesOfAsset:@
videoCompositionWithPropertiesOfAsset :: IsAVAsset asset => asset -> IO (Id AVVideoComposition)
videoCompositionWithPropertiesOfAsset asset =
  do
    cls' <- getRequiredClass "AVVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithPropertiesOfAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

-- | Vends a new instance of AVVideoComposition with values and instructions suitable for presenting the video tracks of the specified asset according to its temporal and geometric properties and those of its tracks.
--
-- The new AVVideoComposition will have instructions that respect the spatial properties and timeRanges of the specified asset's video tracks. It will also have the following values for its properties:
--
-- - If the asset has exactly one video track, the original timing of the source video track will be used. If the asset has more than one video track, and the nominal frame rate of any of video tracks is known, the reciprocal of the greatest known nominalFrameRate will be used as the value of frameDuration. Otherwise, a default framerate of 30fps is used. - If the specified asset is an instance of AVComposition, the renderSize will be set to the naturalSize of the AVComposition; otherwise the renderSize will be set to a value that encompasses all of the asset's video tracks. - A renderScale of 1.0. - A nil animationTool.
--
-- If the specified asset has no video tracks, this method will return an AVVideoComposition instance with an empty collection of instructions.
--
-- - Parameter asset: An instance of AVAsset. - Parameter completionHandler: A block that is invoked when the new video composition has finished being created. If the @videoComposition@ parameter is nil, the @error@ parameter describes the failure that occurred.
--
-- ObjC selector: @+ videoCompositionWithPropertiesOfAsset:completionHandler:@
videoCompositionWithPropertiesOfAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
videoCompositionWithPropertiesOfAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "AVVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithPropertiesOfAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Pass-through initializer, for internal use in AVFoundation only
--
-- ObjC selector: @+ videoCompositionWithVideoComposition:@
videoCompositionWithVideoComposition :: IsAVVideoComposition videoComposition => videoComposition -> IO (Id AVVideoComposition)
videoCompositionWithVideoComposition videoComposition =
  do
    cls' <- getRequiredClass "AVVideoComposition"
    withObjCPtr videoComposition $ \raw_videoComposition ->
      sendClassMsg cls' (mkSelector "videoCompositionWithVideoComposition:") (retPtr retVoid) [argPtr (castPtr raw_videoComposition :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a new instance of AVVideoComposition with values and instructions that will apply the specified handler block to video frames represented as instances of CIImage.
--
-- The returned AVVideoComposition will cause the specified handler block to be called to filter each frame of the asset's first enabled video track. The handler block should use the properties of the provided AVAsynchronousCIImageFilteringRequest and respond using finishWithImage:context: with a "filtered" new CIImage (or the provided source image for no affect). In the event of an error, respond to the request using finishWithError:. The error can be observed via AVPlayerItemFailedToPlayToEndTimeNotification, see AVPlayerItemFailedToPlayToEndTimeErrorKey in notification payload.
--
-- NOTE: The returned AVVideoComposition's properties are private and support only CIFilter-based operations. Mutations are not supported, either in the values of properties of the AVVideoComposition itself or in its private instructions. If rotations or other transformations are desired, they must be accomplished via the application of CIFilters during the execution of your specified handler.
--
-- The video composition will also have the following values for its properties:
--
-- - The original timing of the asset's first enabled video track will be used. - A renderSize that encompasses the asset's first enabled video track respecting the track's preferredTransform. - A renderScale of 1.0.
--
-- The default CIContext has the following properties:
--
-- - iOS: Device RGB color space - macOS: sRGB color space
--
-- Example usage: ```objc playerItem.videoComposition = [AVVideoComposition videoCompositionWithAsset:srcAsset applyingCIFiltersWithHandler: 	^(AVAsynchronousCIImageFilteringRequest *request) 	{ 		NSError *err = nil; 		CIImage *filtered = myRenderer(request, &err); 		if (filtered) 			[request finishWithImage:filtered context:nil]; 		else 			[request finishWithError:err]; 	}]; ```
--
-- - Parameter asset: An instance of AVAsset. For best performance, ensure that the duration and tracks properties of the asset are already loaded before invoking this method.
--
-- - Returns: An instance of AVVideoComposition.
--
-- ObjC selector: @+ videoCompositionWithAsset:applyingCIFiltersWithHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandler :: IsAVAsset asset => asset -> Ptr () -> IO (Id AVVideoComposition)
videoCompositionWithAsset_applyingCIFiltersWithHandler asset applier =
  do
    cls' <- getRequiredClass "AVVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr applier :: Ptr ())] >>= retainedObject . castPtr

-- | Vends a new instance of AVVideoComposition with values and instructions that will apply the specified handler block to video frames represented as instances of CIImage.
--
-- The new AVVideoComposition will cause the specified handler block to be called to filter each frame of the asset's first enabled video track. The handler block should use the properties of the provided AVAsynchronousCIImageFilteringRequest and respond using finishWithImage:context: with a "filtered" new CIImage (or the provided source image for no affect). In the event of an error, respond to the request using finishWithError:. The error can be observed via AVPlayerItemFailedToPlayToEndTimeNotification, see AVPlayerItemFailedToPlayToEndTimeErrorKey in notification payload.
--
-- NOTE: The returned AVVideoComposition's properties are private and support only CIFilter-based operations. Mutations are not supported, either in the values of properties of the AVVideoComposition itself or in its private instructions. If rotations or other transformations are desired, they must be accomplished via the application of CIFilters during the execution of your specified handler.
--
-- The video composition will also have the following values for its properties:
--
-- - The original timing of the asset's first enabled video track will be used. - A renderSize that encompasses the asset's first enabled video track respecting the track's preferredTransform. - A renderScale of 1.0.
--
-- The default CIContext has the following properties:
--
-- - iOS: Device RGB color space - macOS: sRGB color space
--
-- Example usage: ```objc [AVVideoComposition videoCompositionWithAsset:srcAsset applyingCIFiltersWithHandler: 	^(AVAsynchronousCIImageFilteringRequest *request) 	{ 		NSError *err = nil; 		CIImage *filtered = myRenderer(request, &err); 		if (filtered) 			[request finishWithImage:filtered context:nil]; 		else 			[request finishWithError:err]; 	} completionHandler: 	^(AVVideoComposition * _Nullable videoComposition, NSError * _Nullable error) 	{ 		if (videoComposition != nil) { 			playerItem.videoComposition = videoComposition 		else { 			// handle error 	}]; ``` - Parameter asset: An instance of AVAsset. - Parameter completionHandler: A block that is invoked when the new video composition has finished being created. If the @videoComposition@ parameter is nil, the @error@ parameter describes the failure that occurred.
--
-- ObjC selector: @+ videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandler :: IsAVAsset asset => asset -> Ptr () -> Ptr () -> IO ()
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandler asset applier completionHandler =
  do
    cls' <- getRequiredClass "AVVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr applier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | If sourceTrackIDForFrameTiming is not kCMPersistentTrackID_Invalid, frame timing for the video composition is derived from the source asset's track with the corresponding ID. This may be used to preserve a source asset's variable frame timing. If an empty edit is encountered in the source asset’s track, the compositor composes frames as needed up to the frequency specified in frameDuration property. */
--
-- ObjC selector: @- sourceTrackIDForFrameTiming@
sourceTrackIDForFrameTiming :: IsAVVideoComposition avVideoComposition => avVideoComposition -> IO CInt
sourceTrackIDForFrameTiming avVideoComposition  =
  sendMsg avVideoComposition (mkSelector "sourceTrackIDForFrameTiming") retCInt []

-- | Indicates the scale at which the video composition should render. May only be other than 1.0 for a video composition set on an AVPlayerItem
--
-- ObjC selector: @- renderScale@
renderScale :: IsAVVideoComposition avVideoComposition => avVideoComposition -> IO CFloat
renderScale avVideoComposition  =
  sendMsg avVideoComposition (mkSelector "renderScale") retCFloat []

-- | Indicates a special video composition tool for use of Core Animation; may be nil
--
-- ObjC selector: @- animationTool@
animationTool :: IsAVVideoComposition avVideoComposition => avVideoComposition -> IO (Id AVVideoCompositionCoreAnimationTool)
animationTool avVideoComposition  =
  sendMsg avVideoComposition (mkSelector "animationTool") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures policy for per frame HDR display metadata on the rendered frame
--
-- Allows the system to identify situations where HDR metadata can be generated and attached to the rendered video frame. Default is AVVideoCompositionPerFrameHDRDisplayMetadataPolicyPropagate. Any HDR metadata attached to the composed frame will be propagated to the rendered video frames.
--
-- ObjC selector: @- perFrameHDRDisplayMetadataPolicy@
perFrameHDRDisplayMetadataPolicy :: IsAVVideoComposition avVideoComposition => avVideoComposition -> IO (Id NSString)
perFrameHDRDisplayMetadataPolicy avVideoComposition  =
  sendMsg avVideoComposition (mkSelector "perFrameHDRDisplayMetadataPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionWithPropertiesOfAsset:@
videoCompositionWithPropertiesOfAssetSelector :: Selector
videoCompositionWithPropertiesOfAssetSelector = mkSelector "videoCompositionWithPropertiesOfAsset:"

-- | @Selector@ for @videoCompositionWithPropertiesOfAsset:completionHandler:@
videoCompositionWithPropertiesOfAsset_completionHandlerSelector :: Selector
videoCompositionWithPropertiesOfAsset_completionHandlerSelector = mkSelector "videoCompositionWithPropertiesOfAsset:completionHandler:"

-- | @Selector@ for @videoCompositionWithVideoComposition:@
videoCompositionWithVideoCompositionSelector :: Selector
videoCompositionWithVideoCompositionSelector = mkSelector "videoCompositionWithVideoComposition:"

-- | @Selector@ for @videoCompositionWithAsset:applyingCIFiltersWithHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandlerSelector :: Selector
videoCompositionWithAsset_applyingCIFiltersWithHandlerSelector = mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:"

-- | @Selector@ for @videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandlerSelector :: Selector
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandlerSelector = mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:"

-- | @Selector@ for @sourceTrackIDForFrameTiming@
sourceTrackIDForFrameTimingSelector :: Selector
sourceTrackIDForFrameTimingSelector = mkSelector "sourceTrackIDForFrameTiming"

-- | @Selector@ for @renderScale@
renderScaleSelector :: Selector
renderScaleSelector = mkSelector "renderScale"

-- | @Selector@ for @animationTool@
animationToolSelector :: Selector
animationToolSelector = mkSelector "animationTool"

-- | @Selector@ for @perFrameHDRDisplayMetadataPolicy@
perFrameHDRDisplayMetadataPolicySelector :: Selector
perFrameHDRDisplayMetadataPolicySelector = mkSelector "perFrameHDRDisplayMetadataPolicy"

