{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableVideoComposition@.
module ObjC.AVFoundation.AVMutableVideoComposition
  ( AVMutableVideoComposition
  , IsAVMutableVideoComposition(..)
  , videoComposition
  , videoCompositionWithPropertiesOfAsset
  , videoCompositionWithPropertiesOfAsset_completionHandler
  , videoCompositionWithPropertiesOfAsset_prototypeInstruction
  , videoCompositionWithPropertiesOfAsset_prototypeInstruction_completionHandler
  , videoCompositionWithAsset_applyingCIFiltersWithHandler
  , videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandler
  , sourceTrackIDForFrameTiming
  , setSourceTrackIDForFrameTiming
  , renderScale
  , setRenderScale
  , animationTool
  , setAnimationTool
  , perFrameHDRDisplayMetadataPolicy
  , setPerFrameHDRDisplayMetadataPolicy
  , videoCompositionSelector
  , videoCompositionWithPropertiesOfAssetSelector
  , videoCompositionWithPropertiesOfAsset_completionHandlerSelector
  , videoCompositionWithPropertiesOfAsset_prototypeInstructionSelector
  , videoCompositionWithPropertiesOfAsset_prototypeInstruction_completionHandlerSelector
  , videoCompositionWithAsset_applyingCIFiltersWithHandlerSelector
  , videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandlerSelector
  , sourceTrackIDForFrameTimingSelector
  , setSourceTrackIDForFrameTimingSelector
  , renderScaleSelector
  , setRenderScaleSelector
  , animationToolSelector
  , setAnimationToolSelector
  , perFrameHDRDisplayMetadataPolicySelector
  , setPerFrameHDRDisplayMetadataPolicySelector


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

-- | Returns a new instance of AVMutableVideoComposition.
--
-- The returned AVMutableVideoComposition will have a frameDuration of kCMTimeZero, a renderSize of {0.0, 0.0}, a nil array of instructions, and a nil animationTool.
--
-- ObjC selector: @+ videoComposition@
videoComposition :: IO (Id AVMutableVideoComposition)
videoComposition  =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    sendClassMsg cls' (mkSelector "videoComposition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a new instance of AVMutableVideoComposition with values and instructions suitable for presenting the video tracks of the specified asset according to its temporal and geometric properties and those of its tracks.
--
-- The returned AVMutableVideoComposition will have instructions that respect the spatial properties and timeRanges of the specified asset's video tracks. The client can set sourceTrackIDForFrameTiming to kCMPersistentTrackID_Invalid and frameDuration to an appropriate value in order to specify the maximum output frame rate independent of the source track timing. It will also have the following values for its properties:
--
-- - If the asset has exactly one video track, the original timing of the source video track will be used. If the asset has more than one video track, and the nominal frame rate of any of video tracks is known, the reciprocal of the greatest known nominalFrameRate will be used as the value of frameDuration. Otherwise, a default framerate of 30fps is used. - If the specified asset is an instance of AVComposition, the renderSize will be set to the naturalSize of the AVComposition; otherwise the renderSize will be set to a value that encompasses all of the asset's video tracks. - A renderScale of 1.0. - A nil animationTool.
--
-- If the specified asset has no video tracks, this method will return an AVMutableVideoComposition instance with an empty collection of instructions.
--
-- - Parameter asset: An instance of AVAsset. For best performance, ensure that the duration and tracks properties of the asset are already loaded before invoking this method.
--
-- - Returns: An instance of AVMutableVideoComposition.
--
-- ObjC selector: @+ videoCompositionWithPropertiesOfAsset:@
videoCompositionWithPropertiesOfAsset :: IsAVAsset asset => asset -> IO (Id AVMutableVideoComposition)
videoCompositionWithPropertiesOfAsset asset =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithPropertiesOfAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

-- | Vends a new instance of AVMutableVideoComposition with values and instructions suitable for presenting the video tracks of the specified asset according to its temporal and geometric properties and those of its tracks.
--
-- The new AVMutableVideoComposition will have instructions that respect the spatial properties and timeRanges of the specified asset's video tracks. The client can set sourceTrackIDForFrameTiming to kCMPersistentTrackID_Invalid and frameDuration to an appropriate value in order to specify the maximum output frame rate independent of the source track timing. It will also have the following values for its properties:
--
-- - If the asset has exactly one video track, the original timing of the source video track will be used. If the asset has more than one video track, and the nominal frame rate of any of video tracks is known, the reciprocal of the greatest known nominalFrameRate will be used as the value of frameDuration. Otherwise, a default framerate of 30fps is used. - If the specified asset is an instance of AVComposition, the renderSize will be set to the naturalSize of the AVComposition; otherwise the renderSize will be set to a value that encompasses all of the asset's video tracks. - A renderScale of 1.0. - A nil animationTool.
--
-- If the specified asset has no video tracks, this method will return an AVMutableVideoComposition instance with an empty collection of instructions.
--
-- - Parameter asset: An instance of AVAsset. - Parameter completionHandler: A block that is invoked when the new video composition has finished being created. If the @videoComposition@ parameter is nil, the @error@ parameter describes the failure that occurred.
--
-- ObjC selector: @+ videoCompositionWithPropertiesOfAsset:completionHandler:@
videoCompositionWithPropertiesOfAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
videoCompositionWithPropertiesOfAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithPropertiesOfAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Returns a new instance of AVMutableVideoComposition with values and instructions suitable for presenting the video tracks of the specified asset according to its temporal and geometric properties and those of its tracks, and also overrides default properties with those from a prototypeInstruction.
--
-- Also see videoCompositionWithPropertiesOfAsset:. The returned AVVideoComposition will have instructions that respect the spatial properties and timeRanges of the specified asset's video tracks. Anything not pertaining to spatial layout and timing, such as background color for their composition or post-processing behaviors, is eligible to be specified via a prototype instruction. Example: To add a background color, ```objc myPrototypeInstruction = [[AVMutableVideoCompositionInstruction alloc] init]; myPrototypeInstruction.backgroundColor = myCGColorRef; // Do not use constant CGColorRef colors here. myVideoComposition = [AVVideoComposition videoCompositionWithPropertiesOfAsset:myAsset prototypeInstruction:myPrototypeInstruction]; ``` - Parameter asset: An instance of AVAsset. For best performance, ensure that the duration and tracks properties of the asset are already loaded before invoking this method. - Parameter prototypeInstruction: Custom instructions that the client can choose to override.
--
-- - Returns: An instance of AVMutableVideoComposition.
--
-- ObjC selector: @+ videoCompositionWithPropertiesOfAsset:prototypeInstruction:@
videoCompositionWithPropertiesOfAsset_prototypeInstruction :: (IsAVAsset asset, IsAVVideoCompositionInstruction prototypeInstruction) => asset -> prototypeInstruction -> IO (Id AVMutableVideoComposition)
videoCompositionWithPropertiesOfAsset_prototypeInstruction asset prototypeInstruction =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr prototypeInstruction $ \raw_prototypeInstruction ->
        sendClassMsg cls' (mkSelector "videoCompositionWithPropertiesOfAsset:prototypeInstruction:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_prototypeInstruction :: Ptr ())] >>= retainedObject . castPtr

-- | Vends a new instance of AVMutableVideoComposition with values and instructions suitable for presenting the video tracks of the specified asset according to its temporal and geometric properties and those of its tracks, and also overrides default properties with those from a prototypeInstruction.
--
-- Also see videoCompositionWithPropertiesOfAsset:completionHandler:. The new AVMutableVideoComposition will have instructions that respect the spatial properties and timeRanges of the specified asset's video tracks. Anything not pertaining to spatial layout and timing, such as background color for their composition or post-processing behaviors, is eligible to be specified via a prototype instruction. Example: To add a background color, ```objc myPrototypeInstruction = [[AVMutableVideoCompositionInstruction alloc] init]; myPrototypeInstruction.backgroundColor = myCGColorRef; // Do not use constant CGColorRef colors here. myVideoComposition = [AVVideoComposition videoCompositionWithPropertiesOfAsset:myAsset prototypeInstruction:myPrototypeInstruction completionHandler:^(AVMutableVideoComposition * _Nullable myVideoComposition, NSError * _Nullable error) { 	if (myVideoComposition != nil) { 		// use myVideoComposition 	} 	else { 		// handle error 	} }]; ``` - Parameter asset: An instance of AVAsset. - Parameter prototypeInstruction: Custom instructions that the client can choose to override. - Parameter completionHandler: A block that is invoked when the new video composition has finished being created. If the @videoComposition@ parameter is nil, the @error@ parameter describes the failure that occurred.
--
-- ObjC selector: @+ videoCompositionWithPropertiesOfAsset:prototypeInstruction:completionHandler:@
videoCompositionWithPropertiesOfAsset_prototypeInstruction_completionHandler :: (IsAVAsset asset, IsAVVideoCompositionInstruction prototypeInstruction) => asset -> prototypeInstruction -> Ptr () -> IO ()
videoCompositionWithPropertiesOfAsset_prototypeInstruction_completionHandler asset prototypeInstruction completionHandler =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr prototypeInstruction $ \raw_prototypeInstruction ->
        sendClassMsg cls' (mkSelector "videoCompositionWithPropertiesOfAsset:prototypeInstruction:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_prototypeInstruction :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Returns a new instance of AVMutableVideoComposition with values and instructions that will apply the specified handler block to video frames represented as instances of CIImage.
--
-- The returned AVMutableVideoComposition will cause the specified handler block to be called to filter each frame of the asset's first enabled video track. The handler block should use the properties of the provided AVAsynchronousCIImageFilteringRequest and respond using finishWithImage:context: with a "filtered" new CIImage (or the provided source image for no affect). In the event of an error, respond to the request using finishWithError:. The error can be observed via AVPlayerItemFailedToPlayToEndTimeNotification, see AVPlayerItemFailedToPlayToEndTimeErrorKey in notification payload. The client can set sourceTrackIDForFrameTiming to kCMPersistentTrackID_Invalid and frameDuration to an appropriate value in order to specify the maximum output frame rate independent of the source track timing.
--
-- The video composition will also have the following values for its properties:
--
-- - The original timing of the asset's first enabled video track will be used. - A renderSize that encompasses the asset's first enabled video track respecting the track's preferredTransform. - A renderScale of 1.0.
--
-- The default CIContext has the following properties:
--
-- - iOS: Device RGB color space - macOS: sRGB color space
--
-- Example usage: ```objc playerItem.videoComposition = [AVMutableVideoComposition videoCompositionWithAsset:srcAsset applyingCIFiltersWithHandler: 	^(AVAsynchronousCIImageFilteringRequest *request) 	{ 		NSError *err = nil; 		CIImage *filtered = myRenderer(request, &err); 		if (filtered) 			[request finishWithImage:filtered context:nil]; 		else 			[request finishWithError:err]; 	}]; ``` - Parameter asset: An instance of AVAsset. For best performance, ensure that the duration and tracks properties of the asset are already loaded before invoking this method.
--
-- - Returns: An instance of AVMutableVideoComposition.
--
-- ObjC selector: @+ videoCompositionWithAsset:applyingCIFiltersWithHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandler :: IsAVAsset asset => asset -> Ptr () -> IO (Id AVMutableVideoComposition)
videoCompositionWithAsset_applyingCIFiltersWithHandler asset applier =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr applier :: Ptr ())] >>= retainedObject . castPtr

-- | Vends a new instance of AVMutableVideoComposition with values and instructions that will apply the specified handler block to video frames represented as instances of CIImage.
--
-- The new AVMutableVideoComposition will cause the specified handler block to be called to filter each frame of the asset's first enabled video track. The handler block should use the properties of the provided AVAsynchronousCIImageFilteringRequest and respond using finishWithImage:context: with a "filtered" new CIImage (or the provided source image for no affect). In the event of an error, respond to the request using finishWithError:. The error can be observed via AVPlayerItemFailedToPlayToEndTimeNotification, see AVPlayerItemFailedToPlayToEndTimeErrorKey in notification payload. The client can set sourceTrackIDForFrameTiming to kCMPersistentTrackID_Invalid and frameDuration to an appropriate value in order to specify the maximum output frame rate independent of the source track timing.
--
-- The video composition will also have the following values for its properties:
--
-- - The original timing of the asset's first enabled video track will be used. - A renderSize that encompasses the asset's first enabled video track respecting the track's preferredTransform. - A renderScale of 1.0.
--
-- The default CIContext has the following properties:
--
-- - iOS: Device RGB color space - macOS: sRGB color space
--
-- Example usage: ```objc [AVMutableVideoComposition videoCompositionWithAsset:srcAsset applyingCIFiltersWithHandler: ^(AVAsynchronousCIImageFilteringRequest *request) { 	NSError *err = nil; 	CIImage *filtered = myRenderer(request, &err); 	if (filtered) 		[request finishWithImage:filtered context:nil]; 	else 		[request finishWithError:err]; 	} completionHandler: 	^(AVMutableVideoComposition * _Nullable videoComposition, NSError * _Nullable error) 	{ 		if (videoComposition != nil) { 			playerItem.videoComposition = videoComposition 		else { 		// handle error 	}]; ``` - Parameter asset: An instance of AVAsset. - Parameter completionHandler: A block that is invoked when the new video composition has finished being created. If the @videoComposition@ parameter is nil, the @error@ parameter describes the failure that occurred.
--
-- ObjC selector: @+ videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandler :: IsAVAsset asset => asset -> Ptr () -> Ptr () -> IO ()
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandler asset applier completionHandler =
  do
    cls' <- getRequiredClass "AVMutableVideoComposition"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr applier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | If sourceTrackIDForFrameTiming is not kCMPersistentTrackID_Invalid, frame timing for the video composition is derived from the source asset's track with the corresponding ID. This may be used to preserve a source asset's variable frame timing. If an empty edit is encountered in the source asset’s track, the compositor composes frames as needed up to the frequency specified in frameDuration property.
--
-- ObjC selector: @- sourceTrackIDForFrameTiming@
sourceTrackIDForFrameTiming :: IsAVMutableVideoComposition avMutableVideoComposition => avMutableVideoComposition -> IO CInt
sourceTrackIDForFrameTiming avMutableVideoComposition  =
  sendMsg avMutableVideoComposition (mkSelector "sourceTrackIDForFrameTiming") retCInt []

-- | If sourceTrackIDForFrameTiming is not kCMPersistentTrackID_Invalid, frame timing for the video composition is derived from the source asset's track with the corresponding ID. This may be used to preserve a source asset's variable frame timing. If an empty edit is encountered in the source asset’s track, the compositor composes frames as needed up to the frequency specified in frameDuration property.
--
-- ObjC selector: @- setSourceTrackIDForFrameTiming:@
setSourceTrackIDForFrameTiming :: IsAVMutableVideoComposition avMutableVideoComposition => avMutableVideoComposition -> CInt -> IO ()
setSourceTrackIDForFrameTiming avMutableVideoComposition  value =
  sendMsg avMutableVideoComposition (mkSelector "setSourceTrackIDForFrameTiming:") retVoid [argCInt (fromIntegral value)]

-- | Indicates the scale at which the video composition should render. May only be other than 1.0 for a video composition set on an AVPlayerItem
--
-- ObjC selector: @- renderScale@
renderScale :: IsAVMutableVideoComposition avMutableVideoComposition => avMutableVideoComposition -> IO CFloat
renderScale avMutableVideoComposition  =
  sendMsg avMutableVideoComposition (mkSelector "renderScale") retCFloat []

-- | Indicates the scale at which the video composition should render. May only be other than 1.0 for a video composition set on an AVPlayerItem
--
-- ObjC selector: @- setRenderScale:@
setRenderScale :: IsAVMutableVideoComposition avMutableVideoComposition => avMutableVideoComposition -> CFloat -> IO ()
setRenderScale avMutableVideoComposition  value =
  sendMsg avMutableVideoComposition (mkSelector "setRenderScale:") retVoid [argCFloat (fromIntegral value)]

-- | Indicates a special video composition tool for use of Core Animation; may be nil
--
-- ObjC selector: @- animationTool@
animationTool :: IsAVMutableVideoComposition avMutableVideoComposition => avMutableVideoComposition -> IO (Id AVVideoCompositionCoreAnimationTool)
animationTool avMutableVideoComposition  =
  sendMsg avMutableVideoComposition (mkSelector "animationTool") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates a special video composition tool for use of Core Animation; may be nil
--
-- ObjC selector: @- setAnimationTool:@
setAnimationTool :: (IsAVMutableVideoComposition avMutableVideoComposition, IsAVVideoCompositionCoreAnimationTool value) => avMutableVideoComposition -> value -> IO ()
setAnimationTool avMutableVideoComposition  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableVideoComposition (mkSelector "setAnimationTool:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configures policy for per frame HDR display metadata on the rendered frame
--
-- Allows the system to identify situations where HDR metadata can be generated and attached to the rendered video frame. Default is AVVideoCompositionPerFrameHDRDisplayMetadataPolicyPropagate. Any HDR metadata attached to the composed frame will be propagated to the rendered video frames.
--
-- ObjC selector: @- perFrameHDRDisplayMetadataPolicy@
perFrameHDRDisplayMetadataPolicy :: IsAVMutableVideoComposition avMutableVideoComposition => avMutableVideoComposition -> IO (Id NSString)
perFrameHDRDisplayMetadataPolicy avMutableVideoComposition  =
  sendMsg avMutableVideoComposition (mkSelector "perFrameHDRDisplayMetadataPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures policy for per frame HDR display metadata on the rendered frame
--
-- Allows the system to identify situations where HDR metadata can be generated and attached to the rendered video frame. Default is AVVideoCompositionPerFrameHDRDisplayMetadataPolicyPropagate. Any HDR metadata attached to the composed frame will be propagated to the rendered video frames.
--
-- ObjC selector: @- setPerFrameHDRDisplayMetadataPolicy:@
setPerFrameHDRDisplayMetadataPolicy :: (IsAVMutableVideoComposition avMutableVideoComposition, IsNSString value) => avMutableVideoComposition -> value -> IO ()
setPerFrameHDRDisplayMetadataPolicy avMutableVideoComposition  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableVideoComposition (mkSelector "setPerFrameHDRDisplayMetadataPolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector
videoCompositionSelector = mkSelector "videoComposition"

-- | @Selector@ for @videoCompositionWithPropertiesOfAsset:@
videoCompositionWithPropertiesOfAssetSelector :: Selector
videoCompositionWithPropertiesOfAssetSelector = mkSelector "videoCompositionWithPropertiesOfAsset:"

-- | @Selector@ for @videoCompositionWithPropertiesOfAsset:completionHandler:@
videoCompositionWithPropertiesOfAsset_completionHandlerSelector :: Selector
videoCompositionWithPropertiesOfAsset_completionHandlerSelector = mkSelector "videoCompositionWithPropertiesOfAsset:completionHandler:"

-- | @Selector@ for @videoCompositionWithPropertiesOfAsset:prototypeInstruction:@
videoCompositionWithPropertiesOfAsset_prototypeInstructionSelector :: Selector
videoCompositionWithPropertiesOfAsset_prototypeInstructionSelector = mkSelector "videoCompositionWithPropertiesOfAsset:prototypeInstruction:"

-- | @Selector@ for @videoCompositionWithPropertiesOfAsset:prototypeInstruction:completionHandler:@
videoCompositionWithPropertiesOfAsset_prototypeInstruction_completionHandlerSelector :: Selector
videoCompositionWithPropertiesOfAsset_prototypeInstruction_completionHandlerSelector = mkSelector "videoCompositionWithPropertiesOfAsset:prototypeInstruction:completionHandler:"

-- | @Selector@ for @videoCompositionWithAsset:applyingCIFiltersWithHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandlerSelector :: Selector
videoCompositionWithAsset_applyingCIFiltersWithHandlerSelector = mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:"

-- | @Selector@ for @videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:@
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandlerSelector :: Selector
videoCompositionWithAsset_applyingCIFiltersWithHandler_completionHandlerSelector = mkSelector "videoCompositionWithAsset:applyingCIFiltersWithHandler:completionHandler:"

-- | @Selector@ for @sourceTrackIDForFrameTiming@
sourceTrackIDForFrameTimingSelector :: Selector
sourceTrackIDForFrameTimingSelector = mkSelector "sourceTrackIDForFrameTiming"

-- | @Selector@ for @setSourceTrackIDForFrameTiming:@
setSourceTrackIDForFrameTimingSelector :: Selector
setSourceTrackIDForFrameTimingSelector = mkSelector "setSourceTrackIDForFrameTiming:"

-- | @Selector@ for @renderScale@
renderScaleSelector :: Selector
renderScaleSelector = mkSelector "renderScale"

-- | @Selector@ for @setRenderScale:@
setRenderScaleSelector :: Selector
setRenderScaleSelector = mkSelector "setRenderScale:"

-- | @Selector@ for @animationTool@
animationToolSelector :: Selector
animationToolSelector = mkSelector "animationTool"

-- | @Selector@ for @setAnimationTool:@
setAnimationToolSelector :: Selector
setAnimationToolSelector = mkSelector "setAnimationTool:"

-- | @Selector@ for @perFrameHDRDisplayMetadataPolicy@
perFrameHDRDisplayMetadataPolicySelector :: Selector
perFrameHDRDisplayMetadataPolicySelector = mkSelector "perFrameHDRDisplayMetadataPolicy"

-- | @Selector@ for @setPerFrameHDRDisplayMetadataPolicy:@
setPerFrameHDRDisplayMetadataPolicySelector :: Selector
setPerFrameHDRDisplayMetadataPolicySelector = mkSelector "setPerFrameHDRDisplayMetadataPolicy:"

