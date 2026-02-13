{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetImageGenerator@.
module ObjC.AVFoundation.AVAssetImageGenerator
  ( AVAssetImageGenerator
  , IsAVAssetImageGenerator(..)
  , init_
  , new
  , assetImageGeneratorWithAsset
  , initWithAsset
  , generateCGImagesAsynchronouslyForTimes_completionHandler
  , cancelAllCGImageGeneration
  , asset
  , appliesPreferredTrackTransform
  , setAppliesPreferredTrackTransform
  , apertureMode
  , setApertureMode
  , dynamicRangePolicy
  , setDynamicRangePolicy
  , videoComposition
  , setVideoComposition
  , customVideoCompositor
  , apertureModeSelector
  , appliesPreferredTrackTransformSelector
  , assetImageGeneratorWithAssetSelector
  , assetSelector
  , cancelAllCGImageGenerationSelector
  , customVideoCompositorSelector
  , dynamicRangePolicySelector
  , generateCGImagesAsynchronouslyForTimes_completionHandlerSelector
  , initSelector
  , initWithAssetSelector
  , newSelector
  , setApertureModeSelector
  , setAppliesPreferredTrackTransformSelector
  , setDynamicRangePolicySelector
  , setVideoCompositionSelector
  , videoCompositionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id AVAssetImageGenerator)
init_ avAssetImageGenerator =
  sendOwnedMessage avAssetImageGenerator initSelector

-- | @+ new@
new :: IO (Id AVAssetImageGenerator)
new  =
  do
    cls' <- getRequiredClass "AVAssetImageGenerator"
    sendOwnedClassMessage cls' newSelector

-- | assetImageGeneratorWithAsset:
--
-- Returns an instance of AVAssetImageGenerator for use with the specified asset.
--
-- @asset@ — The asset from which images will be extracted.
--
-- Returns: An instance of AVAssetImageGenerator
--
-- This method may succeed even if the asset possesses no visual tracks at the time of initialization.					Clients may wish to test whether an asset has any tracks with the visual characteristic via					-[AVAsset tracksWithMediaCharacteristic:].
--
-- Note also that assets that belong to a mutable subclass of AVAsset, AVMutableComposition or AVMutableMovie,					may gain visual tracks after initialization of an associated AVAssetImageGenerator.
--
-- However, the results of image generation are undefined if mutations of the asset occur while images					are being generated.
--
-- AVAssetImageGenerator will use the default enabled video track(s) to generate images.
--
-- ObjC selector: @+ assetImageGeneratorWithAsset:@
assetImageGeneratorWithAsset :: IsAVAsset asset => asset -> IO (Id AVAssetImageGenerator)
assetImageGeneratorWithAsset asset =
  do
    cls' <- getRequiredClass "AVAssetImageGenerator"
    sendClassMessage cls' assetImageGeneratorWithAssetSelector (toAVAsset asset)

-- | initWithAsset:
--
-- Initializes an instance of AVAssetImageGenerator for use with the specified asset.
--
-- @asset@ — The asset from which images will be extracted.
--
-- Returns: An instance of AVAssetImageGenerator
--
-- This method may succeed even if the asset possesses no visual tracks at the time of initialization.					Clients may wish to test whether an asset has any tracks with the visual characteristic via					-[AVAsset tracksWithMediaCharacteristic:].
--
-- Note also that assets that belong to a mutable subclass of AVAsset, AVMutableComposition or AVMutableMovie,					may gain visual tracks after initialization of an associated AVAssetImageGenerator.
--
-- However, the results of image generation are undefined if mutations of the asset occur while images					are being generated.
--
-- AVAssetImageGenerator will use the default enabled video track(s) to generate images.
--
-- ObjC selector: @- initWithAsset:@
initWithAsset :: (IsAVAssetImageGenerator avAssetImageGenerator, IsAVAsset asset) => avAssetImageGenerator -> asset -> IO (Id AVAssetImageGenerator)
initWithAsset avAssetImageGenerator asset =
  sendOwnedMessage avAssetImageGenerator initWithAssetSelector (toAVAsset asset)

-- | generateCGImagesAsynchronouslyForTimes:completionHandler:
--
-- Returns a series of CGImageRefs for an asset at or near the specified times.
--
-- @requestedTimes@ — An NSArray of NSValues, each containing a CMTime, specifying the asset times at which an image is requested.
--
-- @handler@ — A block that will be called when an image request is complete.
--
-- Employs an efficient "batch mode" for getting images in time order.					The client will receive exactly one handler callback for each requested time in requestedTimes.					Changes to generator properties (snap behavior, maximum size, etc...) will not affect outstanding asynchronous image generation requests.					The generated image is not retained.  Clients should retain the image if they wish it to persist after the completion handler returns.
--
-- ObjC selector: @- generateCGImagesAsynchronouslyForTimes:completionHandler:@
generateCGImagesAsynchronouslyForTimes_completionHandler :: (IsAVAssetImageGenerator avAssetImageGenerator, IsNSArray requestedTimes) => avAssetImageGenerator -> requestedTimes -> Ptr () -> IO ()
generateCGImagesAsynchronouslyForTimes_completionHandler avAssetImageGenerator requestedTimes handler =
  sendMessage avAssetImageGenerator generateCGImagesAsynchronouslyForTimes_completionHandlerSelector (toNSArray requestedTimes) handler

-- | cancelAllCGImageGeneration
--
-- Cancels all outstanding image generation requests.
--
-- Calls the handler block with AVAssetImageGeneratorCancelled for each image time in every previous invocation of -generateCGImagesAsynchronouslyForTimes:completionHandler:					for which images have not yet been supplied.
--
-- ObjC selector: @- cancelAllCGImageGeneration@
cancelAllCGImageGeneration :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO ()
cancelAllCGImageGeneration avAssetImageGenerator =
  sendMessage avAssetImageGenerator cancelAllCGImageGenerationSelector

-- | @- asset@
asset :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id AVAsset)
asset avAssetImageGenerator =
  sendMessage avAssetImageGenerator assetSelector

-- | @- appliesPreferredTrackTransform@
appliesPreferredTrackTransform :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO Bool
appliesPreferredTrackTransform avAssetImageGenerator =
  sendMessage avAssetImageGenerator appliesPreferredTrackTransformSelector

-- | @- setAppliesPreferredTrackTransform:@
setAppliesPreferredTrackTransform :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> Bool -> IO ()
setAppliesPreferredTrackTransform avAssetImageGenerator value =
  sendMessage avAssetImageGenerator setAppliesPreferredTrackTransformSelector value

-- | @- apertureMode@
apertureMode :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id NSString)
apertureMode avAssetImageGenerator =
  sendMessage avAssetImageGenerator apertureModeSelector

-- | @- setApertureMode:@
setApertureMode :: (IsAVAssetImageGenerator avAssetImageGenerator, IsNSString value) => avAssetImageGenerator -> value -> IO ()
setApertureMode avAssetImageGenerator value =
  sendMessage avAssetImageGenerator setApertureModeSelector (toNSString value)

-- | dynamicRangePolicy
--
-- Configures the video dynamic range for the output CGImage
--
-- Default is AVAssetImageGeneratorDynamicRangePolicyForceSDR
--
-- ObjC selector: @- dynamicRangePolicy@
dynamicRangePolicy :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id NSString)
dynamicRangePolicy avAssetImageGenerator =
  sendMessage avAssetImageGenerator dynamicRangePolicySelector

-- | dynamicRangePolicy
--
-- Configures the video dynamic range for the output CGImage
--
-- Default is AVAssetImageGeneratorDynamicRangePolicyForceSDR
--
-- ObjC selector: @- setDynamicRangePolicy:@
setDynamicRangePolicy :: (IsAVAssetImageGenerator avAssetImageGenerator, IsNSString value) => avAssetImageGenerator -> value -> IO ()
setDynamicRangePolicy avAssetImageGenerator value =
  sendMessage avAssetImageGenerator setDynamicRangePolicySelector (toNSString value)

-- | videoComposition
--
-- Specifies the video composition to use when extracting images from assets with multiple video tracks.
--
-- If no videoComposition is specified, only the first enabled video track will be used.				If a videoComposition is specified, the value of appliesPreferredTrackTransform is ignored.				This property throws an exception if a video composition is set with any of the following property values:					- "renderScale" is not equal to one					- "renderSize" width or height is less than zero					- "frameDuration" is invalid or less than or equal to zero					- "sourceTrackIDForFrameTiming" is less than zero					- "outputBufferDescription" is non-nil
--
-- ObjC selector: @- videoComposition@
videoComposition :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id AVVideoComposition)
videoComposition avAssetImageGenerator =
  sendMessage avAssetImageGenerator videoCompositionSelector

-- | videoComposition
--
-- Specifies the video composition to use when extracting images from assets with multiple video tracks.
--
-- If no videoComposition is specified, only the first enabled video track will be used.				If a videoComposition is specified, the value of appliesPreferredTrackTransform is ignored.				This property throws an exception if a video composition is set with any of the following property values:					- "renderScale" is not equal to one					- "renderSize" width or height is less than zero					- "frameDuration" is invalid or less than or equal to zero					- "sourceTrackIDForFrameTiming" is less than zero					- "outputBufferDescription" is non-nil
--
-- ObjC selector: @- setVideoComposition:@
setVideoComposition :: (IsAVAssetImageGenerator avAssetImageGenerator, IsAVVideoComposition value) => avAssetImageGenerator -> value -> IO ()
setVideoComposition avAssetImageGenerator value =
  sendMessage avAssetImageGenerator setVideoCompositionSelector (toAVVideoComposition value)

-- | @- customVideoCompositor@
customVideoCompositor :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO RawId
customVideoCompositor avAssetImageGenerator =
  sendMessage avAssetImageGenerator customVideoCompositorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetImageGenerator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetImageGenerator)
newSelector = mkSelector "new"

-- | @Selector@ for @assetImageGeneratorWithAsset:@
assetImageGeneratorWithAssetSelector :: Selector '[Id AVAsset] (Id AVAssetImageGenerator)
assetImageGeneratorWithAssetSelector = mkSelector "assetImageGeneratorWithAsset:"

-- | @Selector@ for @initWithAsset:@
initWithAssetSelector :: Selector '[Id AVAsset] (Id AVAssetImageGenerator)
initWithAssetSelector = mkSelector "initWithAsset:"

-- | @Selector@ for @generateCGImagesAsynchronouslyForTimes:completionHandler:@
generateCGImagesAsynchronouslyForTimes_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
generateCGImagesAsynchronouslyForTimes_completionHandlerSelector = mkSelector "generateCGImagesAsynchronouslyForTimes:completionHandler:"

-- | @Selector@ for @cancelAllCGImageGeneration@
cancelAllCGImageGenerationSelector :: Selector '[] ()
cancelAllCGImageGenerationSelector = mkSelector "cancelAllCGImageGeneration"

-- | @Selector@ for @asset@
assetSelector :: Selector '[] (Id AVAsset)
assetSelector = mkSelector "asset"

-- | @Selector@ for @appliesPreferredTrackTransform@
appliesPreferredTrackTransformSelector :: Selector '[] Bool
appliesPreferredTrackTransformSelector = mkSelector "appliesPreferredTrackTransform"

-- | @Selector@ for @setAppliesPreferredTrackTransform:@
setAppliesPreferredTrackTransformSelector :: Selector '[Bool] ()
setAppliesPreferredTrackTransformSelector = mkSelector "setAppliesPreferredTrackTransform:"

-- | @Selector@ for @apertureMode@
apertureModeSelector :: Selector '[] (Id NSString)
apertureModeSelector = mkSelector "apertureMode"

-- | @Selector@ for @setApertureMode:@
setApertureModeSelector :: Selector '[Id NSString] ()
setApertureModeSelector = mkSelector "setApertureMode:"

-- | @Selector@ for @dynamicRangePolicy@
dynamicRangePolicySelector :: Selector '[] (Id NSString)
dynamicRangePolicySelector = mkSelector "dynamicRangePolicy"

-- | @Selector@ for @setDynamicRangePolicy:@
setDynamicRangePolicySelector :: Selector '[Id NSString] ()
setDynamicRangePolicySelector = mkSelector "setDynamicRangePolicy:"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector '[] (Id AVVideoComposition)
videoCompositionSelector = mkSelector "videoComposition"

-- | @Selector@ for @setVideoComposition:@
setVideoCompositionSelector :: Selector '[Id AVVideoComposition] ()
setVideoCompositionSelector = mkSelector "setVideoComposition:"

-- | @Selector@ for @customVideoCompositor@
customVideoCompositorSelector :: Selector '[] RawId
customVideoCompositorSelector = mkSelector "customVideoCompositor"

