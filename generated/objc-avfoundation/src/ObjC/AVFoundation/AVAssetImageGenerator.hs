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
  , appliesPreferredTrackTransform
  , setAppliesPreferredTrackTransform
  , apertureMode
  , setApertureMode
  , dynamicRangePolicy
  , setDynamicRangePolicy
  , videoComposition
  , setVideoComposition
  , initSelector
  , newSelector
  , assetImageGeneratorWithAssetSelector
  , initWithAssetSelector
  , generateCGImagesAsynchronouslyForTimes_completionHandlerSelector
  , cancelAllCGImageGenerationSelector
  , appliesPreferredTrackTransformSelector
  , setAppliesPreferredTrackTransformSelector
  , apertureModeSelector
  , setApertureModeSelector
  , dynamicRangePolicySelector
  , setDynamicRangePolicySelector
  , videoCompositionSelector
  , setVideoCompositionSelector


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

-- | @- init@
init_ :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id AVAssetImageGenerator)
init_ avAssetImageGenerator  =
  sendMsg avAssetImageGenerator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetImageGenerator)
new  =
  do
    cls' <- getRequiredClass "AVAssetImageGenerator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "assetImageGeneratorWithAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

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
initWithAsset avAssetImageGenerator  asset =
withObjCPtr asset $ \raw_asset ->
    sendMsg avAssetImageGenerator (mkSelector "initWithAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= ownedObject . castPtr

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
generateCGImagesAsynchronouslyForTimes_completionHandler avAssetImageGenerator  requestedTimes handler =
withObjCPtr requestedTimes $ \raw_requestedTimes ->
    sendMsg avAssetImageGenerator (mkSelector "generateCGImagesAsynchronouslyForTimes:completionHandler:") retVoid [argPtr (castPtr raw_requestedTimes :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | cancelAllCGImageGeneration
--
-- Cancels all outstanding image generation requests.
--
-- Calls the handler block with AVAssetImageGeneratorCancelled for each image time in every previous invocation of -generateCGImagesAsynchronouslyForTimes:completionHandler:					for which images have not yet been supplied.
--
-- ObjC selector: @- cancelAllCGImageGeneration@
cancelAllCGImageGeneration :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO ()
cancelAllCGImageGeneration avAssetImageGenerator  =
  sendMsg avAssetImageGenerator (mkSelector "cancelAllCGImageGeneration") retVoid []

-- | @- appliesPreferredTrackTransform@
appliesPreferredTrackTransform :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO Bool
appliesPreferredTrackTransform avAssetImageGenerator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetImageGenerator (mkSelector "appliesPreferredTrackTransform") retCULong []

-- | @- setAppliesPreferredTrackTransform:@
setAppliesPreferredTrackTransform :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> Bool -> IO ()
setAppliesPreferredTrackTransform avAssetImageGenerator  value =
  sendMsg avAssetImageGenerator (mkSelector "setAppliesPreferredTrackTransform:") retVoid [argCULong (if value then 1 else 0)]

-- | @- apertureMode@
apertureMode :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id NSString)
apertureMode avAssetImageGenerator  =
  sendMsg avAssetImageGenerator (mkSelector "apertureMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApertureMode:@
setApertureMode :: (IsAVAssetImageGenerator avAssetImageGenerator, IsNSString value) => avAssetImageGenerator -> value -> IO ()
setApertureMode avAssetImageGenerator  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetImageGenerator (mkSelector "setApertureMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | dynamicRangePolicy
--
-- Configures the video dynamic range for the output CGImage
--
-- Default is AVAssetImageGeneratorDynamicRangePolicyForceSDR
--
-- ObjC selector: @- dynamicRangePolicy@
dynamicRangePolicy :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id NSString)
dynamicRangePolicy avAssetImageGenerator  =
  sendMsg avAssetImageGenerator (mkSelector "dynamicRangePolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dynamicRangePolicy
--
-- Configures the video dynamic range for the output CGImage
--
-- Default is AVAssetImageGeneratorDynamicRangePolicyForceSDR
--
-- ObjC selector: @- setDynamicRangePolicy:@
setDynamicRangePolicy :: (IsAVAssetImageGenerator avAssetImageGenerator, IsNSString value) => avAssetImageGenerator -> value -> IO ()
setDynamicRangePolicy avAssetImageGenerator  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetImageGenerator (mkSelector "setDynamicRangePolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | videoComposition
--
-- Specifies the video composition to use when extracting images from assets with multiple video tracks.
--
-- If no videoComposition is specified, only the first enabled video track will be used.				If a videoComposition is specified, the value of appliesPreferredTrackTransform is ignored.				This property throws an exception if a video composition is set with any of the following property values:					- "renderScale" is not equal to one					- "renderSize" width or height is less than zero					- "frameDuration" is invalid or less than or equal to zero					- "sourceTrackIDForFrameTiming" is less than zero					- "outputBufferDescription" is non-nil
--
-- ObjC selector: @- videoComposition@
videoComposition :: IsAVAssetImageGenerator avAssetImageGenerator => avAssetImageGenerator -> IO (Id AVVideoComposition)
videoComposition avAssetImageGenerator  =
  sendMsg avAssetImageGenerator (mkSelector "videoComposition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoComposition
--
-- Specifies the video composition to use when extracting images from assets with multiple video tracks.
--
-- If no videoComposition is specified, only the first enabled video track will be used.				If a videoComposition is specified, the value of appliesPreferredTrackTransform is ignored.				This property throws an exception if a video composition is set with any of the following property values:					- "renderScale" is not equal to one					- "renderSize" width or height is less than zero					- "frameDuration" is invalid or less than or equal to zero					- "sourceTrackIDForFrameTiming" is less than zero					- "outputBufferDescription" is non-nil
--
-- ObjC selector: @- setVideoComposition:@
setVideoComposition :: (IsAVAssetImageGenerator avAssetImageGenerator, IsAVVideoComposition value) => avAssetImageGenerator -> value -> IO ()
setVideoComposition avAssetImageGenerator  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetImageGenerator (mkSelector "setVideoComposition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetImageGeneratorWithAsset:@
assetImageGeneratorWithAssetSelector :: Selector
assetImageGeneratorWithAssetSelector = mkSelector "assetImageGeneratorWithAsset:"

-- | @Selector@ for @initWithAsset:@
initWithAssetSelector :: Selector
initWithAssetSelector = mkSelector "initWithAsset:"

-- | @Selector@ for @generateCGImagesAsynchronouslyForTimes:completionHandler:@
generateCGImagesAsynchronouslyForTimes_completionHandlerSelector :: Selector
generateCGImagesAsynchronouslyForTimes_completionHandlerSelector = mkSelector "generateCGImagesAsynchronouslyForTimes:completionHandler:"

-- | @Selector@ for @cancelAllCGImageGeneration@
cancelAllCGImageGenerationSelector :: Selector
cancelAllCGImageGenerationSelector = mkSelector "cancelAllCGImageGeneration"

-- | @Selector@ for @appliesPreferredTrackTransform@
appliesPreferredTrackTransformSelector :: Selector
appliesPreferredTrackTransformSelector = mkSelector "appliesPreferredTrackTransform"

-- | @Selector@ for @setAppliesPreferredTrackTransform:@
setAppliesPreferredTrackTransformSelector :: Selector
setAppliesPreferredTrackTransformSelector = mkSelector "setAppliesPreferredTrackTransform:"

-- | @Selector@ for @apertureMode@
apertureModeSelector :: Selector
apertureModeSelector = mkSelector "apertureMode"

-- | @Selector@ for @setApertureMode:@
setApertureModeSelector :: Selector
setApertureModeSelector = mkSelector "setApertureMode:"

-- | @Selector@ for @dynamicRangePolicy@
dynamicRangePolicySelector :: Selector
dynamicRangePolicySelector = mkSelector "dynamicRangePolicy"

-- | @Selector@ for @setDynamicRangePolicy:@
setDynamicRangePolicySelector :: Selector
setDynamicRangePolicySelector = mkSelector "setDynamicRangePolicy:"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector
videoCompositionSelector = mkSelector "videoComposition"

-- | @Selector@ for @setVideoComposition:@
setVideoCompositionSelector :: Selector
setVideoCompositionSelector = mkSelector "setVideoComposition:"

