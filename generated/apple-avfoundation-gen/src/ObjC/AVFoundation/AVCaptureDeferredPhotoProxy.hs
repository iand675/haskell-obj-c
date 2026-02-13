{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeferredPhotoProxy
--
-- A lightly-processed photo whose data may be used to process and fetch a higher-resolution asset at a later time.
--
-- An AVCaptureDeferredPhotoProxy behaves like a normal AVCapturePhoto, and approximates the look of the final rendered image.  This object represents intermediate data that can be rendered into a final image and ingested into the user's photo library via PHAsset APIs.  The intermediate data are not accessible by the calling process.
--
-- Use a PHAssetCreationRequest with a resourceType of PHAssetResourceTypePhotoProxy using the fileDataRepresentation of this object.  Image processing to finalize the asset will occur either on-demand when accessing the image data via PHImageManager or PHAssetResource, or will execute in the background when the system has determined that it's a good time to process based on thermals, battery level, and other conditions.  If the data provided to the PHAssetCreationRequest does not come from an AVCaptureDeferredPhotoProxy, then PHAssetCreationRequest will fail and a PHPhotosErrorInvalidResource error will be returned.
--
-- Below is a discussion of how the superclass properties behave on an AVCaptureDeferredPhotoProxy.
--
-- (readonly) CMTime timestamp;
--
-- The time of the capture; proxy and final photos will have the same timestamp.
--
-- (readonly) NSDictionary<NSString *, id> *metadata;
--
-- The metadata of the proxy image may differ slightly from the final photo's metadata where some fields may be updated.
--
-- (readonly, getter=isRawPhoto) BOOL rawPhoto;
--
-- Always NO, as deferred processing isn't available for raw photos.
--
-- (nullable, readonly) NSDictionary<NSString *, id> *embeddedThumbnailPhotoFormat;
--
-- Describes the embedded thumbnail format of both the proxy and the final photo which have the same dimensions and codec.
--
-- (readonly) AVCaptureResolvedPhotoSettings *resolvedSettings;
--
-- Describes the resolved settings of the whole capture, including the proxy and final photo. See AVCaptureResolvedPhotoSettings.deferredPhotoProxyDimensions.
--
-- (readonly) NSInteger photoCount;
--
-- Same for both proxy and final.
--
-- (nullable, readonly) AVCaptureDeviceType sourceDeviceType;
--
-- Same for both proxy and final.
--
-- (nullable, readonly) AVCaptureBracketedStillImageSettings *bracketSettings;
--
-- Same for both proxy and final.
--
-- (readonly) NSInteger sequenceCount;
--
-- Same for both proxy and final.
--
-- (readonly) AVCaptureLensStabilizationStatus lensStabilizationStatus;
--
-- Same for both proxy and final.
--
-- Superclass properties/methods that behave differently than a typical AVCapturePhoto:
--
-- (nullable, readonly) CVPixelBufferRef pixelBuffer NS_RETURNS_INNER_POINTER;
--
-- (nullable, readonly) CVPixelBufferRef previewPixelBuffer NS_RETURNS_INNER_POINTER;
--
-- - (nullable CGImageRef)CGImageRepresentation;         - (nullable CGImageRef)previewCGImageRepresentation;            All of the above properties return the same proxy image, either as a pixel buffer or CGImageRef.
--
-- - (nullable NSData *)fileDataRepresentation;         - (nullable NSData *)fileDataRepresentationWithCustomizer:(id<AVCapturePhotoFileDataRepresentationCustomizer>)customizer;             You may call either of the above two methods to create a NSData representation of the image, but note that it is only the proxy image quality being packaged.
--
-- Generated bindings for @AVCaptureDeferredPhotoProxy@.
module ObjC.AVFoundation.AVCaptureDeferredPhotoProxy
  ( AVCaptureDeferredPhotoProxy
  , IsAVCaptureDeferredPhotoProxy(..)
  , init_
  , new
  , initSelector
  , newSelector


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
init_ :: IsAVCaptureDeferredPhotoProxy avCaptureDeferredPhotoProxy => avCaptureDeferredPhotoProxy -> IO (Id AVCaptureDeferredPhotoProxy)
init_ avCaptureDeferredPhotoProxy =
  sendOwnedMessage avCaptureDeferredPhotoProxy initSelector

-- | @+ new@
new :: IO (Id AVCaptureDeferredPhotoProxy)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDeferredPhotoProxy"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureDeferredPhotoProxy)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureDeferredPhotoProxy)
newSelector = mkSelector "new"

