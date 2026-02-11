{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureOutput
--
-- AVCaptureOutput is an abstract class that defines an interface for an output destination of an AVCaptureSession.
--
-- AVCaptureOutput provides an abstract interface for connecting capture output destinations, such as files and video previews, to an AVCaptureSession.
--
-- An AVCaptureOutput can have multiple connections represented by AVCaptureConnection objects, one for each stream of media that it receives from an AVCaptureInput. An AVCaptureOutput does not have any connections when it is first created. When an output is added to an AVCaptureSession, connections are created that map media data from that session's inputs to its outputs.
--
-- Concrete AVCaptureOutput instances can be added to an AVCaptureSession using the -[AVCaptureSession addOutput:] and -[AVCaptureSession addOutputWithNoConnections:] methods.
--
-- Generated bindings for @AVCaptureOutput@.
module ObjC.AVFoundation.AVCaptureOutput
  ( AVCaptureOutput
  , IsAVCaptureOutput(..)
  , init_
  , new
  , connectionWithMediaType
  , transformedMetadataObjectForMetadataObject_connection
  , deferredStartSupported
  , deferredStartEnabled
  , setDeferredStartEnabled
  , initSelector
  , newSelector
  , connectionWithMediaTypeSelector
  , transformedMetadataObjectForMetadataObject_connectionSelector
  , deferredStartSupportedSelector
  , deferredStartEnabledSelector
  , setDeferredStartEnabledSelector


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
init_ :: IsAVCaptureOutput avCaptureOutput => avCaptureOutput -> IO (Id AVCaptureOutput)
init_ avCaptureOutput  =
  sendMsg avCaptureOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | connectionWithMediaType:
--
-- Returns the first connection in the connections array with an inputPort of the specified mediaType.
--
-- @mediaType@ — An AVMediaType constant from AVMediaFormat.h, e.g. AVMediaTypeVideo.
--
-- This convenience method returns the first AVCaptureConnection in the receiver's connections array that has an AVCaptureInputPort of the specified mediaType. If no connection with the specified mediaType is found, nil is returned.
--
-- ObjC selector: @- connectionWithMediaType:@
connectionWithMediaType :: (IsAVCaptureOutput avCaptureOutput, IsNSString mediaType) => avCaptureOutput -> mediaType -> IO (Id AVCaptureConnection)
connectionWithMediaType avCaptureOutput  mediaType =
withObjCPtr mediaType $ \raw_mediaType ->
    sendMsg avCaptureOutput (mkSelector "connectionWithMediaType:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ())] >>= retainedObject . castPtr

-- | transformedMetadataObjectForMetadataObject:connection:
--
-- Converts an AVMetadataObject's visual properties to the receiver's coordinates.
--
-- @metadataObject@ — An AVMetadataObject originating from the same AVCaptureInput as the receiver.
--
-- @connection@ — The receiver's connection whose AVCaptureInput matches that of the metadata object to be converted.
--
-- Returns: An AVMetadataObject whose properties are in output coordinates.
--
-- AVMetadataObject bounds may be expressed as a rect where {0,0} represents the top left of the picture area, and {1,1} represents the bottom right on an unrotated picture. Face metadata objects likewise express yaw and roll angles with respect to an unrotated picture. -transformedMetadataObjectForMetadataObject:connection: converts the visual properties in the coordinate space of the supplied AVMetadataObject to the coordinate space of the receiver. The conversion takes orientation, mirroring, and scaling into consideration. If the provided metadata object originates from an input source other than the preview layer's, nil will be returned.
--
-- If an AVCaptureVideoDataOutput instance's connection's videoOrientation or videoMirrored properties are set to non-default values, the output applies the desired mirroring and orientation by physically rotating and or flipping sample buffers as they pass through it. AVCaptureStillImageOutput, on the other hand, does not physically rotate its buffers. It attaches an appropriate kCGImagePropertyOrientation number to captured still image buffers (see ImageIO/CGImageProperties.h) indicating how the image should be displayed on playback. Likewise, AVCaptureMovieFileOutput does not physically apply orientation/mirroring to its sample buffers -- it uses a QuickTime track matrix to indicate how the buffers should be rotated and/or flipped on playback.
--
-- transformedMetadataObjectForMetadataObject:connection: alters the visual properties of the provided metadata object to match the physical rotation / mirroring of the sample buffers provided by the receiver through the indicated connection. I.e., for video data output, adjusted metadata object coordinates are rotated/mirrored. For still image and movie file output, they are not.
--
-- ObjC selector: @- transformedMetadataObjectForMetadataObject:connection:@
transformedMetadataObjectForMetadataObject_connection :: (IsAVCaptureOutput avCaptureOutput, IsAVMetadataObject metadataObject, IsAVCaptureConnection connection) => avCaptureOutput -> metadataObject -> connection -> IO (Id AVMetadataObject)
transformedMetadataObjectForMetadataObject_connection avCaptureOutput  metadataObject connection =
withObjCPtr metadataObject $ \raw_metadataObject ->
  withObjCPtr connection $ \raw_connection ->
      sendMsg avCaptureOutput (mkSelector "transformedMetadataObjectForMetadataObject:connection:") (retPtr retVoid) [argPtr (castPtr raw_metadataObject :: Ptr ()), argPtr (castPtr raw_connection :: Ptr ())] >>= retainedObject . castPtr

-- | A @BOOL@ value that indicates whether the output supports deferred start.
--
-- You can only set the ``deferredStartEnabled`` property value to @true@ if the output supports deferred start.
--
-- ObjC selector: @- deferredStartSupported@
deferredStartSupported :: IsAVCaptureOutput avCaptureOutput => avCaptureOutput -> IO Bool
deferredStartSupported avCaptureOutput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureOutput (mkSelector "deferredStartSupported") retCULong []

-- | A @BOOL@ value that indicates whether to defer starting this capture output.
--
-- When this value is @true@, the session does not prepare the output's resources until some time after ``AVCaptureSession/startRunning`` returns. You can start the visual parts of your user interface (e.g. preview) prior to other parts (e.g. photo/movie capture, metadata output, etc..) to improve startup performance. Set this value to @false@ for outputs that your app needs for startup, and @true@ for the ones it does not need to start immediately. For example, an ``AVCaptureVideoDataOutput`` that you intend to use for displaying preview should set this value to @false@, so that the frames are available as soon as possible.
--
-- By default, for apps that are linked on or after iOS 26, this property value is @true@ for ``AVCapturePhotoOutput`` and ``AVCaptureFileOutput`` subclasses if supported, and @false@ otherwise. When set to @true@ for ``AVCapturePhotoOutput``, if you want to support multiple capture requests before running deferred start, set ``AVCapturePhotoOutput/responsiveCaptureEnabled`` to @true@ on that output.
--
-- If ``deferredStartSupported`` is @false@, setting this property value to @true@ results in the system throwing an @NSInvalidArgumentException@.
--
-- - Note: Set this value before calling ``AVCaptureSession/commitConfiguration`` as it requires a lengthy reconfiguration of the capture render pipeline.
--
-- ObjC selector: @- deferredStartEnabled@
deferredStartEnabled :: IsAVCaptureOutput avCaptureOutput => avCaptureOutput -> IO Bool
deferredStartEnabled avCaptureOutput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureOutput (mkSelector "deferredStartEnabled") retCULong []

-- | A @BOOL@ value that indicates whether to defer starting this capture output.
--
-- When this value is @true@, the session does not prepare the output's resources until some time after ``AVCaptureSession/startRunning`` returns. You can start the visual parts of your user interface (e.g. preview) prior to other parts (e.g. photo/movie capture, metadata output, etc..) to improve startup performance. Set this value to @false@ for outputs that your app needs for startup, and @true@ for the ones it does not need to start immediately. For example, an ``AVCaptureVideoDataOutput`` that you intend to use for displaying preview should set this value to @false@, so that the frames are available as soon as possible.
--
-- By default, for apps that are linked on or after iOS 26, this property value is @true@ for ``AVCapturePhotoOutput`` and ``AVCaptureFileOutput`` subclasses if supported, and @false@ otherwise. When set to @true@ for ``AVCapturePhotoOutput``, if you want to support multiple capture requests before running deferred start, set ``AVCapturePhotoOutput/responsiveCaptureEnabled`` to @true@ on that output.
--
-- If ``deferredStartSupported`` is @false@, setting this property value to @true@ results in the system throwing an @NSInvalidArgumentException@.
--
-- - Note: Set this value before calling ``AVCaptureSession/commitConfiguration`` as it requires a lengthy reconfiguration of the capture render pipeline.
--
-- ObjC selector: @- setDeferredStartEnabled:@
setDeferredStartEnabled :: IsAVCaptureOutput avCaptureOutput => avCaptureOutput -> Bool -> IO ()
setDeferredStartEnabled avCaptureOutput  value =
  sendMsg avCaptureOutput (mkSelector "setDeferredStartEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @connectionWithMediaType:@
connectionWithMediaTypeSelector :: Selector
connectionWithMediaTypeSelector = mkSelector "connectionWithMediaType:"

-- | @Selector@ for @transformedMetadataObjectForMetadataObject:connection:@
transformedMetadataObjectForMetadataObject_connectionSelector :: Selector
transformedMetadataObjectForMetadataObject_connectionSelector = mkSelector "transformedMetadataObjectForMetadataObject:connection:"

-- | @Selector@ for @deferredStartSupported@
deferredStartSupportedSelector :: Selector
deferredStartSupportedSelector = mkSelector "deferredStartSupported"

-- | @Selector@ for @deferredStartEnabled@
deferredStartEnabledSelector :: Selector
deferredStartEnabledSelector = mkSelector "deferredStartEnabled"

-- | @Selector@ for @setDeferredStartEnabled:@
setDeferredStartEnabledSelector :: Selector
setDeferredStartEnabledSelector = mkSelector "setDeferredStartEnabled:"

