{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureVideoPreviewLayer
--
-- A CoreAnimation layer subclass for previewing the visual output of an AVCaptureSession.
--
-- An AVCaptureVideoPreviewLayer instance is a subclass of CALayer and is therefore suitable for insertion in a layer hierarchy as part of a graphical interface. One creates an AVCaptureVideoPreviewLayer instance with the capture session to be previewed, using +layerWithSession: or -initWithSession:. Using the "videoGravity" property, one can influence how content is viewed relative to the layer bounds. On some hardware configurations, the orientation of the layer can be manipulated using \@"orientation" and \@"mirrored".
--
-- Generated bindings for @AVCaptureVideoPreviewLayer@.
module ObjC.AVFoundation.AVCaptureVideoPreviewLayer
  ( AVCaptureVideoPreviewLayer
  , IsAVCaptureVideoPreviewLayer(..)
  , layerWithSession
  , initWithSession
  , layerWithSessionWithNoConnection
  , initWithSessionWithNoConnection
  , setSessionWithNoConnection
  , transformedMetadataObjectForMetadataObject
  , session
  , setSession
  , connection
  , videoGravity
  , setVideoGravity
  , previewing
  , orientationSupported
  , orientation
  , setOrientation
  , mirroringSupported
  , automaticallyAdjustsMirroring
  , setAutomaticallyAdjustsMirroring
  , mirrored
  , setMirrored
  , deferredStartSupported
  , deferredStartEnabled
  , setDeferredStartEnabled
  , layerWithSessionSelector
  , initWithSessionSelector
  , layerWithSessionWithNoConnectionSelector
  , initWithSessionWithNoConnectionSelector
  , setSessionWithNoConnectionSelector
  , transformedMetadataObjectForMetadataObjectSelector
  , sessionSelector
  , setSessionSelector
  , connectionSelector
  , videoGravitySelector
  , setVideoGravitySelector
  , previewingSelector
  , orientationSupportedSelector
  , orientationSelector
  , setOrientationSelector
  , mirroringSupportedSelector
  , automaticallyAdjustsMirroringSelector
  , setAutomaticallyAdjustsMirroringSelector
  , mirroredSelector
  , setMirroredSelector
  , deferredStartSupportedSelector
  , deferredStartEnabledSelector
  , setDeferredStartEnabledSelector

  -- * Enum types
  , AVCaptureVideoOrientation(AVCaptureVideoOrientation)
  , pattern AVCaptureVideoOrientationPortrait
  , pattern AVCaptureVideoOrientationPortraitUpsideDown
  , pattern AVCaptureVideoOrientationLandscapeRight
  , pattern AVCaptureVideoOrientationLandscapeLeft

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | layerWithSession:
--
-- Creates an AVCaptureVideoPreviewLayer for previewing the visual output of the specified AVCaptureSession.
--
-- @session@ — The AVCaptureSession instance to be previewed.
--
-- Returns: A newly initialized AVCaptureVideoPreviewLayer instance.
--
-- ObjC selector: @+ layerWithSession:@
layerWithSession :: IsAVCaptureSession session => session -> IO (Id AVCaptureVideoPreviewLayer)
layerWithSession session =
  do
    cls' <- getRequiredClass "AVCaptureVideoPreviewLayer"
    withObjCPtr session $ \raw_session ->
      sendClassMsg cls' (mkSelector "layerWithSession:") (retPtr retVoid) [argPtr (castPtr raw_session :: Ptr ())] >>= retainedObject . castPtr

-- | initWithSession:
--
-- Creates an AVCaptureVideoPreviewLayer for previewing the visual output of the specified AVCaptureSession.
--
-- @session@ — The AVCaptureSession instance to be previewed.
--
-- Returns: A newly initialized AVCaptureVideoPreviewLayer instance.
--
-- ObjC selector: @- initWithSession:@
initWithSession :: (IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer, IsAVCaptureSession session) => avCaptureVideoPreviewLayer -> session -> IO (Id AVCaptureVideoPreviewLayer)
initWithSession avCaptureVideoPreviewLayer  session =
  withObjCPtr session $ \raw_session ->
      sendMsg avCaptureVideoPreviewLayer (mkSelector "initWithSession:") (retPtr retVoid) [argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | layerWithSessionWithNoConnection:
--
-- Creates an AVCaptureVideoPreviewLayer for previewing the visual output of the specified AVCaptureSession, but creates no connections to any of the session's eligible video inputs. Only use this initializer if you intend to manually form a connection between a desired AVCaptureInputPort and the receiver using AVCaptureSession's -addConnection: method.
--
-- @session@ — The AVCaptureSession instance to be previewed.
--
-- Returns: A newly initialized AVCaptureVideoPreviewLayer instance.
--
-- ObjC selector: @+ layerWithSessionWithNoConnection:@
layerWithSessionWithNoConnection :: IsAVCaptureSession session => session -> IO (Id AVCaptureVideoPreviewLayer)
layerWithSessionWithNoConnection session =
  do
    cls' <- getRequiredClass "AVCaptureVideoPreviewLayer"
    withObjCPtr session $ \raw_session ->
      sendClassMsg cls' (mkSelector "layerWithSessionWithNoConnection:") (retPtr retVoid) [argPtr (castPtr raw_session :: Ptr ())] >>= retainedObject . castPtr

-- | initWithSessionWithNoConnection:
--
-- Creates an AVCaptureVideoPreviewLayer for previewing the visual output of the specified AVCaptureSession, but creates no connections to any of the session's eligible video inputs. Only use this initializer if you intend to manually form a connection between a desired AVCaptureInputPort and the receiver using AVCaptureSession's -addConnection: method.
--
-- @session@ — The AVCaptureSession instance to be previewed.
--
-- Returns: A newly initialized AVCaptureVideoPreviewLayer instance.
--
-- ObjC selector: @- initWithSessionWithNoConnection:@
initWithSessionWithNoConnection :: (IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer, IsAVCaptureSession session) => avCaptureVideoPreviewLayer -> session -> IO (Id AVCaptureVideoPreviewLayer)
initWithSessionWithNoConnection avCaptureVideoPreviewLayer  session =
  withObjCPtr session $ \raw_session ->
      sendMsg avCaptureVideoPreviewLayer (mkSelector "initWithSessionWithNoConnection:") (retPtr retVoid) [argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | method setSessionWithNoConnection:
--
-- Attaches the receiver to a given session without implicitly forming a connection to the first eligible video AVCaptureInputPort. Only use this setter if you intend to manually form a connection between a desired AVCaptureInputPort and the receiver using AVCaptureSession's -addConnection: method.
--
-- The session is retained by the preview layer.
--
-- ObjC selector: @- setSessionWithNoConnection:@
setSessionWithNoConnection :: (IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer, IsAVCaptureSession session) => avCaptureVideoPreviewLayer -> session -> IO ()
setSessionWithNoConnection avCaptureVideoPreviewLayer  session =
  withObjCPtr session $ \raw_session ->
      sendMsg avCaptureVideoPreviewLayer (mkSelector "setSessionWithNoConnection:") retVoid [argPtr (castPtr raw_session :: Ptr ())]

-- | transformedMetadataObjectForMetadataObject:
--
-- Converts an AVMetadataObject's visual properties to layer coordinates.
--
-- @metadataObject@ — An AVMetadataObject originating from the same AVCaptureInput as the preview layer.
--
-- Returns: An AVMetadataObject whose properties are in layer coordinates.
--
-- AVMetadataObject bounds may be expressed as a rect where {0,0} represents the top left of the picture area, and {1,1} represents the bottom right on an unrotated picture. Face metadata objects likewise express yaw and roll angles with respect to an unrotated picture. -transformedMetadataObjectForMetadataObject: converts the visual properties in the coordinate space of the supplied AVMetadataObject to the coordinate space of the receiver. The conversion takes orientation, mirroring, layer bounds and videoGravity into consideration. If the provided metadata object originates from an input source other than the preview layer's, nil will be returned.
--
-- ObjC selector: @- transformedMetadataObjectForMetadataObject:@
transformedMetadataObjectForMetadataObject :: (IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer, IsAVMetadataObject metadataObject) => avCaptureVideoPreviewLayer -> metadataObject -> IO (Id AVMetadataObject)
transformedMetadataObjectForMetadataObject avCaptureVideoPreviewLayer  metadataObject =
  withObjCPtr metadataObject $ \raw_metadataObject ->
      sendMsg avCaptureVideoPreviewLayer (mkSelector "transformedMetadataObjectForMetadataObject:") (retPtr retVoid) [argPtr (castPtr raw_metadataObject :: Ptr ())] >>= retainedObject . castPtr

-- | session
--
-- The AVCaptureSession instance being previewed by the receiver.
--
-- The session is retained by the preview layer.
--
-- ObjC selector: @- session@
session :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO (Id AVCaptureSession)
session avCaptureVideoPreviewLayer  =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | session
--
-- The AVCaptureSession instance being previewed by the receiver.
--
-- The session is retained by the preview layer.
--
-- ObjC selector: @- setSession:@
setSession :: (IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer, IsAVCaptureSession value) => avCaptureVideoPreviewLayer -> value -> IO ()
setSession avCaptureVideoPreviewLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureVideoPreviewLayer (mkSelector "setSession:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | connection
--
-- The AVCaptureConnection instance describing the AVCaptureInputPort to which the receiver is connected.
--
-- When calling initWithSession: or setSession: with a valid AVCaptureSession instance, a connection is formed to the first eligible video AVCaptureInput. If the receiver is detached from a session, the connection property becomes nil.
--
-- ObjC selector: @- connection@
connection :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO (Id AVCaptureConnection)
connection avCaptureVideoPreviewLayer  =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "connection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoGravity
--
-- A string defining how the video is displayed within an AVCaptureVideoPreviewLayer bounds rect.
--
-- Options are AVLayerVideoGravityResize, AVLayerVideoGravityResizeAspect and AVLayerVideoGravityResizeAspectFill. AVLayerVideoGravityResizeAspect is default. See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO (Id NSString)
videoGravity avCaptureVideoPreviewLayer  =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "videoGravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoGravity
--
-- A string defining how the video is displayed within an AVCaptureVideoPreviewLayer bounds rect.
--
-- Options are AVLayerVideoGravityResize, AVLayerVideoGravityResizeAspect and AVLayerVideoGravityResizeAspectFill. AVLayerVideoGravityResizeAspect is default. See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer, IsNSString value) => avCaptureVideoPreviewLayer -> value -> IO ()
setVideoGravity avCaptureVideoPreviewLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureVideoPreviewLayer (mkSelector "setVideoGravity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | previewing
--
-- A BOOL value indicating whether the receiver is currently rendering video frames from its source.
--
-- An AVCaptureVideoPreviewLayer begins previewing when -[AVCaptureSession startRunning] is called. When associated with an AVCaptureMultiCamSession, all video preview layers are guaranteed to be previewing by the time the blocking call to -startRunning or -commitConfiguration returns. While a session is running, you may enable or disable a video preview layer's connection to re-start or stop the flow of video to the layer. Once you've set enabled to YES, you can observe this property changing from NO to YES and synchronize any UI to take place precisely when the video resumes rendering to the video preview layer.
--
-- ObjC selector: @- previewing@
previewing :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
previewing avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "previewing") retCULong []

-- | orientationSupported
--
-- Specifies whether or not the preview layer supports orientation.
--
-- Changes in orientation are not supported on all hardware configurations. An application should check the value of "orientationSupported" before attempting to manipulate the orientation of the receiver. This property is deprecated. Use AVCaptureConnection's -isVideoOrientationSupported instead.
--
-- ObjC selector: @- orientationSupported@
orientationSupported :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
orientationSupported avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "orientationSupported") retCULong []

-- | orientation
--
-- Specifies the orientation of the preview layer.
--
-- AVCaptureVideoOrientation and its constants are defined in AVCaptureSession.h. The value of "orientationSupported" must be YES in order to set \@"orientation". An exception will be raised if this requirement is ignored. This property is deprecated. Use AVCaptureConnection's -videoOrientation instead.
--
-- ObjC selector: @- orientation@
orientation :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO AVCaptureVideoOrientation
orientation avCaptureVideoPreviewLayer  =
    fmap (coerce :: CLong -> AVCaptureVideoOrientation) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "orientation") retCLong []

-- | orientation
--
-- Specifies the orientation of the preview layer.
--
-- AVCaptureVideoOrientation and its constants are defined in AVCaptureSession.h. The value of "orientationSupported" must be YES in order to set \@"orientation". An exception will be raised if this requirement is ignored. This property is deprecated. Use AVCaptureConnection's -videoOrientation instead.
--
-- ObjC selector: @- setOrientation:@
setOrientation :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> AVCaptureVideoOrientation -> IO ()
setOrientation avCaptureVideoPreviewLayer  value =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "setOrientation:") retVoid [argCLong (coerce value)]

-- | mirroringSupported
--
-- Specifies whether or not the preview layer supports mirroring.
--
-- Mirroring is not supported on all hardware configurations. An application should check the value of "mirroringSupported" before attempting to manipulate mirroring on the receiver. This property is deprecated. Use AVCaptureConnection's -isVideoMirroringSupported instead.
--
-- ObjC selector: @- mirroringSupported@
mirroringSupported :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
mirroringSupported avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "mirroringSupported") retCULong []

-- | automaticallyAdjustsMirroring
--
-- Specifies whether or not the value of "mirrored" can change based on configuration of the session.
--
-- For some session configurations, preview will be mirrored by default. When the value of this property is YES, the value of "mirrored" may change depending on the configuration of the session, for example after switching to a different AVCaptureDeviceInput. The default value is YES. This property is deprecated. Use AVCaptureConnection's -automaticallyAdjustsVideoMirroring instead.
--
-- ObjC selector: @- automaticallyAdjustsMirroring@
automaticallyAdjustsMirroring :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
automaticallyAdjustsMirroring avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "automaticallyAdjustsMirroring") retCULong []

-- | automaticallyAdjustsMirroring
--
-- Specifies whether or not the value of "mirrored" can change based on configuration of the session.
--
-- For some session configurations, preview will be mirrored by default. When the value of this property is YES, the value of "mirrored" may change depending on the configuration of the session, for example after switching to a different AVCaptureDeviceInput. The default value is YES. This property is deprecated. Use AVCaptureConnection's -automaticallyAdjustsVideoMirroring instead.
--
-- ObjC selector: @- setAutomaticallyAdjustsMirroring:@
setAutomaticallyAdjustsMirroring :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> Bool -> IO ()
setAutomaticallyAdjustsMirroring avCaptureVideoPreviewLayer  value =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "setAutomaticallyAdjustsMirroring:") retVoid [argCULong (if value then 1 else 0)]

-- | mirrored
--
-- Specifies whether or not the preview is flipped over a vertical axis.
--
-- For most applications, it is unnecessary to manipulate preview mirroring manually if "automaticallyAdjustsMirroring" is set to YES. The value of \@"automaticallyAdjustsMirroring" must be NO in order to set \@"mirrored". The value of \@"mirroringSupported" must be YES in order to set \@"mirrored". An exception will be raised if the value of \@"mirrored" is mutated without respecting these requirements. This property is deprecated. Use AVCaptureConnection's -videoMirrored instead.
--
-- ObjC selector: @- mirrored@
mirrored :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
mirrored avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "mirrored") retCULong []

-- | mirrored
--
-- Specifies whether or not the preview is flipped over a vertical axis.
--
-- For most applications, it is unnecessary to manipulate preview mirroring manually if "automaticallyAdjustsMirroring" is set to YES. The value of \@"automaticallyAdjustsMirroring" must be NO in order to set \@"mirrored". The value of \@"mirroringSupported" must be YES in order to set \@"mirrored". An exception will be raised if the value of \@"mirrored" is mutated without respecting these requirements. This property is deprecated. Use AVCaptureConnection's -videoMirrored instead.
--
-- ObjC selector: @- setMirrored:@
setMirrored :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> Bool -> IO ()
setMirrored avCaptureVideoPreviewLayer  value =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "setMirrored:") retVoid [argCULong (if value then 1 else 0)]

-- | A @BOOL@ value that indicates whether the preview layer supports deferred start.
--
-- You can only set the ``deferredStartEnabled`` property to @true@ if the preview layer supports deferred start.
--
-- ObjC selector: @- deferredStartSupported@
deferredStartSupported :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
deferredStartSupported avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "deferredStartSupported") retCULong []

-- | A @BOOL@ value that indicates whether to defer starting this preview layer.
--
-- When this value is @true@, the session does not prepare the output's resources until some time after ``AVCaptureSession/startRunning`` returns. You can start the visual parts of your user interface (e.g. preview) prior to other parts (e.g. photo/movie capture, metadata output, etc..) to improve startup performance. Set this value to @false@ if your app needs video preview immediately for startup, and @true@ if it does not.
--
-- By default, this value is @false@ for ``AVCaptureVideoPreviewLayer`` objects, since this object is used to display preview. For best session start performance, set ``deferredStartEnabled`` to @false@ for preview layers. If your app contains multiple preview layers, you may want to display the main preview layer as soon as possible and allow the remaining layers to display subsequently. In this case, set ``deferredStartEnabled`` to @true@ for the remaining layers.
--
-- - Note: Setting this property to the same value for all outputs, including ``AVCaptureVideoPreviewLayer`` and ``AVCaptureOutput``, is equivalent to not using deferred start.
--
-- If ``deferredStartSupported`` is @false@, setting this property value to @true@ results in the session throwing an @NSInvalidArgumentException@.
--
-- - Note: Set this value before calling ``AVCaptureSession/commitConfiguration`` as it requires a lengthy reconfiguration of the capture render pipeline.
--
-- ObjC selector: @- deferredStartEnabled@
deferredStartEnabled :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> IO Bool
deferredStartEnabled avCaptureVideoPreviewLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoPreviewLayer (mkSelector "deferredStartEnabled") retCULong []

-- | A @BOOL@ value that indicates whether to defer starting this preview layer.
--
-- When this value is @true@, the session does not prepare the output's resources until some time after ``AVCaptureSession/startRunning`` returns. You can start the visual parts of your user interface (e.g. preview) prior to other parts (e.g. photo/movie capture, metadata output, etc..) to improve startup performance. Set this value to @false@ if your app needs video preview immediately for startup, and @true@ if it does not.
--
-- By default, this value is @false@ for ``AVCaptureVideoPreviewLayer`` objects, since this object is used to display preview. For best session start performance, set ``deferredStartEnabled`` to @false@ for preview layers. If your app contains multiple preview layers, you may want to display the main preview layer as soon as possible and allow the remaining layers to display subsequently. In this case, set ``deferredStartEnabled`` to @true@ for the remaining layers.
--
-- - Note: Setting this property to the same value for all outputs, including ``AVCaptureVideoPreviewLayer`` and ``AVCaptureOutput``, is equivalent to not using deferred start.
--
-- If ``deferredStartSupported`` is @false@, setting this property value to @true@ results in the session throwing an @NSInvalidArgumentException@.
--
-- - Note: Set this value before calling ``AVCaptureSession/commitConfiguration`` as it requires a lengthy reconfiguration of the capture render pipeline.
--
-- ObjC selector: @- setDeferredStartEnabled:@
setDeferredStartEnabled :: IsAVCaptureVideoPreviewLayer avCaptureVideoPreviewLayer => avCaptureVideoPreviewLayer -> Bool -> IO ()
setDeferredStartEnabled avCaptureVideoPreviewLayer  value =
    sendMsg avCaptureVideoPreviewLayer (mkSelector "setDeferredStartEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithSession:@
layerWithSessionSelector :: Selector
layerWithSessionSelector = mkSelector "layerWithSession:"

-- | @Selector@ for @initWithSession:@
initWithSessionSelector :: Selector
initWithSessionSelector = mkSelector "initWithSession:"

-- | @Selector@ for @layerWithSessionWithNoConnection:@
layerWithSessionWithNoConnectionSelector :: Selector
layerWithSessionWithNoConnectionSelector = mkSelector "layerWithSessionWithNoConnection:"

-- | @Selector@ for @initWithSessionWithNoConnection:@
initWithSessionWithNoConnectionSelector :: Selector
initWithSessionWithNoConnectionSelector = mkSelector "initWithSessionWithNoConnection:"

-- | @Selector@ for @setSessionWithNoConnection:@
setSessionWithNoConnectionSelector :: Selector
setSessionWithNoConnectionSelector = mkSelector "setSessionWithNoConnection:"

-- | @Selector@ for @transformedMetadataObjectForMetadataObject:@
transformedMetadataObjectForMetadataObjectSelector :: Selector
transformedMetadataObjectForMetadataObjectSelector = mkSelector "transformedMetadataObjectForMetadataObject:"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @setSession:@
setSessionSelector :: Selector
setSessionSelector = mkSelector "setSession:"

-- | @Selector@ for @connection@
connectionSelector :: Selector
connectionSelector = mkSelector "connection"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector
setVideoGravitySelector = mkSelector "setVideoGravity:"

-- | @Selector@ for @previewing@
previewingSelector :: Selector
previewingSelector = mkSelector "previewing"

-- | @Selector@ for @orientationSupported@
orientationSupportedSelector :: Selector
orientationSupportedSelector = mkSelector "orientationSupported"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @mirroringSupported@
mirroringSupportedSelector :: Selector
mirroringSupportedSelector = mkSelector "mirroringSupported"

-- | @Selector@ for @automaticallyAdjustsMirroring@
automaticallyAdjustsMirroringSelector :: Selector
automaticallyAdjustsMirroringSelector = mkSelector "automaticallyAdjustsMirroring"

-- | @Selector@ for @setAutomaticallyAdjustsMirroring:@
setAutomaticallyAdjustsMirroringSelector :: Selector
setAutomaticallyAdjustsMirroringSelector = mkSelector "setAutomaticallyAdjustsMirroring:"

-- | @Selector@ for @mirrored@
mirroredSelector :: Selector
mirroredSelector = mkSelector "mirrored"

-- | @Selector@ for @setMirrored:@
setMirroredSelector :: Selector
setMirroredSelector = mkSelector "setMirrored:"

-- | @Selector@ for @deferredStartSupported@
deferredStartSupportedSelector :: Selector
deferredStartSupportedSelector = mkSelector "deferredStartSupported"

-- | @Selector@ for @deferredStartEnabled@
deferredStartEnabledSelector :: Selector
deferredStartEnabledSelector = mkSelector "deferredStartEnabled"

-- | @Selector@ for @setDeferredStartEnabled:@
setDeferredStartEnabledSelector :: Selector
setDeferredStartEnabledSelector = mkSelector "setDeferredStartEnabled:"

