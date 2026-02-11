{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.VideoToolbox.Internal.Classes (
    module ObjC.VideoToolbox.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- VTFrameProcessor ----------

-- | Provides a unified interface you can use to apply video effects to frames.
--
-- The VTFrameProcessor gives access to a set of powerful video processing implementation suitable for different use cases. A configuration object (conforming to the ``VTFrameProcessorConfiguration`` protocol) passes initialization and configuration parameters for the processor. A Parameter object (conforming to the ``VTFrameProcessorParameters`` protocol) provides the parameters for each individual processing operation. A Configuration object and a Parameter object define each processor implementation. These Configuration and Parameters objects for each implementation are defined in a processor-specific header file.
--
-- Use an instance of this class to apply configured video effects either directly to pixel buffers or as a part of Metal pipeline. The video effect must be specified as a ``VTFrameProcessorConfiguration`` instance at session startup. Once a session is started, you need to call one of the process methods for each input frame. After all input frames have been provided, session must be ended for the system to finish all pending processing.
--
-- After you call the process function, you must not modify input and output buffers (including attachments) before the function returns or the system receives the callback, in the case of asynchronous processing.
-- 
-- Phantom type for @VTFrameProcessor@.
data VTFrameProcessor

instance IsObjCObject (Id VTFrameProcessor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTFrameProcessor"

class IsNSObject a => IsVTFrameProcessor a where
  toVTFrameProcessor :: a -> Id VTFrameProcessor

instance IsVTFrameProcessor (Id VTFrameProcessor) where
  toVTFrameProcessor = unsafeCastId

instance IsNSObject (Id VTFrameProcessor) where
  toNSObject = unsafeCastId

-- ---------- VTFrameProcessorFrame ----------

-- | Helper class to wrap pixel buffers as video frames.
--
-- You can use the frames as source frames, reference frames, or output frames of a processor. Frame instances retain the backing pixel buffer.
-- 
-- Phantom type for @VTFrameProcessorFrame@.
data VTFrameProcessorFrame

instance IsObjCObject (Id VTFrameProcessorFrame) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTFrameProcessorFrame"

class IsNSObject a => IsVTFrameProcessorFrame a where
  toVTFrameProcessorFrame :: a -> Id VTFrameProcessorFrame

instance IsVTFrameProcessorFrame (Id VTFrameProcessorFrame) where
  toVTFrameProcessorFrame = unsafeCastId

instance IsNSObject (Id VTFrameProcessorFrame) where
  toNSObject = unsafeCastId

-- ---------- VTFrameProcessorOpticalFlow ----------

-- | Helper class to wrap optical flow.
--
-- Instances retain the backing pixel buffers that you provide.
-- 
-- Phantom type for @VTFrameProcessorOpticalFlow@.
data VTFrameProcessorOpticalFlow

instance IsObjCObject (Id VTFrameProcessorOpticalFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTFrameProcessorOpticalFlow"

class IsNSObject a => IsVTFrameProcessorOpticalFlow a where
  toVTFrameProcessorOpticalFlow :: a -> Id VTFrameProcessorOpticalFlow

instance IsVTFrameProcessorOpticalFlow (Id VTFrameProcessorOpticalFlow) where
  toVTFrameProcessorOpticalFlow = unsafeCastId

instance IsNSObject (Id VTFrameProcessorOpticalFlow) where
  toNSObject = unsafeCastId

-- ---------- VTFrameRateConversionConfiguration ----------

-- | Configuration that you use to set up the frame rate conversion processor.
--
-- This configuration enables the frame-rate conversion on a @VTFrameProcessor@ session.
-- 
-- Phantom type for @VTFrameRateConversionConfiguration@.
data VTFrameRateConversionConfiguration

instance IsObjCObject (Id VTFrameRateConversionConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTFrameRateConversionConfiguration"

class IsNSObject a => IsVTFrameRateConversionConfiguration a where
  toVTFrameRateConversionConfiguration :: a -> Id VTFrameRateConversionConfiguration

instance IsVTFrameRateConversionConfiguration (Id VTFrameRateConversionConfiguration) where
  toVTFrameRateConversionConfiguration = unsafeCastId

instance IsNSObject (Id VTFrameRateConversionConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTFrameRateConversionParameters ----------

-- | An object that contains both input and output parameters, which the frame-rate conversion processor needs to process a frame.
--
-- Use this object as a parameter to the ``VTFrameProcessor/processWithParameters`` method. The output parameter for this class is ``destinationFrame`` where the processor returns output frame (as mutable ``VTFrameProcessorFrame``) back to you once the @processWithParameters@ completes.
--
-- @VTFrameRateConversionParameters@ are frame-level parameters.
-- 
-- Phantom type for @VTFrameRateConversionParameters@.
data VTFrameRateConversionParameters

instance IsObjCObject (Id VTFrameRateConversionParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTFrameRateConversionParameters"

class IsNSObject a => IsVTFrameRateConversionParameters a where
  toVTFrameRateConversionParameters :: a -> Id VTFrameRateConversionParameters

instance IsVTFrameRateConversionParameters (Id VTFrameRateConversionParameters) where
  toVTFrameRateConversionParameters = unsafeCastId

instance IsNSObject (Id VTFrameRateConversionParameters) where
  toNSObject = unsafeCastId

-- ---------- VTLowLatencyFrameInterpolationConfiguration ----------

-- | Configuration that you use to program Video Toolbox frame processor for low-latency frame interpolation.
--
-- This configuration can do either purely temporal interpolation (frame-rate conversion) or temporal and spatial interpolation (scaling and frame-rate conversion). This processor requires a source frame and a previous frame. It does temporal scaling, which interpolates frames between the previous frame and the source frame. When performing both temporal and spatial interpolation, the processor can only perform 2x upscaling, and a single frame of temporal interpolation. When performing spatial scaling, the processor produces upscaled intermediate frames and an upscaled @sourceFrame@, but it does not upscale the previous reference frame you provided.
--
-- > Important: When calling ``VTFrameProcessor/startSessionWithConfiguration:error:`` to create a @VTLowLatencyFrameInterpolation@ session, ML model loading may take longer than a frame time. Avoid blocking the UI thread or stalling frame rendering pipelines during this call.
-- 
-- Phantom type for @VTLowLatencyFrameInterpolationConfiguration@.
data VTLowLatencyFrameInterpolationConfiguration

instance IsObjCObject (Id VTLowLatencyFrameInterpolationConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTLowLatencyFrameInterpolationConfiguration"

class IsNSObject a => IsVTLowLatencyFrameInterpolationConfiguration a where
  toVTLowLatencyFrameInterpolationConfiguration :: a -> Id VTLowLatencyFrameInterpolationConfiguration

instance IsVTLowLatencyFrameInterpolationConfiguration (Id VTLowLatencyFrameInterpolationConfiguration) where
  toVTLowLatencyFrameInterpolationConfiguration = unsafeCastId

instance IsNSObject (Id VTLowLatencyFrameInterpolationConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTLowLatencyFrameInterpolationParameters ----------

-- | An object that contains both input and output parameters that the low-latency frame interpolation processor needs.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class.
--
-- @VTLowLatencyFrameInterpolationParameters@ are frame-level parameters.
-- 
-- Phantom type for @VTLowLatencyFrameInterpolationParameters@.
data VTLowLatencyFrameInterpolationParameters

instance IsObjCObject (Id VTLowLatencyFrameInterpolationParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTLowLatencyFrameInterpolationParameters"

class IsNSObject a => IsVTLowLatencyFrameInterpolationParameters a where
  toVTLowLatencyFrameInterpolationParameters :: a -> Id VTLowLatencyFrameInterpolationParameters

instance IsVTLowLatencyFrameInterpolationParameters (Id VTLowLatencyFrameInterpolationParameters) where
  toVTLowLatencyFrameInterpolationParameters = unsafeCastId

instance IsNSObject (Id VTLowLatencyFrameInterpolationParameters) where
  toNSObject = unsafeCastId

-- ---------- VTLowLatencySuperResolutionScalerConfiguration ----------

-- | An object you use to configure frame processor for low-latency super-resolution scaler processing.
--
-- Use this object to configure a ``VTFrameProcessor``. Query this interface also for important operating details, like the pixel buffer attributes required for frames you submit to the processor.
--
-- > Important: When calling ``VTFrameProcessor/startSessionWithConfiguration:error:`` to create a @VTLowLatencySuperResolutionScaler@ session, ML model loading may take longer than a frame time. Avoid blocking the UI thread or stalling frame rendering pipelines during this call.
-- 
-- Phantom type for @VTLowLatencySuperResolutionScalerConfiguration@.
data VTLowLatencySuperResolutionScalerConfiguration

instance IsObjCObject (Id VTLowLatencySuperResolutionScalerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"

class IsNSObject a => IsVTLowLatencySuperResolutionScalerConfiguration a where
  toVTLowLatencySuperResolutionScalerConfiguration :: a -> Id VTLowLatencySuperResolutionScalerConfiguration

instance IsVTLowLatencySuperResolutionScalerConfiguration (Id VTLowLatencySuperResolutionScalerConfiguration) where
  toVTLowLatencySuperResolutionScalerConfiguration = unsafeCastId

instance IsNSObject (Id VTLowLatencySuperResolutionScalerConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTLowLatencySuperResolutionScalerParameters ----------

-- | An object that contains both input and output parameters that the low-latency super-resolution scaler frame processor needs.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class.
--
-- @VTLowLatencySuperResolutionScalerParameters@ are frame-level parameters.
-- 
-- Phantom type for @VTLowLatencySuperResolutionScalerParameters@.
data VTLowLatencySuperResolutionScalerParameters

instance IsObjCObject (Id VTLowLatencySuperResolutionScalerParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTLowLatencySuperResolutionScalerParameters"

class IsNSObject a => IsVTLowLatencySuperResolutionScalerParameters a where
  toVTLowLatencySuperResolutionScalerParameters :: a -> Id VTLowLatencySuperResolutionScalerParameters

instance IsVTLowLatencySuperResolutionScalerParameters (Id VTLowLatencySuperResolutionScalerParameters) where
  toVTLowLatencySuperResolutionScalerParameters = unsafeCastId

instance IsNSObject (Id VTLowLatencySuperResolutionScalerParameters) where
  toNSObject = unsafeCastId

-- ---------- VTMotionBlurConfiguration ----------

-- | Configuration that you use to set up the motion blur processor.
--
-- This configuration enables the motion blur on a @VTFrameProcessor@ session.
-- 
-- Phantom type for @VTMotionBlurConfiguration@.
data VTMotionBlurConfiguration

instance IsObjCObject (Id VTMotionBlurConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTMotionBlurConfiguration"

class IsNSObject a => IsVTMotionBlurConfiguration a where
  toVTMotionBlurConfiguration :: a -> Id VTMotionBlurConfiguration

instance IsVTMotionBlurConfiguration (Id VTMotionBlurConfiguration) where
  toVTMotionBlurConfiguration = unsafeCastId

instance IsNSObject (Id VTMotionBlurConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTMotionBlurParameters ----------

-- | An object that contains both input and output parameters that the motion blur processor needs to run on a frame.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class. The output parameter for this class is @destinationFrame@ where the processor returns the output frame (as @VTFrameProcessorFrame@) back to you once the @processWithParameters@ completes.
--
-- @VTMotionBlurParameters@ are frame-level parameters.
-- 
-- Phantom type for @VTMotionBlurParameters@.
data VTMotionBlurParameters

instance IsObjCObject (Id VTMotionBlurParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTMotionBlurParameters"

class IsNSObject a => IsVTMotionBlurParameters a where
  toVTMotionBlurParameters :: a -> Id VTMotionBlurParameters

instance IsVTMotionBlurParameters (Id VTMotionBlurParameters) where
  toVTMotionBlurParameters = unsafeCastId

instance IsNSObject (Id VTMotionBlurParameters) where
  toNSObject = unsafeCastId

-- ---------- VTOpticalFlowConfiguration ----------

-- | Configuration that you use to set up an optical flow processor
--
-- This configuration enables the optical flow on a @VTFrameProcessor@ session.
-- 
-- Phantom type for @VTOpticalFlowConfiguration@.
data VTOpticalFlowConfiguration

instance IsObjCObject (Id VTOpticalFlowConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTOpticalFlowConfiguration"

class IsNSObject a => IsVTOpticalFlowConfiguration a where
  toVTOpticalFlowConfiguration :: a -> Id VTOpticalFlowConfiguration

instance IsVTOpticalFlowConfiguration (Id VTOpticalFlowConfiguration) where
  toVTOpticalFlowConfiguration = unsafeCastId

instance IsNSObject (Id VTOpticalFlowConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTOpticalFlowParameters ----------

-- | An object that contains both input and output parameters the frame processor needs to generate optical flow between two frames.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class. The output parameter for this class is @destinationOpticalFlow@ where the processor returns the output flow (as mutable @VTFrameProcessorOpticalFlow@) back to you once the @processWithParameters@ completes.
--
-- @VTOpticalFlowParameters@ are frame-level parameters.
-- 
-- Phantom type for @VTOpticalFlowParameters@.
data VTOpticalFlowParameters

instance IsObjCObject (Id VTOpticalFlowParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTOpticalFlowParameters"

class IsNSObject a => IsVTOpticalFlowParameters a where
  toVTOpticalFlowParameters :: a -> Id VTOpticalFlowParameters

instance IsVTOpticalFlowParameters (Id VTOpticalFlowParameters) where
  toVTOpticalFlowParameters = unsafeCastId

instance IsNSObject (Id VTOpticalFlowParameters) where
  toNSObject = unsafeCastId

-- ---------- VTSuperResolutionScalerConfiguration ----------

-- | Configuration that you use to set up the super-resolution processor.
--
-- This configuration enables the super-resolution processor on a @VTFrameProcessor@ session.
--
-- > Important: The super-resolution processor may require ML models which the framework needs to download in order to operate. Before calling ``VTFrameProcessor/startSessionWithConfiguration:error:`` with an instance of this class, it is important that you verify that the necessary models are present by checking ``configurationModelStatus``. If models are not available, you can trigger model download using the ``downloadConfigurationModelWithCompletionHandler:`` method. Best practice is to confirm availability of models and drive download with user awareness and interaction before engaging workflows that need this processor.
-- 
-- Phantom type for @VTSuperResolutionScalerConfiguration@.
data VTSuperResolutionScalerConfiguration

instance IsObjCObject (Id VTSuperResolutionScalerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTSuperResolutionScalerConfiguration"

class IsNSObject a => IsVTSuperResolutionScalerConfiguration a where
  toVTSuperResolutionScalerConfiguration :: a -> Id VTSuperResolutionScalerConfiguration

instance IsVTSuperResolutionScalerConfiguration (Id VTSuperResolutionScalerConfiguration) where
  toVTSuperResolutionScalerConfiguration = unsafeCastId

instance IsNSObject (Id VTSuperResolutionScalerConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTSuperResolutionScalerParameters ----------

-- | An object that contains both input and output parameters that the super-resolution processor needs to run on a frame.
--
-- Use this object in the @processWithParameters@ call of the @VTFrameProcessor@ class. The output parameter for this class is @destinationFrame@, where the processor returns the output frame (as @VTFrameProcessorFrame@) back to you once @processWithParameters@ completes.
--
-- @VTSuperResolutionScalerParameters@ are frame-level parameters.
-- 
-- Phantom type for @VTSuperResolutionScalerParameters@.
data VTSuperResolutionScalerParameters

instance IsObjCObject (Id VTSuperResolutionScalerParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTSuperResolutionScalerParameters"

class IsNSObject a => IsVTSuperResolutionScalerParameters a where
  toVTSuperResolutionScalerParameters :: a -> Id VTSuperResolutionScalerParameters

instance IsVTSuperResolutionScalerParameters (Id VTSuperResolutionScalerParameters) where
  toVTSuperResolutionScalerParameters = unsafeCastId

instance IsNSObject (Id VTSuperResolutionScalerParameters) where
  toNSObject = unsafeCastId

-- ---------- VTTemporalNoiseFilterConfiguration ----------

-- | A configuration object to initiate a frame processor and use temporal noise-filter processor.
--
-- The class properties of @VTTemporalNoiseFilterConfiguration@ help to identify the capabilities of temporal noise filter processor on the current platform, prior to initiating a session. You can confirm the availability of temporal noise-filter processor in the current platform by checking the ``isSupported`` class property. Verify the processor's capability to process source frames by ensuring that the dimensions are no less than ``minimumDimensions`` and no greater than ``maximumDimensions``. Use the instance properties such as ``frameSupportedPixelFormats``, ``sourcePixelBufferAttributes``, and ``destinationPixelBufferAttributes`` to ensure that the input and output pixel buffer formats and attributes of the processor align with the client's specific requirements. The properties ``previousFrameCount`` and ``nextFrameCount`` represent the maximum number of preceding and subsequent reference frames, used in the processing of a source frame, to achieve optimum noise-reduction quality.
-- 
-- Phantom type for @VTTemporalNoiseFilterConfiguration@.
data VTTemporalNoiseFilterConfiguration

instance IsObjCObject (Id VTTemporalNoiseFilterConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTTemporalNoiseFilterConfiguration"

class IsNSObject a => IsVTTemporalNoiseFilterConfiguration a where
  toVTTemporalNoiseFilterConfiguration :: a -> Id VTTemporalNoiseFilterConfiguration

instance IsVTTemporalNoiseFilterConfiguration (Id VTTemporalNoiseFilterConfiguration) where
  toVTTemporalNoiseFilterConfiguration = unsafeCastId

instance IsNSObject (Id VTTemporalNoiseFilterConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VTTemporalNoiseFilterParameters ----------

-- | Encapsulates the frame-level parameters necessary for processing a source frame using temporal noise-filter processor.
--
-- This object is intended for sending input parameters into the @processWithParameters@ method of the @VTFrameProcessor@ class. Temporal noise-filter processor utilizes past and future reference frames, provided in presentation time order, to reduce noise from the source frame. The @previousFrameCount@ and @nextFrameCount@ properties in ``VTTemporalNoiseFilterConfiguration`` represent the maximum number of past and future reference frames that the processor can use to achieve optimum noise reduction quality. The number of reference frames provided shall depend on their availability, but at a minimum, you must provide one reference frame, either past or future. The parameter @destinationFrame@ stores the output frame that the processor returns to the caller upon the successful completion of the @processWithParameters@ operation.
-- 
-- Phantom type for @VTTemporalNoiseFilterParameters@.
data VTTemporalNoiseFilterParameters

instance IsObjCObject (Id VTTemporalNoiseFilterParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VTTemporalNoiseFilterParameters"

class IsNSObject a => IsVTTemporalNoiseFilterParameters a where
  toVTTemporalNoiseFilterParameters :: a -> Id VTTemporalNoiseFilterParameters

instance IsVTTemporalNoiseFilterParameters (Id VTTemporalNoiseFilterParameters) where
  toVTTemporalNoiseFilterParameters = unsafeCastId

instance IsNSObject (Id VTTemporalNoiseFilterParameters) where
  toNSObject = unsafeCastId
