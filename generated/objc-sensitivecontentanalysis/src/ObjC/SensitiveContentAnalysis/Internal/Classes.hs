{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SensitiveContentAnalysis.Internal.Classes (
    module ObjC.SensitiveContentAnalysis.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SCSensitivityAnalysis ----------

-- | Sensitive Analysis Results object is returned after sensitivity analysis is performed on media
-- 
-- Phantom type for @SCSensitivityAnalysis@.
data SCSensitivityAnalysis

instance IsObjCObject (Id SCSensitivityAnalysis) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCSensitivityAnalysis"

class IsNSObject a => IsSCSensitivityAnalysis a where
  toSCSensitivityAnalysis :: a -> Id SCSensitivityAnalysis

instance IsSCSensitivityAnalysis (Id SCSensitivityAnalysis) where
  toSCSensitivityAnalysis = unsafeCastId

instance IsNSObject (Id SCSensitivityAnalysis) where
  toNSObject = unsafeCastId

-- ---------- SCSensitivityAnalyzer ----------

-- | Main class for content sensitivity analysis
-- 
-- Phantom type for @SCSensitivityAnalyzer@.
data SCSensitivityAnalyzer

instance IsObjCObject (Id SCSensitivityAnalyzer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCSensitivityAnalyzer"

class IsNSObject a => IsSCSensitivityAnalyzer a where
  toSCSensitivityAnalyzer :: a -> Id SCSensitivityAnalyzer

instance IsSCSensitivityAnalyzer (Id SCSensitivityAnalyzer) where
  toSCSensitivityAnalyzer = unsafeCastId

instance IsNSObject (Id SCSensitivityAnalyzer) where
  toNSObject = unsafeCastId

-- ---------- SCVideoStreamAnalyzer ----------

-- | Monitors a stream of video by analyzing frames for sensitive content.
--
-- Use this class to detect senstive content in a video stream, such as on a conference call that your app implements. The class detects senstive content in the video stream from either the device's camera or the remote device(s) signed into the call, depending on how you configure the analyzer.
--
-- Create an instance of this class for each video stream in the call.
--
-- To begin analyzing the stream, pass it to either ``beginAnalysis(of:)-(AVCaptureDeviceInput)`` (<doc://com.apple.documentation/documentation/avfoundation/avcapturedeviceinput>) or ``beginAnalysis(of:)-(VTDecompressionSession)`` (<doc://com.apple.documentation/documentation/videotoolbox/vtdecompressionsession>), depending on your video playback implementation.
--
-- - Important: This class works only when the Communication Safety parental control in Screen Time is enabled, or when Sensitive Content Warnings is on in Settings. The initializers of this class throw an error if both settings are off.
--
-- ### React to sensitive content
--
-- When the framework detects sensitive content in the stream, it calls ``analysisChangedHandler`` immediately with an ``SCSensitivityAnalysis`` object that includes information about the detection.
--
-- You implement the ``analysisChangedHandler`` callback to inspect the detection results, which includes confirmation that content is sensitve as well as guidance on next steps your app can take. The framework offers your app suggestions in the handler, which include:
--
-- - Alerting the person to the presence of sensitive content (``SCSensitivityAnalysis/shouldIndicateSensitivity``) - Interrupting video playback (``SCSensitivityAnalysis/shouldInterruptVideo``) - Muting audio (``SCSensitivityAnalysis/shouldMuteAudio``)
--
-- To stop analyzing the stream, call ``endAnalysis()``. If your app implements a custom stream decoder, you can analyze individual frames by passing pixel buffers to ``analyze(_:)``.
--
-- In the event of an error during analysis, the handler receives an error object that details what went wrong. For more information, see: ``SCVideoStreamAnalysisChangeHandler``.
--
-- ### Add the app entitlement
--
-- To use this class, the system requires the <doc://com.apple.documentation/documentation/bundleresources/entitlements/com.apple.developer.sensitivecontentanalysis.client> entitlement in your app's code signature. Calls to the framework fail to return positive results without it. You can can add this entitlement to your app by enabling the Sensitive Content Analysis capability in Xcode; see <doc://com.apple.documentation/documentation/xcode/adding-capabilities-to-your-app>.
--
-- For more information, see <doc:detecting-nudity-in-media-and-providing-intervention-options>.
-- 
-- Phantom type for @SCVideoStreamAnalyzer@.
data SCVideoStreamAnalyzer

instance IsObjCObject (Id SCVideoStreamAnalyzer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCVideoStreamAnalyzer"

class IsNSObject a => IsSCVideoStreamAnalyzer a where
  toSCVideoStreamAnalyzer :: a -> Id SCVideoStreamAnalyzer

instance IsSCVideoStreamAnalyzer (Id SCVideoStreamAnalyzer) where
  toSCVideoStreamAnalyzer = unsafeCastId

instance IsNSObject (Id SCVideoStreamAnalyzer) where
  toNSObject = unsafeCastId
