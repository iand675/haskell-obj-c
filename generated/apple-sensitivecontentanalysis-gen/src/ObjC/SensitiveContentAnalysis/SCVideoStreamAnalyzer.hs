{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @SCVideoStreamAnalyzer@.
module ObjC.SensitiveContentAnalysis.SCVideoStreamAnalyzer
  ( SCVideoStreamAnalyzer
  , IsSCVideoStreamAnalyzer(..)
  , initWithParticipantUUID_streamDirection_error
  , init_
  , new
  , analyzePixelBuffer
  , beginAnalysisOfDecompressionSession_error
  , beginAnalysisOfCaptureDeviceInput_error
  , endAnalysis
  , continueStream
  , analysis
  , analysisChangedHandler
  , setAnalysisChangedHandler
  , analysisChangedHandlerSelector
  , analysisSelector
  , analyzePixelBufferSelector
  , beginAnalysisOfCaptureDeviceInput_errorSelector
  , beginAnalysisOfDecompressionSession_errorSelector
  , continueStreamSelector
  , endAnalysisSelector
  , initSelector
  , initWithParticipantUUID_streamDirection_errorSelector
  , newSelector
  , setAnalysisChangedHandlerSelector

  -- * Enum types
  , SCVideoStreamAnalyzerStreamDirection(SCVideoStreamAnalyzerStreamDirection)
  , pattern SCVideoStreamAnalyzerStreamDirectionOutgoing
  , pattern SCVideoStreamAnalyzerStreamDirectionIncoming

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensitiveContentAnalysis.Internal.Classes
import ObjC.SensitiveContentAnalysis.Internal.Enums
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a video stream analyzer for the given call participant and stream option.
--
-- - parameter participantUUID: A unique identifier that you provide to distinguish among multiple individuals on a conference call. Set this argument to the same value per person on the call, if your app supports multiple streams per person. - parameter streamDirection: An option that indicates whether the stream comes from the device's camera or from a remote individual signed in to the call. - parameter error: An error that occurs while intializing a video stream analyzer.
--
-- - Important: This class works only when the Communication Safety parental control in Screen Time is enabled, or when Sensitive Content Warnings is on in Settings. This method throws an error if both settings are off, or if the device doesn't support analysis for the specified stream direction.
--
-- ObjC selector: @- initWithParticipantUUID:streamDirection:error:@
initWithParticipantUUID_streamDirection_error :: (IsSCVideoStreamAnalyzer scVideoStreamAnalyzer, IsNSString participantUUID, IsNSError error_) => scVideoStreamAnalyzer -> participantUUID -> SCVideoStreamAnalyzerStreamDirection -> error_ -> IO (Id SCVideoStreamAnalyzer)
initWithParticipantUUID_streamDirection_error scVideoStreamAnalyzer participantUUID streamDirection error_ =
  sendOwnedMessage scVideoStreamAnalyzer initWithParticipantUUID_streamDirection_errorSelector (toNSString participantUUID) streamDirection (toNSError error_)

-- | @- init@
init_ :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> IO (Id SCVideoStreamAnalyzer)
init_ scVideoStreamAnalyzer =
  sendOwnedMessage scVideoStreamAnalyzer initSelector

-- | @- new@
new :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> IO (Id SCVideoStreamAnalyzer)
new scVideoStreamAnalyzer =
  sendOwnedMessage scVideoStreamAnalyzer newSelector

-- | Analyzes individual video-stream frames for sensitive content.
--
-- This method analyzes a specific video frame and updates ``analysis`` according to the results. If your app implements a custom stream decoder, you can call this method for each video frame.
--
-- ObjC selector: @- analyzePixelBuffer:@
analyzePixelBuffer :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> Ptr () -> IO ()
analyzePixelBuffer scVideoStreamAnalyzer pixelBuffer =
  sendMessage scVideoStreamAnalyzer analyzePixelBufferSelector pixelBuffer

-- | Analyzes video frames for the given decompression session.
--
-- - parameter decompressionSession: An object that provides video frames for your app to analyze for sensitive content. - parameter error: An error object that describes an issue that occurs while providing the video decompression session. - returns: @YES@ if the method succeeds; otherwise, @NO@.
--
-- If the framework detects sensitive content in the video stream, the <doc://com.apple.documentation/documentation/videotoolbox/vtdecompressionsession> produces blank frames to effectively censor the video stream on the person's behalf. When your app is ready to show the video stream again, resume analysis by calling ``continueStream``.
--
-- ObjC selector: @- beginAnalysisOfDecompressionSession:error:@
beginAnalysisOfDecompressionSession_error :: (IsSCVideoStreamAnalyzer scVideoStreamAnalyzer, IsNSError error_) => scVideoStreamAnalyzer -> Ptr () -> error_ -> IO Bool
beginAnalysisOfDecompressionSession_error scVideoStreamAnalyzer decompressionSession error_ =
  sendMessage scVideoStreamAnalyzer beginAnalysisOfDecompressionSession_errorSelector decompressionSession (toNSError error_)

-- | Analyzes video frames for the given capture device input.
--
-- - parameter captureDeviceInput: An object that contains information about the specific camera and its captured content in the video stream. - parameter error: An error object that describes an issue that occurs while processing the capture device input. - returns: @YES@ if the method succeeds; otherwise, @NO@.
--
-- Call this method to begin analyzing a video stream from the given <doc://com.apple.documentation/documentation/avfoundation/avcapturedeviceinput>. If the framework detects sensitive content in the video stream, the capture-device-input interrupts subsequent frames with the @AVCaptureSessionInterruptionReasonSensitiveContentMitigationActivated@ interruption reason to effectively censor the video stream on the person's behalf. When your app is ready to show the video stream again, resume analysis by calling ``continueStream``.
--
-- ObjC selector: @- beginAnalysisOfCaptureDeviceInput:error:@
beginAnalysisOfCaptureDeviceInput_error :: (IsSCVideoStreamAnalyzer scVideoStreamAnalyzer, IsAVCaptureDeviceInput captureDeviceInput, IsNSError error_) => scVideoStreamAnalyzer -> captureDeviceInput -> error_ -> IO Bool
beginAnalysisOfCaptureDeviceInput_error scVideoStreamAnalyzer captureDeviceInput error_ =
  sendMessage scVideoStreamAnalyzer beginAnalysisOfCaptureDeviceInput_errorSelector (toAVCaptureDeviceInput captureDeviceInput) (toNSError error_)

-- | Stops stream analysis.
--
-- This method stops analyzing the video stream in reference to the most recent call to @beginAnalysis@.
--
-- ObjC selector: @- endAnalysis@
endAnalysis :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> IO ()
endAnalysis scVideoStreamAnalyzer =
  sendMessage scVideoStreamAnalyzer endAnalysisSelector

-- | Indicates that your app is ready to resume video stream analysis.
--
-- When the framework detects sensitive content in the video stream, it pauses analysis and begins censoring the stream's video frames. Call this method to resume analysis and stop censoring video frames when your app is ready to show the stream again.
--
-- ObjC selector: @- continueStream@
continueStream :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> IO ()
continueStream scVideoStreamAnalyzer =
  sendMessage scVideoStreamAnalyzer continueStreamSelector

-- | The results of the first detected sensitive video frame.
--
-- The analysis also includes suggestions for the app based on the nature of the sensitive content, specifically: ``SCSensitivityAnalysis/shouldInterruptVideo``, ``SCSensitivityAnalysis/shouldIndicateSensitivity`` and ``SCSensitivityAnalysis/shouldMuteAudio``.
--
-- ObjC selector: @- analysis@
analysis :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> IO (Id SCSensitivityAnalysis)
analysis scVideoStreamAnalyzer =
  sendMessage scVideoStreamAnalyzer analysisSelector

-- | A handler that your app provides to react to sensitive content detection.
--
-- The system invokes all analyzer handlers on the same conference call with the updated analysis or error.
--
-- ObjC selector: @- analysisChangedHandler@
analysisChangedHandler :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> IO (Ptr ())
analysisChangedHandler scVideoStreamAnalyzer =
  sendMessage scVideoStreamAnalyzer analysisChangedHandlerSelector

-- | A handler that your app provides to react to sensitive content detection.
--
-- The system invokes all analyzer handlers on the same conference call with the updated analysis or error.
--
-- ObjC selector: @- setAnalysisChangedHandler:@
setAnalysisChangedHandler :: IsSCVideoStreamAnalyzer scVideoStreamAnalyzer => scVideoStreamAnalyzer -> Ptr () -> IO ()
setAnalysisChangedHandler scVideoStreamAnalyzer value =
  sendMessage scVideoStreamAnalyzer setAnalysisChangedHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParticipantUUID:streamDirection:error:@
initWithParticipantUUID_streamDirection_errorSelector :: Selector '[Id NSString, SCVideoStreamAnalyzerStreamDirection, Id NSError] (Id SCVideoStreamAnalyzer)
initWithParticipantUUID_streamDirection_errorSelector = mkSelector "initWithParticipantUUID:streamDirection:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCVideoStreamAnalyzer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SCVideoStreamAnalyzer)
newSelector = mkSelector "new"

-- | @Selector@ for @analyzePixelBuffer:@
analyzePixelBufferSelector :: Selector '[Ptr ()] ()
analyzePixelBufferSelector = mkSelector "analyzePixelBuffer:"

-- | @Selector@ for @beginAnalysisOfDecompressionSession:error:@
beginAnalysisOfDecompressionSession_errorSelector :: Selector '[Ptr (), Id NSError] Bool
beginAnalysisOfDecompressionSession_errorSelector = mkSelector "beginAnalysisOfDecompressionSession:error:"

-- | @Selector@ for @beginAnalysisOfCaptureDeviceInput:error:@
beginAnalysisOfCaptureDeviceInput_errorSelector :: Selector '[Id AVCaptureDeviceInput, Id NSError] Bool
beginAnalysisOfCaptureDeviceInput_errorSelector = mkSelector "beginAnalysisOfCaptureDeviceInput:error:"

-- | @Selector@ for @endAnalysis@
endAnalysisSelector :: Selector '[] ()
endAnalysisSelector = mkSelector "endAnalysis"

-- | @Selector@ for @continueStream@
continueStreamSelector :: Selector '[] ()
continueStreamSelector = mkSelector "continueStream"

-- | @Selector@ for @analysis@
analysisSelector :: Selector '[] (Id SCSensitivityAnalysis)
analysisSelector = mkSelector "analysis"

-- | @Selector@ for @analysisChangedHandler@
analysisChangedHandlerSelector :: Selector '[] (Ptr ())
analysisChangedHandlerSelector = mkSelector "analysisChangedHandler"

-- | @Selector@ for @setAnalysisChangedHandler:@
setAnalysisChangedHandlerSelector :: Selector '[Ptr ()] ()
setAnalysisChangedHandlerSelector = mkSelector "setAnalysisChangedHandler:"

