{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCStreamConfiguration
--
-- SCStreamConfiguration is an object that encapsulates the SCStream properties such as output width, height, pixelformat and others.
--
-- Generated bindings for @SCStreamConfiguration@.
module ObjC.ScreenCaptureKit.SCStreamConfiguration
  ( SCStreamConfiguration
  , IsSCStreamConfiguration(..)
  , streamConfigurationWithPreset
  , width
  , setWidth
  , height
  , setHeight
  , pixelFormat
  , setPixelFormat
  , scalesToFit
  , setScalesToFit
  , preservesAspectRatio
  , setPreservesAspectRatio
  , streamName
  , setStreamName
  , showsCursor
  , setShowsCursor
  , showMouseClicks
  , setShowMouseClicks
  , backgroundColor
  , setBackgroundColor
  , queueDepth
  , setQueueDepth
  , colorMatrix
  , setColorMatrix
  , colorSpaceName
  , setColorSpaceName
  , capturesAudio
  , setCapturesAudio
  , sampleRate
  , setSampleRate
  , channelCount
  , setChannelCount
  , excludesCurrentProcessAudio
  , setExcludesCurrentProcessAudio
  , ignoreShadowsDisplay
  , setIgnoreShadowsDisplay
  , ignoreShadowsSingleWindow
  , setIgnoreShadowsSingleWindow
  , captureResolution
  , setCaptureResolution
  , capturesShadowsOnly
  , setCapturesShadowsOnly
  , shouldBeOpaque
  , setShouldBeOpaque
  , ignoreGlobalClipDisplay
  , setIgnoreGlobalClipDisplay
  , ignoreGlobalClipSingleWindow
  , setIgnoreGlobalClipSingleWindow
  , presenterOverlayPrivacyAlertSetting
  , setPresenterOverlayPrivacyAlertSetting
  , includeChildWindows
  , setIncludeChildWindows
  , captureMicrophone
  , setCaptureMicrophone
  , microphoneCaptureDeviceID
  , setMicrophoneCaptureDeviceID
  , captureDynamicRange
  , setCaptureDynamicRange
  , backgroundColorSelector
  , captureDynamicRangeSelector
  , captureMicrophoneSelector
  , captureResolutionSelector
  , capturesAudioSelector
  , capturesShadowsOnlySelector
  , channelCountSelector
  , colorMatrixSelector
  , colorSpaceNameSelector
  , excludesCurrentProcessAudioSelector
  , heightSelector
  , ignoreGlobalClipDisplaySelector
  , ignoreGlobalClipSingleWindowSelector
  , ignoreShadowsDisplaySelector
  , ignoreShadowsSingleWindowSelector
  , includeChildWindowsSelector
  , microphoneCaptureDeviceIDSelector
  , pixelFormatSelector
  , presenterOverlayPrivacyAlertSettingSelector
  , preservesAspectRatioSelector
  , queueDepthSelector
  , sampleRateSelector
  , scalesToFitSelector
  , setBackgroundColorSelector
  , setCaptureDynamicRangeSelector
  , setCaptureMicrophoneSelector
  , setCaptureResolutionSelector
  , setCapturesAudioSelector
  , setCapturesShadowsOnlySelector
  , setChannelCountSelector
  , setColorMatrixSelector
  , setColorSpaceNameSelector
  , setExcludesCurrentProcessAudioSelector
  , setHeightSelector
  , setIgnoreGlobalClipDisplaySelector
  , setIgnoreGlobalClipSingleWindowSelector
  , setIgnoreShadowsDisplaySelector
  , setIgnoreShadowsSingleWindowSelector
  , setIncludeChildWindowsSelector
  , setMicrophoneCaptureDeviceIDSelector
  , setPixelFormatSelector
  , setPresenterOverlayPrivacyAlertSettingSelector
  , setPreservesAspectRatioSelector
  , setQueueDepthSelector
  , setSampleRateSelector
  , setScalesToFitSelector
  , setShouldBeOpaqueSelector
  , setShowMouseClicksSelector
  , setShowsCursorSelector
  , setStreamNameSelector
  , setWidthSelector
  , shouldBeOpaqueSelector
  , showMouseClicksSelector
  , showsCursorSelector
  , streamConfigurationWithPresetSelector
  , streamNameSelector
  , widthSelector

  -- * Enum types
  , SCCaptureDynamicRange(SCCaptureDynamicRange)
  , pattern SCCaptureDynamicRangeSDR
  , pattern SCCaptureDynamicRangeHDRLocalDisplay
  , pattern SCCaptureDynamicRangeHDRCanonicalDisplay
  , SCCaptureResolutionType(SCCaptureResolutionType)
  , pattern SCCaptureResolutionAutomatic
  , pattern SCCaptureResolutionBest
  , pattern SCCaptureResolutionNominal
  , SCPresenterOverlayAlertSetting(SCPresenterOverlayAlertSetting)
  , pattern SCPresenterOverlayAlertSettingSystem
  , pattern SCPresenterOverlayAlertSettingNever
  , pattern SCPresenterOverlayAlertSettingAlways
  , SCStreamConfigurationPreset(SCStreamConfigurationPreset)
  , pattern SCStreamConfigurationPresetCaptureHDRStreamLocalDisplay
  , pattern SCStreamConfigurationPresetCaptureHDRStreamCanonicalDisplay
  , pattern SCStreamConfigurationPresetCaptureHDRScreenshotLocalDisplay
  , pattern SCStreamConfigurationPresetCaptureHDRScreenshotCanonicalDisplay
  , pattern SCStreamConfigurationPresetCaptureHDRRecordingPreservedSDRHDR10

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | streamConfigurationWithPreset:
--
-- Returns an instance of SCStreamConfiguration corresponding to the given preset
--
-- @preset@ — The enum identifier for the desired preset
--
-- The SCStreamConfiguration of the returned object can be used as a guide for creating and configuring an SCStream. If all the suggested properties are respected in creating the SCStream, the resulting capture result will conform to the criteria implied by the preset.
--
-- ObjC selector: @+ streamConfigurationWithPreset:@
streamConfigurationWithPreset :: SCStreamConfigurationPreset -> IO (Id SCStreamConfiguration)
streamConfigurationWithPreset preset =
  do
    cls' <- getRequiredClass "SCStreamConfiguration"
    sendClassMessage cls' streamConfigurationWithPresetSelector preset

-- | SCStreamProperty for output width as measured in pixels. Default is set to 1920.
--
-- ObjC selector: @- width@
width :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CULong
width scStreamConfiguration =
  sendMessage scStreamConfiguration widthSelector

-- | SCStreamProperty for output width as measured in pixels. Default is set to 1920.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CULong -> IO ()
setWidth scStreamConfiguration value =
  sendMessage scStreamConfiguration setWidthSelector value

-- | SCStreamProperty for output height as measured in pixels. Default is set to 1080.
--
-- ObjC selector: @- height@
height :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CULong
height scStreamConfiguration =
  sendMessage scStreamConfiguration heightSelector

-- | SCStreamProperty for output height as measured in pixels. Default is set to 1080.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CULong -> IO ()
setHeight scStreamConfiguration value =
  sendMessage scStreamConfiguration setHeightSelector value

-- | SCStreamProperty for output pixel format. Supported pixel formats are: 'BGRA': Packed Little Endian ARGB8888 'l10r': Packed Little Endian ARGB2101010 '420v': 2-plane "video" range YCbCr 4:2:0 '420f': 2-plane "full" range YCbCr 4:2:0 'xf44': 2 plane "full" range YCbCr10 4:4:4 'RGhA': 64 bit RGBA IEEE half-precision float, 16-bit little-endian See https://developer.apple.com/documentation/coregraphics/1455170-cgdisplaystreamcreate
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CUInt
pixelFormat scStreamConfiguration =
  sendMessage scStreamConfiguration pixelFormatSelector

-- | SCStreamProperty for output pixel format. Supported pixel formats are: 'BGRA': Packed Little Endian ARGB8888 'l10r': Packed Little Endian ARGB2101010 '420v': 2-plane "video" range YCbCr 4:2:0 '420f': 2-plane "full" range YCbCr 4:2:0 'xf44': 2 plane "full" range YCbCr10 4:4:4 'RGhA': 64 bit RGBA IEEE half-precision float, 16-bit little-endian See https://developer.apple.com/documentation/coregraphics/1455170-cgdisplaystreamcreate
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CUInt -> IO ()
setPixelFormat scStreamConfiguration value =
  sendMessage scStreamConfiguration setPixelFormatSelector value

-- | SCStreamProperty for output to be always scaled to fit into the provided width and height. For use for independent window capture. When true, the output scales up and down. When false, the output only scales down.
--
-- ObjC selector: @- scalesToFit@
scalesToFit :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
scalesToFit scStreamConfiguration =
  sendMessage scStreamConfiguration scalesToFitSelector

-- | SCStreamProperty for output to be always scaled to fit into the provided width and height. For use for independent window capture. When true, the output scales up and down. When false, the output only scales down.
--
-- ObjC selector: @- setScalesToFit:@
setScalesToFit :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setScalesToFit scStreamConfiguration value =
  sendMessage scStreamConfiguration setScalesToFitSelector value

-- | SCStreamProperty that specifies whether the  stream preserves the aspect ratio of the source pixel data. By default the aspect ratio is preserved.
--
-- ObjC selector: @- preservesAspectRatio@
preservesAspectRatio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
preservesAspectRatio scStreamConfiguration =
  sendMessage scStreamConfiguration preservesAspectRatioSelector

-- | SCStreamProperty that specifies whether the  stream preserves the aspect ratio of the source pixel data. By default the aspect ratio is preserved.
--
-- ObjC selector: @- setPreservesAspectRatio:@
setPreservesAspectRatio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setPreservesAspectRatio scStreamConfiguration value =
  sendMessage scStreamConfiguration setPreservesAspectRatioSelector value

-- | SCStreamProperty the name of the stream
--
-- ObjC selector: @- streamName@
streamName :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO (Id NSString)
streamName scStreamConfiguration =
  sendMessage scStreamConfiguration streamNameSelector

-- | SCStreamProperty the name of the stream
--
-- ObjC selector: @- setStreamName:@
setStreamName :: (IsSCStreamConfiguration scStreamConfiguration, IsNSString value) => scStreamConfiguration -> value -> IO ()
setStreamName scStreamConfiguration value =
  sendMessage scStreamConfiguration setStreamNameSelector (toNSString value)

-- | SCStreamProperty that specifies whether the cursor should appear in the stream.  By default the cursor is visible.
--
-- ObjC selector: @- showsCursor@
showsCursor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
showsCursor scStreamConfiguration =
  sendMessage scStreamConfiguration showsCursorSelector

-- | SCStreamProperty that specifies whether the cursor should appear in the stream.  By default the cursor is visible.
--
-- ObjC selector: @- setShowsCursor:@
setShowsCursor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setShowsCursor scStreamConfiguration value =
  sendMessage scStreamConfiguration setShowsCursorSelector value

-- | SCStreamProperty that specifies whether to draw a circle around the cursor click, default is NO. This property will not be affected by showsCursor. This property currently applies when pixelFormat is set to BGRA.
--
-- ObjC selector: @- showMouseClicks@
showMouseClicks :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
showMouseClicks scStreamConfiguration =
  sendMessage scStreamConfiguration showMouseClicksSelector

-- | SCStreamProperty that specifies whether to draw a circle around the cursor click, default is NO. This property will not be affected by showsCursor. This property currently applies when pixelFormat is set to BGRA.
--
-- ObjC selector: @- setShowMouseClicks:@
setShowMouseClicks :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setShowMouseClicks scStreamConfiguration value =
  sendMessage scStreamConfiguration setShowMouseClicksSelector value

-- | SCStreamProperty for background color. By default the background color is clear.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO (Ptr ())
backgroundColor scStreamConfiguration =
  sendMessage scStreamConfiguration backgroundColorSelector

-- | SCStreamProperty for background color. By default the background color is clear.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Ptr () -> IO ()
setBackgroundColor scStreamConfiguration value =
  sendMessage scStreamConfiguration setBackgroundColorSelector value

-- | SCStreamProperty that specifies the number of frames to keep in the queue.  If not set the default value is 8 frames.  Specifying more frames uses more memory, but may allow you to process frame data without stalling the display stream and should not exceed 8 frames.
--
-- ObjC selector: @- queueDepth@
queueDepth :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CLong
queueDepth scStreamConfiguration =
  sendMessage scStreamConfiguration queueDepthSelector

-- | SCStreamProperty that specifies the number of frames to keep in the queue.  If not set the default value is 8 frames.  Specifying more frames uses more memory, but may allow you to process frame data without stalling the display stream and should not exceed 8 frames.
--
-- ObjC selector: @- setQueueDepth:@
setQueueDepth :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CLong -> IO ()
setQueueDepth scStreamConfiguration value =
  sendMessage scStreamConfiguration setQueueDepthSelector value

-- | SCStreamProperty that specifies the YCbCr matrix applied to the output surface.  The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/quartz_display_services/display_stream_ycbcr_to_rgb_conversion_matrix_options. Should only be used if your pixel format is 420v or 420f.
--
-- ObjC selector: @- colorMatrix@
colorMatrix :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO RawId
colorMatrix scStreamConfiguration =
  sendMessage scStreamConfiguration colorMatrixSelector

-- | SCStreamProperty that specifies the YCbCr matrix applied to the output surface.  The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/quartz_display_services/display_stream_ycbcr_to_rgb_conversion_matrix_options. Should only be used if your pixel format is 420v or 420f.
--
-- ObjC selector: @- setColorMatrix:@
setColorMatrix :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> RawId -> IO ()
setColorMatrix scStreamConfiguration value =
  sendMessage scStreamConfiguration setColorMatrixSelector value

-- | SCStreamProperty that specifies the color space of the output buffer.  If not set the output buffer uses the same color space as the display. The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/cgcolorspace/color_space_names.
--
-- ObjC selector: @- colorSpaceName@
colorSpaceName :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO RawId
colorSpaceName scStreamConfiguration =
  sendMessage scStreamConfiguration colorSpaceNameSelector

-- | SCStreamProperty that specifies the color space of the output buffer.  If not set the output buffer uses the same color space as the display. The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/cgcolorspace/color_space_names.
--
-- ObjC selector: @- setColorSpaceName:@
setColorSpaceName :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> RawId -> IO ()
setColorSpaceName scStreamConfiguration value =
  sendMessage scStreamConfiguration setColorSpaceNameSelector value

-- | SCStreamProperty that specifies whether the audio will be captured.  By default audio is not captured.
--
-- ObjC selector: @- capturesAudio@
capturesAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
capturesAudio scStreamConfiguration =
  sendMessage scStreamConfiguration capturesAudioSelector

-- | SCStreamProperty that specifies whether the audio will be captured.  By default audio is not captured.
--
-- ObjC selector: @- setCapturesAudio:@
setCapturesAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setCapturesAudio scStreamConfiguration value =
  sendMessage scStreamConfiguration setCapturesAudioSelector value

-- | SCStreamProperty to specify the sample rate for audio. Default is set to 48000.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CLong
sampleRate scStreamConfiguration =
  sendMessage scStreamConfiguration sampleRateSelector

-- | SCStreamProperty to specify the sample rate for audio. Default is set to 48000.
--
-- ObjC selector: @- setSampleRate:@
setSampleRate :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CLong -> IO ()
setSampleRate scStreamConfiguration value =
  sendMessage scStreamConfiguration setSampleRateSelector value

-- | SCStreamProperty to specify channel count. Default is set to two.
--
-- ObjC selector: @- channelCount@
channelCount :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CLong
channelCount scStreamConfiguration =
  sendMessage scStreamConfiguration channelCountSelector

-- | SCStreamProperty to specify channel count. Default is set to two.
--
-- ObjC selector: @- setChannelCount:@
setChannelCount :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CLong -> IO ()
setChannelCount scStreamConfiguration value =
  sendMessage scStreamConfiguration setChannelCountSelector value

-- | SCAudioProperty whether to exclude audio from current process. Default is set to NO.
--
-- ObjC selector: @- excludesCurrentProcessAudio@
excludesCurrentProcessAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
excludesCurrentProcessAudio scStreamConfiguration =
  sendMessage scStreamConfiguration excludesCurrentProcessAudioSelector

-- | SCAudioProperty whether to exclude audio from current process. Default is set to NO.
--
-- ObjC selector: @- setExcludesCurrentProcessAudio:@
setExcludesCurrentProcessAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setExcludesCurrentProcessAudio scStreamConfiguration value =
  sendMessage scStreamConfiguration setExcludesCurrentProcessAudioSelector value

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreShadowsDisplay@
ignoreShadowsDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreShadowsDisplay scStreamConfiguration =
  sendMessage scStreamConfiguration ignoreShadowsDisplaySelector

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreShadowsDisplay:@
setIgnoreShadowsDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreShadowsDisplay scStreamConfiguration value =
  sendMessage scStreamConfiguration setIgnoreShadowsDisplaySelector value

-- | SCStreamProperty to ignore framing on windows in the single window sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreShadowsSingleWindow@
ignoreShadowsSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreShadowsSingleWindow scStreamConfiguration =
  sendMessage scStreamConfiguration ignoreShadowsSingleWindowSelector

-- | SCStreamProperty to ignore framing on windows in the single window sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreShadowsSingleWindow:@
setIgnoreShadowsSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreShadowsSingleWindow scStreamConfiguration value =
  sendMessage scStreamConfiguration setIgnoreShadowsSingleWindowSelector value

-- | captureResolution Choose between automatic, best, and nominal.
--
-- ObjC selector: @- captureResolution@
captureResolution :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO SCCaptureResolutionType
captureResolution scStreamConfiguration =
  sendMessage scStreamConfiguration captureResolutionSelector

-- | captureResolution Choose between automatic, best, and nominal.
--
-- ObjC selector: @- setCaptureResolution:@
setCaptureResolution :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> SCCaptureResolutionType -> IO ()
setCaptureResolution scStreamConfiguration value =
  sendMessage scStreamConfiguration setCaptureResolutionSelector value

-- | SCStreamProperty to capture only the shadows of windows.
--
-- ObjC selector: @- capturesShadowsOnly@
capturesShadowsOnly :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
capturesShadowsOnly scStreamConfiguration =
  sendMessage scStreamConfiguration capturesShadowsOnlySelector

-- | SCStreamProperty to capture only the shadows of windows.
--
-- ObjC selector: @- setCapturesShadowsOnly:@
setCapturesShadowsOnly :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setCapturesShadowsOnly scStreamConfiguration value =
  sendMessage scStreamConfiguration setCapturesShadowsOnlySelector value

-- | SCStreamProperty to ensure partially transparent areas on windows are backed by a solid white color so that the resulting image is fully opaque.
--
-- ObjC selector: @- shouldBeOpaque@
shouldBeOpaque :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
shouldBeOpaque scStreamConfiguration =
  sendMessage scStreamConfiguration shouldBeOpaqueSelector

-- | SCStreamProperty to ensure partially transparent areas on windows are backed by a solid white color so that the resulting image is fully opaque.
--
-- ObjC selector: @- setShouldBeOpaque:@
setShouldBeOpaque :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setShouldBeOpaque scStreamConfiguration value =
  sendMessage scStreamConfiguration setShouldBeOpaqueSelector value

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreGlobalClipDisplay@
ignoreGlobalClipDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreGlobalClipDisplay scStreamConfiguration =
  sendMessage scStreamConfiguration ignoreGlobalClipDisplaySelector

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreGlobalClipDisplay:@
setIgnoreGlobalClipDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreGlobalClipDisplay scStreamConfiguration value =
  sendMessage scStreamConfiguration setIgnoreGlobalClipDisplaySelector value

-- | SCStreamProperty to ignore global clipping when on single window share. When set to true, single window captures that are partially off the screen will not be clipped. (will ignore window placement in display context).
--
-- ObjC selector: @- ignoreGlobalClipSingleWindow@
ignoreGlobalClipSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreGlobalClipSingleWindow scStreamConfiguration =
  sendMessage scStreamConfiguration ignoreGlobalClipSingleWindowSelector

-- | SCStreamProperty to ignore global clipping when on single window share. When set to true, single window captures that are partially off the screen will not be clipped. (will ignore window placement in display context).
--
-- ObjC selector: @- setIgnoreGlobalClipSingleWindow:@
setIgnoreGlobalClipSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreGlobalClipSingleWindow scStreamConfiguration value =
  sendMessage scStreamConfiguration setIgnoreGlobalClipSingleWindowSelector value

-- | SCStreamProperty that informs the system if a privacy alert should be shown when using presenter overlay for a stream. Defaults to SCPresenterOverlayAlertSettingSystem;
--
-- ObjC selector: @- presenterOverlayPrivacyAlertSetting@
presenterOverlayPrivacyAlertSetting :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO SCPresenterOverlayAlertSetting
presenterOverlayPrivacyAlertSetting scStreamConfiguration =
  sendMessage scStreamConfiguration presenterOverlayPrivacyAlertSettingSelector

-- | SCStreamProperty that informs the system if a privacy alert should be shown when using presenter overlay for a stream. Defaults to SCPresenterOverlayAlertSettingSystem;
--
-- ObjC selector: @- setPresenterOverlayPrivacyAlertSetting:@
setPresenterOverlayPrivacyAlertSetting :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> SCPresenterOverlayAlertSetting -> IO ()
setPresenterOverlayPrivacyAlertSetting scStreamConfiguration value =
  sendMessage scStreamConfiguration setPresenterOverlayPrivacyAlertSettingSelector value

-- | SCStreamProperty to show the child windows in display bound windows and applications sharing.  Child windows are included by default.
--
-- ObjC selector: @- includeChildWindows@
includeChildWindows :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
includeChildWindows scStreamConfiguration =
  sendMessage scStreamConfiguration includeChildWindowsSelector

-- | SCStreamProperty to show the child windows in display bound windows and applications sharing.  Child windows are included by default.
--
-- ObjC selector: @- setIncludeChildWindows:@
setIncludeChildWindows :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIncludeChildWindows scStreamConfiguration value =
  sendMessage scStreamConfiguration setIncludeChildWindowsSelector value

-- | SCStreamProperty that specifies whether the microphone audio will be captured.  By default microphone is not captured.
--
-- ObjC selector: @- captureMicrophone@
captureMicrophone :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
captureMicrophone scStreamConfiguration =
  sendMessage scStreamConfiguration captureMicrophoneSelector

-- | SCStreamProperty that specifies whether the microphone audio will be captured.  By default microphone is not captured.
--
-- ObjC selector: @- setCaptureMicrophone:@
setCaptureMicrophone :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setCaptureMicrophone scStreamConfiguration value =
  sendMessage scStreamConfiguration setCaptureMicrophoneSelector value

-- | SCStreamProperty that specifies which microphone device to capture. This deviceID is the uniqueID from AVCaptureDevice for the microphone. System Default Microphone will be used if not specified by client. For Mac Catalyst apps, the System Default Microphone will be captured.
--
-- ObjC selector: @- microphoneCaptureDeviceID@
microphoneCaptureDeviceID :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO (Id NSString)
microphoneCaptureDeviceID scStreamConfiguration =
  sendMessage scStreamConfiguration microphoneCaptureDeviceIDSelector

-- | SCStreamProperty that specifies which microphone device to capture. This deviceID is the uniqueID from AVCaptureDevice for the microphone. System Default Microphone will be used if not specified by client. For Mac Catalyst apps, the System Default Microphone will be captured.
--
-- ObjC selector: @- setMicrophoneCaptureDeviceID:@
setMicrophoneCaptureDeviceID :: (IsSCStreamConfiguration scStreamConfiguration, IsNSString value) => scStreamConfiguration -> value -> IO ()
setMicrophoneCaptureDeviceID scStreamConfiguration value =
  sendMessage scStreamConfiguration setMicrophoneCaptureDeviceIDSelector (toNSString value)

-- | SCStreamProperty client will choose captureDynamicRange between SCCaptureDynamicRangeSDR, SCCaptureDynamicRangeHDRLocalDisplay,  SCCaptureDynamicRangeHDRCanonicalDisplay. By default, the stream is capturing with SCCaptureDynamicRangeSDR. HDR capture is only supported with Apple Silicon Mac, setting this property on Intel Mac will have no effect. HDR recording is not support yet, adding a recording output to a stream with SCCaptureDynamicRangeHDR set will fail.
--
-- ObjC selector: @- captureDynamicRange@
captureDynamicRange :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO SCCaptureDynamicRange
captureDynamicRange scStreamConfiguration =
  sendMessage scStreamConfiguration captureDynamicRangeSelector

-- | SCStreamProperty client will choose captureDynamicRange between SCCaptureDynamicRangeSDR, SCCaptureDynamicRangeHDRLocalDisplay,  SCCaptureDynamicRangeHDRCanonicalDisplay. By default, the stream is capturing with SCCaptureDynamicRangeSDR. HDR capture is only supported with Apple Silicon Mac, setting this property on Intel Mac will have no effect. HDR recording is not support yet, adding a recording output to a stream with SCCaptureDynamicRangeHDR set will fail.
--
-- ObjC selector: @- setCaptureDynamicRange:@
setCaptureDynamicRange :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> SCCaptureDynamicRange -> IO ()
setCaptureDynamicRange scStreamConfiguration value =
  sendMessage scStreamConfiguration setCaptureDynamicRangeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamConfigurationWithPreset:@
streamConfigurationWithPresetSelector :: Selector '[SCStreamConfigurationPreset] (Id SCStreamConfiguration)
streamConfigurationWithPresetSelector = mkSelector "streamConfigurationWithPreset:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CULong
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CULong] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CULong
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CULong] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] CUInt
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector '[CUInt] ()
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @scalesToFit@
scalesToFitSelector :: Selector '[] Bool
scalesToFitSelector = mkSelector "scalesToFit"

-- | @Selector@ for @setScalesToFit:@
setScalesToFitSelector :: Selector '[Bool] ()
setScalesToFitSelector = mkSelector "setScalesToFit:"

-- | @Selector@ for @preservesAspectRatio@
preservesAspectRatioSelector :: Selector '[] Bool
preservesAspectRatioSelector = mkSelector "preservesAspectRatio"

-- | @Selector@ for @setPreservesAspectRatio:@
setPreservesAspectRatioSelector :: Selector '[Bool] ()
setPreservesAspectRatioSelector = mkSelector "setPreservesAspectRatio:"

-- | @Selector@ for @streamName@
streamNameSelector :: Selector '[] (Id NSString)
streamNameSelector = mkSelector "streamName"

-- | @Selector@ for @setStreamName:@
setStreamNameSelector :: Selector '[Id NSString] ()
setStreamNameSelector = mkSelector "setStreamName:"

-- | @Selector@ for @showsCursor@
showsCursorSelector :: Selector '[] Bool
showsCursorSelector = mkSelector "showsCursor"

-- | @Selector@ for @setShowsCursor:@
setShowsCursorSelector :: Selector '[Bool] ()
setShowsCursorSelector = mkSelector "setShowsCursor:"

-- | @Selector@ for @showMouseClicks@
showMouseClicksSelector :: Selector '[] Bool
showMouseClicksSelector = mkSelector "showMouseClicks"

-- | @Selector@ for @setShowMouseClicks:@
setShowMouseClicksSelector :: Selector '[Bool] ()
setShowMouseClicksSelector = mkSelector "setShowMouseClicks:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Ptr ())
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Ptr ()] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @queueDepth@
queueDepthSelector :: Selector '[] CLong
queueDepthSelector = mkSelector "queueDepth"

-- | @Selector@ for @setQueueDepth:@
setQueueDepthSelector :: Selector '[CLong] ()
setQueueDepthSelector = mkSelector "setQueueDepth:"

-- | @Selector@ for @colorMatrix@
colorMatrixSelector :: Selector '[] RawId
colorMatrixSelector = mkSelector "colorMatrix"

-- | @Selector@ for @setColorMatrix:@
setColorMatrixSelector :: Selector '[RawId] ()
setColorMatrixSelector = mkSelector "setColorMatrix:"

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector '[] RawId
colorSpaceNameSelector = mkSelector "colorSpaceName"

-- | @Selector@ for @setColorSpaceName:@
setColorSpaceNameSelector :: Selector '[RawId] ()
setColorSpaceNameSelector = mkSelector "setColorSpaceName:"

-- | @Selector@ for @capturesAudio@
capturesAudioSelector :: Selector '[] Bool
capturesAudioSelector = mkSelector "capturesAudio"

-- | @Selector@ for @setCapturesAudio:@
setCapturesAudioSelector :: Selector '[Bool] ()
setCapturesAudioSelector = mkSelector "setCapturesAudio:"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector '[] CLong
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @setSampleRate:@
setSampleRateSelector :: Selector '[CLong] ()
setSampleRateSelector = mkSelector "setSampleRate:"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] CLong
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @setChannelCount:@
setChannelCountSelector :: Selector '[CLong] ()
setChannelCountSelector = mkSelector "setChannelCount:"

-- | @Selector@ for @excludesCurrentProcessAudio@
excludesCurrentProcessAudioSelector :: Selector '[] Bool
excludesCurrentProcessAudioSelector = mkSelector "excludesCurrentProcessAudio"

-- | @Selector@ for @setExcludesCurrentProcessAudio:@
setExcludesCurrentProcessAudioSelector :: Selector '[Bool] ()
setExcludesCurrentProcessAudioSelector = mkSelector "setExcludesCurrentProcessAudio:"

-- | @Selector@ for @ignoreShadowsDisplay@
ignoreShadowsDisplaySelector :: Selector '[] Bool
ignoreShadowsDisplaySelector = mkSelector "ignoreShadowsDisplay"

-- | @Selector@ for @setIgnoreShadowsDisplay:@
setIgnoreShadowsDisplaySelector :: Selector '[Bool] ()
setIgnoreShadowsDisplaySelector = mkSelector "setIgnoreShadowsDisplay:"

-- | @Selector@ for @ignoreShadowsSingleWindow@
ignoreShadowsSingleWindowSelector :: Selector '[] Bool
ignoreShadowsSingleWindowSelector = mkSelector "ignoreShadowsSingleWindow"

-- | @Selector@ for @setIgnoreShadowsSingleWindow:@
setIgnoreShadowsSingleWindowSelector :: Selector '[Bool] ()
setIgnoreShadowsSingleWindowSelector = mkSelector "setIgnoreShadowsSingleWindow:"

-- | @Selector@ for @captureResolution@
captureResolutionSelector :: Selector '[] SCCaptureResolutionType
captureResolutionSelector = mkSelector "captureResolution"

-- | @Selector@ for @setCaptureResolution:@
setCaptureResolutionSelector :: Selector '[SCCaptureResolutionType] ()
setCaptureResolutionSelector = mkSelector "setCaptureResolution:"

-- | @Selector@ for @capturesShadowsOnly@
capturesShadowsOnlySelector :: Selector '[] Bool
capturesShadowsOnlySelector = mkSelector "capturesShadowsOnly"

-- | @Selector@ for @setCapturesShadowsOnly:@
setCapturesShadowsOnlySelector :: Selector '[Bool] ()
setCapturesShadowsOnlySelector = mkSelector "setCapturesShadowsOnly:"

-- | @Selector@ for @shouldBeOpaque@
shouldBeOpaqueSelector :: Selector '[] Bool
shouldBeOpaqueSelector = mkSelector "shouldBeOpaque"

-- | @Selector@ for @setShouldBeOpaque:@
setShouldBeOpaqueSelector :: Selector '[Bool] ()
setShouldBeOpaqueSelector = mkSelector "setShouldBeOpaque:"

-- | @Selector@ for @ignoreGlobalClipDisplay@
ignoreGlobalClipDisplaySelector :: Selector '[] Bool
ignoreGlobalClipDisplaySelector = mkSelector "ignoreGlobalClipDisplay"

-- | @Selector@ for @setIgnoreGlobalClipDisplay:@
setIgnoreGlobalClipDisplaySelector :: Selector '[Bool] ()
setIgnoreGlobalClipDisplaySelector = mkSelector "setIgnoreGlobalClipDisplay:"

-- | @Selector@ for @ignoreGlobalClipSingleWindow@
ignoreGlobalClipSingleWindowSelector :: Selector '[] Bool
ignoreGlobalClipSingleWindowSelector = mkSelector "ignoreGlobalClipSingleWindow"

-- | @Selector@ for @setIgnoreGlobalClipSingleWindow:@
setIgnoreGlobalClipSingleWindowSelector :: Selector '[Bool] ()
setIgnoreGlobalClipSingleWindowSelector = mkSelector "setIgnoreGlobalClipSingleWindow:"

-- | @Selector@ for @presenterOverlayPrivacyAlertSetting@
presenterOverlayPrivacyAlertSettingSelector :: Selector '[] SCPresenterOverlayAlertSetting
presenterOverlayPrivacyAlertSettingSelector = mkSelector "presenterOverlayPrivacyAlertSetting"

-- | @Selector@ for @setPresenterOverlayPrivacyAlertSetting:@
setPresenterOverlayPrivacyAlertSettingSelector :: Selector '[SCPresenterOverlayAlertSetting] ()
setPresenterOverlayPrivacyAlertSettingSelector = mkSelector "setPresenterOverlayPrivacyAlertSetting:"

-- | @Selector@ for @includeChildWindows@
includeChildWindowsSelector :: Selector '[] Bool
includeChildWindowsSelector = mkSelector "includeChildWindows"

-- | @Selector@ for @setIncludeChildWindows:@
setIncludeChildWindowsSelector :: Selector '[Bool] ()
setIncludeChildWindowsSelector = mkSelector "setIncludeChildWindows:"

-- | @Selector@ for @captureMicrophone@
captureMicrophoneSelector :: Selector '[] Bool
captureMicrophoneSelector = mkSelector "captureMicrophone"

-- | @Selector@ for @setCaptureMicrophone:@
setCaptureMicrophoneSelector :: Selector '[Bool] ()
setCaptureMicrophoneSelector = mkSelector "setCaptureMicrophone:"

-- | @Selector@ for @microphoneCaptureDeviceID@
microphoneCaptureDeviceIDSelector :: Selector '[] (Id NSString)
microphoneCaptureDeviceIDSelector = mkSelector "microphoneCaptureDeviceID"

-- | @Selector@ for @setMicrophoneCaptureDeviceID:@
setMicrophoneCaptureDeviceIDSelector :: Selector '[Id NSString] ()
setMicrophoneCaptureDeviceIDSelector = mkSelector "setMicrophoneCaptureDeviceID:"

-- | @Selector@ for @captureDynamicRange@
captureDynamicRangeSelector :: Selector '[] SCCaptureDynamicRange
captureDynamicRangeSelector = mkSelector "captureDynamicRange"

-- | @Selector@ for @setCaptureDynamicRange:@
setCaptureDynamicRangeSelector :: Selector '[SCCaptureDynamicRange] ()
setCaptureDynamicRangeSelector = mkSelector "setCaptureDynamicRange:"

