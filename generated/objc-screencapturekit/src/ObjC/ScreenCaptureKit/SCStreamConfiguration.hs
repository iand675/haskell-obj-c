{-# LANGUAGE PatternSynonyms #-}
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
  , captureDynamicRange
  , setCaptureDynamicRange
  , streamConfigurationWithPresetSelector
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , pixelFormatSelector
  , setPixelFormatSelector
  , scalesToFitSelector
  , setScalesToFitSelector
  , preservesAspectRatioSelector
  , setPreservesAspectRatioSelector
  , showsCursorSelector
  , setShowsCursorSelector
  , showMouseClicksSelector
  , setShowMouseClicksSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , queueDepthSelector
  , setQueueDepthSelector
  , colorMatrixSelector
  , setColorMatrixSelector
  , colorSpaceNameSelector
  , setColorSpaceNameSelector
  , capturesAudioSelector
  , setCapturesAudioSelector
  , sampleRateSelector
  , setSampleRateSelector
  , channelCountSelector
  , setChannelCountSelector
  , excludesCurrentProcessAudioSelector
  , setExcludesCurrentProcessAudioSelector
  , ignoreShadowsDisplaySelector
  , setIgnoreShadowsDisplaySelector
  , ignoreShadowsSingleWindowSelector
  , setIgnoreShadowsSingleWindowSelector
  , captureResolutionSelector
  , setCaptureResolutionSelector
  , capturesShadowsOnlySelector
  , setCapturesShadowsOnlySelector
  , shouldBeOpaqueSelector
  , setShouldBeOpaqueSelector
  , ignoreGlobalClipDisplaySelector
  , setIgnoreGlobalClipDisplaySelector
  , ignoreGlobalClipSingleWindowSelector
  , setIgnoreGlobalClipSingleWindowSelector
  , presenterOverlayPrivacyAlertSettingSelector
  , setPresenterOverlayPrivacyAlertSettingSelector
  , includeChildWindowsSelector
  , setIncludeChildWindowsSelector
  , captureMicrophoneSelector
  , setCaptureMicrophoneSelector
  , captureDynamicRangeSelector
  , setCaptureDynamicRangeSelector

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
    sendClassMsg cls' (mkSelector "streamConfigurationWithPreset:") (retPtr retVoid) [argCLong (coerce preset)] >>= retainedObject . castPtr

-- | SCStreamProperty for output width as measured in pixels. Default is set to 1920.
--
-- ObjC selector: @- width@
width :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CULong
width scStreamConfiguration  =
  sendMsg scStreamConfiguration (mkSelector "width") retCULong []

-- | SCStreamProperty for output width as measured in pixels. Default is set to 1920.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CULong -> IO ()
setWidth scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setWidth:") retVoid [argCULong (fromIntegral value)]

-- | SCStreamProperty for output height as measured in pixels. Default is set to 1080.
--
-- ObjC selector: @- height@
height :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CULong
height scStreamConfiguration  =
  sendMsg scStreamConfiguration (mkSelector "height") retCULong []

-- | SCStreamProperty for output height as measured in pixels. Default is set to 1080.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CULong -> IO ()
setHeight scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setHeight:") retVoid [argCULong (fromIntegral value)]

-- | SCStreamProperty for output pixel format. Supported pixel formats are: 'BGRA': Packed Little Endian ARGB8888 'l10r': Packed Little Endian ARGB2101010 '420v': 2-plane "video" range YCbCr 4:2:0 '420f': 2-plane "full" range YCbCr 4:2:0 'xf44': 2 plane "full" range YCbCr10 4:4:4 'RGhA': 64 bit RGBA IEEE half-precision float, 16-bit little-endian See https://developer.apple.com/documentation/coregraphics/1455170-cgdisplaystreamcreate
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CUInt
pixelFormat scStreamConfiguration  =
  sendMsg scStreamConfiguration (mkSelector "pixelFormat") retCUInt []

-- | SCStreamProperty for output pixel format. Supported pixel formats are: 'BGRA': Packed Little Endian ARGB8888 'l10r': Packed Little Endian ARGB2101010 '420v': 2-plane "video" range YCbCr 4:2:0 '420f': 2-plane "full" range YCbCr 4:2:0 'xf44': 2 plane "full" range YCbCr10 4:4:4 'RGhA': 64 bit RGBA IEEE half-precision float, 16-bit little-endian See https://developer.apple.com/documentation/coregraphics/1455170-cgdisplaystreamcreate
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CUInt -> IO ()
setPixelFormat scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setPixelFormat:") retVoid [argCUInt (fromIntegral value)]

-- | SCStreamProperty for output to be always scaled to fit into the provided width and height. For use for independent window capture. When true, the output scales up and down. When false, the output only scales down.
--
-- ObjC selector: @- scalesToFit@
scalesToFit :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
scalesToFit scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "scalesToFit") retCULong []

-- | SCStreamProperty for output to be always scaled to fit into the provided width and height. For use for independent window capture. When true, the output scales up and down. When false, the output only scales down.
--
-- ObjC selector: @- setScalesToFit:@
setScalesToFit :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setScalesToFit scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setScalesToFit:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty that specifies whether the  stream preserves the aspect ratio of the source pixel data. By default the aspect ratio is preserved.
--
-- ObjC selector: @- preservesAspectRatio@
preservesAspectRatio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
preservesAspectRatio scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "preservesAspectRatio") retCULong []

-- | SCStreamProperty that specifies whether the  stream preserves the aspect ratio of the source pixel data. By default the aspect ratio is preserved.
--
-- ObjC selector: @- setPreservesAspectRatio:@
setPreservesAspectRatio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setPreservesAspectRatio scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setPreservesAspectRatio:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty that specifies whether the cursor should appear in the stream.  By default the cursor is visible.
--
-- ObjC selector: @- showsCursor@
showsCursor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
showsCursor scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "showsCursor") retCULong []

-- | SCStreamProperty that specifies whether the cursor should appear in the stream.  By default the cursor is visible.
--
-- ObjC selector: @- setShowsCursor:@
setShowsCursor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setShowsCursor scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setShowsCursor:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty that specifies whether to draw a circle around the cursor click, default is NO. This property will not be affected by showsCursor. This property currently applies when pixelFormat is set to BGRA.
--
-- ObjC selector: @- showMouseClicks@
showMouseClicks :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
showMouseClicks scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "showMouseClicks") retCULong []

-- | SCStreamProperty that specifies whether to draw a circle around the cursor click, default is NO. This property will not be affected by showsCursor. This property currently applies when pixelFormat is set to BGRA.
--
-- ObjC selector: @- setShowMouseClicks:@
setShowMouseClicks :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setShowMouseClicks scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setShowMouseClicks:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty for background color. By default the background color is clear.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO (Ptr ())
backgroundColor scStreamConfiguration  =
  fmap castPtr $ sendMsg scStreamConfiguration (mkSelector "backgroundColor") (retPtr retVoid) []

-- | SCStreamProperty for background color. By default the background color is clear.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Ptr () -> IO ()
setBackgroundColor scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setBackgroundColor:") retVoid [argPtr value]

-- | SCStreamProperty that specifies the number of frames to keep in the queue.  If not set the default value is 8 frames.  Specifying more frames uses more memory, but may allow you to process frame data without stalling the display stream and should not exceed 8 frames.
--
-- ObjC selector: @- queueDepth@
queueDepth :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CLong
queueDepth scStreamConfiguration  =
  sendMsg scStreamConfiguration (mkSelector "queueDepth") retCLong []

-- | SCStreamProperty that specifies the number of frames to keep in the queue.  If not set the default value is 8 frames.  Specifying more frames uses more memory, but may allow you to process frame data without stalling the display stream and should not exceed 8 frames.
--
-- ObjC selector: @- setQueueDepth:@
setQueueDepth :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CLong -> IO ()
setQueueDepth scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setQueueDepth:") retVoid [argCLong (fromIntegral value)]

-- | SCStreamProperty that specifies the YCbCr matrix applied to the output surface.  The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/quartz_display_services/display_stream_ycbcr_to_rgb_conversion_matrix_options. Should only be used if your pixel format is 420v or 420f.
--
-- ObjC selector: @- colorMatrix@
colorMatrix :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO RawId
colorMatrix scStreamConfiguration  =
  fmap (RawId . castPtr) $ sendMsg scStreamConfiguration (mkSelector "colorMatrix") (retPtr retVoid) []

-- | SCStreamProperty that specifies the YCbCr matrix applied to the output surface.  The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/quartz_display_services/display_stream_ycbcr_to_rgb_conversion_matrix_options. Should only be used if your pixel format is 420v or 420f.
--
-- ObjC selector: @- setColorMatrix:@
setColorMatrix :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> RawId -> IO ()
setColorMatrix scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setColorMatrix:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | SCStreamProperty that specifies the color space of the output buffer.  If not set the output buffer uses the same color space as the display. The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/cgcolorspace/color_space_names.
--
-- ObjC selector: @- colorSpaceName@
colorSpaceName :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO RawId
colorSpaceName scStreamConfiguration  =
  fmap (RawId . castPtr) $ sendMsg scStreamConfiguration (mkSelector "colorSpaceName") (retPtr retVoid) []

-- | SCStreamProperty that specifies the color space of the output buffer.  If not set the output buffer uses the same color space as the display. The value must be one of the strings specified in https://developer.apple.com/documentation/coregraphics/cgcolorspace/color_space_names.
--
-- ObjC selector: @- setColorSpaceName:@
setColorSpaceName :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> RawId -> IO ()
setColorSpaceName scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setColorSpaceName:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | SCStreamProperty that specifies whether the audio will be captured.  By default audio is not captured.
--
-- ObjC selector: @- capturesAudio@
capturesAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
capturesAudio scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "capturesAudio") retCULong []

-- | SCStreamProperty that specifies whether the audio will be captured.  By default audio is not captured.
--
-- ObjC selector: @- setCapturesAudio:@
setCapturesAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setCapturesAudio scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setCapturesAudio:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty to specify the sample rate for audio. Default is set to 48000.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CLong
sampleRate scStreamConfiguration  =
  sendMsg scStreamConfiguration (mkSelector "sampleRate") retCLong []

-- | SCStreamProperty to specify the sample rate for audio. Default is set to 48000.
--
-- ObjC selector: @- setSampleRate:@
setSampleRate :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CLong -> IO ()
setSampleRate scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setSampleRate:") retVoid [argCLong (fromIntegral value)]

-- | SCStreamProperty to specify channel count. Default is set to two.
--
-- ObjC selector: @- channelCount@
channelCount :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO CLong
channelCount scStreamConfiguration  =
  sendMsg scStreamConfiguration (mkSelector "channelCount") retCLong []

-- | SCStreamProperty to specify channel count. Default is set to two.
--
-- ObjC selector: @- setChannelCount:@
setChannelCount :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> CLong -> IO ()
setChannelCount scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setChannelCount:") retVoid [argCLong (fromIntegral value)]

-- | SCAudioProperty whether to exclude audio from current process. Default is set to NO.
--
-- ObjC selector: @- excludesCurrentProcessAudio@
excludesCurrentProcessAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
excludesCurrentProcessAudio scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "excludesCurrentProcessAudio") retCULong []

-- | SCAudioProperty whether to exclude audio from current process. Default is set to NO.
--
-- ObjC selector: @- setExcludesCurrentProcessAudio:@
setExcludesCurrentProcessAudio :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setExcludesCurrentProcessAudio scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setExcludesCurrentProcessAudio:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreShadowsDisplay@
ignoreShadowsDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreShadowsDisplay scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "ignoreShadowsDisplay") retCULong []

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreShadowsDisplay:@
setIgnoreShadowsDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreShadowsDisplay scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setIgnoreShadowsDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty to ignore framing on windows in the single window sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreShadowsSingleWindow@
ignoreShadowsSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreShadowsSingleWindow scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "ignoreShadowsSingleWindow") retCULong []

-- | SCStreamProperty to ignore framing on windows in the single window sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreShadowsSingleWindow:@
setIgnoreShadowsSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreShadowsSingleWindow scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setIgnoreShadowsSingleWindow:") retVoid [argCULong (if value then 1 else 0)]

-- | captureResolution Choose between automatic, best, and nominal.
--
-- ObjC selector: @- captureResolution@
captureResolution :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO SCCaptureResolutionType
captureResolution scStreamConfiguration  =
  fmap (coerce :: CLong -> SCCaptureResolutionType) $ sendMsg scStreamConfiguration (mkSelector "captureResolution") retCLong []

-- | captureResolution Choose between automatic, best, and nominal.
--
-- ObjC selector: @- setCaptureResolution:@
setCaptureResolution :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> SCCaptureResolutionType -> IO ()
setCaptureResolution scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setCaptureResolution:") retVoid [argCLong (coerce value)]

-- | SCStreamProperty to capture only the shadows of windows.
--
-- ObjC selector: @- capturesShadowsOnly@
capturesShadowsOnly :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
capturesShadowsOnly scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "capturesShadowsOnly") retCULong []

-- | SCStreamProperty to capture only the shadows of windows.
--
-- ObjC selector: @- setCapturesShadowsOnly:@
setCapturesShadowsOnly :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setCapturesShadowsOnly scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setCapturesShadowsOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty to ensure partially transparent areas on windows are backed by a solid white color so that the resulting image is fully opaque.
--
-- ObjC selector: @- shouldBeOpaque@
shouldBeOpaque :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
shouldBeOpaque scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "shouldBeOpaque") retCULong []

-- | SCStreamProperty to ensure partially transparent areas on windows are backed by a solid white color so that the resulting image is fully opaque.
--
-- ObjC selector: @- setShouldBeOpaque:@
setShouldBeOpaque :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setShouldBeOpaque scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setShouldBeOpaque:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreGlobalClipDisplay@
ignoreGlobalClipDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreGlobalClipDisplay scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "ignoreGlobalClipDisplay") retCULong []

-- | SCStreamProperty to ignore framing on windows in the display sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreGlobalClipDisplay:@
setIgnoreGlobalClipDisplay :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreGlobalClipDisplay scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setIgnoreGlobalClipDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty to ignore global clipping when on single window share. When set to true, single window captures that are partially off the screen will not be clipped. (will ignore window placement in display context).
--
-- ObjC selector: @- ignoreGlobalClipSingleWindow@
ignoreGlobalClipSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
ignoreGlobalClipSingleWindow scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "ignoreGlobalClipSingleWindow") retCULong []

-- | SCStreamProperty to ignore global clipping when on single window share. When set to true, single window captures that are partially off the screen will not be clipped. (will ignore window placement in display context).
--
-- ObjC selector: @- setIgnoreGlobalClipSingleWindow:@
setIgnoreGlobalClipSingleWindow :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIgnoreGlobalClipSingleWindow scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setIgnoreGlobalClipSingleWindow:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty that informs the system if a privacy alert should be shown when using presenter overlay for a stream. Defaults to SCPresenterOverlayAlertSettingSystem;
--
-- ObjC selector: @- presenterOverlayPrivacyAlertSetting@
presenterOverlayPrivacyAlertSetting :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO SCPresenterOverlayAlertSetting
presenterOverlayPrivacyAlertSetting scStreamConfiguration  =
  fmap (coerce :: CLong -> SCPresenterOverlayAlertSetting) $ sendMsg scStreamConfiguration (mkSelector "presenterOverlayPrivacyAlertSetting") retCLong []

-- | SCStreamProperty that informs the system if a privacy alert should be shown when using presenter overlay for a stream. Defaults to SCPresenterOverlayAlertSettingSystem;
--
-- ObjC selector: @- setPresenterOverlayPrivacyAlertSetting:@
setPresenterOverlayPrivacyAlertSetting :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> SCPresenterOverlayAlertSetting -> IO ()
setPresenterOverlayPrivacyAlertSetting scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setPresenterOverlayPrivacyAlertSetting:") retVoid [argCLong (coerce value)]

-- | SCStreamProperty to show the child windows in display bound windows and applications sharing.  Child windows are included by default.
--
-- ObjC selector: @- includeChildWindows@
includeChildWindows :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
includeChildWindows scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "includeChildWindows") retCULong []

-- | SCStreamProperty to show the child windows in display bound windows and applications sharing.  Child windows are included by default.
--
-- ObjC selector: @- setIncludeChildWindows:@
setIncludeChildWindows :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setIncludeChildWindows scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setIncludeChildWindows:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty that specifies whether the microphone audio will be captured.  By default microphone is not captured.
--
-- ObjC selector: @- captureMicrophone@
captureMicrophone :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO Bool
captureMicrophone scStreamConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStreamConfiguration (mkSelector "captureMicrophone") retCULong []

-- | SCStreamProperty that specifies whether the microphone audio will be captured.  By default microphone is not captured.
--
-- ObjC selector: @- setCaptureMicrophone:@
setCaptureMicrophone :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> Bool -> IO ()
setCaptureMicrophone scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setCaptureMicrophone:") retVoid [argCULong (if value then 1 else 0)]

-- | SCStreamProperty client will choose captureDynamicRange between SCCaptureDynamicRangeSDR, SCCaptureDynamicRangeHDRLocalDisplay,  SCCaptureDynamicRangeHDRCanonicalDisplay. By default, the stream is capturing with SCCaptureDynamicRangeSDR. HDR capture is only supported with Apple Silicon Mac, setting this property on Intel Mac will have no effect. HDR recording is not support yet, adding a recording output to a stream with SCCaptureDynamicRangeHDR set will fail.
--
-- ObjC selector: @- captureDynamicRange@
captureDynamicRange :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> IO SCCaptureDynamicRange
captureDynamicRange scStreamConfiguration  =
  fmap (coerce :: CLong -> SCCaptureDynamicRange) $ sendMsg scStreamConfiguration (mkSelector "captureDynamicRange") retCLong []

-- | SCStreamProperty client will choose captureDynamicRange between SCCaptureDynamicRangeSDR, SCCaptureDynamicRangeHDRLocalDisplay,  SCCaptureDynamicRangeHDRCanonicalDisplay. By default, the stream is capturing with SCCaptureDynamicRangeSDR. HDR capture is only supported with Apple Silicon Mac, setting this property on Intel Mac will have no effect. HDR recording is not support yet, adding a recording output to a stream with SCCaptureDynamicRangeHDR set will fail.
--
-- ObjC selector: @- setCaptureDynamicRange:@
setCaptureDynamicRange :: IsSCStreamConfiguration scStreamConfiguration => scStreamConfiguration -> SCCaptureDynamicRange -> IO ()
setCaptureDynamicRange scStreamConfiguration  value =
  sendMsg scStreamConfiguration (mkSelector "setCaptureDynamicRange:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamConfigurationWithPreset:@
streamConfigurationWithPresetSelector :: Selector
streamConfigurationWithPresetSelector = mkSelector "streamConfigurationWithPreset:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @scalesToFit@
scalesToFitSelector :: Selector
scalesToFitSelector = mkSelector "scalesToFit"

-- | @Selector@ for @setScalesToFit:@
setScalesToFitSelector :: Selector
setScalesToFitSelector = mkSelector "setScalesToFit:"

-- | @Selector@ for @preservesAspectRatio@
preservesAspectRatioSelector :: Selector
preservesAspectRatioSelector = mkSelector "preservesAspectRatio"

-- | @Selector@ for @setPreservesAspectRatio:@
setPreservesAspectRatioSelector :: Selector
setPreservesAspectRatioSelector = mkSelector "setPreservesAspectRatio:"

-- | @Selector@ for @showsCursor@
showsCursorSelector :: Selector
showsCursorSelector = mkSelector "showsCursor"

-- | @Selector@ for @setShowsCursor:@
setShowsCursorSelector :: Selector
setShowsCursorSelector = mkSelector "setShowsCursor:"

-- | @Selector@ for @showMouseClicks@
showMouseClicksSelector :: Selector
showMouseClicksSelector = mkSelector "showMouseClicks"

-- | @Selector@ for @setShowMouseClicks:@
setShowMouseClicksSelector :: Selector
setShowMouseClicksSelector = mkSelector "setShowMouseClicks:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @queueDepth@
queueDepthSelector :: Selector
queueDepthSelector = mkSelector "queueDepth"

-- | @Selector@ for @setQueueDepth:@
setQueueDepthSelector :: Selector
setQueueDepthSelector = mkSelector "setQueueDepth:"

-- | @Selector@ for @colorMatrix@
colorMatrixSelector :: Selector
colorMatrixSelector = mkSelector "colorMatrix"

-- | @Selector@ for @setColorMatrix:@
setColorMatrixSelector :: Selector
setColorMatrixSelector = mkSelector "setColorMatrix:"

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector
colorSpaceNameSelector = mkSelector "colorSpaceName"

-- | @Selector@ for @setColorSpaceName:@
setColorSpaceNameSelector :: Selector
setColorSpaceNameSelector = mkSelector "setColorSpaceName:"

-- | @Selector@ for @capturesAudio@
capturesAudioSelector :: Selector
capturesAudioSelector = mkSelector "capturesAudio"

-- | @Selector@ for @setCapturesAudio:@
setCapturesAudioSelector :: Selector
setCapturesAudioSelector = mkSelector "setCapturesAudio:"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @setSampleRate:@
setSampleRateSelector :: Selector
setSampleRateSelector = mkSelector "setSampleRate:"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @setChannelCount:@
setChannelCountSelector :: Selector
setChannelCountSelector = mkSelector "setChannelCount:"

-- | @Selector@ for @excludesCurrentProcessAudio@
excludesCurrentProcessAudioSelector :: Selector
excludesCurrentProcessAudioSelector = mkSelector "excludesCurrentProcessAudio"

-- | @Selector@ for @setExcludesCurrentProcessAudio:@
setExcludesCurrentProcessAudioSelector :: Selector
setExcludesCurrentProcessAudioSelector = mkSelector "setExcludesCurrentProcessAudio:"

-- | @Selector@ for @ignoreShadowsDisplay@
ignoreShadowsDisplaySelector :: Selector
ignoreShadowsDisplaySelector = mkSelector "ignoreShadowsDisplay"

-- | @Selector@ for @setIgnoreShadowsDisplay:@
setIgnoreShadowsDisplaySelector :: Selector
setIgnoreShadowsDisplaySelector = mkSelector "setIgnoreShadowsDisplay:"

-- | @Selector@ for @ignoreShadowsSingleWindow@
ignoreShadowsSingleWindowSelector :: Selector
ignoreShadowsSingleWindowSelector = mkSelector "ignoreShadowsSingleWindow"

-- | @Selector@ for @setIgnoreShadowsSingleWindow:@
setIgnoreShadowsSingleWindowSelector :: Selector
setIgnoreShadowsSingleWindowSelector = mkSelector "setIgnoreShadowsSingleWindow:"

-- | @Selector@ for @captureResolution@
captureResolutionSelector :: Selector
captureResolutionSelector = mkSelector "captureResolution"

-- | @Selector@ for @setCaptureResolution:@
setCaptureResolutionSelector :: Selector
setCaptureResolutionSelector = mkSelector "setCaptureResolution:"

-- | @Selector@ for @capturesShadowsOnly@
capturesShadowsOnlySelector :: Selector
capturesShadowsOnlySelector = mkSelector "capturesShadowsOnly"

-- | @Selector@ for @setCapturesShadowsOnly:@
setCapturesShadowsOnlySelector :: Selector
setCapturesShadowsOnlySelector = mkSelector "setCapturesShadowsOnly:"

-- | @Selector@ for @shouldBeOpaque@
shouldBeOpaqueSelector :: Selector
shouldBeOpaqueSelector = mkSelector "shouldBeOpaque"

-- | @Selector@ for @setShouldBeOpaque:@
setShouldBeOpaqueSelector :: Selector
setShouldBeOpaqueSelector = mkSelector "setShouldBeOpaque:"

-- | @Selector@ for @ignoreGlobalClipDisplay@
ignoreGlobalClipDisplaySelector :: Selector
ignoreGlobalClipDisplaySelector = mkSelector "ignoreGlobalClipDisplay"

-- | @Selector@ for @setIgnoreGlobalClipDisplay:@
setIgnoreGlobalClipDisplaySelector :: Selector
setIgnoreGlobalClipDisplaySelector = mkSelector "setIgnoreGlobalClipDisplay:"

-- | @Selector@ for @ignoreGlobalClipSingleWindow@
ignoreGlobalClipSingleWindowSelector :: Selector
ignoreGlobalClipSingleWindowSelector = mkSelector "ignoreGlobalClipSingleWindow"

-- | @Selector@ for @setIgnoreGlobalClipSingleWindow:@
setIgnoreGlobalClipSingleWindowSelector :: Selector
setIgnoreGlobalClipSingleWindowSelector = mkSelector "setIgnoreGlobalClipSingleWindow:"

-- | @Selector@ for @presenterOverlayPrivacyAlertSetting@
presenterOverlayPrivacyAlertSettingSelector :: Selector
presenterOverlayPrivacyAlertSettingSelector = mkSelector "presenterOverlayPrivacyAlertSetting"

-- | @Selector@ for @setPresenterOverlayPrivacyAlertSetting:@
setPresenterOverlayPrivacyAlertSettingSelector :: Selector
setPresenterOverlayPrivacyAlertSettingSelector = mkSelector "setPresenterOverlayPrivacyAlertSetting:"

-- | @Selector@ for @includeChildWindows@
includeChildWindowsSelector :: Selector
includeChildWindowsSelector = mkSelector "includeChildWindows"

-- | @Selector@ for @setIncludeChildWindows:@
setIncludeChildWindowsSelector :: Selector
setIncludeChildWindowsSelector = mkSelector "setIncludeChildWindows:"

-- | @Selector@ for @captureMicrophone@
captureMicrophoneSelector :: Selector
captureMicrophoneSelector = mkSelector "captureMicrophone"

-- | @Selector@ for @setCaptureMicrophone:@
setCaptureMicrophoneSelector :: Selector
setCaptureMicrophoneSelector = mkSelector "setCaptureMicrophone:"

-- | @Selector@ for @captureDynamicRange@
captureDynamicRangeSelector :: Selector
captureDynamicRangeSelector = mkSelector "captureDynamicRange"

-- | @Selector@ for @setCaptureDynamicRange:@
setCaptureDynamicRangeSelector :: Selector
setCaptureDynamicRangeSelector = mkSelector "setCaptureDynamicRange:"

