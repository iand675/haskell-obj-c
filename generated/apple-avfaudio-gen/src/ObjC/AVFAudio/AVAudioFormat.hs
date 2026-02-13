{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioFormat
--
-- A representation of an audio format.
--
-- AVAudioFormat wraps a Core Audio AudioStreamBasicDescription struct, with convenience		initializers and accessors for common formats, including Core Audio's standard deinterleaved		32-bit floating point.
--
-- Instances of this class are immutable.
--
-- Generated bindings for @AVAudioFormat@.
module ObjC.AVFAudio.AVAudioFormat
  ( AVAudioFormat
  , IsAVAudioFormat(..)
  , initWithStreamDescription
  , initWithStreamDescription_channelLayout
  , initStandardFormatWithSampleRate_channels
  , initStandardFormatWithSampleRate_channelLayout
  , initWithCommonFormat_sampleRate_channels_interleaved
  , initWithCommonFormat_sampleRate_interleaved_channelLayout
  , initWithSettings
  , initWithCMAudioFormatDescription
  , isEqual
  , standard
  , commonFormat
  , channelCount
  , sampleRate
  , interleaved
  , streamDescription
  , channelLayout
  , magicCookie
  , setMagicCookie
  , settings
  , formatDescription
  , channelCountSelector
  , channelLayoutSelector
  , commonFormatSelector
  , formatDescriptionSelector
  , initStandardFormatWithSampleRate_channelLayoutSelector
  , initStandardFormatWithSampleRate_channelsSelector
  , initWithCMAudioFormatDescriptionSelector
  , initWithCommonFormat_sampleRate_channels_interleavedSelector
  , initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector
  , initWithSettingsSelector
  , initWithStreamDescriptionSelector
  , initWithStreamDescription_channelLayoutSelector
  , interleavedSelector
  , isEqualSelector
  , magicCookieSelector
  , sampleRateSelector
  , setMagicCookieSelector
  , settingsSelector
  , standardSelector
  , streamDescriptionSelector

  -- * Enum types
  , AVAudioCommonFormat(AVAudioCommonFormat)
  , pattern AVAudioOtherFormat
  , pattern AVAudioPCMFormatFloat32
  , pattern AVAudioPCMFormatFloat64
  , pattern AVAudioPCMFormatInt16
  , pattern AVAudioPCMFormatInt32

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithStreamDescription:
--
-- Initialize from an AudioStreamBasicDescription.
--
-- @asbd@ — the AudioStreamBasicDescription
--
-- If the format specifies more than 2 channels, this method fails (returns nil).
--
-- ObjC selector: @- initWithStreamDescription:@
initWithStreamDescription :: IsAVAudioFormat avAudioFormat => avAudioFormat -> Const RawId -> IO (Id AVAudioFormat)
initWithStreamDescription avAudioFormat asbd =
  sendOwnedMessage avAudioFormat initWithStreamDescriptionSelector asbd

-- | initWithStreamDescription:channelLayout:
--
-- Initialize from an AudioStreamBasicDescription and optional channel layout.
--
-- @asbd@ — the AudioStreamBasicDescription
--
-- @layout@ — the channel layout. Can be nil only if asbd specifies 1 or 2 channels.
--
-- If the format specifies more than 2 channels, this method fails (returns nil) unless layout		is non-nil.
--
-- ObjC selector: @- initWithStreamDescription:channelLayout:@
initWithStreamDescription_channelLayout :: (IsAVAudioFormat avAudioFormat, IsAVAudioChannelLayout layout) => avAudioFormat -> Const RawId -> layout -> IO (Id AVAudioFormat)
initWithStreamDescription_channelLayout avAudioFormat asbd layout =
  sendOwnedMessage avAudioFormat initWithStreamDescription_channelLayoutSelector asbd (toAVAudioChannelLayout layout)

-- | initStandardFormatWithSampleRate:channels:
--
-- Initialize to deinterleaved float with the specified sample rate and channel count.
--
-- @sampleRate@ — the sample rate
--
-- @channels@ — the channel count
--
-- If the format specifies more than 2 channels, this method fails (returns nil).
--
-- ObjC selector: @- initStandardFormatWithSampleRate:channels:@
initStandardFormatWithSampleRate_channels :: IsAVAudioFormat avAudioFormat => avAudioFormat -> CDouble -> CUInt -> IO (Id AVAudioFormat)
initStandardFormatWithSampleRate_channels avAudioFormat sampleRate channels =
  sendOwnedMessage avAudioFormat initStandardFormatWithSampleRate_channelsSelector sampleRate channels

-- | initStandardFormatWithSampleRate:channelLayout:
--
-- Initialize to deinterleaved float with the specified sample rate and channel layout.
--
-- @sampleRate@ — the sample rate
--
-- @layout@ — the channel layout. must not be nil.
--
-- ObjC selector: @- initStandardFormatWithSampleRate:channelLayout:@
initStandardFormatWithSampleRate_channelLayout :: (IsAVAudioFormat avAudioFormat, IsAVAudioChannelLayout layout) => avAudioFormat -> CDouble -> layout -> IO (Id AVAudioFormat)
initStandardFormatWithSampleRate_channelLayout avAudioFormat sampleRate layout =
  sendOwnedMessage avAudioFormat initStandardFormatWithSampleRate_channelLayoutSelector sampleRate (toAVAudioChannelLayout layout)

-- | initWithCommonFormat:sampleRate:channels:interleaved:
--
-- Initialize to float with the specified sample rate, channel count and interleavedness.
--
-- @format@ — the common format type
--
-- @sampleRate@ — the sample rate
--
-- @channels@ — the channel count
--
-- @interleaved@ — true if interleaved
--
-- If the format specifies more than 2 channels, this method fails (returns nil).
--
-- ObjC selector: @- initWithCommonFormat:sampleRate:channels:interleaved:@
initWithCommonFormat_sampleRate_channels_interleaved :: IsAVAudioFormat avAudioFormat => avAudioFormat -> AVAudioCommonFormat -> CDouble -> CUInt -> Bool -> IO (Id AVAudioFormat)
initWithCommonFormat_sampleRate_channels_interleaved avAudioFormat format sampleRate channels interleaved =
  sendOwnedMessage avAudioFormat initWithCommonFormat_sampleRate_channels_interleavedSelector format sampleRate channels interleaved

-- | initWithCommonFormat:sampleRate:interleaved:channelLayout:
--
-- Initialize to float with the specified sample rate, channel layout and interleavedness.
--
-- @format@ — the common format type
--
-- @sampleRate@ — the sample rate
--
-- @interleaved@ — true if interleaved
--
-- @layout@ — the channel layout. must not be nil.
--
-- ObjC selector: @- initWithCommonFormat:sampleRate:interleaved:channelLayout:@
initWithCommonFormat_sampleRate_interleaved_channelLayout :: (IsAVAudioFormat avAudioFormat, IsAVAudioChannelLayout layout) => avAudioFormat -> AVAudioCommonFormat -> CDouble -> Bool -> layout -> IO (Id AVAudioFormat)
initWithCommonFormat_sampleRate_interleaved_channelLayout avAudioFormat format sampleRate interleaved layout =
  sendOwnedMessage avAudioFormat initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector format sampleRate interleaved (toAVAudioChannelLayout layout)

-- | initWithSettings:
--
-- Initialize using a settings dictionary.
--
-- See AVAudioSettings.h. Note that many settings dictionary elements pertain to encoder		settings, not the basic format, and will be ignored.
--
-- Returns nil if a format cannot be constructed with the provided settings, e.g. when:			- AVNumberOfChannelsKey specifies more than 2 channels, but AVChannelLayoutKey hasn't 			  been specified or the layout does not match			- AVLinearPCMBitDepthKey for linear PCM format specifies less than 8 or greater			  than 32 bits			- values for the keys are not of the expected types
--
-- ObjC selector: @- initWithSettings:@
initWithSettings :: (IsAVAudioFormat avAudioFormat, IsNSDictionary settings) => avAudioFormat -> settings -> IO (Id AVAudioFormat)
initWithSettings avAudioFormat settings =
  sendOwnedMessage avAudioFormat initWithSettingsSelector (toNSDictionary settings)

-- | initWithCMAudioFormatDescription:
--
-- initialize from a CMAudioFormatDescriptionRef.
--
-- @formatDescription@ — the CMAudioFormatDescriptionRef.
--
-- If formatDescription is invalid, this method fails (returns nil).
--
-- ObjC selector: @- initWithCMAudioFormatDescription:@
initWithCMAudioFormatDescription :: IsAVAudioFormat avAudioFormat => avAudioFormat -> RawId -> IO (Id AVAudioFormat)
initWithCMAudioFormatDescription avAudioFormat formatDescription =
  sendOwnedMessage avAudioFormat initWithCMAudioFormatDescriptionSelector formatDescription

-- | isEqual:
--
-- Determine whether another format is functionally equivalent.
--
-- @object@ — the format to compare against
--
-- For PCM, interleavedness is ignored for mono. Differences in the AudioStreamBasicDescription		alignment and packedness are ignored when they are not significant (e.g. with 1 channel, 2		bytes per frame and 16 bits per channel, neither alignment, the format is implicitly packed		and can be interpreted as either high- or low-aligned.)		For AVAudioChannelLayout, a layout with standard mono/stereo tag is considered to be 		equivalent to a nil layout. Otherwise, the layouts are compared for equality.
--
-- ObjC selector: @- isEqual:@
isEqual :: IsAVAudioFormat avAudioFormat => avAudioFormat -> RawId -> IO Bool
isEqual avAudioFormat object =
  sendMessage avAudioFormat isEqualSelector object

-- | standard
--
-- Describes whether the format is deinterleaved native-endian float.
--
-- ObjC selector: @- standard@
standard :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO Bool
standard avAudioFormat =
  sendMessage avAudioFormat standardSelector

-- | commonFormat
--
-- An @AVAudioCommonFormat@ identifying the format
--
-- ObjC selector: @- commonFormat@
commonFormat :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO AVAudioCommonFormat
commonFormat avAudioFormat =
  sendMessage avAudioFormat commonFormatSelector

-- | channelCount
--
-- The number of channels of audio data.
--
-- ObjC selector: @- channelCount@
channelCount :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO CUInt
channelCount avAudioFormat =
  sendMessage avAudioFormat channelCountSelector

-- | sampleRate
--
-- A sampling rate in Hertz.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO CDouble
sampleRate avAudioFormat =
  sendMessage avAudioFormat sampleRateSelector

-- | interleaved
--
-- Describes whether the samples are interleaved.
--
-- For non-PCM formats, the value is undefined.
--
-- ObjC selector: @- interleaved@
interleaved :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO Bool
interleaved avAudioFormat =
  sendMessage avAudioFormat interleavedSelector

-- | streamDescription
--
-- Returns the AudioStreamBasicDescription, for use with lower-level audio API's.
--
-- ObjC selector: @- streamDescription@
streamDescription :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Const RawId)
streamDescription avAudioFormat =
  sendMessage avAudioFormat streamDescriptionSelector

-- | channelLayout
--
-- The underlying AVAudioChannelLayout, if any.
--
-- Only formats with more than 2 channels are required to have channel layouts.
--
-- ObjC selector: @- channelLayout@
channelLayout :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Id AVAudioChannelLayout)
channelLayout avAudioFormat =
  sendMessage avAudioFormat channelLayoutSelector

-- | magicCookie
--
-- The underlying magic cookie, if any.
--
-- A magic cookie contains metadata associated with encoders and decoders.		Encoders produce a magic cookie, and some decoders require a magic cookie to decode properly.
--
-- ObjC selector: @- magicCookie@
magicCookie :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Id NSData)
magicCookie avAudioFormat =
  sendMessage avAudioFormat magicCookieSelector

-- | magicCookie
--
-- The underlying magic cookie, if any.
--
-- A magic cookie contains metadata associated with encoders and decoders.		Encoders produce a magic cookie, and some decoders require a magic cookie to decode properly.
--
-- ObjC selector: @- setMagicCookie:@
setMagicCookie :: (IsAVAudioFormat avAudioFormat, IsNSData value) => avAudioFormat -> value -> IO ()
setMagicCookie avAudioFormat value =
  sendMessage avAudioFormat setMagicCookieSelector (toNSData value)

-- | settings
--
-- Returns the format represented as a dictionary with keys from AVAudioSettings.h.
--
-- ObjC selector: @- settings@
settings :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Id NSDictionary)
settings avAudioFormat =
  sendMessage avAudioFormat settingsSelector

-- | formatDescription
--
-- Converts to a CMAudioFormatDescriptionRef, for use with Core Media API's.
--
-- ObjC selector: @- formatDescription@
formatDescription :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO RawId
formatDescription avAudioFormat =
  sendMessage avAudioFormat formatDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStreamDescription:@
initWithStreamDescriptionSelector :: Selector '[Const RawId] (Id AVAudioFormat)
initWithStreamDescriptionSelector = mkSelector "initWithStreamDescription:"

-- | @Selector@ for @initWithStreamDescription:channelLayout:@
initWithStreamDescription_channelLayoutSelector :: Selector '[Const RawId, Id AVAudioChannelLayout] (Id AVAudioFormat)
initWithStreamDescription_channelLayoutSelector = mkSelector "initWithStreamDescription:channelLayout:"

-- | @Selector@ for @initStandardFormatWithSampleRate:channels:@
initStandardFormatWithSampleRate_channelsSelector :: Selector '[CDouble, CUInt] (Id AVAudioFormat)
initStandardFormatWithSampleRate_channelsSelector = mkSelector "initStandardFormatWithSampleRate:channels:"

-- | @Selector@ for @initStandardFormatWithSampleRate:channelLayout:@
initStandardFormatWithSampleRate_channelLayoutSelector :: Selector '[CDouble, Id AVAudioChannelLayout] (Id AVAudioFormat)
initStandardFormatWithSampleRate_channelLayoutSelector = mkSelector "initStandardFormatWithSampleRate:channelLayout:"

-- | @Selector@ for @initWithCommonFormat:sampleRate:channels:interleaved:@
initWithCommonFormat_sampleRate_channels_interleavedSelector :: Selector '[AVAudioCommonFormat, CDouble, CUInt, Bool] (Id AVAudioFormat)
initWithCommonFormat_sampleRate_channels_interleavedSelector = mkSelector "initWithCommonFormat:sampleRate:channels:interleaved:"

-- | @Selector@ for @initWithCommonFormat:sampleRate:interleaved:channelLayout:@
initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector :: Selector '[AVAudioCommonFormat, CDouble, Bool, Id AVAudioChannelLayout] (Id AVAudioFormat)
initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector = mkSelector "initWithCommonFormat:sampleRate:interleaved:channelLayout:"

-- | @Selector@ for @initWithSettings:@
initWithSettingsSelector :: Selector '[Id NSDictionary] (Id AVAudioFormat)
initWithSettingsSelector = mkSelector "initWithSettings:"

-- | @Selector@ for @initWithCMAudioFormatDescription:@
initWithCMAudioFormatDescriptionSelector :: Selector '[RawId] (Id AVAudioFormat)
initWithCMAudioFormatDescriptionSelector = mkSelector "initWithCMAudioFormatDescription:"

-- | @Selector@ for @isEqual:@
isEqualSelector :: Selector '[RawId] Bool
isEqualSelector = mkSelector "isEqual:"

-- | @Selector@ for @standard@
standardSelector :: Selector '[] Bool
standardSelector = mkSelector "standard"

-- | @Selector@ for @commonFormat@
commonFormatSelector :: Selector '[] AVAudioCommonFormat
commonFormatSelector = mkSelector "commonFormat"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] CUInt
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector '[] CDouble
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @interleaved@
interleavedSelector :: Selector '[] Bool
interleavedSelector = mkSelector "interleaved"

-- | @Selector@ for @streamDescription@
streamDescriptionSelector :: Selector '[] (Const RawId)
streamDescriptionSelector = mkSelector "streamDescription"

-- | @Selector@ for @channelLayout@
channelLayoutSelector :: Selector '[] (Id AVAudioChannelLayout)
channelLayoutSelector = mkSelector "channelLayout"

-- | @Selector@ for @magicCookie@
magicCookieSelector :: Selector '[] (Id NSData)
magicCookieSelector = mkSelector "magicCookie"

-- | @Selector@ for @setMagicCookie:@
setMagicCookieSelector :: Selector '[Id NSData] ()
setMagicCookieSelector = mkSelector "setMagicCookie:"

-- | @Selector@ for @settings@
settingsSelector :: Selector '[] (Id NSDictionary)
settingsSelector = mkSelector "settings"

-- | @Selector@ for @formatDescription@
formatDescriptionSelector :: Selector '[] RawId
formatDescriptionSelector = mkSelector "formatDescription"

