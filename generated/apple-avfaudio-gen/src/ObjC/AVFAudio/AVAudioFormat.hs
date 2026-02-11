{-# LANGUAGE PatternSynonyms #-}
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
  , initWithStreamDescriptionSelector
  , initWithStreamDescription_channelLayoutSelector
  , initStandardFormatWithSampleRate_channelsSelector
  , initStandardFormatWithSampleRate_channelLayoutSelector
  , initWithCommonFormat_sampleRate_channels_interleavedSelector
  , initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector
  , initWithSettingsSelector
  , initWithCMAudioFormatDescriptionSelector
  , isEqualSelector
  , standardSelector
  , commonFormatSelector
  , channelCountSelector
  , sampleRateSelector
  , interleavedSelector
  , streamDescriptionSelector
  , channelLayoutSelector
  , magicCookieSelector
  , setMagicCookieSelector
  , settingsSelector
  , formatDescriptionSelector

  -- * Enum types
  , AVAudioCommonFormat(AVAudioCommonFormat)
  , pattern AVAudioOtherFormat
  , pattern AVAudioPCMFormatFloat32
  , pattern AVAudioPCMFormatFloat64
  , pattern AVAudioPCMFormatInt16
  , pattern AVAudioPCMFormatInt32

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
initWithStreamDescription avAudioFormat  asbd =
    sendMsg avAudioFormat (mkSelector "initWithStreamDescription:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst asbd)) :: Ptr ())] >>= ownedObject . castPtr

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
initWithStreamDescription_channelLayout avAudioFormat  asbd layout =
  withObjCPtr layout $ \raw_layout ->
      sendMsg avAudioFormat (mkSelector "initWithStreamDescription:channelLayout:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst asbd)) :: Ptr ()), argPtr (castPtr raw_layout :: Ptr ())] >>= ownedObject . castPtr

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
initStandardFormatWithSampleRate_channels avAudioFormat  sampleRate channels =
    sendMsg avAudioFormat (mkSelector "initStandardFormatWithSampleRate:channels:") (retPtr retVoid) [argCDouble sampleRate, argCUInt channels] >>= ownedObject . castPtr

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
initStandardFormatWithSampleRate_channelLayout avAudioFormat  sampleRate layout =
  withObjCPtr layout $ \raw_layout ->
      sendMsg avAudioFormat (mkSelector "initStandardFormatWithSampleRate:channelLayout:") (retPtr retVoid) [argCDouble sampleRate, argPtr (castPtr raw_layout :: Ptr ())] >>= ownedObject . castPtr

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
initWithCommonFormat_sampleRate_channels_interleaved avAudioFormat  format sampleRate channels interleaved =
    sendMsg avAudioFormat (mkSelector "initWithCommonFormat:sampleRate:channels:interleaved:") (retPtr retVoid) [argCULong (coerce format), argCDouble sampleRate, argCUInt channels, argCULong (if interleaved then 1 else 0)] >>= ownedObject . castPtr

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
initWithCommonFormat_sampleRate_interleaved_channelLayout avAudioFormat  format sampleRate interleaved layout =
  withObjCPtr layout $ \raw_layout ->
      sendMsg avAudioFormat (mkSelector "initWithCommonFormat:sampleRate:interleaved:channelLayout:") (retPtr retVoid) [argCULong (coerce format), argCDouble sampleRate, argCULong (if interleaved then 1 else 0), argPtr (castPtr raw_layout :: Ptr ())] >>= ownedObject . castPtr

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
initWithSettings avAudioFormat  settings =
  withObjCPtr settings $ \raw_settings ->
      sendMsg avAudioFormat (mkSelector "initWithSettings:") (retPtr retVoid) [argPtr (castPtr raw_settings :: Ptr ())] >>= ownedObject . castPtr

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
initWithCMAudioFormatDescription avAudioFormat  formatDescription =
    sendMsg avAudioFormat (mkSelector "initWithCMAudioFormatDescription:") (retPtr retVoid) [argPtr (castPtr (unRawId formatDescription) :: Ptr ())] >>= ownedObject . castPtr

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
isEqual avAudioFormat  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFormat (mkSelector "isEqual:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | standard
--
-- Describes whether the format is deinterleaved native-endian float.
--
-- ObjC selector: @- standard@
standard :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO Bool
standard avAudioFormat  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFormat (mkSelector "standard") retCULong []

-- | commonFormat
--
-- An @AVAudioCommonFormat@ identifying the format
--
-- ObjC selector: @- commonFormat@
commonFormat :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO AVAudioCommonFormat
commonFormat avAudioFormat  =
    fmap (coerce :: CULong -> AVAudioCommonFormat) $ sendMsg avAudioFormat (mkSelector "commonFormat") retCULong []

-- | channelCount
--
-- The number of channels of audio data.
--
-- ObjC selector: @- channelCount@
channelCount :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO CUInt
channelCount avAudioFormat  =
    sendMsg avAudioFormat (mkSelector "channelCount") retCUInt []

-- | sampleRate
--
-- A sampling rate in Hertz.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO CDouble
sampleRate avAudioFormat  =
    sendMsg avAudioFormat (mkSelector "sampleRate") retCDouble []

-- | interleaved
--
-- Describes whether the samples are interleaved.
--
-- For non-PCM formats, the value is undefined.
--
-- ObjC selector: @- interleaved@
interleaved :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO Bool
interleaved avAudioFormat  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFormat (mkSelector "interleaved") retCULong []

-- | streamDescription
--
-- Returns the AudioStreamBasicDescription, for use with lower-level audio API's.
--
-- ObjC selector: @- streamDescription@
streamDescription :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Const RawId)
streamDescription avAudioFormat  =
    fmap Const $ fmap (RawId . castPtr) $ sendMsg avAudioFormat (mkSelector "streamDescription") (retPtr retVoid) []

-- | channelLayout
--
-- The underlying AVAudioChannelLayout, if any.
--
-- Only formats with more than 2 channels are required to have channel layouts.
--
-- ObjC selector: @- channelLayout@
channelLayout :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Id AVAudioChannelLayout)
channelLayout avAudioFormat  =
    sendMsg avAudioFormat (mkSelector "channelLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | magicCookie
--
-- The underlying magic cookie, if any.
--
-- A magic cookie contains metadata associated with encoders and decoders.		Encoders produce a magic cookie, and some decoders require a magic cookie to decode properly.
--
-- ObjC selector: @- magicCookie@
magicCookie :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Id NSData)
magicCookie avAudioFormat  =
    sendMsg avAudioFormat (mkSelector "magicCookie") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | magicCookie
--
-- The underlying magic cookie, if any.
--
-- A magic cookie contains metadata associated with encoders and decoders.		Encoders produce a magic cookie, and some decoders require a magic cookie to decode properly.
--
-- ObjC selector: @- setMagicCookie:@
setMagicCookie :: (IsAVAudioFormat avAudioFormat, IsNSData value) => avAudioFormat -> value -> IO ()
setMagicCookie avAudioFormat  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avAudioFormat (mkSelector "setMagicCookie:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | settings
--
-- Returns the format represented as a dictionary with keys from AVAudioSettings.h.
--
-- ObjC selector: @- settings@
settings :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO (Id NSDictionary)
settings avAudioFormat  =
    sendMsg avAudioFormat (mkSelector "settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | formatDescription
--
-- Converts to a CMAudioFormatDescriptionRef, for use with Core Media API's.
--
-- ObjC selector: @- formatDescription@
formatDescription :: IsAVAudioFormat avAudioFormat => avAudioFormat -> IO RawId
formatDescription avAudioFormat  =
    fmap (RawId . castPtr) $ sendMsg avAudioFormat (mkSelector "formatDescription") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStreamDescription:@
initWithStreamDescriptionSelector :: Selector
initWithStreamDescriptionSelector = mkSelector "initWithStreamDescription:"

-- | @Selector@ for @initWithStreamDescription:channelLayout:@
initWithStreamDescription_channelLayoutSelector :: Selector
initWithStreamDescription_channelLayoutSelector = mkSelector "initWithStreamDescription:channelLayout:"

-- | @Selector@ for @initStandardFormatWithSampleRate:channels:@
initStandardFormatWithSampleRate_channelsSelector :: Selector
initStandardFormatWithSampleRate_channelsSelector = mkSelector "initStandardFormatWithSampleRate:channels:"

-- | @Selector@ for @initStandardFormatWithSampleRate:channelLayout:@
initStandardFormatWithSampleRate_channelLayoutSelector :: Selector
initStandardFormatWithSampleRate_channelLayoutSelector = mkSelector "initStandardFormatWithSampleRate:channelLayout:"

-- | @Selector@ for @initWithCommonFormat:sampleRate:channels:interleaved:@
initWithCommonFormat_sampleRate_channels_interleavedSelector :: Selector
initWithCommonFormat_sampleRate_channels_interleavedSelector = mkSelector "initWithCommonFormat:sampleRate:channels:interleaved:"

-- | @Selector@ for @initWithCommonFormat:sampleRate:interleaved:channelLayout:@
initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector :: Selector
initWithCommonFormat_sampleRate_interleaved_channelLayoutSelector = mkSelector "initWithCommonFormat:sampleRate:interleaved:channelLayout:"

-- | @Selector@ for @initWithSettings:@
initWithSettingsSelector :: Selector
initWithSettingsSelector = mkSelector "initWithSettings:"

-- | @Selector@ for @initWithCMAudioFormatDescription:@
initWithCMAudioFormatDescriptionSelector :: Selector
initWithCMAudioFormatDescriptionSelector = mkSelector "initWithCMAudioFormatDescription:"

-- | @Selector@ for @isEqual:@
isEqualSelector :: Selector
isEqualSelector = mkSelector "isEqual:"

-- | @Selector@ for @standard@
standardSelector :: Selector
standardSelector = mkSelector "standard"

-- | @Selector@ for @commonFormat@
commonFormatSelector :: Selector
commonFormatSelector = mkSelector "commonFormat"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @interleaved@
interleavedSelector :: Selector
interleavedSelector = mkSelector "interleaved"

-- | @Selector@ for @streamDescription@
streamDescriptionSelector :: Selector
streamDescriptionSelector = mkSelector "streamDescription"

-- | @Selector@ for @channelLayout@
channelLayoutSelector :: Selector
channelLayoutSelector = mkSelector "channelLayout"

-- | @Selector@ for @magicCookie@
magicCookieSelector :: Selector
magicCookieSelector = mkSelector "magicCookie"

-- | @Selector@ for @setMagicCookie:@
setMagicCookieSelector :: Selector
setMagicCookieSelector = mkSelector "setMagicCookie:"

-- | @Selector@ for @settings@
settingsSelector :: Selector
settingsSelector = mkSelector "settings"

-- | @Selector@ for @formatDescription@
formatDescriptionSelector :: Selector
formatDescriptionSelector = mkSelector "formatDescription"

