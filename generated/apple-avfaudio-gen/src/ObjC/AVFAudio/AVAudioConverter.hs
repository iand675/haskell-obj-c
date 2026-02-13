{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioConverter
--
-- Converts streams of audio between various formats.
--
-- Generated bindings for @AVAudioConverter@.
module ObjC.AVFAudio.AVAudioConverter
  ( AVAudioConverter
  , IsAVAudioConverter(..)
  , initFromFormat_toFormat
  , reset
  , convertToBuffer_fromBuffer_error
  , convertToBuffer_error_withInputFromBlock
  , inputFormat
  , outputFormat
  , channelMap
  , setChannelMap
  , magicCookie
  , setMagicCookie
  , downmix
  , setDownmix
  , dither
  , setDither
  , sampleRateConverterQuality
  , setSampleRateConverterQuality
  , sampleRateConverterAlgorithm
  , setSampleRateConverterAlgorithm
  , primeMethod
  , setPrimeMethod
  , primeInfo
  , setPrimeInfo
  , audioSyncPacketFrequency
  , setAudioSyncPacketFrequency
  , contentSource
  , setContentSource
  , dynamicRangeControlConfiguration
  , setDynamicRangeControlConfiguration
  , bitRate
  , setBitRate
  , bitRateStrategy
  , setBitRateStrategy
  , maximumOutputPacketSize
  , availableEncodeBitRates
  , applicableEncodeBitRates
  , availableEncodeSampleRates
  , applicableEncodeSampleRates
  , availableEncodeChannelLayoutTags
  , applicableEncodeBitRatesSelector
  , applicableEncodeSampleRatesSelector
  , audioSyncPacketFrequencySelector
  , availableEncodeBitRatesSelector
  , availableEncodeChannelLayoutTagsSelector
  , availableEncodeSampleRatesSelector
  , bitRateSelector
  , bitRateStrategySelector
  , channelMapSelector
  , contentSourceSelector
  , convertToBuffer_error_withInputFromBlockSelector
  , convertToBuffer_fromBuffer_errorSelector
  , ditherSelector
  , downmixSelector
  , dynamicRangeControlConfigurationSelector
  , initFromFormat_toFormatSelector
  , inputFormatSelector
  , magicCookieSelector
  , maximumOutputPacketSizeSelector
  , outputFormatSelector
  , primeInfoSelector
  , primeMethodSelector
  , resetSelector
  , sampleRateConverterAlgorithmSelector
  , sampleRateConverterQualitySelector
  , setAudioSyncPacketFrequencySelector
  , setBitRateSelector
  , setBitRateStrategySelector
  , setChannelMapSelector
  , setContentSourceSelector
  , setDitherSelector
  , setDownmixSelector
  , setDynamicRangeControlConfigurationSelector
  , setMagicCookieSelector
  , setPrimeInfoSelector
  , setPrimeMethodSelector
  , setSampleRateConverterAlgorithmSelector
  , setSampleRateConverterQualitySelector

  -- * Enum types
  , AVAudioContentSource(AVAudioContentSource)
  , pattern AVAudioContentSource_Unspecified
  , pattern AVAudioContentSource_Reserved
  , pattern AVAudioContentSource_AppleCapture_Traditional
  , pattern AVAudioContentSource_AppleCapture_Spatial
  , pattern AVAudioContentSource_AppleCapture_Spatial_Enhanced
  , pattern AVAudioContentSource_AppleMusic_Traditional
  , pattern AVAudioContentSource_AppleMusic_Spatial
  , pattern AVAudioContentSource_AppleAV_Traditional_Offline
  , pattern AVAudioContentSource_AppleAV_Spatial_Offline
  , pattern AVAudioContentSource_AppleAV_Traditional_Live
  , pattern AVAudioContentSource_AppleAV_Spatial_Live
  , pattern AVAudioContentSource_ApplePassthrough
  , pattern AVAudioContentSource_Capture_Traditional
  , pattern AVAudioContentSource_Capture_Spatial
  , pattern AVAudioContentSource_Capture_Spatial_Enhanced
  , pattern AVAudioContentSource_Music_Traditional
  , pattern AVAudioContentSource_Music_Spatial
  , pattern AVAudioContentSource_AV_Traditional_Offline
  , pattern AVAudioContentSource_AV_Spatial_Offline
  , pattern AVAudioContentSource_AV_Traditional_Live
  , pattern AVAudioContentSource_AV_Spatial_Live
  , pattern AVAudioContentSource_Passthrough
  , AVAudioConverterOutputStatus(AVAudioConverterOutputStatus)
  , pattern AVAudioConverterOutputStatus_HaveData
  , pattern AVAudioConverterOutputStatus_InputRanDry
  , pattern AVAudioConverterOutputStatus_EndOfStream
  , pattern AVAudioConverterOutputStatus_Error
  , AVAudioConverterPrimeMethod(AVAudioConverterPrimeMethod)
  , pattern AVAudioConverterPrimeMethod_Pre
  , pattern AVAudioConverterPrimeMethod_Normal
  , pattern AVAudioConverterPrimeMethod_None
  , AVAudioDynamicRangeControlConfiguration(AVAudioDynamicRangeControlConfiguration)
  , pattern AVAudioDynamicRangeControlConfiguration_None
  , pattern AVAudioDynamicRangeControlConfiguration_Music
  , pattern AVAudioDynamicRangeControlConfiguration_Speech
  , pattern AVAudioDynamicRangeControlConfiguration_Movie
  , pattern AVAudioDynamicRangeControlConfiguration_Capture

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Structs
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initFromFormat:toFormat:
--
-- Initialize from input and output formats.
--
-- @fromFormat@ — The input format.
--
-- @toFormat@ — The output format.
--
-- Returns nil if the format conversion is not possible.
--
-- ObjC selector: @- initFromFormat:toFormat:@
initFromFormat_toFormat :: (IsAVAudioConverter avAudioConverter, IsAVAudioFormat fromFormat, IsAVAudioFormat toFormat) => avAudioConverter -> fromFormat -> toFormat -> IO (Id AVAudioConverter)
initFromFormat_toFormat avAudioConverter fromFormat toFormat =
  sendOwnedMessage avAudioConverter initFromFormat_toFormatSelector (toAVAudioFormat fromFormat) (toAVAudioFormat toFormat)

-- | reset
--
-- Resets the converter so that a new stream may be converted.
--
-- ObjC selector: @- reset@
reset :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO ()
reset avAudioConverter =
  sendMessage avAudioConverter resetSelector

-- | convertToBuffer:fromBuffer:error:
--
-- Perform a simple conversion. That is, a conversion which does not involve codecs or sample rate conversion.
--
-- @inputBuffer@ — The input buffer.
--
-- @outputBuffer@ — The output buffer.
--
-- @outError@ — An error if the conversion fails.
--
-- Returns: YES is returned on success, NO when an error has occurred.
--
-- The output buffer's frameCapacity should be at least at large as the inputBuffer's frameLength.		If the conversion involves a codec or sample rate conversion, you instead must use		convertToBuffer:error:withInputFromBlock:.
--
-- ObjC selector: @- convertToBuffer:fromBuffer:error:@
convertToBuffer_fromBuffer_error :: (IsAVAudioConverter avAudioConverter, IsAVAudioPCMBuffer outputBuffer, IsNSError outError) => avAudioConverter -> outputBuffer -> Const (Id AVAudioPCMBuffer) -> outError -> IO Bool
convertToBuffer_fromBuffer_error avAudioConverter outputBuffer inputBuffer outError =
  sendMessage avAudioConverter convertToBuffer_fromBuffer_errorSelector (toAVAudioPCMBuffer outputBuffer) inputBuffer (toNSError outError)

-- | convertToBuffer:error:withInputFromBlock:
--
-- Perform any supported conversion.
--
-- @inputBlock@ — A block which will be called to get input data as needed. See description for AVAudioConverterInputBlock.
--
-- @outputBuffer@ — The output buffer.
--
-- @outError@ — An error if the conversion fails.
--
-- Returns: An AVAudioConverterOutputStatus is returned.
--
-- It attempts to fill the buffer to its capacity. On return, the buffer's length indicates the number of 		sample frames successfully converted.
--
-- ObjC selector: @- convertToBuffer:error:withInputFromBlock:@
convertToBuffer_error_withInputFromBlock :: (IsAVAudioConverter avAudioConverter, IsAVAudioBuffer outputBuffer, IsNSError outError) => avAudioConverter -> outputBuffer -> outError -> Ptr () -> IO AVAudioConverterOutputStatus
convertToBuffer_error_withInputFromBlock avAudioConverter outputBuffer outError inputBlock =
  sendMessage avAudioConverter convertToBuffer_error_withInputFromBlockSelector (toAVAudioBuffer outputBuffer) (toNSError outError) inputBlock

-- | inputFormat
--
-- The format of the input audio stream. (NB. AVAudioFormat includes the channel layout)
--
-- ObjC selector: @- inputFormat@
inputFormat :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id AVAudioFormat)
inputFormat avAudioConverter =
  sendMessage avAudioConverter inputFormatSelector

-- | outputFormat
--
-- The format of the output audio stream. (NB. AVAudioFormat includes the channel layout)
--
-- ObjC selector: @- outputFormat@
outputFormat :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id AVAudioFormat)
outputFormat avAudioConverter =
  sendMessage avAudioConverter outputFormatSelector

-- | channelMap
--
-- An array of integers indicating from which input to derive each output.
--
-- The array has size equal to the number of output channels. Each element's value is the input		channel number, starting with zero, that is to be copied to that output. A negative value		means that the output channel will have no source and will be silent. Setting a channel map		overrides channel mapping due to any channel layouts in the input and output formats that		may have been supplied.
--
-- ObjC selector: @- channelMap@
channelMap :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
channelMap avAudioConverter =
  sendMessage avAudioConverter channelMapSelector

-- | channelMap
--
-- An array of integers indicating from which input to derive each output.
--
-- The array has size equal to the number of output channels. Each element's value is the input		channel number, starting with zero, that is to be copied to that output. A negative value		means that the output channel will have no source and will be silent. Setting a channel map		overrides channel mapping due to any channel layouts in the input and output formats that		may have been supplied.
--
-- ObjC selector: @- setChannelMap:@
setChannelMap :: (IsAVAudioConverter avAudioConverter, IsNSArray value) => avAudioConverter -> value -> IO ()
setChannelMap avAudioConverter value =
  sendMessage avAudioConverter setChannelMapSelector (toNSArray value)

-- | magicCookie
--
-- Decoders require some data in the form of a magicCookie in order to decode properly.				Encoders will produce a magicCookie.
--
-- ObjC selector: @- magicCookie@
magicCookie :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSData)
magicCookie avAudioConverter =
  sendMessage avAudioConverter magicCookieSelector

-- | magicCookie
--
-- Decoders require some data in the form of a magicCookie in order to decode properly.				Encoders will produce a magicCookie.
--
-- ObjC selector: @- setMagicCookie:@
setMagicCookie :: (IsAVAudioConverter avAudioConverter, IsNSData value) => avAudioConverter -> value -> IO ()
setMagicCookie avAudioConverter value =
  sendMessage avAudioConverter setMagicCookieSelector (toNSData value)

-- | downmix
--
-- If YES and channel remapping is necessary, then channels will be mixed as    			appropriate instead of remapped. Default value is NO.
--
-- ObjC selector: @- downmix@
downmix :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO Bool
downmix avAudioConverter =
  sendMessage avAudioConverter downmixSelector

-- | downmix
--
-- If YES and channel remapping is necessary, then channels will be mixed as    			appropriate instead of remapped. Default value is NO.
--
-- ObjC selector: @- setDownmix:@
setDownmix :: IsAVAudioConverter avAudioConverter => avAudioConverter -> Bool -> IO ()
setDownmix avAudioConverter value =
  sendMessage avAudioConverter setDownmixSelector value

-- | dither
--
-- Setting YES will turn on dither, if dither makes sense in given the current formats    			and settings. Default value is NO.
--
-- ObjC selector: @- dither@
dither :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO Bool
dither avAudioConverter =
  sendMessage avAudioConverter ditherSelector

-- | dither
--
-- Setting YES will turn on dither, if dither makes sense in given the current formats    			and settings. Default value is NO.
--
-- ObjC selector: @- setDither:@
setDither :: IsAVAudioConverter avAudioConverter => avAudioConverter -> Bool -> IO ()
setDither avAudioConverter value =
  sendMessage avAudioConverter setDitherSelector value

-- | sampleRateConverterQuality
--
-- An AVAudioQuality value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- sampleRateConverterQuality@
sampleRateConverterQuality :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
sampleRateConverterQuality avAudioConverter =
  sendMessage avAudioConverter sampleRateConverterQualitySelector

-- | sampleRateConverterQuality
--
-- An AVAudioQuality value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- setSampleRateConverterQuality:@
setSampleRateConverterQuality :: IsAVAudioConverter avAudioConverter => avAudioConverter -> CLong -> IO ()
setSampleRateConverterQuality avAudioConverter value =
  sendMessage avAudioConverter setSampleRateConverterQualitySelector value

-- | sampleRateConverterAlgorithm
--
-- An AVSampleRateConverterAlgorithmKey value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- sampleRateConverterAlgorithm@
sampleRateConverterAlgorithm :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSString)
sampleRateConverterAlgorithm avAudioConverter =
  sendMessage avAudioConverter sampleRateConverterAlgorithmSelector

-- | sampleRateConverterAlgorithm
--
-- An AVSampleRateConverterAlgorithmKey value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- setSampleRateConverterAlgorithm:@
setSampleRateConverterAlgorithm :: (IsAVAudioConverter avAudioConverter, IsNSString value) => avAudioConverter -> value -> IO ()
setSampleRateConverterAlgorithm avAudioConverter value =
  sendMessage avAudioConverter setSampleRateConverterAlgorithmSelector (toNSString value)

-- | primeMethod
--
-- Indicates the priming method to be used by the sample rate converter or decoder.
--
-- ObjC selector: @- primeMethod@
primeMethod :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioConverterPrimeMethod
primeMethod avAudioConverter =
  sendMessage avAudioConverter primeMethodSelector

-- | primeMethod
--
-- Indicates the priming method to be used by the sample rate converter or decoder.
--
-- ObjC selector: @- setPrimeMethod:@
setPrimeMethod :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioConverterPrimeMethod -> IO ()
setPrimeMethod avAudioConverter value =
  sendMessage avAudioConverter setPrimeMethodSelector value

-- | primeInfo
--
-- Indicates the the number of priming frames.
--
-- ObjC selector: @- primeInfo@
primeInfo :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioConverterPrimeInfo
primeInfo avAudioConverter =
  sendMessage avAudioConverter primeInfoSelector

-- | primeInfo
--
-- Indicates the the number of priming frames.
--
-- ObjC selector: @- setPrimeInfo:@
setPrimeInfo :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioConverterPrimeInfo -> IO ()
setPrimeInfo avAudioConverter value =
  sendMessage avAudioConverter setPrimeInfoSelector value

-- | audioSyncPacketFrequency
--
-- Number of packets between consecutive sync packets.
--
-- A sync packet is an independently-decodable packet that completely refreshes the decoder without		needing to decode other packets.  When compressing to a format which supports it (such as APAC),		the audio sync packet frequency indicates the distance in packets between two sync packets, with		non-sync packets between.  This is useful to set when saving compressed packets to a file and		efficient random access is desired.  Note: Separating sync packets by at least one second of		encoded audio (e.g. 75 packets) is recommended.
--
-- ObjC selector: @- audioSyncPacketFrequency@
audioSyncPacketFrequency :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
audioSyncPacketFrequency avAudioConverter =
  sendMessage avAudioConverter audioSyncPacketFrequencySelector

-- | audioSyncPacketFrequency
--
-- Number of packets between consecutive sync packets.
--
-- A sync packet is an independently-decodable packet that completely refreshes the decoder without		needing to decode other packets.  When compressing to a format which supports it (such as APAC),		the audio sync packet frequency indicates the distance in packets between two sync packets, with		non-sync packets between.  This is useful to set when saving compressed packets to a file and		efficient random access is desired.  Note: Separating sync packets by at least one second of		encoded audio (e.g. 75 packets) is recommended.
--
-- ObjC selector: @- setAudioSyncPacketFrequency:@
setAudioSyncPacketFrequency :: IsAVAudioConverter avAudioConverter => avAudioConverter -> CLong -> IO ()
setAudioSyncPacketFrequency avAudioConverter value =
  sendMessage avAudioConverter setAudioSyncPacketFrequencySelector value

-- | contentSource
--
-- Index to select a pre-defined content source type that describes the content type and    			how it was generated.  Note: This is only supported when compressing audio to formats    			which support it.
--
-- ObjC selector: @- contentSource@
contentSource :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioContentSource
contentSource avAudioConverter =
  sendMessage avAudioConverter contentSourceSelector

-- | contentSource
--
-- Index to select a pre-defined content source type that describes the content type and    			how it was generated.  Note: This is only supported when compressing audio to formats    			which support it.
--
-- ObjC selector: @- setContentSource:@
setContentSource :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioContentSource -> IO ()
setContentSource avAudioConverter value =
  sendMessage avAudioConverter setContentSourceSelector value

-- | dynamicRangeControlConfiguration
--
-- Encoder Dynamic Range Control (DRC) configuration.
--
-- When supported by the encoder, this property controls which configuration is applied when a		bitstream is generated.  Note: This is only supported when compressing audio to formats		which support it.
--
-- ObjC selector: @- dynamicRangeControlConfiguration@
dynamicRangeControlConfiguration :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioDynamicRangeControlConfiguration
dynamicRangeControlConfiguration avAudioConverter =
  sendMessage avAudioConverter dynamicRangeControlConfigurationSelector

-- | dynamicRangeControlConfiguration
--
-- Encoder Dynamic Range Control (DRC) configuration.
--
-- When supported by the encoder, this property controls which configuration is applied when a		bitstream is generated.  Note: This is only supported when compressing audio to formats		which support it.
--
-- ObjC selector: @- setDynamicRangeControlConfiguration:@
setDynamicRangeControlConfiguration :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioDynamicRangeControlConfiguration -> IO ()
setDynamicRangeControlConfiguration avAudioConverter value =
  sendMessage avAudioConverter setDynamicRangeControlConfigurationSelector value

-- | bitRate
--
-- bitRate in bits per second. Only applies when encoding.
--
-- ObjC selector: @- bitRate@
bitRate :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
bitRate avAudioConverter =
  sendMessage avAudioConverter bitRateSelector

-- | bitRate
--
-- bitRate in bits per second. Only applies when encoding.
--
-- ObjC selector: @- setBitRate:@
setBitRate :: IsAVAudioConverter avAudioConverter => avAudioConverter -> CLong -> IO ()
setBitRate avAudioConverter value =
  sendMessage avAudioConverter setBitRateSelector value

-- | bitRateStrategy
--
-- When encoding, an AVEncoderBitRateStrategyKey value constant as defined in AVAudioSettings.h. Returns nil if not encoding.
--
-- ObjC selector: @- bitRateStrategy@
bitRateStrategy :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSString)
bitRateStrategy avAudioConverter =
  sendMessage avAudioConverter bitRateStrategySelector

-- | bitRateStrategy
--
-- When encoding, an AVEncoderBitRateStrategyKey value constant as defined in AVAudioSettings.h. Returns nil if not encoding.
--
-- ObjC selector: @- setBitRateStrategy:@
setBitRateStrategy :: (IsAVAudioConverter avAudioConverter, IsNSString value) => avAudioConverter -> value -> IO ()
setBitRateStrategy avAudioConverter value =
  sendMessage avAudioConverter setBitRateStrategySelector (toNSString value)

-- | maximumOutputPacketSize
--
-- The maximum size of an output packet, in bytes.
--
-- When encoding it is useful to know how large a packet can be in order to allocate a buffer to receive the output.
--
-- ObjC selector: @- maximumOutputPacketSize@
maximumOutputPacketSize :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
maximumOutputPacketSize avAudioConverter =
  sendMessage avAudioConverter maximumOutputPacketSizeSelector

-- | availableEncodeBitRates
--
-- When encoding, an NSArray of NSNumber of all bit rates provided by the codec. Returns nil if not encoding.
--
-- ObjC selector: @- availableEncodeBitRates@
availableEncodeBitRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
availableEncodeBitRates avAudioConverter =
  sendMessage avAudioConverter availableEncodeBitRatesSelector

-- | applicableEncodeBitRates
--
-- When encoding, an NSArray of NSNumber of bit rates that can be applied based on the current formats and settings. Returns nil if not encoding.
--
-- ObjC selector: @- applicableEncodeBitRates@
applicableEncodeBitRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
applicableEncodeBitRates avAudioConverter =
  sendMessage avAudioConverter applicableEncodeBitRatesSelector

-- | availableEncodeSampleRates
--
-- When encoding, an NSArray of NSNumber of all output sample rates provided by the codec. Returns nil if not encoding.
--
-- ObjC selector: @- availableEncodeSampleRates@
availableEncodeSampleRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
availableEncodeSampleRates avAudioConverter =
  sendMessage avAudioConverter availableEncodeSampleRatesSelector

-- | applicableEncodeSampleRates
--
-- When encoding, an NSArray of NSNumber of output sample rates that can be applied based on the current formats and settings. Returns nil if not encoding.
--
-- ObjC selector: @- applicableEncodeSampleRates@
applicableEncodeSampleRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
applicableEncodeSampleRates avAudioConverter =
  sendMessage avAudioConverter applicableEncodeSampleRatesSelector

-- | availableEncodeChannelLayoutTags
--
-- When encoding, an NSArray of NSNumber of all output channel layout tags provided by the codec. Returns nil if not encoding.
--
-- ObjC selector: @- availableEncodeChannelLayoutTags@
availableEncodeChannelLayoutTags :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
availableEncodeChannelLayoutTags avAudioConverter =
  sendMessage avAudioConverter availableEncodeChannelLayoutTagsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initFromFormat:toFormat:@
initFromFormat_toFormatSelector :: Selector '[Id AVAudioFormat, Id AVAudioFormat] (Id AVAudioConverter)
initFromFormat_toFormatSelector = mkSelector "initFromFormat:toFormat:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @convertToBuffer:fromBuffer:error:@
convertToBuffer_fromBuffer_errorSelector :: Selector '[Id AVAudioPCMBuffer, Const (Id AVAudioPCMBuffer), Id NSError] Bool
convertToBuffer_fromBuffer_errorSelector = mkSelector "convertToBuffer:fromBuffer:error:"

-- | @Selector@ for @convertToBuffer:error:withInputFromBlock:@
convertToBuffer_error_withInputFromBlockSelector :: Selector '[Id AVAudioBuffer, Id NSError, Ptr ()] AVAudioConverterOutputStatus
convertToBuffer_error_withInputFromBlockSelector = mkSelector "convertToBuffer:error:withInputFromBlock:"

-- | @Selector@ for @inputFormat@
inputFormatSelector :: Selector '[] (Id AVAudioFormat)
inputFormatSelector = mkSelector "inputFormat"

-- | @Selector@ for @outputFormat@
outputFormatSelector :: Selector '[] (Id AVAudioFormat)
outputFormatSelector = mkSelector "outputFormat"

-- | @Selector@ for @channelMap@
channelMapSelector :: Selector '[] (Id NSArray)
channelMapSelector = mkSelector "channelMap"

-- | @Selector@ for @setChannelMap:@
setChannelMapSelector :: Selector '[Id NSArray] ()
setChannelMapSelector = mkSelector "setChannelMap:"

-- | @Selector@ for @magicCookie@
magicCookieSelector :: Selector '[] (Id NSData)
magicCookieSelector = mkSelector "magicCookie"

-- | @Selector@ for @setMagicCookie:@
setMagicCookieSelector :: Selector '[Id NSData] ()
setMagicCookieSelector = mkSelector "setMagicCookie:"

-- | @Selector@ for @downmix@
downmixSelector :: Selector '[] Bool
downmixSelector = mkSelector "downmix"

-- | @Selector@ for @setDownmix:@
setDownmixSelector :: Selector '[Bool] ()
setDownmixSelector = mkSelector "setDownmix:"

-- | @Selector@ for @dither@
ditherSelector :: Selector '[] Bool
ditherSelector = mkSelector "dither"

-- | @Selector@ for @setDither:@
setDitherSelector :: Selector '[Bool] ()
setDitherSelector = mkSelector "setDither:"

-- | @Selector@ for @sampleRateConverterQuality@
sampleRateConverterQualitySelector :: Selector '[] CLong
sampleRateConverterQualitySelector = mkSelector "sampleRateConverterQuality"

-- | @Selector@ for @setSampleRateConverterQuality:@
setSampleRateConverterQualitySelector :: Selector '[CLong] ()
setSampleRateConverterQualitySelector = mkSelector "setSampleRateConverterQuality:"

-- | @Selector@ for @sampleRateConverterAlgorithm@
sampleRateConverterAlgorithmSelector :: Selector '[] (Id NSString)
sampleRateConverterAlgorithmSelector = mkSelector "sampleRateConverterAlgorithm"

-- | @Selector@ for @setSampleRateConverterAlgorithm:@
setSampleRateConverterAlgorithmSelector :: Selector '[Id NSString] ()
setSampleRateConverterAlgorithmSelector = mkSelector "setSampleRateConverterAlgorithm:"

-- | @Selector@ for @primeMethod@
primeMethodSelector :: Selector '[] AVAudioConverterPrimeMethod
primeMethodSelector = mkSelector "primeMethod"

-- | @Selector@ for @setPrimeMethod:@
setPrimeMethodSelector :: Selector '[AVAudioConverterPrimeMethod] ()
setPrimeMethodSelector = mkSelector "setPrimeMethod:"

-- | @Selector@ for @primeInfo@
primeInfoSelector :: Selector '[] AVAudioConverterPrimeInfo
primeInfoSelector = mkSelector "primeInfo"

-- | @Selector@ for @setPrimeInfo:@
setPrimeInfoSelector :: Selector '[AVAudioConverterPrimeInfo] ()
setPrimeInfoSelector = mkSelector "setPrimeInfo:"

-- | @Selector@ for @audioSyncPacketFrequency@
audioSyncPacketFrequencySelector :: Selector '[] CLong
audioSyncPacketFrequencySelector = mkSelector "audioSyncPacketFrequency"

-- | @Selector@ for @setAudioSyncPacketFrequency:@
setAudioSyncPacketFrequencySelector :: Selector '[CLong] ()
setAudioSyncPacketFrequencySelector = mkSelector "setAudioSyncPacketFrequency:"

-- | @Selector@ for @contentSource@
contentSourceSelector :: Selector '[] AVAudioContentSource
contentSourceSelector = mkSelector "contentSource"

-- | @Selector@ for @setContentSource:@
setContentSourceSelector :: Selector '[AVAudioContentSource] ()
setContentSourceSelector = mkSelector "setContentSource:"

-- | @Selector@ for @dynamicRangeControlConfiguration@
dynamicRangeControlConfigurationSelector :: Selector '[] AVAudioDynamicRangeControlConfiguration
dynamicRangeControlConfigurationSelector = mkSelector "dynamicRangeControlConfiguration"

-- | @Selector@ for @setDynamicRangeControlConfiguration:@
setDynamicRangeControlConfigurationSelector :: Selector '[AVAudioDynamicRangeControlConfiguration] ()
setDynamicRangeControlConfigurationSelector = mkSelector "setDynamicRangeControlConfiguration:"

-- | @Selector@ for @bitRate@
bitRateSelector :: Selector '[] CLong
bitRateSelector = mkSelector "bitRate"

-- | @Selector@ for @setBitRate:@
setBitRateSelector :: Selector '[CLong] ()
setBitRateSelector = mkSelector "setBitRate:"

-- | @Selector@ for @bitRateStrategy@
bitRateStrategySelector :: Selector '[] (Id NSString)
bitRateStrategySelector = mkSelector "bitRateStrategy"

-- | @Selector@ for @setBitRateStrategy:@
setBitRateStrategySelector :: Selector '[Id NSString] ()
setBitRateStrategySelector = mkSelector "setBitRateStrategy:"

-- | @Selector@ for @maximumOutputPacketSize@
maximumOutputPacketSizeSelector :: Selector '[] CLong
maximumOutputPacketSizeSelector = mkSelector "maximumOutputPacketSize"

-- | @Selector@ for @availableEncodeBitRates@
availableEncodeBitRatesSelector :: Selector '[] (Id NSArray)
availableEncodeBitRatesSelector = mkSelector "availableEncodeBitRates"

-- | @Selector@ for @applicableEncodeBitRates@
applicableEncodeBitRatesSelector :: Selector '[] (Id NSArray)
applicableEncodeBitRatesSelector = mkSelector "applicableEncodeBitRates"

-- | @Selector@ for @availableEncodeSampleRates@
availableEncodeSampleRatesSelector :: Selector '[] (Id NSArray)
availableEncodeSampleRatesSelector = mkSelector "availableEncodeSampleRates"

-- | @Selector@ for @applicableEncodeSampleRates@
applicableEncodeSampleRatesSelector :: Selector '[] (Id NSArray)
applicableEncodeSampleRatesSelector = mkSelector "applicableEncodeSampleRates"

-- | @Selector@ for @availableEncodeChannelLayoutTags@
availableEncodeChannelLayoutTagsSelector :: Selector '[] (Id NSArray)
availableEncodeChannelLayoutTagsSelector = mkSelector "availableEncodeChannelLayoutTags"

