{-# LANGUAGE PatternSynonyms #-}
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
  , initFromFormat_toFormatSelector
  , resetSelector
  , convertToBuffer_fromBuffer_errorSelector
  , convertToBuffer_error_withInputFromBlockSelector
  , inputFormatSelector
  , outputFormatSelector
  , channelMapSelector
  , setChannelMapSelector
  , magicCookieSelector
  , setMagicCookieSelector
  , downmixSelector
  , setDownmixSelector
  , ditherSelector
  , setDitherSelector
  , sampleRateConverterQualitySelector
  , setSampleRateConverterQualitySelector
  , sampleRateConverterAlgorithmSelector
  , setSampleRateConverterAlgorithmSelector
  , primeMethodSelector
  , setPrimeMethodSelector
  , primeInfoSelector
  , setPrimeInfoSelector
  , audioSyncPacketFrequencySelector
  , setAudioSyncPacketFrequencySelector
  , contentSourceSelector
  , setContentSourceSelector
  , dynamicRangeControlConfigurationSelector
  , setDynamicRangeControlConfigurationSelector
  , bitRateSelector
  , setBitRateSelector
  , bitRateStrategySelector
  , setBitRateStrategySelector
  , maximumOutputPacketSizeSelector
  , availableEncodeBitRatesSelector
  , applicableEncodeBitRatesSelector
  , availableEncodeSampleRatesSelector
  , applicableEncodeSampleRatesSelector
  , availableEncodeChannelLayoutTagsSelector

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
initFromFormat_toFormat avAudioConverter  fromFormat toFormat =
withObjCPtr fromFormat $ \raw_fromFormat ->
  withObjCPtr toFormat $ \raw_toFormat ->
      sendMsg avAudioConverter (mkSelector "initFromFormat:toFormat:") (retPtr retVoid) [argPtr (castPtr raw_fromFormat :: Ptr ()), argPtr (castPtr raw_toFormat :: Ptr ())] >>= ownedObject . castPtr

-- | reset
--
-- Resets the converter so that a new stream may be converted.
--
-- ObjC selector: @- reset@
reset :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO ()
reset avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "reset") retVoid []

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
convertToBuffer_fromBuffer_error avAudioConverter  outputBuffer inputBuffer outError =
withObjCPtr outputBuffer $ \raw_outputBuffer ->
  withObjCPtr inputBuffer $ \raw_inputBuffer ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioConverter (mkSelector "convertToBuffer:fromBuffer:error:") retCULong [argPtr (castPtr raw_outputBuffer :: Ptr ()), argPtr (castPtr raw_inputBuffer :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

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
convertToBuffer_error_withInputFromBlock avAudioConverter  outputBuffer outError inputBlock =
withObjCPtr outputBuffer $ \raw_outputBuffer ->
  withObjCPtr outError $ \raw_outError ->
      fmap (coerce :: CLong -> AVAudioConverterOutputStatus) $ sendMsg avAudioConverter (mkSelector "convertToBuffer:error:withInputFromBlock:") retCLong [argPtr (castPtr raw_outputBuffer :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ()), argPtr (castPtr inputBlock :: Ptr ())]

-- | inputFormat
--
-- The format of the input audio stream. (NB. AVAudioFormat includes the channel layout)
--
-- ObjC selector: @- inputFormat@
inputFormat :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id AVAudioFormat)
inputFormat avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "inputFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputFormat
--
-- The format of the output audio stream. (NB. AVAudioFormat includes the channel layout)
--
-- ObjC selector: @- outputFormat@
outputFormat :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id AVAudioFormat)
outputFormat avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "outputFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | channelMap
--
-- An array of integers indicating from which input to derive each output.
--
-- The array has size equal to the number of output channels. Each element's value is the input		channel number, starting with zero, that is to be copied to that output. A negative value		means that the output channel will have no source and will be silent. Setting a channel map		overrides channel mapping due to any channel layouts in the input and output formats that		may have been supplied.
--
-- ObjC selector: @- channelMap@
channelMap :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
channelMap avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "channelMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | channelMap
--
-- An array of integers indicating from which input to derive each output.
--
-- The array has size equal to the number of output channels. Each element's value is the input		channel number, starting with zero, that is to be copied to that output. A negative value		means that the output channel will have no source and will be silent. Setting a channel map		overrides channel mapping due to any channel layouts in the input and output formats that		may have been supplied.
--
-- ObjC selector: @- setChannelMap:@
setChannelMap :: (IsAVAudioConverter avAudioConverter, IsNSArray value) => avAudioConverter -> value -> IO ()
setChannelMap avAudioConverter  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAudioConverter (mkSelector "setChannelMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | magicCookie
--
-- Decoders require some data in the form of a magicCookie in order to decode properly.				Encoders will produce a magicCookie.
--
-- ObjC selector: @- magicCookie@
magicCookie :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSData)
magicCookie avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "magicCookie") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | magicCookie
--
-- Decoders require some data in the form of a magicCookie in order to decode properly.				Encoders will produce a magicCookie.
--
-- ObjC selector: @- setMagicCookie:@
setMagicCookie :: (IsAVAudioConverter avAudioConverter, IsNSData value) => avAudioConverter -> value -> IO ()
setMagicCookie avAudioConverter  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAudioConverter (mkSelector "setMagicCookie:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | downmix
--
-- If YES and channel remapping is necessary, then channels will be mixed as    			appropriate instead of remapped. Default value is NO.
--
-- ObjC selector: @- downmix@
downmix :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO Bool
downmix avAudioConverter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioConverter (mkSelector "downmix") retCULong []

-- | downmix
--
-- If YES and channel remapping is necessary, then channels will be mixed as    			appropriate instead of remapped. Default value is NO.
--
-- ObjC selector: @- setDownmix:@
setDownmix :: IsAVAudioConverter avAudioConverter => avAudioConverter -> Bool -> IO ()
setDownmix avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setDownmix:") retVoid [argCULong (if value then 1 else 0)]

-- | dither
--
-- Setting YES will turn on dither, if dither makes sense in given the current formats    			and settings. Default value is NO.
--
-- ObjC selector: @- dither@
dither :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO Bool
dither avAudioConverter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioConverter (mkSelector "dither") retCULong []

-- | dither
--
-- Setting YES will turn on dither, if dither makes sense in given the current formats    			and settings. Default value is NO.
--
-- ObjC selector: @- setDither:@
setDither :: IsAVAudioConverter avAudioConverter => avAudioConverter -> Bool -> IO ()
setDither avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setDither:") retVoid [argCULong (if value then 1 else 0)]

-- | sampleRateConverterQuality
--
-- An AVAudioQuality value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- sampleRateConverterQuality@
sampleRateConverterQuality :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
sampleRateConverterQuality avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "sampleRateConverterQuality") retCLong []

-- | sampleRateConverterQuality
--
-- An AVAudioQuality value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- setSampleRateConverterQuality:@
setSampleRateConverterQuality :: IsAVAudioConverter avAudioConverter => avAudioConverter -> CLong -> IO ()
setSampleRateConverterQuality avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setSampleRateConverterQuality:") retVoid [argCLong (fromIntegral value)]

-- | sampleRateConverterAlgorithm
--
-- An AVSampleRateConverterAlgorithmKey value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- sampleRateConverterAlgorithm@
sampleRateConverterAlgorithm :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSString)
sampleRateConverterAlgorithm avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "sampleRateConverterAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleRateConverterAlgorithm
--
-- An AVSampleRateConverterAlgorithmKey value as defined in AVAudioSettings.h.
--
-- ObjC selector: @- setSampleRateConverterAlgorithm:@
setSampleRateConverterAlgorithm :: (IsAVAudioConverter avAudioConverter, IsNSString value) => avAudioConverter -> value -> IO ()
setSampleRateConverterAlgorithm avAudioConverter  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAudioConverter (mkSelector "setSampleRateConverterAlgorithm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | primeMethod
--
-- Indicates the priming method to be used by the sample rate converter or decoder.
--
-- ObjC selector: @- primeMethod@
primeMethod :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioConverterPrimeMethod
primeMethod avAudioConverter  =
  fmap (coerce :: CLong -> AVAudioConverterPrimeMethod) $ sendMsg avAudioConverter (mkSelector "primeMethod") retCLong []

-- | primeMethod
--
-- Indicates the priming method to be used by the sample rate converter or decoder.
--
-- ObjC selector: @- setPrimeMethod:@
setPrimeMethod :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioConverterPrimeMethod -> IO ()
setPrimeMethod avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setPrimeMethod:") retVoid [argCLong (coerce value)]

-- | primeInfo
--
-- Indicates the the number of priming frames.
--
-- ObjC selector: @- primeInfo@
primeInfo :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioConverterPrimeInfo
primeInfo avAudioConverter  =
  sendMsgStret avAudioConverter (mkSelector "primeInfo") retAVAudioConverterPrimeInfo []

-- | primeInfo
--
-- Indicates the the number of priming frames.
--
-- ObjC selector: @- setPrimeInfo:@
setPrimeInfo :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioConverterPrimeInfo -> IO ()
setPrimeInfo avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setPrimeInfo:") retVoid [argAVAudioConverterPrimeInfo value]

-- | audioSyncPacketFrequency
--
-- Number of packets between consecutive sync packets.
--
-- A sync packet is an independently-decodable packet that completely refreshes the decoder without		needing to decode other packets.  When compressing to a format which supports it (such as APAC),		the audio sync packet frequency indicates the distance in packets between two sync packets, with		non-sync packets between.  This is useful to set when saving compressed packets to a file and		efficient random access is desired.  Note: Separating sync packets by at least one second of		encoded audio (e.g. 75 packets) is recommended.
--
-- ObjC selector: @- audioSyncPacketFrequency@
audioSyncPacketFrequency :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
audioSyncPacketFrequency avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "audioSyncPacketFrequency") retCLong []

-- | audioSyncPacketFrequency
--
-- Number of packets between consecutive sync packets.
--
-- A sync packet is an independently-decodable packet that completely refreshes the decoder without		needing to decode other packets.  When compressing to a format which supports it (such as APAC),		the audio sync packet frequency indicates the distance in packets between two sync packets, with		non-sync packets between.  This is useful to set when saving compressed packets to a file and		efficient random access is desired.  Note: Separating sync packets by at least one second of		encoded audio (e.g. 75 packets) is recommended.
--
-- ObjC selector: @- setAudioSyncPacketFrequency:@
setAudioSyncPacketFrequency :: IsAVAudioConverter avAudioConverter => avAudioConverter -> CLong -> IO ()
setAudioSyncPacketFrequency avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setAudioSyncPacketFrequency:") retVoid [argCLong (fromIntegral value)]

-- | contentSource
--
-- Index to select a pre-defined content source type that describes the content type and    			how it was generated.  Note: This is only supported when compressing audio to formats    			which support it.
--
-- ObjC selector: @- contentSource@
contentSource :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioContentSource
contentSource avAudioConverter  =
  fmap (coerce :: CLong -> AVAudioContentSource) $ sendMsg avAudioConverter (mkSelector "contentSource") retCLong []

-- | contentSource
--
-- Index to select a pre-defined content source type that describes the content type and    			how it was generated.  Note: This is only supported when compressing audio to formats    			which support it.
--
-- ObjC selector: @- setContentSource:@
setContentSource :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioContentSource -> IO ()
setContentSource avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setContentSource:") retVoid [argCLong (coerce value)]

-- | dynamicRangeControlConfiguration
--
-- Encoder Dynamic Range Control (DRC) configuration.
--
-- When supported by the encoder, this property controls which configuration is applied when a		bitstream is generated.  Note: This is only supported when compressing audio to formats		which support it.
--
-- ObjC selector: @- dynamicRangeControlConfiguration@
dynamicRangeControlConfiguration :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO AVAudioDynamicRangeControlConfiguration
dynamicRangeControlConfiguration avAudioConverter  =
  fmap (coerce :: CLong -> AVAudioDynamicRangeControlConfiguration) $ sendMsg avAudioConverter (mkSelector "dynamicRangeControlConfiguration") retCLong []

-- | dynamicRangeControlConfiguration
--
-- Encoder Dynamic Range Control (DRC) configuration.
--
-- When supported by the encoder, this property controls which configuration is applied when a		bitstream is generated.  Note: This is only supported when compressing audio to formats		which support it.
--
-- ObjC selector: @- setDynamicRangeControlConfiguration:@
setDynamicRangeControlConfiguration :: IsAVAudioConverter avAudioConverter => avAudioConverter -> AVAudioDynamicRangeControlConfiguration -> IO ()
setDynamicRangeControlConfiguration avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setDynamicRangeControlConfiguration:") retVoid [argCLong (coerce value)]

-- | bitRate
--
-- bitRate in bits per second. Only applies when encoding.
--
-- ObjC selector: @- bitRate@
bitRate :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
bitRate avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "bitRate") retCLong []

-- | bitRate
--
-- bitRate in bits per second. Only applies when encoding.
--
-- ObjC selector: @- setBitRate:@
setBitRate :: IsAVAudioConverter avAudioConverter => avAudioConverter -> CLong -> IO ()
setBitRate avAudioConverter  value =
  sendMsg avAudioConverter (mkSelector "setBitRate:") retVoid [argCLong (fromIntegral value)]

-- | bitRateStrategy
--
-- When encoding, an AVEncoderBitRateStrategyKey value constant as defined in AVAudioSettings.h. Returns nil if not encoding.
--
-- ObjC selector: @- bitRateStrategy@
bitRateStrategy :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSString)
bitRateStrategy avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "bitRateStrategy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | bitRateStrategy
--
-- When encoding, an AVEncoderBitRateStrategyKey value constant as defined in AVAudioSettings.h. Returns nil if not encoding.
--
-- ObjC selector: @- setBitRateStrategy:@
setBitRateStrategy :: (IsAVAudioConverter avAudioConverter, IsNSString value) => avAudioConverter -> value -> IO ()
setBitRateStrategy avAudioConverter  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAudioConverter (mkSelector "setBitRateStrategy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | maximumOutputPacketSize
--
-- The maximum size of an output packet, in bytes.
--
-- When encoding it is useful to know how large a packet can be in order to allocate a buffer to receive the output.
--
-- ObjC selector: @- maximumOutputPacketSize@
maximumOutputPacketSize :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO CLong
maximumOutputPacketSize avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "maximumOutputPacketSize") retCLong []

-- | availableEncodeBitRates
--
-- When encoding, an NSArray of NSNumber of all bit rates provided by the codec. Returns nil if not encoding.
--
-- ObjC selector: @- availableEncodeBitRates@
availableEncodeBitRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
availableEncodeBitRates avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "availableEncodeBitRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicableEncodeBitRates
--
-- When encoding, an NSArray of NSNumber of bit rates that can be applied based on the current formats and settings. Returns nil if not encoding.
--
-- ObjC selector: @- applicableEncodeBitRates@
applicableEncodeBitRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
applicableEncodeBitRates avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "applicableEncodeBitRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | availableEncodeSampleRates
--
-- When encoding, an NSArray of NSNumber of all output sample rates provided by the codec. Returns nil if not encoding.
--
-- ObjC selector: @- availableEncodeSampleRates@
availableEncodeSampleRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
availableEncodeSampleRates avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "availableEncodeSampleRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicableEncodeSampleRates
--
-- When encoding, an NSArray of NSNumber of output sample rates that can be applied based on the current formats and settings. Returns nil if not encoding.
--
-- ObjC selector: @- applicableEncodeSampleRates@
applicableEncodeSampleRates :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
applicableEncodeSampleRates avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "applicableEncodeSampleRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | availableEncodeChannelLayoutTags
--
-- When encoding, an NSArray of NSNumber of all output channel layout tags provided by the codec. Returns nil if not encoding.
--
-- ObjC selector: @- availableEncodeChannelLayoutTags@
availableEncodeChannelLayoutTags :: IsAVAudioConverter avAudioConverter => avAudioConverter -> IO (Id NSArray)
availableEncodeChannelLayoutTags avAudioConverter  =
  sendMsg avAudioConverter (mkSelector "availableEncodeChannelLayoutTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initFromFormat:toFormat:@
initFromFormat_toFormatSelector :: Selector
initFromFormat_toFormatSelector = mkSelector "initFromFormat:toFormat:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @convertToBuffer:fromBuffer:error:@
convertToBuffer_fromBuffer_errorSelector :: Selector
convertToBuffer_fromBuffer_errorSelector = mkSelector "convertToBuffer:fromBuffer:error:"

-- | @Selector@ for @convertToBuffer:error:withInputFromBlock:@
convertToBuffer_error_withInputFromBlockSelector :: Selector
convertToBuffer_error_withInputFromBlockSelector = mkSelector "convertToBuffer:error:withInputFromBlock:"

-- | @Selector@ for @inputFormat@
inputFormatSelector :: Selector
inputFormatSelector = mkSelector "inputFormat"

-- | @Selector@ for @outputFormat@
outputFormatSelector :: Selector
outputFormatSelector = mkSelector "outputFormat"

-- | @Selector@ for @channelMap@
channelMapSelector :: Selector
channelMapSelector = mkSelector "channelMap"

-- | @Selector@ for @setChannelMap:@
setChannelMapSelector :: Selector
setChannelMapSelector = mkSelector "setChannelMap:"

-- | @Selector@ for @magicCookie@
magicCookieSelector :: Selector
magicCookieSelector = mkSelector "magicCookie"

-- | @Selector@ for @setMagicCookie:@
setMagicCookieSelector :: Selector
setMagicCookieSelector = mkSelector "setMagicCookie:"

-- | @Selector@ for @downmix@
downmixSelector :: Selector
downmixSelector = mkSelector "downmix"

-- | @Selector@ for @setDownmix:@
setDownmixSelector :: Selector
setDownmixSelector = mkSelector "setDownmix:"

-- | @Selector@ for @dither@
ditherSelector :: Selector
ditherSelector = mkSelector "dither"

-- | @Selector@ for @setDither:@
setDitherSelector :: Selector
setDitherSelector = mkSelector "setDither:"

-- | @Selector@ for @sampleRateConverterQuality@
sampleRateConverterQualitySelector :: Selector
sampleRateConverterQualitySelector = mkSelector "sampleRateConverterQuality"

-- | @Selector@ for @setSampleRateConverterQuality:@
setSampleRateConverterQualitySelector :: Selector
setSampleRateConverterQualitySelector = mkSelector "setSampleRateConverterQuality:"

-- | @Selector@ for @sampleRateConverterAlgorithm@
sampleRateConverterAlgorithmSelector :: Selector
sampleRateConverterAlgorithmSelector = mkSelector "sampleRateConverterAlgorithm"

-- | @Selector@ for @setSampleRateConverterAlgorithm:@
setSampleRateConverterAlgorithmSelector :: Selector
setSampleRateConverterAlgorithmSelector = mkSelector "setSampleRateConverterAlgorithm:"

-- | @Selector@ for @primeMethod@
primeMethodSelector :: Selector
primeMethodSelector = mkSelector "primeMethod"

-- | @Selector@ for @setPrimeMethod:@
setPrimeMethodSelector :: Selector
setPrimeMethodSelector = mkSelector "setPrimeMethod:"

-- | @Selector@ for @primeInfo@
primeInfoSelector :: Selector
primeInfoSelector = mkSelector "primeInfo"

-- | @Selector@ for @setPrimeInfo:@
setPrimeInfoSelector :: Selector
setPrimeInfoSelector = mkSelector "setPrimeInfo:"

-- | @Selector@ for @audioSyncPacketFrequency@
audioSyncPacketFrequencySelector :: Selector
audioSyncPacketFrequencySelector = mkSelector "audioSyncPacketFrequency"

-- | @Selector@ for @setAudioSyncPacketFrequency:@
setAudioSyncPacketFrequencySelector :: Selector
setAudioSyncPacketFrequencySelector = mkSelector "setAudioSyncPacketFrequency:"

-- | @Selector@ for @contentSource@
contentSourceSelector :: Selector
contentSourceSelector = mkSelector "contentSource"

-- | @Selector@ for @setContentSource:@
setContentSourceSelector :: Selector
setContentSourceSelector = mkSelector "setContentSource:"

-- | @Selector@ for @dynamicRangeControlConfiguration@
dynamicRangeControlConfigurationSelector :: Selector
dynamicRangeControlConfigurationSelector = mkSelector "dynamicRangeControlConfiguration"

-- | @Selector@ for @setDynamicRangeControlConfiguration:@
setDynamicRangeControlConfigurationSelector :: Selector
setDynamicRangeControlConfigurationSelector = mkSelector "setDynamicRangeControlConfiguration:"

-- | @Selector@ for @bitRate@
bitRateSelector :: Selector
bitRateSelector = mkSelector "bitRate"

-- | @Selector@ for @setBitRate:@
setBitRateSelector :: Selector
setBitRateSelector = mkSelector "setBitRate:"

-- | @Selector@ for @bitRateStrategy@
bitRateStrategySelector :: Selector
bitRateStrategySelector = mkSelector "bitRateStrategy"

-- | @Selector@ for @setBitRateStrategy:@
setBitRateStrategySelector :: Selector
setBitRateStrategySelector = mkSelector "setBitRateStrategy:"

-- | @Selector@ for @maximumOutputPacketSize@
maximumOutputPacketSizeSelector :: Selector
maximumOutputPacketSizeSelector = mkSelector "maximumOutputPacketSize"

-- | @Selector@ for @availableEncodeBitRates@
availableEncodeBitRatesSelector :: Selector
availableEncodeBitRatesSelector = mkSelector "availableEncodeBitRates"

-- | @Selector@ for @applicableEncodeBitRates@
applicableEncodeBitRatesSelector :: Selector
applicableEncodeBitRatesSelector = mkSelector "applicableEncodeBitRates"

-- | @Selector@ for @availableEncodeSampleRates@
availableEncodeSampleRatesSelector :: Selector
availableEncodeSampleRatesSelector = mkSelector "availableEncodeSampleRates"

-- | @Selector@ for @applicableEncodeSampleRates@
applicableEncodeSampleRatesSelector :: Selector
applicableEncodeSampleRatesSelector = mkSelector "applicableEncodeSampleRates"

-- | @Selector@ for @availableEncodeChannelLayoutTags@
availableEncodeChannelLayoutTagsSelector :: Selector
availableEncodeChannelLayoutTagsSelector = mkSelector "availableEncodeChannelLayoutTags"

