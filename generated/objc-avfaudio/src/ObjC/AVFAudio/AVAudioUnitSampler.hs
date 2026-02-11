{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitSampler
--
-- Apple's sampler audio unit.
--
-- An AVAudioUnit for Apple's Sampler Audio Unit. The sampler can be configured by loading    instruments from different types of files such as an aupreset, a DLS or SF2 sound bank,    an EXS24 instrument, a single audio file, or an array of audio files.
--
-- The output is a single stereo bus.
--
-- Generated bindings for @AVAudioUnitSampler@.
module ObjC.AVFAudio.AVAudioUnitSampler
  ( AVAudioUnitSampler
  , IsAVAudioUnitSampler(..)
  , loadSoundBankInstrumentAtURL_program_bankMSB_bankLSB_error
  , loadInstrumentAtURL_error
  , loadAudioFilesAtURLs_error
  , stereoPan
  , setStereoPan
  , overallGain
  , setOverallGain
  , masterGain
  , setMasterGain
  , globalTuning
  , setGlobalTuning
  , loadSoundBankInstrumentAtURL_program_bankMSB_bankLSB_errorSelector
  , loadInstrumentAtURL_errorSelector
  , loadAudioFilesAtURLs_errorSelector
  , stereoPanSelector
  , setStereoPanSelector
  , overallGainSelector
  , setOverallGainSelector
  , masterGainSelector
  , setMasterGainSelector
  , globalTuningSelector
  , setGlobalTuningSelector


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
import ObjC.Foundation.Internal.Classes

-- | loadSoundBankInstrumentAtURL:program:bankMSB:bankLSB:error:
--
-- loads a specific instrument from the specified sound bank
--
-- @bankURL@ — URL for a Soundbank file. The file can be either a DLS bank (.dls) or a SoundFont bank (.sf2).
--
-- @program@ — program number for the instrument to load
--
-- @bankMSB@ — MSB for the bank number for the instrument to load.  This is usually 0x79 for melodic		instruments and 0x78 for percussion instruments.
--
-- @bankLSB@ — LSB for the bank number for the instrument to load.  This is often 0, and represents the "bank variation".
--
-- @outError@ — the status of the operation
--
-- This method reads from file and allocates memory, so it should not be called on a real time thread.
--
-- ObjC selector: @- loadSoundBankInstrumentAtURL:program:bankMSB:bankLSB:error:@
loadSoundBankInstrumentAtURL_program_bankMSB_bankLSB_error :: (IsAVAudioUnitSampler avAudioUnitSampler, IsNSURL bankURL, IsNSError outError) => avAudioUnitSampler -> bankURL -> CUChar -> CUChar -> CUChar -> outError -> IO Bool
loadSoundBankInstrumentAtURL_program_bankMSB_bankLSB_error avAudioUnitSampler  bankURL program bankMSB bankLSB outError =
withObjCPtr bankURL $ \raw_bankURL ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitSampler (mkSelector "loadSoundBankInstrumentAtURL:program:bankMSB:bankLSB:error:") retCULong [argPtr (castPtr raw_bankURL :: Ptr ()), argCUChar (fromIntegral program), argCUChar (fromIntegral bankMSB), argCUChar (fromIntegral bankLSB), argPtr (castPtr raw_outError :: Ptr ())]

-- | loadInstrumentAtURL:error:
--
-- configures the sampler by loading the specified preset file.
--
-- @instrumentURL@ — URL to the preset file or audio file
--
-- @outError@ — the status of the operation
--
-- The file can be of one of the following types: Logic/GarageBand EXS24 instrument,		the Sampler AU's native aupreset, or an audio file (eg. .caf, .aiff, .wav, .mp3).
--
-- If an audio file URL is loaded, it will become the sole sample in a new default instrument.		Any information contained in the file regarding its keyboard placement (e.g. root key,		key range) will be used.		This method reads from file and allocates memory, so it should not be called on a real time thread.
--
-- ObjC selector: @- loadInstrumentAtURL:error:@
loadInstrumentAtURL_error :: (IsAVAudioUnitSampler avAudioUnitSampler, IsNSURL instrumentURL, IsNSError outError) => avAudioUnitSampler -> instrumentURL -> outError -> IO Bool
loadInstrumentAtURL_error avAudioUnitSampler  instrumentURL outError =
withObjCPtr instrumentURL $ \raw_instrumentURL ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitSampler (mkSelector "loadInstrumentAtURL:error:") retCULong [argPtr (castPtr raw_instrumentURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | loadAudioFilesAtURLs:error:
--
-- configures the sampler by loading a set of audio files.
--
-- @audioFiles@ — array of URLs for audio files to be loaded
--
-- @outError@ — the status of the operation
--
-- The audio files are loaded into a new default instrument with each audio file placed		into its own sampler zone. Any information contained in the audio file regarding		their placement on the keyboard (e.g. root key, key range) will be used.		This method reads from file and allocates memory, so it should not be called on a real time thread.
--
-- ObjC selector: @- loadAudioFilesAtURLs:error:@
loadAudioFilesAtURLs_error :: (IsAVAudioUnitSampler avAudioUnitSampler, IsNSArray audioFiles, IsNSError outError) => avAudioUnitSampler -> audioFiles -> outError -> IO Bool
loadAudioFilesAtURLs_error avAudioUnitSampler  audioFiles outError =
withObjCPtr audioFiles $ \raw_audioFiles ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitSampler (mkSelector "loadAudioFilesAtURLs:error:") retCULong [argPtr (castPtr raw_audioFiles :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | stereoPan
--
-- adjusts the pan for all the notes played.		Range:     -100 -> +100		Default:   0
--
-- ObjC selector: @- stereoPan@
stereoPan :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> IO CFloat
stereoPan avAudioUnitSampler  =
  sendMsg avAudioUnitSampler (mkSelector "stereoPan") retCFloat []

-- | stereoPan
--
-- adjusts the pan for all the notes played.		Range:     -100 -> +100		Default:   0
--
-- ObjC selector: @- setStereoPan:@
setStereoPan :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> CFloat -> IO ()
setStereoPan avAudioUnitSampler  value =
  sendMsg avAudioUnitSampler (mkSelector "setStereoPan:") retVoid [argCFloat (fromIntegral value)]

-- | overallGain
--
-- adjusts the gain of all the notes played		Range:     -90.0 -> +12 db		Default: 0 db
--
-- ObjC selector: @- overallGain@
overallGain :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> IO CFloat
overallGain avAudioUnitSampler  =
  sendMsg avAudioUnitSampler (mkSelector "overallGain") retCFloat []

-- | overallGain
--
-- adjusts the gain of all the notes played		Range:     -90.0 -> +12 db		Default: 0 db
--
-- ObjC selector: @- setOverallGain:@
setOverallGain :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> CFloat -> IO ()
setOverallGain avAudioUnitSampler  value =
  sendMsg avAudioUnitSampler (mkSelector "setOverallGain:") retVoid [argCFloat (fromIntegral value)]

-- | masterGain
--
-- adjusts the gain of all the notes played		Range:     -90.0 -> +12 db		Default: 0 db
--
-- ObjC selector: @- masterGain@
masterGain :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> IO CFloat
masterGain avAudioUnitSampler  =
  sendMsg avAudioUnitSampler (mkSelector "masterGain") retCFloat []

-- | masterGain
--
-- adjusts the gain of all the notes played		Range:     -90.0 -> +12 db		Default: 0 db
--
-- ObjC selector: @- setMasterGain:@
setMasterGain :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> CFloat -> IO ()
setMasterGain avAudioUnitSampler  value =
  sendMsg avAudioUnitSampler (mkSelector "setMasterGain:") retVoid [argCFloat (fromIntegral value)]

-- | globalTuning
--
-- adjusts the tuning of all the notes played.		Range:     -2400 -> +2400 cents		Default:   0
--
-- ObjC selector: @- globalTuning@
globalTuning :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> IO CFloat
globalTuning avAudioUnitSampler  =
  sendMsg avAudioUnitSampler (mkSelector "globalTuning") retCFloat []

-- | globalTuning
--
-- adjusts the tuning of all the notes played.		Range:     -2400 -> +2400 cents		Default:   0
--
-- ObjC selector: @- setGlobalTuning:@
setGlobalTuning :: IsAVAudioUnitSampler avAudioUnitSampler => avAudioUnitSampler -> CFloat -> IO ()
setGlobalTuning avAudioUnitSampler  value =
  sendMsg avAudioUnitSampler (mkSelector "setGlobalTuning:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadSoundBankInstrumentAtURL:program:bankMSB:bankLSB:error:@
loadSoundBankInstrumentAtURL_program_bankMSB_bankLSB_errorSelector :: Selector
loadSoundBankInstrumentAtURL_program_bankMSB_bankLSB_errorSelector = mkSelector "loadSoundBankInstrumentAtURL:program:bankMSB:bankLSB:error:"

-- | @Selector@ for @loadInstrumentAtURL:error:@
loadInstrumentAtURL_errorSelector :: Selector
loadInstrumentAtURL_errorSelector = mkSelector "loadInstrumentAtURL:error:"

-- | @Selector@ for @loadAudioFilesAtURLs:error:@
loadAudioFilesAtURLs_errorSelector :: Selector
loadAudioFilesAtURLs_errorSelector = mkSelector "loadAudioFilesAtURLs:error:"

-- | @Selector@ for @stereoPan@
stereoPanSelector :: Selector
stereoPanSelector = mkSelector "stereoPan"

-- | @Selector@ for @setStereoPan:@
setStereoPanSelector :: Selector
setStereoPanSelector = mkSelector "setStereoPan:"

-- | @Selector@ for @overallGain@
overallGainSelector :: Selector
overallGainSelector = mkSelector "overallGain"

-- | @Selector@ for @setOverallGain:@
setOverallGainSelector :: Selector
setOverallGainSelector = mkSelector "setOverallGain:"

-- | @Selector@ for @masterGain@
masterGainSelector :: Selector
masterGainSelector = mkSelector "masterGain"

-- | @Selector@ for @setMasterGain:@
setMasterGainSelector :: Selector
setMasterGainSelector = mkSelector "setMasterGain:"

-- | @Selector@ for @globalTuning@
globalTuningSelector :: Selector
globalTuningSelector = mkSelector "globalTuning"

-- | @Selector@ for @setGlobalTuning:@
setGlobalTuningSelector :: Selector
setGlobalTuningSelector = mkSelector "setGlobalTuning:"

