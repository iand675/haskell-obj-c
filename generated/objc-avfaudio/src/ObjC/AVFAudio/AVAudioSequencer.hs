{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioSequencer
--
-- A collection of MIDI events organized into AVMusicTracks, plus a player to play back the events.
--
-- Generated bindings for @AVAudioSequencer@.
module ObjC.AVFAudio.AVAudioSequencer
  ( AVAudioSequencer
  , IsAVAudioSequencer(..)
  , init_
  , initWithAudioEngine
  , loadFromURL_options_error
  , loadFromData_options_error
  , writeToURL_SMPTEResolution_replaceExisting_error
  , dataWithSMPTEResolution_error
  , secondsForBeats
  , beatsForSeconds
  , reverseEvents
  , createAndAppendTrack
  , removeTrack
  , setUserCallback
  , hostTimeForBeats_error
  , beatsForHostTime_error
  , prepareToPlay
  , startAndReturnError
  , stop
  , tracks
  , tempoTrack
  , userInfo
  , currentPositionInSeconds
  , setCurrentPositionInSeconds
  , currentPositionInBeats
  , setCurrentPositionInBeats
  , playing
  , rate
  , setRate
  , initSelector
  , initWithAudioEngineSelector
  , loadFromURL_options_errorSelector
  , loadFromData_options_errorSelector
  , writeToURL_SMPTEResolution_replaceExisting_errorSelector
  , dataWithSMPTEResolution_errorSelector
  , secondsForBeatsSelector
  , beatsForSecondsSelector
  , reverseEventsSelector
  , createAndAppendTrackSelector
  , removeTrackSelector
  , setUserCallbackSelector
  , hostTimeForBeats_errorSelector
  , beatsForHostTime_errorSelector
  , prepareToPlaySelector
  , startAndReturnErrorSelector
  , stopSelector
  , tracksSelector
  , tempoTrackSelector
  , userInfoSelector
  , currentPositionInSecondsSelector
  , setCurrentPositionInSecondsSelector
  , currentPositionInBeatsSelector
  , setCurrentPositionInBeatsSelector
  , playingSelector
  , rateSelector
  , setRateSelector

  -- * Enum types
  , AVMusicSequenceLoadOptions(AVMusicSequenceLoadOptions)
  , pattern AVMusicSequenceLoadSMF_PreserveTracks
  , pattern AVMusicSequenceLoadSMF_ChannelsToTracks

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

-- | init
--
-- Initialize a new sequencer, which will not be connected to an audio engine.
--
-- This is used to create a sequencer whose tracks will only send events to external MIDI endpoints.
--
-- ObjC selector: @- init@
init_ :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id AVAudioSequencer)
init_ avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithAudioEngine:
--
-- Initialize a new sequencer, handing it the audio engine.
--
-- ObjC selector: @- initWithAudioEngine:@
initWithAudioEngine :: (IsAVAudioSequencer avAudioSequencer, IsAVAudioEngine engine) => avAudioSequencer -> engine -> IO (Id AVAudioSequencer)
initWithAudioEngine avAudioSequencer  engine =
withObjCPtr engine $ \raw_engine ->
    sendMsg avAudioSequencer (mkSelector "initWithAudioEngine:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ())] >>= ownedObject . castPtr

-- | loadFromURL:options:error:
--
-- Load the file referenced by the URL and add the events to the sequence
--
-- @fileURL@ — the URL to the file
--
-- @options@ — determines how the file's contents are mapped to tracks inside the sequence
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- ObjC selector: @- loadFromURL:options:error:@
loadFromURL_options_error :: (IsAVAudioSequencer avAudioSequencer, IsNSURL fileURL, IsNSError outError) => avAudioSequencer -> fileURL -> AVMusicSequenceLoadOptions -> outError -> IO Bool
loadFromURL_options_error avAudioSequencer  fileURL options outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSequencer (mkSelector "loadFromURL:options:error:") retCULong [argPtr (castPtr raw_fileURL :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | loadFromData:options:error:
--
-- Parse the data and add the its events to the sequence
--
-- @data@ — the data to load from
--
-- @options@ — determines how the contents are mapped to tracks inside the sequence
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- ObjC selector: @- loadFromData:options:error:@
loadFromData_options_error :: (IsAVAudioSequencer avAudioSequencer, IsNSData data_, IsNSError outError) => avAudioSequencer -> data_ -> AVMusicSequenceLoadOptions -> outError -> IO Bool
loadFromData_options_error avAudioSequencer  data_ options outError =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSequencer (mkSelector "loadFromData:options:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | writeToURL:SMPTEResolution:replaceExisting:error:
--
-- Create and write a MIDI file containing the events and complete state of the sequence
--
-- @fileURL@ — the path for the file to be created
--
-- @resolution@ — the relationship between "tick" and quarter note for saving to a Standard MIDI File - pass in		zero to use default - this will be the value that is currently set on the tempo track
--
-- @replace@ — if the file already exists, YES will cause it to be overwritten with the new data.		Otherwise the call will fail with a permission error.
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- A MIDI file saved via this method will contain not only the complete MIDI content of the sequence,		but also the state of all tracks, including muting, loop points and enablement, etc.  It will also		contain all non-MIDI AVMusicEvent types which had been added to the sequence's track.
--
-- MIDI files are normally beat based, but can also have a SMPTE (or real-time rather than beat time) representation.		The relationship between "tick" and quarter note for saving to Standard MIDI File		- pass in zero to use default - this will be the value that is currently set on the tempo track
--
-- ObjC selector: @- writeToURL:SMPTEResolution:replaceExisting:error:@
writeToURL_SMPTEResolution_replaceExisting_error :: (IsAVAudioSequencer avAudioSequencer, IsNSURL fileURL, IsNSError outError) => avAudioSequencer -> fileURL -> CLong -> Bool -> outError -> IO Bool
writeToURL_SMPTEResolution_replaceExisting_error avAudioSequencer  fileURL resolution replace outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSequencer (mkSelector "writeToURL:SMPTEResolution:replaceExisting:error:") retCULong [argPtr (castPtr raw_fileURL :: Ptr ()), argCLong (fromIntegral resolution), argCULong (if replace then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | dataWithSMPTEResolution:error:
--
-- Return a data object containing the events from the sequence
--
-- All details regarding the SMPTE resolution apply here as well.		The returned NSData lifetime is controlled by the client.
--
-- ObjC selector: @- dataWithSMPTEResolution:error:@
dataWithSMPTEResolution_error :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> CLong -> outError -> IO (Id NSData)
dataWithSMPTEResolution_error avAudioSequencer  smpteResolution outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg avAudioSequencer (mkSelector "dataWithSMPTEResolution:error:") (retPtr retVoid) [argCLong (fromIntegral smpteResolution), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | secondsForBeats:
--
-- Get the time in seconds for the given beat position (timestamp) in the AVMusicTrack
--
-- ObjC selector: @- secondsForBeats:@
secondsForBeats :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO CDouble
secondsForBeats avAudioSequencer  beats =
  sendMsg avAudioSequencer (mkSelector "secondsForBeats:") retCDouble [argCDouble (fromIntegral beats)]

-- | beatsForSeconds:
--
-- Get the beat position (timestamp) for the given time in the AVMusicTrack
--
-- ObjC selector: @- beatsForSeconds:@
beatsForSeconds :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO CDouble
beatsForSeconds avAudioSequencer  seconds =
  sendMsg avAudioSequencer (mkSelector "beatsForSeconds:") retCDouble [argCDouble (fromIntegral seconds)]

-- | reverseEvents:
--
-- Reverse the order of all events in all AVMusicTracks, including the tempo track
--
-- ObjC selector: @- reverseEvents@
reverseEvents :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO ()
reverseEvents avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "reverseEvents") retVoid []

-- | createAndAppendTrack:
--
-- Create a new AVMusicTrack and append it to the AVMusicSequencer's list
--
-- ObjC selector: @- createAndAppendTrack@
createAndAppendTrack :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id AVMusicTrack)
createAndAppendTrack avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "createAndAppendTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | removeTrack:
--
-- Remove the given AVMusicTrack from the AVMusicSequencer.
--
-- This does not destroy the AVMusicTrack because it may be re-used.
--
-- ObjC selector: @- removeTrack:@
removeTrack :: (IsAVAudioSequencer avAudioSequencer, IsAVMusicTrack track) => avAudioSequencer -> track -> IO Bool
removeTrack avAudioSequencer  track =
withObjCPtr track $ \raw_track ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSequencer (mkSelector "removeTrack:") retCULong [argPtr (castPtr raw_track :: Ptr ())]

-- | setUserCallback:
--
-- Add a block which will be called each time the AVAudioSequencer encounters an AVMusicUserEvent during playback.
--
-- The same callback is called for events which occur on any track in the sequencer.
--
-- Set the block to nil to disable it.
--
-- ObjC selector: @- setUserCallback:@
setUserCallback :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> Ptr () -> IO ()
setUserCallback avAudioSequencer  userCallback =
  sendMsg avAudioSequencer (mkSelector "setUserCallback:") retVoid [argPtr (castPtr userCallback :: Ptr ())]

-- | hostTimeForBeats:error:
--
-- Returns the host time that will be (or was) played at the specified beat.
--
-- This call is only valid if the player is playing and will return 0 with an error if the		player is not playing or if the starting position of the player (its "starting beat") was 		after the specified beat.  The method uses the sequence's tempo map to translate a beat		time from the starting time and beat of the player.
--
-- ObjC selector: @- hostTimeForBeats:error:@
hostTimeForBeats_error :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> CDouble -> outError -> IO CULong
hostTimeForBeats_error avAudioSequencer  inBeats outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg avAudioSequencer (mkSelector "hostTimeForBeats:error:") retCULong [argCDouble (fromIntegral inBeats), argPtr (castPtr raw_outError :: Ptr ())]

-- | beatsForHostTime:error:
--
-- Returns the beat that will be (or was) played at the specified host time.
--
-- This call is only valid if the player is playing and will return 0 with an error if the		player is not playing or if the starting time of the player was after the specified host		time.  The method uses the sequence's tempo map to retrieve a beat time from the starting		and specified host time.
--
-- ObjC selector: @- beatsForHostTime:error:@
beatsForHostTime_error :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> CULong -> outError -> IO CDouble
beatsForHostTime_error avAudioSequencer  inHostTime outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg avAudioSequencer (mkSelector "beatsForHostTime:error:") retCDouble [argCULong (fromIntegral inHostTime), argPtr (castPtr raw_outError :: Ptr ())]

-- | prepareToPlay
--
-- Get ready to play the sequence by prerolling all events
--
-- Happens automatically on play if it has not already been called, but may produce a delay in		startup.
--
-- ObjC selector: @- prepareToPlay@
prepareToPlay :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO ()
prepareToPlay avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "prepareToPlay") retVoid []

-- | startAndReturnError:
--
-- Start the sequencer's player
--
-- If the AVAudioSequencer has not been prerolled, it will pre-roll itself and then start.		When the sequencer is associated with an audio engine, the sequencer's player will only		play if the audio engine is running.
--
-- ObjC selector: @- startAndReturnError:@
startAndReturnError :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> outError -> IO Bool
startAndReturnError avAudioSequencer  outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSequencer (mkSelector "startAndReturnError:") retCULong [argPtr (castPtr raw_outError :: Ptr ())]

-- | stop
--
-- Stop the sequencer's player
--
-- Stopping the player leaves it in an un-prerolled state, but stores the playback position so		that a subsequent call to startAndReturnError will resume where it left off. This action		will not stop an associated audio engine.
--
-- ObjC selector: @- stop@
stop :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO ()
stop avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "stop") retVoid []

-- | tracks
--
-- An NSArray containing all the AVMusicTracks in the sequence
--
-- This list will not include the tempo track.
--
-- ObjC selector: @- tracks@
tracks :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id NSArray)
tracks avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "tracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tempoTrack
--
-- The tempo track
--
-- Each AVMusicSequence has a single tempo track.
--
-- All tempo events read from external MIDI files are placed into this track (as well as other		appropriate events (e.g., the time signature meta event from the file).
--
-- The tempo track can be edited and iterated upon as any other track.
--
-- Non-tempo-related events will generate exceptions if added.
--
-- ObjC selector: @- tempoTrack@
tempoTrack :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id AVMusicTrack)
tempoTrack avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "tempoTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | userInfo
--
-- A dictionary containing meta-data derived from a sequence
--
-- The dictionary can contain one or more of the values accessible via the AVAudioSequencerInfoDictionaryKeys.
--
-- ObjC selector: @- userInfo@
userInfo :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id NSDictionary)
userInfo avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | currentPositionInSeconds
--
-- The current playback position in seconds
--
-- Setting this positions the sequencer's player to the specified time.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- currentPositionInSeconds@
currentPositionInSeconds :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO CDouble
currentPositionInSeconds avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "currentPositionInSeconds") retCDouble []

-- | currentPositionInSeconds
--
-- The current playback position in seconds
--
-- Setting this positions the sequencer's player to the specified time.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- setCurrentPositionInSeconds:@
setCurrentPositionInSeconds :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO ()
setCurrentPositionInSeconds avAudioSequencer  value =
  sendMsg avAudioSequencer (mkSelector "setCurrentPositionInSeconds:") retVoid [argCDouble (fromIntegral value)]

-- | currentPositionInBeats
--
-- The current playback position in beats
--
-- Setting this positions the sequencer's player to the specified beat.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- currentPositionInBeats@
currentPositionInBeats :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO CDouble
currentPositionInBeats avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "currentPositionInBeats") retCDouble []

-- | currentPositionInBeats
--
-- The current playback position in beats
--
-- Setting this positions the sequencer's player to the specified beat.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- setCurrentPositionInBeats:@
setCurrentPositionInBeats :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO ()
setCurrentPositionInBeats avAudioSequencer  value =
  sendMsg avAudioSequencer (mkSelector "setCurrentPositionInBeats:") retVoid [argCDouble (fromIntegral value)]

-- | playing
--
-- Indicates whether or not the sequencer's player is playing
--
-- Returns TRUE if the sequencer's player has been started and not stopped. It may have		"played" past the end of the events in the sequence, but it is still considered to be		playing (and its time value increasing) until it is explicitly stopped.
--
-- ObjC selector: @- playing@
playing :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO Bool
playing avAudioSequencer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSequencer (mkSelector "playing") retCULong []

-- | rate
--
-- The playback rate of the sequencer's player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- rate@
rate :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO CFloat
rate avAudioSequencer  =
  sendMsg avAudioSequencer (mkSelector "rate") retCFloat []

-- | rate
--
-- The playback rate of the sequencer's player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CFloat -> IO ()
setRate avAudioSequencer  value =
  sendMsg avAudioSequencer (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAudioEngine:@
initWithAudioEngineSelector :: Selector
initWithAudioEngineSelector = mkSelector "initWithAudioEngine:"

-- | @Selector@ for @loadFromURL:options:error:@
loadFromURL_options_errorSelector :: Selector
loadFromURL_options_errorSelector = mkSelector "loadFromURL:options:error:"

-- | @Selector@ for @loadFromData:options:error:@
loadFromData_options_errorSelector :: Selector
loadFromData_options_errorSelector = mkSelector "loadFromData:options:error:"

-- | @Selector@ for @writeToURL:SMPTEResolution:replaceExisting:error:@
writeToURL_SMPTEResolution_replaceExisting_errorSelector :: Selector
writeToURL_SMPTEResolution_replaceExisting_errorSelector = mkSelector "writeToURL:SMPTEResolution:replaceExisting:error:"

-- | @Selector@ for @dataWithSMPTEResolution:error:@
dataWithSMPTEResolution_errorSelector :: Selector
dataWithSMPTEResolution_errorSelector = mkSelector "dataWithSMPTEResolution:error:"

-- | @Selector@ for @secondsForBeats:@
secondsForBeatsSelector :: Selector
secondsForBeatsSelector = mkSelector "secondsForBeats:"

-- | @Selector@ for @beatsForSeconds:@
beatsForSecondsSelector :: Selector
beatsForSecondsSelector = mkSelector "beatsForSeconds:"

-- | @Selector@ for @reverseEvents@
reverseEventsSelector :: Selector
reverseEventsSelector = mkSelector "reverseEvents"

-- | @Selector@ for @createAndAppendTrack@
createAndAppendTrackSelector :: Selector
createAndAppendTrackSelector = mkSelector "createAndAppendTrack"

-- | @Selector@ for @removeTrack:@
removeTrackSelector :: Selector
removeTrackSelector = mkSelector "removeTrack:"

-- | @Selector@ for @setUserCallback:@
setUserCallbackSelector :: Selector
setUserCallbackSelector = mkSelector "setUserCallback:"

-- | @Selector@ for @hostTimeForBeats:error:@
hostTimeForBeats_errorSelector :: Selector
hostTimeForBeats_errorSelector = mkSelector "hostTimeForBeats:error:"

-- | @Selector@ for @beatsForHostTime:error:@
beatsForHostTime_errorSelector :: Selector
beatsForHostTime_errorSelector = mkSelector "beatsForHostTime:error:"

-- | @Selector@ for @prepareToPlay@
prepareToPlaySelector :: Selector
prepareToPlaySelector = mkSelector "prepareToPlay"

-- | @Selector@ for @startAndReturnError:@
startAndReturnErrorSelector :: Selector
startAndReturnErrorSelector = mkSelector "startAndReturnError:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @tracks@
tracksSelector :: Selector
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @tempoTrack@
tempoTrackSelector :: Selector
tempoTrackSelector = mkSelector "tempoTrack"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @currentPositionInSeconds@
currentPositionInSecondsSelector :: Selector
currentPositionInSecondsSelector = mkSelector "currentPositionInSeconds"

-- | @Selector@ for @setCurrentPositionInSeconds:@
setCurrentPositionInSecondsSelector :: Selector
setCurrentPositionInSecondsSelector = mkSelector "setCurrentPositionInSeconds:"

-- | @Selector@ for @currentPositionInBeats@
currentPositionInBeatsSelector :: Selector
currentPositionInBeatsSelector = mkSelector "currentPositionInBeats"

-- | @Selector@ for @setCurrentPositionInBeats:@
setCurrentPositionInBeatsSelector :: Selector
setCurrentPositionInBeatsSelector = mkSelector "setCurrentPositionInBeats:"

-- | @Selector@ for @playing@
playingSelector :: Selector
playingSelector = mkSelector "playing"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

