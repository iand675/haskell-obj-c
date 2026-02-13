{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , beatsForHostTime_errorSelector
  , beatsForSecondsSelector
  , createAndAppendTrackSelector
  , currentPositionInBeatsSelector
  , currentPositionInSecondsSelector
  , dataWithSMPTEResolution_errorSelector
  , hostTimeForBeats_errorSelector
  , initSelector
  , initWithAudioEngineSelector
  , loadFromData_options_errorSelector
  , loadFromURL_options_errorSelector
  , playingSelector
  , prepareToPlaySelector
  , rateSelector
  , removeTrackSelector
  , reverseEventsSelector
  , secondsForBeatsSelector
  , setCurrentPositionInBeatsSelector
  , setCurrentPositionInSecondsSelector
  , setRateSelector
  , setUserCallbackSelector
  , startAndReturnErrorSelector
  , stopSelector
  , tempoTrackSelector
  , tracksSelector
  , userInfoSelector
  , writeToURL_SMPTEResolution_replaceExisting_errorSelector

  -- * Enum types
  , AVMusicSequenceLoadOptions(AVMusicSequenceLoadOptions)
  , pattern AVMusicSequenceLoadSMF_PreserveTracks
  , pattern AVMusicSequenceLoadSMF_ChannelsToTracks

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

-- | init
--
-- Initialize a new sequencer, which will not be connected to an audio engine.
--
-- This is used to create a sequencer whose tracks will only send events to external MIDI endpoints.
--
-- ObjC selector: @- init@
init_ :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id AVAudioSequencer)
init_ avAudioSequencer =
  sendOwnedMessage avAudioSequencer initSelector

-- | initWithAudioEngine:
--
-- Initialize a new sequencer, handing it the audio engine.
--
-- ObjC selector: @- initWithAudioEngine:@
initWithAudioEngine :: (IsAVAudioSequencer avAudioSequencer, IsAVAudioEngine engine) => avAudioSequencer -> engine -> IO (Id AVAudioSequencer)
initWithAudioEngine avAudioSequencer engine =
  sendOwnedMessage avAudioSequencer initWithAudioEngineSelector (toAVAudioEngine engine)

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
loadFromURL_options_error avAudioSequencer fileURL options outError =
  sendMessage avAudioSequencer loadFromURL_options_errorSelector (toNSURL fileURL) options (toNSError outError)

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
loadFromData_options_error avAudioSequencer data_ options outError =
  sendMessage avAudioSequencer loadFromData_options_errorSelector (toNSData data_) options (toNSError outError)

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
writeToURL_SMPTEResolution_replaceExisting_error avAudioSequencer fileURL resolution replace outError =
  sendMessage avAudioSequencer writeToURL_SMPTEResolution_replaceExisting_errorSelector (toNSURL fileURL) resolution replace (toNSError outError)

-- | dataWithSMPTEResolution:error:
--
-- Return a data object containing the events from the sequence
--
-- All details regarding the SMPTE resolution apply here as well.		The returned NSData lifetime is controlled by the client.
--
-- ObjC selector: @- dataWithSMPTEResolution:error:@
dataWithSMPTEResolution_error :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> CLong -> outError -> IO (Id NSData)
dataWithSMPTEResolution_error avAudioSequencer smpteResolution outError =
  sendMessage avAudioSequencer dataWithSMPTEResolution_errorSelector smpteResolution (toNSError outError)

-- | secondsForBeats:
--
-- Get the time in seconds for the given beat position (timestamp) in the AVMusicTrack
--
-- ObjC selector: @- secondsForBeats:@
secondsForBeats :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO CDouble
secondsForBeats avAudioSequencer beats =
  sendMessage avAudioSequencer secondsForBeatsSelector beats

-- | beatsForSeconds:
--
-- Get the beat position (timestamp) for the given time in the AVMusicTrack
--
-- ObjC selector: @- beatsForSeconds:@
beatsForSeconds :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO CDouble
beatsForSeconds avAudioSequencer seconds =
  sendMessage avAudioSequencer beatsForSecondsSelector seconds

-- | reverseEvents:
--
-- Reverse the order of all events in all AVMusicTracks, including the tempo track
--
-- ObjC selector: @- reverseEvents@
reverseEvents :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO ()
reverseEvents avAudioSequencer =
  sendMessage avAudioSequencer reverseEventsSelector

-- | createAndAppendTrack:
--
-- Create a new AVMusicTrack and append it to the AVMusicSequencer's list
--
-- ObjC selector: @- createAndAppendTrack@
createAndAppendTrack :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id AVMusicTrack)
createAndAppendTrack avAudioSequencer =
  sendMessage avAudioSequencer createAndAppendTrackSelector

-- | removeTrack:
--
-- Remove the given AVMusicTrack from the AVMusicSequencer.
--
-- This does not destroy the AVMusicTrack because it may be re-used.
--
-- ObjC selector: @- removeTrack:@
removeTrack :: (IsAVAudioSequencer avAudioSequencer, IsAVMusicTrack track) => avAudioSequencer -> track -> IO Bool
removeTrack avAudioSequencer track =
  sendMessage avAudioSequencer removeTrackSelector (toAVMusicTrack track)

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
setUserCallback avAudioSequencer userCallback =
  sendMessage avAudioSequencer setUserCallbackSelector userCallback

-- | hostTimeForBeats:error:
--
-- Returns the host time that will be (or was) played at the specified beat.
--
-- This call is only valid if the player is playing and will return 0 with an error if the		player is not playing or if the starting position of the player (its "starting beat") was 		after the specified beat.  The method uses the sequence's tempo map to translate a beat		time from the starting time and beat of the player.
--
-- ObjC selector: @- hostTimeForBeats:error:@
hostTimeForBeats_error :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> CDouble -> outError -> IO CULong
hostTimeForBeats_error avAudioSequencer inBeats outError =
  sendMessage avAudioSequencer hostTimeForBeats_errorSelector inBeats (toNSError outError)

-- | beatsForHostTime:error:
--
-- Returns the beat that will be (or was) played at the specified host time.
--
-- This call is only valid if the player is playing and will return 0 with an error if the		player is not playing or if the starting time of the player was after the specified host		time.  The method uses the sequence's tempo map to retrieve a beat time from the starting		and specified host time.
--
-- ObjC selector: @- beatsForHostTime:error:@
beatsForHostTime_error :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> CULong -> outError -> IO CDouble
beatsForHostTime_error avAudioSequencer inHostTime outError =
  sendMessage avAudioSequencer beatsForHostTime_errorSelector inHostTime (toNSError outError)

-- | prepareToPlay
--
-- Get ready to play the sequence by prerolling all events
--
-- Happens automatically on play if it has not already been called, but may produce a delay in		startup.
--
-- ObjC selector: @- prepareToPlay@
prepareToPlay :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO ()
prepareToPlay avAudioSequencer =
  sendMessage avAudioSequencer prepareToPlaySelector

-- | startAndReturnError:
--
-- Start the sequencer's player
--
-- If the AVAudioSequencer has not been prerolled, it will pre-roll itself and then start.		When the sequencer is associated with an audio engine, the sequencer's player will only		play if the audio engine is running.
--
-- ObjC selector: @- startAndReturnError:@
startAndReturnError :: (IsAVAudioSequencer avAudioSequencer, IsNSError outError) => avAudioSequencer -> outError -> IO Bool
startAndReturnError avAudioSequencer outError =
  sendMessage avAudioSequencer startAndReturnErrorSelector (toNSError outError)

-- | stop
--
-- Stop the sequencer's player
--
-- Stopping the player leaves it in an un-prerolled state, but stores the playback position so		that a subsequent call to startAndReturnError will resume where it left off. This action		will not stop an associated audio engine.
--
-- ObjC selector: @- stop@
stop :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO ()
stop avAudioSequencer =
  sendMessage avAudioSequencer stopSelector

-- | tracks
--
-- An NSArray containing all the AVMusicTracks in the sequence
--
-- This list will not include the tempo track.
--
-- ObjC selector: @- tracks@
tracks :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id NSArray)
tracks avAudioSequencer =
  sendMessage avAudioSequencer tracksSelector

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
tempoTrack avAudioSequencer =
  sendMessage avAudioSequencer tempoTrackSelector

-- | userInfo
--
-- A dictionary containing meta-data derived from a sequence
--
-- The dictionary can contain one or more of the values accessible via the AVAudioSequencerInfoDictionaryKeys.
--
-- ObjC selector: @- userInfo@
userInfo :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO (Id NSDictionary)
userInfo avAudioSequencer =
  sendMessage avAudioSequencer userInfoSelector

-- | currentPositionInSeconds
--
-- The current playback position in seconds
--
-- Setting this positions the sequencer's player to the specified time.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- currentPositionInSeconds@
currentPositionInSeconds :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO CDouble
currentPositionInSeconds avAudioSequencer =
  sendMessage avAudioSequencer currentPositionInSecondsSelector

-- | currentPositionInSeconds
--
-- The current playback position in seconds
--
-- Setting this positions the sequencer's player to the specified time.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- setCurrentPositionInSeconds:@
setCurrentPositionInSeconds :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO ()
setCurrentPositionInSeconds avAudioSequencer value =
  sendMessage avAudioSequencer setCurrentPositionInSecondsSelector value

-- | currentPositionInBeats
--
-- The current playback position in beats
--
-- Setting this positions the sequencer's player to the specified beat.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- currentPositionInBeats@
currentPositionInBeats :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO CDouble
currentPositionInBeats avAudioSequencer =
  sendMessage avAudioSequencer currentPositionInBeatsSelector

-- | currentPositionInBeats
--
-- The current playback position in beats
--
-- Setting this positions the sequencer's player to the specified beat.  This can be set while		the player is playing, in which case playback will resume at the new position.
--
-- ObjC selector: @- setCurrentPositionInBeats:@
setCurrentPositionInBeats :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CDouble -> IO ()
setCurrentPositionInBeats avAudioSequencer value =
  sendMessage avAudioSequencer setCurrentPositionInBeatsSelector value

-- | playing
--
-- Indicates whether or not the sequencer's player is playing
--
-- Returns TRUE if the sequencer's player has been started and not stopped. It may have		"played" past the end of the events in the sequence, but it is still considered to be		playing (and its time value increasing) until it is explicitly stopped.
--
-- ObjC selector: @- playing@
playing :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO Bool
playing avAudioSequencer =
  sendMessage avAudioSequencer playingSelector

-- | rate
--
-- The playback rate of the sequencer's player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- rate@
rate :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> IO CFloat
rate avAudioSequencer =
  sendMessage avAudioSequencer rateSelector

-- | rate
--
-- The playback rate of the sequencer's player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVAudioSequencer avAudioSequencer => avAudioSequencer -> CFloat -> IO ()
setRate avAudioSequencer value =
  sendMessage avAudioSequencer setRateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioSequencer)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAudioEngine:@
initWithAudioEngineSelector :: Selector '[Id AVAudioEngine] (Id AVAudioSequencer)
initWithAudioEngineSelector = mkSelector "initWithAudioEngine:"

-- | @Selector@ for @loadFromURL:options:error:@
loadFromURL_options_errorSelector :: Selector '[Id NSURL, AVMusicSequenceLoadOptions, Id NSError] Bool
loadFromURL_options_errorSelector = mkSelector "loadFromURL:options:error:"

-- | @Selector@ for @loadFromData:options:error:@
loadFromData_options_errorSelector :: Selector '[Id NSData, AVMusicSequenceLoadOptions, Id NSError] Bool
loadFromData_options_errorSelector = mkSelector "loadFromData:options:error:"

-- | @Selector@ for @writeToURL:SMPTEResolution:replaceExisting:error:@
writeToURL_SMPTEResolution_replaceExisting_errorSelector :: Selector '[Id NSURL, CLong, Bool, Id NSError] Bool
writeToURL_SMPTEResolution_replaceExisting_errorSelector = mkSelector "writeToURL:SMPTEResolution:replaceExisting:error:"

-- | @Selector@ for @dataWithSMPTEResolution:error:@
dataWithSMPTEResolution_errorSelector :: Selector '[CLong, Id NSError] (Id NSData)
dataWithSMPTEResolution_errorSelector = mkSelector "dataWithSMPTEResolution:error:"

-- | @Selector@ for @secondsForBeats:@
secondsForBeatsSelector :: Selector '[CDouble] CDouble
secondsForBeatsSelector = mkSelector "secondsForBeats:"

-- | @Selector@ for @beatsForSeconds:@
beatsForSecondsSelector :: Selector '[CDouble] CDouble
beatsForSecondsSelector = mkSelector "beatsForSeconds:"

-- | @Selector@ for @reverseEvents@
reverseEventsSelector :: Selector '[] ()
reverseEventsSelector = mkSelector "reverseEvents"

-- | @Selector@ for @createAndAppendTrack@
createAndAppendTrackSelector :: Selector '[] (Id AVMusicTrack)
createAndAppendTrackSelector = mkSelector "createAndAppendTrack"

-- | @Selector@ for @removeTrack:@
removeTrackSelector :: Selector '[Id AVMusicTrack] Bool
removeTrackSelector = mkSelector "removeTrack:"

-- | @Selector@ for @setUserCallback:@
setUserCallbackSelector :: Selector '[Ptr ()] ()
setUserCallbackSelector = mkSelector "setUserCallback:"

-- | @Selector@ for @hostTimeForBeats:error:@
hostTimeForBeats_errorSelector :: Selector '[CDouble, Id NSError] CULong
hostTimeForBeats_errorSelector = mkSelector "hostTimeForBeats:error:"

-- | @Selector@ for @beatsForHostTime:error:@
beatsForHostTime_errorSelector :: Selector '[CULong, Id NSError] CDouble
beatsForHostTime_errorSelector = mkSelector "beatsForHostTime:error:"

-- | @Selector@ for @prepareToPlay@
prepareToPlaySelector :: Selector '[] ()
prepareToPlaySelector = mkSelector "prepareToPlay"

-- | @Selector@ for @startAndReturnError:@
startAndReturnErrorSelector :: Selector '[Id NSError] Bool
startAndReturnErrorSelector = mkSelector "startAndReturnError:"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @tracks@
tracksSelector :: Selector '[] (Id NSArray)
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @tempoTrack@
tempoTrackSelector :: Selector '[] (Id AVMusicTrack)
tempoTrackSelector = mkSelector "tempoTrack"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @currentPositionInSeconds@
currentPositionInSecondsSelector :: Selector '[] CDouble
currentPositionInSecondsSelector = mkSelector "currentPositionInSeconds"

-- | @Selector@ for @setCurrentPositionInSeconds:@
setCurrentPositionInSecondsSelector :: Selector '[CDouble] ()
setCurrentPositionInSecondsSelector = mkSelector "setCurrentPositionInSeconds:"

-- | @Selector@ for @currentPositionInBeats@
currentPositionInBeatsSelector :: Selector '[] CDouble
currentPositionInBeatsSelector = mkSelector "currentPositionInBeats"

-- | @Selector@ for @setCurrentPositionInBeats:@
setCurrentPositionInBeatsSelector :: Selector '[CDouble] ()
setCurrentPositionInBeatsSelector = mkSelector "setCurrentPositionInBeats:"

-- | @Selector@ for @playing@
playingSelector :: Selector '[] Bool
playingSelector = mkSelector "playing"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

