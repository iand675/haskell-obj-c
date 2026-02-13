{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMusicTrack
--
-- A collection of music events which will be sent to a given destination, and which can be				offset, muted, etc. independently of events in other tracks.
--
-- AVMusicTrack is not a container of AVMusicEvents - it will not hold references to				AVMusicEvents that are added, so an application should maintain its own if it is				desired.
--
-- Generated bindings for @AVMusicTrack@.
module ObjC.AVFAudio.AVMusicTrack
  ( AVMusicTrack
  , IsAVMusicTrack(..)
  , addEvent_atBeat
  , moveEventsInRange_byAmount
  , clearEventsInRange
  , cutEventsInRange
  , copyEventsInRange_fromTrack_insertAtBeat
  , copyAndMergeEventsInRange_fromTrack_mergeAtBeat
  , enumerateEventsInRange_usingBlock
  , destinationAudioUnit
  , setDestinationAudioUnit
  , destinationMIDIEndpoint
  , setDestinationMIDIEndpoint
  , loopRange
  , setLoopRange
  , loopingEnabled
  , setLoopingEnabled
  , numberOfLoops
  , setNumberOfLoops
  , offsetTime
  , setOffsetTime
  , muted
  , setMuted
  , soloed
  , setSoloed
  , lengthInBeats
  , setLengthInBeats
  , lengthInSeconds
  , setLengthInSeconds
  , timeResolution
  , usesAutomatedParameters
  , setUsesAutomatedParameters
  , addEvent_atBeatSelector
  , clearEventsInRangeSelector
  , copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector
  , copyEventsInRange_fromTrack_insertAtBeatSelector
  , cutEventsInRangeSelector
  , destinationAudioUnitSelector
  , destinationMIDIEndpointSelector
  , enumerateEventsInRange_usingBlockSelector
  , lengthInBeatsSelector
  , lengthInSecondsSelector
  , loopRangeSelector
  , loopingEnabledSelector
  , moveEventsInRange_byAmountSelector
  , mutedSelector
  , numberOfLoopsSelector
  , offsetTimeSelector
  , setDestinationAudioUnitSelector
  , setDestinationMIDIEndpointSelector
  , setLengthInBeatsSelector
  , setLengthInSecondsSelector
  , setLoopRangeSelector
  , setLoopingEnabledSelector
  , setMutedSelector
  , setNumberOfLoopsSelector
  , setOffsetTimeSelector
  , setSoloedSelector
  , setUsesAutomatedParametersSelector
  , soloedSelector
  , timeResolutionSelector
  , usesAutomatedParametersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | addAvent:atBeat
--
-- Adds an AVMusicEvent's contents to a track at the specified AVMusicTimeStamp.
--
-- @event@ — the event to be added
--
-- @beat@ — the AVMusicTimeStamp
--
-- Because event contents are copied into the track, the same event may be added multiple				times at different timestamps.
--
-- There are restrictions on which AVMusicEvent subclasses may be added to different tracks:
--
-- - Only AVExtendedTempoEvents and AVMIDIMetaEvents with certain AVMIDIMetaEventTypes				  can be added to an AVMusicSequence's tempo track (see AVMIDIMetaEvent).
--
-- - AVParameterEvents can only be added to automation tracks (see AVParameterEvent).
--
-- - All other event subclasses cannot be added to tempo or automation tracks.
--
-- ObjC selector: @- addEvent:atBeat:@
addEvent_atBeat :: (IsAVMusicTrack avMusicTrack, IsAVMusicEvent event) => avMusicTrack -> event -> CDouble -> IO ()
addEvent_atBeat avMusicTrack event beat =
  sendMessage avMusicTrack addEvent_atBeatSelector (toAVMusicEvent event) beat

-- | moveEventsInRange:byAmount
--
-- Shift the beat location of all events in the given beat range by the amount specified.
--
-- @range@ — the range of beats.  Must be a valid AVBeatRange.
--
-- @beatAmount@ — the amount in beats to shift each event.  The amount may be positive or negative.
--
-- ObjC selector: @- moveEventsInRange:byAmount:@
moveEventsInRange_byAmount :: IsAVMusicTrack avMusicTrack => avMusicTrack -> AVBeatRange -> CDouble -> IO ()
moveEventsInRange_byAmount avMusicTrack range beatAmount =
  sendMessage avMusicTrack moveEventsInRange_byAmountSelector range beatAmount

-- | clearEventsInRange:
--
-- Removes all events in the given beat range, erasing that portion of the AVMusicTrack.
--
-- @range@ — the range of beats.  Must be a valid AVBeatRange.
--
-- All events outside of the specified range left unmodified.
--
-- ObjC selector: @- clearEventsInRange:@
clearEventsInRange :: IsAVMusicTrack avMusicTrack => avMusicTrack -> AVBeatRange -> IO ()
clearEventsInRange avMusicTrack range =
  sendMessage avMusicTrack clearEventsInRangeSelector range

-- | cutEventsInRange:
--
-- Removes all events in the given beat range, splicing out that portion of the AVMusicTrack.
--
-- @range@ — the range of beats.  Must be a valid AVBeatRange.
--
-- All events past the end of the specified range will be shifted backward by the duration of the range.
--
-- ObjC selector: @- cutEventsInRange:@
cutEventsInRange :: IsAVMusicTrack avMusicTrack => avMusicTrack -> AVBeatRange -> IO ()
cutEventsInRange avMusicTrack range =
  sendMessage avMusicTrack cutEventsInRangeSelector range

-- | copyEventsInRange:fromTrack:insertAtBeat
--
-- Copies all events in the given beat range from the specified AVMusicTrack,				splicing them into the current AVMusicTrack.
--
-- @range@ — the range of beats.  Must be a valid AVBeatRange.
--
-- @sourceTrack@ — the AVMusicTrack to copy the events from.
--
-- @insertStartBeat@ — the start beat at which the copied events should be spliced in.
--
-- All events originally at or past insertStartBeat will be shifted forward by the duration				of the copied-in range.
--
-- ObjC selector: @- copyEventsInRange:fromTrack:insertAtBeat:@
copyEventsInRange_fromTrack_insertAtBeat :: (IsAVMusicTrack avMusicTrack, IsAVMusicTrack sourceTrack) => avMusicTrack -> AVBeatRange -> sourceTrack -> CDouble -> IO ()
copyEventsInRange_fromTrack_insertAtBeat avMusicTrack range sourceTrack insertStartBeat =
  sendOwnedMessage avMusicTrack copyEventsInRange_fromTrack_insertAtBeatSelector range (toAVMusicTrack sourceTrack) insertStartBeat

-- | copyAndMergeEventsInRange:fromTrack:mergeAtBeat
--
-- Copies all events in the given beat range from the specified AVMusicTrack,				merging them into the current AVMusicTrack.
--
-- @range@ — the range of beats.  Must be a valid AVBeatRange.
--
-- @sourceTrack@ — the AVMusicTrack to copy the events from.
--
-- @insertStartBeat@ — the start beat at which the copied events should be merged.
--
-- All events originally at or past mergeStartBeat will be left unmodified.
--
-- Copying events from track to track follows the same type-exclusion rules as adding				events:  The operation will generate an exception.
--
-- ObjC selector: @- copyAndMergeEventsInRange:fromTrack:mergeAtBeat:@
copyAndMergeEventsInRange_fromTrack_mergeAtBeat :: (IsAVMusicTrack avMusicTrack, IsAVMusicTrack sourceTrack) => avMusicTrack -> AVBeatRange -> sourceTrack -> CDouble -> IO ()
copyAndMergeEventsInRange_fromTrack_mergeAtBeat avMusicTrack range sourceTrack mergeStartBeat =
  sendOwnedMessage avMusicTrack copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector range (toAVMusicTrack sourceTrack) mergeStartBeat

-- | enumerateEventsInRange:usingBlock:
--
-- Iterates through the AVMusicEvents within the AVMusicTrack whose timestamps fit within the range,				calling the block for each.
--
-- @block@ — the AVMusicEventEnumerationBlock to call for each event.
--
-- Each event returned via the block should be examined using @NSObject(isKindOfClass:)@				to determine its subclass and then cast and accessed/edited accordingly.
--
-- The iteration may continue after removing an event.
--
-- The event objects returned via the block will not be the same instances				which were added to the AVMusicTrack, though their contents will be identical.
--
-- ObjC selector: @- enumerateEventsInRange:usingBlock:@
enumerateEventsInRange_usingBlock :: IsAVMusicTrack avMusicTrack => avMusicTrack -> AVBeatRange -> Ptr () -> IO ()
enumerateEventsInRange_usingBlock avMusicTrack range block =
  sendMessage avMusicTrack enumerateEventsInRange_usingBlockSelector range block

-- | destinationAudioUnit
--
-- The AVAudioUnit which will receive the track's events
--
-- This is mutually exclusive with setting a destination MIDIEndpoint.  The AU must already be		attached to an audio engine, and the track must be part of the AVAudioSequencer associated		with that engine. When playing, the track will send its events to that AVAudioUnit. The		destination AU cannot be changed while the track's sequence is playing.
--
-- ObjC selector: @- destinationAudioUnit@
destinationAudioUnit :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO (Id AVAudioUnit)
destinationAudioUnit avMusicTrack =
  sendMessage avMusicTrack destinationAudioUnitSelector

-- | destinationAudioUnit
--
-- The AVAudioUnit which will receive the track's events
--
-- This is mutually exclusive with setting a destination MIDIEndpoint.  The AU must already be		attached to an audio engine, and the track must be part of the AVAudioSequencer associated		with that engine. When playing, the track will send its events to that AVAudioUnit. The		destination AU cannot be changed while the track's sequence is playing.
--
-- ObjC selector: @- setDestinationAudioUnit:@
setDestinationAudioUnit :: (IsAVMusicTrack avMusicTrack, IsAVAudioUnit value) => avMusicTrack -> value -> IO ()
setDestinationAudioUnit avMusicTrack value =
  sendMessage avMusicTrack setDestinationAudioUnitSelector (toAVAudioUnit value)

-- | @- destinationMIDIEndpoint@
destinationMIDIEndpoint :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CUInt
destinationMIDIEndpoint avMusicTrack =
  sendMessage avMusicTrack destinationMIDIEndpointSelector

-- | @- setDestinationMIDIEndpoint:@
setDestinationMIDIEndpoint :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CUInt -> IO ()
setDestinationMIDIEndpoint avMusicTrack value =
  sendMessage avMusicTrack setDestinationMIDIEndpointSelector value

-- | loopRange
--
-- The timestamp range in beats for the loop
--
-- The loop is set by specifying its beat range.
--
-- ObjC selector: @- loopRange@
loopRange :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO AVBeatRange
loopRange avMusicTrack =
  sendMessage avMusicTrack loopRangeSelector

-- | loopRange
--
-- The timestamp range in beats for the loop
--
-- The loop is set by specifying its beat range.
--
-- ObjC selector: @- setLoopRange:@
setLoopRange :: IsAVMusicTrack avMusicTrack => avMusicTrack -> AVBeatRange -> IO ()
setLoopRange avMusicTrack value =
  sendMessage avMusicTrack setLoopRangeSelector value

-- | loopingEnabled
--
-- Determines whether or not the track is looped.
--
-- If loopRange has not been set, the full track will be looped.
--
-- ObjC selector: @- loopingEnabled@
loopingEnabled :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
loopingEnabled avMusicTrack =
  sendMessage avMusicTrack loopingEnabledSelector

-- | loopingEnabled
--
-- Determines whether or not the track is looped.
--
-- If loopRange has not been set, the full track will be looped.
--
-- ObjC selector: @- setLoopingEnabled:@
setLoopingEnabled :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setLoopingEnabled avMusicTrack value =
  sendMessage avMusicTrack setLoopingEnabledSelector value

-- | numberOfLoops
--
-- The number of times that the track's loop will repeat
--
-- If set to AVMusicTrackLoopCountForever, the track will loop forever.		Otherwise, legal values start with 1.
--
-- ObjC selector: @- numberOfLoops@
numberOfLoops :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CLong
numberOfLoops avMusicTrack =
  sendMessage avMusicTrack numberOfLoopsSelector

-- | numberOfLoops
--
-- The number of times that the track's loop will repeat
--
-- If set to AVMusicTrackLoopCountForever, the track will loop forever.		Otherwise, legal values start with 1.
--
-- ObjC selector: @- setNumberOfLoops:@
setNumberOfLoops :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CLong -> IO ()
setNumberOfLoops avMusicTrack value =
  sendMessage avMusicTrack setNumberOfLoopsSelector value

-- | offsetTime
--
-- Offset the track's start time to the specified time in beats
--
-- By default this value is zero.
--
-- ObjC selector: @- offsetTime@
offsetTime :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CDouble
offsetTime avMusicTrack =
  sendMessage avMusicTrack offsetTimeSelector

-- | offsetTime
--
-- Offset the track's start time to the specified time in beats
--
-- By default this value is zero.
--
-- ObjC selector: @- setOffsetTime:@
setOffsetTime :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CDouble -> IO ()
setOffsetTime avMusicTrack value =
  sendMessage avMusicTrack setOffsetTimeSelector value

-- | muted
--
-- Whether the track is muted
--
-- ObjC selector: @- muted@
muted :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
muted avMusicTrack =
  sendMessage avMusicTrack mutedSelector

-- | muted
--
-- Whether the track is muted
--
-- ObjC selector: @- setMuted:@
setMuted :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setMuted avMusicTrack value =
  sendMessage avMusicTrack setMutedSelector value

-- | soloed
--
-- Whether the track is soloed
--
-- ObjC selector: @- soloed@
soloed :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
soloed avMusicTrack =
  sendMessage avMusicTrack soloedSelector

-- | soloed
--
-- Whether the track is soloed
--
-- ObjC selector: @- setSoloed:@
setSoloed :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setSoloed avMusicTrack value =
  sendMessage avMusicTrack setSoloedSelector value

-- | lengthInBeats
--
-- The total duration of the track in beats
--
-- This will return the beat of the last event in the track plus any additional time that may		be needed for fading out of ending notes or round a loop point to musical bar, etc.  If this		has not been set by the user, the track length will always be adjusted to the end of the		last active event in a track and is adjusted dynamically as events are added or removed.
--
-- The property will return the maximum of the user-set track length, or the calculated length.
--
-- ObjC selector: @- lengthInBeats@
lengthInBeats :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CDouble
lengthInBeats avMusicTrack =
  sendMessage avMusicTrack lengthInBeatsSelector

-- | lengthInBeats
--
-- The total duration of the track in beats
--
-- This will return the beat of the last event in the track plus any additional time that may		be needed for fading out of ending notes or round a loop point to musical bar, etc.  If this		has not been set by the user, the track length will always be adjusted to the end of the		last active event in a track and is adjusted dynamically as events are added or removed.
--
-- The property will return the maximum of the user-set track length, or the calculated length.
--
-- ObjC selector: @- setLengthInBeats:@
setLengthInBeats :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CDouble -> IO ()
setLengthInBeats avMusicTrack value =
  sendMessage avMusicTrack setLengthInBeatsSelector value

-- | lengthInSeconds
--
-- The total duration of the track in seconds
--
-- This will return time of the last event in the track plus any additional time that may be		needed for fading out of ending notes or round a loop point to musical bar, etc.  If this		has not been set by the user, the track length will always be adjusted to the end of the		last active event in a track and is adjusted dynamically as events are added or removed.
--
-- The property will return the maximum of the user-set track length, or the calculated length.
--
-- ObjC selector: @- lengthInSeconds@
lengthInSeconds :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CDouble
lengthInSeconds avMusicTrack =
  sendMessage avMusicTrack lengthInSecondsSelector

-- | lengthInSeconds
--
-- The total duration of the track in seconds
--
-- This will return time of the last event in the track plus any additional time that may be		needed for fading out of ending notes or round a loop point to musical bar, etc.  If this		has not been set by the user, the track length will always be adjusted to the end of the		last active event in a track and is adjusted dynamically as events are added or removed.
--
-- The property will return the maximum of the user-set track length, or the calculated length.
--
-- ObjC selector: @- setLengthInSeconds:@
setLengthInSeconds :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CDouble -> IO ()
setLengthInSeconds avMusicTrack value =
  sendMessage avMusicTrack setLengthInSecondsSelector value

-- | timeResolution
--
-- The time resolution value for the sequence, in ticks (pulses) per quarter note (PPQN)
--
-- If a MIDI file was used to construct the containing sequence, the resolution will be what		was in the file. If you want to keep a time resolution when writing a new file, you can		retrieve this value and then specify it when calling -[AVAudioSequencer		writeToFile:flags:withResolution]. It has no direct bearing on the rendering or notion of		time of the sequence itself, just its representation in MIDI files. By default this is set		to either 480 if the sequence was created manually, or a value based on what was in a MIDI		file if the sequence was created from a MIDI file.
--
-- This can only be retrieved from the tempo track.
--
-- ObjC selector: @- timeResolution@
timeResolution :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CULong
timeResolution avMusicTrack =
  sendMessage avMusicTrack timeResolutionSelector

-- | usesAutomatedParameters
--
-- Indicates whether the track is an automation track.
--
-- If set to YES, this can be used to contain, parameter automation events, exclusively.				Adding any other event types will generate exceptions.
--
-- If a track already contains non-parameter events, setting this to YES will				generate an exception.
--
-- ObjC selector: @- usesAutomatedParameters@
usesAutomatedParameters :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
usesAutomatedParameters avMusicTrack =
  sendMessage avMusicTrack usesAutomatedParametersSelector

-- | usesAutomatedParameters
--
-- Indicates whether the track is an automation track.
--
-- If set to YES, this can be used to contain, parameter automation events, exclusively.				Adding any other event types will generate exceptions.
--
-- If a track already contains non-parameter events, setting this to YES will				generate an exception.
--
-- ObjC selector: @- setUsesAutomatedParameters:@
setUsesAutomatedParameters :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setUsesAutomatedParameters avMusicTrack value =
  sendMessage avMusicTrack setUsesAutomatedParametersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addEvent:atBeat:@
addEvent_atBeatSelector :: Selector '[Id AVMusicEvent, CDouble] ()
addEvent_atBeatSelector = mkSelector "addEvent:atBeat:"

-- | @Selector@ for @moveEventsInRange:byAmount:@
moveEventsInRange_byAmountSelector :: Selector '[AVBeatRange, CDouble] ()
moveEventsInRange_byAmountSelector = mkSelector "moveEventsInRange:byAmount:"

-- | @Selector@ for @clearEventsInRange:@
clearEventsInRangeSelector :: Selector '[AVBeatRange] ()
clearEventsInRangeSelector = mkSelector "clearEventsInRange:"

-- | @Selector@ for @cutEventsInRange:@
cutEventsInRangeSelector :: Selector '[AVBeatRange] ()
cutEventsInRangeSelector = mkSelector "cutEventsInRange:"

-- | @Selector@ for @copyEventsInRange:fromTrack:insertAtBeat:@
copyEventsInRange_fromTrack_insertAtBeatSelector :: Selector '[AVBeatRange, Id AVMusicTrack, CDouble] ()
copyEventsInRange_fromTrack_insertAtBeatSelector = mkSelector "copyEventsInRange:fromTrack:insertAtBeat:"

-- | @Selector@ for @copyAndMergeEventsInRange:fromTrack:mergeAtBeat:@
copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector :: Selector '[AVBeatRange, Id AVMusicTrack, CDouble] ()
copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector = mkSelector "copyAndMergeEventsInRange:fromTrack:mergeAtBeat:"

-- | @Selector@ for @enumerateEventsInRange:usingBlock:@
enumerateEventsInRange_usingBlockSelector :: Selector '[AVBeatRange, Ptr ()] ()
enumerateEventsInRange_usingBlockSelector = mkSelector "enumerateEventsInRange:usingBlock:"

-- | @Selector@ for @destinationAudioUnit@
destinationAudioUnitSelector :: Selector '[] (Id AVAudioUnit)
destinationAudioUnitSelector = mkSelector "destinationAudioUnit"

-- | @Selector@ for @setDestinationAudioUnit:@
setDestinationAudioUnitSelector :: Selector '[Id AVAudioUnit] ()
setDestinationAudioUnitSelector = mkSelector "setDestinationAudioUnit:"

-- | @Selector@ for @destinationMIDIEndpoint@
destinationMIDIEndpointSelector :: Selector '[] CUInt
destinationMIDIEndpointSelector = mkSelector "destinationMIDIEndpoint"

-- | @Selector@ for @setDestinationMIDIEndpoint:@
setDestinationMIDIEndpointSelector :: Selector '[CUInt] ()
setDestinationMIDIEndpointSelector = mkSelector "setDestinationMIDIEndpoint:"

-- | @Selector@ for @loopRange@
loopRangeSelector :: Selector '[] AVBeatRange
loopRangeSelector = mkSelector "loopRange"

-- | @Selector@ for @setLoopRange:@
setLoopRangeSelector :: Selector '[AVBeatRange] ()
setLoopRangeSelector = mkSelector "setLoopRange:"

-- | @Selector@ for @loopingEnabled@
loopingEnabledSelector :: Selector '[] Bool
loopingEnabledSelector = mkSelector "loopingEnabled"

-- | @Selector@ for @setLoopingEnabled:@
setLoopingEnabledSelector :: Selector '[Bool] ()
setLoopingEnabledSelector = mkSelector "setLoopingEnabled:"

-- | @Selector@ for @numberOfLoops@
numberOfLoopsSelector :: Selector '[] CLong
numberOfLoopsSelector = mkSelector "numberOfLoops"

-- | @Selector@ for @setNumberOfLoops:@
setNumberOfLoopsSelector :: Selector '[CLong] ()
setNumberOfLoopsSelector = mkSelector "setNumberOfLoops:"

-- | @Selector@ for @offsetTime@
offsetTimeSelector :: Selector '[] CDouble
offsetTimeSelector = mkSelector "offsetTime"

-- | @Selector@ for @setOffsetTime:@
setOffsetTimeSelector :: Selector '[CDouble] ()
setOffsetTimeSelector = mkSelector "setOffsetTime:"

-- | @Selector@ for @muted@
mutedSelector :: Selector '[] Bool
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector '[Bool] ()
setMutedSelector = mkSelector "setMuted:"

-- | @Selector@ for @soloed@
soloedSelector :: Selector '[] Bool
soloedSelector = mkSelector "soloed"

-- | @Selector@ for @setSoloed:@
setSoloedSelector :: Selector '[Bool] ()
setSoloedSelector = mkSelector "setSoloed:"

-- | @Selector@ for @lengthInBeats@
lengthInBeatsSelector :: Selector '[] CDouble
lengthInBeatsSelector = mkSelector "lengthInBeats"

-- | @Selector@ for @setLengthInBeats:@
setLengthInBeatsSelector :: Selector '[CDouble] ()
setLengthInBeatsSelector = mkSelector "setLengthInBeats:"

-- | @Selector@ for @lengthInSeconds@
lengthInSecondsSelector :: Selector '[] CDouble
lengthInSecondsSelector = mkSelector "lengthInSeconds"

-- | @Selector@ for @setLengthInSeconds:@
setLengthInSecondsSelector :: Selector '[CDouble] ()
setLengthInSecondsSelector = mkSelector "setLengthInSeconds:"

-- | @Selector@ for @timeResolution@
timeResolutionSelector :: Selector '[] CULong
timeResolutionSelector = mkSelector "timeResolution"

-- | @Selector@ for @usesAutomatedParameters@
usesAutomatedParametersSelector :: Selector '[] Bool
usesAutomatedParametersSelector = mkSelector "usesAutomatedParameters"

-- | @Selector@ for @setUsesAutomatedParameters:@
setUsesAutomatedParametersSelector :: Selector '[Bool] ()
setUsesAutomatedParametersSelector = mkSelector "setUsesAutomatedParameters:"

