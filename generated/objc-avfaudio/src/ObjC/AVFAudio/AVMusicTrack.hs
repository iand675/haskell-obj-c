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
  , moveEventsInRange_byAmountSelector
  , clearEventsInRangeSelector
  , cutEventsInRangeSelector
  , copyEventsInRange_fromTrack_insertAtBeatSelector
  , copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector
  , enumerateEventsInRange_usingBlockSelector
  , destinationAudioUnitSelector
  , setDestinationAudioUnitSelector
  , destinationMIDIEndpointSelector
  , setDestinationMIDIEndpointSelector
  , loopRangeSelector
  , setLoopRangeSelector
  , loopingEnabledSelector
  , setLoopingEnabledSelector
  , numberOfLoopsSelector
  , setNumberOfLoopsSelector
  , offsetTimeSelector
  , setOffsetTimeSelector
  , mutedSelector
  , setMutedSelector
  , soloedSelector
  , setSoloedSelector
  , lengthInBeatsSelector
  , setLengthInBeatsSelector
  , lengthInSecondsSelector
  , setLengthInSecondsSelector
  , timeResolutionSelector
  , usesAutomatedParametersSelector
  , setUsesAutomatedParametersSelector


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
addEvent_atBeat avMusicTrack  event beat =
withObjCPtr event $ \raw_event ->
    sendMsg avMusicTrack (mkSelector "addEvent:atBeat:") retVoid [argPtr (castPtr raw_event :: Ptr ()), argCDouble (fromIntegral beat)]

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
moveEventsInRange_byAmount avMusicTrack  range beatAmount =
  sendMsg avMusicTrack (mkSelector "moveEventsInRange:byAmount:") retVoid [argAVBeatRange range, argCDouble (fromIntegral beatAmount)]

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
clearEventsInRange avMusicTrack  range =
  sendMsg avMusicTrack (mkSelector "clearEventsInRange:") retVoid [argAVBeatRange range]

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
cutEventsInRange avMusicTrack  range =
  sendMsg avMusicTrack (mkSelector "cutEventsInRange:") retVoid [argAVBeatRange range]

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
copyEventsInRange_fromTrack_insertAtBeat avMusicTrack  range sourceTrack insertStartBeat =
withObjCPtr sourceTrack $ \raw_sourceTrack ->
    sendMsg avMusicTrack (mkSelector "copyEventsInRange:fromTrack:insertAtBeat:") retVoid [argAVBeatRange range, argPtr (castPtr raw_sourceTrack :: Ptr ()), argCDouble (fromIntegral insertStartBeat)]

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
copyAndMergeEventsInRange_fromTrack_mergeAtBeat avMusicTrack  range sourceTrack mergeStartBeat =
withObjCPtr sourceTrack $ \raw_sourceTrack ->
    sendMsg avMusicTrack (mkSelector "copyAndMergeEventsInRange:fromTrack:mergeAtBeat:") retVoid [argAVBeatRange range, argPtr (castPtr raw_sourceTrack :: Ptr ()), argCDouble (fromIntegral mergeStartBeat)]

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
enumerateEventsInRange_usingBlock avMusicTrack  range block =
  sendMsg avMusicTrack (mkSelector "enumerateEventsInRange:usingBlock:") retVoid [argAVBeatRange range, argPtr (castPtr block :: Ptr ())]

-- | destinationAudioUnit
--
-- The AVAudioUnit which will receive the track's events
--
-- This is mutually exclusive with setting a destination MIDIEndpoint.  The AU must already be		attached to an audio engine, and the track must be part of the AVAudioSequencer associated		with that engine. When playing, the track will send its events to that AVAudioUnit. The		destination AU cannot be changed while the track's sequence is playing.
--
-- ObjC selector: @- destinationAudioUnit@
destinationAudioUnit :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO (Id AVAudioUnit)
destinationAudioUnit avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "destinationAudioUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | destinationAudioUnit
--
-- The AVAudioUnit which will receive the track's events
--
-- This is mutually exclusive with setting a destination MIDIEndpoint.  The AU must already be		attached to an audio engine, and the track must be part of the AVAudioSequencer associated		with that engine. When playing, the track will send its events to that AVAudioUnit. The		destination AU cannot be changed while the track's sequence is playing.
--
-- ObjC selector: @- setDestinationAudioUnit:@
setDestinationAudioUnit :: (IsAVMusicTrack avMusicTrack, IsAVAudioUnit value) => avMusicTrack -> value -> IO ()
setDestinationAudioUnit avMusicTrack  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMusicTrack (mkSelector "setDestinationAudioUnit:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- destinationMIDIEndpoint@
destinationMIDIEndpoint :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CUInt
destinationMIDIEndpoint avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "destinationMIDIEndpoint") retCUInt []

-- | @- setDestinationMIDIEndpoint:@
setDestinationMIDIEndpoint :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CUInt -> IO ()
setDestinationMIDIEndpoint avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setDestinationMIDIEndpoint:") retVoid [argCUInt (fromIntegral value)]

-- | loopRange
--
-- The timestamp range in beats for the loop
--
-- The loop is set by specifying its beat range.
--
-- ObjC selector: @- loopRange@
loopRange :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO AVBeatRange
loopRange avMusicTrack  =
  sendMsgStret avMusicTrack (mkSelector "loopRange") retAVBeatRange []

-- | loopRange
--
-- The timestamp range in beats for the loop
--
-- The loop is set by specifying its beat range.
--
-- ObjC selector: @- setLoopRange:@
setLoopRange :: IsAVMusicTrack avMusicTrack => avMusicTrack -> AVBeatRange -> IO ()
setLoopRange avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setLoopRange:") retVoid [argAVBeatRange value]

-- | loopingEnabled
--
-- Determines whether or not the track is looped.
--
-- If loopRange has not been set, the full track will be looped.
--
-- ObjC selector: @- loopingEnabled@
loopingEnabled :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
loopingEnabled avMusicTrack  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMusicTrack (mkSelector "loopingEnabled") retCULong []

-- | loopingEnabled
--
-- Determines whether or not the track is looped.
--
-- If loopRange has not been set, the full track will be looped.
--
-- ObjC selector: @- setLoopingEnabled:@
setLoopingEnabled :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setLoopingEnabled avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setLoopingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | numberOfLoops
--
-- The number of times that the track's loop will repeat
--
-- If set to AVMusicTrackLoopCountForever, the track will loop forever.		Otherwise, legal values start with 1.
--
-- ObjC selector: @- numberOfLoops@
numberOfLoops :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CLong
numberOfLoops avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "numberOfLoops") retCLong []

-- | numberOfLoops
--
-- The number of times that the track's loop will repeat
--
-- If set to AVMusicTrackLoopCountForever, the track will loop forever.		Otherwise, legal values start with 1.
--
-- ObjC selector: @- setNumberOfLoops:@
setNumberOfLoops :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CLong -> IO ()
setNumberOfLoops avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setNumberOfLoops:") retVoid [argCLong (fromIntegral value)]

-- | offsetTime
--
-- Offset the track's start time to the specified time in beats
--
-- By default this value is zero.
--
-- ObjC selector: @- offsetTime@
offsetTime :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO CDouble
offsetTime avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "offsetTime") retCDouble []

-- | offsetTime
--
-- Offset the track's start time to the specified time in beats
--
-- By default this value is zero.
--
-- ObjC selector: @- setOffsetTime:@
setOffsetTime :: IsAVMusicTrack avMusicTrack => avMusicTrack -> CDouble -> IO ()
setOffsetTime avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setOffsetTime:") retVoid [argCDouble (fromIntegral value)]

-- | muted
--
-- Whether the track is muted
--
-- ObjC selector: @- muted@
muted :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
muted avMusicTrack  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMusicTrack (mkSelector "muted") retCULong []

-- | muted
--
-- Whether the track is muted
--
-- ObjC selector: @- setMuted:@
setMuted :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setMuted avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setMuted:") retVoid [argCULong (if value then 1 else 0)]

-- | soloed
--
-- Whether the track is soloed
--
-- ObjC selector: @- soloed@
soloed :: IsAVMusicTrack avMusicTrack => avMusicTrack -> IO Bool
soloed avMusicTrack  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMusicTrack (mkSelector "soloed") retCULong []

-- | soloed
--
-- Whether the track is soloed
--
-- ObjC selector: @- setSoloed:@
setSoloed :: IsAVMusicTrack avMusicTrack => avMusicTrack -> Bool -> IO ()
setSoloed avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setSoloed:") retVoid [argCULong (if value then 1 else 0)]

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
lengthInBeats avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "lengthInBeats") retCDouble []

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
setLengthInBeats avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setLengthInBeats:") retVoid [argCDouble (fromIntegral value)]

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
lengthInSeconds avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "lengthInSeconds") retCDouble []

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
setLengthInSeconds avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setLengthInSeconds:") retVoid [argCDouble (fromIntegral value)]

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
timeResolution avMusicTrack  =
  sendMsg avMusicTrack (mkSelector "timeResolution") retCULong []

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
usesAutomatedParameters avMusicTrack  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMusicTrack (mkSelector "usesAutomatedParameters") retCULong []

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
setUsesAutomatedParameters avMusicTrack  value =
  sendMsg avMusicTrack (mkSelector "setUsesAutomatedParameters:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addEvent:atBeat:@
addEvent_atBeatSelector :: Selector
addEvent_atBeatSelector = mkSelector "addEvent:atBeat:"

-- | @Selector@ for @moveEventsInRange:byAmount:@
moveEventsInRange_byAmountSelector :: Selector
moveEventsInRange_byAmountSelector = mkSelector "moveEventsInRange:byAmount:"

-- | @Selector@ for @clearEventsInRange:@
clearEventsInRangeSelector :: Selector
clearEventsInRangeSelector = mkSelector "clearEventsInRange:"

-- | @Selector@ for @cutEventsInRange:@
cutEventsInRangeSelector :: Selector
cutEventsInRangeSelector = mkSelector "cutEventsInRange:"

-- | @Selector@ for @copyEventsInRange:fromTrack:insertAtBeat:@
copyEventsInRange_fromTrack_insertAtBeatSelector :: Selector
copyEventsInRange_fromTrack_insertAtBeatSelector = mkSelector "copyEventsInRange:fromTrack:insertAtBeat:"

-- | @Selector@ for @copyAndMergeEventsInRange:fromTrack:mergeAtBeat:@
copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector :: Selector
copyAndMergeEventsInRange_fromTrack_mergeAtBeatSelector = mkSelector "copyAndMergeEventsInRange:fromTrack:mergeAtBeat:"

-- | @Selector@ for @enumerateEventsInRange:usingBlock:@
enumerateEventsInRange_usingBlockSelector :: Selector
enumerateEventsInRange_usingBlockSelector = mkSelector "enumerateEventsInRange:usingBlock:"

-- | @Selector@ for @destinationAudioUnit@
destinationAudioUnitSelector :: Selector
destinationAudioUnitSelector = mkSelector "destinationAudioUnit"

-- | @Selector@ for @setDestinationAudioUnit:@
setDestinationAudioUnitSelector :: Selector
setDestinationAudioUnitSelector = mkSelector "setDestinationAudioUnit:"

-- | @Selector@ for @destinationMIDIEndpoint@
destinationMIDIEndpointSelector :: Selector
destinationMIDIEndpointSelector = mkSelector "destinationMIDIEndpoint"

-- | @Selector@ for @setDestinationMIDIEndpoint:@
setDestinationMIDIEndpointSelector :: Selector
setDestinationMIDIEndpointSelector = mkSelector "setDestinationMIDIEndpoint:"

-- | @Selector@ for @loopRange@
loopRangeSelector :: Selector
loopRangeSelector = mkSelector "loopRange"

-- | @Selector@ for @setLoopRange:@
setLoopRangeSelector :: Selector
setLoopRangeSelector = mkSelector "setLoopRange:"

-- | @Selector@ for @loopingEnabled@
loopingEnabledSelector :: Selector
loopingEnabledSelector = mkSelector "loopingEnabled"

-- | @Selector@ for @setLoopingEnabled:@
setLoopingEnabledSelector :: Selector
setLoopingEnabledSelector = mkSelector "setLoopingEnabled:"

-- | @Selector@ for @numberOfLoops@
numberOfLoopsSelector :: Selector
numberOfLoopsSelector = mkSelector "numberOfLoops"

-- | @Selector@ for @setNumberOfLoops:@
setNumberOfLoopsSelector :: Selector
setNumberOfLoopsSelector = mkSelector "setNumberOfLoops:"

-- | @Selector@ for @offsetTime@
offsetTimeSelector :: Selector
offsetTimeSelector = mkSelector "offsetTime"

-- | @Selector@ for @setOffsetTime:@
setOffsetTimeSelector :: Selector
setOffsetTimeSelector = mkSelector "setOffsetTime:"

-- | @Selector@ for @muted@
mutedSelector :: Selector
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector
setMutedSelector = mkSelector "setMuted:"

-- | @Selector@ for @soloed@
soloedSelector :: Selector
soloedSelector = mkSelector "soloed"

-- | @Selector@ for @setSoloed:@
setSoloedSelector :: Selector
setSoloedSelector = mkSelector "setSoloed:"

-- | @Selector@ for @lengthInBeats@
lengthInBeatsSelector :: Selector
lengthInBeatsSelector = mkSelector "lengthInBeats"

-- | @Selector@ for @setLengthInBeats:@
setLengthInBeatsSelector :: Selector
setLengthInBeatsSelector = mkSelector "setLengthInBeats:"

-- | @Selector@ for @lengthInSeconds@
lengthInSecondsSelector :: Selector
lengthInSecondsSelector = mkSelector "lengthInSeconds"

-- | @Selector@ for @setLengthInSeconds:@
setLengthInSecondsSelector :: Selector
setLengthInSecondsSelector = mkSelector "setLengthInSeconds:"

-- | @Selector@ for @timeResolution@
timeResolutionSelector :: Selector
timeResolutionSelector = mkSelector "timeResolution"

-- | @Selector@ for @usesAutomatedParameters@
usesAutomatedParametersSelector :: Selector
usesAutomatedParametersSelector = mkSelector "usesAutomatedParameters"

-- | @Selector@ for @setUsesAutomatedParameters:@
setUsesAutomatedParametersSelector :: Selector
setUsesAutomatedParametersSelector = mkSelector "setUsesAutomatedParameters:"

