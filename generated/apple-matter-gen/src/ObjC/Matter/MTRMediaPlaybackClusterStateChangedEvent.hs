{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterStateChangedEvent@.
module ObjC.Matter.MTRMediaPlaybackClusterStateChangedEvent
  ( MTRMediaPlaybackClusterStateChangedEvent
  , IsMTRMediaPlaybackClusterStateChangedEvent(..)
  , currentState
  , setCurrentState
  , startTime
  , setStartTime
  , duration
  , setDuration
  , sampledPosition
  , setSampledPosition
  , playbackSpeed
  , setPlaybackSpeed
  , seekRangeEnd
  , setSeekRangeEnd
  , seekRangeStart
  , setSeekRangeStart
  , data_
  , setData
  , audioAdvanceUnmuted
  , setAudioAdvanceUnmuted
  , audioAdvanceUnmutedSelector
  , currentStateSelector
  , dataSelector
  , durationSelector
  , playbackSpeedSelector
  , sampledPositionSelector
  , seekRangeEndSelector
  , seekRangeStartSelector
  , setAudioAdvanceUnmutedSelector
  , setCurrentStateSelector
  , setDataSelector
  , setDurationSelector
  , setPlaybackSpeedSelector
  , setSampledPositionSelector
  , setSeekRangeEndSelector
  , setSeekRangeStartSelector
  , setStartTimeSelector
  , startTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- currentState@
currentState :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
currentState mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent currentStateSelector

-- | @- setCurrentState:@
setCurrentState :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setCurrentState mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setCurrentStateSelector (toNSNumber value)

-- | @- startTime@
startTime :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
startTime mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setStartTime mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setStartTimeSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
duration mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setDuration mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setDurationSelector (toNSNumber value)

-- | @- sampledPosition@
sampledPosition :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id MTRMediaPlaybackClusterPlaybackPositionStruct)
sampledPosition mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent sampledPositionSelector

-- | @- setSampledPosition:@
setSampledPosition :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsMTRMediaPlaybackClusterPlaybackPositionStruct value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setSampledPosition mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setSampledPositionSelector (toMTRMediaPlaybackClusterPlaybackPositionStruct value)

-- | @- playbackSpeed@
playbackSpeed :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
playbackSpeed mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent playbackSpeedSelector

-- | @- setPlaybackSpeed:@
setPlaybackSpeed :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setPlaybackSpeed mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setPlaybackSpeedSelector (toNSNumber value)

-- | @- seekRangeEnd@
seekRangeEnd :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
seekRangeEnd mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent seekRangeEndSelector

-- | @- setSeekRangeEnd:@
setSeekRangeEnd :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setSeekRangeEnd mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setSeekRangeEndSelector (toNSNumber value)

-- | @- seekRangeStart@
seekRangeStart :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
seekRangeStart mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent seekRangeStartSelector

-- | @- setSeekRangeStart:@
setSeekRangeStart :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setSeekRangeStart mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setSeekRangeStartSelector (toNSNumber value)

-- | @- data@
data_ :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSData)
data_ mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent dataSelector

-- | @- setData:@
setData :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSData value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setData mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setDataSelector (toNSData value)

-- | @- audioAdvanceUnmuted@
audioAdvanceUnmuted :: IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent => mtrMediaPlaybackClusterStateChangedEvent -> IO (Id NSNumber)
audioAdvanceUnmuted mtrMediaPlaybackClusterStateChangedEvent =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent audioAdvanceUnmutedSelector

-- | @- setAudioAdvanceUnmuted:@
setAudioAdvanceUnmuted :: (IsMTRMediaPlaybackClusterStateChangedEvent mtrMediaPlaybackClusterStateChangedEvent, IsNSNumber value) => mtrMediaPlaybackClusterStateChangedEvent -> value -> IO ()
setAudioAdvanceUnmuted mtrMediaPlaybackClusterStateChangedEvent value =
  sendMessage mtrMediaPlaybackClusterStateChangedEvent setAudioAdvanceUnmutedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentState@
currentStateSelector :: Selector '[] (Id NSNumber)
currentStateSelector = mkSelector "currentState"

-- | @Selector@ for @setCurrentState:@
setCurrentStateSelector :: Selector '[Id NSNumber] ()
setCurrentStateSelector = mkSelector "setCurrentState:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @sampledPosition@
sampledPositionSelector :: Selector '[] (Id MTRMediaPlaybackClusterPlaybackPositionStruct)
sampledPositionSelector = mkSelector "sampledPosition"

-- | @Selector@ for @setSampledPosition:@
setSampledPositionSelector :: Selector '[Id MTRMediaPlaybackClusterPlaybackPositionStruct] ()
setSampledPositionSelector = mkSelector "setSampledPosition:"

-- | @Selector@ for @playbackSpeed@
playbackSpeedSelector :: Selector '[] (Id NSNumber)
playbackSpeedSelector = mkSelector "playbackSpeed"

-- | @Selector@ for @setPlaybackSpeed:@
setPlaybackSpeedSelector :: Selector '[Id NSNumber] ()
setPlaybackSpeedSelector = mkSelector "setPlaybackSpeed:"

-- | @Selector@ for @seekRangeEnd@
seekRangeEndSelector :: Selector '[] (Id NSNumber)
seekRangeEndSelector = mkSelector "seekRangeEnd"

-- | @Selector@ for @setSeekRangeEnd:@
setSeekRangeEndSelector :: Selector '[Id NSNumber] ()
setSeekRangeEndSelector = mkSelector "setSeekRangeEnd:"

-- | @Selector@ for @seekRangeStart@
seekRangeStartSelector :: Selector '[] (Id NSNumber)
seekRangeStartSelector = mkSelector "seekRangeStart"

-- | @Selector@ for @setSeekRangeStart:@
setSeekRangeStartSelector :: Selector '[Id NSNumber] ()
setSeekRangeStartSelector = mkSelector "setSeekRangeStart:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @audioAdvanceUnmuted@
audioAdvanceUnmutedSelector :: Selector '[] (Id NSNumber)
audioAdvanceUnmutedSelector = mkSelector "audioAdvanceUnmuted"

-- | @Selector@ for @setAudioAdvanceUnmuted:@
setAudioAdvanceUnmutedSelector :: Selector '[Id NSNumber] ()
setAudioAdvanceUnmutedSelector = mkSelector "setAudioAdvanceUnmuted:"

