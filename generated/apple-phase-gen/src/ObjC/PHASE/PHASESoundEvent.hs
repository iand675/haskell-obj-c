{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESoundEvent
--
-- A PHASESoundEvent is an object that represents a playable sound event in the PHASE system.
--
-- Generated bindings for @PHASESoundEvent@.
module ObjC.PHASE.PHASESoundEvent
  ( PHASESoundEvent
  , IsPHASESoundEvent(..)
  , init_
  , new
  , initWithEngine_assetIdentifier_mixerParameters_error
  , initWithEngine_assetIdentifier_error
  , prepareWithCompletion
  , startWithCompletion
  , startAtTime_completion
  , seekToTime_completion
  , seekToTime_resumeAtEngineTime_completion
  , pause
  , resume
  , resumeAtTime
  , stopAndInvalidate
  , renderingState
  , prepareState
  , metaParameters
  , mixers
  , pushStreamNodes
  , pullStreamNodes
  , indefinite
  , indefiniteSelector
  , initSelector
  , initWithEngine_assetIdentifier_errorSelector
  , initWithEngine_assetIdentifier_mixerParameters_errorSelector
  , metaParametersSelector
  , mixersSelector
  , newSelector
  , pauseSelector
  , prepareStateSelector
  , prepareWithCompletionSelector
  , pullStreamNodesSelector
  , pushStreamNodesSelector
  , renderingStateSelector
  , resumeAtTimeSelector
  , resumeSelector
  , seekToTime_completionSelector
  , seekToTime_resumeAtEngineTime_completionSelector
  , startAtTime_completionSelector
  , startWithCompletionSelector
  , stopAndInvalidateSelector

  -- * Enum types
  , PHASERenderingState(PHASERenderingState)
  , pattern PHASERenderingStateStopped
  , pattern PHASERenderingStateStarted
  , pattern PHASERenderingStatePaused
  , PHASESoundEventPrepareState(PHASESoundEventPrepareState)
  , pattern PHASESoundEventPrepareStatePrepareNotStarted
  , pattern PHASESoundEventPrepareStatePrepareInProgress
  , pattern PHASESoundEventPrepareStatePrepared

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id PHASESoundEvent)
init_ phaseSoundEvent =
  sendOwnedMessage phaseSoundEvent initSelector

-- | @+ new@
new :: IO (Id PHASESoundEvent)
new  =
  do
    cls' <- getRequiredClass "PHASESoundEvent"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:assetIdentifier:mixerParameters:error
--
-- Creates a new sound event instance
--
-- @engine@ — The PHASEEngine object that the sound event will be played by.
--
-- @assetIdentifier@ — The identifier registered with the Asset Registry for the particular PHASESoundEventNodeAsset that this sound instance will play.        If the asset identifier is not registered, this function will fail.
--
-- @mixerParameters@ — A dictionary of PHASEMixerParameters objects with keys that match the identifiers of the spatial mixers in the sound event
--
-- This will look up the asset in the asset registry and create the necessary objects to play the sound event
--
-- ObjC selector: @- initWithEngine:assetIdentifier:mixerParameters:error:@
initWithEngine_assetIdentifier_mixerParameters_error :: (IsPHASESoundEvent phaseSoundEvent, IsPHASEEngine engine, IsNSString assetIdentifier, IsPHASEMixerParameters mixerParameters, IsNSError error_) => phaseSoundEvent -> engine -> assetIdentifier -> mixerParameters -> error_ -> IO (Id PHASESoundEvent)
initWithEngine_assetIdentifier_mixerParameters_error phaseSoundEvent engine assetIdentifier mixerParameters error_ =
  sendOwnedMessage phaseSoundEvent initWithEngine_assetIdentifier_mixerParameters_errorSelector (toPHASEEngine engine) (toNSString assetIdentifier) (toPHASEMixerParameters mixerParameters) (toNSError error_)

-- | initWithEngine:assetIdentifier:error
--
-- Creates a new sound event instance
--
-- @engine@ — The PHASEEngine object that the sound event will be played by.
--
-- @assetIdentifier@ — The identifier registered with the Asset Registry for the particular PHASESoundEventNodeAsset that this sound event will play.        If the asset identifier is not registered, this function will fail.
--
-- This will look up the asset in the asset registry and create the necessary objects to play the sound event
--
-- ObjC selector: @- initWithEngine:assetIdentifier:error:@
initWithEngine_assetIdentifier_error :: (IsPHASESoundEvent phaseSoundEvent, IsPHASEEngine engine, IsNSString assetIdentifier, IsNSError error_) => phaseSoundEvent -> engine -> assetIdentifier -> error_ -> IO (Id PHASESoundEvent)
initWithEngine_assetIdentifier_error phaseSoundEvent engine assetIdentifier error_ =
  sendOwnedMessage phaseSoundEvent initWithEngine_assetIdentifier_errorSelector (toPHASEEngine engine) (toNSString assetIdentifier) (toNSError error_)

-- | prepareWithCompletion
--
-- Prepare the sound event
--
-- @handler@ — The block that will be called when the PHASESoundEvent has finished preparing and is ready to start. Pass in nil for no handler.
--
-- This function notifies the engine to begin preparing a sound event, then returns immediately.        Once the sound event is prepared (or has failed to prepare), you will receive a callback via the completion.        If you call startWithCompletion() before receiving the callback, the sound event will start as soon as it's prepared.
--
-- ObjC selector: @- prepareWithCompletion:@
prepareWithCompletion :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> Ptr () -> IO ()
prepareWithCompletion phaseSoundEvent handler =
  sendMessage phaseSoundEvent prepareWithCompletionSelector handler

-- | startWithCompletion
--
-- Start the sound event
--
-- @handler@ — The block that will be called when the sound event has stopped.
--
-- This function notifies the engine to start the sound event, then returns immediately.        Once the sound event is playing (or has failed to start), you will receive a callback via the completion.        Playback will begin immediately if the sound event has been prepared; otherwise, it will start as soon as it is finished preparing.
--
-- ObjC selector: @- startWithCompletion:@
startWithCompletion :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> Ptr () -> IO ()
startWithCompletion phaseSoundEvent handler =
  sendMessage phaseSoundEvent startWithCompletionSelector handler

-- | startAtTime:completion
--
-- Start the sound event
--
-- @when@ — The desired start time based on the engine time retrieved from [PHASEEngine lastRenderTime]        If the sound event starts immediately with an audible sound, it will begin rendering at this time.  The sound event will otherwise begin operating at this time.        A nil value will start the sound event immediately        This time is not scaled by unitsPerSecond.
--
-- @handler@ — The block that will be called when the sound event has stopped.
--
-- This function notifies the engine to start the sound event, then returns immediately.        Once the sound event is playing (or has failed to start), you will receive a callback via the completion.        Playback will begin at the requested time if the sound event has finished preparing in time.        You may wait for preparation to finish with the [PHASESoundEvent prepare:completion] method before calling startAtTime, to ensure that the sound event will start at the desired time.        However if the desired time is far enough into the future to allow for preparation to happen, you may skip calling prepare entirely and just call startAtTime.
--
-- ObjC selector: @- startAtTime:completion:@
startAtTime_completion :: (IsPHASESoundEvent phaseSoundEvent, IsAVAudioTime when) => phaseSoundEvent -> when -> Ptr () -> IO ()
startAtTime_completion phaseSoundEvent when handler =
  sendMessage phaseSoundEvent startAtTime_completionSelector (toAVAudioTime when) handler

-- | seekToTime:completion
--
-- Seeks all leaf nodes in a PHASESoundEvent to a specified time relative to the start of the sound event.
--
-- This function notifies the engine to seek the sound event, then returns immediately.        Once the sound event has seeked to the new offset (or has failed to seek), you will receive a callback via the completion.        If any leaf nodes do not support seeking, those nodes will ignore this command.        Nodes that have finished playing or have stopped will not seek.        Nodes that are sleeping will seek, and will resume at the correct time when they wake up.
--
-- Note: The time is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- seekToTime:completion:@
seekToTime_completion :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> CDouble -> Ptr () -> IO ()
seekToTime_completion phaseSoundEvent time handler =
  sendMessage phaseSoundEvent seekToTime_completionSelector time handler

-- | seekToTime:resumeAtEngineTime:completion
--
-- Seeks all leaf nodes in a PHASESoundEvent to the specified time, and automatically resumes playback at the specified engine time.
--
-- @time@ — The desired time position in seconds to seek the nodes to.
--
-- @engineTime@ — The engine time to resume playback.
--
-- @handler@ — The completion callback that will be called when seeking is complete.
--
-- This is a low latency convenience method that allows for tight deadlines to be met.  However if the seek fails the node state will not be changed.  You should check the callback and handle the failure appropriately.        The time parameter will seek the nodes to the equivalent sample position based on the sample rate of the asset.        The engineTime parameter is the engine timestamp to resume rendering at, based off of [PHASEEngine lastRenderTime].        If any leaf nodes do not support seeking, those nodes will ignore this command.        Nodes that have finished playing or have stopped will not seek.        The time parameter is in seconds and will be scaled by unitsPerSecond.        The time in the AVAudioTime structure is not scaled by unitsPerSecond.        The engineTime parameter will use the sample time if valid, if not, then the host time if valid.
--
-- ObjC selector: @- seekToTime:resumeAtEngineTime:completion:@
seekToTime_resumeAtEngineTime_completion :: (IsPHASESoundEvent phaseSoundEvent, IsAVAudioTime engineTime) => phaseSoundEvent -> CDouble -> engineTime -> Ptr () -> IO ()
seekToTime_resumeAtEngineTime_completion phaseSoundEvent time engineTime handler =
  sendMessage phaseSoundEvent seekToTime_resumeAtEngineTime_completionSelector time (toAVAudioTime engineTime) handler

-- | pause
--
-- Pause the sound event.
--
-- ObjC selector: @- pause@
pause :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO ()
pause phaseSoundEvent =
  sendMessage phaseSoundEvent pauseSelector

-- | resume
--
-- Resume the sound event.
--
-- ObjC selector: @- resume@
resume :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO ()
resume phaseSoundEvent =
  sendMessage phaseSoundEvent resumeSelector

-- | resumeAtTime
--
-- Resume the sound event at a specific time
--
-- @time@ — The desired start time based on the engine time retrieved from [PHASEEngine lastRenderTime]
--
-- A nil time parameter will resume immediately.        The device time is not scaled by UnitsPerSecond and is in seconds.
--
-- ObjC selector: @- resumeAtTime:@
resumeAtTime :: (IsPHASESoundEvent phaseSoundEvent, IsAVAudioTime time) => phaseSoundEvent -> time -> IO ()
resumeAtTime phaseSoundEvent time =
  sendMessage phaseSoundEvent resumeAtTimeSelector (toAVAudioTime time)

-- | stopAndInvalidate
--
-- stop and invalidate the sound event
--
-- ObjC selector: @- stopAndInvalidate@
stopAndInvalidate :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO ()
stopAndInvalidate phaseSoundEvent =
  sendMessage phaseSoundEvent stopAndInvalidateSelector

-- | renderingState
--
-- Sound Event's current rendering state
--
-- ObjC selector: @- renderingState@
renderingState :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO PHASERenderingState
renderingState phaseSoundEvent =
  sendMessage phaseSoundEvent renderingStateSelector

-- | prepareState
--
-- Sound Event's current preparation state
--
-- ObjC selector: @- prepareState@
prepareState :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO PHASESoundEventPrepareState
prepareState phaseSoundEvent =
  sendMessage phaseSoundEvent prepareStateSelector

-- | metaParameters
--
-- A Dictionary containing the MetaParameters associated with this sound event
--
-- ObjC selector: @- metaParameters@
metaParameters :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
metaParameters phaseSoundEvent =
  sendMessage phaseSoundEvent metaParametersSelector

-- | mixNodes
--
-- A Dictionary containing the mix nodes associated with this sound event
--
-- ObjC selector: @- mixers@
mixers :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
mixers phaseSoundEvent =
  sendMessage phaseSoundEvent mixersSelector

-- | pushStreamNodes
--
-- A Dictionary containing the push stream nodes associated with this sound event, for pushing buffers to.
--
-- ObjC selector: @- pushStreamNodes@
pushStreamNodes :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
pushStreamNodes phaseSoundEvent =
  sendMessage phaseSoundEvent pushStreamNodesSelector

-- | pullStreamNodes
--
-- A Dictionary containing the pull stream nodes associated with this sound event, for setting renderBlocks on.
--
-- ObjC selector: @- pullStreamNodes@
pullStreamNodes :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
pullStreamNodes phaseSoundEvent =
  sendMessage phaseSoundEvent pullStreamNodesSelector

-- | indefinite
--
-- A boolean that tell if this sound event will run indefinitely, or finish executing on its own
--
-- ObjC selector: @- indefinite@
indefinite :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO Bool
indefinite phaseSoundEvent =
  sendMessage phaseSoundEvent indefiniteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESoundEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESoundEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:assetIdentifier:mixerParameters:error:@
initWithEngine_assetIdentifier_mixerParameters_errorSelector :: Selector '[Id PHASEEngine, Id NSString, Id PHASEMixerParameters, Id NSError] (Id PHASESoundEvent)
initWithEngine_assetIdentifier_mixerParameters_errorSelector = mkSelector "initWithEngine:assetIdentifier:mixerParameters:error:"

-- | @Selector@ for @initWithEngine:assetIdentifier:error:@
initWithEngine_assetIdentifier_errorSelector :: Selector '[Id PHASEEngine, Id NSString, Id NSError] (Id PHASESoundEvent)
initWithEngine_assetIdentifier_errorSelector = mkSelector "initWithEngine:assetIdentifier:error:"

-- | @Selector@ for @prepareWithCompletion:@
prepareWithCompletionSelector :: Selector '[Ptr ()] ()
prepareWithCompletionSelector = mkSelector "prepareWithCompletion:"

-- | @Selector@ for @startWithCompletion:@
startWithCompletionSelector :: Selector '[Ptr ()] ()
startWithCompletionSelector = mkSelector "startWithCompletion:"

-- | @Selector@ for @startAtTime:completion:@
startAtTime_completionSelector :: Selector '[Id AVAudioTime, Ptr ()] ()
startAtTime_completionSelector = mkSelector "startAtTime:completion:"

-- | @Selector@ for @seekToTime:completion:@
seekToTime_completionSelector :: Selector '[CDouble, Ptr ()] ()
seekToTime_completionSelector = mkSelector "seekToTime:completion:"

-- | @Selector@ for @seekToTime:resumeAtEngineTime:completion:@
seekToTime_resumeAtEngineTime_completionSelector :: Selector '[CDouble, Id AVAudioTime, Ptr ()] ()
seekToTime_resumeAtEngineTime_completionSelector = mkSelector "seekToTime:resumeAtEngineTime:completion:"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @resumeAtTime:@
resumeAtTimeSelector :: Selector '[Id AVAudioTime] ()
resumeAtTimeSelector = mkSelector "resumeAtTime:"

-- | @Selector@ for @stopAndInvalidate@
stopAndInvalidateSelector :: Selector '[] ()
stopAndInvalidateSelector = mkSelector "stopAndInvalidate"

-- | @Selector@ for @renderingState@
renderingStateSelector :: Selector '[] PHASERenderingState
renderingStateSelector = mkSelector "renderingState"

-- | @Selector@ for @prepareState@
prepareStateSelector :: Selector '[] PHASESoundEventPrepareState
prepareStateSelector = mkSelector "prepareState"

-- | @Selector@ for @metaParameters@
metaParametersSelector :: Selector '[] (Id NSDictionary)
metaParametersSelector = mkSelector "metaParameters"

-- | @Selector@ for @mixers@
mixersSelector :: Selector '[] (Id NSDictionary)
mixersSelector = mkSelector "mixers"

-- | @Selector@ for @pushStreamNodes@
pushStreamNodesSelector :: Selector '[] (Id NSDictionary)
pushStreamNodesSelector = mkSelector "pushStreamNodes"

-- | @Selector@ for @pullStreamNodes@
pullStreamNodesSelector :: Selector '[] (Id NSDictionary)
pullStreamNodesSelector = mkSelector "pullStreamNodes"

-- | @Selector@ for @indefinite@
indefiniteSelector :: Selector '[] Bool
indefiniteSelector = mkSelector "indefinite"

