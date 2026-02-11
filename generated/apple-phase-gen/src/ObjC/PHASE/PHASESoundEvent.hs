{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithEngine_assetIdentifier_mixerParameters_errorSelector
  , initWithEngine_assetIdentifier_errorSelector
  , prepareWithCompletionSelector
  , startWithCompletionSelector
  , startAtTime_completionSelector
  , seekToTime_completionSelector
  , seekToTime_resumeAtEngineTime_completionSelector
  , pauseSelector
  , resumeSelector
  , resumeAtTimeSelector
  , stopAndInvalidateSelector
  , renderingStateSelector
  , prepareStateSelector
  , metaParametersSelector
  , mixersSelector
  , pushStreamNodesSelector
  , pullStreamNodesSelector
  , indefiniteSelector

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

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id PHASESoundEvent)
init_ phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESoundEvent)
new  =
  do
    cls' <- getRequiredClass "PHASESoundEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithEngine_assetIdentifier_mixerParameters_error phaseSoundEvent  engine assetIdentifier mixerParameters error_ =
  withObjCPtr engine $ \raw_engine ->
    withObjCPtr assetIdentifier $ \raw_assetIdentifier ->
      withObjCPtr mixerParameters $ \raw_mixerParameters ->
        withObjCPtr error_ $ \raw_error_ ->
            sendMsg phaseSoundEvent (mkSelector "initWithEngine:assetIdentifier:mixerParameters:error:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argPtr (castPtr raw_assetIdentifier :: Ptr ()), argPtr (castPtr raw_mixerParameters :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

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
initWithEngine_assetIdentifier_error phaseSoundEvent  engine assetIdentifier error_ =
  withObjCPtr engine $ \raw_engine ->
    withObjCPtr assetIdentifier $ \raw_assetIdentifier ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg phaseSoundEvent (mkSelector "initWithEngine:assetIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argPtr (castPtr raw_assetIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

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
prepareWithCompletion phaseSoundEvent  handler =
    sendMsg phaseSoundEvent (mkSelector "prepareWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

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
startWithCompletion phaseSoundEvent  handler =
    sendMsg phaseSoundEvent (mkSelector "startWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

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
startAtTime_completion phaseSoundEvent  when handler =
  withObjCPtr when $ \raw_when ->
      sendMsg phaseSoundEvent (mkSelector "startAtTime:completion:") retVoid [argPtr (castPtr raw_when :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

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
seekToTime_completion phaseSoundEvent  time handler =
    sendMsg phaseSoundEvent (mkSelector "seekToTime:completion:") retVoid [argCDouble time, argPtr (castPtr handler :: Ptr ())]

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
seekToTime_resumeAtEngineTime_completion phaseSoundEvent  time engineTime handler =
  withObjCPtr engineTime $ \raw_engineTime ->
      sendMsg phaseSoundEvent (mkSelector "seekToTime:resumeAtEngineTime:completion:") retVoid [argCDouble time, argPtr (castPtr raw_engineTime :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | pause
--
-- Pause the sound event.
--
-- ObjC selector: @- pause@
pause :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO ()
pause phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "pause") retVoid []

-- | resume
--
-- Resume the sound event.
--
-- ObjC selector: @- resume@
resume :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO ()
resume phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "resume") retVoid []

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
resumeAtTime phaseSoundEvent  time =
  withObjCPtr time $ \raw_time ->
      sendMsg phaseSoundEvent (mkSelector "resumeAtTime:") retVoid [argPtr (castPtr raw_time :: Ptr ())]

-- | stopAndInvalidate
--
-- stop and invalidate the sound event
--
-- ObjC selector: @- stopAndInvalidate@
stopAndInvalidate :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO ()
stopAndInvalidate phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "stopAndInvalidate") retVoid []

-- | renderingState
--
-- Sound Event's current rendering state
--
-- ObjC selector: @- renderingState@
renderingState :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO PHASERenderingState
renderingState phaseSoundEvent  =
    fmap (coerce :: CLong -> PHASERenderingState) $ sendMsg phaseSoundEvent (mkSelector "renderingState") retCLong []

-- | prepareState
--
-- Sound Event's current preparation state
--
-- ObjC selector: @- prepareState@
prepareState :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO PHASESoundEventPrepareState
prepareState phaseSoundEvent  =
    fmap (coerce :: CLong -> PHASESoundEventPrepareState) $ sendMsg phaseSoundEvent (mkSelector "prepareState") retCLong []

-- | metaParameters
--
-- A Dictionary containing the MetaParameters associated with this sound event
--
-- ObjC selector: @- metaParameters@
metaParameters :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
metaParameters phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "metaParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mixNodes
--
-- A Dictionary containing the mix nodes associated with this sound event
--
-- ObjC selector: @- mixers@
mixers :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
mixers phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "mixers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pushStreamNodes
--
-- A Dictionary containing the push stream nodes associated with this sound event, for pushing buffers to.
--
-- ObjC selector: @- pushStreamNodes@
pushStreamNodes :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
pushStreamNodes phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "pushStreamNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pullStreamNodes
--
-- A Dictionary containing the pull stream nodes associated with this sound event, for setting renderBlocks on.
--
-- ObjC selector: @- pullStreamNodes@
pullStreamNodes :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO (Id NSDictionary)
pullStreamNodes phaseSoundEvent  =
    sendMsg phaseSoundEvent (mkSelector "pullStreamNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | indefinite
--
-- A boolean that tell if this sound event will run indefinitely, or finish executing on its own
--
-- ObjC selector: @- indefinite@
indefinite :: IsPHASESoundEvent phaseSoundEvent => phaseSoundEvent -> IO Bool
indefinite phaseSoundEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phaseSoundEvent (mkSelector "indefinite") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:assetIdentifier:mixerParameters:error:@
initWithEngine_assetIdentifier_mixerParameters_errorSelector :: Selector
initWithEngine_assetIdentifier_mixerParameters_errorSelector = mkSelector "initWithEngine:assetIdentifier:mixerParameters:error:"

-- | @Selector@ for @initWithEngine:assetIdentifier:error:@
initWithEngine_assetIdentifier_errorSelector :: Selector
initWithEngine_assetIdentifier_errorSelector = mkSelector "initWithEngine:assetIdentifier:error:"

-- | @Selector@ for @prepareWithCompletion:@
prepareWithCompletionSelector :: Selector
prepareWithCompletionSelector = mkSelector "prepareWithCompletion:"

-- | @Selector@ for @startWithCompletion:@
startWithCompletionSelector :: Selector
startWithCompletionSelector = mkSelector "startWithCompletion:"

-- | @Selector@ for @startAtTime:completion:@
startAtTime_completionSelector :: Selector
startAtTime_completionSelector = mkSelector "startAtTime:completion:"

-- | @Selector@ for @seekToTime:completion:@
seekToTime_completionSelector :: Selector
seekToTime_completionSelector = mkSelector "seekToTime:completion:"

-- | @Selector@ for @seekToTime:resumeAtEngineTime:completion:@
seekToTime_resumeAtEngineTime_completionSelector :: Selector
seekToTime_resumeAtEngineTime_completionSelector = mkSelector "seekToTime:resumeAtEngineTime:completion:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @resumeAtTime:@
resumeAtTimeSelector :: Selector
resumeAtTimeSelector = mkSelector "resumeAtTime:"

-- | @Selector@ for @stopAndInvalidate@
stopAndInvalidateSelector :: Selector
stopAndInvalidateSelector = mkSelector "stopAndInvalidate"

-- | @Selector@ for @renderingState@
renderingStateSelector :: Selector
renderingStateSelector = mkSelector "renderingState"

-- | @Selector@ for @prepareState@
prepareStateSelector :: Selector
prepareStateSelector = mkSelector "prepareState"

-- | @Selector@ for @metaParameters@
metaParametersSelector :: Selector
metaParametersSelector = mkSelector "metaParameters"

-- | @Selector@ for @mixers@
mixersSelector :: Selector
mixersSelector = mkSelector "mixers"

-- | @Selector@ for @pushStreamNodes@
pushStreamNodesSelector :: Selector
pushStreamNodesSelector = mkSelector "pushStreamNodes"

-- | @Selector@ for @pullStreamNodes@
pullStreamNodesSelector :: Selector
pullStreamNodesSelector = mkSelector "pullStreamNodes"

-- | @Selector@ for @indefinite@
indefiniteSelector :: Selector
indefiniteSelector = mkSelector "indefinite"

