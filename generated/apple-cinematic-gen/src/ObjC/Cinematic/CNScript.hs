{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Database of focus decisions with methods to change them. Knows what has been detected in each frame and which detection is being focused on. All operations are executed in a thread-safe manner, but that also means that a long-running update can stall a lookup. Best practice is to lookup what you need up front (outside your critical code) and pass the immutable results to where it's needed. That way, you're not blocked when you access the information, say inside the rendering portion of your code.
--
-- Generated bindings for @CNScript@.
module ObjC.Cinematic.CNScript
  ( CNScript
  , IsCNScript(..)
  , loadFromAsset_changes_progress_completionHandler
  , reloadWithChanges
  , changes
  , detectionTrackForID
  , detectionTrackForDecision
  , addUserDecision
  , removeUserDecision
  , removeAllUserDecisions
  , addDetectionTrack
  , removeDetectionTrack
  , init_
  , new
  , fNumber
  , setFNumber
  , addedDetectionTracks
  , addDetectionTrackSelector
  , addUserDecisionSelector
  , addedDetectionTracksSelector
  , changesSelector
  , detectionTrackForDecisionSelector
  , detectionTrackForIDSelector
  , fNumberSelector
  , initSelector
  , loadFromAsset_changes_progress_completionHandlerSelector
  , newSelector
  , reloadWithChangesSelector
  , removeAllUserDecisionsSelector
  , removeDetectionTrackSelector
  , removeUserDecisionSelector
  , setFNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load cinematic script asynchronously from a cinematic asset. - Parameters:   - asset: the cinematic asset to be loaded.   - changes: optional changes since asset was recorded. Can be obtained from a previous editing session. If @nil@, the asset is loaded as originally recorded.   - progress: optional progress object to track progress or cancel loading. Represents just the loading of this asset. Create with desired total unit count or use zero to have the unit count filled in automatically.  If @nil@, no progress is reported.   - completionHandler: called with the loaded cinematic script when done, or with with an error if it fails. If progress is canceled before it completes, the completion handler is called with an error.
--
-- ObjC selector: @+ loadFromAsset:changes:progress:completionHandler:@
loadFromAsset_changes_progress_completionHandler :: (IsAVAsset asset, IsCNScriptChanges changes, IsNSProgress progress) => asset -> changes -> progress -> Ptr () -> IO ()
loadFromAsset_changes_progress_completionHandler asset changes progress completionHandler =
  do
    cls' <- getRequiredClass "CNScript"
    sendClassMessage cls' loadFromAsset_changes_progress_completionHandlerSelector (toAVAsset asset) (toCNScriptChanges changes) (toNSProgress progress) completionHandler

-- | Reload the cinematic script with optional changes applied, removing any previous changes made. This can be more efficient than loading the asset from scratch. - Parameters:   - changes: optional changes since asset was recorded. Can be obtained from a previous editing session. If @nil@, the asset is reloaded as originally recorded.
--
-- ObjC selector: @- reloadWithChanges:@
reloadWithChanges :: (IsCNScript cnScript, IsCNScriptChanges changes) => cnScript -> changes -> IO ()
reloadWithChanges cnScript changes =
  sendMessage cnScript reloadWithChangesSelector (toCNScriptChanges changes)

-- | Changes made since cinematic asset was recorded. Can be used to checkpoint and later restore changes made so far.
--
-- ObjC selector: @- changes@
changes :: IsCNScript cnScript => cnScript -> IO (Id CNScriptChanges)
changes cnScript =
  sendMessage cnScript changesSelector

-- | A detection track representing all detections with the given detectionID over the entire cinematic script.
--
-- ObjC selector: @- detectionTrackForID:@
detectionTrackForID :: IsCNScript cnScript => cnScript -> CLong -> IO (Id CNDetectionTrack)
detectionTrackForID cnScript detectionID =
  sendMessage cnScript detectionTrackForIDSelector detectionID

-- | A detection track representing all detections that would be chosen by a given decision.
--
-- ObjC selector: @- detectionTrackForDecision:@
detectionTrackForDecision :: (IsCNScript cnScript, IsCNDecision decision) => cnScript -> decision -> IO (Id CNDetectionTrack)
detectionTrackForDecision cnScript decision =
  sendMessage cnScript detectionTrackForDecisionSelector (toCNDecision decision)

-- | Add a new user decision. Replaces an existing user decision if the times are identical.
--
-- Adding a decision can fail if the decision focuses on an detection or group that does not exist or if its time is not within the time range of the cinematic script.
--
-- - Returns: whether adding was successful
--
-- ObjC selector: @- addUserDecision:@
addUserDecision :: (IsCNScript cnScript, IsCNDecision decision) => cnScript -> decision -> IO Bool
addUserDecision cnScript decision =
  sendMessage cnScript addUserDecisionSelector (toCNDecision decision)

-- | Remove an existing user decision.
--
-- User decisions added to the script or those made at recording time (by tapping during recording) can be removed. Decisions that are not user decisions cannot be removed.
--
-- - Returns: whether removal was successful
--
-- ObjC selector: @- removeUserDecision:@
removeUserDecision :: (IsCNScript cnScript, IsCNDecision decision) => cnScript -> decision -> IO Bool
removeUserDecision cnScript decision =
  sendMessage cnScript removeUserDecisionSelector (toCNDecision decision)

-- | Remove all user decisions and revert to base decisions only.
--
-- ObjC selector: @- removeAllUserDecisions@
removeAllUserDecisions :: IsCNScript cnScript => cnScript -> IO ()
removeAllUserDecisions cnScript =
  sendMessage cnScript removeAllUserDecisionsSelector

-- | Add user created detection track.
--
-- - Returns: the detectionID assigned to the added track, which can be used for later lookup or decision creation.
--
-- ObjC selector: @- addDetectionTrack:@
addDetectionTrack :: (IsCNScript cnScript, IsCNDetectionTrack detectionTrack) => cnScript -> detectionTrack -> IO CLong
addDetectionTrack cnScript detectionTrack =
  sendMessage cnScript addDetectionTrackSelector (toCNDetectionTrack detectionTrack)

-- | Remove user created detection track.
--
-- Tracks created at recording time cannot be removed.
--
-- - Returns: whether removal was successful
--
-- ObjC selector: @- removeDetectionTrack:@
removeDetectionTrack :: (IsCNScript cnScript, IsCNDetectionTrack detectionTrack) => cnScript -> detectionTrack -> IO Bool
removeDetectionTrack cnScript detectionTrack =
  sendMessage cnScript removeDetectionTrackSelector (toCNDetectionTrack detectionTrack)

-- | @- init@
init_ :: IsCNScript cnScript => cnScript -> IO (Id CNScript)
init_ cnScript =
  sendOwnedMessage cnScript initSelector

-- | @+ new@
new :: IO (Id CNScript)
new  =
  do
    cls' <- getRequiredClass "CNScript"
    sendOwnedClassMessage cls' newSelector

-- | The f/number to apply to the entire movie, initially set to that of the recorded movie.
--
-- Pass this to the rendering session in the rendering frame attributes to match the selected aperture. Change this property when the user selects a different aperture for the edited movie. Changes to this property are reflected in the script changes for later restoration.
--
-- ObjC selector: @- fNumber@
fNumber :: IsCNScript cnScript => cnScript -> IO CFloat
fNumber cnScript =
  sendMessage cnScript fNumberSelector

-- | The f/number to apply to the entire movie, initially set to that of the recorded movie.
--
-- Pass this to the rendering session in the rendering frame attributes to match the selected aperture. Change this property when the user selects a different aperture for the edited movie. Changes to this property are reflected in the script changes for later restoration.
--
-- ObjC selector: @- setFNumber:@
setFNumber :: IsCNScript cnScript => cnScript -> CFloat -> IO ()
setFNumber cnScript value =
  sendMessage cnScript setFNumberSelector value

-- | All detection tracks that have been added since recording.
--
-- ObjC selector: @- addedDetectionTracks@
addedDetectionTracks :: IsCNScript cnScript => cnScript -> IO (Id NSArray)
addedDetectionTracks cnScript =
  sendMessage cnScript addedDetectionTracksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromAsset:changes:progress:completionHandler:@
loadFromAsset_changes_progress_completionHandlerSelector :: Selector '[Id AVAsset, Id CNScriptChanges, Id NSProgress, Ptr ()] ()
loadFromAsset_changes_progress_completionHandlerSelector = mkSelector "loadFromAsset:changes:progress:completionHandler:"

-- | @Selector@ for @reloadWithChanges:@
reloadWithChangesSelector :: Selector '[Id CNScriptChanges] ()
reloadWithChangesSelector = mkSelector "reloadWithChanges:"

-- | @Selector@ for @changes@
changesSelector :: Selector '[] (Id CNScriptChanges)
changesSelector = mkSelector "changes"

-- | @Selector@ for @detectionTrackForID:@
detectionTrackForIDSelector :: Selector '[CLong] (Id CNDetectionTrack)
detectionTrackForIDSelector = mkSelector "detectionTrackForID:"

-- | @Selector@ for @detectionTrackForDecision:@
detectionTrackForDecisionSelector :: Selector '[Id CNDecision] (Id CNDetectionTrack)
detectionTrackForDecisionSelector = mkSelector "detectionTrackForDecision:"

-- | @Selector@ for @addUserDecision:@
addUserDecisionSelector :: Selector '[Id CNDecision] Bool
addUserDecisionSelector = mkSelector "addUserDecision:"

-- | @Selector@ for @removeUserDecision:@
removeUserDecisionSelector :: Selector '[Id CNDecision] Bool
removeUserDecisionSelector = mkSelector "removeUserDecision:"

-- | @Selector@ for @removeAllUserDecisions@
removeAllUserDecisionsSelector :: Selector '[] ()
removeAllUserDecisionsSelector = mkSelector "removeAllUserDecisions"

-- | @Selector@ for @addDetectionTrack:@
addDetectionTrackSelector :: Selector '[Id CNDetectionTrack] CLong
addDetectionTrackSelector = mkSelector "addDetectionTrack:"

-- | @Selector@ for @removeDetectionTrack:@
removeDetectionTrackSelector :: Selector '[Id CNDetectionTrack] Bool
removeDetectionTrackSelector = mkSelector "removeDetectionTrack:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNScript)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNScript)
newSelector = mkSelector "new"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector '[] CFloat
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @setFNumber:@
setFNumberSelector :: Selector '[CFloat] ()
setFNumberSelector = mkSelector "setFNumber:"

-- | @Selector@ for @addedDetectionTracks@
addedDetectionTracksSelector :: Selector '[] (Id NSArray)
addedDetectionTracksSelector = mkSelector "addedDetectionTracks"

