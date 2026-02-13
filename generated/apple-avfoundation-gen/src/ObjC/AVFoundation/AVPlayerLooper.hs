{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVPlayerLooper@.
module ObjC.AVFoundation.AVPlayerLooper
  ( AVPlayerLooper
  , IsAVPlayerLooper(..)
  , init_
  , new
  , playerLooperWithPlayer_templateItem
  , disableLooping
  , status
  , error_
  , loopCount
  , loopingPlayerItems
  , disableLoopingSelector
  , errorSelector
  , initSelector
  , loopCountSelector
  , loopingPlayerItemsSelector
  , newSelector
  , playerLooperWithPlayer_templateItemSelector
  , statusSelector

  -- * Enum types
  , AVPlayerLooperItemOrdering(AVPlayerLooperItemOrdering)
  , pattern AVPlayerLooperItemOrderingLoopingItemsPrecedeExistingItems
  , pattern AVPlayerLooperItemOrderingLoopingItemsFollowExistingItems
  , AVPlayerLooperStatus(AVPlayerLooperStatus)
  , pattern AVPlayerLooperStatusUnknown
  , pattern AVPlayerLooperStatusReady
  , pattern AVPlayerLooperStatusFailed
  , pattern AVPlayerLooperStatusCancelled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerLooper avPlayerLooper => avPlayerLooper -> IO (Id AVPlayerLooper)
init_ avPlayerLooper =
  sendOwnedMessage avPlayerLooper initSelector

-- | @+ new@
new :: IO (Id AVPlayerLooper)
new  =
  do
    cls' <- getRequiredClass "AVPlayerLooper"
    sendOwnedClassMessage cls' newSelector

-- | playerLooperWithPlayer:templateItem:
--
-- Returns an instance of AVPlayerLooper to loop specified AVPlayerItem with specified AVQueuePlayer.
--
-- @player@ — Must not be nil
--
-- @itemToLoop@ — Must not be nil
--
-- Returns: An instance of AVPlayerLooper.
--
-- Equivalent to +playerLooperWithPlayer:templateItem:timeRange: and passing in kCMTimeRangeInvalid for timeRange parameter.
--
-- ObjC selector: @+ playerLooperWithPlayer:templateItem:@
playerLooperWithPlayer_templateItem :: (IsAVQueuePlayer player, IsAVPlayerItem itemToLoop) => player -> itemToLoop -> IO (Id AVPlayerLooper)
playerLooperWithPlayer_templateItem player itemToLoop =
  do
    cls' <- getRequiredClass "AVPlayerLooper"
    sendClassMessage cls' playerLooperWithPlayer_templateItemSelector (toAVQueuePlayer player) (toAVPlayerItem itemToLoop)

-- | disableLooping
--
-- Disables the item looping
--
-- AVPlayerLooper will stop performing player queue operations for looping and let the current looping item replica play to the end. The player's original actionAtItemEnd property will be restored afterwards. After this method is called, the value of the receiver's status property will be AVPlayerLooperStatusCancelled.
--
-- ObjC selector: @- disableLooping@
disableLooping :: IsAVPlayerLooper avPlayerLooper => avPlayerLooper -> IO ()
disableLooping avPlayerLooper =
  sendMessage avPlayerLooper disableLoopingSelector

-- | status
--
-- The ability of the receiver to be used for looping playback.
--
-- The value of this property is an AVPlayerLooperStatus that indicates whether the receiver is ready for looping playback. When the value of this property is AVPlayerStatusFailed, the receiver can no longer be used for playback and a new instance needs to be created in its place. When this happens, clients can check the value of the error property to determine the nature of the failure. This property is key value observable.
--
-- ObjC selector: @- status@
status :: IsAVPlayerLooper avPlayerLooper => avPlayerLooper -> IO AVPlayerLooperStatus
status avPlayerLooper =
  sendMessage avPlayerLooper statusSelector

-- | error
--
-- If the receiver's status is AVPlayerLooperStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is a NSError that describes what caused the receiver to not be able to perform looping playback. If the receiver's status is not AVPlayerLooperStatusFailed, the value of this property is nil.
--
-- ObjC selector: @- error@
error_ :: IsAVPlayerLooper avPlayerLooper => avPlayerLooper -> IO (Id NSError)
error_ avPlayerLooper =
  sendMessage avPlayerLooper errorSelector

-- | loopCount
--
-- Number of times the specified AVPlayerItem has been played
--
-- Starts at 0 and increments when the player starts playback of the AVPlayerItem again. This property is key value observable.
--
-- ObjC selector: @- loopCount@
loopCount :: IsAVPlayerLooper avPlayerLooper => avPlayerLooper -> IO CLong
loopCount avPlayerLooper =
  sendMessage avPlayerLooper loopCountSelector

-- | loopingPlayerItems
--
-- Returns an array containing replicas of specified AVPlayerItem used to accomplish the looping
--
-- AVPlayerLooper creates replicas of the template AVPlayerItem using -copyWithZone: and inserts the replicas in the specified AVQueuePlayer to accomplish the looping. The AVPlayerItem replicas are for informational purposes and to allow the client to apply properties that are not transferred from the template AVPlayerItem to the replicas. The client can determine the number of replicas created and can listen for notifications and property changes from the replicas if desired. AVPlayerItemOutputs and AVPlayerItemMediaDataCollectors are not transferred to the replicas so the client should add them to each replica if desired. The client shall not modify the properties on the replicas that would disrupt looping playback. Examples of such properties are playhead time/date, selected media option, and forward playback end time. This property is key value observable.
--
-- Returns: Array containing replicas of specified AVPlayerItem
--
-- ObjC selector: @- loopingPlayerItems@
loopingPlayerItems :: IsAVPlayerLooper avPlayerLooper => avPlayerLooper -> IO (Id NSArray)
loopingPlayerItems avPlayerLooper =
  sendMessage avPlayerLooper loopingPlayerItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerLooper)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerLooper)
newSelector = mkSelector "new"

-- | @Selector@ for @playerLooperWithPlayer:templateItem:@
playerLooperWithPlayer_templateItemSelector :: Selector '[Id AVQueuePlayer, Id AVPlayerItem] (Id AVPlayerLooper)
playerLooperWithPlayer_templateItemSelector = mkSelector "playerLooperWithPlayer:templateItem:"

-- | @Selector@ for @disableLooping@
disableLoopingSelector :: Selector '[] ()
disableLoopingSelector = mkSelector "disableLooping"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVPlayerLooperStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @loopCount@
loopCountSelector :: Selector '[] CLong
loopCountSelector = mkSelector "loopCount"

-- | @Selector@ for @loopingPlayerItems@
loopingPlayerItemsSelector :: Selector '[] (Id NSArray)
loopingPlayerItemsSelector = mkSelector "loopingPlayerItems"

