{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlayMediaIntent@.
module ObjC.Intents.INPlayMediaIntent
  ( INPlayMediaIntent
  , IsINPlayMediaIntent(..)
  , initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearch
  , initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback
  , mediaItems
  , mediaContainer
  , playShuffled
  , playbackRepeatMode
  , resumePlayback
  , playbackQueueLocation
  , playbackSpeed
  , mediaSearch
  , initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector
  , initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector
  , mediaContainerSelector
  , mediaItemsSelector
  , mediaSearchSelector
  , playShuffledSelector
  , playbackQueueLocationSelector
  , playbackRepeatModeSelector
  , playbackSpeedSelector
  , resumePlaybackSelector

  -- * Enum types
  , INPlaybackQueueLocation(INPlaybackQueueLocation)
  , pattern INPlaybackQueueLocationUnknown
  , pattern INPlaybackQueueLocationNow
  , pattern INPlaybackQueueLocationNext
  , pattern INPlaybackQueueLocationLater
  , INPlaybackRepeatMode(INPlaybackRepeatMode)
  , pattern INPlaybackRepeatModeUnknown
  , pattern INPlaybackRepeatModeNone
  , pattern INPlaybackRepeatModeAll
  , pattern INPlaybackRepeatModeOne

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearch :: (IsINPlayMediaIntent inPlayMediaIntent, IsNSArray mediaItems, IsINMediaItem mediaContainer, IsNSNumber playShuffled, IsNSNumber resumePlayback, IsNSNumber playbackSpeed, IsINMediaSearch mediaSearch) => inPlayMediaIntent -> mediaItems -> mediaContainer -> playShuffled -> INPlaybackRepeatMode -> resumePlayback -> INPlaybackQueueLocation -> playbackSpeed -> mediaSearch -> IO (Id INPlayMediaIntent)
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearch inPlayMediaIntent mediaItems mediaContainer playShuffled playbackRepeatMode resumePlayback playbackQueueLocation playbackSpeed mediaSearch =
  sendOwnedMessage inPlayMediaIntent initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector (toNSArray mediaItems) (toINMediaItem mediaContainer) (toNSNumber playShuffled) playbackRepeatMode (toNSNumber resumePlayback) playbackQueueLocation (toNSNumber playbackSpeed) (toINMediaSearch mediaSearch)

-- | @- initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback :: (IsINPlayMediaIntent inPlayMediaIntent, IsNSArray mediaItems, IsINMediaItem mediaContainer, IsNSNumber playShuffled, IsNSNumber resumePlayback) => inPlayMediaIntent -> mediaItems -> mediaContainer -> playShuffled -> INPlaybackRepeatMode -> resumePlayback -> IO (Id INPlayMediaIntent)
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback inPlayMediaIntent mediaItems mediaContainer playShuffled playbackRepeatMode resumePlayback =
  sendOwnedMessage inPlayMediaIntent initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector (toNSArray mediaItems) (toINMediaItem mediaContainer) (toNSNumber playShuffled) playbackRepeatMode (toNSNumber resumePlayback)

-- | @- mediaItems@
mediaItems :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id NSArray)
mediaItems inPlayMediaIntent =
  sendMessage inPlayMediaIntent mediaItemsSelector

-- | @- mediaContainer@
mediaContainer :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id INMediaItem)
mediaContainer inPlayMediaIntent =
  sendMessage inPlayMediaIntent mediaContainerSelector

-- | @- playShuffled@
playShuffled :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id NSNumber)
playShuffled inPlayMediaIntent =
  sendMessage inPlayMediaIntent playShuffledSelector

-- | @- playbackRepeatMode@
playbackRepeatMode :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO INPlaybackRepeatMode
playbackRepeatMode inPlayMediaIntent =
  sendMessage inPlayMediaIntent playbackRepeatModeSelector

-- | @- resumePlayback@
resumePlayback :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id NSNumber)
resumePlayback inPlayMediaIntent =
  sendMessage inPlayMediaIntent resumePlaybackSelector

-- | @- playbackQueueLocation@
playbackQueueLocation :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO INPlaybackQueueLocation
playbackQueueLocation inPlayMediaIntent =
  sendMessage inPlayMediaIntent playbackQueueLocationSelector

-- | @- playbackSpeed@
playbackSpeed :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id NSNumber)
playbackSpeed inPlayMediaIntent =
  sendMessage inPlayMediaIntent playbackSpeedSelector

-- | @- mediaSearch@
mediaSearch :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id INMediaSearch)
mediaSearch inPlayMediaIntent =
  sendMessage inPlayMediaIntent mediaSearchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector :: Selector '[Id NSArray, Id INMediaItem, Id NSNumber, INPlaybackRepeatMode, Id NSNumber, INPlaybackQueueLocation, Id NSNumber, Id INMediaSearch] (Id INPlayMediaIntent)
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector = mkSelector "initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:"

-- | @Selector@ for @initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector :: Selector '[Id NSArray, Id INMediaItem, Id NSNumber, INPlaybackRepeatMode, Id NSNumber] (Id INPlayMediaIntent)
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector = mkSelector "initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector '[] (Id NSArray)
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaContainer@
mediaContainerSelector :: Selector '[] (Id INMediaItem)
mediaContainerSelector = mkSelector "mediaContainer"

-- | @Selector@ for @playShuffled@
playShuffledSelector :: Selector '[] (Id NSNumber)
playShuffledSelector = mkSelector "playShuffled"

-- | @Selector@ for @playbackRepeatMode@
playbackRepeatModeSelector :: Selector '[] INPlaybackRepeatMode
playbackRepeatModeSelector = mkSelector "playbackRepeatMode"

-- | @Selector@ for @resumePlayback@
resumePlaybackSelector :: Selector '[] (Id NSNumber)
resumePlaybackSelector = mkSelector "resumePlayback"

-- | @Selector@ for @playbackQueueLocation@
playbackQueueLocationSelector :: Selector '[] INPlaybackQueueLocation
playbackQueueLocationSelector = mkSelector "playbackQueueLocation"

-- | @Selector@ for @playbackSpeed@
playbackSpeedSelector :: Selector '[] (Id NSNumber)
playbackSpeedSelector = mkSelector "playbackSpeed"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector '[] (Id INMediaSearch)
mediaSearchSelector = mkSelector "mediaSearch"

