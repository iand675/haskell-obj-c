{-# LANGUAGE PatternSynonyms #-}
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
  , playbackRepeatMode
  , playbackQueueLocation
  , mediaSearch
  , initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector
  , initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector
  , mediaItemsSelector
  , mediaContainerSelector
  , playbackRepeatModeSelector
  , playbackQueueLocationSelector
  , mediaSearchSelector

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearch :: (IsINPlayMediaIntent inPlayMediaIntent, IsNSArray mediaItems, IsINMediaItem mediaContainer, IsNSNumber playShuffled, IsNSNumber resumePlayback, IsNSNumber playbackSpeed, IsINMediaSearch mediaSearch) => inPlayMediaIntent -> mediaItems -> mediaContainer -> playShuffled -> INPlaybackRepeatMode -> resumePlayback -> INPlaybackQueueLocation -> playbackSpeed -> mediaSearch -> IO (Id INPlayMediaIntent)
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearch inPlayMediaIntent  mediaItems mediaContainer playShuffled playbackRepeatMode resumePlayback playbackQueueLocation playbackSpeed mediaSearch =
withObjCPtr mediaItems $ \raw_mediaItems ->
  withObjCPtr mediaContainer $ \raw_mediaContainer ->
    withObjCPtr playShuffled $ \raw_playShuffled ->
      withObjCPtr resumePlayback $ \raw_resumePlayback ->
        withObjCPtr playbackSpeed $ \raw_playbackSpeed ->
          withObjCPtr mediaSearch $ \raw_mediaSearch ->
              sendMsg inPlayMediaIntent (mkSelector "initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:") (retPtr retVoid) [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr raw_mediaContainer :: Ptr ()), argPtr (castPtr raw_playShuffled :: Ptr ()), argCLong (coerce playbackRepeatMode), argPtr (castPtr raw_resumePlayback :: Ptr ()), argCLong (coerce playbackQueueLocation), argPtr (castPtr raw_playbackSpeed :: Ptr ()), argPtr (castPtr raw_mediaSearch :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback :: (IsINPlayMediaIntent inPlayMediaIntent, IsNSArray mediaItems, IsINMediaItem mediaContainer, IsNSNumber playShuffled, IsNSNumber resumePlayback) => inPlayMediaIntent -> mediaItems -> mediaContainer -> playShuffled -> INPlaybackRepeatMode -> resumePlayback -> IO (Id INPlayMediaIntent)
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback inPlayMediaIntent  mediaItems mediaContainer playShuffled playbackRepeatMode resumePlayback =
withObjCPtr mediaItems $ \raw_mediaItems ->
  withObjCPtr mediaContainer $ \raw_mediaContainer ->
    withObjCPtr playShuffled $ \raw_playShuffled ->
      withObjCPtr resumePlayback $ \raw_resumePlayback ->
          sendMsg inPlayMediaIntent (mkSelector "initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:") (retPtr retVoid) [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr raw_mediaContainer :: Ptr ()), argPtr (castPtr raw_playShuffled :: Ptr ()), argCLong (coerce playbackRepeatMode), argPtr (castPtr raw_resumePlayback :: Ptr ())] >>= ownedObject . castPtr

-- | @- mediaItems@
mediaItems :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id NSArray)
mediaItems inPlayMediaIntent  =
  sendMsg inPlayMediaIntent (mkSelector "mediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaContainer@
mediaContainer :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id INMediaItem)
mediaContainer inPlayMediaIntent  =
  sendMsg inPlayMediaIntent (mkSelector "mediaContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playbackRepeatMode@
playbackRepeatMode :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO INPlaybackRepeatMode
playbackRepeatMode inPlayMediaIntent  =
  fmap (coerce :: CLong -> INPlaybackRepeatMode) $ sendMsg inPlayMediaIntent (mkSelector "playbackRepeatMode") retCLong []

-- | @- playbackQueueLocation@
playbackQueueLocation :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO INPlaybackQueueLocation
playbackQueueLocation inPlayMediaIntent  =
  fmap (coerce :: CLong -> INPlaybackQueueLocation) $ sendMsg inPlayMediaIntent (mkSelector "playbackQueueLocation") retCLong []

-- | @- mediaSearch@
mediaSearch :: IsINPlayMediaIntent inPlayMediaIntent => inPlayMediaIntent -> IO (Id INMediaSearch)
mediaSearch inPlayMediaIntent  =
  sendMsg inPlayMediaIntent (mkSelector "mediaSearch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector :: Selector
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlayback_playbackQueueLocation_playbackSpeed_mediaSearchSelector = mkSelector "initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:playbackQueueLocation:playbackSpeed:mediaSearch:"

-- | @Selector@ for @initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:@
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector :: Selector
initWithMediaItems_mediaContainer_playShuffled_playbackRepeatMode_resumePlaybackSelector = mkSelector "initWithMediaItems:mediaContainer:playShuffled:playbackRepeatMode:resumePlayback:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaContainer@
mediaContainerSelector :: Selector
mediaContainerSelector = mkSelector "mediaContainer"

-- | @Selector@ for @playbackRepeatMode@
playbackRepeatModeSelector :: Selector
playbackRepeatModeSelector = mkSelector "playbackRepeatMode"

-- | @Selector@ for @playbackQueueLocation@
playbackQueueLocationSelector :: Selector
playbackQueueLocationSelector = mkSelector "playbackQueueLocation"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector
mediaSearchSelector = mkSelector "mediaSearch"

