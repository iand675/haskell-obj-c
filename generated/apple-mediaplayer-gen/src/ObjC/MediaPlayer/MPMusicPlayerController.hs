{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerController@.
module ObjC.MediaPlayer.MPMusicPlayerController
  ( MPMusicPlayerController
  , IsMPMusicPlayerController(..)
  , new
  , init_
  , setQueueWithQuery
  , setQueueWithItemCollection
  , setQueueWithStoreIDs
  , setQueueWithDescriptor
  , prependQueueDescriptor
  , appendQueueDescriptor
  , prepareToPlayWithCompletionHandler
  , skipToNextItem
  , skipToBeginning
  , skipToPreviousItem
  , beginGeneratingPlaybackNotifications
  , endGeneratingPlaybackNotifications
  , applicationMusicPlayer
  , applicationQueuePlayer
  , systemMusicPlayer
  , playbackState
  , repeatMode
  , setRepeatMode
  , shuffleMode
  , setShuffleMode
  , volume
  , setVolume
  , nowPlayingItem
  , setNowPlayingItem
  , indexOfNowPlayingItem
  , iPodMusicPlayer
  , appendQueueDescriptorSelector
  , applicationMusicPlayerSelector
  , applicationQueuePlayerSelector
  , beginGeneratingPlaybackNotificationsSelector
  , endGeneratingPlaybackNotificationsSelector
  , iPodMusicPlayerSelector
  , indexOfNowPlayingItemSelector
  , initSelector
  , newSelector
  , nowPlayingItemSelector
  , playbackStateSelector
  , prepareToPlayWithCompletionHandlerSelector
  , prependQueueDescriptorSelector
  , repeatModeSelector
  , setNowPlayingItemSelector
  , setQueueWithDescriptorSelector
  , setQueueWithItemCollectionSelector
  , setQueueWithQuerySelector
  , setQueueWithStoreIDsSelector
  , setRepeatModeSelector
  , setShuffleModeSelector
  , setVolumeSelector
  , shuffleModeSelector
  , skipToBeginningSelector
  , skipToNextItemSelector
  , skipToPreviousItemSelector
  , systemMusicPlayerSelector
  , volumeSelector

  -- * Enum types
  , MPMusicPlaybackState(MPMusicPlaybackState)
  , pattern MPMusicPlaybackStateStopped
  , pattern MPMusicPlaybackStatePlaying
  , pattern MPMusicPlaybackStatePaused
  , pattern MPMusicPlaybackStateInterrupted
  , pattern MPMusicPlaybackStateSeekingForward
  , pattern MPMusicPlaybackStateSeekingBackward
  , MPMusicRepeatMode(MPMusicRepeatMode)
  , pattern MPMusicRepeatModeDefault
  , pattern MPMusicRepeatModeNone
  , pattern MPMusicRepeatModeOne
  , pattern MPMusicRepeatModeAll
  , MPMusicShuffleMode(MPMusicShuffleMode)
  , pattern MPMusicShuffleModeDefault
  , pattern MPMusicShuffleModeOff
  , pattern MPMusicShuffleModeSongs
  , pattern MPMusicShuffleModeAlbums

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMusicPlayerController)
new  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO (Id MPMusicPlayerController)
init_ mpMusicPlayerController =
  sendOwnedMessage mpMusicPlayerController initSelector

-- | @- setQueueWithQuery:@
setQueueWithQuery :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMediaQuery query) => mpMusicPlayerController -> query -> IO ()
setQueueWithQuery mpMusicPlayerController query =
  sendMessage mpMusicPlayerController setQueueWithQuerySelector (toMPMediaQuery query)

-- | @- setQueueWithItemCollection:@
setQueueWithItemCollection :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMediaItemCollection itemCollection) => mpMusicPlayerController -> itemCollection -> IO ()
setQueueWithItemCollection mpMusicPlayerController itemCollection =
  sendMessage mpMusicPlayerController setQueueWithItemCollectionSelector (toMPMediaItemCollection itemCollection)

-- | @- setQueueWithStoreIDs:@
setQueueWithStoreIDs :: (IsMPMusicPlayerController mpMusicPlayerController, IsNSArray storeIDs) => mpMusicPlayerController -> storeIDs -> IO ()
setQueueWithStoreIDs mpMusicPlayerController storeIDs =
  sendMessage mpMusicPlayerController setQueueWithStoreIDsSelector (toNSArray storeIDs)

-- | @- setQueueWithDescriptor:@
setQueueWithDescriptor :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMusicPlayerQueueDescriptor descriptor) => mpMusicPlayerController -> descriptor -> IO ()
setQueueWithDescriptor mpMusicPlayerController descriptor =
  sendMessage mpMusicPlayerController setQueueWithDescriptorSelector (toMPMusicPlayerQueueDescriptor descriptor)

-- | @- prependQueueDescriptor:@
prependQueueDescriptor :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMusicPlayerQueueDescriptor descriptor) => mpMusicPlayerController -> descriptor -> IO ()
prependQueueDescriptor mpMusicPlayerController descriptor =
  sendMessage mpMusicPlayerController prependQueueDescriptorSelector (toMPMusicPlayerQueueDescriptor descriptor)

-- | @- appendQueueDescriptor:@
appendQueueDescriptor :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMusicPlayerQueueDescriptor descriptor) => mpMusicPlayerController -> descriptor -> IO ()
appendQueueDescriptor mpMusicPlayerController descriptor =
  sendMessage mpMusicPlayerController appendQueueDescriptorSelector (toMPMusicPlayerQueueDescriptor descriptor)

-- | @- prepareToPlayWithCompletionHandler:@
prepareToPlayWithCompletionHandler :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> Ptr () -> IO ()
prepareToPlayWithCompletionHandler mpMusicPlayerController completionHandler =
  sendMessage mpMusicPlayerController prepareToPlayWithCompletionHandlerSelector completionHandler

-- | @- skipToNextItem@
skipToNextItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
skipToNextItem mpMusicPlayerController =
  sendMessage mpMusicPlayerController skipToNextItemSelector

-- | @- skipToBeginning@
skipToBeginning :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
skipToBeginning mpMusicPlayerController =
  sendMessage mpMusicPlayerController skipToBeginningSelector

-- | @- skipToPreviousItem@
skipToPreviousItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
skipToPreviousItem mpMusicPlayerController =
  sendMessage mpMusicPlayerController skipToPreviousItemSelector

-- | @- beginGeneratingPlaybackNotifications@
beginGeneratingPlaybackNotifications :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
beginGeneratingPlaybackNotifications mpMusicPlayerController =
  sendMessage mpMusicPlayerController beginGeneratingPlaybackNotificationsSelector

-- | @- endGeneratingPlaybackNotifications@
endGeneratingPlaybackNotifications :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
endGeneratingPlaybackNotifications mpMusicPlayerController =
  sendMessage mpMusicPlayerController endGeneratingPlaybackNotificationsSelector

-- | Playing items with applicationMusicPlayer does not affect Music's playback state.
--
-- ObjC selector: @+ applicationMusicPlayer@
applicationMusicPlayer :: IO (Id MPMusicPlayerController)
applicationMusicPlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMessage cls' applicationMusicPlayerSelector

-- | Similar to applicationMusicPlayer, but allows direct manipulation of the queue.
--
-- ObjC selector: @+ applicationQueuePlayer@
applicationQueuePlayer :: IO (Id MPMusicPlayerApplicationController)
applicationQueuePlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMessage cls' applicationQueuePlayerSelector

-- | Playing media items with the systemMusicPlayer will replace the user's current Music state.
--
-- ObjC selector: @+ systemMusicPlayer@
systemMusicPlayer :: IO (Id MPMusicPlayerController)
systemMusicPlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMessage cls' systemMusicPlayerSelector

-- | @- playbackState@
playbackState :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO MPMusicPlaybackState
playbackState mpMusicPlayerController =
  sendMessage mpMusicPlayerController playbackStateSelector

-- | @- repeatMode@
repeatMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO MPMusicRepeatMode
repeatMode mpMusicPlayerController =
  sendMessage mpMusicPlayerController repeatModeSelector

-- | @- setRepeatMode:@
setRepeatMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> MPMusicRepeatMode -> IO ()
setRepeatMode mpMusicPlayerController value =
  sendMessage mpMusicPlayerController setRepeatModeSelector value

-- | @- shuffleMode@
shuffleMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO MPMusicShuffleMode
shuffleMode mpMusicPlayerController =
  sendMessage mpMusicPlayerController shuffleModeSelector

-- | @- setShuffleMode:@
setShuffleMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> MPMusicShuffleMode -> IO ()
setShuffleMode mpMusicPlayerController value =
  sendMessage mpMusicPlayerController setShuffleModeSelector value

-- | @- volume@
volume :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO CFloat
volume mpMusicPlayerController =
  sendMessage mpMusicPlayerController volumeSelector

-- | @- setVolume:@
setVolume :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> CFloat -> IO ()
setVolume mpMusicPlayerController value =
  sendMessage mpMusicPlayerController setVolumeSelector value

-- | @- nowPlayingItem@
nowPlayingItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO (Id MPMediaItem)
nowPlayingItem mpMusicPlayerController =
  sendMessage mpMusicPlayerController nowPlayingItemSelector

-- | @- setNowPlayingItem:@
setNowPlayingItem :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMediaItem value) => mpMusicPlayerController -> value -> IO ()
setNowPlayingItem mpMusicPlayerController value =
  sendMessage mpMusicPlayerController setNowPlayingItemSelector (toMPMediaItem value)

-- | @- indexOfNowPlayingItem@
indexOfNowPlayingItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO CULong
indexOfNowPlayingItem mpMusicPlayerController =
  sendMessage mpMusicPlayerController indexOfNowPlayingItemSelector

-- | @+ iPodMusicPlayer@
iPodMusicPlayer :: IO RawId
iPodMusicPlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMessage cls' iPodMusicPlayerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPMusicPlayerController)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPMusicPlayerController)
initSelector = mkSelector "init"

-- | @Selector@ for @setQueueWithQuery:@
setQueueWithQuerySelector :: Selector '[Id MPMediaQuery] ()
setQueueWithQuerySelector = mkSelector "setQueueWithQuery:"

-- | @Selector@ for @setQueueWithItemCollection:@
setQueueWithItemCollectionSelector :: Selector '[Id MPMediaItemCollection] ()
setQueueWithItemCollectionSelector = mkSelector "setQueueWithItemCollection:"

-- | @Selector@ for @setQueueWithStoreIDs:@
setQueueWithStoreIDsSelector :: Selector '[Id NSArray] ()
setQueueWithStoreIDsSelector = mkSelector "setQueueWithStoreIDs:"

-- | @Selector@ for @setQueueWithDescriptor:@
setQueueWithDescriptorSelector :: Selector '[Id MPMusicPlayerQueueDescriptor] ()
setQueueWithDescriptorSelector = mkSelector "setQueueWithDescriptor:"

-- | @Selector@ for @prependQueueDescriptor:@
prependQueueDescriptorSelector :: Selector '[Id MPMusicPlayerQueueDescriptor] ()
prependQueueDescriptorSelector = mkSelector "prependQueueDescriptor:"

-- | @Selector@ for @appendQueueDescriptor:@
appendQueueDescriptorSelector :: Selector '[Id MPMusicPlayerQueueDescriptor] ()
appendQueueDescriptorSelector = mkSelector "appendQueueDescriptor:"

-- | @Selector@ for @prepareToPlayWithCompletionHandler:@
prepareToPlayWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
prepareToPlayWithCompletionHandlerSelector = mkSelector "prepareToPlayWithCompletionHandler:"

-- | @Selector@ for @skipToNextItem@
skipToNextItemSelector :: Selector '[] ()
skipToNextItemSelector = mkSelector "skipToNextItem"

-- | @Selector@ for @skipToBeginning@
skipToBeginningSelector :: Selector '[] ()
skipToBeginningSelector = mkSelector "skipToBeginning"

-- | @Selector@ for @skipToPreviousItem@
skipToPreviousItemSelector :: Selector '[] ()
skipToPreviousItemSelector = mkSelector "skipToPreviousItem"

-- | @Selector@ for @beginGeneratingPlaybackNotifications@
beginGeneratingPlaybackNotificationsSelector :: Selector '[] ()
beginGeneratingPlaybackNotificationsSelector = mkSelector "beginGeneratingPlaybackNotifications"

-- | @Selector@ for @endGeneratingPlaybackNotifications@
endGeneratingPlaybackNotificationsSelector :: Selector '[] ()
endGeneratingPlaybackNotificationsSelector = mkSelector "endGeneratingPlaybackNotifications"

-- | @Selector@ for @applicationMusicPlayer@
applicationMusicPlayerSelector :: Selector '[] (Id MPMusicPlayerController)
applicationMusicPlayerSelector = mkSelector "applicationMusicPlayer"

-- | @Selector@ for @applicationQueuePlayer@
applicationQueuePlayerSelector :: Selector '[] (Id MPMusicPlayerApplicationController)
applicationQueuePlayerSelector = mkSelector "applicationQueuePlayer"

-- | @Selector@ for @systemMusicPlayer@
systemMusicPlayerSelector :: Selector '[] (Id MPMusicPlayerController)
systemMusicPlayerSelector = mkSelector "systemMusicPlayer"

-- | @Selector@ for @playbackState@
playbackStateSelector :: Selector '[] MPMusicPlaybackState
playbackStateSelector = mkSelector "playbackState"

-- | @Selector@ for @repeatMode@
repeatModeSelector :: Selector '[] MPMusicRepeatMode
repeatModeSelector = mkSelector "repeatMode"

-- | @Selector@ for @setRepeatMode:@
setRepeatModeSelector :: Selector '[MPMusicRepeatMode] ()
setRepeatModeSelector = mkSelector "setRepeatMode:"

-- | @Selector@ for @shuffleMode@
shuffleModeSelector :: Selector '[] MPMusicShuffleMode
shuffleModeSelector = mkSelector "shuffleMode"

-- | @Selector@ for @setShuffleMode:@
setShuffleModeSelector :: Selector '[MPMusicShuffleMode] ()
setShuffleModeSelector = mkSelector "setShuffleMode:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @nowPlayingItem@
nowPlayingItemSelector :: Selector '[] (Id MPMediaItem)
nowPlayingItemSelector = mkSelector "nowPlayingItem"

-- | @Selector@ for @setNowPlayingItem:@
setNowPlayingItemSelector :: Selector '[Id MPMediaItem] ()
setNowPlayingItemSelector = mkSelector "setNowPlayingItem:"

-- | @Selector@ for @indexOfNowPlayingItem@
indexOfNowPlayingItemSelector :: Selector '[] CULong
indexOfNowPlayingItemSelector = mkSelector "indexOfNowPlayingItem"

-- | @Selector@ for @iPodMusicPlayer@
iPodMusicPlayerSelector :: Selector '[] RawId
iPodMusicPlayerSelector = mkSelector "iPodMusicPlayer"

