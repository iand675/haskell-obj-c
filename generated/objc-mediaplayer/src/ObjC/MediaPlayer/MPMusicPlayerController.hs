{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , setQueueWithQuerySelector
  , setQueueWithItemCollectionSelector
  , setQueueWithStoreIDsSelector
  , setQueueWithDescriptorSelector
  , prependQueueDescriptorSelector
  , appendQueueDescriptorSelector
  , prepareToPlayWithCompletionHandlerSelector
  , skipToNextItemSelector
  , skipToBeginningSelector
  , skipToPreviousItemSelector
  , beginGeneratingPlaybackNotificationsSelector
  , endGeneratingPlaybackNotificationsSelector
  , applicationMusicPlayerSelector
  , applicationQueuePlayerSelector
  , systemMusicPlayerSelector
  , playbackStateSelector
  , repeatModeSelector
  , setRepeatModeSelector
  , shuffleModeSelector
  , setShuffleModeSelector
  , volumeSelector
  , setVolumeSelector
  , nowPlayingItemSelector
  , setNowPlayingItemSelector
  , indexOfNowPlayingItemSelector

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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMusicPlayerController)
new  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO (Id MPMusicPlayerController)
init_ mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setQueueWithQuery:@
setQueueWithQuery :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMediaQuery query) => mpMusicPlayerController -> query -> IO ()
setQueueWithQuery mpMusicPlayerController  query =
withObjCPtr query $ \raw_query ->
    sendMsg mpMusicPlayerController (mkSelector "setQueueWithQuery:") retVoid [argPtr (castPtr raw_query :: Ptr ())]

-- | @- setQueueWithItemCollection:@
setQueueWithItemCollection :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMediaItemCollection itemCollection) => mpMusicPlayerController -> itemCollection -> IO ()
setQueueWithItemCollection mpMusicPlayerController  itemCollection =
withObjCPtr itemCollection $ \raw_itemCollection ->
    sendMsg mpMusicPlayerController (mkSelector "setQueueWithItemCollection:") retVoid [argPtr (castPtr raw_itemCollection :: Ptr ())]

-- | @- setQueueWithStoreIDs:@
setQueueWithStoreIDs :: (IsMPMusicPlayerController mpMusicPlayerController, IsNSArray storeIDs) => mpMusicPlayerController -> storeIDs -> IO ()
setQueueWithStoreIDs mpMusicPlayerController  storeIDs =
withObjCPtr storeIDs $ \raw_storeIDs ->
    sendMsg mpMusicPlayerController (mkSelector "setQueueWithStoreIDs:") retVoid [argPtr (castPtr raw_storeIDs :: Ptr ())]

-- | @- setQueueWithDescriptor:@
setQueueWithDescriptor :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMusicPlayerQueueDescriptor descriptor) => mpMusicPlayerController -> descriptor -> IO ()
setQueueWithDescriptor mpMusicPlayerController  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpMusicPlayerController (mkSelector "setQueueWithDescriptor:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ())]

-- | @- prependQueueDescriptor:@
prependQueueDescriptor :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMusicPlayerQueueDescriptor descriptor) => mpMusicPlayerController -> descriptor -> IO ()
prependQueueDescriptor mpMusicPlayerController  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpMusicPlayerController (mkSelector "prependQueueDescriptor:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ())]

-- | @- appendQueueDescriptor:@
appendQueueDescriptor :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMusicPlayerQueueDescriptor descriptor) => mpMusicPlayerController -> descriptor -> IO ()
appendQueueDescriptor mpMusicPlayerController  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpMusicPlayerController (mkSelector "appendQueueDescriptor:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ())]

-- | @- prepareToPlayWithCompletionHandler:@
prepareToPlayWithCompletionHandler :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> Ptr () -> IO ()
prepareToPlayWithCompletionHandler mpMusicPlayerController  completionHandler =
  sendMsg mpMusicPlayerController (mkSelector "prepareToPlayWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipToNextItem@
skipToNextItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
skipToNextItem mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "skipToNextItem") retVoid []

-- | @- skipToBeginning@
skipToBeginning :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
skipToBeginning mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "skipToBeginning") retVoid []

-- | @- skipToPreviousItem@
skipToPreviousItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
skipToPreviousItem mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "skipToPreviousItem") retVoid []

-- | @- beginGeneratingPlaybackNotifications@
beginGeneratingPlaybackNotifications :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
beginGeneratingPlaybackNotifications mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "beginGeneratingPlaybackNotifications") retVoid []

-- | @- endGeneratingPlaybackNotifications@
endGeneratingPlaybackNotifications :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO ()
endGeneratingPlaybackNotifications mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "endGeneratingPlaybackNotifications") retVoid []

-- | Playing items with applicationMusicPlayer does not affect Music's playback state.
--
-- ObjC selector: @+ applicationMusicPlayer@
applicationMusicPlayer :: IO (Id MPMusicPlayerController)
applicationMusicPlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMsg cls' (mkSelector "applicationMusicPlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Similar to applicationMusicPlayer, but allows direct manipulation of the queue.
--
-- ObjC selector: @+ applicationQueuePlayer@
applicationQueuePlayer :: IO (Id MPMusicPlayerApplicationController)
applicationQueuePlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMsg cls' (mkSelector "applicationQueuePlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Playing media items with the systemMusicPlayer will replace the user's current Music state.
--
-- ObjC selector: @+ systemMusicPlayer@
systemMusicPlayer :: IO (Id MPMusicPlayerController)
systemMusicPlayer  =
  do
    cls' <- getRequiredClass "MPMusicPlayerController"
    sendClassMsg cls' (mkSelector "systemMusicPlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playbackState@
playbackState :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO MPMusicPlaybackState
playbackState mpMusicPlayerController  =
  fmap (coerce :: CLong -> MPMusicPlaybackState) $ sendMsg mpMusicPlayerController (mkSelector "playbackState") retCLong []

-- | @- repeatMode@
repeatMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO MPMusicRepeatMode
repeatMode mpMusicPlayerController  =
  fmap (coerce :: CLong -> MPMusicRepeatMode) $ sendMsg mpMusicPlayerController (mkSelector "repeatMode") retCLong []

-- | @- setRepeatMode:@
setRepeatMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> MPMusicRepeatMode -> IO ()
setRepeatMode mpMusicPlayerController  value =
  sendMsg mpMusicPlayerController (mkSelector "setRepeatMode:") retVoid [argCLong (coerce value)]

-- | @- shuffleMode@
shuffleMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO MPMusicShuffleMode
shuffleMode mpMusicPlayerController  =
  fmap (coerce :: CLong -> MPMusicShuffleMode) $ sendMsg mpMusicPlayerController (mkSelector "shuffleMode") retCLong []

-- | @- setShuffleMode:@
setShuffleMode :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> MPMusicShuffleMode -> IO ()
setShuffleMode mpMusicPlayerController  value =
  sendMsg mpMusicPlayerController (mkSelector "setShuffleMode:") retVoid [argCLong (coerce value)]

-- | @- volume@
volume :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO CFloat
volume mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> CFloat -> IO ()
setVolume mpMusicPlayerController  value =
  sendMsg mpMusicPlayerController (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | @- nowPlayingItem@
nowPlayingItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO (Id MPMediaItem)
nowPlayingItem mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "nowPlayingItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNowPlayingItem:@
setNowPlayingItem :: (IsMPMusicPlayerController mpMusicPlayerController, IsMPMediaItem value) => mpMusicPlayerController -> value -> IO ()
setNowPlayingItem mpMusicPlayerController  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMusicPlayerController (mkSelector "setNowPlayingItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- indexOfNowPlayingItem@
indexOfNowPlayingItem :: IsMPMusicPlayerController mpMusicPlayerController => mpMusicPlayerController -> IO CULong
indexOfNowPlayingItem mpMusicPlayerController  =
  sendMsg mpMusicPlayerController (mkSelector "indexOfNowPlayingItem") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setQueueWithQuery:@
setQueueWithQuerySelector :: Selector
setQueueWithQuerySelector = mkSelector "setQueueWithQuery:"

-- | @Selector@ for @setQueueWithItemCollection:@
setQueueWithItemCollectionSelector :: Selector
setQueueWithItemCollectionSelector = mkSelector "setQueueWithItemCollection:"

-- | @Selector@ for @setQueueWithStoreIDs:@
setQueueWithStoreIDsSelector :: Selector
setQueueWithStoreIDsSelector = mkSelector "setQueueWithStoreIDs:"

-- | @Selector@ for @setQueueWithDescriptor:@
setQueueWithDescriptorSelector :: Selector
setQueueWithDescriptorSelector = mkSelector "setQueueWithDescriptor:"

-- | @Selector@ for @prependQueueDescriptor:@
prependQueueDescriptorSelector :: Selector
prependQueueDescriptorSelector = mkSelector "prependQueueDescriptor:"

-- | @Selector@ for @appendQueueDescriptor:@
appendQueueDescriptorSelector :: Selector
appendQueueDescriptorSelector = mkSelector "appendQueueDescriptor:"

-- | @Selector@ for @prepareToPlayWithCompletionHandler:@
prepareToPlayWithCompletionHandlerSelector :: Selector
prepareToPlayWithCompletionHandlerSelector = mkSelector "prepareToPlayWithCompletionHandler:"

-- | @Selector@ for @skipToNextItem@
skipToNextItemSelector :: Selector
skipToNextItemSelector = mkSelector "skipToNextItem"

-- | @Selector@ for @skipToBeginning@
skipToBeginningSelector :: Selector
skipToBeginningSelector = mkSelector "skipToBeginning"

-- | @Selector@ for @skipToPreviousItem@
skipToPreviousItemSelector :: Selector
skipToPreviousItemSelector = mkSelector "skipToPreviousItem"

-- | @Selector@ for @beginGeneratingPlaybackNotifications@
beginGeneratingPlaybackNotificationsSelector :: Selector
beginGeneratingPlaybackNotificationsSelector = mkSelector "beginGeneratingPlaybackNotifications"

-- | @Selector@ for @endGeneratingPlaybackNotifications@
endGeneratingPlaybackNotificationsSelector :: Selector
endGeneratingPlaybackNotificationsSelector = mkSelector "endGeneratingPlaybackNotifications"

-- | @Selector@ for @applicationMusicPlayer@
applicationMusicPlayerSelector :: Selector
applicationMusicPlayerSelector = mkSelector "applicationMusicPlayer"

-- | @Selector@ for @applicationQueuePlayer@
applicationQueuePlayerSelector :: Selector
applicationQueuePlayerSelector = mkSelector "applicationQueuePlayer"

-- | @Selector@ for @systemMusicPlayer@
systemMusicPlayerSelector :: Selector
systemMusicPlayerSelector = mkSelector "systemMusicPlayer"

-- | @Selector@ for @playbackState@
playbackStateSelector :: Selector
playbackStateSelector = mkSelector "playbackState"

-- | @Selector@ for @repeatMode@
repeatModeSelector :: Selector
repeatModeSelector = mkSelector "repeatMode"

-- | @Selector@ for @setRepeatMode:@
setRepeatModeSelector :: Selector
setRepeatModeSelector = mkSelector "setRepeatMode:"

-- | @Selector@ for @shuffleMode@
shuffleModeSelector :: Selector
shuffleModeSelector = mkSelector "shuffleMode"

-- | @Selector@ for @setShuffleMode:@
setShuffleModeSelector :: Selector
setShuffleModeSelector = mkSelector "setShuffleMode:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @nowPlayingItem@
nowPlayingItemSelector :: Selector
nowPlayingItemSelector = mkSelector "nowPlayingItem"

-- | @Selector@ for @setNowPlayingItem:@
setNowPlayingItemSelector :: Selector
setNowPlayingItemSelector = mkSelector "setNowPlayingItem:"

-- | @Selector@ for @indexOfNowPlayingItem@
indexOfNowPlayingItemSelector :: Selector
indexOfNowPlayingItemSelector = mkSelector "indexOfNowPlayingItem"

