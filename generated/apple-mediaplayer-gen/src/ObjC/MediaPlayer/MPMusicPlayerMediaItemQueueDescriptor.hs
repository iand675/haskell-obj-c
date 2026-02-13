{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerMediaItemQueueDescriptor@.
module ObjC.MediaPlayer.MPMusicPlayerMediaItemQueueDescriptor
  ( MPMusicPlayerMediaItemQueueDescriptor
  , IsMPMusicPlayerMediaItemQueueDescriptor(..)
  , initWithQuery
  , initWithItemCollection
  , setStartTime_forItem
  , setEndTime_forItem
  , query
  , itemCollection
  , startItem
  , setStartItem
  , initWithItemCollectionSelector
  , initWithQuerySelector
  , itemCollectionSelector
  , querySelector
  , setEndTime_forItemSelector
  , setStartItemSelector
  , setStartTime_forItemSelector
  , startItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithQuery:@
initWithQuery :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaQuery query) => mpMusicPlayerMediaItemQueueDescriptor -> query -> IO (Id MPMusicPlayerMediaItemQueueDescriptor)
initWithQuery mpMusicPlayerMediaItemQueueDescriptor query =
  sendOwnedMessage mpMusicPlayerMediaItemQueueDescriptor initWithQuerySelector (toMPMediaQuery query)

-- | @- initWithItemCollection:@
initWithItemCollection :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItemCollection itemCollection) => mpMusicPlayerMediaItemQueueDescriptor -> itemCollection -> IO (Id MPMusicPlayerMediaItemQueueDescriptor)
initWithItemCollection mpMusicPlayerMediaItemQueueDescriptor itemCollection =
  sendOwnedMessage mpMusicPlayerMediaItemQueueDescriptor initWithItemCollectionSelector (toMPMediaItemCollection itemCollection)

-- | @- setStartTime:forItem:@
setStartTime_forItem :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItem mediaItem) => mpMusicPlayerMediaItemQueueDescriptor -> CDouble -> mediaItem -> IO ()
setStartTime_forItem mpMusicPlayerMediaItemQueueDescriptor startTime mediaItem =
  sendMessage mpMusicPlayerMediaItemQueueDescriptor setStartTime_forItemSelector startTime (toMPMediaItem mediaItem)

-- | @- setEndTime:forItem:@
setEndTime_forItem :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItem mediaItem) => mpMusicPlayerMediaItemQueueDescriptor -> CDouble -> mediaItem -> IO ()
setEndTime_forItem mpMusicPlayerMediaItemQueueDescriptor endTime mediaItem =
  sendMessage mpMusicPlayerMediaItemQueueDescriptor setEndTime_forItemSelector endTime (toMPMediaItem mediaItem)

-- | @- query@
query :: IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor => mpMusicPlayerMediaItemQueueDescriptor -> IO (Id MPMediaQuery)
query mpMusicPlayerMediaItemQueueDescriptor =
  sendMessage mpMusicPlayerMediaItemQueueDescriptor querySelector

-- | @- itemCollection@
itemCollection :: IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor => mpMusicPlayerMediaItemQueueDescriptor -> IO (Id MPMediaItemCollection)
itemCollection mpMusicPlayerMediaItemQueueDescriptor =
  sendMessage mpMusicPlayerMediaItemQueueDescriptor itemCollectionSelector

-- | @- startItem@
startItem :: IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor => mpMusicPlayerMediaItemQueueDescriptor -> IO (Id MPMediaItem)
startItem mpMusicPlayerMediaItemQueueDescriptor =
  sendMessage mpMusicPlayerMediaItemQueueDescriptor startItemSelector

-- | @- setStartItem:@
setStartItem :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItem value) => mpMusicPlayerMediaItemQueueDescriptor -> value -> IO ()
setStartItem mpMusicPlayerMediaItemQueueDescriptor value =
  sendMessage mpMusicPlayerMediaItemQueueDescriptor setStartItemSelector (toMPMediaItem value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuery:@
initWithQuerySelector :: Selector '[Id MPMediaQuery] (Id MPMusicPlayerMediaItemQueueDescriptor)
initWithQuerySelector = mkSelector "initWithQuery:"

-- | @Selector@ for @initWithItemCollection:@
initWithItemCollectionSelector :: Selector '[Id MPMediaItemCollection] (Id MPMusicPlayerMediaItemQueueDescriptor)
initWithItemCollectionSelector = mkSelector "initWithItemCollection:"

-- | @Selector@ for @setStartTime:forItem:@
setStartTime_forItemSelector :: Selector '[CDouble, Id MPMediaItem] ()
setStartTime_forItemSelector = mkSelector "setStartTime:forItem:"

-- | @Selector@ for @setEndTime:forItem:@
setEndTime_forItemSelector :: Selector '[CDouble, Id MPMediaItem] ()
setEndTime_forItemSelector = mkSelector "setEndTime:forItem:"

-- | @Selector@ for @query@
querySelector :: Selector '[] (Id MPMediaQuery)
querySelector = mkSelector "query"

-- | @Selector@ for @itemCollection@
itemCollectionSelector :: Selector '[] (Id MPMediaItemCollection)
itemCollectionSelector = mkSelector "itemCollection"

-- | @Selector@ for @startItem@
startItemSelector :: Selector '[] (Id MPMediaItem)
startItemSelector = mkSelector "startItem"

-- | @Selector@ for @setStartItem:@
setStartItemSelector :: Selector '[Id MPMediaItem] ()
setStartItemSelector = mkSelector "setStartItem:"

