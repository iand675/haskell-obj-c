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
  , initWithQuerySelector
  , initWithItemCollectionSelector
  , setStartTime_forItemSelector
  , setEndTime_forItemSelector
  , querySelector
  , itemCollectionSelector
  , startItemSelector
  , setStartItemSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithQuery:@
initWithQuery :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaQuery query) => mpMusicPlayerMediaItemQueueDescriptor -> query -> IO (Id MPMusicPlayerMediaItemQueueDescriptor)
initWithQuery mpMusicPlayerMediaItemQueueDescriptor  query =
withObjCPtr query $ \raw_query ->
    sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "initWithQuery:") (retPtr retVoid) [argPtr (castPtr raw_query :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemCollection:@
initWithItemCollection :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItemCollection itemCollection) => mpMusicPlayerMediaItemQueueDescriptor -> itemCollection -> IO (Id MPMusicPlayerMediaItemQueueDescriptor)
initWithItemCollection mpMusicPlayerMediaItemQueueDescriptor  itemCollection =
withObjCPtr itemCollection $ \raw_itemCollection ->
    sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "initWithItemCollection:") (retPtr retVoid) [argPtr (castPtr raw_itemCollection :: Ptr ())] >>= ownedObject . castPtr

-- | @- setStartTime:forItem:@
setStartTime_forItem :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItem mediaItem) => mpMusicPlayerMediaItemQueueDescriptor -> CDouble -> mediaItem -> IO ()
setStartTime_forItem mpMusicPlayerMediaItemQueueDescriptor  startTime mediaItem =
withObjCPtr mediaItem $ \raw_mediaItem ->
    sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "setStartTime:forItem:") retVoid [argCDouble (fromIntegral startTime), argPtr (castPtr raw_mediaItem :: Ptr ())]

-- | @- setEndTime:forItem:@
setEndTime_forItem :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItem mediaItem) => mpMusicPlayerMediaItemQueueDescriptor -> CDouble -> mediaItem -> IO ()
setEndTime_forItem mpMusicPlayerMediaItemQueueDescriptor  endTime mediaItem =
withObjCPtr mediaItem $ \raw_mediaItem ->
    sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "setEndTime:forItem:") retVoid [argCDouble (fromIntegral endTime), argPtr (castPtr raw_mediaItem :: Ptr ())]

-- | @- query@
query :: IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor => mpMusicPlayerMediaItemQueueDescriptor -> IO (Id MPMediaQuery)
query mpMusicPlayerMediaItemQueueDescriptor  =
  sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "query") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- itemCollection@
itemCollection :: IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor => mpMusicPlayerMediaItemQueueDescriptor -> IO (Id MPMediaItemCollection)
itemCollection mpMusicPlayerMediaItemQueueDescriptor  =
  sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "itemCollection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startItem@
startItem :: IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor => mpMusicPlayerMediaItemQueueDescriptor -> IO (Id MPMediaItem)
startItem mpMusicPlayerMediaItemQueueDescriptor  =
  sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "startItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartItem:@
setStartItem :: (IsMPMusicPlayerMediaItemQueueDescriptor mpMusicPlayerMediaItemQueueDescriptor, IsMPMediaItem value) => mpMusicPlayerMediaItemQueueDescriptor -> value -> IO ()
setStartItem mpMusicPlayerMediaItemQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMusicPlayerMediaItemQueueDescriptor (mkSelector "setStartItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuery:@
initWithQuerySelector :: Selector
initWithQuerySelector = mkSelector "initWithQuery:"

-- | @Selector@ for @initWithItemCollection:@
initWithItemCollectionSelector :: Selector
initWithItemCollectionSelector = mkSelector "initWithItemCollection:"

-- | @Selector@ for @setStartTime:forItem:@
setStartTime_forItemSelector :: Selector
setStartTime_forItemSelector = mkSelector "setStartTime:forItem:"

-- | @Selector@ for @setEndTime:forItem:@
setEndTime_forItemSelector :: Selector
setEndTime_forItemSelector = mkSelector "setEndTime:forItem:"

-- | @Selector@ for @query@
querySelector :: Selector
querySelector = mkSelector "query"

-- | @Selector@ for @itemCollection@
itemCollectionSelector :: Selector
itemCollectionSelector = mkSelector "itemCollection"

-- | @Selector@ for @startItem@
startItemSelector :: Selector
startItemSelector = mkSelector "startItem"

-- | @Selector@ for @setStartItem:@
setStartItemSelector :: Selector
setStartItemSelector = mkSelector "setStartItem:"

