{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVQueuePlayer is a subclass of AVPlayer that offers an interface for multiple-item playback.
--
-- AVQueuePlayer extends AVPlayer with methods for managing a queue of items to be played in sequence. It plays these items as gaplessly as possible in the current runtime environment, depending on  the timely availability of media data for the enqueued items.
--
-- For best performance clients should typically enqueue only as many AVPlayerItems as are necessary to ensure smooth playback. Note that once an item is enqueued it becomes eligible to be loaded and made ready for playback, with whatever I/O and processing overhead that entails.
--
-- Generated bindings for @AVQueuePlayer@.
module ObjC.AVFoundation.AVQueuePlayer
  ( AVQueuePlayer
  , IsAVQueuePlayer(..)
  , queuePlayerWithItems
  , initWithItems
  , items
  , advanceToNextItem
  , canInsertItem_afterItem
  , insertItem_afterItem
  , removeItem
  , removeAllItems
  , queuePlayerWithItemsSelector
  , initWithItemsSelector
  , itemsSelector
  , advanceToNextItemSelector
  , canInsertItem_afterItemSelector
  , insertItem_afterItemSelector
  , removeItemSelector
  , removeAllItemsSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an instance of AVQueuePlayer and enqueues the AVPlayerItems from the specified array.
--
-- - Parameter items: An NSArray of AVPlayerItems with which to populate the player's queue initially.
--
-- - Returns: An instance of AVQueuePlayer.
--
-- ObjC selector: @+ queuePlayerWithItems:@
queuePlayerWithItems :: IsNSArray items => items -> IO (Id AVQueuePlayer)
queuePlayerWithItems items =
  do
    cls' <- getRequiredClass "AVQueuePlayer"
    withObjCPtr items $ \raw_items ->
      sendClassMsg cls' (mkSelector "queuePlayerWithItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an instance of AVQueuePlayer by enqueueing the AVPlayerItems from the specified array.
--
-- This method throws an exception if items contains duplicated values or values associated with another AVPlayer.
--
-- - Parameter items: An NSArray of AVPlayerItems with which to populate the player's queue initially.
--
-- - Returns: An instance of AVQueuePlayer.
--
-- ObjC selector: @- initWithItems:@
initWithItems :: (IsAVQueuePlayer avQueuePlayer, IsNSArray items) => avQueuePlayer -> items -> IO (Id AVQueuePlayer)
initWithItems avQueuePlayer  items =
withObjCPtr items $ \raw_items ->
    sendMsg avQueuePlayer (mkSelector "initWithItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= ownedObject . castPtr

-- | Provides an array of the currently enqueued items.
--
-- - Returns: An NSArray containing the enqueued AVPlayerItems.
--
-- ObjC selector: @- items@
items :: IsAVQueuePlayer avQueuePlayer => avQueuePlayer -> IO (Id NSArray)
items avQueuePlayer  =
  sendMsg avQueuePlayer (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Ends playback of the current item and initiates playback of the next item in the player's queue.
--
-- Removes the current item from the play queue.
--
-- ObjC selector: @- advanceToNextItem@
advanceToNextItem :: IsAVQueuePlayer avQueuePlayer => avQueuePlayer -> IO ()
advanceToNextItem avQueuePlayer  =
  sendMsg avQueuePlayer (mkSelector "advanceToNextItem") retVoid []

-- | Tests whether an AVPlayerItem can be inserted into the player's queue.
--
-- Note that adding the same AVPlayerItem to an AVQueuePlayer at more than one position in the queue is not supported.
--
-- - Parameter item: The AVPlayerItem to be tested. - Parameter afterItem: The item that the item to be tested is to follow in the queue. Pass nil to test whether the item can be appended to the queue.
--
-- - Returns: An indication of whether the item can be inserted into the queue after the specified item.
--
-- ObjC selector: @- canInsertItem:afterItem:@
canInsertItem_afterItem :: (IsAVQueuePlayer avQueuePlayer, IsAVPlayerItem item, IsAVPlayerItem afterItem) => avQueuePlayer -> item -> afterItem -> IO Bool
canInsertItem_afterItem avQueuePlayer  item afterItem =
withObjCPtr item $ \raw_item ->
  withObjCPtr afterItem $ \raw_afterItem ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avQueuePlayer (mkSelector "canInsertItem:afterItem:") retCULong [argPtr (castPtr raw_item :: Ptr ()), argPtr (castPtr raw_afterItem :: Ptr ())]

-- | Places an AVPlayerItem after the specified item in the queue.
--
-- This method throws an exception if item already exists in the queue.
--
-- - Parameter item: The item to be inserted. - Parameter afterItem: The item that the newly inserted item should follow in the queue. Pass nil to append the item to the queue.
--
-- ObjC selector: @- insertItem:afterItem:@
insertItem_afterItem :: (IsAVQueuePlayer avQueuePlayer, IsAVPlayerItem item, IsAVPlayerItem afterItem) => avQueuePlayer -> item -> afterItem -> IO ()
insertItem_afterItem avQueuePlayer  item afterItem =
withObjCPtr item $ \raw_item ->
  withObjCPtr afterItem $ \raw_afterItem ->
      sendMsg avQueuePlayer (mkSelector "insertItem:afterItem:") retVoid [argPtr (castPtr raw_item :: Ptr ()), argPtr (castPtr raw_afterItem :: Ptr ())]

-- | Removes an AVPlayerItem from the queue.
--
-- If the item to be removed is currently playing, has the same effect as -advanceToNextItem.
--
-- - Parameter item: The item to be removed.
--
-- ObjC selector: @- removeItem:@
removeItem :: (IsAVQueuePlayer avQueuePlayer, IsAVPlayerItem item) => avQueuePlayer -> item -> IO ()
removeItem avQueuePlayer  item =
withObjCPtr item $ \raw_item ->
    sendMsg avQueuePlayer (mkSelector "removeItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | Removes all items from the queue.
--
-- Stops playback by the target.
--
-- ObjC selector: @- removeAllItems@
removeAllItems :: IsAVQueuePlayer avQueuePlayer => avQueuePlayer -> IO ()
removeAllItems avQueuePlayer  =
  sendMsg avQueuePlayer (mkSelector "removeAllItems") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @queuePlayerWithItems:@
queuePlayerWithItemsSelector :: Selector
queuePlayerWithItemsSelector = mkSelector "queuePlayerWithItems:"

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @advanceToNextItem@
advanceToNextItemSelector :: Selector
advanceToNextItemSelector = mkSelector "advanceToNextItem"

-- | @Selector@ for @canInsertItem:afterItem:@
canInsertItem_afterItemSelector :: Selector
canInsertItem_afterItemSelector = mkSelector "canInsertItem:afterItem:"

-- | @Selector@ for @insertItem:afterItem:@
insertItem_afterItemSelector :: Selector
insertItem_afterItemSelector = mkSelector "insertItem:afterItem:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector
removeItemSelector = mkSelector "removeItem:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector
removeAllItemsSelector = mkSelector "removeAllItems"

