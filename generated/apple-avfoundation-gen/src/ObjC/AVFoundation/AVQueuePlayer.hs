{-# LANGUAGE DataKinds #-}
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
  , advanceToNextItemSelector
  , canInsertItem_afterItemSelector
  , initWithItemsSelector
  , insertItem_afterItemSelector
  , itemsSelector
  , queuePlayerWithItemsSelector
  , removeAllItemsSelector
  , removeItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' queuePlayerWithItemsSelector (toNSArray items)

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
initWithItems avQueuePlayer items =
  sendOwnedMessage avQueuePlayer initWithItemsSelector (toNSArray items)

-- | Provides an array of the currently enqueued items.
--
-- - Returns: An NSArray containing the enqueued AVPlayerItems.
--
-- ObjC selector: @- items@
items :: IsAVQueuePlayer avQueuePlayer => avQueuePlayer -> IO (Id NSArray)
items avQueuePlayer =
  sendMessage avQueuePlayer itemsSelector

-- | Ends playback of the current item and initiates playback of the next item in the player's queue.
--
-- Removes the current item from the play queue.
--
-- ObjC selector: @- advanceToNextItem@
advanceToNextItem :: IsAVQueuePlayer avQueuePlayer => avQueuePlayer -> IO ()
advanceToNextItem avQueuePlayer =
  sendMessage avQueuePlayer advanceToNextItemSelector

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
canInsertItem_afterItem avQueuePlayer item afterItem =
  sendMessage avQueuePlayer canInsertItem_afterItemSelector (toAVPlayerItem item) (toAVPlayerItem afterItem)

-- | Places an AVPlayerItem after the specified item in the queue.
--
-- This method throws an exception if item already exists in the queue.
--
-- - Parameter item: The item to be inserted. - Parameter afterItem: The item that the newly inserted item should follow in the queue. Pass nil to append the item to the queue.
--
-- ObjC selector: @- insertItem:afterItem:@
insertItem_afterItem :: (IsAVQueuePlayer avQueuePlayer, IsAVPlayerItem item, IsAVPlayerItem afterItem) => avQueuePlayer -> item -> afterItem -> IO ()
insertItem_afterItem avQueuePlayer item afterItem =
  sendMessage avQueuePlayer insertItem_afterItemSelector (toAVPlayerItem item) (toAVPlayerItem afterItem)

-- | Removes an AVPlayerItem from the queue.
--
-- If the item to be removed is currently playing, has the same effect as -advanceToNextItem.
--
-- - Parameter item: The item to be removed.
--
-- ObjC selector: @- removeItem:@
removeItem :: (IsAVQueuePlayer avQueuePlayer, IsAVPlayerItem item) => avQueuePlayer -> item -> IO ()
removeItem avQueuePlayer item =
  sendMessage avQueuePlayer removeItemSelector (toAVPlayerItem item)

-- | Removes all items from the queue.
--
-- Stops playback by the target.
--
-- ObjC selector: @- removeAllItems@
removeAllItems :: IsAVQueuePlayer avQueuePlayer => avQueuePlayer -> IO ()
removeAllItems avQueuePlayer =
  sendMessage avQueuePlayer removeAllItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @queuePlayerWithItems:@
queuePlayerWithItemsSelector :: Selector '[Id NSArray] (Id AVQueuePlayer)
queuePlayerWithItemsSelector = mkSelector "queuePlayerWithItems:"

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector '[Id NSArray] (Id AVQueuePlayer)
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @advanceToNextItem@
advanceToNextItemSelector :: Selector '[] ()
advanceToNextItemSelector = mkSelector "advanceToNextItem"

-- | @Selector@ for @canInsertItem:afterItem:@
canInsertItem_afterItemSelector :: Selector '[Id AVPlayerItem, Id AVPlayerItem] Bool
canInsertItem_afterItemSelector = mkSelector "canInsertItem:afterItem:"

-- | @Selector@ for @insertItem:afterItem:@
insertItem_afterItemSelector :: Selector '[Id AVPlayerItem, Id AVPlayerItem] ()
insertItem_afterItemSelector = mkSelector "insertItem:afterItem:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector '[Id AVPlayerItem] ()
removeItemSelector = mkSelector "removeItem:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector '[] ()
removeAllItemsSelector = mkSelector "removeAllItems"

