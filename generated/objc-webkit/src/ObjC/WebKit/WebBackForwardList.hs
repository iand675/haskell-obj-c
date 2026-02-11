{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebBackForwardList
--
-- WebBackForwardList holds an ordered list of WebHistoryItems that comprises the back and    forward lists.
--
-- Note that the methods which modify instances of this class do not cause    navigation to happen in other layers of the stack;  they are only for maintaining this data    structure.
--
-- Generated bindings for @WebBackForwardList@.
module ObjC.WebKit.WebBackForwardList
  ( WebBackForwardList
  , IsWebBackForwardList(..)
  , addItem
  , goBack
  , goForward
  , goToItem
  , backListWithLimit
  , forwardListWithLimit
  , containsItem
  , itemAtIndex
  , setPageCacheSize
  , pageCacheSize
  , backItem
  , currentItem
  , forwardItem
  , capacity
  , setCapacity
  , backListCount
  , forwardListCount
  , addItemSelector
  , goBackSelector
  , goForwardSelector
  , goToItemSelector
  , backListWithLimitSelector
  , forwardListWithLimitSelector
  , containsItemSelector
  , itemAtIndexSelector
  , setPageCacheSizeSelector
  , pageCacheSizeSelector
  , backItemSelector
  , currentItemSelector
  , forwardItemSelector
  , capacitySelector
  , setCapacitySelector
  , backListCountSelector
  , forwardListCountSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | addItem:
--
-- Adds an entry to the list.
--
-- @item@ — The entry to add.
--
-- The added entry is inserted immediately after the current entry.    If the current position in the list is not at the end of the list, elements in the    forward list will be dropped at this point.  In addition, entries may be dropped to keep    the size of the list within the maximum size.
--
-- ObjC selector: @- addItem:@
addItem :: (IsWebBackForwardList webBackForwardList, IsWebHistoryItem item) => webBackForwardList -> item -> IO ()
addItem webBackForwardList  item =
withObjCPtr item $ \raw_item ->
    sendMsg webBackForwardList (mkSelector "addItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | goBack
--
-- Move the current pointer back to the entry before the current entry.
--
-- ObjC selector: @- goBack@
goBack :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO ()
goBack webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "goBack") retVoid []

-- | goForward
--
-- Move the current pointer ahead to the entry after the current entry.
--
-- ObjC selector: @- goForward@
goForward :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO ()
goForward webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "goForward") retVoid []

-- | goToItem:
--
-- Move the current pointer to the given entry.
--
-- @item@ — The history item to move the pointer to
--
-- ObjC selector: @- goToItem:@
goToItem :: (IsWebBackForwardList webBackForwardList, IsWebHistoryItem item) => webBackForwardList -> item -> IO ()
goToItem webBackForwardList  item =
withObjCPtr item $ \raw_item ->
    sendMsg webBackForwardList (mkSelector "goToItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | backListWithLimit:
--
-- Returns a portion of the list before the current entry.
--
-- @limit@ — A cap on the size of the array returned.
--
-- Returns: An array of items before the current entry, or nil if there are none.  The entries are in the order that they were originally visited.
--
-- ObjC selector: @- backListWithLimit:@
backListWithLimit :: IsWebBackForwardList webBackForwardList => webBackForwardList -> CInt -> IO (Id NSArray)
backListWithLimit webBackForwardList  limit =
  sendMsg webBackForwardList (mkSelector "backListWithLimit:") (retPtr retVoid) [argCInt (fromIntegral limit)] >>= retainedObject . castPtr

-- | forwardListWithLimit:
--
-- Returns a portion of the list after the current entry.
--
-- @limit@ — A cap on the size of the array returned.
--
-- Returns: An array of items after the current entry, or nil if there are none.  The entries are in the order that they were originally visited.
--
-- ObjC selector: @- forwardListWithLimit:@
forwardListWithLimit :: IsWebBackForwardList webBackForwardList => webBackForwardList -> CInt -> IO (Id NSArray)
forwardListWithLimit webBackForwardList  limit =
  sendMsg webBackForwardList (mkSelector "forwardListWithLimit:") (retPtr retVoid) [argCInt (fromIntegral limit)] >>= retainedObject . castPtr

-- | containsItem:
--
-- @item@ — The item that will be checked for presence in the WebBackForwardList.
--
-- Returns: Returns YES if the item is in the list.
--
-- ObjC selector: @- containsItem:@
containsItem :: (IsWebBackForwardList webBackForwardList, IsWebHistoryItem item) => webBackForwardList -> item -> IO Bool
containsItem webBackForwardList  item =
withObjCPtr item $ \raw_item ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg webBackForwardList (mkSelector "containsItem:") retCULong [argPtr (castPtr raw_item :: Ptr ())]

-- | itemAtIndex:
--
-- Returns an entry the given distance from the current entry.
--
-- @index@ — Index of the desired list item relative to the current item; 0 is current item, -1 is back item, 1 is forward item, etc.
--
-- Returns: The entry the given distance from the current entry. If index exceeds the limits of the list, nil is returned.
--
-- ObjC selector: @- itemAtIndex:@
itemAtIndex :: IsWebBackForwardList webBackForwardList => webBackForwardList -> CInt -> IO (Id WebHistoryItem)
itemAtIndex webBackForwardList  index =
  sendMsg webBackForwardList (mkSelector "itemAtIndex:") (retPtr retVoid) [argCInt (fromIntegral index)] >>= retainedObject . castPtr

-- | setPageCacheSize:
--
-- The size passed to this method determines whether the WebView     associated with this WebBackForwardList will use the shared page cache.
--
-- @size@ — If size is 0, the WebView associated with this WebBackForwardList    will not use the shared page cache. Otherwise, it will.
--
-- ObjC selector: @- setPageCacheSize:@
setPageCacheSize :: IsWebBackForwardList webBackForwardList => webBackForwardList -> CULong -> IO ()
setPageCacheSize webBackForwardList  size =
  sendMsg webBackForwardList (mkSelector "setPageCacheSize:") retVoid [argCULong (fromIntegral size)]

-- | pageCacheSize
--
-- Returns the size of the shared page cache, or 0.
--
-- Returns: The size of the shared page cache (in pages), or 0 if the WebView     associated with this WebBackForwardList will not use the shared page cache.
--
-- ObjC selector: @- pageCacheSize@
pageCacheSize :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO CULong
pageCacheSize webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "pageCacheSize") retCULong []

-- | backItem
--
-- The entry right before the current entry, or nil if there isn't one.
--
-- ObjC selector: @- backItem@
backItem :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO (Id WebHistoryItem)
backItem webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "backItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | currentItem
--
-- Returns the current entry.
--
-- ObjC selector: @- currentItem@
currentItem :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO (Id WebHistoryItem)
currentItem webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "currentItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | forwardItem
--
-- The entry right after the current entry, or nil if there isn't one.
--
-- ObjC selector: @- forwardItem@
forwardItem :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO (Id WebHistoryItem)
forwardItem webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "forwardItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | capacity
--
-- The list's maximum size.
--
-- ObjC selector: @- capacity@
capacity :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO CInt
capacity webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "capacity") retCInt []

-- | capacity
--
-- The list's maximum size.
--
-- ObjC selector: @- setCapacity:@
setCapacity :: IsWebBackForwardList webBackForwardList => webBackForwardList -> CInt -> IO ()
setCapacity webBackForwardList  value =
  sendMsg webBackForwardList (mkSelector "setCapacity:") retVoid [argCInt (fromIntegral value)]

-- | backListCount
--
-- The number of items in the list.
--
-- ObjC selector: @- backListCount@
backListCount :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO CInt
backListCount webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "backListCount") retCInt []

-- | forwardListCount
--
-- Returns: The number of items in the list.
--
-- ObjC selector: @- forwardListCount@
forwardListCount :: IsWebBackForwardList webBackForwardList => webBackForwardList -> IO CInt
forwardListCount webBackForwardList  =
  sendMsg webBackForwardList (mkSelector "forwardListCount") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addItem:@
addItemSelector :: Selector
addItemSelector = mkSelector "addItem:"

-- | @Selector@ for @goBack@
goBackSelector :: Selector
goBackSelector = mkSelector "goBack"

-- | @Selector@ for @goForward@
goForwardSelector :: Selector
goForwardSelector = mkSelector "goForward"

-- | @Selector@ for @goToItem:@
goToItemSelector :: Selector
goToItemSelector = mkSelector "goToItem:"

-- | @Selector@ for @backListWithLimit:@
backListWithLimitSelector :: Selector
backListWithLimitSelector = mkSelector "backListWithLimit:"

-- | @Selector@ for @forwardListWithLimit:@
forwardListWithLimitSelector :: Selector
forwardListWithLimitSelector = mkSelector "forwardListWithLimit:"

-- | @Selector@ for @containsItem:@
containsItemSelector :: Selector
containsItemSelector = mkSelector "containsItem:"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @setPageCacheSize:@
setPageCacheSizeSelector :: Selector
setPageCacheSizeSelector = mkSelector "setPageCacheSize:"

-- | @Selector@ for @pageCacheSize@
pageCacheSizeSelector :: Selector
pageCacheSizeSelector = mkSelector "pageCacheSize"

-- | @Selector@ for @backItem@
backItemSelector :: Selector
backItemSelector = mkSelector "backItem"

-- | @Selector@ for @currentItem@
currentItemSelector :: Selector
currentItemSelector = mkSelector "currentItem"

-- | @Selector@ for @forwardItem@
forwardItemSelector :: Selector
forwardItemSelector = mkSelector "forwardItem"

-- | @Selector@ for @capacity@
capacitySelector :: Selector
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @setCapacity:@
setCapacitySelector :: Selector
setCapacitySelector = mkSelector "setCapacity:"

-- | @Selector@ for @backListCount@
backListCountSelector :: Selector
backListCountSelector = mkSelector "backListCount"

-- | @Selector@ for @forwardListCount@
forwardListCountSelector :: Selector
forwardListCountSelector = mkSelector "forwardListCount"

