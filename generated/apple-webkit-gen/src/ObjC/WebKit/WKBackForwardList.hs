{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKBackForwardList object is a list of webpages previously visited in a web view that can be reached by going back or forward.
--
-- Generated bindings for @WKBackForwardList@.
module ObjC.WebKit.WKBackForwardList
  ( WKBackForwardList
  , IsWKBackForwardList(..)
  , itemAtIndex
  , currentItem
  , backItem
  , forwardItem
  , backList
  , forwardList
  , backItemSelector
  , backListSelector
  , currentItemSelector
  , forwardItemSelector
  , forwardListSelector
  , itemAtIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the item at a specified distance from the current item.
--
-- @index@ — Index of the desired list item relative to the current item: 0 for the current item, -1 for the immediately preceding item, 1 for the immediately following item, and so on.
--
-- Returns: The item at the specified distance from the current item, or nil if the index parameter exceeds the limits of the list.
--
-- ObjC selector: @- itemAtIndex:@
itemAtIndex :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> CLong -> IO (Id WKBackForwardListItem)
itemAtIndex wkBackForwardList index =
  sendMessage wkBackForwardList itemAtIndexSelector index

-- | The current item.
--
-- ObjC selector: @- currentItem@
currentItem :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id WKBackForwardListItem)
currentItem wkBackForwardList =
  sendMessage wkBackForwardList currentItemSelector

-- | The item immediately preceding the current item, or nilif there isn't one.
--
-- ObjC selector: @- backItem@
backItem :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id WKBackForwardListItem)
backItem wkBackForwardList =
  sendMessage wkBackForwardList backItemSelector

-- | The item immediately following the current item, or nilif there isn't one.
--
-- ObjC selector: @- forwardItem@
forwardItem :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id WKBackForwardListItem)
forwardItem wkBackForwardList =
  sendMessage wkBackForwardList forwardItemSelector

-- | The portion of the list preceding the current item.
--
-- The items are in the order in which they were originally visited.
--
-- ObjC selector: @- backList@
backList :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id NSArray)
backList wkBackForwardList =
  sendMessage wkBackForwardList backListSelector

-- | The portion of the list following the current item.
--
-- The items are in the order in which they were originally visited.
--
-- ObjC selector: @- forwardList@
forwardList :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id NSArray)
forwardList wkBackForwardList =
  sendMessage wkBackForwardList forwardListSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector '[CLong] (Id WKBackForwardListItem)
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @currentItem@
currentItemSelector :: Selector '[] (Id WKBackForwardListItem)
currentItemSelector = mkSelector "currentItem"

-- | @Selector@ for @backItem@
backItemSelector :: Selector '[] (Id WKBackForwardListItem)
backItemSelector = mkSelector "backItem"

-- | @Selector@ for @forwardItem@
forwardItemSelector :: Selector '[] (Id WKBackForwardListItem)
forwardItemSelector = mkSelector "forwardItem"

-- | @Selector@ for @backList@
backListSelector :: Selector '[] (Id NSArray)
backListSelector = mkSelector "backList"

-- | @Selector@ for @forwardList@
forwardListSelector :: Selector '[] (Id NSArray)
forwardListSelector = mkSelector "forwardList"

