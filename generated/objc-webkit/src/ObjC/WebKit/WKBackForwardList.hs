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
  , itemAtIndexSelector
  , currentItemSelector
  , backItemSelector
  , forwardItemSelector
  , backListSelector
  , forwardListSelector


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

-- | Returns the item at a specified distance from the current item.
--
-- @index@ — Index of the desired list item relative to the current item: 0 for the current item, -1 for the immediately preceding item, 1 for the immediately following item, and so on.
--
-- Returns: The item at the specified distance from the current item, or nil if the index parameter exceeds the limits of the list.
--
-- ObjC selector: @- itemAtIndex:@
itemAtIndex :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> CLong -> IO (Id WKBackForwardListItem)
itemAtIndex wkBackForwardList  index =
  sendMsg wkBackForwardList (mkSelector "itemAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | The current item.
--
-- ObjC selector: @- currentItem@
currentItem :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id WKBackForwardListItem)
currentItem wkBackForwardList  =
  sendMsg wkBackForwardList (mkSelector "currentItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The item immediately preceding the current item, or nilif there isn't one.
--
-- ObjC selector: @- backItem@
backItem :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id WKBackForwardListItem)
backItem wkBackForwardList  =
  sendMsg wkBackForwardList (mkSelector "backItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The item immediately following the current item, or nilif there isn't one.
--
-- ObjC selector: @- forwardItem@
forwardItem :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id WKBackForwardListItem)
forwardItem wkBackForwardList  =
  sendMsg wkBackForwardList (mkSelector "forwardItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The portion of the list preceding the current item.
--
-- The items are in the order in which they were originally visited.
--
-- ObjC selector: @- backList@
backList :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id NSArray)
backList wkBackForwardList  =
  sendMsg wkBackForwardList (mkSelector "backList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The portion of the list following the current item.
--
-- The items are in the order in which they were originally visited.
--
-- ObjC selector: @- forwardList@
forwardList :: IsWKBackForwardList wkBackForwardList => wkBackForwardList -> IO (Id NSArray)
forwardList wkBackForwardList  =
  sendMsg wkBackForwardList (mkSelector "forwardList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @currentItem@
currentItemSelector :: Selector
currentItemSelector = mkSelector "currentItem"

-- | @Selector@ for @backItem@
backItemSelector :: Selector
backItemSelector = mkSelector "backItem"

-- | @Selector@ for @forwardItem@
forwardItemSelector :: Selector
forwardItemSelector = mkSelector "forwardItem"

-- | @Selector@ for @backList@
backListSelector :: Selector
backListSelector = mkSelector "backList"

-- | @Selector@ for @forwardList@
forwardListSelector :: Selector
forwardListSelector = mkSelector "forwardList"

