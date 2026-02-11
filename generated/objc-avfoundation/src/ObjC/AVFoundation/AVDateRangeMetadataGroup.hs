{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVDateRangeMetadataGroup
--
-- AVDateRangeMetadataGroup is used to represent a collection of metadata items that are valid for use within a specific range of dates.
--
-- Generated bindings for @AVDateRangeMetadataGroup@.
module ObjC.AVFoundation.AVDateRangeMetadataGroup
  ( AVDateRangeMetadataGroup
  , IsAVDateRangeMetadataGroup(..)
  , initWithItems_startDate_endDate
  , startDate
  , endDate
  , items
  , initWithItems_startDate_endDateSelector
  , startDateSelector
  , endDateSelector
  , itemsSelector


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

-- | initWithItems:startDate:endDate:
--
-- Initializes an instance of AVDateRangeMetadataGroup with a collection of metadata items.
--
-- @items@ — An NSArray of AVMetadataItems.
--
-- @startDate@ — The start date of the collection of AVMetadataItems.
--
-- @endDate@ — The end date of the collection of AVMetadataItems. If the receiver is intended to represent information about an instantaneous event, the value of endDate should be equal to the value of startDate. A value of nil for endDate indicates that the endDate is indefinite.
--
-- Returns: An instance of AVDateRangeMetadataGroup.
--
-- ObjC selector: @- initWithItems:startDate:endDate:@
initWithItems_startDate_endDate :: (IsAVDateRangeMetadataGroup avDateRangeMetadataGroup, IsNSArray items, IsNSDate startDate, IsNSDate endDate) => avDateRangeMetadataGroup -> items -> startDate -> endDate -> IO (Id AVDateRangeMetadataGroup)
initWithItems_startDate_endDate avDateRangeMetadataGroup  items startDate endDate =
withObjCPtr items $ \raw_items ->
  withObjCPtr startDate $ \raw_startDate ->
    withObjCPtr endDate $ \raw_endDate ->
        sendMsg avDateRangeMetadataGroup (mkSelector "initWithItems:startDate:endDate:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= ownedObject . castPtr

-- | @- startDate@
startDate :: IsAVDateRangeMetadataGroup avDateRangeMetadataGroup => avDateRangeMetadataGroup -> IO (Id NSDate)
startDate avDateRangeMetadataGroup  =
  sendMsg avDateRangeMetadataGroup (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsAVDateRangeMetadataGroup avDateRangeMetadataGroup => avDateRangeMetadataGroup -> IO (Id NSDate)
endDate avDateRangeMetadataGroup  =
  sendMsg avDateRangeMetadataGroup (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- items@
items :: IsAVDateRangeMetadataGroup avDateRangeMetadataGroup => avDateRangeMetadataGroup -> IO (Id NSArray)
items avDateRangeMetadataGroup  =
  sendMsg avDateRangeMetadataGroup (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItems:startDate:endDate:@
initWithItems_startDate_endDateSelector :: Selector
initWithItems_startDate_endDateSelector = mkSelector "initWithItems:startDate:endDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

