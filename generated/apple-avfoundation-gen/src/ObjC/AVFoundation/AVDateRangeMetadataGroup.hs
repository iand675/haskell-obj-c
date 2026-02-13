{-# LANGUAGE DataKinds #-}
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
  , endDateSelector
  , initWithItems_startDate_endDateSelector
  , itemsSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithItems_startDate_endDate avDateRangeMetadataGroup items startDate endDate =
  sendOwnedMessage avDateRangeMetadataGroup initWithItems_startDate_endDateSelector (toNSArray items) (toNSDate startDate) (toNSDate endDate)

-- | @- startDate@
startDate :: IsAVDateRangeMetadataGroup avDateRangeMetadataGroup => avDateRangeMetadataGroup -> IO (Id NSDate)
startDate avDateRangeMetadataGroup =
  sendMessage avDateRangeMetadataGroup startDateSelector

-- | @- endDate@
endDate :: IsAVDateRangeMetadataGroup avDateRangeMetadataGroup => avDateRangeMetadataGroup -> IO (Id NSDate)
endDate avDateRangeMetadataGroup =
  sendMessage avDateRangeMetadataGroup endDateSelector

-- | @- items@
items :: IsAVDateRangeMetadataGroup avDateRangeMetadataGroup => avDateRangeMetadataGroup -> IO (Id NSArray)
items avDateRangeMetadataGroup =
  sendMessage avDateRangeMetadataGroup itemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItems:startDate:endDate:@
initWithItems_startDate_endDateSelector :: Selector '[Id NSArray, Id NSDate, Id NSDate] (Id AVDateRangeMetadataGroup)
initWithItems_startDate_endDateSelector = mkSelector "initWithItems:startDate:endDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

