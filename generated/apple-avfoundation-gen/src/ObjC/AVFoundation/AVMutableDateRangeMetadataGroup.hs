{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableDateRangeMetadataGroup
--
-- AVMutableDateRangeMetadataGroup is used to represent a mutable collection of metadata items that are valid for use within a specific range of dates.
--
-- Generated bindings for @AVMutableDateRangeMetadataGroup@.
module ObjC.AVFoundation.AVMutableDateRangeMetadataGroup
  ( AVMutableDateRangeMetadataGroup
  , IsAVMutableDateRangeMetadataGroup(..)
  , startDate
  , setStartDate
  , endDate
  , setEndDate
  , items
  , setItems
  , endDateSelector
  , itemsSelector
  , setEndDateSelector
  , setItemsSelector
  , setStartDateSelector
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

-- | @- startDate@
startDate :: IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup => avMutableDateRangeMetadataGroup -> IO (Id NSDate)
startDate avMutableDateRangeMetadataGroup =
  sendMessage avMutableDateRangeMetadataGroup startDateSelector

-- | @- setStartDate:@
setStartDate :: (IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup, IsNSDate value) => avMutableDateRangeMetadataGroup -> value -> IO ()
setStartDate avMutableDateRangeMetadataGroup value =
  sendMessage avMutableDateRangeMetadataGroup setStartDateSelector (toNSDate value)

-- | @- endDate@
endDate :: IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup => avMutableDateRangeMetadataGroup -> IO (Id NSDate)
endDate avMutableDateRangeMetadataGroup =
  sendMessage avMutableDateRangeMetadataGroup endDateSelector

-- | @- setEndDate:@
setEndDate :: (IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup, IsNSDate value) => avMutableDateRangeMetadataGroup -> value -> IO ()
setEndDate avMutableDateRangeMetadataGroup value =
  sendMessage avMutableDateRangeMetadataGroup setEndDateSelector (toNSDate value)

-- | @- items@
items :: IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup => avMutableDateRangeMetadataGroup -> IO (Id NSArray)
items avMutableDateRangeMetadataGroup =
  sendMessage avMutableDateRangeMetadataGroup itemsSelector

-- | @- setItems:@
setItems :: (IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup, IsNSArray value) => avMutableDateRangeMetadataGroup -> value -> IO ()
setItems avMutableDateRangeMetadataGroup value =
  sendMessage avMutableDateRangeMetadataGroup setItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector '[Id NSDate] ()
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector '[Id NSDate] ()
setEndDateSelector = mkSelector "setEndDate:"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @setItems:@
setItemsSelector :: Selector '[Id NSArray] ()
setItemsSelector = mkSelector "setItems:"

