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
  , startDateSelector
  , setStartDateSelector
  , endDateSelector
  , setEndDateSelector
  , itemsSelector
  , setItemsSelector


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

-- | @- startDate@
startDate :: IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup => avMutableDateRangeMetadataGroup -> IO (Id NSDate)
startDate avMutableDateRangeMetadataGroup  =
  sendMsg avMutableDateRangeMetadataGroup (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartDate:@
setStartDate :: (IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup, IsNSDate value) => avMutableDateRangeMetadataGroup -> value -> IO ()
setStartDate avMutableDateRangeMetadataGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableDateRangeMetadataGroup (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endDate@
endDate :: IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup => avMutableDateRangeMetadataGroup -> IO (Id NSDate)
endDate avMutableDateRangeMetadataGroup  =
  sendMsg avMutableDateRangeMetadataGroup (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndDate:@
setEndDate :: (IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup, IsNSDate value) => avMutableDateRangeMetadataGroup -> value -> IO ()
setEndDate avMutableDateRangeMetadataGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableDateRangeMetadataGroup (mkSelector "setEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- items@
items :: IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup => avMutableDateRangeMetadataGroup -> IO (Id NSArray)
items avMutableDateRangeMetadataGroup  =
  sendMsg avMutableDateRangeMetadataGroup (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setItems:@
setItems :: (IsAVMutableDateRangeMetadataGroup avMutableDateRangeMetadataGroup, IsNSArray value) => avMutableDateRangeMetadataGroup -> value -> IO ()
setItems avMutableDateRangeMetadataGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableDateRangeMetadataGroup (mkSelector "setItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector
setEndDateSelector = mkSelector "setEndDate:"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @setItems:@
setItemsSelector :: Selector
setItemsSelector = mkSelector "setItems:"

