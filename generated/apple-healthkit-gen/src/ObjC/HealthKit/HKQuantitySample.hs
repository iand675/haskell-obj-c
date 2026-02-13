{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantitySample
--
-- An abstract HKSample subclass representing a quantity measurement.
--
-- Generated bindings for @HKQuantitySample@.
module ObjC.HealthKit.HKQuantitySample
  ( HKQuantitySample
  , IsHKQuantitySample(..)
  , quantitySampleWithType_quantity_startDate_endDate
  , quantitySampleWithType_quantity_startDate_endDate_metadata
  , quantitySampleWithType_quantity_startDate_endDate_device_metadata
  , quantityType
  , quantity
  , count
  , countSelector
  , quantitySampleWithType_quantity_startDate_endDateSelector
  , quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector
  , quantitySampleWithType_quantity_startDate_endDate_metadataSelector
  , quantitySelector
  , quantityTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | quantitySampleWithType:quantity:startDate:endDate:
--
-- Creates a new HKQuantitySample with the given type, quantity, start date, and end date.
--
-- The quantity must have a unit that is compatible with the given quantity type.                See -[HKQuantityType isCompatibleWithUnit:].
--
-- ObjC selector: @+ quantitySampleWithType:quantity:startDate:endDate:@
quantitySampleWithType_quantity_startDate_endDate :: (IsHKQuantityType quantityType, IsHKQuantity quantity, IsNSDate startDate, IsNSDate endDate) => quantityType -> quantity -> startDate -> endDate -> IO (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate quantityType quantity startDate endDate =
  do
    cls' <- getRequiredClass "HKQuantitySample"
    sendClassMessage cls' quantitySampleWithType_quantity_startDate_endDateSelector (toHKQuantityType quantityType) (toHKQuantity quantity) (toNSDate startDate) (toNSDate endDate)

-- | quantitySampleWithType:quantity:startDate:endDate:metadata:
--
-- Creates a new HKQuantitySample with the given type, quantity, start date, end date, and metadata.
--
-- The quantity must have a unit that is compatible with the given quantity type.                See -[HKQuantityType isCompatibleWithUnit:].
--
-- ObjC selector: @+ quantitySampleWithType:quantity:startDate:endDate:metadata:@
quantitySampleWithType_quantity_startDate_endDate_metadata :: (IsHKQuantityType quantityType, IsHKQuantity quantity, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => quantityType -> quantity -> startDate -> endDate -> metadata -> IO (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate_metadata quantityType quantity startDate endDate metadata =
  do
    cls' <- getRequiredClass "HKQuantitySample"
    sendClassMessage cls' quantitySampleWithType_quantity_startDate_endDate_metadataSelector (toHKQuantityType quantityType) (toHKQuantity quantity) (toNSDate startDate) (toNSDate endDate) (toNSDictionary metadata)

-- | quantitySampleWithType:quantity:startDate:endDate:device:metadata:
--
-- Creates a new HKQuantitySample with the given type, quantity, start date, end date, and metadata.
--
-- @quantityType@ — The type of the sample.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- @device@ — The HKDevice that generated the sample (optional).
--
-- @metadata@ — Metadata for the sample (optional).
--
-- The quantity must have a unit that is compatible with the given quantity type.                See -[HKQuantityType isCompatibleWithUnit:].
--
-- ObjC selector: @+ quantitySampleWithType:quantity:startDate:endDate:device:metadata:@
quantitySampleWithType_quantity_startDate_endDate_device_metadata :: (IsHKQuantityType quantityType, IsHKQuantity quantity, IsNSDate startDate, IsNSDate endDate, IsHKDevice device, IsNSDictionary metadata) => quantityType -> quantity -> startDate -> endDate -> device -> metadata -> IO (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate_device_metadata quantityType quantity startDate endDate device metadata =
  do
    cls' <- getRequiredClass "HKQuantitySample"
    sendClassMessage cls' quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector (toHKQuantityType quantityType) (toHKQuantity quantity) (toNSDate startDate) (toNSDate endDate) (toHKDevice device) (toNSDictionary metadata)

-- | @- quantityType@
quantityType :: IsHKQuantitySample hkQuantitySample => hkQuantitySample -> IO (Id HKQuantityType)
quantityType hkQuantitySample =
  sendMessage hkQuantitySample quantityTypeSelector

-- | @- quantity@
quantity :: IsHKQuantitySample hkQuantitySample => hkQuantitySample -> IO (Id HKQuantity)
quantity hkQuantitySample =
  sendMessage hkQuantitySample quantitySelector

-- | count
--
-- The number of individual values making up the receiver's quantity.
--
-- Requests for the individual series quantities can be made using HKQuantitySeriesSampleQuery.
--
-- ObjC selector: @- count@
count :: IsHKQuantitySample hkQuantitySample => hkQuantitySample -> IO CLong
count hkQuantitySample =
  sendMessage hkQuantitySample countSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quantitySampleWithType:quantity:startDate:endDate:@
quantitySampleWithType_quantity_startDate_endDateSelector :: Selector '[Id HKQuantityType, Id HKQuantity, Id NSDate, Id NSDate] (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDateSelector = mkSelector "quantitySampleWithType:quantity:startDate:endDate:"

-- | @Selector@ for @quantitySampleWithType:quantity:startDate:endDate:metadata:@
quantitySampleWithType_quantity_startDate_endDate_metadataSelector :: Selector '[Id HKQuantityType, Id HKQuantity, Id NSDate, Id NSDate, Id NSDictionary] (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate_metadataSelector = mkSelector "quantitySampleWithType:quantity:startDate:endDate:metadata:"

-- | @Selector@ for @quantitySampleWithType:quantity:startDate:endDate:device:metadata:@
quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector :: Selector '[Id HKQuantityType, Id HKQuantity, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector = mkSelector "quantitySampleWithType:quantity:startDate:endDate:device:metadata:"

-- | @Selector@ for @quantityType@
quantityTypeSelector :: Selector '[] (Id HKQuantityType)
quantityTypeSelector = mkSelector "quantityType"

-- | @Selector@ for @quantity@
quantitySelector :: Selector '[] (Id HKQuantity)
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @count@
countSelector :: Selector '[] CLong
countSelector = mkSelector "count"

