{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCategorySample
--
-- An HKObject subclass representing an category measurement
--
-- Category samples are samples that can be categorized into an enum of concrete values
--
-- Generated bindings for @HKCategorySample@.
module ObjC.HealthKit.HKCategorySample
  ( HKCategorySample
  , IsHKCategorySample(..)
  , init_
  , categorySampleWithType_value_startDate_endDate_metadata
  , categorySampleWithType_value_startDate_endDate
  , categorySampleWithType_value_startDate_endDate_device_metadata
  , categoryType
  , value
  , categorySampleWithType_value_startDate_endDateSelector
  , categorySampleWithType_value_startDate_endDate_device_metadataSelector
  , categorySampleWithType_value_startDate_endDate_metadataSelector
  , categoryTypeSelector
  , initSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKCategorySample hkCategorySample => hkCategorySample -> IO (Id HKCategorySample)
init_ hkCategorySample =
  sendOwnedMessage hkCategorySample initSelector

-- | categorySampleWithType:value:startDate:endDate:metadata:
--
-- Creates a new HKCategorySample.
--
-- @type@ — The type of the sample.
--
-- @value@ — The enumeration value for the sample. See HKCategoryTypeIdentifier for appropriate value.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- @metadata@ — Metadata for the sample (optional).
--
-- ObjC selector: @+ categorySampleWithType:value:startDate:endDate:metadata:@
categorySampleWithType_value_startDate_endDate_metadata :: (IsHKCategoryType type_, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => type_ -> CLong -> startDate -> endDate -> metadata -> IO (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate_metadata type_ value startDate endDate metadata =
  do
    cls' <- getRequiredClass "HKCategorySample"
    sendClassMessage cls' categorySampleWithType_value_startDate_endDate_metadataSelector (toHKCategoryType type_) value (toNSDate startDate) (toNSDate endDate) (toNSDictionary metadata)

-- | categorySampleWithType:value:startDate:endDate:
--
-- Creates a new HKCategorySample.
--
-- @type@ — The type of the sample.
--
-- @value@ — The enumeration value for the sample. See HKCategoryTypeIdentifier for appropriate value.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- ObjC selector: @+ categorySampleWithType:value:startDate:endDate:@
categorySampleWithType_value_startDate_endDate :: (IsHKCategoryType type_, IsNSDate startDate, IsNSDate endDate) => type_ -> CLong -> startDate -> endDate -> IO (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate type_ value startDate endDate =
  do
    cls' <- getRequiredClass "HKCategorySample"
    sendClassMessage cls' categorySampleWithType_value_startDate_endDateSelector (toHKCategoryType type_) value (toNSDate startDate) (toNSDate endDate)

-- | categorySampleWithType:value:startDate:endDate:device:metadata:
--
-- Creates a new HKCategorySample.
--
-- @type@ — The type of the sample.
--
-- @value@ — The enumeration value for the sample. See HKCategoryTypeIdentifier for appropriate value.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- @device@ — The HKDevice that generated the sample (optional).
--
-- @metadata@ — Metadata for the sample (optional).
--
-- ObjC selector: @+ categorySampleWithType:value:startDate:endDate:device:metadata:@
categorySampleWithType_value_startDate_endDate_device_metadata :: (IsHKCategoryType type_, IsNSDate startDate, IsNSDate endDate, IsHKDevice device, IsNSDictionary metadata) => type_ -> CLong -> startDate -> endDate -> device -> metadata -> IO (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate_device_metadata type_ value startDate endDate device metadata =
  do
    cls' <- getRequiredClass "HKCategorySample"
    sendClassMessage cls' categorySampleWithType_value_startDate_endDate_device_metadataSelector (toHKCategoryType type_) value (toNSDate startDate) (toNSDate endDate) (toHKDevice device) (toNSDictionary metadata)

-- | @- categoryType@
categoryType :: IsHKCategorySample hkCategorySample => hkCategorySample -> IO (Id HKCategoryType)
categoryType hkCategorySample =
  sendMessage hkCategorySample categoryTypeSelector

-- | value
--
-- The preferred enum for the value is determined by the receiver's category type.
--
-- ObjC selector: @- value@
value :: IsHKCategorySample hkCategorySample => hkCategorySample -> IO CLong
value hkCategorySample =
  sendMessage hkCategorySample valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKCategorySample)
initSelector = mkSelector "init"

-- | @Selector@ for @categorySampleWithType:value:startDate:endDate:metadata:@
categorySampleWithType_value_startDate_endDate_metadataSelector :: Selector '[Id HKCategoryType, CLong, Id NSDate, Id NSDate, Id NSDictionary] (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate_metadataSelector = mkSelector "categorySampleWithType:value:startDate:endDate:metadata:"

-- | @Selector@ for @categorySampleWithType:value:startDate:endDate:@
categorySampleWithType_value_startDate_endDateSelector :: Selector '[Id HKCategoryType, CLong, Id NSDate, Id NSDate] (Id HKCategorySample)
categorySampleWithType_value_startDate_endDateSelector = mkSelector "categorySampleWithType:value:startDate:endDate:"

-- | @Selector@ for @categorySampleWithType:value:startDate:endDate:device:metadata:@
categorySampleWithType_value_startDate_endDate_device_metadataSelector :: Selector '[Id HKCategoryType, CLong, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKCategorySample)
categorySampleWithType_value_startDate_endDate_device_metadataSelector = mkSelector "categorySampleWithType:value:startDate:endDate:device:metadata:"

-- | @Selector@ for @categoryType@
categoryTypeSelector :: Selector '[] (Id HKCategoryType)
categoryTypeSelector = mkSelector "categoryType"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CLong
valueSelector = mkSelector "value"

