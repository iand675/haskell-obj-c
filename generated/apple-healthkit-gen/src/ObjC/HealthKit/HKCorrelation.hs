{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCorrelation
--
-- An HKCorrelation is a collection of correlated objects.
--
-- When multiple readings are taken together, it may be beneficial to correlate them so that they can be                displayed together and share common metadata about how they were created.
--
-- For example, systolic and diastolic blood pressure readings are typically presented together so these                readings should be saved with a correlation of type blood pressure.
--
-- Generated bindings for @HKCorrelation@.
module ObjC.HealthKit.HKCorrelation
  ( HKCorrelation
  , IsHKCorrelation(..)
  , correlationWithType_startDate_endDate_objects
  , correlationWithType_startDate_endDate_objects_metadata
  , correlationWithType_startDate_endDate_objects_device_metadata
  , objectsForType
  , correlationType
  , objects
  , correlationTypeSelector
  , correlationWithType_startDate_endDate_objectsSelector
  , correlationWithType_startDate_endDate_objects_device_metadataSelector
  , correlationWithType_startDate_endDate_objects_metadataSelector
  , objectsForTypeSelector
  , objectsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | correlationWithType:startDate:endDate:objects:
--
-- Creates a new HKCorrelation with the given type, start date, end date, and objects.
--
-- objects must be a set of HKQuantitySamples and HKCategorySamples
--
-- ObjC selector: @+ correlationWithType:startDate:endDate:objects:@
correlationWithType_startDate_endDate_objects :: (IsHKCorrelationType correlationType, IsNSDate startDate, IsNSDate endDate, IsNSSet objects) => correlationType -> startDate -> endDate -> objects -> IO (Id HKCorrelation)
correlationWithType_startDate_endDate_objects correlationType startDate endDate objects =
  do
    cls' <- getRequiredClass "HKCorrelation"
    sendClassMessage cls' correlationWithType_startDate_endDate_objectsSelector (toHKCorrelationType correlationType) (toNSDate startDate) (toNSDate endDate) (toNSSet objects)

-- | correlationWithType:startDate:endDate:objects:metadata:
--
-- Creates a new HKCorrelation with the given type, start date, end date, objects, and metadata.
--
-- objects must be a set of HKQuantitySamples and HKCategorySamples
--
-- ObjC selector: @+ correlationWithType:startDate:endDate:objects:metadata:@
correlationWithType_startDate_endDate_objects_metadata :: (IsHKCorrelationType correlationType, IsNSDate startDate, IsNSDate endDate, IsNSSet objects, IsNSDictionary metadata) => correlationType -> startDate -> endDate -> objects -> metadata -> IO (Id HKCorrelation)
correlationWithType_startDate_endDate_objects_metadata correlationType startDate endDate objects metadata =
  do
    cls' <- getRequiredClass "HKCorrelation"
    sendClassMessage cls' correlationWithType_startDate_endDate_objects_metadataSelector (toHKCorrelationType correlationType) (toNSDate startDate) (toNSDate endDate) (toNSSet objects) (toNSDictionary metadata)

-- | correlationWithType:startDate:endDate:objects:device:metadata:
--
-- Creates a new HKCorrelation with the given type, start date, end date, objects, and metadata.
--
-- @correlationType@ — The correlation type of the objects set.
--
-- @startDate@ — The start date of the correlation.
--
-- @endDate@ — The end date of the correlation.
--
-- @device@ — The HKDevice that generated the samples (optional).
--
-- @metadata@ — Metadata for the correlation (optional).
--
-- objects must be a set of HKQuantitySamples and HKCategorySamples
--
-- ObjC selector: @+ correlationWithType:startDate:endDate:objects:device:metadata:@
correlationWithType_startDate_endDate_objects_device_metadata :: (IsHKCorrelationType correlationType, IsNSDate startDate, IsNSDate endDate, IsNSSet objects, IsHKDevice device, IsNSDictionary metadata) => correlationType -> startDate -> endDate -> objects -> device -> metadata -> IO (Id HKCorrelation)
correlationWithType_startDate_endDate_objects_device_metadata correlationType startDate endDate objects device metadata =
  do
    cls' <- getRequiredClass "HKCorrelation"
    sendClassMessage cls' correlationWithType_startDate_endDate_objects_device_metadataSelector (toHKCorrelationType correlationType) (toNSDate startDate) (toNSDate endDate) (toNSSet objects) (toHKDevice device) (toNSDictionary metadata)

-- | objectsForType:
--
-- Returns the set of correlated objects with the specified type.
--
-- ObjC selector: @- objectsForType:@
objectsForType :: (IsHKCorrelation hkCorrelation, IsHKObjectType objectType) => hkCorrelation -> objectType -> IO (Id NSSet)
objectsForType hkCorrelation objectType =
  sendMessage hkCorrelation objectsForTypeSelector (toHKObjectType objectType)

-- | @- correlationType@
correlationType :: IsHKCorrelation hkCorrelation => hkCorrelation -> IO (Id HKCorrelationType)
correlationType hkCorrelation =
  sendMessage hkCorrelation correlationTypeSelector

-- | objects
--
-- A set of HKSamples containing all of the objects that were saved with the receiver.
--
-- ObjC selector: @- objects@
objects :: IsHKCorrelation hkCorrelation => hkCorrelation -> IO (Id NSSet)
objects hkCorrelation =
  sendMessage hkCorrelation objectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @correlationWithType:startDate:endDate:objects:@
correlationWithType_startDate_endDate_objectsSelector :: Selector '[Id HKCorrelationType, Id NSDate, Id NSDate, Id NSSet] (Id HKCorrelation)
correlationWithType_startDate_endDate_objectsSelector = mkSelector "correlationWithType:startDate:endDate:objects:"

-- | @Selector@ for @correlationWithType:startDate:endDate:objects:metadata:@
correlationWithType_startDate_endDate_objects_metadataSelector :: Selector '[Id HKCorrelationType, Id NSDate, Id NSDate, Id NSSet, Id NSDictionary] (Id HKCorrelation)
correlationWithType_startDate_endDate_objects_metadataSelector = mkSelector "correlationWithType:startDate:endDate:objects:metadata:"

-- | @Selector@ for @correlationWithType:startDate:endDate:objects:device:metadata:@
correlationWithType_startDate_endDate_objects_device_metadataSelector :: Selector '[Id HKCorrelationType, Id NSDate, Id NSDate, Id NSSet, Id HKDevice, Id NSDictionary] (Id HKCorrelation)
correlationWithType_startDate_endDate_objects_device_metadataSelector = mkSelector "correlationWithType:startDate:endDate:objects:device:metadata:"

-- | @Selector@ for @objectsForType:@
objectsForTypeSelector :: Selector '[Id HKObjectType] (Id NSSet)
objectsForTypeSelector = mkSelector "objectsForType:"

-- | @Selector@ for @correlationType@
correlationTypeSelector :: Selector '[] (Id HKCorrelationType)
correlationTypeSelector = mkSelector "correlationType"

-- | @Selector@ for @objects@
objectsSelector :: Selector '[] (Id NSSet)
objectsSelector = mkSelector "objects"

