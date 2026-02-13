{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKAudiogramSample
--
-- A sample object representing the results of a standard hearing test.
--
-- Generated bindings for @HKAudiogramSample@.
module ObjC.HealthKit.HKAudiogramSample
  ( HKAudiogramSample
  , IsHKAudiogramSample(..)
  , audiogramSampleWithSensitivityPoints_startDate_endDate_metadata
  , audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadata
  , sensitivityPoints
  , audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector
  , audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector
  , sensitivityPointsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:
--
-- Creates a new audiogram sample with the specified attributes.
--
-- @sensitivityPoints@ — Sensitivity data associated with the sample, with a maximum limit of 30 points. Frequencies must be unique, and ordered ascending.
--
-- @startDate@ — The start date for the hearing test.
--
-- @endDate@ — The end date for the hearing test.
--
-- @metadata@ — Optional meta data associated with the sample.
--
-- Returns: A new instance of an audiogram sample.
--
-- ObjC selector: @+ audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_metadata :: (IsNSArray sensitivityPoints, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => sensitivityPoints -> startDate -> endDate -> metadata -> IO (Id HKAudiogramSample)
audiogramSampleWithSensitivityPoints_startDate_endDate_metadata sensitivityPoints startDate endDate metadata =
  do
    cls' <- getRequiredClass "HKAudiogramSample"
    sendClassMessage cls' audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector (toNSArray sensitivityPoints) (toNSDate startDate) (toNSDate endDate) (toNSDictionary metadata)

-- | audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:
--
-- Creates a new audiogram sample with the specified attributes.
--
-- @sensitivityPoints@ — Sensitivity data associated with the sample, with a maximum limit of 30 points. Frequencies must be unique, and ordered ascending.
--
-- @startDate@ — The start date of the hearing test.
--
-- @endDate@ — The end date of the hearing test.
--
-- @device@ — The device that generated the sample data.
--
-- @metadata@ — Optional metadata associated with the sample.
--
-- Returns: A new instance of an audiogram sample.
--
-- ObjC selector: @+ audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadata :: (IsNSArray sensitivityPoints, IsNSDate startDate, IsNSDate endDate, IsHKDevice device, IsNSDictionary metadata) => sensitivityPoints -> startDate -> endDate -> device -> metadata -> IO (Id HKAudiogramSample)
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadata sensitivityPoints startDate endDate device metadata =
  do
    cls' <- getRequiredClass "HKAudiogramSample"
    sendClassMessage cls' audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector (toNSArray sensitivityPoints) (toNSDate startDate) (toNSDate endDate) (toHKDevice device) (toNSDictionary metadata)

-- | sensitivityPoints
--
-- The hearing sensitivity readings associated with a hearing test.
--
-- ObjC selector: @- sensitivityPoints@
sensitivityPoints :: IsHKAudiogramSample hkAudiogramSample => hkAudiogramSample -> IO (Id NSArray)
sensitivityPoints hkAudiogramSample =
  sendMessage hkAudiogramSample sensitivityPointsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector :: Selector '[Id NSArray, Id NSDate, Id NSDate, Id NSDictionary] (Id HKAudiogramSample)
audiogramSampleWithSensitivityPoints_startDate_endDate_metadataSelector = mkSelector "audiogramSampleWithSensitivityPoints:startDate:endDate:metadata:"

-- | @Selector@ for @audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:@
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector :: Selector '[Id NSArray, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKAudiogramSample)
audiogramSampleWithSensitivityPoints_startDate_endDate_device_metadataSelector = mkSelector "audiogramSampleWithSensitivityPoints:startDate:endDate:device:metadata:"

-- | @Selector@ for @sensitivityPoints@
sensitivityPointsSelector :: Selector '[] (Id NSArray)
sensitivityPointsSelector = mkSelector "sensitivityPoints"

