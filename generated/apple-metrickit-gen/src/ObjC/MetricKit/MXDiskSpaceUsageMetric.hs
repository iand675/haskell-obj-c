{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXDiskSpaceUsageMetric
--
-- An MXMetric subclass that encapsulates app disk space usage Metrics.
--
-- Disk space is a shared resource on-device, and overusage of available disk space can negatively impact the customer experience.
--
-- This Metric subclass contains properties that describe disk space usage in various locations in the apps container. Some properties also describe the number of files in common locations, and the types of files.
--
-- The data contained in this Metric is computed as a daily snapshot, and should be used as a guide to optimize app disk space usage.
--
-- Generated bindings for @MXDiskSpaceUsageMetric@.
module ObjC.MetricKit.MXDiskSpaceUsageMetric
  ( MXDiskSpaceUsageMetric
  , IsMXDiskSpaceUsageMetric(..)
  , totalBinaryFileSize
  , totalBinaryFileCount
  , totalDataFileSize
  , totalDataFileCount
  , totalCacheFolderSize
  , totalCloneSize
  , totalDiskSpaceUsedSize
  , totalDiskSpaceCapacity
  , totalBinaryFileCountSelector
  , totalBinaryFileSizeSelector
  , totalCacheFolderSizeSelector
  , totalCloneSizeSelector
  , totalDataFileCountSelector
  , totalDataFileSizeSelector
  , totalDiskSpaceCapacitySelector
  , totalDiskSpaceUsedSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | totalBinaryFileSize
--
-- Total fixed size used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalBinaryFileSize@
totalBinaryFileSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalBinaryFileSize mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalBinaryFileSizeSelector

-- | totalBinaryFileCount
--
-- Total count of fixed files owned by the app.
--
-- ObjC selector: @- totalBinaryFileCount@
totalBinaryFileCount :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO CLong
totalBinaryFileCount mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalBinaryFileCountSelector

-- | totalDataFileSize
--
-- Total data file size used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalDataFileSize@
totalDataFileSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalDataFileSize mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalDataFileSizeSelector

-- | totalDataFileCount
--
-- Total count of data files owned by the app.
--
-- ObjC selector: @- totalDataFileCount@
totalDataFileCount :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO CLong
totalDataFileCount mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalDataFileCountSelector

-- | totalCacheFolderSize
--
-- Total file size contained within the apps cache folder.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalCacheFolderSize@
totalCacheFolderSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalCacheFolderSize mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalCacheFolderSizeSelector

-- | totalCloneSize
--
-- Total clone size used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalCloneSize@
totalCloneSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalCloneSize mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalCloneSizeSelector

-- | totalDiskSpaceUsedSize
--
-- Total disk space used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalDiskSpaceUsedSize@
totalDiskSpaceUsedSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalDiskSpaceUsedSize mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalDiskSpaceUsedSizeSelector

-- | totalDiskSpaceCapacity
--
-- Total disk space capacity of the device
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalDiskSpaceCapacity@
totalDiskSpaceCapacity :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalDiskSpaceCapacity mxDiskSpaceUsageMetric =
  sendMessage mxDiskSpaceUsageMetric totalDiskSpaceCapacitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @totalBinaryFileSize@
totalBinaryFileSizeSelector :: Selector '[] (Id NSMeasurement)
totalBinaryFileSizeSelector = mkSelector "totalBinaryFileSize"

-- | @Selector@ for @totalBinaryFileCount@
totalBinaryFileCountSelector :: Selector '[] CLong
totalBinaryFileCountSelector = mkSelector "totalBinaryFileCount"

-- | @Selector@ for @totalDataFileSize@
totalDataFileSizeSelector :: Selector '[] (Id NSMeasurement)
totalDataFileSizeSelector = mkSelector "totalDataFileSize"

-- | @Selector@ for @totalDataFileCount@
totalDataFileCountSelector :: Selector '[] CLong
totalDataFileCountSelector = mkSelector "totalDataFileCount"

-- | @Selector@ for @totalCacheFolderSize@
totalCacheFolderSizeSelector :: Selector '[] (Id NSMeasurement)
totalCacheFolderSizeSelector = mkSelector "totalCacheFolderSize"

-- | @Selector@ for @totalCloneSize@
totalCloneSizeSelector :: Selector '[] (Id NSMeasurement)
totalCloneSizeSelector = mkSelector "totalCloneSize"

-- | @Selector@ for @totalDiskSpaceUsedSize@
totalDiskSpaceUsedSizeSelector :: Selector '[] (Id NSMeasurement)
totalDiskSpaceUsedSizeSelector = mkSelector "totalDiskSpaceUsedSize"

-- | @Selector@ for @totalDiskSpaceCapacity@
totalDiskSpaceCapacitySelector :: Selector '[] (Id NSMeasurement)
totalDiskSpaceCapacitySelector = mkSelector "totalDiskSpaceCapacity"

