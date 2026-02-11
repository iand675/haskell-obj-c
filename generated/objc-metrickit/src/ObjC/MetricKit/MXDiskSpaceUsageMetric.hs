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
  , totalBinaryFileSizeSelector
  , totalBinaryFileCountSelector
  , totalDataFileSizeSelector
  , totalDataFileCountSelector
  , totalCacheFolderSizeSelector
  , totalCloneSizeSelector
  , totalDiskSpaceUsedSizeSelector
  , totalDiskSpaceCapacitySelector


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
totalBinaryFileSize mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalBinaryFileSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalBinaryFileCount
--
-- Total count of fixed files owned by the app.
--
-- ObjC selector: @- totalBinaryFileCount@
totalBinaryFileCount :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO CLong
totalBinaryFileCount mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalBinaryFileCount") retCLong []

-- | totalDataFileSize
--
-- Total data file size used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalDataFileSize@
totalDataFileSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalDataFileSize mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalDataFileSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalDataFileCount
--
-- Total count of data files owned by the app.
--
-- ObjC selector: @- totalDataFileCount@
totalDataFileCount :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO CLong
totalDataFileCount mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalDataFileCount") retCLong []

-- | totalCacheFolderSize
--
-- Total file size contained within the apps cache folder.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalCacheFolderSize@
totalCacheFolderSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalCacheFolderSize mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalCacheFolderSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalCloneSize
--
-- Total clone size used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalCloneSize@
totalCloneSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalCloneSize mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalCloneSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalDiskSpaceUsedSize
--
-- Total disk space used by the app.
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalDiskSpaceUsedSize@
totalDiskSpaceUsedSize :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalDiskSpaceUsedSize mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalDiskSpaceUsedSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalDiskSpaceCapacity
--
-- Total disk space capacity of the device
--
-- Dimensioned as NSUnitInformationStorage, base unit is bytes.
--
-- ObjC selector: @- totalDiskSpaceCapacity@
totalDiskSpaceCapacity :: IsMXDiskSpaceUsageMetric mxDiskSpaceUsageMetric => mxDiskSpaceUsageMetric -> IO (Id NSMeasurement)
totalDiskSpaceCapacity mxDiskSpaceUsageMetric  =
  sendMsg mxDiskSpaceUsageMetric (mkSelector "totalDiskSpaceCapacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @totalBinaryFileSize@
totalBinaryFileSizeSelector :: Selector
totalBinaryFileSizeSelector = mkSelector "totalBinaryFileSize"

-- | @Selector@ for @totalBinaryFileCount@
totalBinaryFileCountSelector :: Selector
totalBinaryFileCountSelector = mkSelector "totalBinaryFileCount"

-- | @Selector@ for @totalDataFileSize@
totalDataFileSizeSelector :: Selector
totalDataFileSizeSelector = mkSelector "totalDataFileSize"

-- | @Selector@ for @totalDataFileCount@
totalDataFileCountSelector :: Selector
totalDataFileCountSelector = mkSelector "totalDataFileCount"

-- | @Selector@ for @totalCacheFolderSize@
totalCacheFolderSizeSelector :: Selector
totalCacheFolderSizeSelector = mkSelector "totalCacheFolderSize"

-- | @Selector@ for @totalCloneSize@
totalCloneSizeSelector :: Selector
totalCloneSizeSelector = mkSelector "totalCloneSize"

-- | @Selector@ for @totalDiskSpaceUsedSize@
totalDiskSpaceUsedSizeSelector :: Selector
totalDiskSpaceUsedSizeSelector = mkSelector "totalDiskSpaceUsedSize"

-- | @Selector@ for @totalDiskSpaceCapacity@
totalDiskSpaceCapacitySelector :: Selector
totalDiskSpaceCapacitySelector = mkSelector "totalDiskSpaceCapacity"

