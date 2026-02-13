{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXMetricPayload
--
-- A wrapper class which contains a metric payload and associated properties of that payload.
--
-- MXMetricPayload encapsulates currently supported metric types that can be vended by MetricKit. MXMetric subclasses on MXMetricPayload are nullable. If an MXMetric subclass is nil, it indicates that the data is not available for this payload.
--
-- MXMetricPayload exposes a convenience function, JSONRepresentation, to convert the contents of the payload to a human readable JSON. This should be used in conjunction with other APIs that accept NSData.
--
-- An MXMetricPayload contains data that covers a 24 hour period of application usage. The properties timeStampBegin and timeStampEnd should be used to determine which time range the payload covers.
--
-- It is possible for an MXMetricPayload to cover regions of time where an application was updated, and thus had multiple different app version strings. The property latestApplicationVersion will always reflect the latest appVersion at the time the metric payload was created. Use includesMultipleApplicationVersions to determine if an application changed versions during the time range the payload covers.
--
-- Generated bindings for @MXMetricPayload@.
module ObjC.MetricKit.MXMetricPayload
  ( MXMetricPayload
  , IsMXMetricPayload(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , latestApplicationVersion
  , includesMultipleApplicationVersions
  , timeStampBegin
  , timeStampEnd
  , cpuMetrics
  , gpuMetrics
  , cellularConditionMetrics
  , applicationTimeMetrics
  , locationActivityMetrics
  , networkTransferMetrics
  , applicationLaunchMetrics
  , applicationResponsivenessMetrics
  , diskIOMetrics
  , memoryMetrics
  , displayMetrics
  , animationMetrics
  , applicationExitMetrics
  , diskSpaceUsageMetrics
  , signpostMetrics
  , metaData
  , animationMetricsSelector
  , applicationExitMetricsSelector
  , applicationLaunchMetricsSelector
  , applicationResponsivenessMetricsSelector
  , applicationTimeMetricsSelector
  , cellularConditionMetricsSelector
  , cpuMetricsSelector
  , dictionaryRepresentationSelector
  , diskIOMetricsSelector
  , diskSpaceUsageMetricsSelector
  , displayMetricsSelector
  , gpuMetricsSelector
  , includesMultipleApplicationVersionsSelector
  , jsonRepresentationSelector
  , latestApplicationVersionSelector
  , locationActivityMetricsSelector
  , memoryMetricsSelector
  , metaDataSelector
  , networkTransferMetricsSelector
  , signpostMetricsSelector
  , timeStampBeginSelector
  , timeStampEndSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this payload.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSData)
jsonRepresentation mxMetricPayload =
  sendMessage mxMetricPayload jsonRepresentationSelector

-- | DictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this payload.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- DictionaryRepresentation@
dictionaryRepresentation :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSDictionary)
dictionaryRepresentation mxMetricPayload =
  sendMessage mxMetricPayload dictionaryRepresentationSelector

-- | latestApplicationVersion
--
-- An NSString representation of the application version from which this payload was generated.
--
-- If the application version was changed during the aggregation of this data, this value will reflect the latest application version at the time of retrieval.
--
-- ObjC selector: @- latestApplicationVersion@
latestApplicationVersion :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSString)
latestApplicationVersion mxMetricPayload =
  sendMessage mxMetricPayload latestApplicationVersionSelector

-- | includesMultipleApplicationVersions
--
-- A bool which indicates whether or not this payload contains data from multiple application versions.
--
-- A value of YES indicates that this payload's data reflects multiple application versions.
--
-- A value of NO indicates that this payload only reflects data from the application version specified by latestApplicationVersion.
--
-- ObjC selector: @- includesMultipleApplicationVersions@
includesMultipleApplicationVersions :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO Bool
includesMultipleApplicationVersions mxMetricPayload =
  sendMessage mxMetricPayload includesMultipleApplicationVersionsSelector

-- | timeStampBegin
--
-- An NSDate object that indicates the time which the payload was generated.
--
-- ObjC selector: @- timeStampBegin@
timeStampBegin :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSDate)
timeStampBegin mxMetricPayload =
  sendMessage mxMetricPayload timeStampBeginSelector

-- | timeStampEnd
--
-- An NSDate object that indicates the time which the payload was generated.
--
-- ObjC selector: @- timeStampEnd@
timeStampEnd :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSDate)
timeStampEnd mxMetricPayload =
  sendMessage mxMetricPayload timeStampEndSelector

-- | cpuMetrics
--
-- An object containing CPU metrics for this application.
--
-- ObjC selector: @- cpuMetrics@
cpuMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXCPUMetric)
cpuMetrics mxMetricPayload =
  sendMessage mxMetricPayload cpuMetricsSelector

-- | gpuMetrics
--
-- An object containing GPU metrics for this application.
--
-- ObjC selector: @- gpuMetrics@
gpuMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXGPUMetric)
gpuMetrics mxMetricPayload =
  sendMessage mxMetricPayload gpuMetricsSelector

-- | cellularConditionMetrics
--
-- An object containing a cellular condition metrics for this application.
--
-- ObjC selector: @- cellularConditionMetrics@
cellularConditionMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXCellularConditionMetric)
cellularConditionMetrics mxMetricPayload =
  sendMessage mxMetricPayload cellularConditionMetricsSelector

-- | applicationTimeMetrics
--
-- An object containing running mode metrics for this application.
--
-- ObjC selector: @- applicationTimeMetrics@
applicationTimeMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppRunTimeMetric)
applicationTimeMetrics mxMetricPayload =
  sendMessage mxMetricPayload applicationTimeMetricsSelector

-- | locationActivityMetrics
--
-- An object containing location activity metrics for this application.
--
-- ObjC selector: @- locationActivityMetrics@
locationActivityMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXLocationActivityMetric)
locationActivityMetrics mxMetricPayload =
  sendMessage mxMetricPayload locationActivityMetricsSelector

-- | networkTransferMetrics
--
-- An object containing network transfer metrics for this application.
--
-- ObjC selector: @- networkTransferMetrics@
networkTransferMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXNetworkTransferMetric)
networkTransferMetrics mxMetricPayload =
  sendMessage mxMetricPayload networkTransferMetricsSelector

-- | applicationLaunchMetrics
--
-- An object containing launch metrics for this application.
--
-- ObjC selector: @- applicationLaunchMetrics@
applicationLaunchMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppLaunchMetric)
applicationLaunchMetrics mxMetricPayload =
  sendMessage mxMetricPayload applicationLaunchMetricsSelector

-- | applicationResponsivenessMetrics
--
-- An object containing hang metrics for this application.
--
-- ObjC selector: @- applicationResponsivenessMetrics@
applicationResponsivenessMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppResponsivenessMetric)
applicationResponsivenessMetrics mxMetricPayload =
  sendMessage mxMetricPayload applicationResponsivenessMetricsSelector

-- | diskIOMetrics
--
-- An object containing disk IO metrics for this application.
--
-- ObjC selector: @- diskIOMetrics@
diskIOMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXDiskIOMetric)
diskIOMetrics mxMetricPayload =
  sendMessage mxMetricPayload diskIOMetricsSelector

-- | memoryMetrics
--
-- An object containing memory metrics for this application.
--
-- ObjC selector: @- memoryMetrics@
memoryMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXMemoryMetric)
memoryMetrics mxMetricPayload =
  sendMessage mxMetricPayload memoryMetricsSelector

-- | displayMetrics
--
-- An object containing display metrics for this application.
--
-- ObjC selector: @- displayMetrics@
displayMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXDisplayMetric)
displayMetrics mxMetricPayload =
  sendMessage mxMetricPayload displayMetricsSelector

-- | animationMetrics
--
-- An object containing animation metrics for this application.
--
-- ObjC selector: @- animationMetrics@
animationMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAnimationMetric)
animationMetrics mxMetricPayload =
  sendMessage mxMetricPayload animationMetricsSelector

-- | applicationExitMetrics
--
-- An object containing exit metrics for this application.
--
-- ObjC selector: @- applicationExitMetrics@
applicationExitMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppExitMetric)
applicationExitMetrics mxMetricPayload =
  sendMessage mxMetricPayload applicationExitMetricsSelector

-- | diskSpaceUsageMetrics
--
-- An object containing disk space usage metrics for this application.
--
-- ObjC selector: @- diskSpaceUsageMetrics@
diskSpaceUsageMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXDiskSpaceUsageMetric)
diskSpaceUsageMetrics mxMetricPayload =
  sendMessage mxMetricPayload diskSpaceUsageMetricsSelector

-- | signpostMetrics
--
-- An array containing signpost metrics for this application.
--
-- ObjC selector: @- signpostMetrics@
signpostMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSArray)
signpostMetrics mxMetricPayload =
  sendMessage mxMetricPayload signpostMetricsSelector

-- | metaData
--
-- An object containing extra metadata for this payload.
--
-- ObjC selector: @- metaData@
metaData :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXMetaData)
metaData mxMetricPayload =
  sendMessage mxMetricPayload metaDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @DictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "DictionaryRepresentation"

-- | @Selector@ for @latestApplicationVersion@
latestApplicationVersionSelector :: Selector '[] (Id NSString)
latestApplicationVersionSelector = mkSelector "latestApplicationVersion"

-- | @Selector@ for @includesMultipleApplicationVersions@
includesMultipleApplicationVersionsSelector :: Selector '[] Bool
includesMultipleApplicationVersionsSelector = mkSelector "includesMultipleApplicationVersions"

-- | @Selector@ for @timeStampBegin@
timeStampBeginSelector :: Selector '[] (Id NSDate)
timeStampBeginSelector = mkSelector "timeStampBegin"

-- | @Selector@ for @timeStampEnd@
timeStampEndSelector :: Selector '[] (Id NSDate)
timeStampEndSelector = mkSelector "timeStampEnd"

-- | @Selector@ for @cpuMetrics@
cpuMetricsSelector :: Selector '[] (Id MXCPUMetric)
cpuMetricsSelector = mkSelector "cpuMetrics"

-- | @Selector@ for @gpuMetrics@
gpuMetricsSelector :: Selector '[] (Id MXGPUMetric)
gpuMetricsSelector = mkSelector "gpuMetrics"

-- | @Selector@ for @cellularConditionMetrics@
cellularConditionMetricsSelector :: Selector '[] (Id MXCellularConditionMetric)
cellularConditionMetricsSelector = mkSelector "cellularConditionMetrics"

-- | @Selector@ for @applicationTimeMetrics@
applicationTimeMetricsSelector :: Selector '[] (Id MXAppRunTimeMetric)
applicationTimeMetricsSelector = mkSelector "applicationTimeMetrics"

-- | @Selector@ for @locationActivityMetrics@
locationActivityMetricsSelector :: Selector '[] (Id MXLocationActivityMetric)
locationActivityMetricsSelector = mkSelector "locationActivityMetrics"

-- | @Selector@ for @networkTransferMetrics@
networkTransferMetricsSelector :: Selector '[] (Id MXNetworkTransferMetric)
networkTransferMetricsSelector = mkSelector "networkTransferMetrics"

-- | @Selector@ for @applicationLaunchMetrics@
applicationLaunchMetricsSelector :: Selector '[] (Id MXAppLaunchMetric)
applicationLaunchMetricsSelector = mkSelector "applicationLaunchMetrics"

-- | @Selector@ for @applicationResponsivenessMetrics@
applicationResponsivenessMetricsSelector :: Selector '[] (Id MXAppResponsivenessMetric)
applicationResponsivenessMetricsSelector = mkSelector "applicationResponsivenessMetrics"

-- | @Selector@ for @diskIOMetrics@
diskIOMetricsSelector :: Selector '[] (Id MXDiskIOMetric)
diskIOMetricsSelector = mkSelector "diskIOMetrics"

-- | @Selector@ for @memoryMetrics@
memoryMetricsSelector :: Selector '[] (Id MXMemoryMetric)
memoryMetricsSelector = mkSelector "memoryMetrics"

-- | @Selector@ for @displayMetrics@
displayMetricsSelector :: Selector '[] (Id MXDisplayMetric)
displayMetricsSelector = mkSelector "displayMetrics"

-- | @Selector@ for @animationMetrics@
animationMetricsSelector :: Selector '[] (Id MXAnimationMetric)
animationMetricsSelector = mkSelector "animationMetrics"

-- | @Selector@ for @applicationExitMetrics@
applicationExitMetricsSelector :: Selector '[] (Id MXAppExitMetric)
applicationExitMetricsSelector = mkSelector "applicationExitMetrics"

-- | @Selector@ for @diskSpaceUsageMetrics@
diskSpaceUsageMetricsSelector :: Selector '[] (Id MXDiskSpaceUsageMetric)
diskSpaceUsageMetricsSelector = mkSelector "diskSpaceUsageMetrics"

-- | @Selector@ for @signpostMetrics@
signpostMetricsSelector :: Selector '[] (Id NSArray)
signpostMetricsSelector = mkSelector "signpostMetrics"

-- | @Selector@ for @metaData@
metaDataSelector :: Selector '[] (Id MXMetaData)
metaDataSelector = mkSelector "metaData"

