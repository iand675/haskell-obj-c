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
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector
  , latestApplicationVersionSelector
  , includesMultipleApplicationVersionsSelector
  , timeStampBeginSelector
  , timeStampEndSelector
  , cpuMetricsSelector
  , gpuMetricsSelector
  , cellularConditionMetricsSelector
  , applicationTimeMetricsSelector
  , locationActivityMetricsSelector
  , networkTransferMetricsSelector
  , applicationLaunchMetricsSelector
  , applicationResponsivenessMetricsSelector
  , diskIOMetricsSelector
  , memoryMetricsSelector
  , displayMetricsSelector
  , animationMetricsSelector
  , applicationExitMetricsSelector
  , diskSpaceUsageMetricsSelector
  , signpostMetricsSelector
  , metaDataSelector


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

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this payload.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSData)
jsonRepresentation mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this payload.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- DictionaryRepresentation@
dictionaryRepresentation :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSDictionary)
dictionaryRepresentation mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "DictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | latestApplicationVersion
--
-- An NSString representation of the application version from which this payload was generated.
--
-- If the application version was changed during the aggregation of this data, this value will reflect the latest application version at the time of retrieval.
--
-- ObjC selector: @- latestApplicationVersion@
latestApplicationVersion :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSString)
latestApplicationVersion mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "latestApplicationVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

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
includesMultipleApplicationVersions mxMetricPayload  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mxMetricPayload (mkSelector "includesMultipleApplicationVersions") retCULong []

-- | timeStampBegin
--
-- An NSDate object that indicates the time which the payload was generated.
--
-- ObjC selector: @- timeStampBegin@
timeStampBegin :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSDate)
timeStampBegin mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "timeStampBegin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | timeStampEnd
--
-- An NSDate object that indicates the time which the payload was generated.
--
-- ObjC selector: @- timeStampEnd@
timeStampEnd :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSDate)
timeStampEnd mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "timeStampEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cpuMetrics
--
-- An object containing CPU metrics for this application.
--
-- ObjC selector: @- cpuMetrics@
cpuMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXCPUMetric)
cpuMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "cpuMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gpuMetrics
--
-- An object containing GPU metrics for this application.
--
-- ObjC selector: @- gpuMetrics@
gpuMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXGPUMetric)
gpuMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "gpuMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cellularConditionMetrics
--
-- An object containing a cellular condition metrics for this application.
--
-- ObjC selector: @- cellularConditionMetrics@
cellularConditionMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXCellularConditionMetric)
cellularConditionMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "cellularConditionMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationTimeMetrics
--
-- An object containing running mode metrics for this application.
--
-- ObjC selector: @- applicationTimeMetrics@
applicationTimeMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppRunTimeMetric)
applicationTimeMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "applicationTimeMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | locationActivityMetrics
--
-- An object containing location activity metrics for this application.
--
-- ObjC selector: @- locationActivityMetrics@
locationActivityMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXLocationActivityMetric)
locationActivityMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "locationActivityMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | networkTransferMetrics
--
-- An object containing network transfer metrics for this application.
--
-- ObjC selector: @- networkTransferMetrics@
networkTransferMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXNetworkTransferMetric)
networkTransferMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "networkTransferMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationLaunchMetrics
--
-- An object containing launch metrics for this application.
--
-- ObjC selector: @- applicationLaunchMetrics@
applicationLaunchMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppLaunchMetric)
applicationLaunchMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "applicationLaunchMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationResponsivenessMetrics
--
-- An object containing hang metrics for this application.
--
-- ObjC selector: @- applicationResponsivenessMetrics@
applicationResponsivenessMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppResponsivenessMetric)
applicationResponsivenessMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "applicationResponsivenessMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | diskIOMetrics
--
-- An object containing disk IO metrics for this application.
--
-- ObjC selector: @- diskIOMetrics@
diskIOMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXDiskIOMetric)
diskIOMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "diskIOMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | memoryMetrics
--
-- An object containing memory metrics for this application.
--
-- ObjC selector: @- memoryMetrics@
memoryMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXMemoryMetric)
memoryMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "memoryMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | displayMetrics
--
-- An object containing display metrics for this application.
--
-- ObjC selector: @- displayMetrics@
displayMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXDisplayMetric)
displayMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "displayMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | animationMetrics
--
-- An object containing animation metrics for this application.
--
-- ObjC selector: @- animationMetrics@
animationMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAnimationMetric)
animationMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "animationMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationExitMetrics
--
-- An object containing exit metrics for this application.
--
-- ObjC selector: @- applicationExitMetrics@
applicationExitMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXAppExitMetric)
applicationExitMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "applicationExitMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | diskSpaceUsageMetrics
--
-- An object containing disk space usage metrics for this application.
--
-- ObjC selector: @- diskSpaceUsageMetrics@
diskSpaceUsageMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXDiskSpaceUsageMetric)
diskSpaceUsageMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "diskSpaceUsageMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | signpostMetrics
--
-- An array containing signpost metrics for this application.
--
-- ObjC selector: @- signpostMetrics@
signpostMetrics :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id NSArray)
signpostMetrics mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "signpostMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metaData
--
-- An object containing extra metadata for this payload.
--
-- ObjC selector: @- metaData@
metaData :: IsMXMetricPayload mxMetricPayload => mxMetricPayload -> IO (Id MXMetaData)
metaData mxMetricPayload  =
  sendMsg mxMetricPayload (mkSelector "metaData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @DictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "DictionaryRepresentation"

-- | @Selector@ for @latestApplicationVersion@
latestApplicationVersionSelector :: Selector
latestApplicationVersionSelector = mkSelector "latestApplicationVersion"

-- | @Selector@ for @includesMultipleApplicationVersions@
includesMultipleApplicationVersionsSelector :: Selector
includesMultipleApplicationVersionsSelector = mkSelector "includesMultipleApplicationVersions"

-- | @Selector@ for @timeStampBegin@
timeStampBeginSelector :: Selector
timeStampBeginSelector = mkSelector "timeStampBegin"

-- | @Selector@ for @timeStampEnd@
timeStampEndSelector :: Selector
timeStampEndSelector = mkSelector "timeStampEnd"

-- | @Selector@ for @cpuMetrics@
cpuMetricsSelector :: Selector
cpuMetricsSelector = mkSelector "cpuMetrics"

-- | @Selector@ for @gpuMetrics@
gpuMetricsSelector :: Selector
gpuMetricsSelector = mkSelector "gpuMetrics"

-- | @Selector@ for @cellularConditionMetrics@
cellularConditionMetricsSelector :: Selector
cellularConditionMetricsSelector = mkSelector "cellularConditionMetrics"

-- | @Selector@ for @applicationTimeMetrics@
applicationTimeMetricsSelector :: Selector
applicationTimeMetricsSelector = mkSelector "applicationTimeMetrics"

-- | @Selector@ for @locationActivityMetrics@
locationActivityMetricsSelector :: Selector
locationActivityMetricsSelector = mkSelector "locationActivityMetrics"

-- | @Selector@ for @networkTransferMetrics@
networkTransferMetricsSelector :: Selector
networkTransferMetricsSelector = mkSelector "networkTransferMetrics"

-- | @Selector@ for @applicationLaunchMetrics@
applicationLaunchMetricsSelector :: Selector
applicationLaunchMetricsSelector = mkSelector "applicationLaunchMetrics"

-- | @Selector@ for @applicationResponsivenessMetrics@
applicationResponsivenessMetricsSelector :: Selector
applicationResponsivenessMetricsSelector = mkSelector "applicationResponsivenessMetrics"

-- | @Selector@ for @diskIOMetrics@
diskIOMetricsSelector :: Selector
diskIOMetricsSelector = mkSelector "diskIOMetrics"

-- | @Selector@ for @memoryMetrics@
memoryMetricsSelector :: Selector
memoryMetricsSelector = mkSelector "memoryMetrics"

-- | @Selector@ for @displayMetrics@
displayMetricsSelector :: Selector
displayMetricsSelector = mkSelector "displayMetrics"

-- | @Selector@ for @animationMetrics@
animationMetricsSelector :: Selector
animationMetricsSelector = mkSelector "animationMetrics"

-- | @Selector@ for @applicationExitMetrics@
applicationExitMetricsSelector :: Selector
applicationExitMetricsSelector = mkSelector "applicationExitMetrics"

-- | @Selector@ for @diskSpaceUsageMetrics@
diskSpaceUsageMetricsSelector :: Selector
diskSpaceUsageMetricsSelector = mkSelector "diskSpaceUsageMetrics"

-- | @Selector@ for @signpostMetrics@
signpostMetricsSelector :: Selector
signpostMetricsSelector = mkSelector "signpostMetrics"

-- | @Selector@ for @metaData@
metaDataSelector :: Selector
metaDataSelector = mkSelector "metaData"

