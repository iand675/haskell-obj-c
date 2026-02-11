{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MetricKit.Internal.Classes (
    module ObjC.MetricKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MXAverage ----------

-- | MXAverage
--
-- A class representing metric data that is averaged.
-- 
-- Phantom type for @MXAverage@.
data MXAverage

instance IsObjCObject (Id MXAverage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAverage"

class IsNSObject a => IsMXAverage a where
  toMXAverage :: a -> Id MXAverage

instance IsMXAverage (Id MXAverage) where
  toMXAverage = unsafeCastId

instance IsNSObject (Id MXAverage) where
  toNSObject = unsafeCastId

-- ---------- MXBackgroundExitData ----------

-- | MXBackgroundExitData
--
-- A class that encapsulates cumulative application exit metrics when the application is off screen.
--
-- Background exits are terminations that, when unexpected, can impact performance metrics, such as launch time.
--
-- Not all background exits are unexpected. See the documentation for each exit reason for more information.
-- 
-- Phantom type for @MXBackgroundExitData@.
data MXBackgroundExitData

instance IsObjCObject (Id MXBackgroundExitData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXBackgroundExitData"

class IsNSObject a => IsMXBackgroundExitData a where
  toMXBackgroundExitData :: a -> Id MXBackgroundExitData

instance IsMXBackgroundExitData (Id MXBackgroundExitData) where
  toMXBackgroundExitData = unsafeCastId

instance IsNSObject (Id MXBackgroundExitData) where
  toNSObject = unsafeCastId

-- ---------- MXCallStackTree ----------

-- | MXCallStackTree
--
-- A data class that encapsulates call stack trees vended by MetricKit.
--
-- You should use the JSONRepresentation API to generate human readable call stack trees for symbolication off device.
-- 
-- Phantom type for @MXCallStackTree@.
data MXCallStackTree

instance IsObjCObject (Id MXCallStackTree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXCallStackTree"

class IsNSObject a => IsMXCallStackTree a where
  toMXCallStackTree :: a -> Id MXCallStackTree

instance IsMXCallStackTree (Id MXCallStackTree) where
  toMXCallStackTree = unsafeCastId

instance IsNSObject (Id MXCallStackTree) where
  toNSObject = unsafeCastId

-- ---------- MXCrashDiagnosticObjectiveCExceptionReason ----------

-- | MXCrashDiagnosticObjectiveCExceptionReason
--
-- A class that represents Crash exception reason.
--
-- Crash reports that are caused by an uncaught Objective-C NSException can in some cases contain detailed information about the type, name and description of the exception object.                This information is captured in a structured way in a MXCrashDiagnosticObjectiveCExceptionReason object and may have some pieces redacted to avoid exposing sensitive user data.
-- 
-- Phantom type for @MXCrashDiagnosticObjectiveCExceptionReason@.
data MXCrashDiagnosticObjectiveCExceptionReason

instance IsObjCObject (Id MXCrashDiagnosticObjectiveCExceptionReason) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXCrashDiagnosticObjectiveCExceptionReason"

class IsNSObject a => IsMXCrashDiagnosticObjectiveCExceptionReason a where
  toMXCrashDiagnosticObjectiveCExceptionReason :: a -> Id MXCrashDiagnosticObjectiveCExceptionReason

instance IsMXCrashDiagnosticObjectiveCExceptionReason (Id MXCrashDiagnosticObjectiveCExceptionReason) where
  toMXCrashDiagnosticObjectiveCExceptionReason = unsafeCastId

instance IsNSObject (Id MXCrashDiagnosticObjectiveCExceptionReason) where
  toNSObject = unsafeCastId

-- ---------- MXDiagnostic ----------

-- | MXDiagnostic
--
-- An abstract class that describes a diagnostic report vended by MetricKit.
--
-- All supported diagnostics are subclasses of MXDiagnostic.
-- 
-- Phantom type for @MXDiagnostic@.
data MXDiagnostic

instance IsObjCObject (Id MXDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXDiagnostic"

class IsNSObject a => IsMXDiagnostic a where
  toMXDiagnostic :: a -> Id MXDiagnostic

instance IsMXDiagnostic (Id MXDiagnostic) where
  toMXDiagnostic = unsafeCastId

instance IsNSObject (Id MXDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- MXDiagnosticPayload ----------

-- | MXDiagnosticPayload
--
-- A wrapper class which contains a diagnostic payload and associated properties of that payload.
--
-- MXDiagnosticPayload encapsulates currently supported diagnostics that can be vended by MetricKit. Arrays of MXDiangostic subclasses on MXDiagnosticPayload are nullable. If an array of MXDiagnostic subclasses is nil, it indicates that the diagnostics are not available for this payload.
--
-- MXDiagnosticPayload exposes a convenience function, JSONRepresentation, to convert the contents of the payload to a human readable JSON. This should be used in conjunction with other APIs that accept NSData.
--
-- An MXDiagnosticPayload contains diagnostics that cover a 24 hour period of application usage. The properties timeStampBegin and timeStampEnd should be used to determine which time range the payload covers.
--
-- It is possible for an MXDiagnosticPayload to cover regions of time where an application was updated, and thus each MXDiagnostic subclass will contain its own application version string. This is in contrast to MXMetricPayload, where only the latest application version string is included as metadata of the payload. Each MXDiagnostic subclass application version string should be inspected prior to processing.
-- 
-- Phantom type for @MXDiagnosticPayload@.
data MXDiagnosticPayload

instance IsObjCObject (Id MXDiagnosticPayload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXDiagnosticPayload"

class IsNSObject a => IsMXDiagnosticPayload a where
  toMXDiagnosticPayload :: a -> Id MXDiagnosticPayload

instance IsMXDiagnosticPayload (Id MXDiagnosticPayload) where
  toMXDiagnosticPayload = unsafeCastId

instance IsNSObject (Id MXDiagnosticPayload) where
  toNSObject = unsafeCastId

-- ---------- MXForegroundExitData ----------

-- | MXForegroundExitData
--
-- A class that encapsulates cumulative application exit metrics when the application is on screen.
--
-- Foreground exits are user visible terminations that, when unexpected, interrupt usage.
--
-- Not all foreground exits are unexpected. See the documentation for each exit reason for more information.
-- 
-- Phantom type for @MXForegroundExitData@.
data MXForegroundExitData

instance IsObjCObject (Id MXForegroundExitData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXForegroundExitData"

class IsNSObject a => IsMXForegroundExitData a where
  toMXForegroundExitData :: a -> Id MXForegroundExitData

instance IsMXForegroundExitData (Id MXForegroundExitData) where
  toMXForegroundExitData = unsafeCastId

instance IsNSObject (Id MXForegroundExitData) where
  toNSObject = unsafeCastId

-- ---------- MXHistogram ----------

-- | MXHistogram
--
-- A class representing bucketized histogram data.
-- 
-- Phantom type for @MXHistogram@.
data MXHistogram

instance IsObjCObject (Id MXHistogram) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXHistogram"

class IsNSObject a => IsMXHistogram a where
  toMXHistogram :: a -> Id MXHistogram

instance IsMXHistogram (Id MXHistogram) where
  toMXHistogram = unsafeCastId

instance IsNSObject (Id MXHistogram) where
  toNSObject = unsafeCastId

-- ---------- MXHistogramBucket ----------

-- | MXHistogramBucket
--
-- A class that represents a bucket within an MXHistogram
--
-- Histogram buckets are sorted in ascending order.
--
-- Histogram bucket start and end values are exclusive.
-- 
-- Phantom type for @MXHistogramBucket@.
data MXHistogramBucket

instance IsObjCObject (Id MXHistogramBucket) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXHistogramBucket"

class IsNSObject a => IsMXHistogramBucket a where
  toMXHistogramBucket :: a -> Id MXHistogramBucket

instance IsMXHistogramBucket (Id MXHistogramBucket) where
  toMXHistogramBucket = unsafeCastId

instance IsNSObject (Id MXHistogramBucket) where
  toNSObject = unsafeCastId

-- ---------- MXMetaData ----------

-- | MXMetaData
--
-- A class that contains miscellaneous metadata about an associated payload.
-- 
-- Phantom type for @MXMetaData@.
data MXMetaData

instance IsObjCObject (Id MXMetaData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXMetaData"

class IsNSObject a => IsMXMetaData a where
  toMXMetaData :: a -> Id MXMetaData

instance IsMXMetaData (Id MXMetaData) where
  toMXMetaData = unsafeCastId

instance IsNSObject (Id MXMetaData) where
  toNSObject = unsafeCastId

-- ---------- MXMetric ----------

-- | MXMetric
--
-- An abstract class that describes a specific metric vended by MetricKit.
--
-- All supported metrics are subclasses of MXMetric.
-- 
-- Phantom type for @MXMetric@.
data MXMetric

instance IsObjCObject (Id MXMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXMetric"

class IsNSObject a => IsMXMetric a where
  toMXMetric :: a -> Id MXMetric

instance IsMXMetric (Id MXMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXMetric) where
  toNSObject = unsafeCastId

-- ---------- MXMetricManager ----------

-- | MXMetricManager
--
-- An instance of this class can be used to retrieve periodic, aggregated power and performance metrics.
--
-- To receive metrics, clients must acquire a reference to the shared instance of the metric manager and add an eligible MXMetricManagerSubscriber.
--
-- Metrics are not guaranteed to be delivered, but can be expected atleast once per day when conditions permit.
--
-- Subscribers to the metric manager can remove themselves using removeSubscriber:subscriber if they no longer wish to receive metrics.
-- 
-- Phantom type for @MXMetricManager@.
data MXMetricManager

instance IsObjCObject (Id MXMetricManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXMetricManager"

class IsNSObject a => IsMXMetricManager a where
  toMXMetricManager :: a -> Id MXMetricManager

instance IsMXMetricManager (Id MXMetricManager) where
  toMXMetricManager = unsafeCastId

instance IsNSObject (Id MXMetricManager) where
  toNSObject = unsafeCastId

-- ---------- MXMetricPayload ----------

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
-- Phantom type for @MXMetricPayload@.
data MXMetricPayload

instance IsObjCObject (Id MXMetricPayload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXMetricPayload"

class IsNSObject a => IsMXMetricPayload a where
  toMXMetricPayload :: a -> Id MXMetricPayload

instance IsMXMetricPayload (Id MXMetricPayload) where
  toMXMetricPayload = unsafeCastId

instance IsNSObject (Id MXMetricPayload) where
  toNSObject = unsafeCastId

-- ---------- MXSignpostIntervalData ----------

-- | MXSignpostIntervalData
--
-- A class that encapsulates metrics associated with app specific signpost intervals.
--
-- These metrics will be collected and aggregated if the associated signposts were emit using MXSignpost or MXSignpostAnimation APIs
--
-- To limit on-device overhead, the system will automatically limit the number of signposts (emitted using the MetricKit log handle) processed.
--
-- Avoid losing telemetry by limiting usage of signposts (emitted using the MetricKit log handle) to critical sections of code.
-- 
-- Phantom type for @MXSignpostIntervalData@.
data MXSignpostIntervalData

instance IsObjCObject (Id MXSignpostIntervalData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXSignpostIntervalData"

class IsNSObject a => IsMXSignpostIntervalData a where
  toMXSignpostIntervalData :: a -> Id MXSignpostIntervalData

instance IsMXSignpostIntervalData (Id MXSignpostIntervalData) where
  toMXSignpostIntervalData = unsafeCastId

instance IsNSObject (Id MXSignpostIntervalData) where
  toNSObject = unsafeCastId

-- ---------- MXSignpostRecord ----------

-- | MXSignpostRecord
--
-- A class that represents a record of signpost instance.
--
-- Signpost instances are either Signpost intervals or events and MXSignpostRecord captures information reagarding such signpost instances
-- 
-- Phantom type for @MXSignpostRecord@.
data MXSignpostRecord

instance IsObjCObject (Id MXSignpostRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXSignpostRecord"

class IsNSObject a => IsMXSignpostRecord a where
  toMXSignpostRecord :: a -> Id MXSignpostRecord

instance IsMXSignpostRecord (Id MXSignpostRecord) where
  toMXSignpostRecord = unsafeCastId

instance IsNSObject (Id MXSignpostRecord) where
  toNSObject = unsafeCastId

-- ---------- MXAppLaunchDiagnostic ----------

-- | MXAppLaunchDiagnostic
--
-- An MXDiagnostic subclass that encapsulates app launch diagnostic reports.
-- 
-- Phantom type for @MXAppLaunchDiagnostic@.
data MXAppLaunchDiagnostic

instance IsObjCObject (Id MXAppLaunchDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAppLaunchDiagnostic"

class IsMXDiagnostic a => IsMXAppLaunchDiagnostic a where
  toMXAppLaunchDiagnostic :: a -> Id MXAppLaunchDiagnostic

instance IsMXAppLaunchDiagnostic (Id MXAppLaunchDiagnostic) where
  toMXAppLaunchDiagnostic = unsafeCastId

instance IsMXDiagnostic (Id MXAppLaunchDiagnostic) where
  toMXDiagnostic = unsafeCastId

instance IsNSObject (Id MXAppLaunchDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- MXCPUExceptionDiagnostic ----------

-- | MXCPUExceptionDiagnostic
--
-- An MXDiagnostic subclass that encapsulates CPU exception diagnostic reports.
--
-- CPU exceptions occur when your application consumes excessive CPU time in a short period of time.
--
-- CPU exceptions can be both fatal and non-fatal to your application.
-- 
-- Phantom type for @MXCPUExceptionDiagnostic@.
data MXCPUExceptionDiagnostic

instance IsObjCObject (Id MXCPUExceptionDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXCPUExceptionDiagnostic"

class IsMXDiagnostic a => IsMXCPUExceptionDiagnostic a where
  toMXCPUExceptionDiagnostic :: a -> Id MXCPUExceptionDiagnostic

instance IsMXCPUExceptionDiagnostic (Id MXCPUExceptionDiagnostic) where
  toMXCPUExceptionDiagnostic = unsafeCastId

instance IsMXDiagnostic (Id MXCPUExceptionDiagnostic) where
  toMXDiagnostic = unsafeCastId

instance IsNSObject (Id MXCPUExceptionDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- MXCrashDiagnostic ----------

-- | MXCrashDiagnostic
--
-- An MXDiagnostic subclass that encapsulates crash reports.
--
-- See "Analyzing a Crash Report" for more information on crash diagnostics.
-- 
-- Phantom type for @MXCrashDiagnostic@.
data MXCrashDiagnostic

instance IsObjCObject (Id MXCrashDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXCrashDiagnostic"

class IsMXDiagnostic a => IsMXCrashDiagnostic a where
  toMXCrashDiagnostic :: a -> Id MXCrashDiagnostic

instance IsMXCrashDiagnostic (Id MXCrashDiagnostic) where
  toMXCrashDiagnostic = unsafeCastId

instance IsMXDiagnostic (Id MXCrashDiagnostic) where
  toMXDiagnostic = unsafeCastId

instance IsNSObject (Id MXCrashDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- MXDiskWriteExceptionDiagnostic ----------

-- | MXDiskWriteExceptionDiagnostic
--
-- An MXDiagnostic subclass that encapsulates disk write exception reports.
--
-- Disk write exceptions occur when your application writes data excessively to disk.
-- 
-- Phantom type for @MXDiskWriteExceptionDiagnostic@.
data MXDiskWriteExceptionDiagnostic

instance IsObjCObject (Id MXDiskWriteExceptionDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXDiskWriteExceptionDiagnostic"

class IsMXDiagnostic a => IsMXDiskWriteExceptionDiagnostic a where
  toMXDiskWriteExceptionDiagnostic :: a -> Id MXDiskWriteExceptionDiagnostic

instance IsMXDiskWriteExceptionDiagnostic (Id MXDiskWriteExceptionDiagnostic) where
  toMXDiskWriteExceptionDiagnostic = unsafeCastId

instance IsMXDiagnostic (Id MXDiskWriteExceptionDiagnostic) where
  toMXDiagnostic = unsafeCastId

instance IsNSObject (Id MXDiskWriteExceptionDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- MXHangDiagnostic ----------

-- | MXHangDiagnostic
--
-- An MXDiagnostic subclass that encapsulates hang diagnostic reports.
--
-- Applications are considered to be "hanging" when they are unable to handle user input responsively.
--
-- This generally occurs when your applications main thread is blocked.
-- 
-- Phantom type for @MXHangDiagnostic@.
data MXHangDiagnostic

instance IsObjCObject (Id MXHangDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXHangDiagnostic"

class IsMXDiagnostic a => IsMXHangDiagnostic a where
  toMXHangDiagnostic :: a -> Id MXHangDiagnostic

instance IsMXHangDiagnostic (Id MXHangDiagnostic) where
  toMXHangDiagnostic = unsafeCastId

instance IsMXDiagnostic (Id MXHangDiagnostic) where
  toMXDiagnostic = unsafeCastId

instance IsNSObject (Id MXHangDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- MXAnimationMetric ----------

-- | MXAnimationMetric
--
-- An MXMetric subclass that encapsulates app animation metrics.
-- 
-- Phantom type for @MXAnimationMetric@.
data MXAnimationMetric

instance IsObjCObject (Id MXAnimationMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAnimationMetric"

class IsMXMetric a => IsMXAnimationMetric a where
  toMXAnimationMetric :: a -> Id MXAnimationMetric

instance IsMXAnimationMetric (Id MXAnimationMetric) where
  toMXAnimationMetric = unsafeCastId

instance IsMXMetric (Id MXAnimationMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXAnimationMetric) where
  toNSObject = unsafeCastId

-- ---------- MXAppExitMetric ----------

-- | MXAppExitMetric
--
-- A class that encapsulates application exit metrics for both on screen and off screen exits.
--
-- Application exits can be expected, such as when the application is killed in the app switcher by the user, or unexpected, such as when a runtime error occurs.
--
-- Minimizing unexpected exits and maximizing expected exits can improve performance and reliability of your application.
-- 
-- Phantom type for @MXAppExitMetric@.
data MXAppExitMetric

instance IsObjCObject (Id MXAppExitMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAppExitMetric"

class IsMXMetric a => IsMXAppExitMetric a where
  toMXAppExitMetric :: a -> Id MXAppExitMetric

instance IsMXAppExitMetric (Id MXAppExitMetric) where
  toMXAppExitMetric = unsafeCastId

instance IsMXMetric (Id MXAppExitMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXAppExitMetric) where
  toNSObject = unsafeCastId

-- ---------- MXAppLaunchMetric ----------

-- | MXAppLaunchMetric
--
-- An MXMetric subclass that encapsulates app launch metrics.
-- 
-- Phantom type for @MXAppLaunchMetric@.
data MXAppLaunchMetric

instance IsObjCObject (Id MXAppLaunchMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAppLaunchMetric"

class IsMXMetric a => IsMXAppLaunchMetric a where
  toMXAppLaunchMetric :: a -> Id MXAppLaunchMetric

instance IsMXAppLaunchMetric (Id MXAppLaunchMetric) where
  toMXAppLaunchMetric = unsafeCastId

instance IsMXMetric (Id MXAppLaunchMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXAppLaunchMetric) where
  toNSObject = unsafeCastId

-- ---------- MXAppResponsivenessMetric ----------

-- | MXAppResponsivenessMetric
--
-- An MXMetric subclass that encapsulates app responsiveness metrics.
-- 
-- Phantom type for @MXAppResponsivenessMetric@.
data MXAppResponsivenessMetric

instance IsObjCObject (Id MXAppResponsivenessMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAppResponsivenessMetric"

class IsMXMetric a => IsMXAppResponsivenessMetric a where
  toMXAppResponsivenessMetric :: a -> Id MXAppResponsivenessMetric

instance IsMXAppResponsivenessMetric (Id MXAppResponsivenessMetric) where
  toMXAppResponsivenessMetric = unsafeCastId

instance IsMXMetric (Id MXAppResponsivenessMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXAppResponsivenessMetric) where
  toNSObject = unsafeCastId

-- ---------- MXAppRunTimeMetric ----------

-- | MXAppRunTimeMetric
--
-- An MXMetric subclass that encapsulates app runtime metrics.
--
-- Runtime metrics describe application time spent running in different modes, such as audio, location, etc.
-- 
-- Phantom type for @MXAppRunTimeMetric@.
data MXAppRunTimeMetric

instance IsObjCObject (Id MXAppRunTimeMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXAppRunTimeMetric"

class IsMXMetric a => IsMXAppRunTimeMetric a where
  toMXAppRunTimeMetric :: a -> Id MXAppRunTimeMetric

instance IsMXAppRunTimeMetric (Id MXAppRunTimeMetric) where
  toMXAppRunTimeMetric = unsafeCastId

instance IsMXMetric (Id MXAppRunTimeMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXAppRunTimeMetric) where
  toNSObject = unsafeCastId

-- ---------- MXCPUMetric ----------

-- | MXCPUMetric
--
-- An MXMetric subclass that encapsulates CPU metrics.
-- 
-- Phantom type for @MXCPUMetric@.
data MXCPUMetric

instance IsObjCObject (Id MXCPUMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXCPUMetric"

class IsMXMetric a => IsMXCPUMetric a where
  toMXCPUMetric :: a -> Id MXCPUMetric

instance IsMXCPUMetric (Id MXCPUMetric) where
  toMXCPUMetric = unsafeCastId

instance IsMXMetric (Id MXCPUMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXCPUMetric) where
  toNSObject = unsafeCastId

-- ---------- MXCellularConditionMetric ----------

-- | MXCellConditionMetric
--
-- An MXMetric subclass that encapsulates cellular condition metrics.
-- 
-- Phantom type for @MXCellularConditionMetric@.
data MXCellularConditionMetric

instance IsObjCObject (Id MXCellularConditionMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXCellularConditionMetric"

class IsMXMetric a => IsMXCellularConditionMetric a where
  toMXCellularConditionMetric :: a -> Id MXCellularConditionMetric

instance IsMXCellularConditionMetric (Id MXCellularConditionMetric) where
  toMXCellularConditionMetric = unsafeCastId

instance IsMXMetric (Id MXCellularConditionMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXCellularConditionMetric) where
  toNSObject = unsafeCastId

-- ---------- MXDiskIOMetric ----------

-- | MXDiskIOMetric
--
-- An MXMetric subclass that encapsulates disk IO metrics.
-- 
-- Phantom type for @MXDiskIOMetric@.
data MXDiskIOMetric

instance IsObjCObject (Id MXDiskIOMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXDiskIOMetric"

class IsMXMetric a => IsMXDiskIOMetric a where
  toMXDiskIOMetric :: a -> Id MXDiskIOMetric

instance IsMXDiskIOMetric (Id MXDiskIOMetric) where
  toMXDiskIOMetric = unsafeCastId

instance IsMXMetric (Id MXDiskIOMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXDiskIOMetric) where
  toNSObject = unsafeCastId

-- ---------- MXDiskSpaceUsageMetric ----------

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
-- Phantom type for @MXDiskSpaceUsageMetric@.
data MXDiskSpaceUsageMetric

instance IsObjCObject (Id MXDiskSpaceUsageMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXDiskSpaceUsageMetric"

class IsMXMetric a => IsMXDiskSpaceUsageMetric a where
  toMXDiskSpaceUsageMetric :: a -> Id MXDiskSpaceUsageMetric

instance IsMXDiskSpaceUsageMetric (Id MXDiskSpaceUsageMetric) where
  toMXDiskSpaceUsageMetric = unsafeCastId

instance IsMXMetric (Id MXDiskSpaceUsageMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXDiskSpaceUsageMetric) where
  toNSObject = unsafeCastId

-- ---------- MXDisplayMetric ----------

-- | MXDisplayMetric
--
-- An MXMetric subclass that encapsulates display metrics.
-- 
-- Phantom type for @MXDisplayMetric@.
data MXDisplayMetric

instance IsObjCObject (Id MXDisplayMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXDisplayMetric"

class IsMXMetric a => IsMXDisplayMetric a where
  toMXDisplayMetric :: a -> Id MXDisplayMetric

instance IsMXDisplayMetric (Id MXDisplayMetric) where
  toMXDisplayMetric = unsafeCastId

instance IsMXMetric (Id MXDisplayMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXDisplayMetric) where
  toNSObject = unsafeCastId

-- ---------- MXGPUMetric ----------

-- | MXGPUMetric
--
-- An MXMetric subclass that encapsulates GPU metrics.
-- 
-- Phantom type for @MXGPUMetric@.
data MXGPUMetric

instance IsObjCObject (Id MXGPUMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXGPUMetric"

class IsMXMetric a => IsMXGPUMetric a where
  toMXGPUMetric :: a -> Id MXGPUMetric

instance IsMXGPUMetric (Id MXGPUMetric) where
  toMXGPUMetric = unsafeCastId

instance IsMXMetric (Id MXGPUMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXGPUMetric) where
  toNSObject = unsafeCastId

-- ---------- MXLocationActivityMetric ----------

-- | MXLocationActivityMetric
--
-- An MXMetric subclass that encapsulates location metrics
--
-- The metrics contained in this class describe properties of location activity. See MXAppRunTimeMetric for time spent performing location activities.
-- 
-- Phantom type for @MXLocationActivityMetric@.
data MXLocationActivityMetric

instance IsObjCObject (Id MXLocationActivityMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXLocationActivityMetric"

class IsMXMetric a => IsMXLocationActivityMetric a where
  toMXLocationActivityMetric :: a -> Id MXLocationActivityMetric

instance IsMXLocationActivityMetric (Id MXLocationActivityMetric) where
  toMXLocationActivityMetric = unsafeCastId

instance IsMXMetric (Id MXLocationActivityMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXLocationActivityMetric) where
  toNSObject = unsafeCastId

-- ---------- MXMemoryMetric ----------

-- | MXMemoryMetric
--
-- An MXMetric subclass that encapsulates memory metrics.
-- 
-- Phantom type for @MXMemoryMetric@.
data MXMemoryMetric

instance IsObjCObject (Id MXMemoryMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXMemoryMetric"

class IsMXMetric a => IsMXMemoryMetric a where
  toMXMemoryMetric :: a -> Id MXMemoryMetric

instance IsMXMemoryMetric (Id MXMemoryMetric) where
  toMXMemoryMetric = unsafeCastId

instance IsMXMetric (Id MXMemoryMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXMemoryMetric) where
  toNSObject = unsafeCastId

-- ---------- MXNetworkTransferMetric ----------

-- | MXNetworkTransferMetric
--
-- An MXMetric subclass that encapsulates network transfer metrics
-- 
-- Phantom type for @MXNetworkTransferMetric@.
data MXNetworkTransferMetric

instance IsObjCObject (Id MXNetworkTransferMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXNetworkTransferMetric"

class IsMXMetric a => IsMXNetworkTransferMetric a where
  toMXNetworkTransferMetric :: a -> Id MXNetworkTransferMetric

instance IsMXNetworkTransferMetric (Id MXNetworkTransferMetric) where
  toMXNetworkTransferMetric = unsafeCastId

instance IsMXMetric (Id MXNetworkTransferMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXNetworkTransferMetric) where
  toNSObject = unsafeCastId

-- ---------- MXSignpostMetric ----------

-- | MXSignpostMetric
--
-- An MXMetric subclass that encapsulates signpost metrics.
--
-- Signposts emit using the os_log_t generated by makeLogHandleWithCategory: in MXMetricManger can be flagged for aggregation and reported back in MXMetricPayload.
-- 
-- Phantom type for @MXSignpostMetric@.
data MXSignpostMetric

instance IsObjCObject (Id MXSignpostMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXSignpostMetric"

class IsMXMetric a => IsMXSignpostMetric a where
  toMXSignpostMetric :: a -> Id MXSignpostMetric

instance IsMXSignpostMetric (Id MXSignpostMetric) where
  toMXSignpostMetric = unsafeCastId

instance IsMXMetric (Id MXSignpostMetric) where
  toMXMetric = unsafeCastId

instance IsNSObject (Id MXSignpostMetric) where
  toNSObject = unsafeCastId

-- ---------- MXUnitAveragePixelLuminance ----------

-- | MXUnitAveragePixelLuminance
--
-- An NSUnit subclass representing the linear space Display APL.
-- 
-- Phantom type for @MXUnitAveragePixelLuminance@.
data MXUnitAveragePixelLuminance

instance IsObjCObject (Id MXUnitAveragePixelLuminance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXUnitAveragePixelLuminance"

class IsNSDimension a => IsMXUnitAveragePixelLuminance a where
  toMXUnitAveragePixelLuminance :: a -> Id MXUnitAveragePixelLuminance

instance IsMXUnitAveragePixelLuminance (Id MXUnitAveragePixelLuminance) where
  toMXUnitAveragePixelLuminance = unsafeCastId

instance IsNSDimension (Id MXUnitAveragePixelLuminance) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id MXUnitAveragePixelLuminance) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id MXUnitAveragePixelLuminance) where
  toNSUnit = unsafeCastId

-- ---------- MXUnitSignalBars ----------

-- | MXUnitSignalBars
--
-- An NSUnit subclass representing the number of signal bars for signal strength.
-- 
-- Phantom type for @MXUnitSignalBars@.
data MXUnitSignalBars

instance IsObjCObject (Id MXUnitSignalBars) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MXUnitSignalBars"

class IsNSDimension a => IsMXUnitSignalBars a where
  toMXUnitSignalBars :: a -> Id MXUnitSignalBars

instance IsMXUnitSignalBars (Id MXUnitSignalBars) where
  toMXUnitSignalBars = unsafeCastId

instance IsNSDimension (Id MXUnitSignalBars) where
  toNSDimension = unsafeCastId

instance IsNSObject (Id MXUnitSignalBars) where
  toNSObject = unsafeCastId

instance IsNSUnit (Id MXUnitSignalBars) where
  toNSUnit = unsafeCastId
