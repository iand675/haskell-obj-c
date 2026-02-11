{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreMotion.Internal.Classes (
    module ObjC.CoreMotion.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CMAltimeter ----------

-- | Phantom type for @CMAltimeter@.
data CMAltimeter

instance IsObjCObject (Id CMAltimeter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMAltimeter"

class IsNSObject a => IsCMAltimeter a where
  toCMAltimeter :: a -> Id CMAltimeter

instance IsCMAltimeter (Id CMAltimeter) where
  toCMAltimeter = unsafeCastId

instance IsNSObject (Id CMAltimeter) where
  toNSObject = unsafeCastId

-- ---------- CMAttitude ----------

-- | Phantom type for @CMAttitude@.
data CMAttitude

instance IsObjCObject (Id CMAttitude) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMAttitude"

class IsNSObject a => IsCMAttitude a where
  toCMAttitude :: a -> Id CMAttitude

instance IsCMAttitude (Id CMAttitude) where
  toCMAttitude = unsafeCastId

instance IsNSObject (Id CMAttitude) where
  toNSObject = unsafeCastId

-- ---------- CMBatchedSensorManager ----------

-- | Phantom type for @CMBatchedSensorManager@.
data CMBatchedSensorManager

instance IsObjCObject (Id CMBatchedSensorManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMBatchedSensorManager"

class IsNSObject a => IsCMBatchedSensorManager a where
  toCMBatchedSensorManager :: a -> Id CMBatchedSensorManager

instance IsCMBatchedSensorManager (Id CMBatchedSensorManager) where
  toCMBatchedSensorManager = unsafeCastId

instance IsNSObject (Id CMBatchedSensorManager) where
  toNSObject = unsafeCastId

-- ---------- CMDyskineticSymptomResult ----------

-- | CMDyskineticSymptomResult
--
-- A CMDyskineticSymptomResult object describes the presence and prevalence of dyskinetic symptoms (specifically, choreiform movements) during a one minute result period when subjects wear the Apple Watch on their most affected arm. percentUnlikely + percentLikely = 1.0 Please note dyskinetic symptom measurements are designed for subjects with known presence of chorea in the arm and should not be displayed to users who do not report episodes of dyskinetic symptoms.
-- 
-- Phantom type for @CMDyskineticSymptomResult@.
data CMDyskineticSymptomResult

instance IsObjCObject (Id CMDyskineticSymptomResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMDyskineticSymptomResult"

class IsNSObject a => IsCMDyskineticSymptomResult a where
  toCMDyskineticSymptomResult :: a -> Id CMDyskineticSymptomResult

instance IsCMDyskineticSymptomResult (Id CMDyskineticSymptomResult) where
  toCMDyskineticSymptomResult = unsafeCastId

instance IsNSObject (Id CMDyskineticSymptomResult) where
  toNSObject = unsafeCastId

-- ---------- CMFallDetectionEvent ----------

-- | Fall Detection Event
--
-- CMFallDetectionEventResolution
--
-- This object represents a Fall Detection event and how it was resolved
-- 
-- Phantom type for @CMFallDetectionEvent@.
data CMFallDetectionEvent

instance IsObjCObject (Id CMFallDetectionEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMFallDetectionEvent"

class IsNSObject a => IsCMFallDetectionEvent a where
  toCMFallDetectionEvent :: a -> Id CMFallDetectionEvent

instance IsCMFallDetectionEvent (Id CMFallDetectionEvent) where
  toCMFallDetectionEvent = unsafeCastId

instance IsNSObject (Id CMFallDetectionEvent) where
  toNSObject = unsafeCastId

-- ---------- CMFallDetectionManager ----------

-- | CMFallDetectionManager
--
-- Use CMFallDetectionManager to receive information about Fall Detection events. Not all watch models support Fall Detection, check for availability before creating an instance of CMFallDetectionManager.  CMFallDetectionManager requires an entitlement from Apple. To apply for the entitlement, see Fall Detection Entitlement Request.
--
-- Set the delegate immediately after creating an instance of CMFallDetectionManager. Creating multiple instances of CMFallDetectionManager is not supported and should be avoided.
-- 
-- Phantom type for @CMFallDetectionManager@.
data CMFallDetectionManager

instance IsObjCObject (Id CMFallDetectionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMFallDetectionManager"

class IsNSObject a => IsCMFallDetectionManager a where
  toCMFallDetectionManager :: a -> Id CMFallDetectionManager

instance IsCMFallDetectionManager (Id CMFallDetectionManager) where
  toCMFallDetectionManager = unsafeCastId

instance IsNSObject (Id CMFallDetectionManager) where
  toNSObject = unsafeCastId

-- ---------- CMHeadphoneActivityManager ----------

-- | Phantom type for @CMHeadphoneActivityManager@.
data CMHeadphoneActivityManager

instance IsObjCObject (Id CMHeadphoneActivityManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMHeadphoneActivityManager"

class IsNSObject a => IsCMHeadphoneActivityManager a where
  toCMHeadphoneActivityManager :: a -> Id CMHeadphoneActivityManager

instance IsCMHeadphoneActivityManager (Id CMHeadphoneActivityManager) where
  toCMHeadphoneActivityManager = unsafeCastId

instance IsNSObject (Id CMHeadphoneActivityManager) where
  toNSObject = unsafeCastId

-- ---------- CMHeadphoneMotionManager ----------

-- | Phantom type for @CMHeadphoneMotionManager@.
data CMHeadphoneMotionManager

instance IsObjCObject (Id CMHeadphoneMotionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMHeadphoneMotionManager"

class IsNSObject a => IsCMHeadphoneMotionManager a where
  toCMHeadphoneMotionManager :: a -> Id CMHeadphoneMotionManager

instance IsCMHeadphoneMotionManager (Id CMHeadphoneMotionManager) where
  toCMHeadphoneMotionManager = unsafeCastId

instance IsNSObject (Id CMHeadphoneMotionManager) where
  toNSObject = unsafeCastId

-- ---------- CMLogItem ----------

-- | Phantom type for @CMLogItem@.
data CMLogItem

instance IsObjCObject (Id CMLogItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMLogItem"

class IsNSObject a => IsCMLogItem a where
  toCMLogItem :: a -> Id CMLogItem

instance IsCMLogItem (Id CMLogItem) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMLogItem) where
  toNSObject = unsafeCastId

-- ---------- CMMotionActivityManager ----------

-- | Phantom type for @CMMotionActivityManager@.
data CMMotionActivityManager

instance IsObjCObject (Id CMMotionActivityManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMMotionActivityManager"

class IsNSObject a => IsCMMotionActivityManager a where
  toCMMotionActivityManager :: a -> Id CMMotionActivityManager

instance IsCMMotionActivityManager (Id CMMotionActivityManager) where
  toCMMotionActivityManager = unsafeCastId

instance IsNSObject (Id CMMotionActivityManager) where
  toNSObject = unsafeCastId

-- ---------- CMMotionManager ----------

-- | Phantom type for @CMMotionManager@.
data CMMotionManager

instance IsObjCObject (Id CMMotionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMMotionManager"

class IsNSObject a => IsCMMotionManager a where
  toCMMotionManager :: a -> Id CMMotionManager

instance IsCMMotionManager (Id CMMotionManager) where
  toCMMotionManager = unsafeCastId

instance IsNSObject (Id CMMotionManager) where
  toNSObject = unsafeCastId

-- ---------- CMMovementDisorderManager ----------

-- | CMMovementDisorderManager
--
-- A CMMovementDisorderManager object with methods for persistence and query of movement disorder results.
-- 
-- Phantom type for @CMMovementDisorderManager@.
data CMMovementDisorderManager

instance IsObjCObject (Id CMMovementDisorderManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMMovementDisorderManager"

class IsNSObject a => IsCMMovementDisorderManager a where
  toCMMovementDisorderManager :: a -> Id CMMovementDisorderManager

instance IsCMMovementDisorderManager (Id CMMovementDisorderManager) where
  toCMMovementDisorderManager = unsafeCastId

instance IsNSObject (Id CMMovementDisorderManager) where
  toNSObject = unsafeCastId

-- ---------- CMOdometerData ----------

-- | Phantom type for @CMOdometerData@.
data CMOdometerData

instance IsObjCObject (Id CMOdometerData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMOdometerData"

class IsNSObject a => IsCMOdometerData a where
  toCMOdometerData :: a -> Id CMOdometerData

instance IsCMOdometerData (Id CMOdometerData) where
  toCMOdometerData = unsafeCastId

instance IsNSObject (Id CMOdometerData) where
  toNSObject = unsafeCastId

-- ---------- CMPedometer ----------

-- | Phantom type for @CMPedometer@.
data CMPedometer

instance IsObjCObject (Id CMPedometer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMPedometer"

class IsNSObject a => IsCMPedometer a where
  toCMPedometer :: a -> Id CMPedometer

instance IsCMPedometer (Id CMPedometer) where
  toCMPedometer = unsafeCastId

instance IsNSObject (Id CMPedometer) where
  toNSObject = unsafeCastId

-- ---------- CMPedometerData ----------

-- | Phantom type for @CMPedometerData@.
data CMPedometerData

instance IsObjCObject (Id CMPedometerData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMPedometerData"

class IsNSObject a => IsCMPedometerData a where
  toCMPedometerData :: a -> Id CMPedometerData

instance IsCMPedometerData (Id CMPedometerData) where
  toCMPedometerData = unsafeCastId

instance IsNSObject (Id CMPedometerData) where
  toNSObject = unsafeCastId

-- ---------- CMPedometerEvent ----------

-- | Phantom type for @CMPedometerEvent@.
data CMPedometerEvent

instance IsObjCObject (Id CMPedometerEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMPedometerEvent"

class IsNSObject a => IsCMPedometerEvent a where
  toCMPedometerEvent :: a -> Id CMPedometerEvent

instance IsCMPedometerEvent (Id CMPedometerEvent) where
  toCMPedometerEvent = unsafeCastId

instance IsNSObject (Id CMPedometerEvent) where
  toNSObject = unsafeCastId

-- ---------- CMSensorDataList ----------

-- | Phantom type for @CMSensorDataList@.
data CMSensorDataList

instance IsObjCObject (Id CMSensorDataList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMSensorDataList"

class IsNSObject a => IsCMSensorDataList a where
  toCMSensorDataList :: a -> Id CMSensorDataList

instance IsCMSensorDataList (Id CMSensorDataList) where
  toCMSensorDataList = unsafeCastId

instance IsNSObject (Id CMSensorDataList) where
  toNSObject = unsafeCastId

-- ---------- CMSensorRecorder ----------

-- | Phantom type for @CMSensorRecorder@.
data CMSensorRecorder

instance IsObjCObject (Id CMSensorRecorder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMSensorRecorder"

class IsNSObject a => IsCMSensorRecorder a where
  toCMSensorRecorder :: a -> Id CMSensorRecorder

instance IsCMSensorRecorder (Id CMSensorRecorder) where
  toCMSensorRecorder = unsafeCastId

instance IsNSObject (Id CMSensorRecorder) where
  toNSObject = unsafeCastId

-- ---------- CMStepCounter ----------

-- | Phantom type for @CMStepCounter@.
data CMStepCounter

instance IsObjCObject (Id CMStepCounter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMStepCounter"

class IsNSObject a => IsCMStepCounter a where
  toCMStepCounter :: a -> Id CMStepCounter

instance IsCMStepCounter (Id CMStepCounter) where
  toCMStepCounter = unsafeCastId

instance IsNSObject (Id CMStepCounter) where
  toNSObject = unsafeCastId

-- ---------- CMTremorResult ----------

-- | CMTremorResult
--
-- A CMTremorResult object describes the presence and prevalence of tremor symptoms (specifically, resting tremor) during a one minute result period when subjects wear the Apple Watch on their most affected arm. percentUnknown + percentNoTremor + percentTremorSlight + percentTremorMild + percentTremorModerate + percentTremorStrong = 1.0
-- 
-- Phantom type for @CMTremorResult@.
data CMTremorResult

instance IsObjCObject (Id CMTremorResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMTremorResult"

class IsNSObject a => IsCMTremorResult a where
  toCMTremorResult :: a -> Id CMTremorResult

instance IsCMTremorResult (Id CMTremorResult) where
  toCMTremorResult = unsafeCastId

instance IsNSObject (Id CMTremorResult) where
  toNSObject = unsafeCastId

-- ---------- CMWaterSubmersionEvent ----------

-- | Phantom type for @CMWaterSubmersionEvent@.
data CMWaterSubmersionEvent

instance IsObjCObject (Id CMWaterSubmersionEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMWaterSubmersionEvent"

class IsNSObject a => IsCMWaterSubmersionEvent a where
  toCMWaterSubmersionEvent :: a -> Id CMWaterSubmersionEvent

instance IsCMWaterSubmersionEvent (Id CMWaterSubmersionEvent) where
  toCMWaterSubmersionEvent = unsafeCastId

instance IsNSObject (Id CMWaterSubmersionEvent) where
  toNSObject = unsafeCastId

-- ---------- CMWaterSubmersionManager ----------

-- | Phantom type for @CMWaterSubmersionManager@.
data CMWaterSubmersionManager

instance IsObjCObject (Id CMWaterSubmersionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMWaterSubmersionManager"

class IsNSObject a => IsCMWaterSubmersionManager a where
  toCMWaterSubmersionManager :: a -> Id CMWaterSubmersionManager

instance IsCMWaterSubmersionManager (Id CMWaterSubmersionManager) where
  toCMWaterSubmersionManager = unsafeCastId

instance IsNSObject (Id CMWaterSubmersionManager) where
  toNSObject = unsafeCastId

-- ---------- CMWaterSubmersionMeasurement ----------

-- | Phantom type for @CMWaterSubmersionMeasurement@.
data CMWaterSubmersionMeasurement

instance IsObjCObject (Id CMWaterSubmersionMeasurement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMWaterSubmersionMeasurement"

class IsNSObject a => IsCMWaterSubmersionMeasurement a where
  toCMWaterSubmersionMeasurement :: a -> Id CMWaterSubmersionMeasurement

instance IsCMWaterSubmersionMeasurement (Id CMWaterSubmersionMeasurement) where
  toCMWaterSubmersionMeasurement = unsafeCastId

instance IsNSObject (Id CMWaterSubmersionMeasurement) where
  toNSObject = unsafeCastId

-- ---------- CMWaterTemperature ----------

-- | Phantom type for @CMWaterTemperature@.
data CMWaterTemperature

instance IsObjCObject (Id CMWaterTemperature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMWaterTemperature"

class IsNSObject a => IsCMWaterTemperature a where
  toCMWaterTemperature :: a -> Id CMWaterTemperature

instance IsCMWaterTemperature (Id CMWaterTemperature) where
  toCMWaterTemperature = unsafeCastId

instance IsNSObject (Id CMWaterTemperature) where
  toNSObject = unsafeCastId

-- ---------- CMAbsoluteAltitudeData ----------

-- | Phantom type for @CMAbsoluteAltitudeData@.
data CMAbsoluteAltitudeData

instance IsObjCObject (Id CMAbsoluteAltitudeData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMAbsoluteAltitudeData"

class IsCMLogItem a => IsCMAbsoluteAltitudeData a where
  toCMAbsoluteAltitudeData :: a -> Id CMAbsoluteAltitudeData

instance IsCMAbsoluteAltitudeData (Id CMAbsoluteAltitudeData) where
  toCMAbsoluteAltitudeData = unsafeCastId

instance IsCMLogItem (Id CMAbsoluteAltitudeData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMAbsoluteAltitudeData) where
  toNSObject = unsafeCastId

-- ---------- CMAccelerometerData ----------

-- | Phantom type for @CMAccelerometerData@.
data CMAccelerometerData

instance IsObjCObject (Id CMAccelerometerData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMAccelerometerData"

class IsCMLogItem a => IsCMAccelerometerData a where
  toCMAccelerometerData :: a -> Id CMAccelerometerData

instance IsCMAccelerometerData (Id CMAccelerometerData) where
  toCMAccelerometerData = unsafeCastId

instance IsCMLogItem (Id CMAccelerometerData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMAccelerometerData) where
  toNSObject = unsafeCastId

-- ---------- CMAltitudeData ----------

-- | Phantom type for @CMAltitudeData@.
data CMAltitudeData

instance IsObjCObject (Id CMAltitudeData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMAltitudeData"

class IsCMLogItem a => IsCMAltitudeData a where
  toCMAltitudeData :: a -> Id CMAltitudeData

instance IsCMAltitudeData (Id CMAltitudeData) where
  toCMAltitudeData = unsafeCastId

instance IsCMLogItem (Id CMAltitudeData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMAltitudeData) where
  toNSObject = unsafeCastId

-- ---------- CMAmbientPressureData ----------

-- | Phantom type for @CMAmbientPressureData@.
data CMAmbientPressureData

instance IsObjCObject (Id CMAmbientPressureData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMAmbientPressureData"

class IsCMLogItem a => IsCMAmbientPressureData a where
  toCMAmbientPressureData :: a -> Id CMAmbientPressureData

instance IsCMAmbientPressureData (Id CMAmbientPressureData) where
  toCMAmbientPressureData = unsafeCastId

instance IsCMLogItem (Id CMAmbientPressureData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMAmbientPressureData) where
  toNSObject = unsafeCastId

-- ---------- CMDeviceMotion ----------

-- | Phantom type for @CMDeviceMotion@.
data CMDeviceMotion

instance IsObjCObject (Id CMDeviceMotion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMDeviceMotion"

class IsCMLogItem a => IsCMDeviceMotion a where
  toCMDeviceMotion :: a -> Id CMDeviceMotion

instance IsCMDeviceMotion (Id CMDeviceMotion) where
  toCMDeviceMotion = unsafeCastId

instance IsCMLogItem (Id CMDeviceMotion) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMDeviceMotion) where
  toNSObject = unsafeCastId

-- ---------- CMGyroData ----------

-- | Phantom type for @CMGyroData@.
data CMGyroData

instance IsObjCObject (Id CMGyroData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMGyroData"

class IsCMLogItem a => IsCMGyroData a where
  toCMGyroData :: a -> Id CMGyroData

instance IsCMGyroData (Id CMGyroData) where
  toCMGyroData = unsafeCastId

instance IsCMLogItem (Id CMGyroData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMGyroData) where
  toNSObject = unsafeCastId

-- ---------- CMHighFrequencyHeartRateData ----------

-- | Phantom type for @CMHighFrequencyHeartRateData@.
data CMHighFrequencyHeartRateData

instance IsObjCObject (Id CMHighFrequencyHeartRateData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMHighFrequencyHeartRateData"

class IsCMLogItem a => IsCMHighFrequencyHeartRateData a where
  toCMHighFrequencyHeartRateData :: a -> Id CMHighFrequencyHeartRateData

instance IsCMHighFrequencyHeartRateData (Id CMHighFrequencyHeartRateData) where
  toCMHighFrequencyHeartRateData = unsafeCastId

instance IsCMLogItem (Id CMHighFrequencyHeartRateData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMHighFrequencyHeartRateData) where
  toNSObject = unsafeCastId

-- ---------- CMMagnetometerData ----------

-- | Phantom type for @CMMagnetometerData@.
data CMMagnetometerData

instance IsObjCObject (Id CMMagnetometerData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMMagnetometerData"

class IsCMLogItem a => IsCMMagnetometerData a where
  toCMMagnetometerData :: a -> Id CMMagnetometerData

instance IsCMMagnetometerData (Id CMMagnetometerData) where
  toCMMagnetometerData = unsafeCastId

instance IsCMLogItem (Id CMMagnetometerData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMMagnetometerData) where
  toNSObject = unsafeCastId

-- ---------- CMMotionActivity ----------

-- | Phantom type for @CMMotionActivity@.
data CMMotionActivity

instance IsObjCObject (Id CMMotionActivity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMMotionActivity"

class IsCMLogItem a => IsCMMotionActivity a where
  toCMMotionActivity :: a -> Id CMMotionActivity

instance IsCMMotionActivity (Id CMMotionActivity) where
  toCMMotionActivity = unsafeCastId

instance IsCMLogItem (Id CMMotionActivity) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMMotionActivity) where
  toNSObject = unsafeCastId

-- ---------- CMRotationRateData ----------

-- | Phantom type for @CMRotationRateData@.
data CMRotationRateData

instance IsObjCObject (Id CMRotationRateData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMRotationRateData"

class IsCMLogItem a => IsCMRotationRateData a where
  toCMRotationRateData :: a -> Id CMRotationRateData

instance IsCMRotationRateData (Id CMRotationRateData) where
  toCMRotationRateData = unsafeCastId

instance IsCMLogItem (Id CMRotationRateData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMRotationRateData) where
  toNSObject = unsafeCastId

-- ---------- CMRecordedAccelerometerData ----------

-- | Phantom type for @CMRecordedAccelerometerData@.
data CMRecordedAccelerometerData

instance IsObjCObject (Id CMRecordedAccelerometerData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMRecordedAccelerometerData"

class IsCMAccelerometerData a => IsCMRecordedAccelerometerData a where
  toCMRecordedAccelerometerData :: a -> Id CMRecordedAccelerometerData

instance IsCMRecordedAccelerometerData (Id CMRecordedAccelerometerData) where
  toCMRecordedAccelerometerData = unsafeCastId

instance IsCMAccelerometerData (Id CMRecordedAccelerometerData) where
  toCMAccelerometerData = unsafeCastId

instance IsCMLogItem (Id CMRecordedAccelerometerData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMRecordedAccelerometerData) where
  toNSObject = unsafeCastId

-- ---------- CMRecordedPressureData ----------

-- | Phantom type for @CMRecordedPressureData@.
data CMRecordedPressureData

instance IsObjCObject (Id CMRecordedPressureData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMRecordedPressureData"

class IsCMAmbientPressureData a => IsCMRecordedPressureData a where
  toCMRecordedPressureData :: a -> Id CMRecordedPressureData

instance IsCMRecordedPressureData (Id CMRecordedPressureData) where
  toCMRecordedPressureData = unsafeCastId

instance IsCMAmbientPressureData (Id CMRecordedPressureData) where
  toCMAmbientPressureData = unsafeCastId

instance IsCMLogItem (Id CMRecordedPressureData) where
  toCMLogItem = unsafeCastId

instance IsNSObject (Id CMRecordedPressureData) where
  toNSObject = unsafeCastId

-- ---------- CMRecordedRotationRateData ----------

-- | Phantom type for @CMRecordedRotationRateData@.
data CMRecordedRotationRateData

instance IsObjCObject (Id CMRecordedRotationRateData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMRecordedRotationRateData"

class IsCMRotationRateData a => IsCMRecordedRotationRateData a where
  toCMRecordedRotationRateData :: a -> Id CMRecordedRotationRateData

instance IsCMRecordedRotationRateData (Id CMRecordedRotationRateData) where
  toCMRecordedRotationRateData = unsafeCastId

instance IsCMLogItem (Id CMRecordedRotationRateData) where
  toCMLogItem = unsafeCastId

instance IsCMRotationRateData (Id CMRecordedRotationRateData) where
  toCMRotationRateData = unsafeCastId

instance IsNSObject (Id CMRecordedRotationRateData) where
  toNSObject = unsafeCastId
