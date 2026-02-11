{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreLocation.Internal.Classes (
    module ObjC.CoreLocation.Internal.Classes,
    module ObjC.Contacts.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- CLBackgroundActivitySession ----------

-- | Phantom type for @CLBackgroundActivitySession@.
data CLBackgroundActivitySession

instance IsObjCObject (Id CLBackgroundActivitySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLBackgroundActivitySession"

class IsNSObject a => IsCLBackgroundActivitySession a where
  toCLBackgroundActivitySession :: a -> Id CLBackgroundActivitySession

instance IsCLBackgroundActivitySession (Id CLBackgroundActivitySession) where
  toCLBackgroundActivitySession = unsafeCastId

instance IsNSObject (Id CLBackgroundActivitySession) where
  toNSObject = unsafeCastId

-- ---------- CLBackgroundActivitySessionDiagnostic ----------

-- | Phantom type for @CLBackgroundActivitySessionDiagnostic@.
data CLBackgroundActivitySessionDiagnostic

instance IsObjCObject (Id CLBackgroundActivitySessionDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLBackgroundActivitySessionDiagnostic"

class IsNSObject a => IsCLBackgroundActivitySessionDiagnostic a where
  toCLBackgroundActivitySessionDiagnostic :: a -> Id CLBackgroundActivitySessionDiagnostic

instance IsCLBackgroundActivitySessionDiagnostic (Id CLBackgroundActivitySessionDiagnostic) where
  toCLBackgroundActivitySessionDiagnostic = unsafeCastId

instance IsNSObject (Id CLBackgroundActivitySessionDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- CLBeacon ----------

-- | Phantom type for @CLBeacon@.
data CLBeacon

instance IsObjCObject (Id CLBeacon) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLBeacon"

class IsNSObject a => IsCLBeacon a where
  toCLBeacon :: a -> Id CLBeacon

instance IsCLBeacon (Id CLBeacon) where
  toCLBeacon = unsafeCastId

instance IsNSObject (Id CLBeacon) where
  toNSObject = unsafeCastId

-- ---------- CLCondition ----------

-- | Phantom type for @CLCondition@.
data CLCondition

instance IsObjCObject (Id CLCondition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLCondition"

class IsNSObject a => IsCLCondition a where
  toCLCondition :: a -> Id CLCondition

instance IsCLCondition (Id CLCondition) where
  toCLCondition = unsafeCastId

instance IsNSObject (Id CLCondition) where
  toNSObject = unsafeCastId

-- ---------- CLFloor ----------

-- | Phantom type for @CLFloor@.
data CLFloor

instance IsObjCObject (Id CLFloor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLFloor"

class IsNSObject a => IsCLFloor a where
  toCLFloor :: a -> Id CLFloor

instance IsCLFloor (Id CLFloor) where
  toCLFloor = unsafeCastId

instance IsNSObject (Id CLFloor) where
  toNSObject = unsafeCastId

-- ---------- CLGeocoder ----------

-- | Phantom type for @CLGeocoder@.
data CLGeocoder

instance IsObjCObject (Id CLGeocoder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLGeocoder"

class IsNSObject a => IsCLGeocoder a where
  toCLGeocoder :: a -> Id CLGeocoder

instance IsCLGeocoder (Id CLGeocoder) where
  toCLGeocoder = unsafeCastId

instance IsNSObject (Id CLGeocoder) where
  toNSObject = unsafeCastId

-- ---------- CLHeading ----------

-- | Phantom type for @CLHeading@.
data CLHeading

instance IsObjCObject (Id CLHeading) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLHeading"

class IsNSObject a => IsCLHeading a where
  toCLHeading :: a -> Id CLHeading

instance IsCLHeading (Id CLHeading) where
  toCLHeading = unsafeCastId

instance IsNSObject (Id CLHeading) where
  toNSObject = unsafeCastId

-- ---------- CLLocation ----------

-- | Phantom type for @CLLocation@.
data CLLocation

instance IsObjCObject (Id CLLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLLocation"

class IsNSObject a => IsCLLocation a where
  toCLLocation :: a -> Id CLLocation

instance IsCLLocation (Id CLLocation) where
  toCLLocation = unsafeCastId

instance IsNSObject (Id CLLocation) where
  toNSObject = unsafeCastId

-- ---------- CLLocationManager ----------

-- | Phantom type for @CLLocationManager@.
data CLLocationManager

instance IsObjCObject (Id CLLocationManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLLocationManager"

class IsNSObject a => IsCLLocationManager a where
  toCLLocationManager :: a -> Id CLLocationManager

instance IsCLLocationManager (Id CLLocationManager) where
  toCLLocationManager = unsafeCastId

instance IsNSObject (Id CLLocationManager) where
  toNSObject = unsafeCastId

-- ---------- CLLocationSourceInformation ----------

-- | Phantom type for @CLLocationSourceInformation@.
data CLLocationSourceInformation

instance IsObjCObject (Id CLLocationSourceInformation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLLocationSourceInformation"

class IsNSObject a => IsCLLocationSourceInformation a where
  toCLLocationSourceInformation :: a -> Id CLLocationSourceInformation

instance IsCLLocationSourceInformation (Id CLLocationSourceInformation) where
  toCLLocationSourceInformation = unsafeCastId

instance IsNSObject (Id CLLocationSourceInformation) where
  toNSObject = unsafeCastId

-- ---------- CLLocationUpdater ----------

-- | Phantom type for @CLLocationUpdater@.
data CLLocationUpdater

instance IsObjCObject (Id CLLocationUpdater) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLLocationUpdater"

class IsNSObject a => IsCLLocationUpdater a where
  toCLLocationUpdater :: a -> Id CLLocationUpdater

instance IsCLLocationUpdater (Id CLLocationUpdater) where
  toCLLocationUpdater = unsafeCastId

instance IsNSObject (Id CLLocationUpdater) where
  toNSObject = unsafeCastId

-- ---------- CLMonitor ----------

-- | Phantom type for @CLMonitor@.
data CLMonitor

instance IsObjCObject (Id CLMonitor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLMonitor"

class IsNSObject a => IsCLMonitor a where
  toCLMonitor :: a -> Id CLMonitor

instance IsCLMonitor (Id CLMonitor) where
  toCLMonitor = unsafeCastId

instance IsNSObject (Id CLMonitor) where
  toNSObject = unsafeCastId

-- ---------- CLMonitorConfiguration ----------

-- | Phantom type for @CLMonitorConfiguration@.
data CLMonitorConfiguration

instance IsObjCObject (Id CLMonitorConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLMonitorConfiguration"

class IsNSObject a => IsCLMonitorConfiguration a where
  toCLMonitorConfiguration :: a -> Id CLMonitorConfiguration

instance IsCLMonitorConfiguration (Id CLMonitorConfiguration) where
  toCLMonitorConfiguration = unsafeCastId

instance IsNSObject (Id CLMonitorConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CLMonitoringEvent ----------

-- | Phantom type for @CLMonitoringEvent@.
data CLMonitoringEvent

instance IsObjCObject (Id CLMonitoringEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLMonitoringEvent"

class IsNSObject a => IsCLMonitoringEvent a where
  toCLMonitoringEvent :: a -> Id CLMonitoringEvent

instance IsCLMonitoringEvent (Id CLMonitoringEvent) where
  toCLMonitoringEvent = unsafeCastId

instance IsNSObject (Id CLMonitoringEvent) where
  toNSObject = unsafeCastId

-- ---------- CLMonitoringRecord ----------

-- | Phantom type for @CLMonitoringRecord@.
data CLMonitoringRecord

instance IsObjCObject (Id CLMonitoringRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLMonitoringRecord"

class IsNSObject a => IsCLMonitoringRecord a where
  toCLMonitoringRecord :: a -> Id CLMonitoringRecord

instance IsCLMonitoringRecord (Id CLMonitoringRecord) where
  toCLMonitoringRecord = unsafeCastId

instance IsNSObject (Id CLMonitoringRecord) where
  toNSObject = unsafeCastId

-- ---------- CLPlacemark ----------

-- | Phantom type for @CLPlacemark@.
data CLPlacemark

instance IsObjCObject (Id CLPlacemark) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLPlacemark"

class IsNSObject a => IsCLPlacemark a where
  toCLPlacemark :: a -> Id CLPlacemark

instance IsCLPlacemark (Id CLPlacemark) where
  toCLPlacemark = unsafeCastId

instance IsNSObject (Id CLPlacemark) where
  toNSObject = unsafeCastId

-- ---------- CLRegion ----------

-- | Phantom type for @CLRegion@.
data CLRegion

instance IsObjCObject (Id CLRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLRegion"

class IsNSObject a => IsCLRegion a where
  toCLRegion :: a -> Id CLRegion

instance IsCLRegion (Id CLRegion) where
  toCLRegion = unsafeCastId

instance IsNSObject (Id CLRegion) where
  toNSObject = unsafeCastId

-- ---------- CLServiceSession ----------

-- | Phantom type for @CLServiceSession@.
data CLServiceSession

instance IsObjCObject (Id CLServiceSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLServiceSession"

class IsNSObject a => IsCLServiceSession a where
  toCLServiceSession :: a -> Id CLServiceSession

instance IsCLServiceSession (Id CLServiceSession) where
  toCLServiceSession = unsafeCastId

instance IsNSObject (Id CLServiceSession) where
  toNSObject = unsafeCastId

-- ---------- CLServiceSessionDiagnostic ----------

-- | Phantom type for @CLServiceSessionDiagnostic@.
data CLServiceSessionDiagnostic

instance IsObjCObject (Id CLServiceSessionDiagnostic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLServiceSessionDiagnostic"

class IsNSObject a => IsCLServiceSessionDiagnostic a where
  toCLServiceSessionDiagnostic :: a -> Id CLServiceSessionDiagnostic

instance IsCLServiceSessionDiagnostic (Id CLServiceSessionDiagnostic) where
  toCLServiceSessionDiagnostic = unsafeCastId

instance IsNSObject (Id CLServiceSessionDiagnostic) where
  toNSObject = unsafeCastId

-- ---------- CLUpdate ----------

-- | Phantom type for @CLUpdate@.
data CLUpdate

instance IsObjCObject (Id CLUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLUpdate"

class IsNSObject a => IsCLUpdate a where
  toCLUpdate :: a -> Id CLUpdate

instance IsCLUpdate (Id CLUpdate) where
  toCLUpdate = unsafeCastId

instance IsNSObject (Id CLUpdate) where
  toNSObject = unsafeCastId

-- ---------- CLVisit ----------

-- | Phantom type for @CLVisit@.
data CLVisit

instance IsObjCObject (Id CLVisit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLVisit"

class IsNSObject a => IsCLVisit a where
  toCLVisit :: a -> Id CLVisit

instance IsCLVisit (Id CLVisit) where
  toCLVisit = unsafeCastId

instance IsNSObject (Id CLVisit) where
  toNSObject = unsafeCastId

-- ---------- CLBeaconIdentityCondition ----------

-- | Phantom type for @CLBeaconIdentityCondition@.
data CLBeaconIdentityCondition

instance IsObjCObject (Id CLBeaconIdentityCondition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLBeaconIdentityCondition"

class IsCLCondition a => IsCLBeaconIdentityCondition a where
  toCLBeaconIdentityCondition :: a -> Id CLBeaconIdentityCondition

instance IsCLBeaconIdentityCondition (Id CLBeaconIdentityCondition) where
  toCLBeaconIdentityCondition = unsafeCastId

instance IsCLCondition (Id CLBeaconIdentityCondition) where
  toCLCondition = unsafeCastId

instance IsNSObject (Id CLBeaconIdentityCondition) where
  toNSObject = unsafeCastId

-- ---------- CLCircularGeographicCondition ----------

-- | Phantom type for @CLCircularGeographicCondition@.
data CLCircularGeographicCondition

instance IsObjCObject (Id CLCircularGeographicCondition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLCircularGeographicCondition"

class IsCLCondition a => IsCLCircularGeographicCondition a where
  toCLCircularGeographicCondition :: a -> Id CLCircularGeographicCondition

instance IsCLCircularGeographicCondition (Id CLCircularGeographicCondition) where
  toCLCircularGeographicCondition = unsafeCastId

instance IsCLCondition (Id CLCircularGeographicCondition) where
  toCLCondition = unsafeCastId

instance IsNSObject (Id CLCircularGeographicCondition) where
  toNSObject = unsafeCastId

-- ---------- CLBeaconRegion ----------

-- | Phantom type for @CLBeaconRegion@.
data CLBeaconRegion

instance IsObjCObject (Id CLBeaconRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLBeaconRegion"

class IsCLRegion a => IsCLBeaconRegion a where
  toCLBeaconRegion :: a -> Id CLBeaconRegion

instance IsCLBeaconRegion (Id CLBeaconRegion) where
  toCLBeaconRegion = unsafeCastId

instance IsCLRegion (Id CLBeaconRegion) where
  toCLRegion = unsafeCastId

instance IsNSObject (Id CLBeaconRegion) where
  toNSObject = unsafeCastId

-- ---------- CLCircularRegion ----------

-- | Phantom type for @CLCircularRegion@.
data CLCircularRegion

instance IsObjCObject (Id CLCircularRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLCircularRegion"

class IsCLRegion a => IsCLCircularRegion a where
  toCLCircularRegion :: a -> Id CLCircularRegion

instance IsCLCircularRegion (Id CLCircularRegion) where
  toCLCircularRegion = unsafeCastId

instance IsCLRegion (Id CLCircularRegion) where
  toCLRegion = unsafeCastId

instance IsNSObject (Id CLCircularRegion) where
  toNSObject = unsafeCastId

-- ---------- CLBeaconIdentityConstraint ----------

-- | Phantom type for @CLBeaconIdentityConstraint@.
data CLBeaconIdentityConstraint

instance IsObjCObject (Id CLBeaconIdentityConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CLBeaconIdentityConstraint"

class IsCLBeaconIdentityCondition a => IsCLBeaconIdentityConstraint a where
  toCLBeaconIdentityConstraint :: a -> Id CLBeaconIdentityConstraint

instance IsCLBeaconIdentityConstraint (Id CLBeaconIdentityConstraint) where
  toCLBeaconIdentityConstraint = unsafeCastId

instance IsCLBeaconIdentityCondition (Id CLBeaconIdentityConstraint) where
  toCLBeaconIdentityCondition = unsafeCastId

instance IsCLCondition (Id CLBeaconIdentityConstraint) where
  toCLCondition = unsafeCastId

instance IsNSObject (Id CLBeaconIdentityConstraint) where
  toNSObject = unsafeCastId
