{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ImageCaptureCore.Internal.Classes (
    module ObjC.ImageCaptureCore.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ICCameraItem ----------

-- | ICCameraItem
--
-- ICCameraItem is an abstract class that represents an item in an ICCameraDevice object. ICCameraDevice object creates instances of two concrete subclasses of ICCameraItem: ICCameraFolder and ICCameraFile.
-- 
-- Phantom type for @ICCameraItem@.
data ICCameraItem

instance IsObjCObject (Id ICCameraItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICCameraItem"

class IsNSObject a => IsICCameraItem a where
  toICCameraItem :: a -> Id ICCameraItem

instance IsICCameraItem (Id ICCameraItem) where
  toICCameraItem = unsafeCastId

instance IsNSObject (Id ICCameraItem) where
  toNSObject = unsafeCastId

-- ---------- ICDevice ----------

-- | ICDevice
--
-- ICDevice is an abstract class that represents a device supported by Image Capture facility. ImageCaptureCore defines two concrete subclasses of ICDevice, ICCameraDevice and ICScannerDevice. ICDeviceBrowser creates instances of these two subclasses to represent cameras and scanners it finds.
-- 
-- Phantom type for @ICDevice@.
data ICDevice

instance IsObjCObject (Id ICDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICDevice"

class IsNSObject a => IsICDevice a where
  toICDevice :: a -> Id ICDevice

instance IsICDevice (Id ICDevice) where
  toICDevice = unsafeCastId

instance IsNSObject (Id ICDevice) where
  toNSObject = unsafeCastId

-- ---------- ICDeviceBrowser ----------

-- | ICDeviceBrowser
--
-- The ICDeviceBrowser object is used to find devices such as digital cameras and scanners that are supported by Image Capture. These device may be directly attached to the USB or FireWire bus on the host computer, or available over a TCP/IP network. This object communicates with an Image Capture agent process asynchronously to accomplish this.
-- 
-- Phantom type for @ICDeviceBrowser@.
data ICDeviceBrowser

instance IsObjCObject (Id ICDeviceBrowser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICDeviceBrowser"

class IsNSObject a => IsICDeviceBrowser a where
  toICDeviceBrowser :: a -> Id ICDeviceBrowser

instance IsICDeviceBrowser (Id ICDeviceBrowser) where
  toICDeviceBrowser = unsafeCastId

instance IsNSObject (Id ICDeviceBrowser) where
  toNSObject = unsafeCastId

-- ---------- ICScannerBandData ----------

-- | Phantom type for @ICScannerBandData@.
data ICScannerBandData

instance IsObjCObject (Id ICScannerBandData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerBandData"

class IsNSObject a => IsICScannerBandData a where
  toICScannerBandData :: a -> Id ICScannerBandData

instance IsICScannerBandData (Id ICScannerBandData) where
  toICScannerBandData = unsafeCastId

instance IsNSObject (Id ICScannerBandData) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFeature ----------

-- | ICScannerFeature
--
-- ICScannerFeature class is an abstract base class used to describe a scanner feature. ImageCaptureCore defines three concrete subclasses of ICScannerFeature: ICScannerFeatureEnumeration, ICScannerFeatureRange and ICScannerFeatureBoolean.
--
-- The scanner functional units may have one or more instances of these classes to allow users to choose scanner-specific settings or operations before performing a scan.
-- 
-- Phantom type for @ICScannerFeature@.
data ICScannerFeature

instance IsObjCObject (Id ICScannerFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFeature"

class IsNSObject a => IsICScannerFeature a where
  toICScannerFeature :: a -> Id ICScannerFeature

instance IsICScannerFeature (Id ICScannerFeature) where
  toICScannerFeature = unsafeCastId

instance IsNSObject (Id ICScannerFeature) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFunctionalUnit ----------

-- | ICScannerFunctionalUnit
--
-- ICScannerFunctionalUnit is an abstract class that represents a scanner functiona unit. ImageCaptureCore defines three concrete subclasses of ICScannerFunctionalUnit: ICScannerFunctionalUnitFlatbed, ICScannerFunctionalUnitPositiveTransparency, ICScannerFunctionalUnitNegativeTransparency and ICScannerFunctionalUnitDocumentFeeder. ICScannerDevice creates instances of these concrete subclasses.
-- 
-- Phantom type for @ICScannerFunctionalUnit@.
data ICScannerFunctionalUnit

instance IsObjCObject (Id ICScannerFunctionalUnit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFunctionalUnit"

class IsNSObject a => IsICScannerFunctionalUnit a where
  toICScannerFunctionalUnit :: a -> Id ICScannerFunctionalUnit

instance IsICScannerFunctionalUnit (Id ICScannerFunctionalUnit) where
  toICScannerFunctionalUnit = unsafeCastId

instance IsNSObject (Id ICScannerFunctionalUnit) where
  toNSObject = unsafeCastId

-- ---------- ICCameraFile ----------

-- | ICCameraFile
--
-- This class represents a file on an ICCameraDevice object.
-- 
-- Phantom type for @ICCameraFile@.
data ICCameraFile

instance IsObjCObject (Id ICCameraFile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICCameraFile"

class IsICCameraItem a => IsICCameraFile a where
  toICCameraFile :: a -> Id ICCameraFile

instance IsICCameraFile (Id ICCameraFile) where
  toICCameraFile = unsafeCastId

instance IsICCameraItem (Id ICCameraFile) where
  toICCameraItem = unsafeCastId

instance IsNSObject (Id ICCameraFile) where
  toNSObject = unsafeCastId

-- ---------- ICCameraFolder ----------

-- | ICCameraFolder
--
-- This class represents a folder on an ICCameraDevice object.
-- 
-- Phantom type for @ICCameraFolder@.
data ICCameraFolder

instance IsObjCObject (Id ICCameraFolder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICCameraFolder"

class IsICCameraItem a => IsICCameraFolder a where
  toICCameraFolder :: a -> Id ICCameraFolder

instance IsICCameraFolder (Id ICCameraFolder) where
  toICCameraFolder = unsafeCastId

instance IsICCameraItem (Id ICCameraFolder) where
  toICCameraItem = unsafeCastId

instance IsNSObject (Id ICCameraFolder) where
  toNSObject = unsafeCastId

-- ---------- ICCameraDevice ----------

-- | ICCameraDevice
--
-- ICCameraDevice is a concrete subclass of ICDevice class. ICDeviceBrowser creates instances of this class.
-- 
-- Phantom type for @ICCameraDevice@.
data ICCameraDevice

instance IsObjCObject (Id ICCameraDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICCameraDevice"

class IsICDevice a => IsICCameraDevice a where
  toICCameraDevice :: a -> Id ICCameraDevice

instance IsICCameraDevice (Id ICCameraDevice) where
  toICCameraDevice = unsafeCastId

instance IsICDevice (Id ICCameraDevice) where
  toICDevice = unsafeCastId

instance IsNSObject (Id ICCameraDevice) where
  toNSObject = unsafeCastId

-- ---------- ICScannerDevice ----------

-- | ICScannerDevice
--
-- ICScannerDevice is a concrete subclass of ICDevice class. ICDeviceBrowser creates instances of this class.
--
-- In this release, an instance of ICScannerDevice class is intended to be used by the ICScannerDeviceView object. The ICScannerDeviceView class encapsulates the complexities of setting scan parameters, performing scans and saving the result. The developer should consider using ICScannerDeviceView instead of building their own views using the ICScannerDevice object.
-- 
-- Phantom type for @ICScannerDevice@.
data ICScannerDevice

instance IsObjCObject (Id ICScannerDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerDevice"

class IsICDevice a => IsICScannerDevice a where
  toICScannerDevice :: a -> Id ICScannerDevice

instance IsICScannerDevice (Id ICScannerDevice) where
  toICScannerDevice = unsafeCastId

instance IsICDevice (Id ICScannerDevice) where
  toICDevice = unsafeCastId

instance IsNSObject (Id ICScannerDevice) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFeatureBoolean ----------

-- | ICScannerFeatureBoolean
--
-- ICScannerFeatureBoolean object is used to represent a property of a scanner functional unit whose value can be YES or NO.
-- 
-- Phantom type for @ICScannerFeatureBoolean@.
data ICScannerFeatureBoolean

instance IsObjCObject (Id ICScannerFeatureBoolean) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFeatureBoolean"

class IsICScannerFeature a => IsICScannerFeatureBoolean a where
  toICScannerFeatureBoolean :: a -> Id ICScannerFeatureBoolean

instance IsICScannerFeatureBoolean (Id ICScannerFeatureBoolean) where
  toICScannerFeatureBoolean = unsafeCastId

instance IsICScannerFeature (Id ICScannerFeatureBoolean) where
  toICScannerFeature = unsafeCastId

instance IsNSObject (Id ICScannerFeatureBoolean) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFeatureEnumeration ----------

-- | ICScannerFeatureEnumeration
--
-- ICScannerFeatureEnumeration object is used to represent a feature of a scanner functional unit that can have one of several discrete values.
-- 
-- Phantom type for @ICScannerFeatureEnumeration@.
data ICScannerFeatureEnumeration

instance IsObjCObject (Id ICScannerFeatureEnumeration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFeatureEnumeration"

class IsICScannerFeature a => IsICScannerFeatureEnumeration a where
  toICScannerFeatureEnumeration :: a -> Id ICScannerFeatureEnumeration

instance IsICScannerFeatureEnumeration (Id ICScannerFeatureEnumeration) where
  toICScannerFeatureEnumeration = unsafeCastId

instance IsICScannerFeature (Id ICScannerFeatureEnumeration) where
  toICScannerFeature = unsafeCastId

instance IsNSObject (Id ICScannerFeatureEnumeration) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFeatureRange ----------

-- | ICScannerFeatureRange
--
-- ICScannerFeatureRange object is used to represent a property of a scanner functional unit whose value lies within a range.
-- 
-- Phantom type for @ICScannerFeatureRange@.
data ICScannerFeatureRange

instance IsObjCObject (Id ICScannerFeatureRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFeatureRange"

class IsICScannerFeature a => IsICScannerFeatureRange a where
  toICScannerFeatureRange :: a -> Id ICScannerFeatureRange

instance IsICScannerFeatureRange (Id ICScannerFeatureRange) where
  toICScannerFeatureRange = unsafeCastId

instance IsICScannerFeature (Id ICScannerFeatureRange) where
  toICScannerFeature = unsafeCastId

instance IsNSObject (Id ICScannerFeatureRange) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFeatureTemplate ----------

-- | ICScannerFeatureTemplate
--
-- ICScannerFeatureTemplate object is used to define a group of one or more rectangular scan areas that can be used with a scanner functional unit.
-- 
-- Phantom type for @ICScannerFeatureTemplate@.
data ICScannerFeatureTemplate

instance IsObjCObject (Id ICScannerFeatureTemplate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFeatureTemplate"

class IsICScannerFeature a => IsICScannerFeatureTemplate a where
  toICScannerFeatureTemplate :: a -> Id ICScannerFeatureTemplate

instance IsICScannerFeatureTemplate (Id ICScannerFeatureTemplate) where
  toICScannerFeatureTemplate = unsafeCastId

instance IsICScannerFeature (Id ICScannerFeatureTemplate) where
  toICScannerFeature = unsafeCastId

instance IsNSObject (Id ICScannerFeatureTemplate) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFunctionalUnitDocumentFeeder ----------

-- | ICScannerFunctionalUnitDocumentFeeder
--
-- ICScannerFunctionalUnitDocumentFeeder is a concrete subclass of ICScannerFunctionalUnit class. ICScannerDevice creates instances of this class.
--
-- This represents the document feeder unit on the scanner.
-- 
-- Phantom type for @ICScannerFunctionalUnitDocumentFeeder@.
data ICScannerFunctionalUnitDocumentFeeder

instance IsObjCObject (Id ICScannerFunctionalUnitDocumentFeeder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFunctionalUnitDocumentFeeder"

class IsICScannerFunctionalUnit a => IsICScannerFunctionalUnitDocumentFeeder a where
  toICScannerFunctionalUnitDocumentFeeder :: a -> Id ICScannerFunctionalUnitDocumentFeeder

instance IsICScannerFunctionalUnitDocumentFeeder (Id ICScannerFunctionalUnitDocumentFeeder) where
  toICScannerFunctionalUnitDocumentFeeder = unsafeCastId

instance IsICScannerFunctionalUnit (Id ICScannerFunctionalUnitDocumentFeeder) where
  toICScannerFunctionalUnit = unsafeCastId

instance IsNSObject (Id ICScannerFunctionalUnitDocumentFeeder) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFunctionalUnitFlatbed ----------

-- | ICScannerFunctionalUnitFlatbed
--
-- ICScannerFunctionalUnitFlatbed is a concrete subclass of ICScannerFunctionalUnit class. ICScannerDevice creates instances of this class.
--
-- This represents the flatbed  unit on the scanner.
-- 
-- Phantom type for @ICScannerFunctionalUnitFlatbed@.
data ICScannerFunctionalUnitFlatbed

instance IsObjCObject (Id ICScannerFunctionalUnitFlatbed) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFunctionalUnitFlatbed"

class IsICScannerFunctionalUnit a => IsICScannerFunctionalUnitFlatbed a where
  toICScannerFunctionalUnitFlatbed :: a -> Id ICScannerFunctionalUnitFlatbed

instance IsICScannerFunctionalUnitFlatbed (Id ICScannerFunctionalUnitFlatbed) where
  toICScannerFunctionalUnitFlatbed = unsafeCastId

instance IsICScannerFunctionalUnit (Id ICScannerFunctionalUnitFlatbed) where
  toICScannerFunctionalUnit = unsafeCastId

instance IsNSObject (Id ICScannerFunctionalUnitFlatbed) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFunctionalUnitNegativeTransparency ----------

-- | ICScannerFunctionalUnitNegativeTransparency
--
-- ICScannerFunctionalUnitNegativeTransparency is a concrete subclass of ICScannerFunctionalUnit class. ICScannerDevice creates instances of this class.
--
-- This represents the transparency unit on the scanner for scanning negatives.
-- 
-- Phantom type for @ICScannerFunctionalUnitNegativeTransparency@.
data ICScannerFunctionalUnitNegativeTransparency

instance IsObjCObject (Id ICScannerFunctionalUnitNegativeTransparency) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFunctionalUnitNegativeTransparency"

class IsICScannerFunctionalUnit a => IsICScannerFunctionalUnitNegativeTransparency a where
  toICScannerFunctionalUnitNegativeTransparency :: a -> Id ICScannerFunctionalUnitNegativeTransparency

instance IsICScannerFunctionalUnitNegativeTransparency (Id ICScannerFunctionalUnitNegativeTransparency) where
  toICScannerFunctionalUnitNegativeTransparency = unsafeCastId

instance IsICScannerFunctionalUnit (Id ICScannerFunctionalUnitNegativeTransparency) where
  toICScannerFunctionalUnit = unsafeCastId

instance IsNSObject (Id ICScannerFunctionalUnitNegativeTransparency) where
  toNSObject = unsafeCastId

-- ---------- ICScannerFunctionalUnitPositiveTransparency ----------

-- | ICScannerFunctionalUnitPositiveTransparency
--
-- ICScannerFunctionalUnitPositiveTransparency is a concrete subclass of ICScannerFunctionalUnit class. ICScannerDevice creates instances of this class.
--
-- This represents the transparency unit on the scanner for scanning postives
-- 
-- Phantom type for @ICScannerFunctionalUnitPositiveTransparency@.
data ICScannerFunctionalUnitPositiveTransparency

instance IsObjCObject (Id ICScannerFunctionalUnitPositiveTransparency) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ICScannerFunctionalUnitPositiveTransparency"

class IsICScannerFunctionalUnit a => IsICScannerFunctionalUnitPositiveTransparency a where
  toICScannerFunctionalUnitPositiveTransparency :: a -> Id ICScannerFunctionalUnitPositiveTransparency

instance IsICScannerFunctionalUnitPositiveTransparency (Id ICScannerFunctionalUnitPositiveTransparency) where
  toICScannerFunctionalUnitPositiveTransparency = unsafeCastId

instance IsICScannerFunctionalUnit (Id ICScannerFunctionalUnitPositiveTransparency) where
  toICScannerFunctionalUnit = unsafeCastId

instance IsNSObject (Id ICScannerFunctionalUnitPositiveTransparency) where
  toNSObject = unsafeCastId
