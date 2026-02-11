{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreMediaIO.Internal.Classes (
    module ObjC.CoreMediaIO.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CMIOExtensionClient ----------

-- | Phantom type for @CMIOExtensionClient@.
data CMIOExtensionClient

instance IsObjCObject (Id CMIOExtensionClient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionClient"

class IsNSObject a => IsCMIOExtensionClient a where
  toCMIOExtensionClient :: a -> Id CMIOExtensionClient

instance IsCMIOExtensionClient (Id CMIOExtensionClient) where
  toCMIOExtensionClient = unsafeCastId

instance IsNSObject (Id CMIOExtensionClient) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionDevice ----------

-- | CMIOExtensionDevice
--
-- A CMIOExtensionDevice describes a device.
-- 
-- Phantom type for @CMIOExtensionDevice@.
data CMIOExtensionDevice

instance IsObjCObject (Id CMIOExtensionDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionDevice"

class IsNSObject a => IsCMIOExtensionDevice a where
  toCMIOExtensionDevice :: a -> Id CMIOExtensionDevice

instance IsCMIOExtensionDevice (Id CMIOExtensionDevice) where
  toCMIOExtensionDevice = unsafeCastId

instance IsNSObject (Id CMIOExtensionDevice) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionDeviceProperties ----------

-- | CMIOExtensionDeviceProperties
--
-- A CMIOExtensionDeviceProperties describes a CoreMediaIO extension device properties.
-- 
-- Phantom type for @CMIOExtensionDeviceProperties@.
data CMIOExtensionDeviceProperties

instance IsObjCObject (Id CMIOExtensionDeviceProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionDeviceProperties"

class IsNSObject a => IsCMIOExtensionDeviceProperties a where
  toCMIOExtensionDeviceProperties :: a -> Id CMIOExtensionDeviceProperties

instance IsCMIOExtensionDeviceProperties (Id CMIOExtensionDeviceProperties) where
  toCMIOExtensionDeviceProperties = unsafeCastId

instance IsNSObject (Id CMIOExtensionDeviceProperties) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionPropertyAttributes ----------

-- | CMIOExtensionPropertyAttributes
--
-- A CMIOExtensionPropertyAttributes describes attributes of a property's value.
-- 
-- Phantom type for @CMIOExtensionPropertyAttributes@.
data CMIOExtensionPropertyAttributes

instance IsObjCObject (Id CMIOExtensionPropertyAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionPropertyAttributes"

class IsNSObject a => IsCMIOExtensionPropertyAttributes a where
  toCMIOExtensionPropertyAttributes :: a -> Id CMIOExtensionPropertyAttributes

instance IsCMIOExtensionPropertyAttributes (Id CMIOExtensionPropertyAttributes) where
  toCMIOExtensionPropertyAttributes = unsafeCastId

instance IsNSObject (Id CMIOExtensionPropertyAttributes) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionPropertyState ----------

-- | CMIOExtensionPropertyState
--
-- A CMIOExtensionPropertyState describes a property state.
-- 
-- Phantom type for @CMIOExtensionPropertyState@.
data CMIOExtensionPropertyState

instance IsObjCObject (Id CMIOExtensionPropertyState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionPropertyState"

class IsNSObject a => IsCMIOExtensionPropertyState a where
  toCMIOExtensionPropertyState :: a -> Id CMIOExtensionPropertyState

instance IsCMIOExtensionPropertyState (Id CMIOExtensionPropertyState) where
  toCMIOExtensionPropertyState = unsafeCastId

instance IsNSObject (Id CMIOExtensionPropertyState) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionProvider ----------

-- | CMIOExtensionProvider
--
-- A CMIOExtensionProvider describes a CoreMediaIO extension provider.
-- 
-- Phantom type for @CMIOExtensionProvider@.
data CMIOExtensionProvider

instance IsObjCObject (Id CMIOExtensionProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionProvider"

class IsNSObject a => IsCMIOExtensionProvider a where
  toCMIOExtensionProvider :: a -> Id CMIOExtensionProvider

instance IsCMIOExtensionProvider (Id CMIOExtensionProvider) where
  toCMIOExtensionProvider = unsafeCastId

instance IsNSObject (Id CMIOExtensionProvider) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionProviderProperties ----------

-- | CMIOExtensionProviderProperties
--
-- A CMIOExtensionProviderProperties describes CoreMediaIO extension provider properties.
-- 
-- Phantom type for @CMIOExtensionProviderProperties@.
data CMIOExtensionProviderProperties

instance IsObjCObject (Id CMIOExtensionProviderProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionProviderProperties"

class IsNSObject a => IsCMIOExtensionProviderProperties a where
  toCMIOExtensionProviderProperties :: a -> Id CMIOExtensionProviderProperties

instance IsCMIOExtensionProviderProperties (Id CMIOExtensionProviderProperties) where
  toCMIOExtensionProviderProperties = unsafeCastId

instance IsNSObject (Id CMIOExtensionProviderProperties) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionScheduledOutput ----------

-- | Phantom type for @CMIOExtensionScheduledOutput@.
data CMIOExtensionScheduledOutput

instance IsObjCObject (Id CMIOExtensionScheduledOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionScheduledOutput"

class IsNSObject a => IsCMIOExtensionScheduledOutput a where
  toCMIOExtensionScheduledOutput :: a -> Id CMIOExtensionScheduledOutput

instance IsCMIOExtensionScheduledOutput (Id CMIOExtensionScheduledOutput) where
  toCMIOExtensionScheduledOutput = unsafeCastId

instance IsNSObject (Id CMIOExtensionScheduledOutput) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionStream ----------

-- | CMIOExtensionStream
--
-- A CMIOExtensionStream describes a stream of media data.
-- 
-- Phantom type for @CMIOExtensionStream@.
data CMIOExtensionStream

instance IsObjCObject (Id CMIOExtensionStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionStream"

class IsNSObject a => IsCMIOExtensionStream a where
  toCMIOExtensionStream :: a -> Id CMIOExtensionStream

instance IsCMIOExtensionStream (Id CMIOExtensionStream) where
  toCMIOExtensionStream = unsafeCastId

instance IsNSObject (Id CMIOExtensionStream) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionStreamCustomClockConfiguration ----------

-- | CMIOExtensionStreamCustomClockConfiguration
--
-- A CMIOExtensionStreamCustomClockProperties describes the parameters used to create a custom clock on the host side (as opposed to the stream using hosttime or a linked Core Audio clock.
-- 
-- Phantom type for @CMIOExtensionStreamCustomClockConfiguration@.
data CMIOExtensionStreamCustomClockConfiguration

instance IsObjCObject (Id CMIOExtensionStreamCustomClockConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionStreamCustomClockConfiguration"

class IsNSObject a => IsCMIOExtensionStreamCustomClockConfiguration a where
  toCMIOExtensionStreamCustomClockConfiguration :: a -> Id CMIOExtensionStreamCustomClockConfiguration

instance IsCMIOExtensionStreamCustomClockConfiguration (Id CMIOExtensionStreamCustomClockConfiguration) where
  toCMIOExtensionStreamCustomClockConfiguration = unsafeCastId

instance IsNSObject (Id CMIOExtensionStreamCustomClockConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionStreamFormat ----------

-- | CMIOExtensionStreamFormat
--
-- A CMIOExtensionStreamFormat describes a stream format.
-- 
-- Phantom type for @CMIOExtensionStreamFormat@.
data CMIOExtensionStreamFormat

instance IsObjCObject (Id CMIOExtensionStreamFormat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionStreamFormat"

class IsNSObject a => IsCMIOExtensionStreamFormat a where
  toCMIOExtensionStreamFormat :: a -> Id CMIOExtensionStreamFormat

instance IsCMIOExtensionStreamFormat (Id CMIOExtensionStreamFormat) where
  toCMIOExtensionStreamFormat = unsafeCastId

instance IsNSObject (Id CMIOExtensionStreamFormat) where
  toNSObject = unsafeCastId

-- ---------- CMIOExtensionStreamProperties ----------

-- | CMIOExtensionStreamProperties
--
-- A CMIOExtensionStreamProperties describes a CoreMediaIO extension stream properties.
-- 
-- Phantom type for @CMIOExtensionStreamProperties@.
data CMIOExtensionStreamProperties

instance IsObjCObject (Id CMIOExtensionStreamProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CMIOExtensionStreamProperties"

class IsNSObject a => IsCMIOExtensionStreamProperties a where
  toCMIOExtensionStreamProperties :: a -> Id CMIOExtensionStreamProperties

instance IsCMIOExtensionStreamProperties (Id CMIOExtensionStreamProperties) where
  toCMIOExtensionStreamProperties = unsafeCastId

instance IsNSObject (Id CMIOExtensionStreamProperties) where
  toNSObject = unsafeCastId
