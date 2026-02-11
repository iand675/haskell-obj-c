{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreHaptics.Internal.Classes (
    module ObjC.CoreHaptics.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- CHHapticDynamicParameter ----------

-- | CHHapticDynamicParameter
--
-- A CHHapticDynamicParameter contains a CHHapticDynamicParameterID/value pair which will modify (modulate) the ongoing character		of a haptic or audio event.
--
-- CHHapticDynamicParameters have a relative time property to allow specifying the time relationship between parameters in a pattern.
-- 
-- Phantom type for @CHHapticDynamicParameter@.
data CHHapticDynamicParameter

instance IsObjCObject (Id CHHapticDynamicParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticDynamicParameter"

class IsNSObject a => IsCHHapticDynamicParameter a where
  toCHHapticDynamicParameter :: a -> Id CHHapticDynamicParameter

instance IsCHHapticDynamicParameter (Id CHHapticDynamicParameter) where
  toCHHapticDynamicParameter = unsafeCastId

instance IsNSObject (Id CHHapticDynamicParameter) where
  toNSObject = unsafeCastId

-- ---------- CHHapticEngine ----------

-- | CHHapticEngine
--
-- Represents the connection with the haptic server.
-- 
-- Phantom type for @CHHapticEngine@.
data CHHapticEngine

instance IsObjCObject (Id CHHapticEngine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticEngine"

class IsNSObject a => IsCHHapticEngine a where
  toCHHapticEngine :: a -> Id CHHapticEngine

instance IsCHHapticEngine (Id CHHapticEngine) where
  toCHHapticEngine = unsafeCastId

instance IsNSObject (Id CHHapticEngine) where
  toNSObject = unsafeCastId

-- ---------- CHHapticEvent ----------

-- | CHHapticEvent
--
-- The description of a single haptic/audio event, plus optional Event parameters which modify the event.
--
-- CHHapticEvents have a relative time property to allow specifying the time relationship between events in a pattern.
-- 
-- Phantom type for @CHHapticEvent@.
data CHHapticEvent

instance IsObjCObject (Id CHHapticEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticEvent"

class IsNSObject a => IsCHHapticEvent a where
  toCHHapticEvent :: a -> Id CHHapticEvent

instance IsCHHapticEvent (Id CHHapticEvent) where
  toCHHapticEvent = unsafeCastId

instance IsNSObject (Id CHHapticEvent) where
  toNSObject = unsafeCastId

-- ---------- CHHapticEventParameter ----------

-- | CHHapticEventParameter
--
-- A CHHapticEventParameter contains a CHHapticEventParameterID/value pair which helps determine the initial character		of a haptic or audio event.
-- 
-- Phantom type for @CHHapticEventParameter@.
data CHHapticEventParameter

instance IsObjCObject (Id CHHapticEventParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticEventParameter"

class IsNSObject a => IsCHHapticEventParameter a where
  toCHHapticEventParameter :: a -> Id CHHapticEventParameter

instance IsCHHapticEventParameter (Id CHHapticEventParameter) where
  toCHHapticEventParameter = unsafeCastId

instance IsNSObject (Id CHHapticEventParameter) where
  toNSObject = unsafeCastId

-- ---------- CHHapticParameterCurve ----------

-- | CHHapticParameterCurve
--
-- A CHHapticParameterCurve is a set of CHHapticParameterCurveControlPoints which describe the control (inflection) points 		for the parameter values to be applied to the associated pattern.
--
-- The CHHapticParameterCurve generates an interpolated value output which passed through each control point at its 		associated relative time.  These times will all be relative to the start time of the CHHapticParameterCurve within the 		playing pattern.
-- 
-- Phantom type for @CHHapticParameterCurve@.
data CHHapticParameterCurve

instance IsObjCObject (Id CHHapticParameterCurve) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticParameterCurve"

class IsNSObject a => IsCHHapticParameterCurve a where
  toCHHapticParameterCurve :: a -> Id CHHapticParameterCurve

instance IsCHHapticParameterCurve (Id CHHapticParameterCurve) where
  toCHHapticParameterCurve = unsafeCastId

instance IsNSObject (Id CHHapticParameterCurve) where
  toNSObject = unsafeCastId

-- ---------- CHHapticParameterCurveControlPoint ----------

-- | CHHapticParameterCurveControlPoint
--
-- A CHHapticParameterCurveControlPoint contains a time/value pair for a single control point within a CHHapticParameterCurve.
--
-- The relativeTime property specifies the amount of time elapsed since the start of the CHHapticParameterCurve before the 		value is reached.
-- 
-- Phantom type for @CHHapticParameterCurveControlPoint@.
data CHHapticParameterCurveControlPoint

instance IsObjCObject (Id CHHapticParameterCurveControlPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticParameterCurveControlPoint"

class IsNSObject a => IsCHHapticParameterCurveControlPoint a where
  toCHHapticParameterCurveControlPoint :: a -> Id CHHapticParameterCurveControlPoint

instance IsCHHapticParameterCurveControlPoint (Id CHHapticParameterCurveControlPoint) where
  toCHHapticParameterCurveControlPoint = unsafeCastId

instance IsNSObject (Id CHHapticParameterCurveControlPoint) where
  toNSObject = unsafeCastId

-- ---------- CHHapticPattern ----------

-- | CHHapticPattern
--
-- A set of one or more haptic events and/or Dynamic parameters/parameter curves.
--
-- The passed-in arrays' contents are not owned by the pattern object.  Changes made to those arrays		after a CHHapticPattern object is created have no effect on that object.
-- 
-- Phantom type for @CHHapticPattern@.
data CHHapticPattern

instance IsObjCObject (Id CHHapticPattern) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CHHapticPattern"

class IsNSObject a => IsCHHapticPattern a where
  toCHHapticPattern :: a -> Id CHHapticPattern

instance IsCHHapticPattern (Id CHHapticPattern) where
  toCHHapticPattern = unsafeCastId

instance IsNSObject (Id CHHapticPattern) where
  toNSObject = unsafeCastId
