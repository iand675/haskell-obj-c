{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AudioToolbox.Internal.Classes (
    module ObjC.AudioToolbox.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.CoreAudioKit.Internal.Classes,
    module ObjC.CoreMIDI.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.CoreAudioKit.Internal.Classes
import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- AUAudioUnit ----------

-- | AUAudioUnit
--
-- An audio unit instance.
--
-- AUAudioUnit is a host interface to an audio unit. Hosts can instantiate either version 2 or		version 3 units with this class, and to some extent control whether an audio unit is		instantiated in-process or in a separate extension process.
--
-- Implementors of version 3 audio units can and should subclass AUAudioUnit. To port an		existing version 2 audio unit easily, AUAudioUnitV2Bridge can be subclassed.
--
-- These are the ways in which audio unit components can be registered:
--
-- - (v2) Packaged into a component bundle containing an @AudioComponents@ Info.plist entry,		referring to an @AudioComponentFactoryFunction@. See AudioComponent.h.
--
-- - (v2) AudioComponentRegister(). Associates a component description with an		AudioComponentFactoryFunction. See AudioComponent.h.
--
-- - (v3) Packaged into an app extension containing an AudioComponents Info.plist entry.		The principal class must conform to the AUAudioUnitFactory protocol, which will typically		instantiate an AUAudioUnit subclass.
--
-- - (v3) @+[AUAudioUnit registerSubclass:asComponentDescription:name:version:]@. Associates		a component description with an AUAudioUnit subclass.
--
-- A host need not be aware of the concrete subclass of AUAudioUnit that is being instantiated.		@initWithComponentDescription:options:error:@ ensures that the proper subclass is used.
--
-- When using AUAudioUnit with a v2 audio unit, or the C AudioComponent and AudioUnit API's		with a v3 audio unit, all major pieces of functionality are bridged between the		two API's. This header describes, for each v3 method or property, the v2 equivalent.
-- 
-- Phantom type for @AUAudioUnit@.
data AUAudioUnit

instance IsObjCObject (Id AUAudioUnit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUAudioUnit"

class IsNSObject a => IsAUAudioUnit a where
  toAUAudioUnit :: a -> Id AUAudioUnit

instance IsAUAudioUnit (Id AUAudioUnit) where
  toAUAudioUnit = unsafeCastId

instance IsNSObject (Id AUAudioUnit) where
  toNSObject = unsafeCastId

-- ---------- AUAudioUnitBus ----------

-- | AUAudioUnitBus
--
-- An input or output connection point on an audio unit.
-- 
-- Phantom type for @AUAudioUnitBus@.
data AUAudioUnitBus

instance IsObjCObject (Id AUAudioUnitBus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUAudioUnitBus"

class IsNSObject a => IsAUAudioUnitBus a where
  toAUAudioUnitBus :: a -> Id AUAudioUnitBus

instance IsAUAudioUnitBus (Id AUAudioUnitBus) where
  toAUAudioUnitBus = unsafeCastId

instance IsNSObject (Id AUAudioUnitBus) where
  toNSObject = unsafeCastId

-- ---------- AUAudioUnitBusArray ----------

-- | AUAudioUnitBusArray
--
-- Container for an audio unit's input or output busses.
--
-- Hosts can observe a bus property across all busses by using KVO on this object, without		having to observe it on each individual bus. (One could add listeners to individual busses,		but that means one has to observe bus count changes and add/remove listeners in response.		Also, NSArray's addObserver:toObjectsAtIndexes:forKeyPath:options:context: is problematic;		it does not let the individual objects override the observation request, and so a bus which		is proxying a bus in an extension process does not get the message.)
--
-- Some audio units (e.g. mixers) support variable numbers of busses, via subclassing. When the		bus count changes, a KVO notification is sent on "inputBusses" or "outputBusses," as		appropriate.
--
-- Subclassers should see also the AUAudioUnitBusImplementation category.
--
-- The bus array is bridged to the v2 property kAudioUnitProperty_ElementCount.
-- 
-- Phantom type for @AUAudioUnitBusArray@.
data AUAudioUnitBusArray

instance IsObjCObject (Id AUAudioUnitBusArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUAudioUnitBusArray"

class IsNSObject a => IsAUAudioUnitBusArray a where
  toAUAudioUnitBusArray :: a -> Id AUAudioUnitBusArray

instance IsAUAudioUnitBusArray (Id AUAudioUnitBusArray) where
  toAUAudioUnitBusArray = unsafeCastId

instance IsNSObject (Id AUAudioUnitBusArray) where
  toNSObject = unsafeCastId

-- ---------- AUAudioUnitPreset ----------

-- | AUAudioUnitPreset
--
-- A collection of parameter settings provided by the audio unit implementor, producing a			useful sound or starting point.
-- 
-- Phantom type for @AUAudioUnitPreset@.
data AUAudioUnitPreset

instance IsObjCObject (Id AUAudioUnitPreset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUAudioUnitPreset"

class IsNSObject a => IsAUAudioUnitPreset a where
  toAUAudioUnitPreset :: a -> Id AUAudioUnitPreset

instance IsAUAudioUnitPreset (Id AUAudioUnitPreset) where
  toAUAudioUnitPreset = unsafeCastId

instance IsNSObject (Id AUAudioUnitPreset) where
  toNSObject = unsafeCastId

-- ---------- AUParameterNode ----------

-- | AUParameterNode
--
-- A node in an audio unit's tree of parameters.
--
-- Nodes are instances of either AUParameterGroup or AUParameter.
-- 
-- Phantom type for @AUParameterNode@.
data AUParameterNode

instance IsObjCObject (Id AUParameterNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUParameterNode"

class IsNSObject a => IsAUParameterNode a where
  toAUParameterNode :: a -> Id AUParameterNode

instance IsAUParameterNode (Id AUParameterNode) where
  toAUParameterNode = unsafeCastId

instance IsNSObject (Id AUParameterNode) where
  toNSObject = unsafeCastId

-- ---------- AUAudioUnitV2Bridge ----------

-- | Wraps a v2 audio unit in an AUAudioUnit subclass.
--
-- Implementors of version 3 audio units may derive their implementations from		AUAudioUnitV2Bridge. It expects the component description with which it is initialized to		refer to a registered component with a v2 implementation using an		AudioComponentFactoryFunction. The bridge will instantiate the v2 audio unit via the factory		function and communicate it with it using the v2 AudioUnit API's (AudioUnitSetProperty,		etc.)
--
-- Hosts should not access this class; it will be instantiated when needed when creating an		AUAudioUnit.
-- 
-- Phantom type for @AUAudioUnitV2Bridge@.
data AUAudioUnitV2Bridge

instance IsObjCObject (Id AUAudioUnitV2Bridge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUAudioUnitV2Bridge"

class IsAUAudioUnit a => IsAUAudioUnitV2Bridge a where
  toAUAudioUnitV2Bridge :: a -> Id AUAudioUnitV2Bridge

instance IsAUAudioUnitV2Bridge (Id AUAudioUnitV2Bridge) where
  toAUAudioUnitV2Bridge = unsafeCastId

instance IsAUAudioUnit (Id AUAudioUnitV2Bridge) where
  toAUAudioUnit = unsafeCastId

instance IsNSObject (Id AUAudioUnitV2Bridge) where
  toNSObject = unsafeCastId

-- ---------- AUParameter ----------

-- | AUParameter
--
-- A node representing a single parameter.
-- 
-- Phantom type for @AUParameter@.
data AUParameter

instance IsObjCObject (Id AUParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUParameter"

class IsAUParameterNode a => IsAUParameter a where
  toAUParameter :: a -> Id AUParameter

instance IsAUParameter (Id AUParameter) where
  toAUParameter = unsafeCastId

instance IsAUParameterNode (Id AUParameter) where
  toAUParameterNode = unsafeCastId

instance IsNSObject (Id AUParameter) where
  toNSObject = unsafeCastId

-- ---------- AUParameterGroup ----------

-- | AUParameterGroup
--
-- A group of related parameters.
--
-- A parameter group is KVC-compliant for its children; e.g. valueForKey:"volume" will		return a child parameter whose identifier is "volume".
-- 
-- Phantom type for @AUParameterGroup@.
data AUParameterGroup

instance IsObjCObject (Id AUParameterGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUParameterGroup"

class IsAUParameterNode a => IsAUParameterGroup a where
  toAUParameterGroup :: a -> Id AUParameterGroup

instance IsAUParameterGroup (Id AUParameterGroup) where
  toAUParameterGroup = unsafeCastId

instance IsAUParameterNode (Id AUParameterGroup) where
  toAUParameterNode = unsafeCastId

instance IsNSObject (Id AUParameterGroup) where
  toNSObject = unsafeCastId

-- ---------- AUParameterTree ----------

-- | AUParameterTree
--
-- The top level group node, representing all of an audio unit's parameters.
--
-- An audio unit's parameters are organized into a tree containing groups and parameters.		Groups may be nested.
--
-- The tree is KVO-compliant. For example, if the tree contains a group named "LFO" ,		containing a parameter named rate, then "LFO.rate" refers to that parameter object, and		"LFO.rate.value" refers to that parameter's value.
--
-- An audio unit may choose to dynamically rearrange the tree. When doing so, it must		issue a KVO notification on the audio unit's parameterTree property. The tree's elements are		mostly immutable (except for values and implementor hooks); the only way to modify them		is to publish a new tree.
-- 
-- Phantom type for @AUParameterTree@.
data AUParameterTree

instance IsObjCObject (Id AUParameterTree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUParameterTree"

class IsAUParameterGroup a => IsAUParameterTree a where
  toAUParameterTree :: a -> Id AUParameterTree

instance IsAUParameterTree (Id AUParameterTree) where
  toAUParameterTree = unsafeCastId

instance IsAUParameterGroup (Id AUParameterTree) where
  toAUParameterGroup = unsafeCastId

instance IsAUParameterNode (Id AUParameterTree) where
  toAUParameterNode = unsafeCastId

instance IsNSObject (Id AUParameterTree) where
  toNSObject = unsafeCastId
