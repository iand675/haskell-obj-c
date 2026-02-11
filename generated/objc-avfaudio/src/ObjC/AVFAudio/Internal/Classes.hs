{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AVFAudio.Internal.Classes (
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.AudioToolbox.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- AVAudioApplication ----------

-- | Class containing methods that relate to an application bundle's audio (i.e. a collection of one or more AVAudioSession instances)
-- 
-- Phantom type for @AVAudioApplication@.
data AVAudioApplication

instance IsObjCObject (Id AVAudioApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioApplication"

class IsNSObject a => IsAVAudioApplication a where
  toAVAudioApplication :: a -> Id AVAudioApplication

instance IsAVAudioApplication (Id AVAudioApplication) where
  toAVAudioApplication = unsafeCastId

instance IsNSObject (Id AVAudioApplication) where
  toNSObject = unsafeCastId

-- ---------- AVAudioBuffer ----------

-- | AVAudioBuffer
--
-- A buffer of audio data, with a format.
--
-- AVAudioBuffer represents a buffer of audio data and its format.
-- 
-- Phantom type for @AVAudioBuffer@.
data AVAudioBuffer

instance IsObjCObject (Id AVAudioBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioBuffer"

class IsNSObject a => IsAVAudioBuffer a where
  toAVAudioBuffer :: a -> Id AVAudioBuffer

instance IsAVAudioBuffer (Id AVAudioBuffer) where
  toAVAudioBuffer = unsafeCastId

instance IsNSObject (Id AVAudioBuffer) where
  toNSObject = unsafeCastId

-- ---------- AVAudioChannelLayout ----------

-- | AVAudioChannelLayout
--
-- A description of the roles of a set of audio channels.
--
-- This object is a thin wrapper for the AudioChannelLayout structure, described		in <CoreAudio/CoreAudioTypes.h>.
-- 
-- Phantom type for @AVAudioChannelLayout@.
data AVAudioChannelLayout

instance IsObjCObject (Id AVAudioChannelLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioChannelLayout"

class IsNSObject a => IsAVAudioChannelLayout a where
  toAVAudioChannelLayout :: a -> Id AVAudioChannelLayout

instance IsAVAudioChannelLayout (Id AVAudioChannelLayout) where
  toAVAudioChannelLayout = unsafeCastId

instance IsNSObject (Id AVAudioChannelLayout) where
  toNSObject = unsafeCastId

-- ---------- AVAudioConnectionPoint ----------

-- | AVAudioConnectionPoint
--
-- A representation of either a source or destination connection point in AVAudioEngine.
--
-- AVAudioConnectionPoint describes either a source or destination connection point (node, bus)		in AVAudioEngine's graph.
--
-- Instances of this class are immutable.
-- 
-- Phantom type for @AVAudioConnectionPoint@.
data AVAudioConnectionPoint

instance IsObjCObject (Id AVAudioConnectionPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioConnectionPoint"

class IsNSObject a => IsAVAudioConnectionPoint a where
  toAVAudioConnectionPoint :: a -> Id AVAudioConnectionPoint

instance IsAVAudioConnectionPoint (Id AVAudioConnectionPoint) where
  toAVAudioConnectionPoint = unsafeCastId

instance IsNSObject (Id AVAudioConnectionPoint) where
  toNSObject = unsafeCastId

-- ---------- AVAudioConverter ----------

-- | AVAudioConverter
--
-- Converts streams of audio between various formats.
-- 
-- Phantom type for @AVAudioConverter@.
data AVAudioConverter

instance IsObjCObject (Id AVAudioConverter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioConverter"

class IsNSObject a => IsAVAudioConverter a where
  toAVAudioConverter :: a -> Id AVAudioConverter

instance IsAVAudioConverter (Id AVAudioConverter) where
  toAVAudioConverter = unsafeCastId

instance IsNSObject (Id AVAudioConverter) where
  toNSObject = unsafeCastId

-- ---------- AVAudioEngine ----------

-- | AVAudioEngine
--
-- An AVAudioEngine contains a group of connected AVAudioNodes ("nodes"), each of which performs	an audio signal generation, processing, or input/output task.
--
-- Nodes are created separately and attached to the engine.
--
-- The engine supports dynamic connection, disconnection and removal of nodes while running,	with only minor limitations:	- all dynamic reconnections must occur upstream of a mixer	- while removals of effects will normally result in the automatic connection of the adjacent		nodes, removal of a node which has differing input vs. output channel counts, or which		is a mixer, is likely to result in a broken graph.
--
-- By default, the engine is connected to an audio device and automatically renders in realtime. 	It can also be configured to operate in manual rendering mode, i.e. not connected to an	audio device and rendering in response to requests from the client, normally at or	faster than realtime rate.
-- 
-- Phantom type for @AVAudioEngine@.
data AVAudioEngine

instance IsObjCObject (Id AVAudioEngine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioEngine"

class IsNSObject a => IsAVAudioEngine a where
  toAVAudioEngine :: a -> Id AVAudioEngine

instance IsAVAudioEngine (Id AVAudioEngine) where
  toAVAudioEngine = unsafeCastId

instance IsNSObject (Id AVAudioEngine) where
  toNSObject = unsafeCastId

-- ---------- AVAudioEnvironmentDistanceAttenuationParameters ----------

-- | AVAudioEnvironmentDistanceAttenuationParameters
--
-- Parameters specifying the amount of distance attenuation
--
-- A standalone instance of AVAudioEnvironmentDistanceAttenuationParameters cannot be created.         Only an instance vended out by a source object (e.g. AVAudioEnvironmentNode) can be used.
-- 
-- Phantom type for @AVAudioEnvironmentDistanceAttenuationParameters@.
data AVAudioEnvironmentDistanceAttenuationParameters

instance IsObjCObject (Id AVAudioEnvironmentDistanceAttenuationParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioEnvironmentDistanceAttenuationParameters"

class IsNSObject a => IsAVAudioEnvironmentDistanceAttenuationParameters a where
  toAVAudioEnvironmentDistanceAttenuationParameters :: a -> Id AVAudioEnvironmentDistanceAttenuationParameters

instance IsAVAudioEnvironmentDistanceAttenuationParameters (Id AVAudioEnvironmentDistanceAttenuationParameters) where
  toAVAudioEnvironmentDistanceAttenuationParameters = unsafeCastId

instance IsNSObject (Id AVAudioEnvironmentDistanceAttenuationParameters) where
  toNSObject = unsafeCastId

-- ---------- AVAudioEnvironmentReverbParameters ----------

-- | AVAudioEnvironmentReverbParameters
--
-- Parameters used to control the reverb in AVAudioEnvironmentNode
--
-- Reverberation can be used to simulate the acoustic characteristics of an environment.        AVAudioEnvironmentNode has a built in reverb that describes the space that the listener         is in.
--
-- The reverb also has a single filter that sits at the end of the chain. This filter is useful         to shape the overall sound of the reverb. For instance, one of the reverb presets can be         selected to simulate the general space and then the filter can be used to brighten or darken         the overall sound.
--
-- A standalone instance of AVAudioEnvironmentReverbParameters cannot be created.        Only an instance vended out by a source object (e.g. AVAudioEnvironmentNode) can be used.
-- 
-- Phantom type for @AVAudioEnvironmentReverbParameters@.
data AVAudioEnvironmentReverbParameters

instance IsObjCObject (Id AVAudioEnvironmentReverbParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioEnvironmentReverbParameters"

class IsNSObject a => IsAVAudioEnvironmentReverbParameters a where
  toAVAudioEnvironmentReverbParameters :: a -> Id AVAudioEnvironmentReverbParameters

instance IsAVAudioEnvironmentReverbParameters (Id AVAudioEnvironmentReverbParameters) where
  toAVAudioEnvironmentReverbParameters = unsafeCastId

instance IsNSObject (Id AVAudioEnvironmentReverbParameters) where
  toNSObject = unsafeCastId

-- ---------- AVAudioFile ----------

-- | AVAudioFile
--
-- An audio file opened for reading or writing.
--
-- Regardless of the file's actual format, reading and writing the file is done via 		@AVAudioPCMBuffer@ objects, containing samples in an @AVAudioCommonFormat@,		referred to as the file's "processing format." Conversions are performed to and from		the file's actual format.
--
-- Reads and writes are always sequential, but random access is possible by setting the		framePosition property.
-- 
-- Phantom type for @AVAudioFile@.
data AVAudioFile

instance IsObjCObject (Id AVAudioFile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioFile"

class IsNSObject a => IsAVAudioFile a where
  toAVAudioFile :: a -> Id AVAudioFile

instance IsAVAudioFile (Id AVAudioFile) where
  toAVAudioFile = unsafeCastId

instance IsNSObject (Id AVAudioFile) where
  toNSObject = unsafeCastId

-- ---------- AVAudioFormat ----------

-- | AVAudioFormat
--
-- A representation of an audio format.
--
-- AVAudioFormat wraps a Core Audio AudioStreamBasicDescription struct, with convenience		initializers and accessors for common formats, including Core Audio's standard deinterleaved		32-bit floating point.
--
-- Instances of this class are immutable.
-- 
-- Phantom type for @AVAudioFormat@.
data AVAudioFormat

instance IsObjCObject (Id AVAudioFormat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioFormat"

class IsNSObject a => IsAVAudioFormat a where
  toAVAudioFormat :: a -> Id AVAudioFormat

instance IsAVAudioFormat (Id AVAudioFormat) where
  toAVAudioFormat = unsafeCastId

instance IsNSObject (Id AVAudioFormat) where
  toNSObject = unsafeCastId

-- ---------- AVAudioMixingDestination ----------

-- | AVAudioMixingDestination
--
-- An object representing a connection to a mixer node from a node that		conforms to AVAudioMixing protocol
--
-- A standalone instance of AVAudioMixingDestination cannot be created.		Only an instance vended by a source node (e.g. AVAudioPlayerNode) can be used		(see @AVAudioMixing@).
-- 
-- Phantom type for @AVAudioMixingDestination@.
data AVAudioMixingDestination

instance IsObjCObject (Id AVAudioMixingDestination) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioMixingDestination"

class IsNSObject a => IsAVAudioMixingDestination a where
  toAVAudioMixingDestination :: a -> Id AVAudioMixingDestination

instance IsAVAudioMixingDestination (Id AVAudioMixingDestination) where
  toAVAudioMixingDestination = unsafeCastId

instance IsNSObject (Id AVAudioMixingDestination) where
  toNSObject = unsafeCastId

-- ---------- AVAudioNode ----------

-- | AVAudioNode
--
-- Base class for an audio generation, processing, or I/O block.
--
-- @AVAudioEngine@ objects contain instances of various AVAudioNode subclasses. This		base class provides certain common functionality.
--
-- Nodes have input and output busses, which can be thought of as connection points.		For example, an effect typically has one input bus and one output bus. A mixer		typically has multiple input busses and one output bus.
--
-- Busses have formats, expressed in terms of sample rate and channel count. When making		connections between nodes, often the format must match exactly. There are exceptions		(e.g. @AVAudioMixerNode@ and @AVAudioOutputNode@).
--
-- Nodes do not currently provide useful functionality until attached to an engine.
-- 
-- Phantom type for @AVAudioNode@.
data AVAudioNode

instance IsObjCObject (Id AVAudioNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioNode"

class IsNSObject a => IsAVAudioNode a where
  toAVAudioNode :: a -> Id AVAudioNode

instance IsAVAudioNode (Id AVAudioNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioPlayer ----------

-- | Phantom type for @AVAudioPlayer@.
data AVAudioPlayer

instance IsObjCObject (Id AVAudioPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioPlayer"

class IsNSObject a => IsAVAudioPlayer a where
  toAVAudioPlayer :: a -> Id AVAudioPlayer

instance IsAVAudioPlayer (Id AVAudioPlayer) where
  toAVAudioPlayer = unsafeCastId

instance IsNSObject (Id AVAudioPlayer) where
  toNSObject = unsafeCastId

-- ---------- AVAudioRecorder ----------

-- | AVAudioRecorder
--
-- An object that records audio data to a file.
-- 
-- Phantom type for @AVAudioRecorder@.
data AVAudioRecorder

instance IsObjCObject (Id AVAudioRecorder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioRecorder"

class IsNSObject a => IsAVAudioRecorder a where
  toAVAudioRecorder :: a -> Id AVAudioRecorder

instance IsAVAudioRecorder (Id AVAudioRecorder) where
  toAVAudioRecorder = unsafeCastId

instance IsNSObject (Id AVAudioRecorder) where
  toNSObject = unsafeCastId

-- ---------- AVAudioRoutingArbiter ----------

-- | AVAudioRoutingArbiter
--
-- The interface to participate in audio routing arbitration.
-- 
-- Phantom type for @AVAudioRoutingArbiter@.
data AVAudioRoutingArbiter

instance IsObjCObject (Id AVAudioRoutingArbiter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioRoutingArbiter"

class IsNSObject a => IsAVAudioRoutingArbiter a where
  toAVAudioRoutingArbiter :: a -> Id AVAudioRoutingArbiter

instance IsAVAudioRoutingArbiter (Id AVAudioRoutingArbiter) where
  toAVAudioRoutingArbiter = unsafeCastId

instance IsNSObject (Id AVAudioRoutingArbiter) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSequencer ----------

-- | AVAudioSequencer
--
-- A collection of MIDI events organized into AVMusicTracks, plus a player to play back the events.
-- 
-- Phantom type for @AVAudioSequencer@.
data AVAudioSequencer

instance IsObjCObject (Id AVAudioSequencer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSequencer"

class IsNSObject a => IsAVAudioSequencer a where
  toAVAudioSequencer :: a -> Id AVAudioSequencer

instance IsAVAudioSequencer (Id AVAudioSequencer) where
  toAVAudioSequencer = unsafeCastId

instance IsNSObject (Id AVAudioSequencer) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSession ----------

-- | Phantom type for @AVAudioSession@.
data AVAudioSession

instance IsObjCObject (Id AVAudioSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSession"

class IsNSObject a => IsAVAudioSession a where
  toAVAudioSession :: a -> Id AVAudioSession

instance IsAVAudioSession (Id AVAudioSession) where
  toAVAudioSession = unsafeCastId

instance IsNSObject (Id AVAudioSession) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSessionCapability ----------

-- | Describes whether a specific capability is supported and if that capability is currently enabled
-- 
-- Phantom type for @AVAudioSessionCapability@.
data AVAudioSessionCapability

instance IsObjCObject (Id AVAudioSessionCapability) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSessionCapability"

class IsNSObject a => IsAVAudioSessionCapability a where
  toAVAudioSessionCapability :: a -> Id AVAudioSessionCapability

instance IsAVAudioSessionCapability (Id AVAudioSessionCapability) where
  toAVAudioSessionCapability = unsafeCastId

instance IsNSObject (Id AVAudioSessionCapability) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSessionChannelDescription ----------

-- | AVAudioSessionChannelDescription
--
-- Information about a port's audio channels.
--
-- AudioQueue, AURemoteIO and AUVoiceIO instances can be assigned to communicate with specific	hardware channels by setting an array of <port UID, channel index> pairs.
-- 
-- Phantom type for @AVAudioSessionChannelDescription@.
data AVAudioSessionChannelDescription

instance IsObjCObject (Id AVAudioSessionChannelDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSessionChannelDescription"

class IsNSObject a => IsAVAudioSessionChannelDescription a where
  toAVAudioSessionChannelDescription :: a -> Id AVAudioSessionChannelDescription

instance IsAVAudioSessionChannelDescription (Id AVAudioSessionChannelDescription) where
  toAVAudioSessionChannelDescription = unsafeCastId

instance IsNSObject (Id AVAudioSessionChannelDescription) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSessionDataSourceDescription ----------

-- | Information about one of potentially multiple data sources associated with a port.
-- 
-- Phantom type for @AVAudioSessionDataSourceDescription@.
data AVAudioSessionDataSourceDescription

instance IsObjCObject (Id AVAudioSessionDataSourceDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSessionDataSourceDescription"

class IsNSObject a => IsAVAudioSessionDataSourceDescription a where
  toAVAudioSessionDataSourceDescription :: a -> Id AVAudioSessionDataSourceDescription

instance IsAVAudioSessionDataSourceDescription (Id AVAudioSessionDataSourceDescription) where
  toAVAudioSessionDataSourceDescription = unsafeCastId

instance IsNSObject (Id AVAudioSessionDataSourceDescription) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSessionPortDescription ----------

-- | Information about a port, a physical connector or audio device.
-- 
-- Phantom type for @AVAudioSessionPortDescription@.
data AVAudioSessionPortDescription

instance IsObjCObject (Id AVAudioSessionPortDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSessionPortDescription"

class IsNSObject a => IsAVAudioSessionPortDescription a where
  toAVAudioSessionPortDescription :: a -> Id AVAudioSessionPortDescription

instance IsAVAudioSessionPortDescription (Id AVAudioSessionPortDescription) where
  toAVAudioSessionPortDescription = unsafeCastId

instance IsNSObject (Id AVAudioSessionPortDescription) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSessionPortExtensionBluetoothMicrophone ----------

-- | An object that describes capabilities of Bluetooth microphone ports.
-- 
-- Phantom type for @AVAudioSessionPortExtensionBluetoothMicrophone@.
data AVAudioSessionPortExtensionBluetoothMicrophone

instance IsObjCObject (Id AVAudioSessionPortExtensionBluetoothMicrophone) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSessionPortExtensionBluetoothMicrophone"

class IsNSObject a => IsAVAudioSessionPortExtensionBluetoothMicrophone a where
  toAVAudioSessionPortExtensionBluetoothMicrophone :: a -> Id AVAudioSessionPortExtensionBluetoothMicrophone

instance IsAVAudioSessionPortExtensionBluetoothMicrophone (Id AVAudioSessionPortExtensionBluetoothMicrophone) where
  toAVAudioSessionPortExtensionBluetoothMicrophone = unsafeCastId

instance IsNSObject (Id AVAudioSessionPortExtensionBluetoothMicrophone) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSessionRouteDescription ----------

-- | A description of the input and output ports which comprise a route.
-- 
-- Phantom type for @AVAudioSessionRouteDescription@.
data AVAudioSessionRouteDescription

instance IsObjCObject (Id AVAudioSessionRouteDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSessionRouteDescription"

class IsNSObject a => IsAVAudioSessionRouteDescription a where
  toAVAudioSessionRouteDescription :: a -> Id AVAudioSessionRouteDescription

instance IsAVAudioSessionRouteDescription (Id AVAudioSessionRouteDescription) where
  toAVAudioSessionRouteDescription = unsafeCastId

instance IsNSObject (Id AVAudioSessionRouteDescription) where
  toNSObject = unsafeCastId

-- ---------- AVAudioTime ----------

-- | AVAudioTime
--
-- Represent a moment in time.
--
-- AVAudioTime is used in AVAudioEngine to represent time. Instances are immutable.
--
-- A single moment in time may be represented in two different ways:		1. mach_absolute_time(), the system's basic clock. Commonly referred to as "host time."		2. audio samples at a particular sample rate
--
-- A single AVAudioTime instance may contain either or both representations; it might		represent only a sample time, only a host time, or both.
--
-- Rationale for using host time:[a] internally we are using AudioTimeStamp, which uses host time, and it seems silly to divide[b] it is consistent with a standard system timing service[c] we do provide conveniences to convert between host ticks and seconds (host time divided by	frequency) so client code wanting to do what should be straightforward time computations can at 	least not be cluttered by ugly multiplications and divisions by the host clock frequency.
-- 
-- Phantom type for @AVAudioTime@.
data AVAudioTime

instance IsObjCObject (Id AVAudioTime) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioTime"

class IsNSObject a => IsAVAudioTime a where
  toAVAudioTime :: a -> Id AVAudioTime

instance IsAVAudioTime (Id AVAudioTime) where
  toAVAudioTime = unsafeCastId

instance IsNSObject (Id AVAudioTime) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitComponent ----------

-- | AVAudioUnitComponent
--
-- Provides details about an audio unit such as type, subtype, manufacturer, location etc. User	 tags can be added to the AVAudioUnitComponent which can be queried later for display.
-- 
-- Phantom type for @AVAudioUnitComponent@.
data AVAudioUnitComponent

instance IsObjCObject (Id AVAudioUnitComponent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitComponent"

class IsNSObject a => IsAVAudioUnitComponent a where
  toAVAudioUnitComponent :: a -> Id AVAudioUnitComponent

instance IsAVAudioUnitComponent (Id AVAudioUnitComponent) where
  toAVAudioUnitComponent = unsafeCastId

instance IsNSObject (Id AVAudioUnitComponent) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitComponentManager ----------

-- | AVAudioUnitComponentManager
--
-- A singleton object that provides an easy way to find audio components that are			registered with the system.
--
-- AVAudioUnitComponentManager provides methods to search and query various information about the	audio components without opening them.
--
-- Currently audio components that are audio units can only be searched.
--
-- The class also supports predefined system tags and arbitrary user tags. Each audio unit can be	tagged as part of its definition. Refer to AudioComponent.h for more details. AudioUnit Hosts	such as Logic or GarageBand can present groupings of audio units based on the tags.
--
-- Searching for audio units can be done in various ways		- using a NSPredicate that contains search strings for tags or descriptions		- using a block to match on custom criteria		- using an AudioComponentDescription
-- 
-- Phantom type for @AVAudioUnitComponentManager@.
data AVAudioUnitComponentManager

instance IsObjCObject (Id AVAudioUnitComponentManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitComponentManager"

class IsNSObject a => IsAVAudioUnitComponentManager a where
  toAVAudioUnitComponentManager :: a -> Id AVAudioUnitComponentManager

instance IsAVAudioUnitComponentManager (Id AVAudioUnitComponentManager) where
  toAVAudioUnitComponentManager = unsafeCastId

instance IsNSObject (Id AVAudioUnitComponentManager) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitEQFilterParameters ----------

-- | AVAudioUnitEQFilterParameters
--
-- Filter parameters used by AVAudioUnitEQ.
--
-- A standalone instance of AVAudioUnitEQFilterParameters cannot be created. Only an instance        vended out by a source object (e.g. AVAudioUnitEQ) can be used.
-- 
-- Phantom type for @AVAudioUnitEQFilterParameters@.
data AVAudioUnitEQFilterParameters

instance IsObjCObject (Id AVAudioUnitEQFilterParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitEQFilterParameters"

class IsNSObject a => IsAVAudioUnitEQFilterParameters a where
  toAVAudioUnitEQFilterParameters :: a -> Id AVAudioUnitEQFilterParameters

instance IsAVAudioUnitEQFilterParameters (Id AVAudioUnitEQFilterParameters) where
  toAVAudioUnitEQFilterParameters = unsafeCastId

instance IsNSObject (Id AVAudioUnitEQFilterParameters) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIPlayer ----------

-- | AVMIDIPlayer
--
-- A player for music file formats (MIDI, iMelody).
-- 
-- Phantom type for @AVMIDIPlayer@.
data AVMIDIPlayer

instance IsObjCObject (Id AVMIDIPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIPlayer"

class IsNSObject a => IsAVMIDIPlayer a where
  toAVMIDIPlayer :: a -> Id AVMIDIPlayer

instance IsAVMIDIPlayer (Id AVMIDIPlayer) where
  toAVMIDIPlayer = unsafeCastId

instance IsNSObject (Id AVMIDIPlayer) where
  toNSObject = unsafeCastId

-- ---------- AVMusicEvent ----------

-- | AVMusicEvent
--
-- The base class for all events associated with an AVMusicTrack.
--
-- This class is provided to allow enumeration of the heterogenous events contained within an AVMusicTrack.
-- 
-- Phantom type for @AVMusicEvent@.
data AVMusicEvent

instance IsObjCObject (Id AVMusicEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMusicEvent"

class IsNSObject a => IsAVMusicEvent a where
  toAVMusicEvent :: a -> Id AVMusicEvent

instance IsAVMusicEvent (Id AVMusicEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMusicEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMusicTrack ----------

-- | AVMusicTrack
--
-- A collection of music events which will be sent to a given destination, and which can be				offset, muted, etc. independently of events in other tracks.
--
-- AVMusicTrack is not a container of AVMusicEvents - it will not hold references to				AVMusicEvents that are added, so an application should maintain its own if it is				desired.
-- 
-- Phantom type for @AVMusicTrack@.
data AVMusicTrack

instance IsObjCObject (Id AVMusicTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMusicTrack"

class IsNSObject a => IsAVMusicTrack a where
  toAVMusicTrack :: a -> Id AVMusicTrack

instance IsAVMusicTrack (Id AVMusicTrack) where
  toAVMusicTrack = unsafeCastId

instance IsNSObject (Id AVMusicTrack) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechSynthesisMarker ----------

-- | Phantom type for @AVSpeechSynthesisMarker@.
data AVSpeechSynthesisMarker

instance IsObjCObject (Id AVSpeechSynthesisMarker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechSynthesisMarker"

class IsNSObject a => IsAVSpeechSynthesisMarker a where
  toAVSpeechSynthesisMarker :: a -> Id AVSpeechSynthesisMarker

instance IsAVSpeechSynthesisMarker (Id AVSpeechSynthesisMarker) where
  toAVSpeechSynthesisMarker = unsafeCastId

instance IsNSObject (Id AVSpeechSynthesisMarker) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechSynthesisProviderRequest ----------

-- | An @AVSpeechSynthesisProviderRequest@ gets delivered to an @AVSpeechSynthesisProviderAudioUnit@ in order to synthesize audio.    This is distinct from an @AVSpeechUtterance,@ which is a generic utterance to be spoken.
-- 
-- Phantom type for @AVSpeechSynthesisProviderRequest@.
data AVSpeechSynthesisProviderRequest

instance IsObjCObject (Id AVSpeechSynthesisProviderRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechSynthesisProviderRequest"

class IsNSObject a => IsAVSpeechSynthesisProviderRequest a where
  toAVSpeechSynthesisProviderRequest :: a -> Id AVSpeechSynthesisProviderRequest

instance IsAVSpeechSynthesisProviderRequest (Id AVSpeechSynthesisProviderRequest) where
  toAVSpeechSynthesisProviderRequest = unsafeCastId

instance IsNSObject (Id AVSpeechSynthesisProviderRequest) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechSynthesisProviderVoice ----------

-- | The representation of a provided voice that is available for speech synthesis.
--
-- @AVSpeechSynthesisProviderVoice@ is distinct from @AVSpeechSynthesisVoice,@ in that it is a voice provided to the system by an @AVSpeechSynthesisProviderAudioUnit.@         An @AVSpeechSynthesisProviderVoice@ will surface as an @AVSpeechSynthesisVoice@ when using @AVSpeechSynthesisVoice.speechVoices().@ The quality will always be listed as @.enhanced@
-- 
-- Phantom type for @AVSpeechSynthesisProviderVoice@.
data AVSpeechSynthesisProviderVoice

instance IsObjCObject (Id AVSpeechSynthesisProviderVoice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechSynthesisProviderVoice"

class IsNSObject a => IsAVSpeechSynthesisProviderVoice a where
  toAVSpeechSynthesisProviderVoice :: a -> Id AVSpeechSynthesisProviderVoice

instance IsAVSpeechSynthesisProviderVoice (Id AVSpeechSynthesisProviderVoice) where
  toAVSpeechSynthesisProviderVoice = unsafeCastId

instance IsNSObject (Id AVSpeechSynthesisProviderVoice) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechSynthesisVoice ----------

-- | AVSpeechSynthesisVoice
--
-- AVSpeechSynthesisVoice encapsulates the attributes of the voice used to synthesize speech on the system.
--
-- Retrieve a voice by specifying the language code your text should be spoken in, or by using voiceWithIdentifier for a known voice identifier.
-- 
-- Phantom type for @AVSpeechSynthesisVoice@.
data AVSpeechSynthesisVoice

instance IsObjCObject (Id AVSpeechSynthesisVoice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechSynthesisVoice"

class IsNSObject a => IsAVSpeechSynthesisVoice a where
  toAVSpeechSynthesisVoice :: a -> Id AVSpeechSynthesisVoice

instance IsAVSpeechSynthesisVoice (Id AVSpeechSynthesisVoice) where
  toAVSpeechSynthesisVoice = unsafeCastId

instance IsNSObject (Id AVSpeechSynthesisVoice) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechSynthesizer ----------

-- | AVSpeechSynthesizer
--
-- AVSpeechSynthesizer allows speaking of speech utterances with a basic queuing mechanism.
--
-- Create an instance of AVSpeechSynthesizer to start generating synthesized speech by using AVSpeechUtterance objects.
-- 
-- Phantom type for @AVSpeechSynthesizer@.
data AVSpeechSynthesizer

instance IsObjCObject (Id AVSpeechSynthesizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechSynthesizer"

class IsNSObject a => IsAVSpeechSynthesizer a where
  toAVSpeechSynthesizer :: a -> Id AVSpeechSynthesizer

instance IsAVSpeechSynthesizer (Id AVSpeechSynthesizer) where
  toAVSpeechSynthesizer = unsafeCastId

instance IsNSObject (Id AVSpeechSynthesizer) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechUtterance ----------

-- | AVSpeechUtterance
--
-- AVSpeechUtterance is the atom of speaking a string or pausing the synthesizer.
--
-- To start speaking, specify the AVSpeechSynthesisVoice and the string to be spoken, then optionally change the rate, pitch or volume if desired.
-- 
-- Phantom type for @AVSpeechUtterance@.
data AVSpeechUtterance

instance IsObjCObject (Id AVSpeechUtterance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechUtterance"

class IsNSObject a => IsAVSpeechUtterance a where
  toAVSpeechUtterance :: a -> Id AVSpeechUtterance

instance IsAVSpeechUtterance (Id AVSpeechUtterance) where
  toAVSpeechUtterance = unsafeCastId

instance IsNSObject (Id AVSpeechUtterance) where
  toNSObject = unsafeCastId

-- ---------- AVSpeechSynthesisProviderAudioUnit ----------

-- | Phantom type for @AVSpeechSynthesisProviderAudioUnit@.
data AVSpeechSynthesisProviderAudioUnit

instance IsObjCObject (Id AVSpeechSynthesisProviderAudioUnit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpeechSynthesisProviderAudioUnit"

class IsAUAudioUnit a => IsAVSpeechSynthesisProviderAudioUnit a where
  toAVSpeechSynthesisProviderAudioUnit :: a -> Id AVSpeechSynthesisProviderAudioUnit

instance IsAVSpeechSynthesisProviderAudioUnit (Id AVSpeechSynthesisProviderAudioUnit) where
  toAVSpeechSynthesisProviderAudioUnit = unsafeCastId

instance IsAUAudioUnit (Id AVSpeechSynthesisProviderAudioUnit) where
  toAUAudioUnit = unsafeCastId

instance IsNSObject (Id AVSpeechSynthesisProviderAudioUnit) where
  toNSObject = unsafeCastId

-- ---------- AVAudioCompressedBuffer ----------

-- | AVAudioCompressedBuffer
--
-- A subclass of AVAudioBuffer for use with compressed audio formats.
-- 
-- Phantom type for @AVAudioCompressedBuffer@.
data AVAudioCompressedBuffer

instance IsObjCObject (Id AVAudioCompressedBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioCompressedBuffer"

class IsAVAudioBuffer a => IsAVAudioCompressedBuffer a where
  toAVAudioCompressedBuffer :: a -> Id AVAudioCompressedBuffer

instance IsAVAudioCompressedBuffer (Id AVAudioCompressedBuffer) where
  toAVAudioCompressedBuffer = unsafeCastId

instance IsAVAudioBuffer (Id AVAudioCompressedBuffer) where
  toAVAudioBuffer = unsafeCastId

instance IsNSObject (Id AVAudioCompressedBuffer) where
  toNSObject = unsafeCastId

-- ---------- AVAudioPCMBuffer ----------

-- | AVAudioPCMBuffer
--
-- A subclass of AVAudioBuffer for use with PCM audio formats.
--
-- AVAudioPCMBuffer provides a number of methods useful for manipulating buffers of		audio in PCM format.
-- 
-- Phantom type for @AVAudioPCMBuffer@.
data AVAudioPCMBuffer

instance IsObjCObject (Id AVAudioPCMBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioPCMBuffer"

class IsAVAudioBuffer a => IsAVAudioPCMBuffer a where
  toAVAudioPCMBuffer :: a -> Id AVAudioPCMBuffer

instance IsAVAudioPCMBuffer (Id AVAudioPCMBuffer) where
  toAVAudioPCMBuffer = unsafeCastId

instance IsAVAudioBuffer (Id AVAudioPCMBuffer) where
  toAVAudioBuffer = unsafeCastId

instance IsNSObject (Id AVAudioPCMBuffer) where
  toNSObject = unsafeCastId

-- ---------- AVAudioEnvironmentNode ----------

-- | AVAudioEnvironmentNode
--
-- Mixer node that simulates a 3D environment
--
-- AVAudioEnvironmentNode is a mixer node that simulates a 3D audio environment. Any node that         conforms to the AVAudioMixing protocol (e.g. AVAudioPlayerNode) can act as a source in this        environment.
--
-- The environment has an implicit "listener". By controlling the listener's position and        orientation, the application controls the way the user experiences the virtual world.         In addition, this node also defines properties for distance attenuation and reverberation         that help characterize the environment.
--
-- It is important to note that AVAudio3DMixingSourceMode affects how inputs with different channel        configurations are rendered. By default, only inputs with a mono channel are spatialized.
--
-- In order to set the environment node’s output to a multichannel format, use an AVAudioFormat        with a desired AudioChannelLayout.
-- 
-- Phantom type for @AVAudioEnvironmentNode@.
data AVAudioEnvironmentNode

instance IsObjCObject (Id AVAudioEnvironmentNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioEnvironmentNode"

class IsAVAudioNode a => IsAVAudioEnvironmentNode a where
  toAVAudioEnvironmentNode :: a -> Id AVAudioEnvironmentNode

instance IsAVAudioEnvironmentNode (Id AVAudioEnvironmentNode) where
  toAVAudioEnvironmentNode = unsafeCastId

instance IsAVAudioNode (Id AVAudioEnvironmentNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioEnvironmentNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioIONode ----------

-- | AVAudioIONode
--
-- Base class for a node that performs audio input or output in the engine.
--
-- When the engine is configured to render to/from an audio device, on macOS, AVAudioInputNode 		and AVAudioOutputNode communicate with the system's default input and output devices. 		On iOS, they communicate with the devices appropriate to the app's AVAudioSession category 		and other configuration, also considering the user's actions such as 		connecting/disconnecting external devices.
--
-- In the manual rendering mode, the AVAudioInputNode and AVAudioOutputNode perform the input		and output in the engine, in response to client's request.
-- 
-- Phantom type for @AVAudioIONode@.
data AVAudioIONode

instance IsObjCObject (Id AVAudioIONode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioIONode"

class IsAVAudioNode a => IsAVAudioIONode a where
  toAVAudioIONode :: a -> Id AVAudioIONode

instance IsAVAudioIONode (Id AVAudioIONode) where
  toAVAudioIONode = unsafeCastId

instance IsAVAudioNode (Id AVAudioIONode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioIONode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioMixerNode ----------

-- | AVAudioMixerNode
--
-- A node that mixes its inputs to a single output.
--
-- Mixers may have any number of inputs.
--
-- The mixer accepts input at any sample rate and efficiently combines sample rate		conversions. It also accepts any channel count and will correctly upmix or downmix		to the output channel count.
-- 
-- Phantom type for @AVAudioMixerNode@.
data AVAudioMixerNode

instance IsObjCObject (Id AVAudioMixerNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioMixerNode"

class IsAVAudioNode a => IsAVAudioMixerNode a where
  toAVAudioMixerNode :: a -> Id AVAudioMixerNode

instance IsAVAudioMixerNode (Id AVAudioMixerNode) where
  toAVAudioMixerNode = unsafeCastId

instance IsAVAudioNode (Id AVAudioMixerNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioMixerNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioPlayerNode ----------

-- | AVAudioPlayerNode
--
-- Play buffers or segments of audio files.
--
-- AVAudioPlayerNode supports scheduling the playback of @AVAudioBuffer@ instances,		or segments of audio files opened via @AVAudioFile@. Buffers and segments may be		scheduled at specific points in time, or to play immediately following preceding segments.
--
-- FORMATS
--
-- Normally, you will want to configure the node's output format with the same number of		channels as are in the files and buffers to be played. Otherwise, channels will be dropped		or added as required. It is usually better to use an @AVAudioMixerNode@ to		do this.
--
-- Similarly, when playing file segments, the node will sample rate convert if necessary, but		it is often preferable to configure the node's output sample rate to match that of the file(s)		and use a mixer to perform the rate conversion.
--
-- When playing buffers, there is an implicit assumption that the buffers are at the same		sample rate as the node's output format.
--
-- TIMELINES
--
-- The usual @AVAudioNode@ sample times (as observed by @lastRenderTime@)		have an arbitrary zero point. AVAudioPlayerNode superimposes a second "player timeline" on		top of this, to reflect when the player was started, and intervals during which it was		paused. The methods @nodeTimeForPlayerTime:@ and @playerTimeForNodeTime:@		convert between the two.
--
-- This class' @stop@ method unschedules all previously scheduled buffers and		file segments, and returns the player timeline to sample time 0.
--
-- TIMESTAMPS
--
-- The "schedule" methods all take an @AVAudioTime@ "when" parameter. This is		interpreted as follows:
--
-- 1. nil:			- if there have been previous commands, the new one is played immediately following the				last one.			- otherwise, if the node is playing, the event is played in the very near future.			- otherwise, the command is played at sample time 0.		2. sample time:			- relative to the node's start time (which begins at 0 when the node is started).		3. host time:			- ignored unless the sample time is invalid when the engine is rendering to an audio 			  device.			- ignored in manual rendering mode.
--
-- ERRORS
--
-- The "schedule" methods can fail if:
--
-- 1. a buffer's channel count does not match that of the node's output format.		2. a file can't be accessed.		3. an AVAudioTime specifies neither a valid sample time or host time.		4. a segment's start frame or frame count is negative.
--
-- BUFFER/FILE COMPLETION HANDLERS
--
-- The buffer or file completion handlers (see scheduling methods) are a means to schedule 		more data if available on the player node. See @AVAudioPlayerNodeCompletionCallbackType@ 		for details on the different buffer/file completion callback types.
--
-- Note that a player should not be stopped from within a completion handler callback because		it can deadlock while trying to unschedule previously scheduled buffers.
--
-- OFFLINE RENDERING
--
-- When a player node is used with the engine operating in the manual rendering mode, the		buffer/file completion handlers, @lastRenderTime@ and the latencies (@latency@ and		@outputPresentationLatency@) can be used to track how much data the player has rendered and		how much more data is left to render.
-- 
-- Phantom type for @AVAudioPlayerNode@.
data AVAudioPlayerNode

instance IsObjCObject (Id AVAudioPlayerNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioPlayerNode"

class IsAVAudioNode a => IsAVAudioPlayerNode a where
  toAVAudioPlayerNode :: a -> Id AVAudioPlayerNode

instance IsAVAudioPlayerNode (Id AVAudioPlayerNode) where
  toAVAudioPlayerNode = unsafeCastId

instance IsAVAudioNode (Id AVAudioPlayerNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioPlayerNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSinkNode ----------

-- | AVAudioSinkNode
--
-- AVAudioSinkNode wraps a client provided block to receive input audio on the audio IO thread.
--
-- AVAudioSinkNode is restricted to be used in the input chain and does not support format        conversion. Hence when connecting to an AVAudioSinkNode node, the format for the connection        should be the output scope format of the input node (essentialy the format should match the input hardware 		sample rate).
--
-- The voice processing IO unit is an exception to the above as it supports sample rate conversion.         The input scope format (HW format) and output scope format (client format) of the input node can differ         in that case.
--
-- This node is only supported when the engine is rendering to the audio device and not in        manual rendering mode.
--
-- AVAudioSinkNode does not have an output bus and therefore it does not support tapping.
-- 
-- Phantom type for @AVAudioSinkNode@.
data AVAudioSinkNode

instance IsObjCObject (Id AVAudioSinkNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSinkNode"

class IsAVAudioNode a => IsAVAudioSinkNode a where
  toAVAudioSinkNode :: a -> Id AVAudioSinkNode

instance IsAVAudioSinkNode (Id AVAudioSinkNode) where
  toAVAudioSinkNode = unsafeCastId

instance IsAVAudioNode (Id AVAudioSinkNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioSinkNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioSourceNode ----------

-- | AVAudioSourceNode
--
-- AVAudioSourceNode wraps a client provided block to supply audio.
--
-- With AVAudioSourceNode the client can supply audio data for rendering through an        AVAudioSourceNodeRenderBlock block.        This is similar to setting the input callback on an Audio Unit with the        kAudioUnitProperty_SetRenderCallback property.
-- 
-- Phantom type for @AVAudioSourceNode@.
data AVAudioSourceNode

instance IsObjCObject (Id AVAudioSourceNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioSourceNode"

class IsAVAudioNode a => IsAVAudioSourceNode a where
  toAVAudioSourceNode :: a -> Id AVAudioSourceNode

instance IsAVAudioSourceNode (Id AVAudioSourceNode) where
  toAVAudioSourceNode = unsafeCastId

instance IsAVAudioNode (Id AVAudioSourceNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioSourceNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnit ----------

-- | AVAudioUnit
--
-- An AVAudioNode implemented by an audio unit.
--
-- An AVAudioUnit is an AVAudioNode implemented by an audio unit. Depending on the type of        the audio unit, audio is processed either in real-time or non real-time.
-- 
-- Phantom type for @AVAudioUnit@.
data AVAudioUnit

instance IsObjCObject (Id AVAudioUnit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnit"

class IsAVAudioNode a => IsAVAudioUnit a where
  toAVAudioUnit :: a -> Id AVAudioUnit

instance IsAVAudioUnit (Id AVAudioUnit) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnit) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioUnit) where
  toNSObject = unsafeCastId

-- ---------- AVAUPresetEvent ----------

-- | AVAUPresetEvent
--
-- The event class representing a preset load and change on the AVMusicTrack's destinationAudioUnit.
--
-- AVAUPresetEvents make it possible to schedule and/or automate preset changes on the audio unit		that has been configured as the destination for the AVMusicTrack containing this event.
-- 
-- Phantom type for @AVAUPresetEvent@.
data AVAUPresetEvent

instance IsObjCObject (Id AVAUPresetEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAUPresetEvent"

class IsAVMusicEvent a => IsAVAUPresetEvent a where
  toAVAUPresetEvent :: a -> Id AVAUPresetEvent

instance IsAVAUPresetEvent (Id AVAUPresetEvent) where
  toAVAUPresetEvent = unsafeCastId

instance IsAVMusicEvent (Id AVAUPresetEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVAUPresetEvent) where
  toNSObject = unsafeCastId

-- ---------- AVExtendedNoteOnEvent ----------

-- | AVExtendedNoteOnEvent
--
-- The event class representing a custom extension of a MIDI note-on.
--
-- Using an AVExtendedNoteOnEvent allows an application to trigger a specialized note-on event on one of several		Apple audio units which support it.  The floating point note and velocity numbers allow optional fractional control		of the note's run-time properties which are modulated by those inputs.  In addition, it supports the possibility		of an audio unit with more than the standard 16 MIDI channels.
-- 
-- Phantom type for @AVExtendedNoteOnEvent@.
data AVExtendedNoteOnEvent

instance IsObjCObject (Id AVExtendedNoteOnEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExtendedNoteOnEvent"

class IsAVMusicEvent a => IsAVExtendedNoteOnEvent a where
  toAVExtendedNoteOnEvent :: a -> Id AVExtendedNoteOnEvent

instance IsAVExtendedNoteOnEvent (Id AVExtendedNoteOnEvent) where
  toAVExtendedNoteOnEvent = unsafeCastId

instance IsAVMusicEvent (Id AVExtendedNoteOnEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVExtendedNoteOnEvent) where
  toNSObject = unsafeCastId

-- ---------- AVExtendedTempoEvent ----------

-- | AVExtendedTempoEvent
--
-- The event class representing a tempo change to a specific beats-per-minute value.
--
-- This event provides a way to specify a tempo change that is less cumbersome than using		tempo meta-events.
-- 
-- Phantom type for @AVExtendedTempoEvent@.
data AVExtendedTempoEvent

instance IsObjCObject (Id AVExtendedTempoEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExtendedTempoEvent"

class IsAVMusicEvent a => IsAVExtendedTempoEvent a where
  toAVExtendedTempoEvent :: a -> Id AVExtendedTempoEvent

instance IsAVExtendedTempoEvent (Id AVExtendedTempoEvent) where
  toAVExtendedTempoEvent = unsafeCastId

instance IsAVMusicEvent (Id AVExtendedTempoEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVExtendedTempoEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIChannelEvent ----------

-- | AVMIDIChannelEvent
--
-- The event base class for all MIDI messages which operate on a single MIDI channel.
-- 
-- Phantom type for @AVMIDIChannelEvent@.
data AVMIDIChannelEvent

instance IsObjCObject (Id AVMIDIChannelEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIChannelEvent"

class IsAVMusicEvent a => IsAVMIDIChannelEvent a where
  toAVMIDIChannelEvent :: a -> Id AVMIDIChannelEvent

instance IsAVMIDIChannelEvent (Id AVMIDIChannelEvent) where
  toAVMIDIChannelEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIChannelEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIChannelEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIMetaEvent ----------

-- | AVMIDIMetaEvent
--
-- The event class representing MIDI Meta-Event messages.
--
-- The size and contents of an AVMIDIMetaEvent cannot be modified once created.
--
-- Events with AVMIDIMetaEventType AVMIDIMetaEventTypeTempo, AVMIDIMetaEventTypeSmpteOffset,		or AVMIDIMetaEventTypeTimeSignature can only be added to a sequence's tempo track.
--
-- The class does not verify that the content matches the MIDI specification.
-- 
-- Phantom type for @AVMIDIMetaEvent@.
data AVMIDIMetaEvent

instance IsObjCObject (Id AVMIDIMetaEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIMetaEvent"

class IsAVMusicEvent a => IsAVMIDIMetaEvent a where
  toAVMIDIMetaEvent :: a -> Id AVMIDIMetaEvent

instance IsAVMIDIMetaEvent (Id AVMIDIMetaEvent) where
  toAVMIDIMetaEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIMetaEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIMetaEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDINoteEvent ----------

-- | AVMIDINoteEvent
--
-- The event class representing MIDI note-on/off messages.
--
-- @channel@ — The MIDI channel for the note.  Range: 0-15.
--
-- @key@ — The MIDI key number for the note.  Range: 0-127.
--
-- @velocity@ — The MIDI velocity for the note.  Range: 0-127 (see discussion).
--
-- @duration@ — The duration of this note event in AVMusicTimeStamp beats.  Range: Any non-negative number.
--
-- The AVAudioSequencer will automatically send a MIDI note-off after the note duration has passed.		To send an explicit note-off event, create an AVMIDINoteEvent with its velocity set to zero.
-- 
-- Phantom type for @AVMIDINoteEvent@.
data AVMIDINoteEvent

instance IsObjCObject (Id AVMIDINoteEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDINoteEvent"

class IsAVMusicEvent a => IsAVMIDINoteEvent a where
  toAVMIDINoteEvent :: a -> Id AVMIDINoteEvent

instance IsAVMIDINoteEvent (Id AVMIDINoteEvent) where
  toAVMIDINoteEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDINoteEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDINoteEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDISysexEvent ----------

-- | AVMIDISysexEvent
--
-- The event class representing MIDI system exclusive messages.
--
-- The size and contents of an AVMIDISysexEvent cannot be modified once created.
-- 
-- Phantom type for @AVMIDISysexEvent@.
data AVMIDISysexEvent

instance IsObjCObject (Id AVMIDISysexEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDISysexEvent"

class IsAVMusicEvent a => IsAVMIDISysexEvent a where
  toAVMIDISysexEvent :: a -> Id AVMIDISysexEvent

instance IsAVMIDISysexEvent (Id AVMIDISysexEvent) where
  toAVMIDISysexEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDISysexEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDISysexEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMusicUserEvent ----------

-- | AVMusicUserEvent
--
-- The event class representing custom user messages.
--
-- When a scheduled AVMusicUserEvent is reached during playback of a AVMusicTrack, the track's		user callback block will be called if it has been set.  The event's NSData will be provided as		an argument to that block.		The size and contents of an AVMusicUserEvent cannot be modified once created.
-- 
-- Phantom type for @AVMusicUserEvent@.
data AVMusicUserEvent

instance IsObjCObject (Id AVMusicUserEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMusicUserEvent"

class IsAVMusicEvent a => IsAVMusicUserEvent a where
  toAVMusicUserEvent :: a -> Id AVMusicUserEvent

instance IsAVMusicUserEvent (Id AVMusicUserEvent) where
  toAVMusicUserEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMusicUserEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMusicUserEvent) where
  toNSObject = unsafeCastId

-- ---------- AVParameterEvent ----------

-- | AVParameterEvent
--
-- The event class representing a parameter set/change event on the AVMusicTrack's destinationAudioUnit.
--
-- AVParameterEvents make it possible to schedule and/or automate parameter changes on the audio unit		that has been configured as the destination for the AVMusicTrack containing this event.
--
-- When the track is played as part of a sequence, the destination audio unit will receive set-parameter		messages whose values change smoothly along a linear ramp between each event's beat location.
--
-- If an AVParameterEvent is added to an empty, non-automation track, the track becomes an automation track.
-- 
-- Phantom type for @AVParameterEvent@.
data AVParameterEvent

instance IsObjCObject (Id AVParameterEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVParameterEvent"

class IsAVMusicEvent a => IsAVParameterEvent a where
  toAVParameterEvent :: a -> Id AVParameterEvent

instance IsAVParameterEvent (Id AVParameterEvent) where
  toAVParameterEvent = unsafeCastId

instance IsAVMusicEvent (Id AVParameterEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVParameterEvent) where
  toNSObject = unsafeCastId

-- ---------- AVAudioInputNode ----------

-- | AVAudioInputNode
--
-- A node that performs audio input in the engine.
--
-- When the engine is rendering to/from an audio device, this node connects to the system's 		audio input.		When the engine is operating in manual rendering mode, this node can be used to supply		the input data to the engine.
--
-- This node has one element.		The format of the input scope reflects:			- the audio hardware sample rate and channel count, when connected to the hardware			- the format of the PCM audio data that the node will supply to the engine, in the			  manual rendering mode (see @setManualRenderingInputPCMFormat:inputBlock:@)
--
-- When rendering from an audio device, the input node does not support format conversion.		Hence the format of the output scope must be same as that of the input, as well as the		formats for all the nodes connected in the input node chain.
--
-- In the manual rendering mode, the format of the output scope is initially the same as that		of the input, but you may set it to a different format, in which case the node will convert.
-- 
-- Phantom type for @AVAudioInputNode@.
data AVAudioInputNode

instance IsObjCObject (Id AVAudioInputNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioInputNode"

class IsAVAudioIONode a => IsAVAudioInputNode a where
  toAVAudioInputNode :: a -> Id AVAudioInputNode

instance IsAVAudioInputNode (Id AVAudioInputNode) where
  toAVAudioInputNode = unsafeCastId

instance IsAVAudioIONode (Id AVAudioInputNode) where
  toAVAudioIONode = unsafeCastId

instance IsAVAudioNode (Id AVAudioInputNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioInputNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioOutputNode ----------

-- | AVAudioOutputNode
--
-- A node that performs audio output in the engine.
--
-- When the engine is rendering to/from an audio device, this node connects to the system's 		audio output.		When the engine is operating in manual rendering mode, this node performs output in		response to client's requests.
--
-- This node has one element.		The format of the output scope reflects:			- the audio hardware sample rate and channel count, when connected to the hardware			- the engine's manual rendering mode output format (see 			  @AVAudioEngine(manualRenderingFormat)@), in the manual rendering mode
--
-- The format of the input scope is initially the same as that of the		output, but you may set it to a different format, in which case the node will convert.
-- 
-- Phantom type for @AVAudioOutputNode@.
data AVAudioOutputNode

instance IsObjCObject (Id AVAudioOutputNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioOutputNode"

class IsAVAudioIONode a => IsAVAudioOutputNode a where
  toAVAudioOutputNode :: a -> Id AVAudioOutputNode

instance IsAVAudioOutputNode (Id AVAudioOutputNode) where
  toAVAudioOutputNode = unsafeCastId

instance IsAVAudioIONode (Id AVAudioOutputNode) where
  toAVAudioIONode = unsafeCastId

instance IsAVAudioNode (Id AVAudioOutputNode) where
  toAVAudioNode = unsafeCastId

instance IsNSObject (Id AVAudioOutputNode) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitEffect ----------

-- | AVAudioUnitEffect
--
-- an AVAudioUnit that processes audio in real-time
--
-- An AVAudioUnitEffect represents an audio unit of type kAudioUnitType_Effect,    kAudioUnitType_MusicEffect, kAudioUnitType_Panner, kAudioUnitType_RemoteEffect or     kAudioUnitType_RemoteMusicEffect.
--
-- These effects run in real-time and process some x number of audio input     samples to produce x number of audio output samples. A delay unit is an     example of an effect unit.
-- 
-- Phantom type for @AVAudioUnitEffect@.
data AVAudioUnitEffect

instance IsObjCObject (Id AVAudioUnitEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitEffect"

class IsAVAudioUnit a => IsAVAudioUnitEffect a where
  toAVAudioUnitEffect :: a -> Id AVAudioUnitEffect

instance IsAVAudioUnitEffect (Id AVAudioUnitEffect) where
  toAVAudioUnitEffect = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitEffect) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitEffect) where
  toAVAudioUnit = unsafeCastId

instance IsNSObject (Id AVAudioUnitEffect) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitGenerator ----------

-- | AVAudioUnitGenerator
--
-- an AVAudioUnit that generates audio output
--
-- An AVAudioUnitGenerator represents an audio unit of type kAudioUnitType_Generator or	kAudioUnitType_RemoteGenerator.    A generator will have no audio input, but will just produce audio output.    A tone generator is an example of this.
-- 
-- Phantom type for @AVAudioUnitGenerator@.
data AVAudioUnitGenerator

instance IsObjCObject (Id AVAudioUnitGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitGenerator"

class IsAVAudioUnit a => IsAVAudioUnitGenerator a where
  toAVAudioUnitGenerator :: a -> Id AVAudioUnitGenerator

instance IsAVAudioUnitGenerator (Id AVAudioUnitGenerator) where
  toAVAudioUnitGenerator = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitGenerator) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitGenerator) where
  toAVAudioUnit = unsafeCastId

instance IsNSObject (Id AVAudioUnitGenerator) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitMIDIInstrument ----------

-- | Base class for MIDI instruments.
-- 
-- Phantom type for @AVAudioUnitMIDIInstrument@.
data AVAudioUnitMIDIInstrument

instance IsObjCObject (Id AVAudioUnitMIDIInstrument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitMIDIInstrument"

class IsAVAudioUnit a => IsAVAudioUnitMIDIInstrument a where
  toAVAudioUnitMIDIInstrument :: a -> Id AVAudioUnitMIDIInstrument

instance IsAVAudioUnitMIDIInstrument (Id AVAudioUnitMIDIInstrument) where
  toAVAudioUnitMIDIInstrument = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitMIDIInstrument) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitMIDIInstrument) where
  toAVAudioUnit = unsafeCastId

instance IsNSObject (Id AVAudioUnitMIDIInstrument) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitTimeEffect ----------

-- | AVAudioUnitTimeEffect
--
-- an AVAudioUnit that processes audio in non real-time
--
-- An AVAudioUnitTimeEffect represents an audio unit of type aufc.    These effects do not process audio in real-time. The varispeed    unit is an example of a time effect unit.
-- 
-- Phantom type for @AVAudioUnitTimeEffect@.
data AVAudioUnitTimeEffect

instance IsObjCObject (Id AVAudioUnitTimeEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitTimeEffect"

class IsAVAudioUnit a => IsAVAudioUnitTimeEffect a where
  toAVAudioUnitTimeEffect :: a -> Id AVAudioUnitTimeEffect

instance IsAVAudioUnitTimeEffect (Id AVAudioUnitTimeEffect) where
  toAVAudioUnitTimeEffect = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitTimeEffect) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitTimeEffect) where
  toAVAudioUnit = unsafeCastId

instance IsNSObject (Id AVAudioUnitTimeEffect) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIChannelPressureEvent ----------

-- | AVMIDIChannelPressureEvent
--
-- The event class representing MIDI channel pressure messages.
--
-- The effect of these messages will depend on the containing AVMusicTrack's destinationAudioUnit		and the capabilities of the destination's currently-loaded instrument.
-- 
-- Phantom type for @AVMIDIChannelPressureEvent@.
data AVMIDIChannelPressureEvent

instance IsObjCObject (Id AVMIDIChannelPressureEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIChannelPressureEvent"

class IsAVMIDIChannelEvent a => IsAVMIDIChannelPressureEvent a where
  toAVMIDIChannelPressureEvent :: a -> Id AVMIDIChannelPressureEvent

instance IsAVMIDIChannelPressureEvent (Id AVMIDIChannelPressureEvent) where
  toAVMIDIChannelPressureEvent = unsafeCastId

instance IsAVMIDIChannelEvent (Id AVMIDIChannelPressureEvent) where
  toAVMIDIChannelEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIChannelPressureEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIChannelPressureEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIControlChangeEvent ----------

-- | AVMIDIControlChangeEvent
--
-- The event class representing MIDI control change messages.
-- 
-- Phantom type for @AVMIDIControlChangeEvent@.
data AVMIDIControlChangeEvent

instance IsObjCObject (Id AVMIDIControlChangeEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIControlChangeEvent"

class IsAVMIDIChannelEvent a => IsAVMIDIControlChangeEvent a where
  toAVMIDIControlChangeEvent :: a -> Id AVMIDIControlChangeEvent

instance IsAVMIDIControlChangeEvent (Id AVMIDIControlChangeEvent) where
  toAVMIDIControlChangeEvent = unsafeCastId

instance IsAVMIDIChannelEvent (Id AVMIDIControlChangeEvent) where
  toAVMIDIChannelEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIControlChangeEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIControlChangeEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIPitchBendEvent ----------

-- | AVMIDIPitchBendEvent
--
-- The event class representing MIDI pitch bend messages.
--
-- The effect of these messages will depend on the AVMusicTrack's destinationAudioUnit		and the capabilities of the destination's currently-loaded instrument.
-- 
-- Phantom type for @AVMIDIPitchBendEvent@.
data AVMIDIPitchBendEvent

instance IsObjCObject (Id AVMIDIPitchBendEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIPitchBendEvent"

class IsAVMIDIChannelEvent a => IsAVMIDIPitchBendEvent a where
  toAVMIDIPitchBendEvent :: a -> Id AVMIDIPitchBendEvent

instance IsAVMIDIPitchBendEvent (Id AVMIDIPitchBendEvent) where
  toAVMIDIPitchBendEvent = unsafeCastId

instance IsAVMIDIChannelEvent (Id AVMIDIPitchBendEvent) where
  toAVMIDIChannelEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIPitchBendEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIPitchBendEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIPolyPressureEvent ----------

-- | AVMIDIPolyPressureEvent
--
-- The event class representing MIDI "poly" or "key" pressure messages.
-- 
-- Phantom type for @AVMIDIPolyPressureEvent@.
data AVMIDIPolyPressureEvent

instance IsObjCObject (Id AVMIDIPolyPressureEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIPolyPressureEvent"

class IsAVMIDIChannelEvent a => IsAVMIDIPolyPressureEvent a where
  toAVMIDIPolyPressureEvent :: a -> Id AVMIDIPolyPressureEvent

instance IsAVMIDIPolyPressureEvent (Id AVMIDIPolyPressureEvent) where
  toAVMIDIPolyPressureEvent = unsafeCastId

instance IsAVMIDIChannelEvent (Id AVMIDIPolyPressureEvent) where
  toAVMIDIChannelEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIPolyPressureEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIPolyPressureEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMIDIProgramChangeEvent ----------

-- | AVMIDIProgramChangeEvent
--
-- The event class representing MIDI program or patch change messages.
--
-- The effect of these messages will depend on the containing AVMusicTrack's destinationAudioUnit.
-- 
-- Phantom type for @AVMIDIProgramChangeEvent@.
data AVMIDIProgramChangeEvent

instance IsObjCObject (Id AVMIDIProgramChangeEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMIDIProgramChangeEvent"

class IsAVMIDIChannelEvent a => IsAVMIDIProgramChangeEvent a where
  toAVMIDIProgramChangeEvent :: a -> Id AVMIDIProgramChangeEvent

instance IsAVMIDIProgramChangeEvent (Id AVMIDIProgramChangeEvent) where
  toAVMIDIProgramChangeEvent = unsafeCastId

instance IsAVMIDIChannelEvent (Id AVMIDIProgramChangeEvent) where
  toAVMIDIChannelEvent = unsafeCastId

instance IsAVMusicEvent (Id AVMIDIProgramChangeEvent) where
  toAVMusicEvent = unsafeCastId

instance IsNSObject (Id AVMIDIProgramChangeEvent) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitDelay ----------

-- | AVAudioUnitDelay
--
-- an AVAudioUnitEffect that implements a delay effect
--
-- A delay unit delays the input signal by the specified time interval        and then blends it with the input signal. The amount of high frequency        roll-off can also be controlled in order to simulate the effect of        a tape delay.
-- 
-- Phantom type for @AVAudioUnitDelay@.
data AVAudioUnitDelay

instance IsObjCObject (Id AVAudioUnitDelay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitDelay"

class IsAVAudioUnitEffect a => IsAVAudioUnitDelay a where
  toAVAudioUnitDelay :: a -> Id AVAudioUnitDelay

instance IsAVAudioUnitDelay (Id AVAudioUnitDelay) where
  toAVAudioUnitDelay = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitDelay) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitDelay) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitEffect (Id AVAudioUnitDelay) where
  toAVAudioUnitEffect = unsafeCastId

instance IsNSObject (Id AVAudioUnitDelay) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitDistortion ----------

-- | AVAudioUnitDistortion
--
-- An AVAudioUnitEffect that implements a multi-stage distortion effect.
-- 
-- Phantom type for @AVAudioUnitDistortion@.
data AVAudioUnitDistortion

instance IsObjCObject (Id AVAudioUnitDistortion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitDistortion"

class IsAVAudioUnitEffect a => IsAVAudioUnitDistortion a where
  toAVAudioUnitDistortion :: a -> Id AVAudioUnitDistortion

instance IsAVAudioUnitDistortion (Id AVAudioUnitDistortion) where
  toAVAudioUnitDistortion = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitDistortion) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitDistortion) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitEffect (Id AVAudioUnitDistortion) where
  toAVAudioUnitEffect = unsafeCastId

instance IsNSObject (Id AVAudioUnitDistortion) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitEQ ----------

-- | AVAudioUnitEQ
--
-- An AVAudioUnitEffect that implements a Multi-Band Equalizer.
-- 
-- Phantom type for @AVAudioUnitEQ@.
data AVAudioUnitEQ

instance IsObjCObject (Id AVAudioUnitEQ) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitEQ"

class IsAVAudioUnitEffect a => IsAVAudioUnitEQ a where
  toAVAudioUnitEQ :: a -> Id AVAudioUnitEQ

instance IsAVAudioUnitEQ (Id AVAudioUnitEQ) where
  toAVAudioUnitEQ = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitEQ) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitEQ) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitEffect (Id AVAudioUnitEQ) where
  toAVAudioUnitEffect = unsafeCastId

instance IsNSObject (Id AVAudioUnitEQ) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitReverb ----------

-- | AVAudioUnitReverb
--
-- an AVAudioUnitEffect that implements a reverb
--
-- A reverb simulates the acoustic characteristics of a particular environment.        Use the different presets to simulate a particular space and blend it in with        the original signal using the wetDryMix parameter.
-- 
-- Phantom type for @AVAudioUnitReverb@.
data AVAudioUnitReverb

instance IsObjCObject (Id AVAudioUnitReverb) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitReverb"

class IsAVAudioUnitEffect a => IsAVAudioUnitReverb a where
  toAVAudioUnitReverb :: a -> Id AVAudioUnitReverb

instance IsAVAudioUnitReverb (Id AVAudioUnitReverb) where
  toAVAudioUnitReverb = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitReverb) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitReverb) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitEffect (Id AVAudioUnitReverb) where
  toAVAudioUnitEffect = unsafeCastId

instance IsNSObject (Id AVAudioUnitReverb) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitSampler ----------

-- | AVAudioUnitSampler
--
-- Apple's sampler audio unit.
--
-- An AVAudioUnit for Apple's Sampler Audio Unit. The sampler can be configured by loading    instruments from different types of files such as an aupreset, a DLS or SF2 sound bank,    an EXS24 instrument, a single audio file, or an array of audio files.
--
-- The output is a single stereo bus.
-- 
-- Phantom type for @AVAudioUnitSampler@.
data AVAudioUnitSampler

instance IsObjCObject (Id AVAudioUnitSampler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitSampler"

class IsAVAudioUnitMIDIInstrument a => IsAVAudioUnitSampler a where
  toAVAudioUnitSampler :: a -> Id AVAudioUnitSampler

instance IsAVAudioUnitSampler (Id AVAudioUnitSampler) where
  toAVAudioUnitSampler = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitSampler) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitSampler) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitMIDIInstrument (Id AVAudioUnitSampler) where
  toAVAudioUnitMIDIInstrument = unsafeCastId

instance IsNSObject (Id AVAudioUnitSampler) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitTimePitch ----------

-- | AVAudioUnitTimePitch
--
-- an AVAudioUnitTimeEffect that provides good quality time stretching and pitch shifting
--
-- In this time effect, the playback rate and pitch parameters function independently of each other
-- 
-- Phantom type for @AVAudioUnitTimePitch@.
data AVAudioUnitTimePitch

instance IsObjCObject (Id AVAudioUnitTimePitch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitTimePitch"

class IsAVAudioUnitTimeEffect a => IsAVAudioUnitTimePitch a where
  toAVAudioUnitTimePitch :: a -> Id AVAudioUnitTimePitch

instance IsAVAudioUnitTimePitch (Id AVAudioUnitTimePitch) where
  toAVAudioUnitTimePitch = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitTimePitch) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitTimePitch) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitTimeEffect (Id AVAudioUnitTimePitch) where
  toAVAudioUnitTimeEffect = unsafeCastId

instance IsNSObject (Id AVAudioUnitTimePitch) where
  toNSObject = unsafeCastId

-- ---------- AVAudioUnitVarispeed ----------

-- | AVAudioUnitVarispeed
--
-- an AVAudioUnitTimeEffect that can be used to control the playback rate
-- 
-- Phantom type for @AVAudioUnitVarispeed@.
data AVAudioUnitVarispeed

instance IsObjCObject (Id AVAudioUnitVarispeed) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioUnitVarispeed"

class IsAVAudioUnitTimeEffect a => IsAVAudioUnitVarispeed a where
  toAVAudioUnitVarispeed :: a -> Id AVAudioUnitVarispeed

instance IsAVAudioUnitVarispeed (Id AVAudioUnitVarispeed) where
  toAVAudioUnitVarispeed = unsafeCastId

instance IsAVAudioNode (Id AVAudioUnitVarispeed) where
  toAVAudioNode = unsafeCastId

instance IsAVAudioUnit (Id AVAudioUnitVarispeed) where
  toAVAudioUnit = unsafeCastId

instance IsAVAudioUnitTimeEffect (Id AVAudioUnitVarispeed) where
  toAVAudioUnitTimeEffect = unsafeCastId

instance IsNSObject (Id AVAudioUnitVarispeed) where
  toNSObject = unsafeCastId
