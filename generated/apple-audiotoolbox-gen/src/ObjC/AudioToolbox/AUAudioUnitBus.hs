{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUAudioUnitBus
--
-- An input or output connection point on an audio unit.
--
-- Generated bindings for @AUAudioUnitBus@.
module ObjC.AudioToolbox.AUAudioUnitBus
  ( AUAudioUnitBus
  , IsAUAudioUnitBus(..)
  , setFormat_error
  , initWithFormat_error
  , shouldAllocateBuffer
  , setShouldAllocateBuffer
  , enabled
  , setEnabled
  , name
  , setName
  , index
  , busType
  , ownerAudioUnit
  , supportedChannelLayoutTags
  , contextPresentationLatency
  , setContextPresentationLatency
  , supportedChannelCounts
  , setSupportedChannelCounts
  , maximumChannelCount
  , setMaximumChannelCount
  , busTypeSelector
  , contextPresentationLatencySelector
  , enabledSelector
  , indexSelector
  , initWithFormat_errorSelector
  , maximumChannelCountSelector
  , nameSelector
  , ownerAudioUnitSelector
  , setContextPresentationLatencySelector
  , setEnabledSelector
  , setFormat_errorSelector
  , setMaximumChannelCountSelector
  , setNameSelector
  , setShouldAllocateBufferSelector
  , setSupportedChannelCountsSelector
  , shouldAllocateBufferSelector
  , supportedChannelCountsSelector
  , supportedChannelLayoutTagsSelector

  -- * Enum types
  , AUAudioUnitBusType(AUAudioUnitBusType)
  , pattern AUAudioUnitBusTypeInput
  , pattern AUAudioUnitBusTypeOutput

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioToolbox.Internal.Classes
import ObjC.AudioToolbox.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | setFormat:error:
--
-- Sets the bus's audio format.
--
-- Audio units can generally be expected to support AVAudioFormat's standard format		(deinterleaved 32-bit float), at any sample rate. Channel counts can be more complex;		see AUAudioUnit.channelCapabilities.
--
-- ObjC selector: @- setFormat:error:@
setFormat_error :: (IsAUAudioUnitBus auAudioUnitBus, IsNSError outError) => auAudioUnitBus -> RawId -> outError -> IO Bool
setFormat_error auAudioUnitBus format outError =
  sendMessage auAudioUnitBus setFormat_errorSelector format (toNSError outError)

-- | initWithFormat:error:
--
-- initialize with a default format.
--
-- @format@ — The initial format for the bus.
--
-- @outError@ — An error if the format is unsupported for the bus.
--
-- ObjC selector: @- initWithFormat:error:@
initWithFormat_error :: (IsAUAudioUnitBus auAudioUnitBus, IsNSError outError) => auAudioUnitBus -> RawId -> outError -> IO (Id AUAudioUnitBus)
initWithFormat_error auAudioUnitBus format outError =
  sendOwnedMessage auAudioUnitBus initWithFormat_errorSelector format (toNSError outError)

-- | shouldAllocateBuffer
--
-- Controls the audio unit's allocation strategy for a bus.
--
-- Hosts can set this flag to communicate whether an audio unit should allocate its own buffer.        By default this flag is set to true.
--
-- On the output side, shouldAllocateBuffer=false means the AU can assume that it will be        called with non-null output buffers. If shouldAllocateBuffer=true (the default), the AU must        be prepared to be called with null pointers and replace them with pointers to its internally        allocated buffer.
--
-- On the input side, shouldAllocateBuffer=false means the AU can pull for input using a buffer        list with null buffer pointers, and assume that the pull input block will provide pointers.        If shouldAllocateBuffer=true (the default), the AU must pull with non-null pointers while        still being prepared for the source to replace them with pointers of its own.
--
-- Bridged to the v2 property kAudioUnitProperty_ShouldAllocateBuffer.
--
-- ObjC selector: @- shouldAllocateBuffer@
shouldAllocateBuffer :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO Bool
shouldAllocateBuffer auAudioUnitBus =
  sendMessage auAudioUnitBus shouldAllocateBufferSelector

-- | shouldAllocateBuffer
--
-- Controls the audio unit's allocation strategy for a bus.
--
-- Hosts can set this flag to communicate whether an audio unit should allocate its own buffer.        By default this flag is set to true.
--
-- On the output side, shouldAllocateBuffer=false means the AU can assume that it will be        called with non-null output buffers. If shouldAllocateBuffer=true (the default), the AU must        be prepared to be called with null pointers and replace them with pointers to its internally        allocated buffer.
--
-- On the input side, shouldAllocateBuffer=false means the AU can pull for input using a buffer        list with null buffer pointers, and assume that the pull input block will provide pointers.        If shouldAllocateBuffer=true (the default), the AU must pull with non-null pointers while        still being prepared for the source to replace them with pointers of its own.
--
-- Bridged to the v2 property kAudioUnitProperty_ShouldAllocateBuffer.
--
-- ObjC selector: @- setShouldAllocateBuffer:@
setShouldAllocateBuffer :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> Bool -> IO ()
setShouldAllocateBuffer auAudioUnitBus value =
  sendMessage auAudioUnitBus setShouldAllocateBufferSelector value

-- | enabled
--
-- Whether the bus is active.
--
-- Hosts must enable input busses before using them. The reason for this is to allow a unit		such as a mixer to be prepared to render a large number of inputs, but avoid the work		of preparing to pull inputs which are not in use.
--
-- Bridged to the v2 properties kAudioUnitProperty_MakeConnection and		kAudioUnitProperty_SetRenderCallback.
--
-- ObjC selector: @- enabled@
enabled :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO Bool
enabled auAudioUnitBus =
  sendMessage auAudioUnitBus enabledSelector

-- | enabled
--
-- Whether the bus is active.
--
-- Hosts must enable input busses before using them. The reason for this is to allow a unit		such as a mixer to be prepared to render a large number of inputs, but avoid the work		of preparing to pull inputs which are not in use.
--
-- Bridged to the v2 properties kAudioUnitProperty_MakeConnection and		kAudioUnitProperty_SetRenderCallback.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> Bool -> IO ()
setEnabled auAudioUnitBus value =
  sendMessage auAudioUnitBus setEnabledSelector value

-- | name
--
-- A name for the bus. Can be set by host.
--
-- ObjC selector: @- name@
name :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id NSString)
name auAudioUnitBus =
  sendMessage auAudioUnitBus nameSelector

-- | name
--
-- A name for the bus. Can be set by host.
--
-- ObjC selector: @- setName:@
setName :: (IsAUAudioUnitBus auAudioUnitBus, IsNSString value) => auAudioUnitBus -> value -> IO ()
setName auAudioUnitBus value =
  sendMessage auAudioUnitBus setNameSelector (toNSString value)

-- | index
--
-- The index of this bus in the containing array.
--
-- ObjC selector: @- index@
index :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO CULong
index auAudioUnitBus =
  sendMessage auAudioUnitBus indexSelector

-- | busType
--
-- The AUAudioUnitBusType.
--
-- ObjC selector: @- busType@
busType :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO AUAudioUnitBusType
busType auAudioUnitBus =
  sendMessage auAudioUnitBus busTypeSelector

-- | ownerAudioUnit
--
-- The audio unit that owns the bus.
--
-- ObjC selector: @- ownerAudioUnit@
ownerAudioUnit :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id AUAudioUnit)
ownerAudioUnit auAudioUnitBus =
  sendMessage auAudioUnitBus ownerAudioUnitSelector

-- | supportedChannelLayoutTags
--
-- This is an array of NSNumbers representing AudioChannelLayoutTag.
--
-- ObjC selector: @- supportedChannelLayoutTags@
supportedChannelLayoutTags :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id NSArray)
supportedChannelLayoutTags auAudioUnitBus =
  sendMessage auAudioUnitBus supportedChannelLayoutTagsSelector

-- | contextPresentationLatency
--
-- Information about latency in the audio unit's processing context.
--
-- This should not be confused with the audio unit's latency property, where the audio unit		describes to the host any processing latency it introduces between its input and its output.
--
-- A host may set this property to describe to the audio unit the presentation latency of its		input and/or output audio data. Latency is described in seconds. A value of zero means		either no latency or an unknown latency.
--
-- A host should set this property on each active bus, since, for example, the audio routing		path to each of multiple output busses may differ.
--
-- For input busses:			Describes how long ago the audio arriving on this bus was acquired. For instance, when			reading from a file to the first audio unit in a chain, the input presentation latency			is zero. For audio input from a device, this initial input latency is the presentation			latency of the device itself, i.e. the device's safety offset and latency.
--
-- A second chained audio unit's input presentation latency will be the input presentation			latency of the first unit, plus the processing latency of the first unit.
--
-- For output busses:			Describes how long it will be before the output audio of an audio unit is presented. For			instance, when writing to a file, the output presentation latency of the last audio unit			in a chain is zero. When the audio from that audio unit is to be played to a device,			then that initial presentation latency will be the presentation latency of the device			itself, which is the I/O buffer size, plus the device's safety offset and latency
--
-- A previous chained audio unit's output presentation latency is the last unit's			presentation latency plus its processing latency.
--
-- So, for a given audio unit anywhere within a mixing graph, the input and output presentation 		latencies describe to that unit how long from the moment of generation it has taken for its 		input to arrive, and how long it will take for its output to be presented.
--
-- Bridged to the v2 property kAudioUnitProperty_PresentationLatency.
--
-- ObjC selector: @- contextPresentationLatency@
contextPresentationLatency :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO CDouble
contextPresentationLatency auAudioUnitBus =
  sendMessage auAudioUnitBus contextPresentationLatencySelector

-- | contextPresentationLatency
--
-- Information about latency in the audio unit's processing context.
--
-- This should not be confused with the audio unit's latency property, where the audio unit		describes to the host any processing latency it introduces between its input and its output.
--
-- A host may set this property to describe to the audio unit the presentation latency of its		input and/or output audio data. Latency is described in seconds. A value of zero means		either no latency or an unknown latency.
--
-- A host should set this property on each active bus, since, for example, the audio routing		path to each of multiple output busses may differ.
--
-- For input busses:			Describes how long ago the audio arriving on this bus was acquired. For instance, when			reading from a file to the first audio unit in a chain, the input presentation latency			is zero. For audio input from a device, this initial input latency is the presentation			latency of the device itself, i.e. the device's safety offset and latency.
--
-- A second chained audio unit's input presentation latency will be the input presentation			latency of the first unit, plus the processing latency of the first unit.
--
-- For output busses:			Describes how long it will be before the output audio of an audio unit is presented. For			instance, when writing to a file, the output presentation latency of the last audio unit			in a chain is zero. When the audio from that audio unit is to be played to a device,			then that initial presentation latency will be the presentation latency of the device			itself, which is the I/O buffer size, plus the device's safety offset and latency
--
-- A previous chained audio unit's output presentation latency is the last unit's			presentation latency plus its processing latency.
--
-- So, for a given audio unit anywhere within a mixing graph, the input and output presentation 		latencies describe to that unit how long from the moment of generation it has taken for its 		input to arrive, and how long it will take for its output to be presented.
--
-- Bridged to the v2 property kAudioUnitProperty_PresentationLatency.
--
-- ObjC selector: @- setContextPresentationLatency:@
setContextPresentationLatency :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> CDouble -> IO ()
setContextPresentationLatency auAudioUnitBus value =
  sendMessage auAudioUnitBus setContextPresentationLatencySelector value

-- | supportedChannelCounts
--
-- An array of numbers giving the supported numbers of channels for this bus.
--
-- If supportedChannelCounts is nil, then any number less than or equal to maximumChannelCount		is supported. If setting supportedChannelCounts makes the current format unsupported, then		format will be set to nil. The default value is nil.
--
-- ObjC selector: @- supportedChannelCounts@
supportedChannelCounts :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id NSArray)
supportedChannelCounts auAudioUnitBus =
  sendMessage auAudioUnitBus supportedChannelCountsSelector

-- | supportedChannelCounts
--
-- An array of numbers giving the supported numbers of channels for this bus.
--
-- If supportedChannelCounts is nil, then any number less than or equal to maximumChannelCount		is supported. If setting supportedChannelCounts makes the current format unsupported, then		format will be set to nil. The default value is nil.
--
-- ObjC selector: @- setSupportedChannelCounts:@
setSupportedChannelCounts :: (IsAUAudioUnitBus auAudioUnitBus, IsNSArray value) => auAudioUnitBus -> value -> IO ()
setSupportedChannelCounts auAudioUnitBus value =
  sendMessage auAudioUnitBus setSupportedChannelCountsSelector (toNSArray value)

-- | maximumChannelCount
--
-- The maximum numbers of channels supported for this bus.
--
-- If supportedChannelCounts is set, then this value is derived from supportedChannelCounts. If		setting maximumChannelCount makes the current format unsupported, then format will be set to		nil. The default value is UINT_MAX.
--
-- ObjC selector: @- maximumChannelCount@
maximumChannelCount :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO CUInt
maximumChannelCount auAudioUnitBus =
  sendMessage auAudioUnitBus maximumChannelCountSelector

-- | maximumChannelCount
--
-- The maximum numbers of channels supported for this bus.
--
-- If supportedChannelCounts is set, then this value is derived from supportedChannelCounts. If		setting maximumChannelCount makes the current format unsupported, then format will be set to		nil. The default value is UINT_MAX.
--
-- ObjC selector: @- setMaximumChannelCount:@
setMaximumChannelCount :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> CUInt -> IO ()
setMaximumChannelCount auAudioUnitBus value =
  sendMessage auAudioUnitBus setMaximumChannelCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFormat:error:@
setFormat_errorSelector :: Selector '[RawId, Id NSError] Bool
setFormat_errorSelector = mkSelector "setFormat:error:"

-- | @Selector@ for @initWithFormat:error:@
initWithFormat_errorSelector :: Selector '[RawId, Id NSError] (Id AUAudioUnitBus)
initWithFormat_errorSelector = mkSelector "initWithFormat:error:"

-- | @Selector@ for @shouldAllocateBuffer@
shouldAllocateBufferSelector :: Selector '[] Bool
shouldAllocateBufferSelector = mkSelector "shouldAllocateBuffer"

-- | @Selector@ for @setShouldAllocateBuffer:@
setShouldAllocateBufferSelector :: Selector '[Bool] ()
setShouldAllocateBufferSelector = mkSelector "setShouldAllocateBuffer:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @busType@
busTypeSelector :: Selector '[] AUAudioUnitBusType
busTypeSelector = mkSelector "busType"

-- | @Selector@ for @ownerAudioUnit@
ownerAudioUnitSelector :: Selector '[] (Id AUAudioUnit)
ownerAudioUnitSelector = mkSelector "ownerAudioUnit"

-- | @Selector@ for @supportedChannelLayoutTags@
supportedChannelLayoutTagsSelector :: Selector '[] (Id NSArray)
supportedChannelLayoutTagsSelector = mkSelector "supportedChannelLayoutTags"

-- | @Selector@ for @contextPresentationLatency@
contextPresentationLatencySelector :: Selector '[] CDouble
contextPresentationLatencySelector = mkSelector "contextPresentationLatency"

-- | @Selector@ for @setContextPresentationLatency:@
setContextPresentationLatencySelector :: Selector '[CDouble] ()
setContextPresentationLatencySelector = mkSelector "setContextPresentationLatency:"

-- | @Selector@ for @supportedChannelCounts@
supportedChannelCountsSelector :: Selector '[] (Id NSArray)
supportedChannelCountsSelector = mkSelector "supportedChannelCounts"

-- | @Selector@ for @setSupportedChannelCounts:@
setSupportedChannelCountsSelector :: Selector '[Id NSArray] ()
setSupportedChannelCountsSelector = mkSelector "setSupportedChannelCounts:"

-- | @Selector@ for @maximumChannelCount@
maximumChannelCountSelector :: Selector '[] CUInt
maximumChannelCountSelector = mkSelector "maximumChannelCount"

-- | @Selector@ for @setMaximumChannelCount:@
setMaximumChannelCountSelector :: Selector '[CUInt] ()
setMaximumChannelCountSelector = mkSelector "setMaximumChannelCount:"

