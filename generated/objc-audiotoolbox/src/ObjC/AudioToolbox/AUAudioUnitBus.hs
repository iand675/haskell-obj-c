{-# LANGUAGE PatternSynonyms #-}
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
  , format
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
  , setFormat_errorSelector
  , initWithFormat_errorSelector
  , formatSelector
  , shouldAllocateBufferSelector
  , setShouldAllocateBufferSelector
  , enabledSelector
  , setEnabledSelector
  , nameSelector
  , setNameSelector
  , indexSelector
  , busTypeSelector
  , ownerAudioUnitSelector
  , supportedChannelLayoutTagsSelector
  , contextPresentationLatencySelector
  , setContextPresentationLatencySelector
  , supportedChannelCountsSelector
  , setSupportedChannelCountsSelector
  , maximumChannelCountSelector
  , setMaximumChannelCountSelector

  -- * Enum types
  , AUAudioUnitBusType(AUAudioUnitBusType)
  , pattern AUAudioUnitBusTypeInput
  , pattern AUAudioUnitBusTypeOutput

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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.AudioToolbox.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setFormat:error:
--
-- Sets the bus's audio format.
--
-- Audio units can generally be expected to support AVAudioFormat's standard format		(deinterleaved 32-bit float), at any sample rate. Channel counts can be more complex;		see AUAudioUnit.channelCapabilities.
--
-- ObjC selector: @- setFormat:error:@
setFormat_error :: (IsAUAudioUnitBus auAudioUnitBus, IsAVAudioFormat format, IsNSError outError) => auAudioUnitBus -> format -> outError -> IO Bool
setFormat_error auAudioUnitBus  format outError =
withObjCPtr format $ \raw_format ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg auAudioUnitBus (mkSelector "setFormat:error:") retCULong [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | initWithFormat:error:
--
-- initialize with a default format.
--
-- @format@ — The initial format for the bus.
--
-- @outError@ — An error if the format is unsupported for the bus.
--
-- ObjC selector: @- initWithFormat:error:@
initWithFormat_error :: (IsAUAudioUnitBus auAudioUnitBus, IsAVAudioFormat format, IsNSError outError) => auAudioUnitBus -> format -> outError -> IO (Id AUAudioUnitBus)
initWithFormat_error auAudioUnitBus  format outError =
withObjCPtr format $ \raw_format ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg auAudioUnitBus (mkSelector "initWithFormat:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | format
--
-- The audio format and channel layout of audio being transferred on the bus.
--
-- Bridged to the v2 property kAudioUnitProperty_StreamFormat.
--
-- ObjC selector: @- format@
format :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id AVAudioFormat)
format auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

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
shouldAllocateBuffer auAudioUnitBus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg auAudioUnitBus (mkSelector "shouldAllocateBuffer") retCULong []

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
setShouldAllocateBuffer auAudioUnitBus  value =
  sendMsg auAudioUnitBus (mkSelector "setShouldAllocateBuffer:") retVoid [argCULong (if value then 1 else 0)]

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
enabled auAudioUnitBus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg auAudioUnitBus (mkSelector "enabled") retCULong []

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
setEnabled auAudioUnitBus  value =
  sendMsg auAudioUnitBus (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | name
--
-- A name for the bus. Can be set by host.
--
-- ObjC selector: @- name@
name :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id NSString)
name auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- A name for the bus. Can be set by host.
--
-- ObjC selector: @- setName:@
setName :: (IsAUAudioUnitBus auAudioUnitBus, IsNSString value) => auAudioUnitBus -> value -> IO ()
setName auAudioUnitBus  value =
withObjCPtr value $ \raw_value ->
    sendMsg auAudioUnitBus (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | index
--
-- The index of this bus in the containing array.
--
-- ObjC selector: @- index@
index :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO CULong
index auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "index") retCULong []

-- | busType
--
-- The AUAudioUnitBusType.
--
-- ObjC selector: @- busType@
busType :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO AUAudioUnitBusType
busType auAudioUnitBus  =
  fmap (coerce :: CLong -> AUAudioUnitBusType) $ sendMsg auAudioUnitBus (mkSelector "busType") retCLong []

-- | ownerAudioUnit
--
-- The audio unit that owns the bus.
--
-- ObjC selector: @- ownerAudioUnit@
ownerAudioUnit :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id AUAudioUnit)
ownerAudioUnit auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "ownerAudioUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | supportedChannelLayoutTags
--
-- This is an array of NSNumbers representing AudioChannelLayoutTag.
--
-- ObjC selector: @- supportedChannelLayoutTags@
supportedChannelLayoutTags :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id NSArray)
supportedChannelLayoutTags auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "supportedChannelLayoutTags") (retPtr retVoid) [] >>= retainedObject . castPtr

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
contextPresentationLatency auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "contextPresentationLatency") retCDouble []

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
setContextPresentationLatency auAudioUnitBus  value =
  sendMsg auAudioUnitBus (mkSelector "setContextPresentationLatency:") retVoid [argCDouble (fromIntegral value)]

-- | supportedChannelCounts
--
-- An array of numbers giving the supported numbers of channels for this bus.
--
-- If supportedChannelCounts is nil, then any number less than or equal to maximumChannelCount		is supported. If setting supportedChannelCounts makes the current format unsupported, then		format will be set to nil. The default value is nil.
--
-- ObjC selector: @- supportedChannelCounts@
supportedChannelCounts :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO (Id NSArray)
supportedChannelCounts auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "supportedChannelCounts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | supportedChannelCounts
--
-- An array of numbers giving the supported numbers of channels for this bus.
--
-- If supportedChannelCounts is nil, then any number less than or equal to maximumChannelCount		is supported. If setting supportedChannelCounts makes the current format unsupported, then		format will be set to nil. The default value is nil.
--
-- ObjC selector: @- setSupportedChannelCounts:@
setSupportedChannelCounts :: (IsAUAudioUnitBus auAudioUnitBus, IsNSArray value) => auAudioUnitBus -> value -> IO ()
setSupportedChannelCounts auAudioUnitBus  value =
withObjCPtr value $ \raw_value ->
    sendMsg auAudioUnitBus (mkSelector "setSupportedChannelCounts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | maximumChannelCount
--
-- The maximum numbers of channels supported for this bus.
--
-- If supportedChannelCounts is set, then this value is derived from supportedChannelCounts. If		setting maximumChannelCount makes the current format unsupported, then format will be set to		nil. The default value is UINT_MAX.
--
-- ObjC selector: @- maximumChannelCount@
maximumChannelCount :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> IO CUInt
maximumChannelCount auAudioUnitBus  =
  sendMsg auAudioUnitBus (mkSelector "maximumChannelCount") retCUInt []

-- | maximumChannelCount
--
-- The maximum numbers of channels supported for this bus.
--
-- If supportedChannelCounts is set, then this value is derived from supportedChannelCounts. If		setting maximumChannelCount makes the current format unsupported, then format will be set to		nil. The default value is UINT_MAX.
--
-- ObjC selector: @- setMaximumChannelCount:@
setMaximumChannelCount :: IsAUAudioUnitBus auAudioUnitBus => auAudioUnitBus -> CUInt -> IO ()
setMaximumChannelCount auAudioUnitBus  value =
  sendMsg auAudioUnitBus (mkSelector "setMaximumChannelCount:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFormat:error:@
setFormat_errorSelector :: Selector
setFormat_errorSelector = mkSelector "setFormat:error:"

-- | @Selector@ for @initWithFormat:error:@
initWithFormat_errorSelector :: Selector
initWithFormat_errorSelector = mkSelector "initWithFormat:error:"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @shouldAllocateBuffer@
shouldAllocateBufferSelector :: Selector
shouldAllocateBufferSelector = mkSelector "shouldAllocateBuffer"

-- | @Selector@ for @setShouldAllocateBuffer:@
setShouldAllocateBufferSelector :: Selector
setShouldAllocateBufferSelector = mkSelector "setShouldAllocateBuffer:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @busType@
busTypeSelector :: Selector
busTypeSelector = mkSelector "busType"

-- | @Selector@ for @ownerAudioUnit@
ownerAudioUnitSelector :: Selector
ownerAudioUnitSelector = mkSelector "ownerAudioUnit"

-- | @Selector@ for @supportedChannelLayoutTags@
supportedChannelLayoutTagsSelector :: Selector
supportedChannelLayoutTagsSelector = mkSelector "supportedChannelLayoutTags"

-- | @Selector@ for @contextPresentationLatency@
contextPresentationLatencySelector :: Selector
contextPresentationLatencySelector = mkSelector "contextPresentationLatency"

-- | @Selector@ for @setContextPresentationLatency:@
setContextPresentationLatencySelector :: Selector
setContextPresentationLatencySelector = mkSelector "setContextPresentationLatency:"

-- | @Selector@ for @supportedChannelCounts@
supportedChannelCountsSelector :: Selector
supportedChannelCountsSelector = mkSelector "supportedChannelCounts"

-- | @Selector@ for @setSupportedChannelCounts:@
setSupportedChannelCountsSelector :: Selector
setSupportedChannelCountsSelector = mkSelector "setSupportedChannelCounts:"

-- | @Selector@ for @maximumChannelCount@
maximumChannelCountSelector :: Selector
maximumChannelCountSelector = mkSelector "maximumChannelCount"

-- | @Selector@ for @setMaximumChannelCount:@
setMaximumChannelCountSelector :: Selector
setMaximumChannelCountSelector = mkSelector "setMaximumChannelCount:"

