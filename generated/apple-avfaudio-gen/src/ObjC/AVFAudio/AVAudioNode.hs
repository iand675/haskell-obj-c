{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @AVAudioNode@.
module ObjC.AVFAudio.AVAudioNode
  ( AVAudioNode
  , IsAVAudioNode(..)
  , reset
  , inputFormatForBus
  , outputFormatForBus
  , nameForInputBus
  , nameForOutputBus
  , installTapOnBus_bufferSize_format_block
  , removeTapOnBus
  , engine
  , numberOfInputs
  , numberOfOutputs
  , lastRenderTime
  , auAudioUnit
  , latency
  , outputPresentationLatency
  , auAudioUnitSelector
  , engineSelector
  , inputFormatForBusSelector
  , installTapOnBus_bufferSize_format_blockSelector
  , lastRenderTimeSelector
  , latencySelector
  , nameForInputBusSelector
  , nameForOutputBusSelector
  , numberOfInputsSelector
  , numberOfOutputsSelector
  , outputFormatForBusSelector
  , outputPresentationLatencySelector
  , removeTapOnBusSelector
  , resetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | reset
--
-- Clear a unit's previous processing state.
--
-- ObjC selector: @- reset@
reset :: IsAVAudioNode avAudioNode => avAudioNode -> IO ()
reset avAudioNode =
  sendMessage avAudioNode resetSelector

-- | inputFormatForBus:
--
-- Obtain an input bus's format.
--
-- ObjC selector: @- inputFormatForBus:@
inputFormatForBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id AVAudioFormat)
inputFormatForBus avAudioNode bus =
  sendMessage avAudioNode inputFormatForBusSelector bus

-- | outputFormatForBus:
--
-- Obtain an output bus's format.
--
-- ObjC selector: @- outputFormatForBus:@
outputFormatForBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id AVAudioFormat)
outputFormatForBus avAudioNode bus =
  sendMessage avAudioNode outputFormatForBusSelector bus

-- | nameForInputBus:
--
-- Return the name of an input bus.
--
-- ObjC selector: @- nameForInputBus:@
nameForInputBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id NSString)
nameForInputBus avAudioNode bus =
  sendMessage avAudioNode nameForInputBusSelector bus

-- | nameForOutputBus:
--
-- Return the name of an output bus.
--
-- ObjC selector: @- nameForOutputBus:@
nameForOutputBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id NSString)
nameForOutputBus avAudioNode bus =
  sendMessage avAudioNode nameForOutputBusSelector bus

-- | installTapOnBus:bufferSize:format:block:
--
-- Create a "tap" to record/monitor/observe the output of the node.
--
-- @bus@ — the node output bus to which to attach the tap
--
-- @bufferSize@ — the requested size of the incoming buffers in sample frames. Supported range is [100, 400] ms.
--
-- @format@ — If non-nil, attempts to apply this as the format of the specified output bus. This should		only be done when attaching to an output bus which is not connected to another node; an		error will result otherwise.		The tap and connection formats (if non-nil) on the specified bus should be identical. 		Otherwise, the latter operation will override any previously set format.
--
-- @tapBlock@ — a block to be called with audio buffers
--
-- Only one tap may be installed on any bus. Taps may be safely installed and removed while		the engine is running.
--
-- Note that if you have a tap installed on AVAudioOutputNode, there could be a mismatch		between the tap buffer format and AVAudioOutputNode's output format, depending on the		underlying physical device. Hence, instead of tapping the AVAudioOutputNode, it is		advised to tap the node connected to it.
--
-- E.g. to capture audio from input node:AVAudioEngine *engine = [[AVAudioEngine alloc] init];AVAudioInputNode *input = [engine inputNode];AVAudioFormat *format = [input outputFormatForBus: 0];[input installTapOnBus: 0 bufferSize: 8192 format: format block: ^(AVAudioPCMBuffer *buf, AVAudioTime *when) {// ‘buf' contains audio captured from input node at time 'when'}];....// start engine
--
-- ObjC selector: @- installTapOnBus:bufferSize:format:block:@
installTapOnBus_bufferSize_format_block :: (IsAVAudioNode avAudioNode, IsAVAudioFormat format) => avAudioNode -> CULong -> CUInt -> format -> Ptr () -> IO ()
installTapOnBus_bufferSize_format_block avAudioNode bus bufferSize format tapBlock =
  sendMessage avAudioNode installTapOnBus_bufferSize_format_blockSelector bus bufferSize (toAVAudioFormat format) tapBlock

-- | removeTapOnBus:
--
-- Destroy a tap.
--
-- @bus@ — the node output bus whose tap is to be destroyed
--
-- ObjC selector: @- removeTapOnBus:@
removeTapOnBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO ()
removeTapOnBus avAudioNode bus =
  sendMessage avAudioNode removeTapOnBusSelector bus

-- | engine
--
-- The engine to which the node is attached (or nil).
--
-- ObjC selector: @- engine@
engine :: IsAVAudioNode avAudioNode => avAudioNode -> IO (Id AVAudioEngine)
engine avAudioNode =
  sendMessage avAudioNode engineSelector

-- | numberOfInputs
--
-- The node's number of input busses.
--
-- ObjC selector: @- numberOfInputs@
numberOfInputs :: IsAVAudioNode avAudioNode => avAudioNode -> IO CULong
numberOfInputs avAudioNode =
  sendMessage avAudioNode numberOfInputsSelector

-- | numberOfOutputs
--
-- The node's number of output busses.
--
-- ObjC selector: @- numberOfOutputs@
numberOfOutputs :: IsAVAudioNode avAudioNode => avAudioNode -> IO CULong
numberOfOutputs avAudioNode =
  sendMessage avAudioNode numberOfOutputsSelector

-- | lastRenderTime
--
-- Obtain the time for which the node most recently rendered.
--
-- Will return nil if the engine is not running or if the node is not connected to an input or		output node.
--
-- ObjC selector: @- lastRenderTime@
lastRenderTime :: IsAVAudioNode avAudioNode => avAudioNode -> IO (Id AVAudioTime)
lastRenderTime avAudioNode =
  sendMessage avAudioNode lastRenderTimeSelector

-- | AUAudioUnit
--
-- An AUAudioUnit wrapping or underlying the implementation's AudioUnit.
--
-- This provides an AUAudioUnit which either wraps or underlies the implementation's		AudioUnit, depending on how that audio unit is packaged. Applications can interact with this		AUAudioUnit to control custom properties, select presets, change parameters, etc.
--
-- No operations that may conflict with state maintained by the engine should be performed 		directly on the audio unit. These include changing initialization state, stream formats, 		channel layouts or connections to other audio units.
--
-- ObjC selector: @- AUAudioUnit@
auAudioUnit :: IsAVAudioNode avAudioNode => avAudioNode -> IO (Id AUAudioUnit)
auAudioUnit avAudioNode =
  sendMessage avAudioNode auAudioUnitSelector

-- | latency
--
-- The processing latency of the node, in seconds.
--
-- This property reflects the delay between when an impulse in the audio stream arrives at the		input vs. output of the node. This should reflect the delay due to signal processing 		(e.g. filters, FFT's, etc.), not delay or reverberation which is being applied as an effect. 		A value of zero indicates either no latency or an unknown latency.
--
-- ObjC selector: @- latency@
latency :: IsAVAudioNode avAudioNode => avAudioNode -> IO CDouble
latency avAudioNode =
  sendMessage avAudioNode latencySelector

-- | outputPresentationLatency
--
-- The maximum render pipeline latency downstream of the node, in seconds.
--
-- This describes the maximum time it will take for the audio at the output of a node to be		presented. 		For instance, the output presentation latency of the output node in the engine is:			- zero in manual rendering mode			- the presentation latency of the device itself when rendering to an audio device			  (see @AVAudioIONode(presentationLatency)@)		The output presentation latency of a node connected directly to the output node is the		output node's presentation latency plus the output node's processing latency (see @latency@).
--
-- For a node which is exclusively in the input node chain (i.e. not connected to engine's 		output node), this property reflects the latency for the output of this node to be 		presented at the output of the terminating node in the input chain.
--
-- A value of zero indicates either an unknown or no latency.
--
-- Note that this latency value can change as the engine is reconfigured (started/stopped, 		connections made/altered downstream of this node etc.). So it is recommended not to cache		this value and fetch it whenever it's needed.
--
-- ObjC selector: @- outputPresentationLatency@
outputPresentationLatency :: IsAVAudioNode avAudioNode => avAudioNode -> IO CDouble
outputPresentationLatency avAudioNode =
  sendMessage avAudioNode outputPresentationLatencySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @inputFormatForBus:@
inputFormatForBusSelector :: Selector '[CULong] (Id AVAudioFormat)
inputFormatForBusSelector = mkSelector "inputFormatForBus:"

-- | @Selector@ for @outputFormatForBus:@
outputFormatForBusSelector :: Selector '[CULong] (Id AVAudioFormat)
outputFormatForBusSelector = mkSelector "outputFormatForBus:"

-- | @Selector@ for @nameForInputBus:@
nameForInputBusSelector :: Selector '[CULong] (Id NSString)
nameForInputBusSelector = mkSelector "nameForInputBus:"

-- | @Selector@ for @nameForOutputBus:@
nameForOutputBusSelector :: Selector '[CULong] (Id NSString)
nameForOutputBusSelector = mkSelector "nameForOutputBus:"

-- | @Selector@ for @installTapOnBus:bufferSize:format:block:@
installTapOnBus_bufferSize_format_blockSelector :: Selector '[CULong, CUInt, Id AVAudioFormat, Ptr ()] ()
installTapOnBus_bufferSize_format_blockSelector = mkSelector "installTapOnBus:bufferSize:format:block:"

-- | @Selector@ for @removeTapOnBus:@
removeTapOnBusSelector :: Selector '[CULong] ()
removeTapOnBusSelector = mkSelector "removeTapOnBus:"

-- | @Selector@ for @engine@
engineSelector :: Selector '[] (Id AVAudioEngine)
engineSelector = mkSelector "engine"

-- | @Selector@ for @numberOfInputs@
numberOfInputsSelector :: Selector '[] CULong
numberOfInputsSelector = mkSelector "numberOfInputs"

-- | @Selector@ for @numberOfOutputs@
numberOfOutputsSelector :: Selector '[] CULong
numberOfOutputsSelector = mkSelector "numberOfOutputs"

-- | @Selector@ for @lastRenderTime@
lastRenderTimeSelector :: Selector '[] (Id AVAudioTime)
lastRenderTimeSelector = mkSelector "lastRenderTime"

-- | @Selector@ for @AUAudioUnit@
auAudioUnitSelector :: Selector '[] (Id AUAudioUnit)
auAudioUnitSelector = mkSelector "AUAudioUnit"

-- | @Selector@ for @latency@
latencySelector :: Selector '[] CDouble
latencySelector = mkSelector "latency"

-- | @Selector@ for @outputPresentationLatency@
outputPresentationLatencySelector :: Selector '[] CDouble
outputPresentationLatencySelector = mkSelector "outputPresentationLatency"

