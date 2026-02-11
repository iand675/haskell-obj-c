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
  , resetSelector
  , inputFormatForBusSelector
  , outputFormatForBusSelector
  , nameForInputBusSelector
  , nameForOutputBusSelector
  , installTapOnBus_bufferSize_format_blockSelector
  , removeTapOnBusSelector
  , engineSelector
  , numberOfInputsSelector
  , numberOfOutputsSelector
  , lastRenderTimeSelector
  , auAudioUnitSelector
  , latencySelector
  , outputPresentationLatencySelector


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

import ObjC.AVFAudio.Internal.Classes
import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | reset
--
-- Clear a unit's previous processing state.
--
-- ObjC selector: @- reset@
reset :: IsAVAudioNode avAudioNode => avAudioNode -> IO ()
reset avAudioNode  =
  sendMsg avAudioNode (mkSelector "reset") retVoid []

-- | inputFormatForBus:
--
-- Obtain an input bus's format.
--
-- ObjC selector: @- inputFormatForBus:@
inputFormatForBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id AVAudioFormat)
inputFormatForBus avAudioNode  bus =
  sendMsg avAudioNode (mkSelector "inputFormatForBus:") (retPtr retVoid) [argCULong (fromIntegral bus)] >>= retainedObject . castPtr

-- | outputFormatForBus:
--
-- Obtain an output bus's format.
--
-- ObjC selector: @- outputFormatForBus:@
outputFormatForBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id AVAudioFormat)
outputFormatForBus avAudioNode  bus =
  sendMsg avAudioNode (mkSelector "outputFormatForBus:") (retPtr retVoid) [argCULong (fromIntegral bus)] >>= retainedObject . castPtr

-- | nameForInputBus:
--
-- Return the name of an input bus.
--
-- ObjC selector: @- nameForInputBus:@
nameForInputBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id NSString)
nameForInputBus avAudioNode  bus =
  sendMsg avAudioNode (mkSelector "nameForInputBus:") (retPtr retVoid) [argCULong (fromIntegral bus)] >>= retainedObject . castPtr

-- | nameForOutputBus:
--
-- Return the name of an output bus.
--
-- ObjC selector: @- nameForOutputBus:@
nameForOutputBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO (Id NSString)
nameForOutputBus avAudioNode  bus =
  sendMsg avAudioNode (mkSelector "nameForOutputBus:") (retPtr retVoid) [argCULong (fromIntegral bus)] >>= retainedObject . castPtr

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
installTapOnBus_bufferSize_format_block avAudioNode  bus bufferSize format tapBlock =
withObjCPtr format $ \raw_format ->
    sendMsg avAudioNode (mkSelector "installTapOnBus:bufferSize:format:block:") retVoid [argCULong (fromIntegral bus), argCUInt (fromIntegral bufferSize), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr tapBlock :: Ptr ())]

-- | removeTapOnBus:
--
-- Destroy a tap.
--
-- @bus@ — the node output bus whose tap is to be destroyed
--
-- ObjC selector: @- removeTapOnBus:@
removeTapOnBus :: IsAVAudioNode avAudioNode => avAudioNode -> CULong -> IO ()
removeTapOnBus avAudioNode  bus =
  sendMsg avAudioNode (mkSelector "removeTapOnBus:") retVoid [argCULong (fromIntegral bus)]

-- | engine
--
-- The engine to which the node is attached (or nil).
--
-- ObjC selector: @- engine@
engine :: IsAVAudioNode avAudioNode => avAudioNode -> IO (Id AVAudioEngine)
engine avAudioNode  =
  sendMsg avAudioNode (mkSelector "engine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | numberOfInputs
--
-- The node's number of input busses.
--
-- ObjC selector: @- numberOfInputs@
numberOfInputs :: IsAVAudioNode avAudioNode => avAudioNode -> IO CULong
numberOfInputs avAudioNode  =
  sendMsg avAudioNode (mkSelector "numberOfInputs") retCULong []

-- | numberOfOutputs
--
-- The node's number of output busses.
--
-- ObjC selector: @- numberOfOutputs@
numberOfOutputs :: IsAVAudioNode avAudioNode => avAudioNode -> IO CULong
numberOfOutputs avAudioNode  =
  sendMsg avAudioNode (mkSelector "numberOfOutputs") retCULong []

-- | lastRenderTime
--
-- Obtain the time for which the node most recently rendered.
--
-- Will return nil if the engine is not running or if the node is not connected to an input or		output node.
--
-- ObjC selector: @- lastRenderTime@
lastRenderTime :: IsAVAudioNode avAudioNode => avAudioNode -> IO (Id AVAudioTime)
lastRenderTime avAudioNode  =
  sendMsg avAudioNode (mkSelector "lastRenderTime") (retPtr retVoid) [] >>= retainedObject . castPtr

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
auAudioUnit avAudioNode  =
  sendMsg avAudioNode (mkSelector "AUAudioUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | latency
--
-- The processing latency of the node, in seconds.
--
-- This property reflects the delay between when an impulse in the audio stream arrives at the		input vs. output of the node. This should reflect the delay due to signal processing 		(e.g. filters, FFT's, etc.), not delay or reverberation which is being applied as an effect. 		A value of zero indicates either no latency or an unknown latency.
--
-- ObjC selector: @- latency@
latency :: IsAVAudioNode avAudioNode => avAudioNode -> IO CDouble
latency avAudioNode  =
  sendMsg avAudioNode (mkSelector "latency") retCDouble []

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
outputPresentationLatency avAudioNode  =
  sendMsg avAudioNode (mkSelector "outputPresentationLatency") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @inputFormatForBus:@
inputFormatForBusSelector :: Selector
inputFormatForBusSelector = mkSelector "inputFormatForBus:"

-- | @Selector@ for @outputFormatForBus:@
outputFormatForBusSelector :: Selector
outputFormatForBusSelector = mkSelector "outputFormatForBus:"

-- | @Selector@ for @nameForInputBus:@
nameForInputBusSelector :: Selector
nameForInputBusSelector = mkSelector "nameForInputBus:"

-- | @Selector@ for @nameForOutputBus:@
nameForOutputBusSelector :: Selector
nameForOutputBusSelector = mkSelector "nameForOutputBus:"

-- | @Selector@ for @installTapOnBus:bufferSize:format:block:@
installTapOnBus_bufferSize_format_blockSelector :: Selector
installTapOnBus_bufferSize_format_blockSelector = mkSelector "installTapOnBus:bufferSize:format:block:"

-- | @Selector@ for @removeTapOnBus:@
removeTapOnBusSelector :: Selector
removeTapOnBusSelector = mkSelector "removeTapOnBus:"

-- | @Selector@ for @engine@
engineSelector :: Selector
engineSelector = mkSelector "engine"

-- | @Selector@ for @numberOfInputs@
numberOfInputsSelector :: Selector
numberOfInputsSelector = mkSelector "numberOfInputs"

-- | @Selector@ for @numberOfOutputs@
numberOfOutputsSelector :: Selector
numberOfOutputsSelector = mkSelector "numberOfOutputs"

-- | @Selector@ for @lastRenderTime@
lastRenderTimeSelector :: Selector
lastRenderTimeSelector = mkSelector "lastRenderTime"

-- | @Selector@ for @AUAudioUnit@
auAudioUnitSelector :: Selector
auAudioUnitSelector = mkSelector "AUAudioUnit"

-- | @Selector@ for @latency@
latencySelector :: Selector
latencySelector = mkSelector "latency"

-- | @Selector@ for @outputPresentationLatency@
outputPresentationLatencySelector :: Selector
outputPresentationLatencySelector = mkSelector "outputPresentationLatency"

