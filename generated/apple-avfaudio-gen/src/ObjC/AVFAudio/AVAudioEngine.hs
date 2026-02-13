{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @AVAudioEngine@.
module ObjC.AVFAudio.AVAudioEngine
  ( AVAudioEngine
  , IsAVAudioEngine(..)
  , init_
  , attachNode
  , detachNode
  , connect_to_fromBus_toBus_format
  , connect_to_format
  , connect_toConnectionPoints_fromBus_format
  , disconnectNodeInput_bus
  , disconnectNodeInput
  , disconnectNodeOutput_bus
  , disconnectNodeOutput
  , prepare
  , startAndReturnError
  , pause
  , reset
  , stop
  , inputConnectionPointForNode_inputBus
  , outputConnectionPointsForNode_outputBus
  , enableManualRenderingMode_format_maximumFrameCount_error
  , disableManualRenderingMode
  , renderOffline_toBuffer_error
  , connectMIDI_to_format_block
  , connectMIDI_to_format_eventListBlock
  , connectMIDI_toNodes_format_block
  , connectMIDI_toNodes_format_eventListBlock
  , disconnectMIDI_from
  , disconnectMIDI_fromNodes
  , disconnectMIDIInput
  , disconnectMIDIOutput
  , musicSequence
  , setMusicSequence
  , outputNode
  , inputNode
  , mainMixerNode
  , running
  , autoShutdownEnabled
  , setAutoShutdownEnabled
  , attachedNodes
  , manualRenderingBlock
  , isInManualRenderingMode
  , manualRenderingMode
  , manualRenderingFormat
  , manualRenderingMaximumFrameCount
  , manualRenderingSampleTime
  , attachNodeSelector
  , attachedNodesSelector
  , autoShutdownEnabledSelector
  , connectMIDI_toNodes_format_blockSelector
  , connectMIDI_toNodes_format_eventListBlockSelector
  , connectMIDI_to_format_blockSelector
  , connectMIDI_to_format_eventListBlockSelector
  , connect_toConnectionPoints_fromBus_formatSelector
  , connect_to_formatSelector
  , connect_to_fromBus_toBus_formatSelector
  , detachNodeSelector
  , disableManualRenderingModeSelector
  , disconnectMIDIInputSelector
  , disconnectMIDIOutputSelector
  , disconnectMIDI_fromNodesSelector
  , disconnectMIDI_fromSelector
  , disconnectNodeInputSelector
  , disconnectNodeInput_busSelector
  , disconnectNodeOutputSelector
  , disconnectNodeOutput_busSelector
  , enableManualRenderingMode_format_maximumFrameCount_errorSelector
  , initSelector
  , inputConnectionPointForNode_inputBusSelector
  , inputNodeSelector
  , isInManualRenderingModeSelector
  , mainMixerNodeSelector
  , manualRenderingBlockSelector
  , manualRenderingFormatSelector
  , manualRenderingMaximumFrameCountSelector
  , manualRenderingModeSelector
  , manualRenderingSampleTimeSelector
  , musicSequenceSelector
  , outputConnectionPointsForNode_outputBusSelector
  , outputNodeSelector
  , pauseSelector
  , prepareSelector
  , renderOffline_toBuffer_errorSelector
  , resetSelector
  , runningSelector
  , setAutoShutdownEnabledSelector
  , setMusicSequenceSelector
  , startAndReturnErrorSelector
  , stopSelector

  -- * Enum types
  , AVAudioEngineManualRenderingMode(AVAudioEngineManualRenderingMode)
  , pattern AVAudioEngineManualRenderingModeOffline
  , pattern AVAudioEngineManualRenderingModeRealtime
  , AVAudioEngineManualRenderingStatus(AVAudioEngineManualRenderingStatus)
  , pattern AVAudioEngineManualRenderingStatusError
  , pattern AVAudioEngineManualRenderingStatusSuccess
  , pattern AVAudioEngineManualRenderingStatusInsufficientDataFromInputNode
  , pattern AVAudioEngineManualRenderingStatusCannotDoInCurrentContext

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Initialize a new engine.
--
-- On creation, the engine is by default connected to an audio device and automatically renders 	in realtime. It can be configured to operate in manual rendering mode through 	@enableManualRenderingMode:format:maximumFrameCount:error:@.
--
-- ObjC selector: @- init@
init_ :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Id AVAudioEngine)
init_ avAudioEngine =
  sendOwnedMessage avAudioEngine initSelector

-- | attachNode:
--
-- Take ownership of a new node.
--
-- @node@ — The node to be attached to the engine.
--
-- To support the instantiation of arbitrary AVAudioNode subclasses, instances are created	externally to the engine, but are not usable until they are attached to the engine via	this method. Thus the idiom, without ARC, is:
--
-- ```	// when building engine:	AVAudioNode *_player;	// member of controller class (for example)	...	_player = [[AVAudioPlayerNode alloc] init];	[engine attachNode: _player];	...	// when destroying engine (without ARC)	[_player release];	```
--
-- ObjC selector: @- attachNode:@
attachNode :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> IO ()
attachNode avAudioEngine node =
  sendMessage avAudioEngine attachNodeSelector (toAVAudioNode node)

-- | detachNode:
--
-- Detach a node previously attached to the engine.
--
-- If necessary, the engine will safely disconnect the node before detaching it.
--
-- ObjC selector: @- detachNode:@
detachNode :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> IO ()
detachNode avAudioEngine node =
  sendMessage avAudioEngine detachNodeSelector (toAVAudioNode node)

-- | connect:to:fromBus:toBus:format:
--
-- Establish a connection between two nodes.
--
-- @node1@ — The source node
--
-- @node2@ — The destination node
--
-- @bus1@ — The output bus on the source node
--
-- @bus2@ — The input bus on the destination node
--
-- @format@ — If non-nil, the format of the source node's output bus is set to this		format. In all cases, the format of the destination node's input bus is set to		match that of the source node's output bus.
--
-- Nodes have input and output buses (AVAudioNodeBus). Use this method to establish	one-to-one connections betweeen nodes. Connections made using this method are always	one-to-one, never one-to-many or many-to-one.
--
-- Note that any pre-existing connection(s) involving the source's output bus or the	destination's input bus will be broken.
--
-- ObjC selector: @- connect:to:fromBus:toBus:format:@
connect_to_fromBus_toBus_format :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node1, IsAVAudioNode node2, IsAVAudioFormat format) => avAudioEngine -> node1 -> node2 -> CULong -> CULong -> format -> IO ()
connect_to_fromBus_toBus_format avAudioEngine node1 node2 bus1 bus2 format =
  sendMessage avAudioEngine connect_to_fromBus_toBus_formatSelector (toAVAudioNode node1) (toAVAudioNode node2) bus1 bus2 (toAVAudioFormat format)

-- | connect:to:format:
--
-- Establish a connection between two nodes
--
-- This calls connect:to:fromBus:toBus:format: using bus 0 on the source node,	and bus 0 on the destination node, except in the case of a destination which is a mixer,	in which case the destination is the mixer's nextAvailableInputBus.
--
-- ObjC selector: @- connect:to:format:@
connect_to_format :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node1, IsAVAudioNode node2, IsAVAudioFormat format) => avAudioEngine -> node1 -> node2 -> format -> IO ()
connect_to_format avAudioEngine node1 node2 format =
  sendMessage avAudioEngine connect_to_formatSelector (toAVAudioNode node1) (toAVAudioNode node2) (toAVAudioFormat format)

-- | connect:toConnectionPoints:fromBus:format:
--
-- Establish connections between a source node and multiple destination nodes.
--
-- @sourceNode@ — The source node
--
-- @destNodes@ — An array of AVAudioConnectionPoint objects specifying destination		nodes and busses
--
-- @sourceBus@ — The output bus on source node
--
-- @format@ — If non-nil, the format of the source node's output bus is set to this		format. In all cases, the format of the destination nodes' input bus is set to		match that of the source node's output bus
--
-- Use this method to establish connections from a source node to multiple destination nodes.	Connections made using this method are either one-to-one (when a single destination	connection is specified) or one-to-many (when multiple connections are specified), but 	never many-to-one.
--
-- To incrementally add a new connection to a source node, use this method with an array	of AVAudioConnectionPoint objects comprising of pre-existing connections (obtained from	@outputConnectionPointsForNode:outputBus:@) and the new connection.
--
-- Note that any pre-existing connection involving the destination's input bus will be 	broken. And, any pre-existing connection on source node which is not a part of the	specified destination connection array will also be broken.
--
-- Also note that when the output of a node is split into multiple paths, all the paths	must render at the same rate until they reach a common mixer.	In other words, starting from the split node until the common mixer node where all split 	paths terminate, you cannot have:		- any AVAudioUnitTimeEffect		- any sample rate conversion
--
-- ObjC selector: @- connect:toConnectionPoints:fromBus:format:@
connect_toConnectionPoints_fromBus_format :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsNSArray destNodes, IsAVAudioFormat format) => avAudioEngine -> sourceNode -> destNodes -> CULong -> format -> IO ()
connect_toConnectionPoints_fromBus_format avAudioEngine sourceNode destNodes sourceBus format =
  sendMessage avAudioEngine connect_toConnectionPoints_fromBus_formatSelector (toAVAudioNode sourceNode) (toNSArray destNodes) sourceBus (toAVAudioFormat format)

-- | disconnectNodeInput:bus:
--
-- Remove a connection between two nodes.
--
-- @node@ — The node whose input is to be disconnected
--
-- @bus@ — The destination's input bus to disconnect
--
-- ObjC selector: @- disconnectNodeInput:bus:@
disconnectNodeInput_bus :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> CULong -> IO ()
disconnectNodeInput_bus avAudioEngine node bus =
  sendMessage avAudioEngine disconnectNodeInput_busSelector (toAVAudioNode node) bus

-- | disconnectNodeInput:
--
-- Remove a connection between two nodes.
--
-- @node@ — The node whose inputs are to be disconnected
--
-- Connections are broken on each of the node's input busses.
--
-- ObjC selector: @- disconnectNodeInput:@
disconnectNodeInput :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> IO ()
disconnectNodeInput avAudioEngine node =
  sendMessage avAudioEngine disconnectNodeInputSelector (toAVAudioNode node)

-- | disconnectNodeOutput:bus:
--
-- Remove a connection between two nodes.
--
-- @node@ — The node whose output is to be disconnected
--
-- @bus@ — The source's output bus to disconnect
--
-- ObjC selector: @- disconnectNodeOutput:bus:@
disconnectNodeOutput_bus :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> CULong -> IO ()
disconnectNodeOutput_bus avAudioEngine node bus =
  sendMessage avAudioEngine disconnectNodeOutput_busSelector (toAVAudioNode node) bus

-- | disconnectNodeOutput:
--
-- Remove a connection between two nodes.
--
-- @node@ — The node whose outputs are to be disconnected
--
-- Connections are broken on each of the node's output busses.
--
-- ObjC selector: @- disconnectNodeOutput:@
disconnectNodeOutput :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> IO ()
disconnectNodeOutput avAudioEngine node =
  sendMessage avAudioEngine disconnectNodeOutputSelector (toAVAudioNode node)

-- | prepare
--
-- Prepare the engine for starting.
--
-- This method preallocates many of the resources the engine requires in order to start.    Use it to responsively start audio input or output.
--
-- On AVAudioSession supported platforms, this method may cause the audio session to be implicitly activated. Activating the audio session (implicitly or explicitly) may cause other audio sessions to be interrupted or ducked depending on the session's configuration. It is recommended to configure and activate the app's audio session before preparing the engine.    See https://developer.apple.com/library/archive/documentation/Audio/Conceptual/AudioSessionProgrammingGuide/Introduction/Introduction.html for details.
--
-- ObjC selector: @- prepare@
prepare :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO ()
prepare avAudioEngine =
  sendMessage avAudioEngine prepareSelector

-- | startAndReturnError:
--
-- Start the engine.
--
-- Returns: YES for success
--
-- Calls prepare if it has not already been called since stop.
--
-- When the engine is rendering to/from an audio device, starts the audio hardware via the	AVAudioInputNode and/or AVAudioOutputNode instances in the engine. Audio begins to flow 	through the engine.	Reasons for potential failure to start in this mode include:	1. There is problem in the structure of the graph. Input can't be routed to output or to a		recording tap through converter type nodes.	2. An AVAudioSession error.	3. The driver failed to start the hardware.
--
-- In manual rendering mode, prepares the engine to render when requested by the client.
--
-- On AVAudioSession supported platforms, this method may cause the audio session to be implicitly activated. It is recommended to configure and activate the app's audio session before starting the engine. For more information, see the @prepare@ method above.
--
-- ObjC selector: @- startAndReturnError:@
startAndReturnError :: (IsAVAudioEngine avAudioEngine, IsNSError outError) => avAudioEngine -> outError -> IO Bool
startAndReturnError avAudioEngine outError =
  sendMessage avAudioEngine startAndReturnErrorSelector (toNSError outError)

-- | pause
--
-- Pause the engine.
--
-- When the engine is rendering to/from an audio device, stops the audio hardware and the flow	of audio through the engine. When operating in this mode, it is recommended that the engine	be paused or stopped (as applicable) when not in use, to minimize power consumption.
--
-- Pausing the engine does not deallocate the resources allocated by prepare. Resume the	engine by invoking start again.
--
-- ObjC selector: @- pause@
pause :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO ()
pause avAudioEngine =
  sendMessage avAudioEngine pauseSelector

-- | reset
--
-- reset		Reset all of the nodes in the engine.
--
-- This will reset all of the nodes in the engine. This is useful, for example, for silencing	reverb and delay tails.
--
-- In manual rendering mode, the render timeline is reset to a sample time of zero.
--
-- ObjC selector: @- reset@
reset :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO ()
reset avAudioEngine =
  sendMessage avAudioEngine resetSelector

-- | stop
--
-- When the engine is rendering to/from an audio device, stops the audio hardware and the		engine. When operating in this mode, it is recommended that the engine be paused or stopped		 (as applicable) when not in use, to minimize power consumption.
--
-- Stopping the engine releases the resources allocated by prepare.
--
-- ObjC selector: @- stop@
stop :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO ()
stop avAudioEngine =
  sendMessage avAudioEngine stopSelector

-- | inputConnectionPointForNode:inputBus:
--
-- Get connection information on a node's input bus.
--
-- @node@ — The node whose input connection is being queried.
--
-- @bus@ — The node's input bus on which the connection is being queried.
--
-- Returns: An AVAudioConnectionPoint object with connection information on the node's		specified input bus.
--
-- Connections are always one-to-one or one-to-many, never many-to-one.
--
-- Returns nil if there is no connection on the node's specified input bus.
--
-- ObjC selector: @- inputConnectionPointForNode:inputBus:@
inputConnectionPointForNode_inputBus :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> CULong -> IO (Id AVAudioConnectionPoint)
inputConnectionPointForNode_inputBus avAudioEngine node bus =
  sendMessage avAudioEngine inputConnectionPointForNode_inputBusSelector (toAVAudioNode node) bus

-- | outputConnectionPointsForNode:outputBus:
--
-- Get connection information on a node's output bus.
--
-- @node@ — The node whose output connections are being queried.
--
-- @bus@ — The node's output bus on which connections are being queried.
--
-- Returns: An array of AVAudioConnectionPoint objects with connection information on the node's		specified output bus.
--
-- Connections are always one-to-one or one-to-many, never many-to-one.
--
-- Returns an empty array if there are no connections on the node's specified output bus.
--
-- ObjC selector: @- outputConnectionPointsForNode:outputBus:@
outputConnectionPointsForNode_outputBus :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> CULong -> IO (Id NSArray)
outputConnectionPointsForNode_outputBus avAudioEngine node bus =
  sendMessage avAudioEngine outputConnectionPointsForNode_outputBusSelector (toAVAudioNode node) bus

-- | enableManualRenderingMode:format:maximumFrameCount:error:
--
-- Set the engine to operate in a manual rendering mode with the specified render format and		maximum frame count.
--
-- @mode@ — The manual rendering mode to use.
--
-- @pcmFormat@ — The format of the output PCM audio data from the engine.
--
-- @maximumFrameCount@ — The maximum number of PCM sample frames the engine will be asked to produce in any single		render call.
--
-- @outError@ — On exit, if the engine cannot switch to the manual rendering mode, a description of the		error (see @AVAudioEngineManualRenderingError@ for the possible errors).
--
-- Returns: YES for success.
--
-- Use this method to configure the engine to render in response to requests from the client.
--
-- The engine must be in a stopped state before calling this method.	The render format must be a PCM format and match the format of the buffer to which	the engine is asked to render (see @renderOffline:toBuffer:error:@).
--
-- It is advised to enable manual rendering mode soon after the engine is created, and	before accessing any of mainMixerNode, inputNode or outputNode of the engine.	Otherwise, accessing or interacting with the engine before enabling manual rendering	mode could have the unintended side-effect of configuring the hardware for device-rendering	mode.
--
-- The input data in manual rendering mode can be supplied through the source nodes, e.g.	@AVAudioPlayerNode@, @AVAudioInputNode@ etc.
--
-- When switching to manual rendering mode, the engine:	1. Switches the input and output nodes to manual rendering mode. Their input and output	   formats may change.	2. Removes any taps previously installed on the input and output nodes.	3. Maintains all the engine connections as is.
--
-- Reasons for potential failure when switching to manual rendering mode include:	- Engine is not in a stopped state.
--
-- ObjC selector: @- enableManualRenderingMode:format:maximumFrameCount:error:@
enableManualRenderingMode_format_maximumFrameCount_error :: (IsAVAudioEngine avAudioEngine, IsAVAudioFormat pcmFormat, IsNSError outError) => avAudioEngine -> AVAudioEngineManualRenderingMode -> pcmFormat -> CUInt -> outError -> IO Bool
enableManualRenderingMode_format_maximumFrameCount_error avAudioEngine mode pcmFormat maximumFrameCount outError =
  sendMessage avAudioEngine enableManualRenderingMode_format_maximumFrameCount_errorSelector mode (toAVAudioFormat pcmFormat) maximumFrameCount (toNSError outError)

-- | disableManualRenderingMode
--
-- Set the engine to render to/from an audio device.
--
-- When disabling the manual rendering mode, the engine:	1. Stops and resets itself (see @stop@ and @reset@).	2. Switches the output/input nodes to render to/from an audio device. Their input and	   output formats may change.	3. Removes any taps previously installed on the input and output nodes.	4. Maintains all the engine connections as is.
--
-- Calling this method when the engine is already rendering to/from an audio device has no 	effect.
--
-- ObjC selector: @- disableManualRenderingMode@
disableManualRenderingMode :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO ()
disableManualRenderingMode avAudioEngine =
  sendMessage avAudioEngine disableManualRenderingModeSelector

-- | renderOffline:toBuffer:error:
--
-- Render call to the engine operating in the offline manual rendering mode
--
-- @numberOfFrames@ — The number of PCM sample frames to be rendered
--
-- @buffer@ — The PCM buffer to which the engine must render the audio
--
-- @outError@ — On exit, if an error occurs during rendering, a description of the error (see		@AVAudioEngineManualRenderingError@ for the possible errors)
--
-- Returns: One of the status codes from @AVAudioEngineManualRenderingStatus@. Irrespective of the		returned status code, on exit, the output buffer's frameLength will indicate the number of		PCM samples rendered by the engine
--
-- The engine must be in the offline manual rendering mode 	(@AVAudioEngineManualRenderingModeOffline@) and started before calling this method.
--
-- The format of the buffer must match the render format set through 	@enableManualRenderingMode:format:maximumFrameCount:error:@. The buffer capacity must be	greater than or equal to the number of samples asked to render.	On exit, the buffer's frameLength will indicate the number of PCM samples rendered by the 	engine.
--
-- The engine's timeline in manual rendering mode starts at a sample time of zero, and is in	terms of the render format's sample rate. Resetting the engine (see @reset@) will reset the	timeline back to zero.
--
-- When rendering in @AVAudioEngineManualRenderingModeRealtime@, this ObjC render method 	must not be used, an error is returned otherwise. Use the block based render call	(@manualRenderingBlock@) in that mode instead.
--
-- ObjC selector: @- renderOffline:toBuffer:error:@
renderOffline_toBuffer_error :: (IsAVAudioEngine avAudioEngine, IsAVAudioPCMBuffer buffer, IsNSError outError) => avAudioEngine -> CUInt -> buffer -> outError -> IO AVAudioEngineManualRenderingStatus
renderOffline_toBuffer_error avAudioEngine numberOfFrames buffer outError =
  sendMessage avAudioEngine renderOffline_toBuffer_errorSelector numberOfFrames (toAVAudioPCMBuffer buffer) (toNSError outError)

-- | connectMIDI:to:format:block:
--
-- Establish a MIDI only connection between two nodes.
--
-- @sourceNode@ — The source node.
--
-- @destinationNode@ — The destination node.
--
-- @format@ — If non-nil, the format of the source node's output bus is set to this format.        In all cases, the format of the source nodes' output bus has to match with the        destination nodes' output bus format.        Although the output bus of the source is not in use, the format needs to be set        in order to be able to use the sample rate for MIDI event timing calculations.
--
-- @tapBlock@ — If non-nil, this block is called from the source node's @AUMIDIOutputEventBlock@        on the realtime thread. The host can tap the MIDI data of the source node through        this block. May be nil.
--
-- Use this method to establish a MIDI only connection between a source node and a	destination node that has MIDI input capability.
--
-- The source node can only be a AVAudioUnit node of type @kAudioUnitType_MIDIProcessor@.	The destination node types can be @kAudioUnitType_MusicDevice@,	@kAudioUnitType_MusicEffect@ or @kAudioUnitType_MIDIProcessor@.
--
-- Note that any pre-existing MIDI connection involving the destination will be broken.
--
-- Any client installed block on the source node's audio unit @AUMIDIOutputEventBlock@	will be overwritten when making the MIDI connection.
--
-- ObjC selector: @- connectMIDI:to:format:block:@
connectMIDI_to_format_block :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsAVAudioNode destinationNode, IsAVAudioFormat format) => avAudioEngine -> sourceNode -> destinationNode -> format -> Ptr () -> IO ()
connectMIDI_to_format_block avAudioEngine sourceNode destinationNode format tapBlock =
  sendMessage avAudioEngine connectMIDI_to_format_blockSelector (toAVAudioNode sourceNode) (toAVAudioNode destinationNode) (toAVAudioFormat format) tapBlock

-- | connectMIDI:to:format:eventListblock:
--
-- Establish a MIDI only connection between two nodes.
--
-- @sourceNode@ — The source node.
--
-- @destinationNode@ — The destination node.
--
-- @format@ — If non-nil, the format of the source node's output bus is set to this format.        In all cases, the format of the source nodes' output bus has to match with the        destination nodes' output bus format.        Although the output bus of the source is not in use, the format needs to be set        in order to be able to use the sample rate for MIDI event timing calculations.
--
-- @tapBlock@ — This block is called from the source node's @AUMIDIOutputEventListBlock@        on the realtime thread. The host can tap the MIDI data of the source node through        this block.
--
-- Use this method to establish a MIDI only connection between a source node and a    destination node that has MIDI input capability.
--
-- The source node can only be a AVAudioUnit node of type @kAudioUnitType_MIDIProcessor@.    The destination node types can be @kAudioUnitType_MusicDevice@,    @kAudioUnitType_MusicEffect@ or @kAudioUnitType_MIDIProcessor@.
--
-- Note that any pre-existing MIDI connection involving the destination will be broken.
--
-- Any client installed block on the source node's audio unit @AUMIDIOutputEventListBlock@    will be overwritten when making the MIDI connection.
--
-- ObjC selector: @- connectMIDI:to:format:eventListBlock:@
connectMIDI_to_format_eventListBlock :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsAVAudioNode destinationNode, IsAVAudioFormat format) => avAudioEngine -> sourceNode -> destinationNode -> format -> Ptr () -> IO ()
connectMIDI_to_format_eventListBlock avAudioEngine sourceNode destinationNode format tapBlock =
  sendMessage avAudioEngine connectMIDI_to_format_eventListBlockSelector (toAVAudioNode sourceNode) (toAVAudioNode destinationNode) (toAVAudioFormat format) tapBlock

-- | connectMIDI:toNodes:format:block:
--
-- Establish a MIDI only connection between a source node and multiple destination nodes.
--
-- @sourceNode@ — The source node.
--
-- @destinationNodes@ — An array of AVAudioNodes specifying destination nodes.
--
-- @format@ — If non-nil, the format of the source node's output bus is set to this format.        In all cases, the format of the source nodes' output bus has to match with the        destination nodes' output bus format.        Although the output bus of the source is not in use, the format needs to be set        in order to be able to use the sample rate for MIDI event timing calculations.
--
-- @tapBlock@ — If non-nil, this block is called from the source node's @AUMIDIOutputEventBlock@        on the realtime thread. The host can tap the MIDI data of the source node through        this block. May be nil.
--
-- Use this method to establish a MIDI only connection between a source node and	multiple destination nodes.
--
-- The source node can only be a AVAudioUnit node of type @kAudioUnitType_MIDIProcessor@.	The destination node types can be @kAudioUnitType_MusicDevice@,	@kAudioUnitType_MusicEffect@ or @kAudioUnitType_MIDIProcessor@.
--
-- MIDI connections made using this method are either one-to-one (when a single	destination connection is specified) or one-to-many (when multiple connections are	specified), but never many-to-one.
--
-- Note that any pre-existing connection involving the destination will be broken.
--
-- Any client installed block on the source node's audio unit @AUMIDIOutputEventBlock@	will be overwritten when making the MIDI connection.
--
-- ObjC selector: @- connectMIDI:toNodes:format:block:@
connectMIDI_toNodes_format_block :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsNSArray destinationNodes, IsAVAudioFormat format) => avAudioEngine -> sourceNode -> destinationNodes -> format -> Ptr () -> IO ()
connectMIDI_toNodes_format_block avAudioEngine sourceNode destinationNodes format tapBlock =
  sendMessage avAudioEngine connectMIDI_toNodes_format_blockSelector (toAVAudioNode sourceNode) (toNSArray destinationNodes) (toAVAudioFormat format) tapBlock

-- | connectMIDI:toNodes:format:eventListBlock:
--
-- Establish a MIDI only connection between a source node and multiple destination nodes.
--
-- @sourceNode@ — The source node.
--
-- @destinationNodes@ — An array of AVAudioNodes specifying destination nodes.
--
-- @format@ — If non-nil, the format of the source node's output bus is set to this format.        In all cases, the format of the source nodes' output bus has to match with the        destination nodes' output bus format.        Although the output bus of the source is not in use, the format needs to be set        in order to be able to use the sample rate for MIDI event timing calculations.
--
-- @tapBlock@ — This block is called from the source node's @AUMIDIOutputEventListBlock@        on the realtime thread. The host can tap the MIDI data of the source node through        this block.
--
-- Use this method to establish a MIDI only connection between a source node and    multiple destination nodes.
--
-- The source node can only be a AVAudioUnit node of type @kAudioUnitType_MIDIProcessor@.    The destination node types can be @kAudioUnitType_MusicDevice@,    @kAudioUnitType_MusicEffect@ or @kAudioUnitType_MIDIProcessor@.
--
-- MIDI connections made using this method are either one-to-one (when a single    destination connection is specified) or one-to-many (when multiple connections are    specified), but never many-to-one.
--
-- Note that any pre-existing connection involving the destination will be broken.
--
-- Any client installed block on the source node's audio unit @AUMIDIOutputEventListBlock@    will be overwritten when making the MIDI connection.
--
-- ObjC selector: @- connectMIDI:toNodes:format:eventListBlock:@
connectMIDI_toNodes_format_eventListBlock :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsNSArray destinationNodes, IsAVAudioFormat format) => avAudioEngine -> sourceNode -> destinationNodes -> format -> Ptr () -> IO ()
connectMIDI_toNodes_format_eventListBlock avAudioEngine sourceNode destinationNodes format tapBlock =
  sendMessage avAudioEngine connectMIDI_toNodes_format_eventListBlockSelector (toAVAudioNode sourceNode) (toNSArray destinationNodes) (toAVAudioFormat format) tapBlock

-- | disconnectMIDI:from:
--
-- Remove a MIDI connection between two nodes.
--
-- @sourceNode@ — The node whose MIDI output is to be disconnected.
--
-- @destinationNode@ — The node whose MIDI input is to be disconnected.
--
-- If a tap block is installed on the source node, it will be removed when the last	connection from the source node is removed.
--
-- ObjC selector: @- disconnectMIDI:from:@
disconnectMIDI_from :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsAVAudioNode destinationNode) => avAudioEngine -> sourceNode -> destinationNode -> IO ()
disconnectMIDI_from avAudioEngine sourceNode destinationNode =
  sendMessage avAudioEngine disconnectMIDI_fromSelector (toAVAudioNode sourceNode) (toAVAudioNode destinationNode)

-- | disconnectMIDI:fromNodes:
--
-- Remove a MIDI connection between one source node and multiple destination nodes.
--
-- @sourceNode@ — The node whose MIDI output is to be disconnected.
--
-- @destinationNodes@ — An array of AVAudioNodes specifying nodes whose MIDI input is to be disconnected.
--
-- If a tap block is installed on the source node, it will be removed when the last	connection from the source node is removed.
--
-- ObjC selector: @- disconnectMIDI:fromNodes:@
disconnectMIDI_fromNodes :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode sourceNode, IsNSArray destinationNodes) => avAudioEngine -> sourceNode -> destinationNodes -> IO ()
disconnectMIDI_fromNodes avAudioEngine sourceNode destinationNodes =
  sendMessage avAudioEngine disconnectMIDI_fromNodesSelector (toAVAudioNode sourceNode) (toNSArray destinationNodes)

-- | disconnectMIDIInput:
--
-- Disconnects all input MIDI connections of this node.
--
-- @node@ — The node whose MIDI input is to be disconnected.
--
-- ObjC selector: @- disconnectMIDIInput:@
disconnectMIDIInput :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> IO ()
disconnectMIDIInput avAudioEngine node =
  sendMessage avAudioEngine disconnectMIDIInputSelector (toAVAudioNode node)

-- | disconnectMIDIOutput:
--
-- Disconnects all output MIDI connections of this node.
--
-- @node@ — The node whose MIDI outputs are to be disconnected.
--
-- ObjC selector: @- disconnectMIDIOutput:@
disconnectMIDIOutput :: (IsAVAudioEngine avAudioEngine, IsAVAudioNode node) => avAudioEngine -> node -> IO ()
disconnectMIDIOutput avAudioEngine node =
  sendMessage avAudioEngine disconnectMIDIOutputSelector (toAVAudioNode node)

-- | musicSequence
--
-- The MusicSequence previously attached to the engine (if any).
--
-- ObjC selector: @- musicSequence@
musicSequence :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Ptr ())
musicSequence avAudioEngine =
  sendMessage avAudioEngine musicSequenceSelector

-- | musicSequence
--
-- The MusicSequence previously attached to the engine (if any).
--
-- ObjC selector: @- setMusicSequence:@
setMusicSequence :: IsAVAudioEngine avAudioEngine => avAudioEngine -> Ptr () -> IO ()
setMusicSequence avAudioEngine value =
  sendMessage avAudioEngine setMusicSequenceSelector value

-- | outputNode
--
-- The engine's singleton output node.
--
-- Audio output is performed via an output node. The engine creates a singleton on demand when	this property is first accessed. Connect another node to the input of the output node, or	obtain a mixer that is connected there by default, using the "mainMixerNode" property.
--
-- When the engine is rendering to/from an audio device, the AVAudioSesssion category and/or	availability of hardware determine whether an app can perform output. Check the output	format of output node (i.e. hardware format) for non-zero sample rate and channel count to	see if output is enabled. 	Trying to perform output through the output node when it is not enabled or available will 	cause the engine to throw an error (when possible) or an exception.
--
-- In manual rendering mode, the output format of the output node will determine the	render format of the engine. It can be changed through	@enableManualRenderingMode:format:maximumFrameCount:error:@.
--
-- ObjC selector: @- outputNode@
outputNode :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Id AVAudioOutputNode)
outputNode avAudioEngine =
  sendMessage avAudioEngine outputNodeSelector

-- | inputNode
--
-- The engine's singleton input node.
--
-- Audio input is performed via an input node. The engine creates a singleton on demand when	this property is first accessed. To receive input, connect another node from the output of 	the input node, or create a recording tap on it.
--
-- When the engine is rendering to/from an audio device, the AVAudioSesssion category and/or	availability of hardware determine whether an app can perform input. Check for the input node's    input format (i.e. hardware format) for non-zero sample rate and channel count to see if input is enabled.	Trying to perform input through the input node when it is not enabled or available will	cause the engine to throw an error (when possible) or an exception.
--
-- Note that if the engine has at any point previously had its inputNode enabled and permission to	record was granted, then any time the engine is running, the mic-in-use indicator will appear.
--
-- For applications which may need to dynamically switch between output-only and input-output	modes, it may be advantageous to use two engine instances.
--
-- In manual rendering mode, the input node can be used to synchronously supply data to	the engine while it is rendering (see 	@AVAudioInputNode(setManualRenderingInputPCMFormat:inputBlock:)@.
--
-- ObjC selector: @- inputNode@
inputNode :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Id AVAudioInputNode)
inputNode avAudioEngine =
  sendMessage avAudioEngine inputNodeSelector

-- | mainMixerNode
--
-- The engine's optional singleton main mixer node.
--
-- The engine will construct a singleton main mixer and connect it to the outputNode on demand,	when this property is first accessed. You can then connect additional nodes to the mixer.
--
-- If the client has never explicitly set the connection format between the mainMixerNode and	the outputNode, the engine will always set/update the format to track the format of the outputNode	on (re)start, even after an AVAudioEngineConfigurationChangeNotification.	Otherwise, it's the client's responsibility to set/update this connection format after an	AVAudioEngineConfigurationChangeNotification.
--
-- By default, the mixer's output format (sample rate and channel count) will track the format 	of the output node. You may however make the connection explicitly with a different format.
--
-- ObjC selector: @- mainMixerNode@
mainMixerNode :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Id AVAudioMixerNode)
mainMixerNode avAudioEngine =
  sendMessage avAudioEngine mainMixerNodeSelector

-- | running
--
-- The engine's running state.
--
-- ObjC selector: @- running@
running :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO Bool
running avAudioEngine =
  sendMessage avAudioEngine runningSelector

-- | autoShutdownEnabled
--
-- When auto shutdown is enabled, the engine can start and stop the audio hardware dynamically,		to conserve power. This is the enforced behavior on watchOS and can be optionally enabled on		other platforms.
--
-- To conserve power, it is advised that the client pause/stop the engine when not in use.	But when auto shutdown is enabled, the engine will stop the audio hardware if it was running 	idle for a certain duration, and restart it later when required.	Note that, because this operation is dynamic, it may affect the start times of the source 	nodes (e.g. @AVAudioPlayerNode@), if the engine has to resume from its shutdown state.
--
-- On watchOS, auto shutdown is always enabled. On other platforms, it is disabled by	default, but the client can enable it if needed.
--
-- This property is applicable only when the engine is rendering to/from an audio device. If	the value is changed when the engine is in manual rendering mode, it will take effect	whenever the engine is switched to render to/from the audio device.
--
-- ObjC selector: @- autoShutdownEnabled@
autoShutdownEnabled :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO Bool
autoShutdownEnabled avAudioEngine =
  sendMessage avAudioEngine autoShutdownEnabledSelector

-- | autoShutdownEnabled
--
-- When auto shutdown is enabled, the engine can start and stop the audio hardware dynamically,		to conserve power. This is the enforced behavior on watchOS and can be optionally enabled on		other platforms.
--
-- To conserve power, it is advised that the client pause/stop the engine when not in use.	But when auto shutdown is enabled, the engine will stop the audio hardware if it was running 	idle for a certain duration, and restart it later when required.	Note that, because this operation is dynamic, it may affect the start times of the source 	nodes (e.g. @AVAudioPlayerNode@), if the engine has to resume from its shutdown state.
--
-- On watchOS, auto shutdown is always enabled. On other platforms, it is disabled by	default, but the client can enable it if needed.
--
-- This property is applicable only when the engine is rendering to/from an audio device. If	the value is changed when the engine is in manual rendering mode, it will take effect	whenever the engine is switched to render to/from the audio device.
--
-- ObjC selector: @- setAutoShutdownEnabled:@
setAutoShutdownEnabled :: IsAVAudioEngine avAudioEngine => avAudioEngine -> Bool -> IO ()
setAutoShutdownEnabled avAudioEngine value =
  sendMessage avAudioEngine setAutoShutdownEnabledSelector value

-- | attachedNodes
--
-- Set of all nodes attached to the engine.
--
-- ObjC selector: @- attachedNodes@
attachedNodes :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Id NSSet)
attachedNodes avAudioEngine =
  sendMessage avAudioEngine attachedNodesSelector

-- | manualRenderingBlock
--
-- Block to render the engine operating in manual rendering mode
--
-- This block based render call must be used to render the engine when operating in	@AVAudioEngineManualRenderingModeRealtime@. In this mode, the engine operates under	realtime constraints and will not make any blocking call (e.g. calling libdispatch, blocking 	on a mutex, allocating memory etc.) while rendering.
--
-- Before invoking the rendering functionality, client must fetch this block and cache the	result. The block can then be called from a realtime context, without any possibility of 	blocking.
--
-- When rendering in @AVAudioEngineManualRenderingModeOffline@, either this block based render	call or	@renderOffline:toBuffer:error:@ ObjC method can be used.	All the rules outlined in @renderOffline:toBuffer:error:@ are applicable here as well.
--
-- ObjC selector: @- manualRenderingBlock@
manualRenderingBlock :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Ptr ())
manualRenderingBlock avAudioEngine =
  sendMessage avAudioEngine manualRenderingBlockSelector

-- | isInManualRenderingMode
--
-- Whether or not the engine is operating in manual rendering mode, i.e. not connected		to an audio device and rendering in response to the requests from the client
--
-- ObjC selector: @- isInManualRenderingMode@
isInManualRenderingMode :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO Bool
isInManualRenderingMode avAudioEngine =
  sendMessage avAudioEngine isInManualRenderingModeSelector

-- | manualRenderingMode
--
-- The manual rendering mode configured on the engine
--
-- This property is meaningful only when the engine is operating in manual rendering mode,	i.e. when @isInManualRenderingMode@ returns true.
--
-- ObjC selector: @- manualRenderingMode@
manualRenderingMode :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO AVAudioEngineManualRenderingMode
manualRenderingMode avAudioEngine =
  sendMessage avAudioEngine manualRenderingModeSelector

-- | manualRenderingFormat
--
-- The render format of the engine in manual rendering mode.
--
-- Querying this property when the engine is not in manual rendering mode will return an	invalid format, with zero sample rate and channel count.
--
-- ObjC selector: @- manualRenderingFormat@
manualRenderingFormat :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO (Id AVAudioFormat)
manualRenderingFormat avAudioEngine =
  sendMessage avAudioEngine manualRenderingFormatSelector

-- | manualRenderingMaximumFrameCount
--
-- The maximum number of PCM sample frames the engine can produce in any single render call in 		the manual rendering mode.
--
-- Querying this property when the engine is not in manual rendering mode will return zero.
--
-- ObjC selector: @- manualRenderingMaximumFrameCount@
manualRenderingMaximumFrameCount :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO CUInt
manualRenderingMaximumFrameCount avAudioEngine =
  sendMessage avAudioEngine manualRenderingMaximumFrameCountSelector

-- | manualRenderingSampleTime
--
-- Indicates where the engine is on its render timeline in manual rendering mode.
--
-- The timeline in manual rendering mode starts at a sample time of zero, and is in terms	of the render format's sample rate. Resetting the engine (see @reset@) will reset the	timeline back to zero.
--
-- ObjC selector: @- manualRenderingSampleTime@
manualRenderingSampleTime :: IsAVAudioEngine avAudioEngine => avAudioEngine -> IO CLong
manualRenderingSampleTime avAudioEngine =
  sendMessage avAudioEngine manualRenderingSampleTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioEngine)
initSelector = mkSelector "init"

-- | @Selector@ for @attachNode:@
attachNodeSelector :: Selector '[Id AVAudioNode] ()
attachNodeSelector = mkSelector "attachNode:"

-- | @Selector@ for @detachNode:@
detachNodeSelector :: Selector '[Id AVAudioNode] ()
detachNodeSelector = mkSelector "detachNode:"

-- | @Selector@ for @connect:to:fromBus:toBus:format:@
connect_to_fromBus_toBus_formatSelector :: Selector '[Id AVAudioNode, Id AVAudioNode, CULong, CULong, Id AVAudioFormat] ()
connect_to_fromBus_toBus_formatSelector = mkSelector "connect:to:fromBus:toBus:format:"

-- | @Selector@ for @connect:to:format:@
connect_to_formatSelector :: Selector '[Id AVAudioNode, Id AVAudioNode, Id AVAudioFormat] ()
connect_to_formatSelector = mkSelector "connect:to:format:"

-- | @Selector@ for @connect:toConnectionPoints:fromBus:format:@
connect_toConnectionPoints_fromBus_formatSelector :: Selector '[Id AVAudioNode, Id NSArray, CULong, Id AVAudioFormat] ()
connect_toConnectionPoints_fromBus_formatSelector = mkSelector "connect:toConnectionPoints:fromBus:format:"

-- | @Selector@ for @disconnectNodeInput:bus:@
disconnectNodeInput_busSelector :: Selector '[Id AVAudioNode, CULong] ()
disconnectNodeInput_busSelector = mkSelector "disconnectNodeInput:bus:"

-- | @Selector@ for @disconnectNodeInput:@
disconnectNodeInputSelector :: Selector '[Id AVAudioNode] ()
disconnectNodeInputSelector = mkSelector "disconnectNodeInput:"

-- | @Selector@ for @disconnectNodeOutput:bus:@
disconnectNodeOutput_busSelector :: Selector '[Id AVAudioNode, CULong] ()
disconnectNodeOutput_busSelector = mkSelector "disconnectNodeOutput:bus:"

-- | @Selector@ for @disconnectNodeOutput:@
disconnectNodeOutputSelector :: Selector '[Id AVAudioNode] ()
disconnectNodeOutputSelector = mkSelector "disconnectNodeOutput:"

-- | @Selector@ for @prepare@
prepareSelector :: Selector '[] ()
prepareSelector = mkSelector "prepare"

-- | @Selector@ for @startAndReturnError:@
startAndReturnErrorSelector :: Selector '[Id NSError] Bool
startAndReturnErrorSelector = mkSelector "startAndReturnError:"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @inputConnectionPointForNode:inputBus:@
inputConnectionPointForNode_inputBusSelector :: Selector '[Id AVAudioNode, CULong] (Id AVAudioConnectionPoint)
inputConnectionPointForNode_inputBusSelector = mkSelector "inputConnectionPointForNode:inputBus:"

-- | @Selector@ for @outputConnectionPointsForNode:outputBus:@
outputConnectionPointsForNode_outputBusSelector :: Selector '[Id AVAudioNode, CULong] (Id NSArray)
outputConnectionPointsForNode_outputBusSelector = mkSelector "outputConnectionPointsForNode:outputBus:"

-- | @Selector@ for @enableManualRenderingMode:format:maximumFrameCount:error:@
enableManualRenderingMode_format_maximumFrameCount_errorSelector :: Selector '[AVAudioEngineManualRenderingMode, Id AVAudioFormat, CUInt, Id NSError] Bool
enableManualRenderingMode_format_maximumFrameCount_errorSelector = mkSelector "enableManualRenderingMode:format:maximumFrameCount:error:"

-- | @Selector@ for @disableManualRenderingMode@
disableManualRenderingModeSelector :: Selector '[] ()
disableManualRenderingModeSelector = mkSelector "disableManualRenderingMode"

-- | @Selector@ for @renderOffline:toBuffer:error:@
renderOffline_toBuffer_errorSelector :: Selector '[CUInt, Id AVAudioPCMBuffer, Id NSError] AVAudioEngineManualRenderingStatus
renderOffline_toBuffer_errorSelector = mkSelector "renderOffline:toBuffer:error:"

-- | @Selector@ for @connectMIDI:to:format:block:@
connectMIDI_to_format_blockSelector :: Selector '[Id AVAudioNode, Id AVAudioNode, Id AVAudioFormat, Ptr ()] ()
connectMIDI_to_format_blockSelector = mkSelector "connectMIDI:to:format:block:"

-- | @Selector@ for @connectMIDI:to:format:eventListBlock:@
connectMIDI_to_format_eventListBlockSelector :: Selector '[Id AVAudioNode, Id AVAudioNode, Id AVAudioFormat, Ptr ()] ()
connectMIDI_to_format_eventListBlockSelector = mkSelector "connectMIDI:to:format:eventListBlock:"

-- | @Selector@ for @connectMIDI:toNodes:format:block:@
connectMIDI_toNodes_format_blockSelector :: Selector '[Id AVAudioNode, Id NSArray, Id AVAudioFormat, Ptr ()] ()
connectMIDI_toNodes_format_blockSelector = mkSelector "connectMIDI:toNodes:format:block:"

-- | @Selector@ for @connectMIDI:toNodes:format:eventListBlock:@
connectMIDI_toNodes_format_eventListBlockSelector :: Selector '[Id AVAudioNode, Id NSArray, Id AVAudioFormat, Ptr ()] ()
connectMIDI_toNodes_format_eventListBlockSelector = mkSelector "connectMIDI:toNodes:format:eventListBlock:"

-- | @Selector@ for @disconnectMIDI:from:@
disconnectMIDI_fromSelector :: Selector '[Id AVAudioNode, Id AVAudioNode] ()
disconnectMIDI_fromSelector = mkSelector "disconnectMIDI:from:"

-- | @Selector@ for @disconnectMIDI:fromNodes:@
disconnectMIDI_fromNodesSelector :: Selector '[Id AVAudioNode, Id NSArray] ()
disconnectMIDI_fromNodesSelector = mkSelector "disconnectMIDI:fromNodes:"

-- | @Selector@ for @disconnectMIDIInput:@
disconnectMIDIInputSelector :: Selector '[Id AVAudioNode] ()
disconnectMIDIInputSelector = mkSelector "disconnectMIDIInput:"

-- | @Selector@ for @disconnectMIDIOutput:@
disconnectMIDIOutputSelector :: Selector '[Id AVAudioNode] ()
disconnectMIDIOutputSelector = mkSelector "disconnectMIDIOutput:"

-- | @Selector@ for @musicSequence@
musicSequenceSelector :: Selector '[] (Ptr ())
musicSequenceSelector = mkSelector "musicSequence"

-- | @Selector@ for @setMusicSequence:@
setMusicSequenceSelector :: Selector '[Ptr ()] ()
setMusicSequenceSelector = mkSelector "setMusicSequence:"

-- | @Selector@ for @outputNode@
outputNodeSelector :: Selector '[] (Id AVAudioOutputNode)
outputNodeSelector = mkSelector "outputNode"

-- | @Selector@ for @inputNode@
inputNodeSelector :: Selector '[] (Id AVAudioInputNode)
inputNodeSelector = mkSelector "inputNode"

-- | @Selector@ for @mainMixerNode@
mainMixerNodeSelector :: Selector '[] (Id AVAudioMixerNode)
mainMixerNodeSelector = mkSelector "mainMixerNode"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @autoShutdownEnabled@
autoShutdownEnabledSelector :: Selector '[] Bool
autoShutdownEnabledSelector = mkSelector "autoShutdownEnabled"

-- | @Selector@ for @setAutoShutdownEnabled:@
setAutoShutdownEnabledSelector :: Selector '[Bool] ()
setAutoShutdownEnabledSelector = mkSelector "setAutoShutdownEnabled:"

-- | @Selector@ for @attachedNodes@
attachedNodesSelector :: Selector '[] (Id NSSet)
attachedNodesSelector = mkSelector "attachedNodes"

-- | @Selector@ for @manualRenderingBlock@
manualRenderingBlockSelector :: Selector '[] (Ptr ())
manualRenderingBlockSelector = mkSelector "manualRenderingBlock"

-- | @Selector@ for @isInManualRenderingMode@
isInManualRenderingModeSelector :: Selector '[] Bool
isInManualRenderingModeSelector = mkSelector "isInManualRenderingMode"

-- | @Selector@ for @manualRenderingMode@
manualRenderingModeSelector :: Selector '[] AVAudioEngineManualRenderingMode
manualRenderingModeSelector = mkSelector "manualRenderingMode"

-- | @Selector@ for @manualRenderingFormat@
manualRenderingFormatSelector :: Selector '[] (Id AVAudioFormat)
manualRenderingFormatSelector = mkSelector "manualRenderingFormat"

-- | @Selector@ for @manualRenderingMaximumFrameCount@
manualRenderingMaximumFrameCountSelector :: Selector '[] CUInt
manualRenderingMaximumFrameCountSelector = mkSelector "manualRenderingMaximumFrameCount"

-- | @Selector@ for @manualRenderingSampleTime@
manualRenderingSampleTimeSelector :: Selector '[] CLong
manualRenderingSampleTimeSelector = mkSelector "manualRenderingSampleTime"

