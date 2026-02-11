{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for MIDI instruments.
--
-- Generated bindings for @AVAudioUnitMIDIInstrument@.
module ObjC.AVFAudio.AVAudioUnitMIDIInstrument
  ( AVAudioUnitMIDIInstrument
  , IsAVAudioUnitMIDIInstrument(..)
  , initWithAudioComponentDescription
  , startNote_withVelocity_onChannel
  , stopNote_onChannel
  , sendController_withValue_onChannel
  , sendPitchBend_onChannel
  , sendPressure_onChannel
  , sendPressureForKey_withValue_onChannel
  , sendProgramChange_onChannel
  , sendProgramChange_bankMSB_bankLSB_onChannel
  , sendMIDIEvent_data1_data2
  , sendMIDIEvent_data1
  , sendMIDISysExEvent
  , initWithAudioComponentDescriptionSelector
  , startNote_withVelocity_onChannelSelector
  , stopNote_onChannelSelector
  , sendController_withValue_onChannelSelector
  , sendPitchBend_onChannelSelector
  , sendPressure_onChannelSelector
  , sendPressureForKey_withValue_onChannelSelector
  , sendProgramChange_onChannelSelector
  , sendProgramChange_bankMSB_bankLSB_onChannelSelector
  , sendMIDIEvent_data1_data2Selector
  , sendMIDIEvent_data1Selector
  , sendMIDISysExEventSelector


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
import ObjC.AudioToolbox.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Initialize the node with the component description for an AUv2 Audio Unit.
--
-- - Parameter description: audio component description structure that describes the audio component of type kAudioUnitType_MusicDevice   or kAudioUnitType_RemoteInstrument.
--
-- - note: To load AUv3 audio units (or any audio unit asynchronously), use the class method ``AVAudioUnit/instantiateWithComponentDescription:options:completionHandler:`` instead.
--
-- ObjC selector: @- initWithAudioComponentDescription:@
initWithAudioComponentDescription :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> AudioComponentDescription -> IO (Id AVAudioUnitMIDIInstrument)
initWithAudioComponentDescription avAudioUnitMIDIInstrument  description =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "initWithAudioComponentDescription:") (retPtr retVoid) [argAudioComponentDescription description] >>= ownedObject . castPtr

-- | Sends a MIDI Note On event to the instrument
--
-- - Parameters:   - note: the note number (key) to play. Range: 0 -> 127   - velocity: specifies the volume with which the note is played. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- startNote:withVelocity:onChannel:@
startNote_withVelocity_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> CUChar -> IO ()
startNote_withVelocity_onChannel avAudioUnitMIDIInstrument  note velocity channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "startNote:withVelocity:onChannel:") retVoid [argCUChar (fromIntegral note), argCUChar (fromIntegral velocity), argCUChar (fromIntegral channel)]

-- | Sends a MIDI Note Off event to the instrument
--
-- - Parameters:   - note: the note number (key) to stop. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- stopNote:onChannel:@
stopNote_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> IO ()
stopNote_onChannel avAudioUnitMIDIInstrument  note channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "stopNote:onChannel:") retVoid [argCUChar (fromIntegral note), argCUChar (fromIntegral channel)]

-- | Sends a MIDI controller event to the instrument.
--
-- - Parameters:   - controller: a standard MIDI controller number. Range: 0 -> 127   - value: value for the controller. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- sendController:withValue:onChannel:@
sendController_withValue_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> CUChar -> IO ()
sendController_withValue_onChannel avAudioUnitMIDIInstrument  controller value channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendController:withValue:onChannel:") retVoid [argCUChar (fromIntegral controller), argCUChar (fromIntegral value), argCUChar (fromIntegral channel)]

-- | Sends a MIDI controller event to the instrument.
--
-- - Parameters:   - controller: a standard MIDI controller number. Range: 0 -> 127   - value: value for the controller. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- sendPitchBend:onChannel:@
sendPitchBend_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUShort -> CUChar -> IO ()
sendPitchBend_onChannel avAudioUnitMIDIInstrument  pitchbend channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendPitchBend:onChannel:") retVoid [argCUInt (fromIntegral pitchbend), argCUChar (fromIntegral channel)]

-- | Sends MIDI channel pressure event to the instrument.
--
-- - Parameters:   - pressure: value of the pressure. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- sendPressure:onChannel:@
sendPressure_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> IO ()
sendPressure_onChannel avAudioUnitMIDIInstrument  pressure channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendPressure:onChannel:") retVoid [argCUChar (fromIntegral pressure), argCUChar (fromIntegral channel)]

-- | Sends MIDI Polyphonic key pressure event to the instrument
--
-- - Parameters:   - key: the key (note) number to which the pressure event applies. Range: 0 -> 127   - value: value of the pressure. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15so
--
-- ObjC selector: @- sendPressureForKey:withValue:onChannel:@
sendPressureForKey_withValue_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> CUChar -> IO ()
sendPressureForKey_withValue_onChannel avAudioUnitMIDIInstrument  key value channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendPressureForKey:withValue:onChannel:") retVoid [argCUChar (fromIntegral key), argCUChar (fromIntegral value), argCUChar (fromIntegral channel)]

-- | Sends MIDI Program Change event to the instrument
--
-- The instrument will be loaded from the bank that has been previous set by MIDI Bank Select   controller messages (0 and 31). If none has been set, bank 0 will be used. - Parameters:   - program: the program number. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- sendProgramChange:onChannel:@
sendProgramChange_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> IO ()
sendProgramChange_onChannel avAudioUnitMIDIInstrument  program channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendProgramChange:onChannel:") retVoid [argCUChar (fromIntegral program), argCUChar (fromIntegral channel)]

-- | Sends a MIDI Program Change and Bank Select events to the instrument
--
-- - Parameters:   - program: specifies the program (preset) number within the bank to load. Range: 0 -> 127   - bankMSB: specifies the most significant byte value for the bank to select. Range: 0 -> 127   - bankLSB: specifies the least significant byte value for the bank to select. Range: 0 -> 127   - channel: the channel number to which the event is sent. Range: 0 -> 15
--
-- ObjC selector: @- sendProgramChange:bankMSB:bankLSB:onChannel:@
sendProgramChange_bankMSB_bankLSB_onChannel :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> CUChar -> CUChar -> IO ()
sendProgramChange_bankMSB_bankLSB_onChannel avAudioUnitMIDIInstrument  program bankMSB bankLSB channel =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendProgramChange:bankMSB:bankLSB:onChannel:") retVoid [argCUChar (fromIntegral program), argCUChar (fromIntegral bankMSB), argCUChar (fromIntegral bankLSB), argCUChar (fromIntegral channel)]

-- | Sends a MIDI event which contains two data bytes to the instrument.
--
-- - Parameters:   - midiStatus: the STATUS value of the MIDI event   - data1: the first data byte of the MIDI event   - data2: the second data byte of the MIDI event.
--
-- ObjC selector: @- sendMIDIEvent:data1:data2:@
sendMIDIEvent_data1_data2 :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> CUChar -> IO ()
sendMIDIEvent_data1_data2 avAudioUnitMIDIInstrument  midiStatus data1 data2 =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendMIDIEvent:data1:data2:") retVoid [argCUChar (fromIntegral midiStatus), argCUChar (fromIntegral data1), argCUChar (fromIntegral data2)]

-- | Sends a MIDI event which contains one data byte to the instrument.
--
-- - Parameters:   - midiStatus: the STATUS value of the MIDI event   - data1: the first data byte of the MIDI event
--
-- ObjC selector: @- sendMIDIEvent:data1:@
sendMIDIEvent_data1 :: IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument => avAudioUnitMIDIInstrument -> CUChar -> CUChar -> IO ()
sendMIDIEvent_data1 avAudioUnitMIDIInstrument  midiStatus data1 =
  sendMsg avAudioUnitMIDIInstrument (mkSelector "sendMIDIEvent:data1:") retVoid [argCUChar (fromIntegral midiStatus), argCUChar (fromIntegral data1)]

-- | Sends a MIDI System Exclusive event to the instrument.
--
-- - Parameters:   - midiData: a NSData object containing the complete SysEx data including start(F0) and termination(F7) bytes.
--
-- ObjC selector: @- sendMIDISysExEvent:@
sendMIDISysExEvent :: (IsAVAudioUnitMIDIInstrument avAudioUnitMIDIInstrument, IsNSData midiData) => avAudioUnitMIDIInstrument -> midiData -> IO ()
sendMIDISysExEvent avAudioUnitMIDIInstrument  midiData =
withObjCPtr midiData $ \raw_midiData ->
    sendMsg avAudioUnitMIDIInstrument (mkSelector "sendMIDISysExEvent:") retVoid [argPtr (castPtr raw_midiData :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioComponentDescription:@
initWithAudioComponentDescriptionSelector :: Selector
initWithAudioComponentDescriptionSelector = mkSelector "initWithAudioComponentDescription:"

-- | @Selector@ for @startNote:withVelocity:onChannel:@
startNote_withVelocity_onChannelSelector :: Selector
startNote_withVelocity_onChannelSelector = mkSelector "startNote:withVelocity:onChannel:"

-- | @Selector@ for @stopNote:onChannel:@
stopNote_onChannelSelector :: Selector
stopNote_onChannelSelector = mkSelector "stopNote:onChannel:"

-- | @Selector@ for @sendController:withValue:onChannel:@
sendController_withValue_onChannelSelector :: Selector
sendController_withValue_onChannelSelector = mkSelector "sendController:withValue:onChannel:"

-- | @Selector@ for @sendPitchBend:onChannel:@
sendPitchBend_onChannelSelector :: Selector
sendPitchBend_onChannelSelector = mkSelector "sendPitchBend:onChannel:"

-- | @Selector@ for @sendPressure:onChannel:@
sendPressure_onChannelSelector :: Selector
sendPressure_onChannelSelector = mkSelector "sendPressure:onChannel:"

-- | @Selector@ for @sendPressureForKey:withValue:onChannel:@
sendPressureForKey_withValue_onChannelSelector :: Selector
sendPressureForKey_withValue_onChannelSelector = mkSelector "sendPressureForKey:withValue:onChannel:"

-- | @Selector@ for @sendProgramChange:onChannel:@
sendProgramChange_onChannelSelector :: Selector
sendProgramChange_onChannelSelector = mkSelector "sendProgramChange:onChannel:"

-- | @Selector@ for @sendProgramChange:bankMSB:bankLSB:onChannel:@
sendProgramChange_bankMSB_bankLSB_onChannelSelector :: Selector
sendProgramChange_bankMSB_bankLSB_onChannelSelector = mkSelector "sendProgramChange:bankMSB:bankLSB:onChannel:"

-- | @Selector@ for @sendMIDIEvent:data1:data2:@
sendMIDIEvent_data1_data2Selector :: Selector
sendMIDIEvent_data1_data2Selector = mkSelector "sendMIDIEvent:data1:data2:"

-- | @Selector@ for @sendMIDIEvent:data1:@
sendMIDIEvent_data1Selector :: Selector
sendMIDIEvent_data1Selector = mkSelector "sendMIDIEvent:data1:"

-- | @Selector@ for @sendMIDISysExEvent:@
sendMIDISysExEventSelector :: Selector
sendMIDISysExEventSelector = mkSelector "sendMIDISysExEvent:"

