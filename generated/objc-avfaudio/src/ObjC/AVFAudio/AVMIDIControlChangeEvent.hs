{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIControlChangeEvent
--
-- The event class representing MIDI control change messages.
--
-- Generated bindings for @AVMIDIControlChangeEvent@.
module ObjC.AVFAudio.AVMIDIControlChangeEvent
  ( AVMIDIControlChangeEvent
  , IsAVMIDIControlChangeEvent(..)
  , initWithChannel_messageType_value
  , messageType
  , value
  , initWithChannel_messageType_valueSelector
  , messageTypeSelector
  , valueSelector

  -- * Enum types
  , AVMIDIControlChangeMessageType(AVMIDIControlChangeMessageType)
  , pattern AVMIDIControlChangeMessageTypeBankSelect
  , pattern AVMIDIControlChangeMessageTypeModWheel
  , pattern AVMIDIControlChangeMessageTypeBreath
  , pattern AVMIDIControlChangeMessageTypeFoot
  , pattern AVMIDIControlChangeMessageTypePortamentoTime
  , pattern AVMIDIControlChangeMessageTypeDataEntry
  , pattern AVMIDIControlChangeMessageTypeVolume
  , pattern AVMIDIControlChangeMessageTypeBalance
  , pattern AVMIDIControlChangeMessageTypePan
  , pattern AVMIDIControlChangeMessageTypeExpression
  , pattern AVMIDIControlChangeMessageTypeSustain
  , pattern AVMIDIControlChangeMessageTypePortamento
  , pattern AVMIDIControlChangeMessageTypeSostenuto
  , pattern AVMIDIControlChangeMessageTypeSoft
  , pattern AVMIDIControlChangeMessageTypeLegatoPedal
  , pattern AVMIDIControlChangeMessageTypeHold2Pedal
  , pattern AVMIDIControlChangeMessageTypeFilterResonance
  , pattern AVMIDIControlChangeMessageTypeReleaseTime
  , pattern AVMIDIControlChangeMessageTypeAttackTime
  , pattern AVMIDIControlChangeMessageTypeBrightness
  , pattern AVMIDIControlChangeMessageTypeDecayTime
  , pattern AVMIDIControlChangeMessageTypeVibratoRate
  , pattern AVMIDIControlChangeMessageTypeVibratoDepth
  , pattern AVMIDIControlChangeMessageTypeVibratoDelay
  , pattern AVMIDIControlChangeMessageTypeReverbLevel
  , pattern AVMIDIControlChangeMessageTypeChorusLevel
  , pattern AVMIDIControlChangeMessageTypeRPN_LSB
  , pattern AVMIDIControlChangeMessageTypeRPN_MSB
  , pattern AVMIDIControlChangeMessageTypeAllSoundOff
  , pattern AVMIDIControlChangeMessageTypeResetAllControllers
  , pattern AVMIDIControlChangeMessageTypeAllNotesOff
  , pattern AVMIDIControlChangeMessageTypeOmniModeOff
  , pattern AVMIDIControlChangeMessageTypeOmniModeOn
  , pattern AVMIDIControlChangeMessageTypeMonoModeOn
  , pattern AVMIDIControlChangeMessageTypeMonoModeOff

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
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithChannel:messageType:value
--
-- Initialize the event with a channel, a control change type, and a control value.
--
-- @channel@ — The MIDI channel for the control change.  Range: 0-15.
--
-- @messageType@ — The AVMIDIControlChangeMessageType indicating which MIDI control change message to send.
--
-- @value@ — The value for this control change.  Range: Depends on the type (see the General MIDI specification).
--
-- ObjC selector: @- initWithChannel:messageType:value:@
initWithChannel_messageType_value :: IsAVMIDIControlChangeEvent avmidiControlChangeEvent => avmidiControlChangeEvent -> CUInt -> AVMIDIControlChangeMessageType -> CUInt -> IO (Id AVMIDIControlChangeEvent)
initWithChannel_messageType_value avmidiControlChangeEvent  channel messageType value =
  sendMsg avmidiControlChangeEvent (mkSelector "initWithChannel:messageType:value:") (retPtr retVoid) [argCUInt (fromIntegral channel), argCLong (coerce messageType), argCUInt (fromIntegral value)] >>= ownedObject . castPtr

-- | messageType
--
-- The type of control change message, specified as an AVMIDIControlChangeMessageType.
--
-- ObjC selector: @- messageType@
messageType :: IsAVMIDIControlChangeEvent avmidiControlChangeEvent => avmidiControlChangeEvent -> IO AVMIDIControlChangeMessageType
messageType avmidiControlChangeEvent  =
  fmap (coerce :: CLong -> AVMIDIControlChangeMessageType) $ sendMsg avmidiControlChangeEvent (mkSelector "messageType") retCLong []

-- | value
--
-- The value of the control change event.  The range of this value depends on the type (see the General MIDI specification).
--
-- ObjC selector: @- value@
value :: IsAVMIDIControlChangeEvent avmidiControlChangeEvent => avmidiControlChangeEvent -> IO CUInt
value avmidiControlChangeEvent  =
  sendMsg avmidiControlChangeEvent (mkSelector "value") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:messageType:value:@
initWithChannel_messageType_valueSelector :: Selector
initWithChannel_messageType_valueSelector = mkSelector "initWithChannel:messageType:value:"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

