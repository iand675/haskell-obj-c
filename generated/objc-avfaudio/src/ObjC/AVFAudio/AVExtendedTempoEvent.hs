{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVExtendedTempoEvent
--
-- The event class representing a tempo change to a specific beats-per-minute value.
--
-- This event provides a way to specify a tempo change that is less cumbersome than using		tempo meta-events.
--
-- Generated bindings for @AVExtendedTempoEvent@.
module ObjC.AVFAudio.AVExtendedTempoEvent
  ( AVExtendedTempoEvent
  , IsAVExtendedTempoEvent(..)
  , initWithTempo
  , tempo
  , setTempo
  , initWithTempoSelector
  , tempoSelector
  , setTempoSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithTempo:
--
-- Initialize the event tempo.
--
-- @tempo@ — The new tempo in beats-per-minute.  Range:  Any positive value.		The new tempo will begin at the timestamp for this event.
--
-- ObjC selector: @- initWithTempo:@
initWithTempo :: IsAVExtendedTempoEvent avExtendedTempoEvent => avExtendedTempoEvent -> CDouble -> IO (Id AVExtendedTempoEvent)
initWithTempo avExtendedTempoEvent  tempo =
  sendMsg avExtendedTempoEvent (mkSelector "initWithTempo:") (retPtr retVoid) [argCDouble (fromIntegral tempo)] >>= ownedObject . castPtr

-- | tempo
--
-- The new tempo in beats-per-minute.  Range:  Any positive value.
--
-- ObjC selector: @- tempo@
tempo :: IsAVExtendedTempoEvent avExtendedTempoEvent => avExtendedTempoEvent -> IO CDouble
tempo avExtendedTempoEvent  =
  sendMsg avExtendedTempoEvent (mkSelector "tempo") retCDouble []

-- | tempo
--
-- The new tempo in beats-per-minute.  Range:  Any positive value.
--
-- ObjC selector: @- setTempo:@
setTempo :: IsAVExtendedTempoEvent avExtendedTempoEvent => avExtendedTempoEvent -> CDouble -> IO ()
setTempo avExtendedTempoEvent  value =
  sendMsg avExtendedTempoEvent (mkSelector "setTempo:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTempo:@
initWithTempoSelector :: Selector
initWithTempoSelector = mkSelector "initWithTempo:"

-- | @Selector@ for @tempo@
tempoSelector :: Selector
tempoSelector = mkSelector "tempo"

-- | @Selector@ for @setTempo:@
setTempoSelector :: Selector
setTempoSelector = mkSelector "setTempo:"

