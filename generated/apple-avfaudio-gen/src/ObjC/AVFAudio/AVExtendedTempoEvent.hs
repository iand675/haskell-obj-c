{-# LANGUAGE DataKinds #-}
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
  , setTempoSelector
  , tempoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithTempo avExtendedTempoEvent tempo =
  sendOwnedMessage avExtendedTempoEvent initWithTempoSelector tempo

-- | tempo
--
-- The new tempo in beats-per-minute.  Range:  Any positive value.
--
-- ObjC selector: @- tempo@
tempo :: IsAVExtendedTempoEvent avExtendedTempoEvent => avExtendedTempoEvent -> IO CDouble
tempo avExtendedTempoEvent =
  sendMessage avExtendedTempoEvent tempoSelector

-- | tempo
--
-- The new tempo in beats-per-minute.  Range:  Any positive value.
--
-- ObjC selector: @- setTempo:@
setTempo :: IsAVExtendedTempoEvent avExtendedTempoEvent => avExtendedTempoEvent -> CDouble -> IO ()
setTempo avExtendedTempoEvent value =
  sendMessage avExtendedTempoEvent setTempoSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTempo:@
initWithTempoSelector :: Selector '[CDouble] (Id AVExtendedTempoEvent)
initWithTempoSelector = mkSelector "initWithTempo:"

-- | @Selector@ for @tempo@
tempoSelector :: Selector '[] CDouble
tempoSelector = mkSelector "tempo"

-- | @Selector@ for @setTempo:@
setTempoSelector :: Selector '[CDouble] ()
setTempoSelector = mkSelector "setTempo:"

