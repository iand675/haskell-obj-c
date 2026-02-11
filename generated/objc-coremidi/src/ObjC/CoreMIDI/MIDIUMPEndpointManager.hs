{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDIUMPEndpointManager
--
-- A singleton object that performs system-wide UMP Endpoint bookkeeping.
--
-- MIDIUMPEndpointManager is used to retrieve information about UMP Endpoint				pairs detected by or explicitly declared to the MIDI UMP subsystem.
--
-- Generated bindings for @MIDIUMPEndpointManager@.
module ObjC.CoreMIDI.MIDIUMPEndpointManager
  ( MIDIUMPEndpointManager
  , IsMIDIUMPEndpointManager(..)
  , umpEndpoints
  , umpEndpointsSelector


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

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | UMPEndpoints
--
-- A  list of UMP endpoints discovered using UMP endpoint discovery.
--
-- ObjC selector: @- UMPEndpoints@
umpEndpoints :: IsMIDIUMPEndpointManager midiumpEndpointManager => midiumpEndpointManager -> IO (Id NSArray)
umpEndpoints midiumpEndpointManager  =
  sendMsg midiumpEndpointManager (mkSelector "UMPEndpoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @UMPEndpoints@
umpEndpointsSelector :: Selector
umpEndpointsSelector = mkSelector "UMPEndpoints"

