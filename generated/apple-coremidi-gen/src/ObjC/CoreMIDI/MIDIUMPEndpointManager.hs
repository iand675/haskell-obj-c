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
  , sharedInstance
  , umpEndpoints
  , sharedInstanceSelector
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

-- | sharedInstance
--
-- Retrieve the shared UMP Endpoint manager for the client process.
--
-- After first access to this property, the client process may begin observing notifications				which are posted when the system-wide cache changes.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id MIDIUMPEndpointManager)
sharedInstance  =
  do
    cls' <- getRequiredClass "MIDIUMPEndpointManager"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @UMPEndpoints@
umpEndpointsSelector :: Selector
umpEndpointsSelector = mkSelector "UMPEndpoints"

