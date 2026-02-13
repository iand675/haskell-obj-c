{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedInstanceSelector

-- | UMPEndpoints
--
-- A  list of UMP endpoints discovered using UMP endpoint discovery.
--
-- ObjC selector: @- UMPEndpoints@
umpEndpoints :: IsMIDIUMPEndpointManager midiumpEndpointManager => midiumpEndpointManager -> IO (Id NSArray)
umpEndpoints midiumpEndpointManager =
  sendMessage midiumpEndpointManager umpEndpointsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id MIDIUMPEndpointManager)
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @UMPEndpoints@
umpEndpointsSelector :: Selector '[] (Id NSArray)
umpEndpointsSelector = mkSelector "UMPEndpoints"

