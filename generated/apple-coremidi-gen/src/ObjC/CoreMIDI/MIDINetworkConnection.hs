{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDINetworkConnection@.
module ObjC.CoreMIDI.MIDINetworkConnection
  ( MIDINetworkConnection
  , IsMIDINetworkConnection(..)
  , connectionWithHost
  , host
  , connectionWithHostSelector
  , hostSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ connectionWithHost:@
connectionWithHost :: IsMIDINetworkHost host => host -> IO (Id MIDINetworkConnection)
connectionWithHost host =
  do
    cls' <- getRequiredClass "MIDINetworkConnection"
    sendClassMessage cls' connectionWithHostSelector (toMIDINetworkHost host)

-- | @- host@
host :: IsMIDINetworkConnection midiNetworkConnection => midiNetworkConnection -> IO (Id MIDINetworkHost)
host midiNetworkConnection =
  sendMessage midiNetworkConnection hostSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionWithHost:@
connectionWithHostSelector :: Selector '[Id MIDINetworkHost] (Id MIDINetworkConnection)
connectionWithHostSelector = mkSelector "connectionWithHost:"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id MIDINetworkHost)
hostSelector = mkSelector "host"

