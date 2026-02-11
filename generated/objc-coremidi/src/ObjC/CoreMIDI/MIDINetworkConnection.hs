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

-- | @+ connectionWithHost:@
connectionWithHost :: IsMIDINetworkHost host => host -> IO (Id MIDINetworkConnection)
connectionWithHost host =
  do
    cls' <- getRequiredClass "MIDINetworkConnection"
    withObjCPtr host $ \raw_host ->
      sendClassMsg cls' (mkSelector "connectionWithHost:") (retPtr retVoid) [argPtr (castPtr raw_host :: Ptr ())] >>= retainedObject . castPtr

-- | @- host@
host :: IsMIDINetworkConnection midiNetworkConnection => midiNetworkConnection -> IO (Id MIDINetworkHost)
host midiNetworkConnection  =
  sendMsg midiNetworkConnection (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionWithHost:@
connectionWithHostSelector :: Selector
connectionWithHostSelector = mkSelector "connectionWithHost:"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

