{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CBPeer@.
module ObjC.CoreBluetooth.CBPeer
  ( CBPeer
  , IsCBPeer(..)
  , init_
  , identifier
  , identifierSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCBPeer cbPeer => cbPeer -> IO (Id CBPeer)
init_ cbPeer =
  sendOwnedMessage cbPeer initSelector

-- | identifier
--
-- The unique, persistent identifier associated with the peer.
--
-- ObjC selector: @- identifier@
identifier :: IsCBPeer cbPeer => cbPeer -> IO RawId
identifier cbPeer =
  sendMessage cbPeer identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CBPeer)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] RawId
identifierSelector = mkSelector "identifier"

