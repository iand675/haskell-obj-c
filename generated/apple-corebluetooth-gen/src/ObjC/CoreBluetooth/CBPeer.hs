{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CBPeer@.
module ObjC.CoreBluetooth.CBPeer
  ( CBPeer
  , IsCBPeer(..)
  , init_
  , identifier
  , initSelector
  , identifierSelector


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

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCBPeer cbPeer => cbPeer -> IO (Id CBPeer)
init_ cbPeer  =
    sendMsg cbPeer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- The unique, persistent identifier associated with the peer.
--
-- ObjC selector: @- identifier@
identifier :: IsCBPeer cbPeer => cbPeer -> IO RawId
identifier cbPeer  =
    fmap (RawId . castPtr) $ sendMsg cbPeer (mkSelector "identifier") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

