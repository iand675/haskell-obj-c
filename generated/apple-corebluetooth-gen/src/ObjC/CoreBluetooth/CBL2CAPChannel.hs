{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBL2CAPChannel
--
-- A CBL2CAPChannel represents a live L2CAP connection to a remote device
--
-- Generated bindings for @CBL2CAPChannel@.
module ObjC.CoreBluetooth.CBL2CAPChannel
  ( CBL2CAPChannel
  , IsCBL2CAPChannel(..)
  , peer
  , inputStream
  , outputStream
  , psm
  , inputStreamSelector
  , outputStreamSelector
  , peerSelector
  , psmSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | peer
--
-- The peer connected to the channel
--
-- ObjC selector: @- peer@
peer :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO (Id CBPeer)
peer cbL2CAPChannel =
  sendMessage cbL2CAPChannel peerSelector

-- | inputStream
--
-- An NSStream used for reading data from the remote peer
--
-- ObjC selector: @- inputStream@
inputStream :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO (Id NSInputStream)
inputStream cbL2CAPChannel =
  sendMessage cbL2CAPChannel inputStreamSelector

-- | outputStream
--
-- An NSStream used for writing data to the peer
--
-- ObjC selector: @- outputStream@
outputStream :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO (Id NSOutputStream)
outputStream cbL2CAPChannel =
  sendMessage cbL2CAPChannel outputStreamSelector

-- | PSM
--
-- The PSM (Protocol/Service Multiplexer) of the channel
--
-- ObjC selector: @- PSM@
psm :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO CUShort
psm cbL2CAPChannel =
  sendMessage cbL2CAPChannel psmSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peer@
peerSelector :: Selector '[] (Id CBPeer)
peerSelector = mkSelector "peer"

-- | @Selector@ for @inputStream@
inputStreamSelector :: Selector '[] (Id NSInputStream)
inputStreamSelector = mkSelector "inputStream"

-- | @Selector@ for @outputStream@
outputStreamSelector :: Selector '[] (Id NSOutputStream)
outputStreamSelector = mkSelector "outputStream"

-- | @Selector@ for @PSM@
psmSelector :: Selector '[] CUShort
psmSelector = mkSelector "PSM"

