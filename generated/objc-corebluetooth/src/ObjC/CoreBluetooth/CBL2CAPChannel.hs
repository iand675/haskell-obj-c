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
  , peerSelector
  , inputStreamSelector
  , outputStreamSelector
  , psmSelector


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

-- | peer
--
-- The peer connected to the channel
--
-- ObjC selector: @- peer@
peer :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO (Id CBPeer)
peer cbL2CAPChannel  =
  sendMsg cbL2CAPChannel (mkSelector "peer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | inputStream
--
-- An NSStream used for reading data from the remote peer
--
-- ObjC selector: @- inputStream@
inputStream :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO (Id NSInputStream)
inputStream cbL2CAPChannel  =
  sendMsg cbL2CAPChannel (mkSelector "inputStream") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputStream
--
-- An NSStream used for writing data to the peer
--
-- ObjC selector: @- outputStream@
outputStream :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO (Id NSOutputStream)
outputStream cbL2CAPChannel  =
  sendMsg cbL2CAPChannel (mkSelector "outputStream") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | PSM
--
-- The PSM (Protocol/Service Multiplexer) of the channel
--
-- ObjC selector: @- PSM@
psm :: IsCBL2CAPChannel cbL2CAPChannel => cbL2CAPChannel -> IO CUShort
psm cbL2CAPChannel  =
  fmap fromIntegral $ sendMsg cbL2CAPChannel (mkSelector "PSM") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peer@
peerSelector :: Selector
peerSelector = mkSelector "peer"

-- | @Selector@ for @inputStream@
inputStreamSelector :: Selector
inputStreamSelector = mkSelector "inputStream"

-- | @Selector@ for @outputStream@
outputStreamSelector :: Selector
outputStreamSelector = mkSelector "outputStream"

-- | @Selector@ for @PSM@
psmSelector :: Selector
psmSelector = mkSelector "PSM"

