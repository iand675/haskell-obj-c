{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEPacketTunnelFlow
--
-- The NEPacketTunnelFlow class declares the programmatic interface of an object that is used by NEPacketTunnelProvider implementations to tunnel IP packets.
--
-- NEPacketTunnelFlow is part of NetworkExtension.framework
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEPacketTunnelFlow@.
module ObjC.NetworkExtension.NEPacketTunnelFlow
  ( NEPacketTunnelFlow
  , IsNEPacketTunnelFlow(..)
  , writePackets_withProtocols
  , writePacketObjects
  , writePackets_withProtocolsSelector
  , writePacketObjectsSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | writePackets:completionHandler:
--
-- Write multiple IP packets to the flow.
--
-- @packets@ — An array of NSData objects, each containing packet data to be written.
--
-- @protocols@ — An array of NSNumber objects. Each number contains the protocol of the packet in the corresponding index in the packets array.
--
-- ObjC selector: @- writePackets:withProtocols:@
writePackets_withProtocols :: (IsNEPacketTunnelFlow nePacketTunnelFlow, IsNSArray packets, IsNSArray protocols) => nePacketTunnelFlow -> packets -> protocols -> IO Bool
writePackets_withProtocols nePacketTunnelFlow  packets protocols =
withObjCPtr packets $ \raw_packets ->
  withObjCPtr protocols $ \raw_protocols ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nePacketTunnelFlow (mkSelector "writePackets:withProtocols:") retCULong [argPtr (castPtr raw_packets :: Ptr ()), argPtr (castPtr raw_protocols :: Ptr ())]

-- | writePacketObjects:
--
-- Write multiple IP packets to the flow.
--
-- @packets@ — An array of NEPacket objects, each containing packet data and protocol family to be written.
--
-- ObjC selector: @- writePacketObjects:@
writePacketObjects :: (IsNEPacketTunnelFlow nePacketTunnelFlow, IsNSArray packets) => nePacketTunnelFlow -> packets -> IO Bool
writePacketObjects nePacketTunnelFlow  packets =
withObjCPtr packets $ \raw_packets ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nePacketTunnelFlow (mkSelector "writePacketObjects:") retCULong [argPtr (castPtr raw_packets :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @writePackets:withProtocols:@
writePackets_withProtocolsSelector :: Selector
writePackets_withProtocolsSelector = mkSelector "writePackets:withProtocols:"

-- | @Selector@ for @writePacketObjects:@
writePacketObjectsSelector :: Selector
writePacketObjectsSelector = mkSelector "writePacketObjects:"

