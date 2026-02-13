{-# LANGUAGE DataKinds #-}
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
  , writePacketObjectsSelector
  , writePackets_withProtocolsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
writePackets_withProtocols nePacketTunnelFlow packets protocols =
  sendMessage nePacketTunnelFlow writePackets_withProtocolsSelector (toNSArray packets) (toNSArray protocols)

-- | writePacketObjects:
--
-- Write multiple IP packets to the flow.
--
-- @packets@ — An array of NEPacket objects, each containing packet data and protocol family to be written.
--
-- ObjC selector: @- writePacketObjects:@
writePacketObjects :: (IsNEPacketTunnelFlow nePacketTunnelFlow, IsNSArray packets) => nePacketTunnelFlow -> packets -> IO Bool
writePacketObjects nePacketTunnelFlow packets =
  sendMessage nePacketTunnelFlow writePacketObjectsSelector (toNSArray packets)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @writePackets:withProtocols:@
writePackets_withProtocolsSelector :: Selector '[Id NSArray, Id NSArray] Bool
writePackets_withProtocolsSelector = mkSelector "writePackets:withProtocols:"

-- | @Selector@ for @writePacketObjects:@
writePacketObjectsSelector :: Selector '[Id NSArray] Bool
writePacketObjectsSelector = mkSelector "writePacketObjects:"

