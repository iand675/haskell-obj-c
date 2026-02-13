{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEPacket
--
-- An NEPacket object represents the data, protocol family, and metadata associated with an IP packet. 	These packets are used to read and write on an NEPacketTunnelFlow.
--
-- NEPacket is part of NetworkExtension.framework
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEPacket@.
module ObjC.NetworkExtension.NEPacket
  ( NEPacket
  , IsNEPacket(..)
  , initWithData_protocolFamily
  , data_
  , protocolFamily
  , direction
  , metadata
  , dataSelector
  , directionSelector
  , initWithData_protocolFamilySelector
  , metadataSelector
  , protocolFamilySelector

  -- * Enum types
  , NETrafficDirection(NETrafficDirection)
  , pattern NETrafficDirectionAny
  , pattern NETrafficDirectionInbound
  , pattern NETrafficDirectionOutbound

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithData:protocolFamily:
--
-- Initializes a new NEPacket object with data and protocol family.
--
-- @data@ — The content of the packet.
--
-- @protocolFamily@ — The protocol family of the packet (such as AF_INET or AF_INET6).
--
-- ObjC selector: @- initWithData:protocolFamily:@
initWithData_protocolFamily :: (IsNEPacket nePacket, IsNSData data_) => nePacket -> data_ -> CUChar -> IO (Id NEPacket)
initWithData_protocolFamily nePacket data_ protocolFamily =
  sendOwnedMessage nePacket initWithData_protocolFamilySelector (toNSData data_) protocolFamily

-- | data
--
-- The data content of the packet.
--
-- ObjC selector: @- data@
data_ :: IsNEPacket nePacket => nePacket -> IO (Id NSData)
data_ nePacket =
  sendMessage nePacket dataSelector

-- | protocolFamily
--
-- The protocol family of the packet (such as AF_INET or AF_INET6).
--
-- ObjC selector: @- protocolFamily@
protocolFamily :: IsNEPacket nePacket => nePacket -> IO CUChar
protocolFamily nePacket =
  sendMessage nePacket protocolFamilySelector

-- | direction
--
-- The direction of the packet.
--
-- ObjC selector: @- direction@
direction :: IsNEPacket nePacket => nePacket -> IO NETrafficDirection
direction nePacket =
  sendMessage nePacket directionSelector

-- | metadata
--
-- Metadata about the source application and flow for this packet.	This property will only be non-nil when the routing method for the NEPacketTunnelProvider	is NETunnelProviderRoutingMethodSourceApplication.
--
-- ObjC selector: @- metadata@
metadata :: IsNEPacket nePacket => nePacket -> IO (Id NEFlowMetaData)
metadata nePacket =
  sendMessage nePacket metadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:protocolFamily:@
initWithData_protocolFamilySelector :: Selector '[Id NSData, CUChar] (Id NEPacket)
initWithData_protocolFamilySelector = mkSelector "initWithData:protocolFamily:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @protocolFamily@
protocolFamilySelector :: Selector '[] CUChar
protocolFamilySelector = mkSelector "protocolFamily"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] NETrafficDirection
directionSelector = mkSelector "direction"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NEFlowMetaData)
metadataSelector = mkSelector "metadata"

