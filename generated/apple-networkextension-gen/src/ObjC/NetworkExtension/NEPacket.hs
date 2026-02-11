{-# LANGUAGE PatternSynonyms #-}
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
  , initWithData_protocolFamilySelector
  , dataSelector
  , protocolFamilySelector
  , directionSelector
  , metadataSelector

  -- * Enum types
  , NETrafficDirection(NETrafficDirection)
  , pattern NETrafficDirectionAny
  , pattern NETrafficDirectionInbound
  , pattern NETrafficDirectionOutbound

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
initWithData_protocolFamily nePacket  data_ protocolFamily =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nePacket (mkSelector "initWithData:protocolFamily:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCUChar protocolFamily] >>= ownedObject . castPtr

-- | data
--
-- The data content of the packet.
--
-- ObjC selector: @- data@
data_ :: IsNEPacket nePacket => nePacket -> IO (Id NSData)
data_ nePacket  =
    sendMsg nePacket (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | protocolFamily
--
-- The protocol family of the packet (such as AF_INET or AF_INET6).
--
-- ObjC selector: @- protocolFamily@
protocolFamily :: IsNEPacket nePacket => nePacket -> IO CUChar
protocolFamily nePacket  =
    sendMsg nePacket (mkSelector "protocolFamily") retCUChar []

-- | direction
--
-- The direction of the packet.
--
-- ObjC selector: @- direction@
direction :: IsNEPacket nePacket => nePacket -> IO NETrafficDirection
direction nePacket  =
    fmap (coerce :: CLong -> NETrafficDirection) $ sendMsg nePacket (mkSelector "direction") retCLong []

-- | metadata
--
-- Metadata about the source application and flow for this packet.	This property will only be non-nil when the routing method for the NEPacketTunnelProvider	is NETunnelProviderRoutingMethodSourceApplication.
--
-- ObjC selector: @- metadata@
metadata :: IsNEPacket nePacket => nePacket -> IO (Id NEFlowMetaData)
metadata nePacket  =
    sendMsg nePacket (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:protocolFamily:@
initWithData_protocolFamilySelector :: Selector
initWithData_protocolFamilySelector = mkSelector "initWithData:protocolFamily:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @protocolFamily@
protocolFamilySelector :: Selector
protocolFamilySelector = mkSelector "protocolFamily"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

