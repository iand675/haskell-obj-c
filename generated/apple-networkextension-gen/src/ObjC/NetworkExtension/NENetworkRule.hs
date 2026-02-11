{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NENetworkRule
--
-- The NENetworkRule class declares the programmatic interface of an object that contains a specification of a rule that matches the attributes of network traffic.
--
-- Generated bindings for @NENetworkRule@.
module ObjC.NetworkExtension.NENetworkRule
  ( NENetworkRule
  , IsNENetworkRule(..)
  , initWithDestinationNetworkEndpoint_prefix_protocol
  , initWithDestinationNetwork_prefix_protocol
  , initWithDestinationHostEndpoint_protocol
  , initWithDestinationHost_protocol
  , initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_direction
  , initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_direction
  , matchRemoteHostOrNetworkEndpoint
  , matchRemoteEndpoint
  , matchRemotePrefix
  , matchLocalNetworkEndpoint
  , matchLocalNetwork
  , matchLocalPrefix
  , matchProtocol
  , matchDirection
  , initWithDestinationNetworkEndpoint_prefix_protocolSelector
  , initWithDestinationNetwork_prefix_protocolSelector
  , initWithDestinationHostEndpoint_protocolSelector
  , initWithDestinationHost_protocolSelector
  , initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector
  , initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector
  , matchRemoteHostOrNetworkEndpointSelector
  , matchRemoteEndpointSelector
  , matchRemotePrefixSelector
  , matchLocalNetworkEndpointSelector
  , matchLocalNetworkSelector
  , matchLocalPrefixSelector
  , matchProtocolSelector
  , matchDirectionSelector

  -- * Enum types
  , NENetworkRuleProtocol(NENetworkRuleProtocol)
  , pattern NENetworkRuleProtocolAny
  , pattern NENetworkRuleProtocolTCP
  , pattern NENetworkRuleProtocolUDP
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

-- | initWithDestinationNetworkEndpoint:prefix:protocol:
--
-- Initialize a newly-allocated NENetworkRule object that matches network traffic destined for a host within a specific network.
--
-- @networkEndpoint@ — An endpoint object that contains the port and address or network that the rule matches. This endpoint must contain an address, not a hostname.        If the address is a wildcard address (0.0.0.0 or ::) then the rule will match all destinations except for loopback (127.0.0.1 or ::1). To match loopback traffic set the address to the loopback address.        If the port string of the endpoint is "0" or is the empty string, then the rule will match traffic on any port destined for the given address or network.
--
-- @destinationPrefix@ — An integer that in combination with the address in the endpoint specifies the destination network that the rule matches.
--
-- @protocol@ — A NENetworkRuleProtocol value indicating the protocol that the rule matches.
--
-- Returns: The initialized NENetworkRule instance.
--
-- ObjC selector: @- initWithDestinationNetworkEndpoint:prefix:protocol:@
initWithDestinationNetworkEndpoint_prefix_protocol :: (IsNENetworkRule neNetworkRule, IsNSObject networkEndpoint) => neNetworkRule -> networkEndpoint -> CULong -> NENetworkRuleProtocol -> IO (Id NENetworkRule)
initWithDestinationNetworkEndpoint_prefix_protocol neNetworkRule  networkEndpoint destinationPrefix protocol =
  withObjCPtr networkEndpoint $ \raw_networkEndpoint ->
      sendMsg neNetworkRule (mkSelector "initWithDestinationNetworkEndpoint:prefix:protocol:") (retPtr retVoid) [argPtr (castPtr raw_networkEndpoint :: Ptr ()), argCULong destinationPrefix, argCLong (coerce protocol)] >>= ownedObject . castPtr

-- | initWithDestinationNetwork:prefix:protocol:
--
-- Initialize a newly-allocated NENetworkRule object that matches network traffic destined for a host within a specific network.
--
-- @networkEndpoint@ — An endpoint object that contains the port and address or network that the rule matches. This endpoint must contain an address, not a hostname.        If the address is a wildcard address (0.0.0.0 or ::) then the rule will match all destinations except for loopback (127.0.0.1 or ::1). To match loopback traffic set the address to the loopback address.        If the port string of the endpoint is "0" or is the empty string, then the rule will match traffic on any port destined for the given address or network.
--
-- @destinationPrefix@ — An integer that in combination with the address in the endpoint specifies the destination network that the rule matches.
--
-- @protocol@ — A NENetworkRuleProtocol value indicating the protocol that the rule matches.
--
-- Returns: The initialized NENetworkRule instance.
--
-- ObjC selector: @- initWithDestinationNetwork:prefix:protocol:@
initWithDestinationNetwork_prefix_protocol :: (IsNENetworkRule neNetworkRule, IsNWHostEndpoint networkEndpoint) => neNetworkRule -> networkEndpoint -> CULong -> NENetworkRuleProtocol -> IO (Id NENetworkRule)
initWithDestinationNetwork_prefix_protocol neNetworkRule  networkEndpoint destinationPrefix protocol =
  withObjCPtr networkEndpoint $ \raw_networkEndpoint ->
      sendMsg neNetworkRule (mkSelector "initWithDestinationNetwork:prefix:protocol:") (retPtr retVoid) [argPtr (castPtr raw_networkEndpoint :: Ptr ()), argCULong destinationPrefix, argCLong (coerce protocol)] >>= ownedObject . castPtr

-- | initWithDestinationHostEndpoint:protocol:
--
-- Initialize a newly-allocated NENetworkRule object that matches network traffic destined for a host within a specific DNS domain.
--
-- @hostEndpoint@ — An endpoint object that contains the port and hostname or domain that the rule matches. This endpoint must contain a hostname, not an address.    If the port string of the @nw_endpoint_t@ is "0" or is the empty string, then the rule will match traffic on any port destined for the given hostname or domain.    If the hostname string of the endpoint consists of a single label, then the rule will match traffic destined to the specific host with that single label as its name.    If the hostname string of the endpoint consists of 2 or more labels, then the rule will match traffic destined to hosts within the domain specified by the hostname string.    Examples:        [[NENetworkRule alloc] initWithDestinationHost:nw_endpoint_create_host("com", "0") protocol:NENetworkRuleProtocolAny] - matches all TCP and UDP traffic to the host named "com".        [[NENetworkRule alloc] initWithDestinationHost:nw_endpoint_create_host("example.com", "0") protocol:NENetworkRuleProtocolAny] - matches all TCP and UDP traffic to hosts in the "example.com" DNS domain, including all DNS queries for names in the example.com DNS domain.        [[NENetworkRule alloc] initWithDestinationHost:nw_endpoint_create_host("example.com", "53") protocol:NENetworkRuleProtocolAny] - matches all DNS queries/responses for hosts in the "example.com" domain.        [[NENetworkRule alloc] initWithDestinationHost:nw_endpoint_create_host("example.com", "443") protocol:NENetworkRuleProtocolTCP] - matches all TCP port 443 traffic to hosts in the "example.com" domain.
--
-- @protocol@ — A NENetworkRuleProtocol value indicating the protocol that the rule matches.
--
-- Returns: The initialized NENetworkRule instance.
--
-- ObjC selector: @- initWithDestinationHostEndpoint:protocol:@
initWithDestinationHostEndpoint_protocol :: (IsNENetworkRule neNetworkRule, IsNSObject hostEndpoint) => neNetworkRule -> hostEndpoint -> NENetworkRuleProtocol -> IO (Id NENetworkRule)
initWithDestinationHostEndpoint_protocol neNetworkRule  hostEndpoint protocol =
  withObjCPtr hostEndpoint $ \raw_hostEndpoint ->
      sendMsg neNetworkRule (mkSelector "initWithDestinationHostEndpoint:protocol:") (retPtr retVoid) [argPtr (castPtr raw_hostEndpoint :: Ptr ()), argCLong (coerce protocol)] >>= ownedObject . castPtr

-- | initWithDestinationHost:protocol:
--
-- Initialize a newly-allocated NENetworkRule object that matches network traffic destined for a host within a specific DNS domain.
--
-- @hostEndpoint@ — An endpoint object that contains the port and hostname or domain that the rule matches. This endpoint must contain a hostname, not an address.    If the port string of the NWHostEndpoint is "0" or is the empty string, then the rule will match traffic on any port destined for the given hostname or domain.    If the hostname string of the endpoint consists of a single label, then the rule will match traffic destined to the specific host with that single label as its name.    If the hostname string of the endpoint consists of 2 or more labels, then the rule will match traffic destined to hosts within the domain specified by the hostname string.    Examples:        [[NENetworkRule alloc] initWithDestinationHost:[NWHostEndpoint endpointWithHostname:"com" port:\@"0"] protocol:NENetworkRuleProtocolAny] - matches all TCP and UDP traffic to the host named "com".        [[NENetworkRule alloc] initWithDestinationHost:[NWHostEndpoint endpointWithHostname:"example.com" port:\@"0"] protocol:NENetworkRuleProtocolAny] - matches all TCP and UDP traffic to hosts in the "example.com" DNS domain, including all DNS queries for names in the example.com DNS domain.        [[NENetworkRule alloc] initWithDestinationHost:[NWHostEndpoint endpointWithHostname:"example.com" port:\@"53"] protocol:NENetworkRuleProtocolAny] - matches all DNS queries/responses for hosts in the "example.com" domain.        [[NENetworkRule alloc] initWithDestinationHost:[NWHostEndpoint endpointWithHostname:"example.com" port:\@"443"] protocol:NENetworkRuleProtocolTCP] - matches all TCP port 443 traffic to hosts in the "example.com" domain.
--
-- @protocol@ — A NENetworkRuleProtocol value indicating the protocol that the rule matches.
--
-- Returns: The initialized NENetworkRule instance.
--
-- ObjC selector: @- initWithDestinationHost:protocol:@
initWithDestinationHost_protocol :: (IsNENetworkRule neNetworkRule, IsNWHostEndpoint hostEndpoint) => neNetworkRule -> hostEndpoint -> NENetworkRuleProtocol -> IO (Id NENetworkRule)
initWithDestinationHost_protocol neNetworkRule  hostEndpoint protocol =
  withObjCPtr hostEndpoint $ \raw_hostEndpoint ->
      sendMsg neNetworkRule (mkSelector "initWithDestinationHost:protocol:") (retPtr retVoid) [argPtr (castPtr raw_hostEndpoint :: Ptr ()), argCLong (coerce protocol)] >>= ownedObject . castPtr

-- | initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:
--
-- Initialize a newly-allocated NENetworkRule object that matches traffic by remote network, local network, protocol, and direction. If both remoteNetwork and localNetwork are nil    then the rule will match all traffic of the given protocol and direction, except for loopback traffic. To match loopback traffic create a NENetworkRule with remoteNetwork and/or localNetwork properties that    explicitly match traffic to the loopback address (127.0.0.1 or ::1).
--
-- @remoteNetwork@ — An endpoint object that contains the remote port and the remote address or network that the rule matches. This endpoint must contain an address, not a hostname.    If the address is a wildcard address (0.0.0.0 or ::) then the rule will match all destinations except for loopback (127.0.0.1 or ::1). To match loopback traffic set the address to the loopback address.    If the port string of the endpoint is "0" or is the empty string, then the rule will match traffic on any port coming from the remote network.    Pass nil to cause the rule to match any remote network.
--
-- @remotePrefix@ — An integer that in combination with the address in remoteNetwork specifies the remote network that the rule matches.
--
-- @localNetwork@ — An endpoint object that contains the local port and the local address or network that the rule matches. This endpoint must contain an address, not a hostname.    If the address is a wildcard address (0.0.0.0 or ::) then the rule will match all local networks except for loopback (127.0.0.1 or ::1). To match loopback traffic set the address to the loopback address.    If the port string of the endpoint is "0" or is the empty string, then the rule will match traffic on any port coming from the local network.    Pass nil to cause the rule to match any local network.
--
-- @localPrefix@ — An integer that in combination with the address in localNetwork specifies the local network that the rule matches. This parameter    is ignored if localNetwork is nil.
--
-- @protocol@ — A NENetworkRuleProtocol value indicating the protocol that the rule matches.
--
-- @direction@ — A NETrafficDirection value indicating the direction of network traffic that the rule matches.
--
-- Returns: The initialized NENetworkRule instance.
--
-- ObjC selector: @- initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:@
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_direction :: (IsNENetworkRule neNetworkRule, IsNSObject remoteNetwork, IsNSObject localNetwork) => neNetworkRule -> remoteNetwork -> CULong -> localNetwork -> CULong -> NENetworkRuleProtocol -> NETrafficDirection -> IO (Id NENetworkRule)
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_direction neNetworkRule  remoteNetwork remotePrefix localNetwork localPrefix protocol direction =
  withObjCPtr remoteNetwork $ \raw_remoteNetwork ->
    withObjCPtr localNetwork $ \raw_localNetwork ->
        sendMsg neNetworkRule (mkSelector "initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:") (retPtr retVoid) [argPtr (castPtr raw_remoteNetwork :: Ptr ()), argCULong remotePrefix, argPtr (castPtr raw_localNetwork :: Ptr ()), argCULong localPrefix, argCLong (coerce protocol), argCLong (coerce direction)] >>= ownedObject . castPtr

-- | initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:
--
-- Initialize a newly-allocated NENetworkRule object that matches traffic by remote network, local network, protocol, and direction. If both remoteNetwork and localNetwork are nil    then the rule will match all traffic of the given protocol and direction, except for loopback traffic. To match loopback traffic create a NENetworkRule with remoteNetwork and/or localNetwork properties that    explicitly match traffic to the loopback address (127.0.0.1 or ::1).
--
-- @remoteNetwork@ — An endpoint object that contains the remote port and the remote address or network that the rule matches. This endpoint must contain an address, not a hostname.    If the address is a wildcard address (0.0.0.0 or ::) then the rule will match all destinations except for loopback (127.0.0.1 or ::1). To match loopback traffic set the address to the loopback address.    If the port string of the endpoint is "0" or is the empty string, then the rule will match traffic on any port coming from the remote network.    Pass nil to cause the rule to match any remote network.
--
-- @remotePrefix@ — An integer that in combination with the address in remoteNetwork specifies the remote network that the rule matches.
--
-- @localNetwork@ — An endpoint object that contains the local port and the local address or network that the rule matches. This endpoint must contain an address, not a hostname.    If the address is a wildcard address (0.0.0.0 or ::) then the rule will match all local networks except for loopback (127.0.0.1 or ::1). To match loopback traffic set the address to the loopback address.    If the port string of the endpoint is "0" or is the empty string, then the rule will match traffic on any port coming from the local network.    Pass nil to cause the rule to match any local network.
--
-- @localPrefix@ — An integer that in combination with the address in localNetwork specifies the local network that the rule matches. This parameter    is ignored if localNetwork is nil.
--
-- @protocol@ — A NENetworkRuleProtocol value indicating the protocol that the rule matches.
--
-- @direction@ — A NETrafficDirection value indicating the direction of network traffic that the rule matches.
--
-- Returns: The initialized NENetworkRule instance.
--
-- ObjC selector: @- initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:@
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_direction :: (IsNENetworkRule neNetworkRule, IsNWHostEndpoint remoteNetwork, IsNWHostEndpoint localNetwork) => neNetworkRule -> remoteNetwork -> CULong -> localNetwork -> CULong -> NENetworkRuleProtocol -> NETrafficDirection -> IO (Id NENetworkRule)
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_direction neNetworkRule  remoteNetwork remotePrefix localNetwork localPrefix protocol direction =
  withObjCPtr remoteNetwork $ \raw_remoteNetwork ->
    withObjCPtr localNetwork $ \raw_localNetwork ->
        sendMsg neNetworkRule (mkSelector "initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:") (retPtr retVoid) [argPtr (castPtr raw_remoteNetwork :: Ptr ()), argCULong remotePrefix, argPtr (castPtr raw_localNetwork :: Ptr ()), argCULong localPrefix, argCLong (coerce protocol), argCLong (coerce direction)] >>= ownedObject . castPtr

-- | matchRemoteHostOrNetworkEndpoint
--
-- The remote endpoint that the rule matches.
--
-- ObjC selector: @- matchRemoteHostOrNetworkEndpoint@
matchRemoteHostOrNetworkEndpoint :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NSObject)
matchRemoteHostOrNetworkEndpoint neNetworkRule  =
    sendMsg neNetworkRule (mkSelector "matchRemoteHostOrNetworkEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchRemoteEndpoint
--
-- The remote endpoint that the rule matches.
--
-- ObjC selector: @- matchRemoteEndpoint@
matchRemoteEndpoint :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NWHostEndpoint)
matchRemoteEndpoint neNetworkRule  =
    sendMsg neNetworkRule (mkSelector "matchRemoteEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchRemotePrefix
--
-- A number that specifies the remote sub-network that the rule matches. This property is set to NSNotFound for rules where matchRemoteEndpoint does not contain an IP address.
--
-- ObjC selector: @- matchRemotePrefix@
matchRemotePrefix :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO CULong
matchRemotePrefix neNetworkRule  =
    sendMsg neNetworkRule (mkSelector "matchRemotePrefix") retCULong []

-- | matchLocalNetworkEndpoint
--
-- The local network that the rule matches.
--
-- ObjC selector: @- matchLocalNetworkEndpoint@
matchLocalNetworkEndpoint :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NSObject)
matchLocalNetworkEndpoint neNetworkRule  =
    sendMsg neNetworkRule (mkSelector "matchLocalNetworkEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchLocalNetwork
--
-- The local network that the rule matches.
--
-- ObjC selector: @- matchLocalNetwork@
matchLocalNetwork :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NWHostEndpoint)
matchLocalNetwork neNetworkRule  =
    sendMsg neNetworkRule (mkSelector "matchLocalNetwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchLocalPrefix
--
-- A number that specifies the local sub-network that the rule matches. This property is set to NSNotFound for rules with a nil matchLocalNetwork property.
--
-- ObjC selector: @- matchLocalPrefix@
matchLocalPrefix :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO CULong
matchLocalPrefix neNetworkRule  =
    sendMsg neNetworkRule (mkSelector "matchLocalPrefix") retCULong []

-- | matchProtocol
--
-- A NENetworkRuleProtocol value containing the protocol that the rule matches.
--
-- ObjC selector: @- matchProtocol@
matchProtocol :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO NENetworkRuleProtocol
matchProtocol neNetworkRule  =
    fmap (coerce :: CLong -> NENetworkRuleProtocol) $ sendMsg neNetworkRule (mkSelector "matchProtocol") retCLong []

-- | matchDirection
--
-- A NETrafficDirection value indicating the network traffic direction that the rule matches.
--
-- ObjC selector: @- matchDirection@
matchDirection :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO NETrafficDirection
matchDirection neNetworkRule  =
    fmap (coerce :: CLong -> NETrafficDirection) $ sendMsg neNetworkRule (mkSelector "matchDirection") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationNetworkEndpoint:prefix:protocol:@
initWithDestinationNetworkEndpoint_prefix_protocolSelector :: Selector
initWithDestinationNetworkEndpoint_prefix_protocolSelector = mkSelector "initWithDestinationNetworkEndpoint:prefix:protocol:"

-- | @Selector@ for @initWithDestinationNetwork:prefix:protocol:@
initWithDestinationNetwork_prefix_protocolSelector :: Selector
initWithDestinationNetwork_prefix_protocolSelector = mkSelector "initWithDestinationNetwork:prefix:protocol:"

-- | @Selector@ for @initWithDestinationHostEndpoint:protocol:@
initWithDestinationHostEndpoint_protocolSelector :: Selector
initWithDestinationHostEndpoint_protocolSelector = mkSelector "initWithDestinationHostEndpoint:protocol:"

-- | @Selector@ for @initWithDestinationHost:protocol:@
initWithDestinationHost_protocolSelector :: Selector
initWithDestinationHost_protocolSelector = mkSelector "initWithDestinationHost:protocol:"

-- | @Selector@ for @initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:@
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector :: Selector
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector = mkSelector "initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:"

-- | @Selector@ for @initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:@
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector :: Selector
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector = mkSelector "initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:"

-- | @Selector@ for @matchRemoteHostOrNetworkEndpoint@
matchRemoteHostOrNetworkEndpointSelector :: Selector
matchRemoteHostOrNetworkEndpointSelector = mkSelector "matchRemoteHostOrNetworkEndpoint"

-- | @Selector@ for @matchRemoteEndpoint@
matchRemoteEndpointSelector :: Selector
matchRemoteEndpointSelector = mkSelector "matchRemoteEndpoint"

-- | @Selector@ for @matchRemotePrefix@
matchRemotePrefixSelector :: Selector
matchRemotePrefixSelector = mkSelector "matchRemotePrefix"

-- | @Selector@ for @matchLocalNetworkEndpoint@
matchLocalNetworkEndpointSelector :: Selector
matchLocalNetworkEndpointSelector = mkSelector "matchLocalNetworkEndpoint"

-- | @Selector@ for @matchLocalNetwork@
matchLocalNetworkSelector :: Selector
matchLocalNetworkSelector = mkSelector "matchLocalNetwork"

-- | @Selector@ for @matchLocalPrefix@
matchLocalPrefixSelector :: Selector
matchLocalPrefixSelector = mkSelector "matchLocalPrefix"

-- | @Selector@ for @matchProtocol@
matchProtocolSelector :: Selector
matchProtocolSelector = mkSelector "matchProtocol"

-- | @Selector@ for @matchDirection@
matchDirectionSelector :: Selector
matchDirectionSelector = mkSelector "matchDirection"

