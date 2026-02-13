{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initWithDestinationHostEndpoint_protocolSelector
  , initWithDestinationHost_protocolSelector
  , initWithDestinationNetworkEndpoint_prefix_protocolSelector
  , initWithDestinationNetwork_prefix_protocolSelector
  , initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector
  , initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector
  , matchDirectionSelector
  , matchLocalNetworkEndpointSelector
  , matchLocalNetworkSelector
  , matchLocalPrefixSelector
  , matchProtocolSelector
  , matchRemoteEndpointSelector
  , matchRemoteHostOrNetworkEndpointSelector
  , matchRemotePrefixSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDestinationNetworkEndpoint_prefix_protocol neNetworkRule networkEndpoint destinationPrefix protocol =
  sendOwnedMessage neNetworkRule initWithDestinationNetworkEndpoint_prefix_protocolSelector (toNSObject networkEndpoint) destinationPrefix protocol

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
initWithDestinationNetwork_prefix_protocol neNetworkRule networkEndpoint destinationPrefix protocol =
  sendOwnedMessage neNetworkRule initWithDestinationNetwork_prefix_protocolSelector (toNWHostEndpoint networkEndpoint) destinationPrefix protocol

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
initWithDestinationHostEndpoint_protocol neNetworkRule hostEndpoint protocol =
  sendOwnedMessage neNetworkRule initWithDestinationHostEndpoint_protocolSelector (toNSObject hostEndpoint) protocol

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
initWithDestinationHost_protocol neNetworkRule hostEndpoint protocol =
  sendOwnedMessage neNetworkRule initWithDestinationHost_protocolSelector (toNWHostEndpoint hostEndpoint) protocol

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
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_direction neNetworkRule remoteNetwork remotePrefix localNetwork localPrefix protocol direction =
  sendOwnedMessage neNetworkRule initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector (toNSObject remoteNetwork) remotePrefix (toNSObject localNetwork) localPrefix protocol direction

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
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_direction neNetworkRule remoteNetwork remotePrefix localNetwork localPrefix protocol direction =
  sendOwnedMessage neNetworkRule initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector (toNWHostEndpoint remoteNetwork) remotePrefix (toNWHostEndpoint localNetwork) localPrefix protocol direction

-- | matchRemoteHostOrNetworkEndpoint
--
-- The remote endpoint that the rule matches.
--
-- ObjC selector: @- matchRemoteHostOrNetworkEndpoint@
matchRemoteHostOrNetworkEndpoint :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NSObject)
matchRemoteHostOrNetworkEndpoint neNetworkRule =
  sendMessage neNetworkRule matchRemoteHostOrNetworkEndpointSelector

-- | matchRemoteEndpoint
--
-- The remote endpoint that the rule matches.
--
-- ObjC selector: @- matchRemoteEndpoint@
matchRemoteEndpoint :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NWHostEndpoint)
matchRemoteEndpoint neNetworkRule =
  sendMessage neNetworkRule matchRemoteEndpointSelector

-- | matchRemotePrefix
--
-- A number that specifies the remote sub-network that the rule matches. This property is set to NSNotFound for rules where matchRemoteEndpoint does not contain an IP address.
--
-- ObjC selector: @- matchRemotePrefix@
matchRemotePrefix :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO CULong
matchRemotePrefix neNetworkRule =
  sendMessage neNetworkRule matchRemotePrefixSelector

-- | matchLocalNetworkEndpoint
--
-- The local network that the rule matches.
--
-- ObjC selector: @- matchLocalNetworkEndpoint@
matchLocalNetworkEndpoint :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NSObject)
matchLocalNetworkEndpoint neNetworkRule =
  sendMessage neNetworkRule matchLocalNetworkEndpointSelector

-- | matchLocalNetwork
--
-- The local network that the rule matches.
--
-- ObjC selector: @- matchLocalNetwork@
matchLocalNetwork :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO (Id NWHostEndpoint)
matchLocalNetwork neNetworkRule =
  sendMessage neNetworkRule matchLocalNetworkSelector

-- | matchLocalPrefix
--
-- A number that specifies the local sub-network that the rule matches. This property is set to NSNotFound for rules with a nil matchLocalNetwork property.
--
-- ObjC selector: @- matchLocalPrefix@
matchLocalPrefix :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO CULong
matchLocalPrefix neNetworkRule =
  sendMessage neNetworkRule matchLocalPrefixSelector

-- | matchProtocol
--
-- A NENetworkRuleProtocol value containing the protocol that the rule matches.
--
-- ObjC selector: @- matchProtocol@
matchProtocol :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO NENetworkRuleProtocol
matchProtocol neNetworkRule =
  sendMessage neNetworkRule matchProtocolSelector

-- | matchDirection
--
-- A NETrafficDirection value indicating the network traffic direction that the rule matches.
--
-- ObjC selector: @- matchDirection@
matchDirection :: IsNENetworkRule neNetworkRule => neNetworkRule -> IO NETrafficDirection
matchDirection neNetworkRule =
  sendMessage neNetworkRule matchDirectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationNetworkEndpoint:prefix:protocol:@
initWithDestinationNetworkEndpoint_prefix_protocolSelector :: Selector '[Id NSObject, CULong, NENetworkRuleProtocol] (Id NENetworkRule)
initWithDestinationNetworkEndpoint_prefix_protocolSelector = mkSelector "initWithDestinationNetworkEndpoint:prefix:protocol:"

-- | @Selector@ for @initWithDestinationNetwork:prefix:protocol:@
initWithDestinationNetwork_prefix_protocolSelector :: Selector '[Id NWHostEndpoint, CULong, NENetworkRuleProtocol] (Id NENetworkRule)
initWithDestinationNetwork_prefix_protocolSelector = mkSelector "initWithDestinationNetwork:prefix:protocol:"

-- | @Selector@ for @initWithDestinationHostEndpoint:protocol:@
initWithDestinationHostEndpoint_protocolSelector :: Selector '[Id NSObject, NENetworkRuleProtocol] (Id NENetworkRule)
initWithDestinationHostEndpoint_protocolSelector = mkSelector "initWithDestinationHostEndpoint:protocol:"

-- | @Selector@ for @initWithDestinationHost:protocol:@
initWithDestinationHost_protocolSelector :: Selector '[Id NWHostEndpoint, NENetworkRuleProtocol] (Id NENetworkRule)
initWithDestinationHost_protocolSelector = mkSelector "initWithDestinationHost:protocol:"

-- | @Selector@ for @initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:@
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector :: Selector '[Id NSObject, CULong, Id NSObject, CULong, NENetworkRuleProtocol, NETrafficDirection] (Id NENetworkRule)
initWithRemoteNetworkEndpoint_remotePrefix_localNetworkEndpoint_localPrefix_protocol_directionSelector = mkSelector "initWithRemoteNetworkEndpoint:remotePrefix:localNetworkEndpoint:localPrefix:protocol:direction:"

-- | @Selector@ for @initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:@
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector :: Selector '[Id NWHostEndpoint, CULong, Id NWHostEndpoint, CULong, NENetworkRuleProtocol, NETrafficDirection] (Id NENetworkRule)
initWithRemoteNetwork_remotePrefix_localNetwork_localPrefix_protocol_directionSelector = mkSelector "initWithRemoteNetwork:remotePrefix:localNetwork:localPrefix:protocol:direction:"

-- | @Selector@ for @matchRemoteHostOrNetworkEndpoint@
matchRemoteHostOrNetworkEndpointSelector :: Selector '[] (Id NSObject)
matchRemoteHostOrNetworkEndpointSelector = mkSelector "matchRemoteHostOrNetworkEndpoint"

-- | @Selector@ for @matchRemoteEndpoint@
matchRemoteEndpointSelector :: Selector '[] (Id NWHostEndpoint)
matchRemoteEndpointSelector = mkSelector "matchRemoteEndpoint"

-- | @Selector@ for @matchRemotePrefix@
matchRemotePrefixSelector :: Selector '[] CULong
matchRemotePrefixSelector = mkSelector "matchRemotePrefix"

-- | @Selector@ for @matchLocalNetworkEndpoint@
matchLocalNetworkEndpointSelector :: Selector '[] (Id NSObject)
matchLocalNetworkEndpointSelector = mkSelector "matchLocalNetworkEndpoint"

-- | @Selector@ for @matchLocalNetwork@
matchLocalNetworkSelector :: Selector '[] (Id NWHostEndpoint)
matchLocalNetworkSelector = mkSelector "matchLocalNetwork"

-- | @Selector@ for @matchLocalPrefix@
matchLocalPrefixSelector :: Selector '[] CULong
matchLocalPrefixSelector = mkSelector "matchLocalPrefix"

-- | @Selector@ for @matchProtocol@
matchProtocolSelector :: Selector '[] NENetworkRuleProtocol
matchProtocolSelector = mkSelector "matchProtocol"

-- | @Selector@ for @matchDirection@
matchDirectionSelector :: Selector '[] NETrafficDirection
matchDirectionSelector = mkSelector "matchDirection"

