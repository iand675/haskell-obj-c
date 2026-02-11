{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.NetworkExtension.Internal.Classes (
    module ObjC.NetworkExtension.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- NEFilterPacketSet ----------

-- | NEFilterPacketProvider.h
--
-- This file declares the NEFilterPacketProvider API. The NEFilterPacketProvider API is used to implement custom network packet filters.
--
-- This API is part of NetworkExtension.framework.
-- 
-- Phantom type for @NEFilterPacketSet@.
data NEFilterPacketSet

instance IsObjCObject (Id NEFilterPacketSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterPacketSet"

class IsObjCObject a => IsNEFilterPacketSet a where
  toNEFilterPacketSet :: a -> Id NEFilterPacketSet

instance IsNEFilterPacketSet (Id NEFilterPacketSet) where
  toNEFilterPacketSet = unsafeCastId

-- ---------- NEAppProxyFlow ----------

-- | NEAppProxyFlow
--
-- The NEAppProxyFlow class is an abstract base class that declares the programmatic interface for a flow of network data.
--
-- NEAppProxyFlow is part of NetworkExtension.framework.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEAppProxyFlow@.
data NEAppProxyFlow

instance IsObjCObject (Id NEAppProxyFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppProxyFlow"

class IsNSObject a => IsNEAppProxyFlow a where
  toNEAppProxyFlow :: a -> Id NEAppProxyFlow

instance IsNEAppProxyFlow (Id NEAppProxyFlow) where
  toNEAppProxyFlow = unsafeCastId

instance IsNSObject (Id NEAppProxyFlow) where
  toNSObject = unsafeCastId

-- ---------- NEAppPushManager ----------

-- | NEAppPushManager
--
-- The NEAppPushManager class declares a programmatic interface to configure NEAppPushProvider.
--
-- NEAppPushManager declares methods and properties for configuring and managing life cycle of app push provider.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEAppPushManager@.
data NEAppPushManager

instance IsObjCObject (Id NEAppPushManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppPushManager"

class IsNSObject a => IsNEAppPushManager a where
  toNEAppPushManager :: a -> Id NEAppPushManager

instance IsNEAppPushManager (Id NEAppPushManager) where
  toNEAppPushManager = unsafeCastId

instance IsNSObject (Id NEAppPushManager) where
  toNSObject = unsafeCastId

-- ---------- NEAppRule ----------

-- | NEAppRule
--
-- The NEAppRule class declares the programmatic interface for an object that contains the match conditions for a rule that is used to match network traffic originated by applications.
--
-- NEAppRule is used in the context of a Network Extension configuration to specify what traffic should be made available to the Network Extension.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEAppRule@.
data NEAppRule

instance IsObjCObject (Id NEAppRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppRule"

class IsNSObject a => IsNEAppRule a where
  toNEAppRule :: a -> Id NEAppRule

instance IsNEAppRule (Id NEAppRule) where
  toNEAppRule = unsafeCastId

instance IsNSObject (Id NEAppRule) where
  toNSObject = unsafeCastId

-- ---------- NEDNSProxyManager ----------

-- | NEDNSProxyManager
--
-- The NEDNSProxyManager class declares the programmatic interface for an object that manages DNS proxy configurations.
--
-- NEDNSProxyManager declares methods and properties for configuring and controlling a DNS proxy.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEDNSProxyManager@.
data NEDNSProxyManager

instance IsObjCObject (Id NEDNSProxyManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSProxyManager"

class IsNSObject a => IsNEDNSProxyManager a where
  toNEDNSProxyManager :: a -> Id NEDNSProxyManager

instance IsNEDNSProxyManager (Id NEDNSProxyManager) where
  toNEDNSProxyManager = unsafeCastId

instance IsNSObject (Id NEDNSProxyManager) where
  toNSObject = unsafeCastId

-- ---------- NEDNSSettings ----------

-- | NEDNSSettings
--
-- The NEDNSSettings class declares the programmatic interface for an object that contains DNS settings.
-- 
-- Phantom type for @NEDNSSettings@.
data NEDNSSettings

instance IsObjCObject (Id NEDNSSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSSettings"

class IsNSObject a => IsNEDNSSettings a where
  toNEDNSSettings :: a -> Id NEDNSSettings

instance IsNEDNSSettings (Id NEDNSSettings) where
  toNEDNSSettings = unsafeCastId

instance IsNSObject (Id NEDNSSettings) where
  toNSObject = unsafeCastId

-- ---------- NEDNSSettingsManager ----------

-- | NEDNSSettingsManager
--
-- The NEDNSSettingsManager class declares the programmatic interface for an object that manages DNS settings configurations.
--
-- NEDNSSettingsManager declares methods and properties for configuring and controlling DNS settings on the system.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEDNSSettingsManager@.
data NEDNSSettingsManager

instance IsObjCObject (Id NEDNSSettingsManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSSettingsManager"

class IsNSObject a => IsNEDNSSettingsManager a where
  toNEDNSSettingsManager :: a -> Id NEDNSSettingsManager

instance IsNEDNSSettingsManager (Id NEDNSSettingsManager) where
  toNEDNSSettingsManager = unsafeCastId

instance IsNSObject (Id NEDNSSettingsManager) where
  toNSObject = unsafeCastId

-- ---------- NEEvaluateConnectionRule ----------

-- | NEEvaluateConnectionRule
--
-- The NEEvaluateConnectionRule class declares the programmatic interface for an object that associates properties of network connections with an action.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEEvaluateConnectionRule@.
data NEEvaluateConnectionRule

instance IsObjCObject (Id NEEvaluateConnectionRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEEvaluateConnectionRule"

class IsNSObject a => IsNEEvaluateConnectionRule a where
  toNEEvaluateConnectionRule :: a -> Id NEEvaluateConnectionRule

instance IsNEEvaluateConnectionRule (Id NEEvaluateConnectionRule) where
  toNEEvaluateConnectionRule = unsafeCastId

instance IsNSObject (Id NEEvaluateConnectionRule) where
  toNSObject = unsafeCastId

-- ---------- NEFilterFlow ----------

-- | NEFilterFlow
--
-- The NEFilterFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered.
--
-- NEFilterFlow is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterFlow@.
data NEFilterFlow

instance IsObjCObject (Id NEFilterFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterFlow"

class IsNSObject a => IsNEFilterFlow a where
  toNEFilterFlow :: a -> Id NEFilterFlow

instance IsNEFilterFlow (Id NEFilterFlow) where
  toNEFilterFlow = unsafeCastId

instance IsNSObject (Id NEFilterFlow) where
  toNSObject = unsafeCastId

-- ---------- NEFilterManager ----------

-- | NEFilterManager
--
-- The NEFilterManager class declares the programmatic interface for an object that manages content filtering configurations.
--
-- NEFilterManager declares methods and properties for configuring and controlling a filter.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEFilterManager@.
data NEFilterManager

instance IsObjCObject (Id NEFilterManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterManager"

class IsNSObject a => IsNEFilterManager a where
  toNEFilterManager :: a -> Id NEFilterManager

instance IsNEFilterManager (Id NEFilterManager) where
  toNEFilterManager = unsafeCastId

instance IsNSObject (Id NEFilterManager) where
  toNSObject = unsafeCastId

-- ---------- NEFilterPacketContext ----------

-- | NEFilterPacketContext
--
-- The NEFilterPacketContext class identifies the current filtering context.
-- 
-- Phantom type for @NEFilterPacketContext@.
data NEFilterPacketContext

instance IsObjCObject (Id NEFilterPacketContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterPacketContext"

class IsNSObject a => IsNEFilterPacketContext a where
  toNEFilterPacketContext :: a -> Id NEFilterPacketContext

instance IsNEFilterPacketContext (Id NEFilterPacketContext) where
  toNEFilterPacketContext = unsafeCastId

instance IsNSObject (Id NEFilterPacketContext) where
  toNSObject = unsafeCastId

-- ---------- NEFilterProviderConfiguration ----------

-- | NEFilterProviderConfiguration
--
-- The NEFilterProviderConfiguration class declares the programmatic interface of an object that configures a plugin-based content filter.
-- 
-- Phantom type for @NEFilterProviderConfiguration@.
data NEFilterProviderConfiguration

instance IsObjCObject (Id NEFilterProviderConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterProviderConfiguration"

class IsNSObject a => IsNEFilterProviderConfiguration a where
  toNEFilterProviderConfiguration :: a -> Id NEFilterProviderConfiguration

instance IsNEFilterProviderConfiguration (Id NEFilterProviderConfiguration) where
  toNEFilterProviderConfiguration = unsafeCastId

instance IsNSObject (Id NEFilterProviderConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NEFilterReport ----------

-- | NEFilterReport
--
-- The NEFilterReport declares the programmatic interface of an object that is a report of actions taken by the data provider.
--
-- NEFilterReport is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterReport@.
data NEFilterReport

instance IsObjCObject (Id NEFilterReport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterReport"

class IsNSObject a => IsNEFilterReport a where
  toNEFilterReport :: a -> Id NEFilterReport

instance IsNEFilterReport (Id NEFilterReport) where
  toNEFilterReport = unsafeCastId

instance IsNSObject (Id NEFilterReport) where
  toNSObject = unsafeCastId

-- ---------- NEFilterRule ----------

-- | NEFilterRule
--
-- The NEFilterRule class declares the programmatic interface of an object that defines a rule for matching network traffic and the action to take when the rule matches.
-- 
-- Phantom type for @NEFilterRule@.
data NEFilterRule

instance IsObjCObject (Id NEFilterRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterRule"

class IsNSObject a => IsNEFilterRule a where
  toNEFilterRule :: a -> Id NEFilterRule

instance IsNEFilterRule (Id NEFilterRule) where
  toNEFilterRule = unsafeCastId

instance IsNSObject (Id NEFilterRule) where
  toNSObject = unsafeCastId

-- ---------- NEFilterSettings ----------

-- | NEFilterSettings
--
-- The NEFilterSettings class declares the programmatic interface for an object that contains filter settings.
--
-- NEFilterSettings is used by NEFilterDataProviders to communicate the desired settings for the filter to the framework. The framework takes care of applying the contained settings to the system.
-- 
-- Phantom type for @NEFilterSettings@.
data NEFilterSettings

instance IsObjCObject (Id NEFilterSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterSettings"

class IsNSObject a => IsNEFilterSettings a where
  toNEFilterSettings :: a -> Id NEFilterSettings

instance IsNEFilterSettings (Id NEFilterSettings) where
  toNEFilterSettings = unsafeCastId

instance IsNSObject (Id NEFilterSettings) where
  toNSObject = unsafeCastId

-- ---------- NEFilterVerdict ----------

-- | NEFilterVerdict
--
-- The NEFilterVerdict class declares the programmatic interface for an object that is the verdict for a flow of network data.
--
-- NEFilterVerdict is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterVerdict@.
data NEFilterVerdict

instance IsObjCObject (Id NEFilterVerdict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterVerdict"

class IsNSObject a => IsNEFilterVerdict a where
  toNEFilterVerdict :: a -> Id NEFilterVerdict

instance IsNEFilterVerdict (Id NEFilterVerdict) where
  toNEFilterVerdict = unsafeCastId

instance IsNSObject (Id NEFilterVerdict) where
  toNSObject = unsafeCastId

-- ---------- NEFlowMetaData ----------

-- | NEFlowMetaData
--
-- The NEFlowMetaData class declares the programmatic interface for an object that contains extra information about a flow.
-- 
-- Phantom type for @NEFlowMetaData@.
data NEFlowMetaData

instance IsObjCObject (Id NEFlowMetaData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFlowMetaData"

class IsNSObject a => IsNEFlowMetaData a where
  toNEFlowMetaData :: a -> Id NEFlowMetaData

instance IsNEFlowMetaData (Id NEFlowMetaData) where
  toNEFlowMetaData = unsafeCastId

instance IsNSObject (Id NEFlowMetaData) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotConfiguration ----------

-- | NEHotspotConfiguration
--
-- The NEHotspotConfiguration class represents set of properties that are required   to configure a Wi-Fi Network.
-- 
-- Phantom type for @NEHotspotConfiguration@.
data NEHotspotConfiguration

instance IsObjCObject (Id NEHotspotConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotConfiguration"

class IsNSObject a => IsNEHotspotConfiguration a where
  toNEHotspotConfiguration :: a -> Id NEHotspotConfiguration

instance IsNEHotspotConfiguration (Id NEHotspotConfiguration) where
  toNEHotspotConfiguration = unsafeCastId

instance IsNSObject (Id NEHotspotConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotConfigurationManager ----------

-- | NEHotspotConfigurationManager
--
-- The NEHotspotConfigurationManager class allows an application to   Add/Update/Remove Wi-Fi Network Configuraton.
-- 
-- Phantom type for @NEHotspotConfigurationManager@.
data NEHotspotConfigurationManager

instance IsObjCObject (Id NEHotspotConfigurationManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotConfigurationManager"

class IsNSObject a => IsNEHotspotConfigurationManager a where
  toNEHotspotConfigurationManager :: a -> Id NEHotspotConfigurationManager

instance IsNEHotspotConfigurationManager (Id NEHotspotConfigurationManager) where
  toNEHotspotConfigurationManager = unsafeCastId

instance IsNSObject (Id NEHotspotConfigurationManager) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotEAPSettings ----------

-- | NEHotspotEAPSettings
--
-- NEHotspotEAPSettings class provides a set of properties that are required   to configure a WPA/WPA2 Enterprise or Hotspot 2.0 Wi-Fi networks.
-- 
-- Phantom type for @NEHotspotEAPSettings@.
data NEHotspotEAPSettings

instance IsObjCObject (Id NEHotspotEAPSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotEAPSettings"

class IsNSObject a => IsNEHotspotEAPSettings a where
  toNEHotspotEAPSettings :: a -> Id NEHotspotEAPSettings

instance IsNEHotspotEAPSettings (Id NEHotspotEAPSettings) where
  toNEHotspotEAPSettings = unsafeCastId

instance IsNSObject (Id NEHotspotEAPSettings) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotHS20Settings ----------

-- | NEHotspotHS20Settings
--
-- NEHotspotHS20Settings class provides a set of properties that are required   to discover and negotiate Hotspot 2.0 Wi-Fi networks.
-- 
-- Phantom type for @NEHotspotHS20Settings@.
data NEHotspotHS20Settings

instance IsObjCObject (Id NEHotspotHS20Settings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotHS20Settings"

class IsNSObject a => IsNEHotspotHS20Settings a where
  toNEHotspotHS20Settings :: a -> Id NEHotspotHS20Settings

instance IsNEHotspotHS20Settings (Id NEHotspotHS20Settings) where
  toNEHotspotHS20Settings = unsafeCastId

instance IsNSObject (Id NEHotspotHS20Settings) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotHelper ----------

-- | NEHotspotHelper
--
-- The NEHotspotHelper class allows an application to register itself as a   HotspotHelper.
-- 
-- Phantom type for @NEHotspotHelper@.
data NEHotspotHelper

instance IsObjCObject (Id NEHotspotHelper) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotHelper"

class IsNSObject a => IsNEHotspotHelper a where
  toNEHotspotHelper :: a -> Id NEHotspotHelper

instance IsNEHotspotHelper (Id NEHotspotHelper) where
  toNEHotspotHelper = unsafeCastId

instance IsNSObject (Id NEHotspotHelper) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotHelperCommand ----------

-- | NEHotspotHelperCommand
--
-- An NEHotspotHelperCommand object is provided to the helper's   command handler block. The HotspotHelper processes the command   instantiates an NEHotspotHelperResponse object, sets the annotated   network or networkList (Evaluate/FilterScanList only),   then delivers it.
-- 
-- Phantom type for @NEHotspotHelperCommand@.
data NEHotspotHelperCommand

instance IsObjCObject (Id NEHotspotHelperCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotHelperCommand"

class IsNSObject a => IsNEHotspotHelperCommand a where
  toNEHotspotHelperCommand :: a -> Id NEHotspotHelperCommand

instance IsNEHotspotHelperCommand (Id NEHotspotHelperCommand) where
  toNEHotspotHelperCommand = unsafeCastId

instance IsNSObject (Id NEHotspotHelperCommand) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotHelperResponse ----------

-- | NEHotspotHelperResponse
--
-- The HotspotHelper creates an NEHotspotHelperResponse object to provide   the results of running the corresponding NEHotspotHelperCommand.
-- 
-- Phantom type for @NEHotspotHelperResponse@.
data NEHotspotHelperResponse

instance IsObjCObject (Id NEHotspotHelperResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotHelperResponse"

class IsNSObject a => IsNEHotspotHelperResponse a where
  toNEHotspotHelperResponse :: a -> Id NEHotspotHelperResponse

instance IsNEHotspotHelperResponse (Id NEHotspotHelperResponse) where
  toNEHotspotHelperResponse = unsafeCastId

instance IsNSObject (Id NEHotspotHelperResponse) where
  toNSObject = unsafeCastId

-- ---------- NEHotspotNetwork ----------

-- | NEHotspotNetwork
--
-- The NEHotspotNetwork class provides a class method to get the SSID and BSSID of   the current Wi-Fi network.
--
-- NEHotspotNetwork is part of NetworkExtension.framework
-- 
-- Phantom type for @NEHotspotNetwork@.
data NEHotspotNetwork

instance IsObjCObject (Id NEHotspotNetwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEHotspotNetwork"

class IsNSObject a => IsNEHotspotNetwork a where
  toNEHotspotNetwork :: a -> Id NEHotspotNetwork

instance IsNEHotspotNetwork (Id NEHotspotNetwork) where
  toNEHotspotNetwork = unsafeCastId

instance IsNSObject (Id NEHotspotNetwork) where
  toNSObject = unsafeCastId

-- ---------- NEIPv4Route ----------

-- | NEIPv4Route
--
-- The NEIPv4Route class declares the programmatic interface for an object that contains settings for an IPv4 route.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEIPv4Route@.
data NEIPv4Route

instance IsObjCObject (Id NEIPv4Route) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEIPv4Route"

class IsNSObject a => IsNEIPv4Route a where
  toNEIPv4Route :: a -> Id NEIPv4Route

instance IsNEIPv4Route (Id NEIPv4Route) where
  toNEIPv4Route = unsafeCastId

instance IsNSObject (Id NEIPv4Route) where
  toNSObject = unsafeCastId

-- ---------- NEIPv4Settings ----------

-- | NEIPv4Settings
--
-- The NEIPv4Settings class declares the programmatic interface for an object that contains IPv4 settings.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEIPv4Settings@.
data NEIPv4Settings

instance IsObjCObject (Id NEIPv4Settings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEIPv4Settings"

class IsNSObject a => IsNEIPv4Settings a where
  toNEIPv4Settings :: a -> Id NEIPv4Settings

instance IsNEIPv4Settings (Id NEIPv4Settings) where
  toNEIPv4Settings = unsafeCastId

instance IsNSObject (Id NEIPv4Settings) where
  toNSObject = unsafeCastId

-- ---------- NEIPv6Route ----------

-- | NEIPv6Route
--
-- The NEIPv6Route class declares the programmatic interface for an object that contains settings for an IPv6 route.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEIPv6Route@.
data NEIPv6Route

instance IsObjCObject (Id NEIPv6Route) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEIPv6Route"

class IsNSObject a => IsNEIPv6Route a where
  toNEIPv6Route :: a -> Id NEIPv6Route

instance IsNEIPv6Route (Id NEIPv6Route) where
  toNEIPv6Route = unsafeCastId

instance IsNSObject (Id NEIPv6Route) where
  toNSObject = unsafeCastId

-- ---------- NEIPv6Settings ----------

-- | NEIPv6Settings
--
-- The NEIPv6Settings class declares the programmatic interface for an object that contains IPv6 settings.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEIPv6Settings@.
data NEIPv6Settings

instance IsObjCObject (Id NEIPv6Settings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEIPv6Settings"

class IsNSObject a => IsNEIPv6Settings a where
  toNEIPv6Settings :: a -> Id NEIPv6Settings

instance IsNEIPv6Settings (Id NEIPv6Settings) where
  toNEIPv6Settings = unsafeCastId

instance IsNSObject (Id NEIPv6Settings) where
  toNSObject = unsafeCastId

-- ---------- NENetworkRule ----------

-- | NENetworkRule
--
-- The NENetworkRule class declares the programmatic interface of an object that contains a specification of a rule that matches the attributes of network traffic.
-- 
-- Phantom type for @NENetworkRule@.
data NENetworkRule

instance IsObjCObject (Id NENetworkRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NENetworkRule"

class IsNSObject a => IsNENetworkRule a where
  toNENetworkRule :: a -> Id NENetworkRule

instance IsNENetworkRule (Id NENetworkRule) where
  toNENetworkRule = unsafeCastId

instance IsNSObject (Id NENetworkRule) where
  toNSObject = unsafeCastId

-- ---------- NEOnDemandRule ----------

-- | NEOnDemandRule
--
-- The NEOnDemandRule class declares the programmatic interface for an object that defines an On Demand rule.
--
-- NEOnDemandRule is an abstract base class from which other action-specific rule classes are derived.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEOnDemandRule@.
data NEOnDemandRule

instance IsObjCObject (Id NEOnDemandRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEOnDemandRule"

class IsNSObject a => IsNEOnDemandRule a where
  toNEOnDemandRule :: a -> Id NEOnDemandRule

instance IsNEOnDemandRule (Id NEOnDemandRule) where
  toNEOnDemandRule = unsafeCastId

instance IsNSObject (Id NEOnDemandRule) where
  toNSObject = unsafeCastId

-- ---------- NEPacket ----------

-- | NEPacket
--
-- An NEPacket object represents the data, protocol family, and metadata associated with an IP packet. 	These packets are used to read and write on an NEPacketTunnelFlow.
--
-- NEPacket is part of NetworkExtension.framework
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEPacket@.
data NEPacket

instance IsObjCObject (Id NEPacket) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEPacket"

class IsNSObject a => IsNEPacket a where
  toNEPacket :: a -> Id NEPacket

instance IsNEPacket (Id NEPacket) where
  toNEPacket = unsafeCastId

instance IsNSObject (Id NEPacket) where
  toNSObject = unsafeCastId

-- ---------- NEPacketTunnelFlow ----------

-- | NEPacketTunnelFlow
--
-- The NEPacketTunnelFlow class declares the programmatic interface of an object that is used by NEPacketTunnelProvider implementations to tunnel IP packets.
--
-- NEPacketTunnelFlow is part of NetworkExtension.framework
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEPacketTunnelFlow@.
data NEPacketTunnelFlow

instance IsObjCObject (Id NEPacketTunnelFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEPacketTunnelFlow"

class IsNSObject a => IsNEPacketTunnelFlow a where
  toNEPacketTunnelFlow :: a -> Id NEPacketTunnelFlow

instance IsNEPacketTunnelFlow (Id NEPacketTunnelFlow) where
  toNEPacketTunnelFlow = unsafeCastId

instance IsNSObject (Id NEPacketTunnelFlow) where
  toNSObject = unsafeCastId

-- ---------- NEPrivateLTENetwork ----------

-- | NEPrivateLTENetwork
--
-- The NEPrivateLTENetwork class declares an object that contains the parameters of a private LTE network.
-- 
-- Phantom type for @NEPrivateLTENetwork@.
data NEPrivateLTENetwork

instance IsObjCObject (Id NEPrivateLTENetwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEPrivateLTENetwork"

class IsNSObject a => IsNEPrivateLTENetwork a where
  toNEPrivateLTENetwork :: a -> Id NEPrivateLTENetwork

instance IsNEPrivateLTENetwork (Id NEPrivateLTENetwork) where
  toNEPrivateLTENetwork = unsafeCastId

instance IsNSObject (Id NEPrivateLTENetwork) where
  toNSObject = unsafeCastId

-- ---------- NEProvider ----------

-- | NEProvider
--
-- The NEProvider class declares the programmatic interface that is common for all Network Extension providers.
--
-- See the sub classes of NEProvider for more details. Developers of Network Extension providers should create sub classes of the sub classes of NEProvider.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEProvider@.
data NEProvider

instance IsObjCObject (Id NEProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEProvider"

class IsNSObject a => IsNEProvider a where
  toNEProvider :: a -> Id NEProvider

instance IsNEProvider (Id NEProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEProvider) where
  toNSObject = unsafeCastId

-- ---------- NEProxyServer ----------

-- | NEProxyServer
--
-- The NEProxyServer class declares the programmatic interface for an object that contains settings for a proxy server.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEProxyServer@.
data NEProxyServer

instance IsObjCObject (Id NEProxyServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEProxyServer"

class IsNSObject a => IsNEProxyServer a where
  toNEProxyServer :: a -> Id NEProxyServer

instance IsNEProxyServer (Id NEProxyServer) where
  toNEProxyServer = unsafeCastId

instance IsNSObject (Id NEProxyServer) where
  toNSObject = unsafeCastId

-- ---------- NEProxySettings ----------

-- | NEProxySettings
--
-- The NEProxySettings class declares the programmatic interface for an object that contains proxy settings.
--
-- NEProxySettings is used in the context of a Network Extension configuration to specify the proxy that should be used for network traffic when the Network Extension is active.
-- 
-- Phantom type for @NEProxySettings@.
data NEProxySettings

instance IsObjCObject (Id NEProxySettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEProxySettings"

class IsNSObject a => IsNEProxySettings a where
  toNEProxySettings :: a -> Id NEProxySettings

instance IsNEProxySettings (Id NEProxySettings) where
  toNEProxySettings = unsafeCastId

instance IsNSObject (Id NEProxySettings) where
  toNSObject = unsafeCastId

-- ---------- NERelay ----------

-- | NERelay
--
-- The NERelay class declares the programmatic interface of an object that 			manages the details of a relay's configuration, such as authentication and URL details.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NERelay@.
data NERelay

instance IsObjCObject (Id NERelay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NERelay"

class IsNSObject a => IsNERelay a where
  toNERelay :: a -> Id NERelay

instance IsNERelay (Id NERelay) where
  toNERelay = unsafeCastId

instance IsNSObject (Id NERelay) where
  toNSObject = unsafeCastId

-- ---------- NERelayManager ----------

-- | NERelayManager
--
-- The NERelayManager class declares the programmatic interface for an object that manages relay configurations.
--
-- NERelayManager declares methods and properties for configuring and controlling relay settings on the system.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NERelayManager@.
data NERelayManager

instance IsObjCObject (Id NERelayManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NERelayManager"

class IsNSObject a => IsNERelayManager a where
  toNERelayManager :: a -> Id NERelayManager

instance IsNERelayManager (Id NERelayManager) where
  toNERelayManager = unsafeCastId

instance IsNSObject (Id NERelayManager) where
  toNSObject = unsafeCastId

-- ---------- NETunnelNetworkSettings ----------

-- | NETunnelNetworkSettings
--
-- The NETunnelNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NETunnelNetworkSettings is used by NETunnelProviders to communicate the desired network settings for the tunnel to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NETunnelNetworkSettings@.
data NETunnelNetworkSettings

instance IsObjCObject (Id NETunnelNetworkSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETunnelNetworkSettings"

class IsNSObject a => IsNETunnelNetworkSettings a where
  toNETunnelNetworkSettings :: a -> Id NETunnelNetworkSettings

instance IsNETunnelNetworkSettings (Id NETunnelNetworkSettings) where
  toNETunnelNetworkSettings = unsafeCastId

instance IsNSObject (Id NETunnelNetworkSettings) where
  toNSObject = unsafeCastId

-- ---------- NEURLFilter ----------

-- | Phantom type for @NEURLFilter@.
data NEURLFilter

instance IsObjCObject (Id NEURLFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEURLFilter"

class IsNSObject a => IsNEURLFilter a where
  toNEURLFilter :: a -> Id NEURLFilter

instance IsNEURLFilter (Id NEURLFilter) where
  toNEURLFilter = unsafeCastId

instance IsNSObject (Id NEURLFilter) where
  toNSObject = unsafeCastId

-- ---------- NEVPNConnection ----------

-- | NEVPNConnection
--
-- The NEVPNConnection class declares the programmatic interface for an object that manages VPN connections.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEVPNConnection@.
data NEVPNConnection

instance IsObjCObject (Id NEVPNConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNConnection"

class IsNSObject a => IsNEVPNConnection a where
  toNEVPNConnection :: a -> Id NEVPNConnection

instance IsNEVPNConnection (Id NEVPNConnection) where
  toNEVPNConnection = unsafeCastId

instance IsNSObject (Id NEVPNConnection) where
  toNSObject = unsafeCastId

-- ---------- NEVPNIKEv2PPKConfiguration ----------

-- | NEVPNIKEv2PPKConfiguration
--
-- The NEVPNIKEv2PPKConfiguration class declares the programmatic interface of an object that manages parameters for a Post-quantum Pre-shared Key (PPK)
--
-- Instances of this class conform to RFC 8784. Instances of this class are thread safe.
-- 
-- Phantom type for @NEVPNIKEv2PPKConfiguration@.
data NEVPNIKEv2PPKConfiguration

instance IsObjCObject (Id NEVPNIKEv2PPKConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNIKEv2PPKConfiguration"

class IsNSObject a => IsNEVPNIKEv2PPKConfiguration a where
  toNEVPNIKEv2PPKConfiguration :: a -> Id NEVPNIKEv2PPKConfiguration

instance IsNEVPNIKEv2PPKConfiguration (Id NEVPNIKEv2PPKConfiguration) where
  toNEVPNIKEv2PPKConfiguration = unsafeCastId

instance IsNSObject (Id NEVPNIKEv2PPKConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NEVPNIKEv2SecurityAssociationParameters ----------

-- | NEVPNIKEv2SecurityAssociationParameters
--
-- The NEVPNIKEv2SecurityAssociationParameters class declares the programmatic interface of an object that manages parameters for an IPSec Security Association
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEVPNIKEv2SecurityAssociationParameters@.
data NEVPNIKEv2SecurityAssociationParameters

instance IsObjCObject (Id NEVPNIKEv2SecurityAssociationParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNIKEv2SecurityAssociationParameters"

class IsNSObject a => IsNEVPNIKEv2SecurityAssociationParameters a where
  toNEVPNIKEv2SecurityAssociationParameters :: a -> Id NEVPNIKEv2SecurityAssociationParameters

instance IsNEVPNIKEv2SecurityAssociationParameters (Id NEVPNIKEv2SecurityAssociationParameters) where
  toNEVPNIKEv2SecurityAssociationParameters = unsafeCastId

instance IsNSObject (Id NEVPNIKEv2SecurityAssociationParameters) where
  toNSObject = unsafeCastId

-- ---------- NEVPNManager ----------

-- | NEVPNManager
--
-- The NEVPNManager class declares the programmatic interface for an object that manages Virtual Private Network (VPN) configurations.
--
-- NEVPNManager declares methods and properties for configuring and controlling a VPN.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEVPNManager@.
data NEVPNManager

instance IsObjCObject (Id NEVPNManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNManager"

class IsNSObject a => IsNEVPNManager a where
  toNEVPNManager :: a -> Id NEVPNManager

instance IsNEVPNManager (Id NEVPNManager) where
  toNEVPNManager = unsafeCastId

instance IsNSObject (Id NEVPNManager) where
  toNSObject = unsafeCastId

-- ---------- NEVPNProtocol ----------

-- | NETunnelProvider.h
--
-- This file declares the NETunnelProvider API. The NETunnelProvider API is used to implement Network Extension providers that provide network tunneling services.
--
-- This API is part of NetworkExtension.framework
-- 
-- Phantom type for @NEVPNProtocol@.
data NEVPNProtocol

instance IsObjCObject (Id NEVPNProtocol) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNProtocol"

class IsNSObject a => IsNEVPNProtocol a where
  toNEVPNProtocol :: a -> Id NEVPNProtocol

instance IsNEVPNProtocol (Id NEVPNProtocol) where
  toNEVPNProtocol = unsafeCastId

instance IsNSObject (Id NEVPNProtocol) where
  toNSObject = unsafeCastId

-- ---------- NWEndpoint ----------

-- | NEAppProxyTCPFlow
--
-- This file declares the NEAppProxyTCPFlow API. The NEAppProxyTCPFlow API is used by NEAppProxyProvider implementations to proxy the payload of TCP connections.
-- 
-- Phantom type for @NWEndpoint@.
data NWEndpoint

instance IsObjCObject (Id NWEndpoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWEndpoint"

class IsNSObject a => IsNWEndpoint a where
  toNWEndpoint :: a -> Id NWEndpoint

instance IsNWEndpoint (Id NWEndpoint) where
  toNWEndpoint = unsafeCastId

instance IsNSObject (Id NWEndpoint) where
  toNSObject = unsafeCastId

-- ---------- NWPath ----------

-- | NWPath
--
-- A network path, represented with NWPath, expresses the viability status and		properties of the path that a networking connection will take on the device. For example,		if the path status is NWPathStatusSatisfied, then a connection could use that path.
-- 
-- Phantom type for @NWPath@.
data NWPath

instance IsObjCObject (Id NWPath) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWPath"

class IsNSObject a => IsNWPath a where
  toNWPath :: a -> Id NWPath

instance IsNWPath (Id NWPath) where
  toNWPath = unsafeCastId

instance IsNSObject (Id NWPath) where
  toNSObject = unsafeCastId

-- ---------- NWTCPConnection ----------

-- | NWTCPConnection
--
-- Establish TCP connections to an endpoint, and send and receive data on the TCP connection.
-- 
-- Phantom type for @NWTCPConnection@.
data NWTCPConnection

instance IsObjCObject (Id NWTCPConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWTCPConnection"

class IsNSObject a => IsNWTCPConnection a where
  toNWTCPConnection :: a -> Id NWTCPConnection

instance IsNWTCPConnection (Id NWTCPConnection) where
  toNWTCPConnection = unsafeCastId

instance IsNSObject (Id NWTCPConnection) where
  toNSObject = unsafeCastId

-- ---------- NWTLSParameters ----------

-- | DEPRECATION NOTICE
--
-- NW object wrappers are hidden in Swift 6. To continue accessing them, you can prepend double underscores to the symbol name.
-- 
-- Phantom type for @NWTLSParameters@.
data NWTLSParameters

instance IsObjCObject (Id NWTLSParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWTLSParameters"

class IsNSObject a => IsNWTLSParameters a where
  toNWTLSParameters :: a -> Id NWTLSParameters

instance IsNWTLSParameters (Id NWTLSParameters) where
  toNWTLSParameters = unsafeCastId

instance IsNSObject (Id NWTLSParameters) where
  toNSObject = unsafeCastId

-- ---------- NWUDPSession ----------

-- | NWUDPSession
--
-- Open UDP datagram sessions to an endpoint, and send and receive datagrams.
-- 
-- Phantom type for @NWUDPSession@.
data NWUDPSession

instance IsObjCObject (Id NWUDPSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWUDPSession"

class IsNSObject a => IsNWUDPSession a where
  toNWUDPSession :: a -> Id NWUDPSession

instance IsNWUDPSession (Id NWUDPSession) where
  toNWUDPSession = unsafeCastId

instance IsNSObject (Id NWUDPSession) where
  toNSObject = unsafeCastId

-- ---------- NEAppProxyTCPFlow ----------

-- | NEAppProxyTCPFlow
--
-- The NEAppProxyTCPFlow class declares the programmatic interface of an object that is used by NEAppProxyProvider implementations to proxy the payload of TCP connections.
--
-- NEAppProxyTCPFlow is part of NetworkExtension.framework
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEAppProxyTCPFlow@.
data NEAppProxyTCPFlow

instance IsObjCObject (Id NEAppProxyTCPFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppProxyTCPFlow"

class IsNEAppProxyFlow a => IsNEAppProxyTCPFlow a where
  toNEAppProxyTCPFlow :: a -> Id NEAppProxyTCPFlow

instance IsNEAppProxyTCPFlow (Id NEAppProxyTCPFlow) where
  toNEAppProxyTCPFlow = unsafeCastId

instance IsNEAppProxyFlow (Id NEAppProxyTCPFlow) where
  toNEAppProxyFlow = unsafeCastId

instance IsNSObject (Id NEAppProxyTCPFlow) where
  toNSObject = unsafeCastId

-- ---------- NEAppProxyUDPFlow ----------

-- | NEAppProxyUDPFlow
--
-- The NEAppProxyUDPFlow class declares the programmatic interface of an object that is used by NEAppProxyProvider implementations to proxy the payload of UDP datagrams.
--
-- NEAppProxyUDPFlow is part of NetworkExtension.framework.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEAppProxyUDPFlow@.
data NEAppProxyUDPFlow

instance IsObjCObject (Id NEAppProxyUDPFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppProxyUDPFlow"

class IsNEAppProxyFlow a => IsNEAppProxyUDPFlow a where
  toNEAppProxyUDPFlow :: a -> Id NEAppProxyUDPFlow

instance IsNEAppProxyUDPFlow (Id NEAppProxyUDPFlow) where
  toNEAppProxyUDPFlow = unsafeCastId

instance IsNEAppProxyFlow (Id NEAppProxyUDPFlow) where
  toNEAppProxyFlow = unsafeCastId

instance IsNSObject (Id NEAppProxyUDPFlow) where
  toNSObject = unsafeCastId

-- ---------- NEDNSOverHTTPSSettings ----------

-- | Phantom type for @NEDNSOverHTTPSSettings@.
data NEDNSOverHTTPSSettings

instance IsObjCObject (Id NEDNSOverHTTPSSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSOverHTTPSSettings"

class IsNEDNSSettings a => IsNEDNSOverHTTPSSettings a where
  toNEDNSOverHTTPSSettings :: a -> Id NEDNSOverHTTPSSettings

instance IsNEDNSOverHTTPSSettings (Id NEDNSOverHTTPSSettings) where
  toNEDNSOverHTTPSSettings = unsafeCastId

instance IsNEDNSSettings (Id NEDNSOverHTTPSSettings) where
  toNEDNSSettings = unsafeCastId

instance IsNSObject (Id NEDNSOverHTTPSSettings) where
  toNSObject = unsafeCastId

-- ---------- NEDNSOverTLSSettings ----------

-- | Phantom type for @NEDNSOverTLSSettings@.
data NEDNSOverTLSSettings

instance IsObjCObject (Id NEDNSOverTLSSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSOverTLSSettings"

class IsNEDNSSettings a => IsNEDNSOverTLSSettings a where
  toNEDNSOverTLSSettings :: a -> Id NEDNSOverTLSSettings

instance IsNEDNSOverTLSSettings (Id NEDNSOverTLSSettings) where
  toNEDNSOverTLSSettings = unsafeCastId

instance IsNEDNSSettings (Id NEDNSOverTLSSettings) where
  toNEDNSSettings = unsafeCastId

instance IsNSObject (Id NEDNSOverTLSSettings) where
  toNSObject = unsafeCastId

-- ---------- NEFilterBrowserFlow ----------

-- | NEFilterBrowserFlow
--
-- The NEFilterBrowserFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered, which is originated from NEFilterSource.
--
-- NEFilterBrowserFlow is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterBrowserFlow@.
data NEFilterBrowserFlow

instance IsObjCObject (Id NEFilterBrowserFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterBrowserFlow"

class IsNEFilterFlow a => IsNEFilterBrowserFlow a where
  toNEFilterBrowserFlow :: a -> Id NEFilterBrowserFlow

instance IsNEFilterBrowserFlow (Id NEFilterBrowserFlow) where
  toNEFilterBrowserFlow = unsafeCastId

instance IsNEFilterFlow (Id NEFilterBrowserFlow) where
  toNEFilterFlow = unsafeCastId

instance IsNSObject (Id NEFilterBrowserFlow) where
  toNSObject = unsafeCastId

-- ---------- NEFilterSocketFlow ----------

-- | NEFilterSocketFlow
--
-- The NEFilterSocketFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered, which is originated from the socket.
--
-- NEFilterSocketFlow is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterSocketFlow@.
data NEFilterSocketFlow

instance IsObjCObject (Id NEFilterSocketFlow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterSocketFlow"

class IsNEFilterFlow a => IsNEFilterSocketFlow a where
  toNEFilterSocketFlow :: a -> Id NEFilterSocketFlow

instance IsNEFilterSocketFlow (Id NEFilterSocketFlow) where
  toNEFilterSocketFlow = unsafeCastId

instance IsNEFilterFlow (Id NEFilterSocketFlow) where
  toNEFilterFlow = unsafeCastId

instance IsNSObject (Id NEFilterSocketFlow) where
  toNSObject = unsafeCastId

-- ---------- NEFilterDataVerdict ----------

-- | NEFilterDataVerdict
--
-- The NEFilterDataVerdict class declares the programmatic interface of an object that is the verdict for a flow of network data after some of the data has been seen by the filter.
--
-- NEFilterDataVerdict is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterDataVerdict@.
data NEFilterDataVerdict

instance IsObjCObject (Id NEFilterDataVerdict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterDataVerdict"

class IsNEFilterVerdict a => IsNEFilterDataVerdict a where
  toNEFilterDataVerdict :: a -> Id NEFilterDataVerdict

instance IsNEFilterDataVerdict (Id NEFilterDataVerdict) where
  toNEFilterDataVerdict = unsafeCastId

instance IsNEFilterVerdict (Id NEFilterDataVerdict) where
  toNEFilterVerdict = unsafeCastId

instance IsNSObject (Id NEFilterDataVerdict) where
  toNSObject = unsafeCastId

-- ---------- NEFilterNewFlowVerdict ----------

-- | NEFilterNewFlowVerdict
--
-- The NEFilterNewFlowVerdict declares the programmatic interface of an object that is the verdict for a new flow of network data before any of the flow's data has been seen by the filter.
--
-- NEFilterNewFlowVerdict is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterNewFlowVerdict@.
data NEFilterNewFlowVerdict

instance IsObjCObject (Id NEFilterNewFlowVerdict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterNewFlowVerdict"

class IsNEFilterVerdict a => IsNEFilterNewFlowVerdict a where
  toNEFilterNewFlowVerdict :: a -> Id NEFilterNewFlowVerdict

instance IsNEFilterNewFlowVerdict (Id NEFilterNewFlowVerdict) where
  toNEFilterNewFlowVerdict = unsafeCastId

instance IsNEFilterVerdict (Id NEFilterNewFlowVerdict) where
  toNEFilterVerdict = unsafeCastId

instance IsNSObject (Id NEFilterNewFlowVerdict) where
  toNSObject = unsafeCastId

-- ---------- NEFilterRemediationVerdict ----------

-- | NEFilterRemediationVerdict
--
-- The NEFilterRemediationVerdict class declares the programmatic interface of an object that is the verdict for a flow which has been blocked by the filter, but the user has made a request for remediation.
--
-- NEFilterRemediationVerdict is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterRemediationVerdict@.
data NEFilterRemediationVerdict

instance IsObjCObject (Id NEFilterRemediationVerdict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterRemediationVerdict"

class IsNEFilterVerdict a => IsNEFilterRemediationVerdict a where
  toNEFilterRemediationVerdict :: a -> Id NEFilterRemediationVerdict

instance IsNEFilterRemediationVerdict (Id NEFilterRemediationVerdict) where
  toNEFilterRemediationVerdict = unsafeCastId

instance IsNEFilterVerdict (Id NEFilterRemediationVerdict) where
  toNEFilterVerdict = unsafeCastId

instance IsNSObject (Id NEFilterRemediationVerdict) where
  toNSObject = unsafeCastId

-- ---------- NEOnDemandRuleConnect ----------

-- | NEOnDemandRuleConnect
--
-- The NEOnDemandRuleConnect class declares the programmatic interface for an object that defines an On Demand rule with the "Connect" action.
--
-- When rules of this class match, the VPN connection is started whenever an application running on the system opens a network connection.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEOnDemandRuleConnect@.
data NEOnDemandRuleConnect

instance IsObjCObject (Id NEOnDemandRuleConnect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEOnDemandRuleConnect"

class IsNEOnDemandRule a => IsNEOnDemandRuleConnect a where
  toNEOnDemandRuleConnect :: a -> Id NEOnDemandRuleConnect

instance IsNEOnDemandRuleConnect (Id NEOnDemandRuleConnect) where
  toNEOnDemandRuleConnect = unsafeCastId

instance IsNEOnDemandRule (Id NEOnDemandRuleConnect) where
  toNEOnDemandRule = unsafeCastId

instance IsNSObject (Id NEOnDemandRuleConnect) where
  toNSObject = unsafeCastId

-- ---------- NEOnDemandRuleDisconnect ----------

-- | NEOnDemandRuleDisconnect
--
-- The NEOnDemandRuleDisconnect class declares the programmatic interface for an object that defines an On Demand rule with the "Disconnect" action.
--
-- When rules of this class match, the VPN connection is not started, and the VPN connection is disconnected if it is not currently disconnected.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEOnDemandRuleDisconnect@.
data NEOnDemandRuleDisconnect

instance IsObjCObject (Id NEOnDemandRuleDisconnect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEOnDemandRuleDisconnect"

class IsNEOnDemandRule a => IsNEOnDemandRuleDisconnect a where
  toNEOnDemandRuleDisconnect :: a -> Id NEOnDemandRuleDisconnect

instance IsNEOnDemandRuleDisconnect (Id NEOnDemandRuleDisconnect) where
  toNEOnDemandRuleDisconnect = unsafeCastId

instance IsNEOnDemandRule (Id NEOnDemandRuleDisconnect) where
  toNEOnDemandRule = unsafeCastId

instance IsNSObject (Id NEOnDemandRuleDisconnect) where
  toNSObject = unsafeCastId

-- ---------- NEOnDemandRuleEvaluateConnection ----------

-- | NEOnDemandRuleEvaluateConnection
--
-- The NEOnDemandRuleEvaluateConnection class declares the programmatic interface for an object that defines an On Demand rule with the "Evaluate Connection" action.
--
-- When rules of this class match, the properties of the network connection being established are matched against a set of connection rules. The action of the matched rule (if any) is used to determine whether or not the VPN will be started.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEOnDemandRuleEvaluateConnection@.
data NEOnDemandRuleEvaluateConnection

instance IsObjCObject (Id NEOnDemandRuleEvaluateConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEOnDemandRuleEvaluateConnection"

class IsNEOnDemandRule a => IsNEOnDemandRuleEvaluateConnection a where
  toNEOnDemandRuleEvaluateConnection :: a -> Id NEOnDemandRuleEvaluateConnection

instance IsNEOnDemandRuleEvaluateConnection (Id NEOnDemandRuleEvaluateConnection) where
  toNEOnDemandRuleEvaluateConnection = unsafeCastId

instance IsNEOnDemandRule (Id NEOnDemandRuleEvaluateConnection) where
  toNEOnDemandRule = unsafeCastId

instance IsNSObject (Id NEOnDemandRuleEvaluateConnection) where
  toNSObject = unsafeCastId

-- ---------- NEOnDemandRuleIgnore ----------

-- | NEOnDemandRuleIgnore
--
-- The NEOnDemandRuleIgnore class declares the programmatic interface for an object that defines an On Demand rule with the "Ignore" action.
--
-- When rules of this class match, the VPN connection is not started, and the current status of the VPN connection is left unchanged.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEOnDemandRuleIgnore@.
data NEOnDemandRuleIgnore

instance IsObjCObject (Id NEOnDemandRuleIgnore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEOnDemandRuleIgnore"

class IsNEOnDemandRule a => IsNEOnDemandRuleIgnore a where
  toNEOnDemandRuleIgnore :: a -> Id NEOnDemandRuleIgnore

instance IsNEOnDemandRuleIgnore (Id NEOnDemandRuleIgnore) where
  toNEOnDemandRuleIgnore = unsafeCastId

instance IsNEOnDemandRule (Id NEOnDemandRuleIgnore) where
  toNEOnDemandRule = unsafeCastId

instance IsNSObject (Id NEOnDemandRuleIgnore) where
  toNSObject = unsafeCastId

-- ---------- NEAppPushProvider ----------

-- | NEAppPushProvider
--
-- The NEAppPushProvider class declares a programmatic interface to manage a life cycle of app push provider. It also allows the provider to handle outgoing communication message from the containing app, and pass incoming call message to the containing app. NEAppPushProvider is part of NetworkExtension.framework
-- 
-- Phantom type for @NEAppPushProvider@.
data NEAppPushProvider

instance IsObjCObject (Id NEAppPushProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppPushProvider"

class IsNEProvider a => IsNEAppPushProvider a where
  toNEAppPushProvider :: a -> Id NEAppPushProvider

instance IsNEAppPushProvider (Id NEAppPushProvider) where
  toNEAppPushProvider = unsafeCastId

instance IsNEProvider (Id NEAppPushProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEAppPushProvider) where
  toNSObject = unsafeCastId

-- ---------- NEDNSProxyProvider ----------

-- | NEDNSProxyProvider
--
-- The NEDNSProxyProvider class declares the programmatic interface for an object that implements the client side of a custom DNS proxy solution.
--
-- NEDNSProxyProvider is part of NetworkExtension.framework
-- 
-- Phantom type for @NEDNSProxyProvider@.
data NEDNSProxyProvider

instance IsObjCObject (Id NEDNSProxyProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSProxyProvider"

class IsNEProvider a => IsNEDNSProxyProvider a where
  toNEDNSProxyProvider :: a -> Id NEDNSProxyProvider

instance IsNEDNSProxyProvider (Id NEDNSProxyProvider) where
  toNEDNSProxyProvider = unsafeCastId

instance IsNEProvider (Id NEDNSProxyProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEDNSProxyProvider) where
  toNSObject = unsafeCastId

-- ---------- NEFilterProvider ----------

-- | NEFilterProvider
--
-- The NEFilterProvider class is an abstract base class that declares the programmatic interface of an object that implements a socket filter.
--
-- NEFilterProvider is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterProvider@.
data NEFilterProvider

instance IsObjCObject (Id NEFilterProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterProvider"

class IsNEProvider a => IsNEFilterProvider a where
  toNEFilterProvider :: a -> Id NEFilterProvider

instance IsNEFilterProvider (Id NEFilterProvider) where
  toNEFilterProvider = unsafeCastId

instance IsNEProvider (Id NEFilterProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEFilterProvider) where
  toNSObject = unsafeCastId

-- ---------- NETunnelProvider ----------

-- | NETunnelProvider
--
-- The NETunnelProvider class declares the programmatic interface for an object that provides a network tunnel service.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NETunnelProvider@.
data NETunnelProvider

instance IsObjCObject (Id NETunnelProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETunnelProvider"

class IsNEProvider a => IsNETunnelProvider a where
  toNETunnelProvider :: a -> Id NETunnelProvider

instance IsNETunnelProvider (Id NETunnelProvider) where
  toNETunnelProvider = unsafeCastId

instance IsNEProvider (Id NETunnelProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NETunnelProvider) where
  toNSObject = unsafeCastId

-- ---------- NEPacketTunnelNetworkSettings ----------

-- | NEPacketTunnelNetworkSettings
--
-- The NEPacketTunnelNetworkSettings class declares the programmatic interface for an object that contains IP network settings.
--
-- NEPacketTunnelNetworkSettings is used by NEPacketTunnelProviders to communicate the desired IP network settings for the packet tunnel to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEPacketTunnelNetworkSettings@.
data NEPacketTunnelNetworkSettings

instance IsObjCObject (Id NEPacketTunnelNetworkSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEPacketTunnelNetworkSettings"

class IsNETunnelNetworkSettings a => IsNEPacketTunnelNetworkSettings a where
  toNEPacketTunnelNetworkSettings :: a -> Id NEPacketTunnelNetworkSettings

instance IsNEPacketTunnelNetworkSettings (Id NEPacketTunnelNetworkSettings) where
  toNEPacketTunnelNetworkSettings = unsafeCastId

instance IsNETunnelNetworkSettings (Id NEPacketTunnelNetworkSettings) where
  toNETunnelNetworkSettings = unsafeCastId

instance IsNSObject (Id NEPacketTunnelNetworkSettings) where
  toNSObject = unsafeCastId

-- ---------- NETransparentProxyNetworkSettings ----------

-- | NETransparentProxyNetworkSettings
--
-- The NETransparentProxyNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NETransparentProxyNetworkSettings is used by NEAppProxyProviders to communicate the desired network settings for the proxy to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NETransparentProxyNetworkSettings@.
data NETransparentProxyNetworkSettings

instance IsObjCObject (Id NETransparentProxyNetworkSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETransparentProxyNetworkSettings"

class IsNETunnelNetworkSettings a => IsNETransparentProxyNetworkSettings a where
  toNETransparentProxyNetworkSettings :: a -> Id NETransparentProxyNetworkSettings

instance IsNETransparentProxyNetworkSettings (Id NETransparentProxyNetworkSettings) where
  toNETransparentProxyNetworkSettings = unsafeCastId

instance IsNETunnelNetworkSettings (Id NETransparentProxyNetworkSettings) where
  toNETunnelNetworkSettings = unsafeCastId

instance IsNSObject (Id NETransparentProxyNetworkSettings) where
  toNSObject = unsafeCastId

-- ---------- NETunnelProviderSession ----------

-- | NETunnelProviderSession.h
--
-- This file declares the NETunnelProviderSession API. The NETunnelProviderSession API is used to control network tunnel services provided by NETunnelProvider implementations.
--
-- This API is part of NetworkExtension.framework.
-- 
-- Phantom type for @NETunnelProviderSession@.
data NETunnelProviderSession

instance IsObjCObject (Id NETunnelProviderSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETunnelProviderSession"

class IsNEVPNConnection a => IsNETunnelProviderSession a where
  toNETunnelProviderSession :: a -> Id NETunnelProviderSession

instance IsNETunnelProviderSession (Id NETunnelProviderSession) where
  toNETunnelProviderSession = unsafeCastId

instance IsNEVPNConnection (Id NETunnelProviderSession) where
  toNEVPNConnection = unsafeCastId

instance IsNSObject (Id NETunnelProviderSession) where
  toNSObject = unsafeCastId

-- ---------- NETransparentProxyManager ----------

-- | NETransparentProxyManager
--
-- The NETransparentProxyManager class declares the programmatic interface for an object that is used to configure and control transparent proxies provided by NEAppProxyProviders.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NETransparentProxyManager@.
data NETransparentProxyManager

instance IsObjCObject (Id NETransparentProxyManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETransparentProxyManager"

class IsNEVPNManager a => IsNETransparentProxyManager a where
  toNETransparentProxyManager :: a -> Id NETransparentProxyManager

instance IsNETransparentProxyManager (Id NETransparentProxyManager) where
  toNETransparentProxyManager = unsafeCastId

instance IsNEVPNManager (Id NETransparentProxyManager) where
  toNEVPNManager = unsafeCastId

instance IsNSObject (Id NETransparentProxyManager) where
  toNSObject = unsafeCastId

-- ---------- NETunnelProviderManager ----------

-- | NETunnelProviderManager
--
-- The NETunnelProviderManager class declares the programmatic interface for an object that is used to configure and control network tunnels provided by NETunnelProviders.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NETunnelProviderManager@.
data NETunnelProviderManager

instance IsObjCObject (Id NETunnelProviderManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETunnelProviderManager"

class IsNEVPNManager a => IsNETunnelProviderManager a where
  toNETunnelProviderManager :: a -> Id NETunnelProviderManager

instance IsNETunnelProviderManager (Id NETunnelProviderManager) where
  toNETunnelProviderManager = unsafeCastId

instance IsNEVPNManager (Id NETunnelProviderManager) where
  toNEVPNManager = unsafeCastId

instance IsNSObject (Id NETunnelProviderManager) where
  toNSObject = unsafeCastId

-- ---------- NEDNSProxyProviderProtocol ----------

-- | NEDNSProxyProviderProtocol
--
-- The NEDNSProxyProviderProtocol class declares the programmatic interface for an object that contains NEDNSProxyProvider-specific configuration settings.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEDNSProxyProviderProtocol@.
data NEDNSProxyProviderProtocol

instance IsObjCObject (Id NEDNSProxyProviderProtocol) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEDNSProxyProviderProtocol"

class IsNEVPNProtocol a => IsNEDNSProxyProviderProtocol a where
  toNEDNSProxyProviderProtocol :: a -> Id NEDNSProxyProviderProtocol

instance IsNEDNSProxyProviderProtocol (Id NEDNSProxyProviderProtocol) where
  toNEDNSProxyProviderProtocol = unsafeCastId

instance IsNEVPNProtocol (Id NEDNSProxyProviderProtocol) where
  toNEVPNProtocol = unsafeCastId

instance IsNSObject (Id NEDNSProxyProviderProtocol) where
  toNSObject = unsafeCastId

-- ---------- NETunnelProviderProtocol ----------

-- | NETunnelProviderProtocol
--
-- The NETunnelProviderProtocol class declares the programmatic interface for an object that contains NETunnelProvider-specific configuration settings.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NETunnelProviderProtocol@.
data NETunnelProviderProtocol

instance IsObjCObject (Id NETunnelProviderProtocol) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETunnelProviderProtocol"

class IsNEVPNProtocol a => IsNETunnelProviderProtocol a where
  toNETunnelProviderProtocol :: a -> Id NETunnelProviderProtocol

instance IsNETunnelProviderProtocol (Id NETunnelProviderProtocol) where
  toNETunnelProviderProtocol = unsafeCastId

instance IsNEVPNProtocol (Id NETunnelProviderProtocol) where
  toNEVPNProtocol = unsafeCastId

instance IsNSObject (Id NETunnelProviderProtocol) where
  toNSObject = unsafeCastId

-- ---------- NEVPNProtocolIPSec ----------

-- | NEVPNProtocolIPSec
--
-- The NEVPNProtocolIPSec class declares the programmatic interface of an object that manages the IPSec-specific portion of a VPN configuration.
--
-- Instances of this class use IKE version 1 for key negotiation.
-- 
-- Phantom type for @NEVPNProtocolIPSec@.
data NEVPNProtocolIPSec

instance IsObjCObject (Id NEVPNProtocolIPSec) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNProtocolIPSec"

class IsNEVPNProtocol a => IsNEVPNProtocolIPSec a where
  toNEVPNProtocolIPSec :: a -> Id NEVPNProtocolIPSec

instance IsNEVPNProtocolIPSec (Id NEVPNProtocolIPSec) where
  toNEVPNProtocolIPSec = unsafeCastId

instance IsNEVPNProtocol (Id NEVPNProtocolIPSec) where
  toNEVPNProtocol = unsafeCastId

instance IsNSObject (Id NEVPNProtocolIPSec) where
  toNSObject = unsafeCastId

-- ---------- NWBonjourServiceEndpoint ----------

-- | NWBonjourServiceEndpoint
--
-- NWBonjourServiceEndpoint is a subclass of NWEndpoint. It represents an endpoint		backed by a Bonjour service, specified with a name, type, and domain. For example, the		Bonjour service MyMusicStudio._music._tcp.local. has the name "MyMusicStudio",		the type "_music._tcp", and the domain "local".
-- 
-- Phantom type for @NWBonjourServiceEndpoint@.
data NWBonjourServiceEndpoint

instance IsObjCObject (Id NWBonjourServiceEndpoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWBonjourServiceEndpoint"

class IsNWEndpoint a => IsNWBonjourServiceEndpoint a where
  toNWBonjourServiceEndpoint :: a -> Id NWBonjourServiceEndpoint

instance IsNWBonjourServiceEndpoint (Id NWBonjourServiceEndpoint) where
  toNWBonjourServiceEndpoint = unsafeCastId

instance IsNSObject (Id NWBonjourServiceEndpoint) where
  toNSObject = unsafeCastId

instance IsNWEndpoint (Id NWBonjourServiceEndpoint) where
  toNWEndpoint = unsafeCastId

-- ---------- NWHostEndpoint ----------

-- | NWHostEndpoint
--
-- NWHostEndpoint is a subclass of NWEndpoint. It represents an endpoint backed by a		hostname and port. Note that a hostname string may be an IP or IPv6 address.
-- 
-- Phantom type for @NWHostEndpoint@.
data NWHostEndpoint

instance IsObjCObject (Id NWHostEndpoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NWHostEndpoint"

class IsNWEndpoint a => IsNWHostEndpoint a where
  toNWHostEndpoint :: a -> Id NWHostEndpoint

instance IsNWHostEndpoint (Id NWHostEndpoint) where
  toNWHostEndpoint = unsafeCastId

instance IsNSObject (Id NWHostEndpoint) where
  toNSObject = unsafeCastId

instance IsNWEndpoint (Id NWHostEndpoint) where
  toNWEndpoint = unsafeCastId

-- ---------- NEFilterControlVerdict ----------

-- | NEFilterControlVerdict
--
-- The NEFilterControlVerdict declares the programmatic interface of an object that is the verdict for a new flow of network data by the control provider.
--
-- NEFilterControlVerdict is part of NetworkExtension.framework
-- 
-- Phantom type for @NEFilterControlVerdict@.
data NEFilterControlVerdict

instance IsObjCObject (Id NEFilterControlVerdict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterControlVerdict"

class IsNEFilterNewFlowVerdict a => IsNEFilterControlVerdict a where
  toNEFilterControlVerdict :: a -> Id NEFilterControlVerdict

instance IsNEFilterControlVerdict (Id NEFilterControlVerdict) where
  toNEFilterControlVerdict = unsafeCastId

instance IsNEFilterNewFlowVerdict (Id NEFilterControlVerdict) where
  toNEFilterNewFlowVerdict = unsafeCastId

instance IsNEFilterVerdict (Id NEFilterControlVerdict) where
  toNEFilterVerdict = unsafeCastId

instance IsNSObject (Id NEFilterControlVerdict) where
  toNSObject = unsafeCastId

-- ---------- NEFilterControlProvider ----------

-- | NEFilterControlProvider
--
-- The NEFilterControlProvider class declares the programmatic interface for an object that is responsible for installing filtering rules on the device.
-- 
-- Phantom type for @NEFilterControlProvider@.
data NEFilterControlProvider

instance IsObjCObject (Id NEFilterControlProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterControlProvider"

class IsNEFilterProvider a => IsNEFilterControlProvider a where
  toNEFilterControlProvider :: a -> Id NEFilterControlProvider

instance IsNEFilterControlProvider (Id NEFilterControlProvider) where
  toNEFilterControlProvider = unsafeCastId

instance IsNEFilterProvider (Id NEFilterControlProvider) where
  toNEFilterProvider = unsafeCastId

instance IsNEProvider (Id NEFilterControlProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEFilterControlProvider) where
  toNSObject = unsafeCastId

-- ---------- NEFilterDataProvider ----------

-- | NEFilterDataProvider
--
-- The NEFilterDataProvider class declares the programmatic interface for an object that evaluates network data flows based on a set of locally-available rules and makes decisions about whether to block or allow the flows.
-- 
-- Phantom type for @NEFilterDataProvider@.
data NEFilterDataProvider

instance IsObjCObject (Id NEFilterDataProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterDataProvider"

class IsNEFilterProvider a => IsNEFilterDataProvider a where
  toNEFilterDataProvider :: a -> Id NEFilterDataProvider

instance IsNEFilterDataProvider (Id NEFilterDataProvider) where
  toNEFilterDataProvider = unsafeCastId

instance IsNEFilterProvider (Id NEFilterDataProvider) where
  toNEFilterProvider = unsafeCastId

instance IsNEProvider (Id NEFilterDataProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEFilterDataProvider) where
  toNSObject = unsafeCastId

-- ---------- NEFilterPacketProvider ----------

-- | NEFilterPacketProvider
--
-- The NEFilterPacketProvider class declares the programmatic interface for an object that evaluates network packets decisions about whether to block, allow, or delay the packets.
-- 
-- Phantom type for @NEFilterPacketProvider@.
data NEFilterPacketProvider

instance IsObjCObject (Id NEFilterPacketProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEFilterPacketProvider"

class IsNEFilterProvider a => IsNEFilterPacketProvider a where
  toNEFilterPacketProvider :: a -> Id NEFilterPacketProvider

instance IsNEFilterPacketProvider (Id NEFilterPacketProvider) where
  toNEFilterPacketProvider = unsafeCastId

instance IsNEFilterProvider (Id NEFilterPacketProvider) where
  toNEFilterProvider = unsafeCastId

instance IsNEProvider (Id NEFilterPacketProvider) where
  toNEProvider = unsafeCastId

instance IsNSObject (Id NEFilterPacketProvider) where
  toNSObject = unsafeCastId

-- ---------- NEAppProxyProvider ----------

-- | NEAppProxyProvider
--
-- The NEAppProxyProvider class declares the programmatic interface for an object that implements the client side of a custom network proxy solution.
--
-- NEAppProxyProvider is part of NetworkExtension.framework
-- 
-- Phantom type for @NEAppProxyProvider@.
data NEAppProxyProvider

instance IsObjCObject (Id NEAppProxyProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppProxyProvider"

class IsNETunnelProvider a => IsNEAppProxyProvider a where
  toNEAppProxyProvider :: a -> Id NEAppProxyProvider

instance IsNEAppProxyProvider (Id NEAppProxyProvider) where
  toNEAppProxyProvider = unsafeCastId

instance IsNEProvider (Id NEAppProxyProvider) where
  toNEProvider = unsafeCastId

instance IsNETunnelProvider (Id NEAppProxyProvider) where
  toNETunnelProvider = unsafeCastId

instance IsNSObject (Id NEAppProxyProvider) where
  toNSObject = unsafeCastId

-- ---------- NEPacketTunnelProvider ----------

-- | NEPacketTunnelProvider
--
-- The NEPacketTunnelProvider class declares the programmatic interface of an object that implements the client side of a custom IP packet tunneling protocol.
--
-- NEPacketTunnelProvider is part of NetworkExtension.framework.
-- 
-- Phantom type for @NEPacketTunnelProvider@.
data NEPacketTunnelProvider

instance IsObjCObject (Id NEPacketTunnelProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEPacketTunnelProvider"

class IsNETunnelProvider a => IsNEPacketTunnelProvider a where
  toNEPacketTunnelProvider :: a -> Id NEPacketTunnelProvider

instance IsNEPacketTunnelProvider (Id NEPacketTunnelProvider) where
  toNEPacketTunnelProvider = unsafeCastId

instance IsNEProvider (Id NEPacketTunnelProvider) where
  toNEProvider = unsafeCastId

instance IsNETunnelProvider (Id NEPacketTunnelProvider) where
  toNETunnelProvider = unsafeCastId

instance IsNSObject (Id NEPacketTunnelProvider) where
  toNSObject = unsafeCastId

-- ---------- NEEthernetTunnelNetworkSettings ----------

-- | NEEthernetTunnelNetworkSettings
--
-- The NEEthernetTunnelNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NEEthernetTunnelNetworkSettings is used by NEEthernetTunnelProviders to communicate the desired network settings for the packet tunnel to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEEthernetTunnelNetworkSettings@.
data NEEthernetTunnelNetworkSettings

instance IsObjCObject (Id NEEthernetTunnelNetworkSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEEthernetTunnelNetworkSettings"

class IsNEPacketTunnelNetworkSettings a => IsNEEthernetTunnelNetworkSettings a where
  toNEEthernetTunnelNetworkSettings :: a -> Id NEEthernetTunnelNetworkSettings

instance IsNEEthernetTunnelNetworkSettings (Id NEEthernetTunnelNetworkSettings) where
  toNEEthernetTunnelNetworkSettings = unsafeCastId

instance IsNEPacketTunnelNetworkSettings (Id NEEthernetTunnelNetworkSettings) where
  toNEPacketTunnelNetworkSettings = unsafeCastId

instance IsNETunnelNetworkSettings (Id NEEthernetTunnelNetworkSettings) where
  toNETunnelNetworkSettings = unsafeCastId

instance IsNSObject (Id NEEthernetTunnelNetworkSettings) where
  toNSObject = unsafeCastId

-- ---------- NEAppProxyProviderManager ----------

-- | NEAppProxyProviderManager
--
-- The NEAppProxyProviderManager class declares the programmatic interface for an object that is used to configure and control network tunnels provided by NEAppProxyProviders.
--
-- Instances of this class are thread safe.
-- 
-- Phantom type for @NEAppProxyProviderManager@.
data NEAppProxyProviderManager

instance IsObjCObject (Id NEAppProxyProviderManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEAppProxyProviderManager"

class IsNETunnelProviderManager a => IsNEAppProxyProviderManager a where
  toNEAppProxyProviderManager :: a -> Id NEAppProxyProviderManager

instance IsNEAppProxyProviderManager (Id NEAppProxyProviderManager) where
  toNEAppProxyProviderManager = unsafeCastId

instance IsNETunnelProviderManager (Id NEAppProxyProviderManager) where
  toNETunnelProviderManager = unsafeCastId

instance IsNEVPNManager (Id NEAppProxyProviderManager) where
  toNEVPNManager = unsafeCastId

instance IsNSObject (Id NEAppProxyProviderManager) where
  toNSObject = unsafeCastId

-- ---------- NEVPNProtocolIKEv2 ----------

-- | NEVPNProtocolIKEv2
--
-- The NEVPNProtocolIKEv2 class declares the programmatic interface of an object that manages the IKEv2-specific portion of a VPN configuration.
--
-- Instances of this class use IKE version 2 for key negotiation. Instances of this class are thread safe.
-- 
-- Phantom type for @NEVPNProtocolIKEv2@.
data NEVPNProtocolIKEv2

instance IsObjCObject (Id NEVPNProtocolIKEv2) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEVPNProtocolIKEv2"

class IsNEVPNProtocolIPSec a => IsNEVPNProtocolIKEv2 a where
  toNEVPNProtocolIKEv2 :: a -> Id NEVPNProtocolIKEv2

instance IsNEVPNProtocolIKEv2 (Id NEVPNProtocolIKEv2) where
  toNEVPNProtocolIKEv2 = unsafeCastId

instance IsNEVPNProtocol (Id NEVPNProtocolIKEv2) where
  toNEVPNProtocol = unsafeCastId

instance IsNEVPNProtocolIPSec (Id NEVPNProtocolIKEv2) where
  toNEVPNProtocolIPSec = unsafeCastId

instance IsNSObject (Id NEVPNProtocolIKEv2) where
  toNSObject = unsafeCastId

-- ---------- NETransparentProxyProvider ----------

-- | NETransparentProxyProvider
--
-- The NETransparentProxyProvider class declares the programmatic interface for an object that implements the client side of a custom transparent network proxy solution.     The NETransparentProxyProvider class has the following behavior differences from its super class NEAppProxyProvider:         - Returning NO from handleNewFlow: and handleNewUDPFlow:initialRemoteEndpoint: causes the flow to proceed to communicate directly with the flow's ultimate destination, instead of closing the flow with a "Connection Refused" error.         - NEDNSSettings and NEProxySettings specified within NETransparentProxyNetworkSettings are ignored. Flows that match the includedNetworkRules within NETransparentProxyNetworkSettings will use the same DNS and proxy settings that other flows on the system are currently using.         - Flows that are created using a "connect by name" API (such as Network.framework or NSURLSession) that match the includedNetworkRules will not bypass DNS resolution.
--
-- NETransparentProxyProvider is part of NetworkExtension.framework
-- 
-- Phantom type for @NETransparentProxyProvider@.
data NETransparentProxyProvider

instance IsObjCObject (Id NETransparentProxyProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NETransparentProxyProvider"

class IsNEAppProxyProvider a => IsNETransparentProxyProvider a where
  toNETransparentProxyProvider :: a -> Id NETransparentProxyProvider

instance IsNETransparentProxyProvider (Id NETransparentProxyProvider) where
  toNETransparentProxyProvider = unsafeCastId

instance IsNEAppProxyProvider (Id NETransparentProxyProvider) where
  toNEAppProxyProvider = unsafeCastId

instance IsNEProvider (Id NETransparentProxyProvider) where
  toNEProvider = unsafeCastId

instance IsNETunnelProvider (Id NETransparentProxyProvider) where
  toNETunnelProvider = unsafeCastId

instance IsNSObject (Id NETransparentProxyProvider) where
  toNSObject = unsafeCastId

-- ---------- NEEthernetTunnelProvider ----------

-- | NEEthernetTunnelProvider
--
-- The NEEthernetTunnelProvider class declares the programmatic interface of an object that implements the client side of a custom link-layer packet tunneling protocol.
--
-- NEEthernetTunnelProvider is part of NetworkExtension.framework.
-- 
-- Phantom type for @NEEthernetTunnelProvider@.
data NEEthernetTunnelProvider

instance IsObjCObject (Id NEEthernetTunnelProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NEEthernetTunnelProvider"

class IsNEPacketTunnelProvider a => IsNEEthernetTunnelProvider a where
  toNEEthernetTunnelProvider :: a -> Id NEEthernetTunnelProvider

instance IsNEEthernetTunnelProvider (Id NEEthernetTunnelProvider) where
  toNEEthernetTunnelProvider = unsafeCastId

instance IsNEPacketTunnelProvider (Id NEEthernetTunnelProvider) where
  toNEPacketTunnelProvider = unsafeCastId

instance IsNEProvider (Id NEEthernetTunnelProvider) where
  toNEProvider = unsafeCastId

instance IsNETunnelProvider (Id NEEthernetTunnelProvider) where
  toNETunnelProvider = unsafeCastId

instance IsNSObject (Id NEEthernetTunnelProvider) where
  toNSObject = unsafeCastId
