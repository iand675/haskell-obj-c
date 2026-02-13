{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEVPNManager
--
-- The NEVPNManager class declares the programmatic interface for an object that manages Virtual Private Network (VPN) configurations.
--
-- NEVPNManager declares methods and properties for configuring and controlling a VPN.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEVPNManager@.
module ObjC.NetworkExtension.NEVPNManager
  ( NEVPNManager
  , IsNEVPNManager(..)
  , sharedManager
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , setAuthorization
  , onDemandRules
  , setOnDemandRules
  , onDemandEnabled
  , setOnDemandEnabled
  , localizedDescription
  , setLocalizedDescription
  , protocol
  , setProtocol
  , protocolConfiguration
  , setProtocolConfiguration
  , connection
  , enabled
  , setEnabled
  , connectionSelector
  , enabledSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , onDemandEnabledSelector
  , onDemandRulesSelector
  , protocolConfigurationSelector
  , protocolSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setAuthorizationSelector
  , setEnabledSelector
  , setLocalizedDescriptionSelector
  , setOnDemandEnabledSelector
  , setOnDemandRulesSelector
  , setProtocolConfigurationSelector
  , setProtocolSelector
  , sharedManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sharedManager
--
-- Returns: The singleton NEVPNManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NEVPNManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEVPNManager"
    sendClassMessage cls' sharedManagerSelector

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current VPN configuration from the caller's VPN preferences.
--
-- @completionHandler@ — A block that will be called on the main thread when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEVPNManager nevpnManager => nevpnManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler nevpnManager completionHandler =
  sendMessage nevpnManager loadFromPreferencesWithCompletionHandlerSelector completionHandler

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the VPN configuration from the caller's VPN preferences. If the VPN is enabled, has VPN On Demand enabled, and has VPN On Demand rules, the VPN is disabled and the VPN On Demand rules are de-activated.
--
-- @completionHandler@ — A block that will be called on the main thread when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEVPNManager nevpnManager => nevpnManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler nevpnManager completionHandler =
  sendMessage nevpnManager removeFromPreferencesWithCompletionHandlerSelector completionHandler

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the VPN configuration in the caller's VPN preferences. If the VPN is enabled, has VPN On Demand enabled, and has VPN On Demand rules, the VPN On Demand rules are activated.
--
-- @completionHandler@ — A block that will be called on the main thread when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEVPNManager nevpnManager => nevpnManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler nevpnManager completionHandler =
  sendMessage nevpnManager saveToPreferencesWithCompletionHandlerSelector completionHandler

-- | setAuthorization:
--
-- This function sets an authorization object that can be used to obtain the authorization rights necessary to modify the system VPN configuration.
--
-- @authorization@ — The AuthorizationRef to use to obtain rights.
--
-- ObjC selector: @- setAuthorization:@
setAuthorization :: IsNEVPNManager nevpnManager => nevpnManager -> RawId -> IO ()
setAuthorization nevpnManager authorization =
  sendMessage nevpnManager setAuthorizationSelector authorization

-- | onDemandRules
--
-- An array of NEOnDemandRule objects.
--
-- ObjC selector: @- onDemandRules@
onDemandRules :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NSArray)
onDemandRules nevpnManager =
  sendMessage nevpnManager onDemandRulesSelector

-- | onDemandRules
--
-- An array of NEOnDemandRule objects.
--
-- ObjC selector: @- setOnDemandRules:@
setOnDemandRules :: (IsNEVPNManager nevpnManager, IsNSArray value) => nevpnManager -> value -> IO ()
setOnDemandRules nevpnManager value =
  sendMessage nevpnManager setOnDemandRulesSelector (toNSArray value)

-- | onDemandEnabled
--
-- Toggles VPN On Demand.
--
-- ObjC selector: @- onDemandEnabled@
onDemandEnabled :: IsNEVPNManager nevpnManager => nevpnManager -> IO Bool
onDemandEnabled nevpnManager =
  sendMessage nevpnManager onDemandEnabledSelector

-- | onDemandEnabled
--
-- Toggles VPN On Demand.
--
-- ObjC selector: @- setOnDemandEnabled:@
setOnDemandEnabled :: IsNEVPNManager nevpnManager => nevpnManager -> Bool -> IO ()
setOnDemandEnabled nevpnManager value =
  sendMessage nevpnManager setOnDemandEnabledSelector value

-- | localizedDescription
--
-- A string containing a description of the VPN.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NSString)
localizedDescription nevpnManager =
  sendMessage nevpnManager localizedDescriptionSelector

-- | localizedDescription
--
-- A string containing a description of the VPN.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEVPNManager nevpnManager, IsNSString value) => nevpnManager -> value -> IO ()
setLocalizedDescription nevpnManager value =
  sendMessage nevpnManager setLocalizedDescriptionSelector (toNSString value)

-- | protocol
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- protocol@
protocol :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NEVPNProtocol)
protocol nevpnManager =
  sendMessage nevpnManager protocolSelector

-- | protocol
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- setProtocol:@
setProtocol :: (IsNEVPNManager nevpnManager, IsNEVPNProtocol value) => nevpnManager -> value -> IO ()
setProtocol nevpnManager value =
  sendMessage nevpnManager setProtocolSelector (toNEVPNProtocol value)

-- | protocolConfiguration
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- protocolConfiguration@
protocolConfiguration :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NEVPNProtocol)
protocolConfiguration nevpnManager =
  sendMessage nevpnManager protocolConfigurationSelector

-- | protocolConfiguration
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- setProtocolConfiguration:@
setProtocolConfiguration :: (IsNEVPNManager nevpnManager, IsNEVPNProtocol value) => nevpnManager -> value -> IO ()
setProtocolConfiguration nevpnManager value =
  sendMessage nevpnManager setProtocolConfigurationSelector (toNEVPNProtocol value)

-- | connection
--
-- The NEVPNConnection object used for controlling the VPN tunnel.
--
-- ObjC selector: @- connection@
connection :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NEVPNConnection)
connection nevpnManager =
  sendMessage nevpnManager connectionSelector

-- | enabled
--
-- Toggles the enabled status of the VPN. Setting this property will disable VPN configurations of other apps. This property will be set to NO  when other VPN configurations are enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsNEVPNManager nevpnManager => nevpnManager -> IO Bool
enabled nevpnManager =
  sendMessage nevpnManager enabledSelector

-- | enabled
--
-- Toggles the enabled status of the VPN. Setting this property will disable VPN configurations of other apps. This property will be set to NO  when other VPN configurations are enabled.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEVPNManager nevpnManager => nevpnManager -> Bool -> IO ()
setEnabled nevpnManager value =
  sendMessage nevpnManager setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id NEVPNManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @setAuthorization:@
setAuthorizationSelector :: Selector '[RawId] ()
setAuthorizationSelector = mkSelector "setAuthorization:"

-- | @Selector@ for @onDemandRules@
onDemandRulesSelector :: Selector '[] (Id NSArray)
onDemandRulesSelector = mkSelector "onDemandRules"

-- | @Selector@ for @setOnDemandRules:@
setOnDemandRulesSelector :: Selector '[Id NSArray] ()
setOnDemandRulesSelector = mkSelector "setOnDemandRules:"

-- | @Selector@ for @onDemandEnabled@
onDemandEnabledSelector :: Selector '[] Bool
onDemandEnabledSelector = mkSelector "onDemandEnabled"

-- | @Selector@ for @setOnDemandEnabled:@
setOnDemandEnabledSelector :: Selector '[Bool] ()
setOnDemandEnabledSelector = mkSelector "setOnDemandEnabled:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] (Id NEVPNProtocol)
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @setProtocol:@
setProtocolSelector :: Selector '[Id NEVPNProtocol] ()
setProtocolSelector = mkSelector "setProtocol:"

-- | @Selector@ for @protocolConfiguration@
protocolConfigurationSelector :: Selector '[] (Id NEVPNProtocol)
protocolConfigurationSelector = mkSelector "protocolConfiguration"

-- | @Selector@ for @setProtocolConfiguration:@
setProtocolConfigurationSelector :: Selector '[Id NEVPNProtocol] ()
setProtocolConfigurationSelector = mkSelector "setProtocolConfiguration:"

-- | @Selector@ for @connection@
connectionSelector :: Selector '[] (Id NEVPNConnection)
connectionSelector = mkSelector "connection"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

