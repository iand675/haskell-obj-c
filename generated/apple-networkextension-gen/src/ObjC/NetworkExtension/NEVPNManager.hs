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
  , sharedManagerSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setAuthorizationSelector
  , onDemandRulesSelector
  , setOnDemandRulesSelector
  , onDemandEnabledSelector
  , setOnDemandEnabledSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , protocolSelector
  , setProtocolSelector
  , protocolConfigurationSelector
  , setProtocolConfigurationSelector
  , connectionSelector
  , enabledSelector
  , setEnabledSelector


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

-- | sharedManager
--
-- Returns: The singleton NEVPNManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NEVPNManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEVPNManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current VPN configuration from the caller's VPN preferences.
--
-- @completionHandler@ — A block that will be called on the main thread when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEVPNManager nevpnManager => nevpnManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler nevpnManager  completionHandler =
    sendMsg nevpnManager (mkSelector "loadFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the VPN configuration from the caller's VPN preferences. If the VPN is enabled, has VPN On Demand enabled, and has VPN On Demand rules, the VPN is disabled and the VPN On Demand rules are de-activated.
--
-- @completionHandler@ — A block that will be called on the main thread when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEVPNManager nevpnManager => nevpnManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler nevpnManager  completionHandler =
    sendMsg nevpnManager (mkSelector "removeFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the VPN configuration in the caller's VPN preferences. If the VPN is enabled, has VPN On Demand enabled, and has VPN On Demand rules, the VPN On Demand rules are activated.
--
-- @completionHandler@ — A block that will be called on the main thread when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEVPNManager nevpnManager => nevpnManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler nevpnManager  completionHandler =
    sendMsg nevpnManager (mkSelector "saveToPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | setAuthorization:
--
-- This function sets an authorization object that can be used to obtain the authorization rights necessary to modify the system VPN configuration.
--
-- @authorization@ — The AuthorizationRef to use to obtain rights.
--
-- ObjC selector: @- setAuthorization:@
setAuthorization :: IsNEVPNManager nevpnManager => nevpnManager -> RawId -> IO ()
setAuthorization nevpnManager  authorization =
    sendMsg nevpnManager (mkSelector "setAuthorization:") retVoid [argPtr (castPtr (unRawId authorization) :: Ptr ())]

-- | onDemandRules
--
-- An array of NEOnDemandRule objects.
--
-- ObjC selector: @- onDemandRules@
onDemandRules :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NSArray)
onDemandRules nevpnManager  =
    sendMsg nevpnManager (mkSelector "onDemandRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | onDemandRules
--
-- An array of NEOnDemandRule objects.
--
-- ObjC selector: @- setOnDemandRules:@
setOnDemandRules :: (IsNEVPNManager nevpnManager, IsNSArray value) => nevpnManager -> value -> IO ()
setOnDemandRules nevpnManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnManager (mkSelector "setOnDemandRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | onDemandEnabled
--
-- Toggles VPN On Demand.
--
-- ObjC selector: @- onDemandEnabled@
onDemandEnabled :: IsNEVPNManager nevpnManager => nevpnManager -> IO Bool
onDemandEnabled nevpnManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnManager (mkSelector "onDemandEnabled") retCULong []

-- | onDemandEnabled
--
-- Toggles VPN On Demand.
--
-- ObjC selector: @- setOnDemandEnabled:@
setOnDemandEnabled :: IsNEVPNManager nevpnManager => nevpnManager -> Bool -> IO ()
setOnDemandEnabled nevpnManager  value =
    sendMsg nevpnManager (mkSelector "setOnDemandEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | localizedDescription
--
-- A string containing a description of the VPN.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NSString)
localizedDescription nevpnManager  =
    sendMsg nevpnManager (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedDescription
--
-- A string containing a description of the VPN.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEVPNManager nevpnManager, IsNSString value) => nevpnManager -> value -> IO ()
setLocalizedDescription nevpnManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnManager (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | protocol
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- protocol@
protocol :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NEVPNProtocol)
protocol nevpnManager  =
    sendMsg nevpnManager (mkSelector "protocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | protocol
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- setProtocol:@
setProtocol :: (IsNEVPNManager nevpnManager, IsNEVPNProtocol value) => nevpnManager -> value -> IO ()
setProtocol nevpnManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnManager (mkSelector "setProtocol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | protocolConfiguration
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- protocolConfiguration@
protocolConfiguration :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NEVPNProtocol)
protocolConfiguration nevpnManager  =
    sendMsg nevpnManager (mkSelector "protocolConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | protocolConfiguration
--
-- An NEVPNProtocol object containing the protocol-specific portion of the VPN configuration.
--
-- ObjC selector: @- setProtocolConfiguration:@
setProtocolConfiguration :: (IsNEVPNManager nevpnManager, IsNEVPNProtocol value) => nevpnManager -> value -> IO ()
setProtocolConfiguration nevpnManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnManager (mkSelector "setProtocolConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | connection
--
-- The NEVPNConnection object used for controlling the VPN tunnel.
--
-- ObjC selector: @- connection@
connection :: IsNEVPNManager nevpnManager => nevpnManager -> IO (Id NEVPNConnection)
connection nevpnManager  =
    sendMsg nevpnManager (mkSelector "connection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | enabled
--
-- Toggles the enabled status of the VPN. Setting this property will disable VPN configurations of other apps. This property will be set to NO  when other VPN configurations are enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsNEVPNManager nevpnManager => nevpnManager -> IO Bool
enabled nevpnManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnManager (mkSelector "enabled") retCULong []

-- | enabled
--
-- Toggles the enabled status of the VPN. Setting this property will disable VPN configurations of other apps. This property will be set to NO  when other VPN configurations are enabled.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEVPNManager nevpnManager => nevpnManager -> Bool -> IO ()
setEnabled nevpnManager  value =
    sendMsg nevpnManager (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @setAuthorization:@
setAuthorizationSelector :: Selector
setAuthorizationSelector = mkSelector "setAuthorization:"

-- | @Selector@ for @onDemandRules@
onDemandRulesSelector :: Selector
onDemandRulesSelector = mkSelector "onDemandRules"

-- | @Selector@ for @setOnDemandRules:@
setOnDemandRulesSelector :: Selector
setOnDemandRulesSelector = mkSelector "setOnDemandRules:"

-- | @Selector@ for @onDemandEnabled@
onDemandEnabledSelector :: Selector
onDemandEnabledSelector = mkSelector "onDemandEnabled"

-- | @Selector@ for @setOnDemandEnabled:@
setOnDemandEnabledSelector :: Selector
setOnDemandEnabledSelector = mkSelector "setOnDemandEnabled:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @setProtocol:@
setProtocolSelector :: Selector
setProtocolSelector = mkSelector "setProtocol:"

-- | @Selector@ for @protocolConfiguration@
protocolConfigurationSelector :: Selector
protocolConfigurationSelector = mkSelector "protocolConfiguration"

-- | @Selector@ for @setProtocolConfiguration:@
setProtocolConfigurationSelector :: Selector
setProtocolConfigurationSelector = mkSelector "setProtocolConfiguration:"

-- | @Selector@ for @connection@
connectionSelector :: Selector
connectionSelector = mkSelector "connection"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

