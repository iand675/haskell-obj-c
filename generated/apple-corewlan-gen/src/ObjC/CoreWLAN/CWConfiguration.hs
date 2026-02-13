{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Encapsulates the system configuration for a given Wi-Fi interface.
--
-- The CWConfiguration class contains basic network configuration settings and also the list of preferred networks. CWConfiguration is an immutable object. For changing configuration settings and/or the preferred networks list, see CWMutableConfiguration.
--
-- Generated bindings for @CWConfiguration@.
module ObjC.CoreWLAN.CWConfiguration
  ( CWConfiguration
  , IsCWConfiguration(..)
  , configuration
  , init_
  , initWithConfiguration
  , configurationWithConfiguration
  , isEqualToConfiguration
  , requireAdministratorForAssociation
  , requireAdministratorForPower
  , requireAdministratorForIBSSMode
  , rememberJoinedNetworks
  , configurationSelector
  , configurationWithConfigurationSelector
  , initSelector
  , initWithConfigurationSelector
  , isEqualToConfigurationSelector
  , rememberJoinedNetworksSelector
  , requireAdministratorForAssociationSelector
  , requireAdministratorForIBSSModeSelector
  , requireAdministratorForPowerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreWLAN.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Convenience method for getting a CWConfiguration object.
--
-- ObjC selector: @+ configuration@
configuration :: IO (Id CWConfiguration)
configuration  =
  do
    cls' <- getRequiredClass "CWConfiguration"
    sendClassMessage cls' configurationSelector

-- | Initializes a CWConfiguration object.
--
-- ObjC selector: @- init@
init_ :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO (Id CWConfiguration)
init_ cwConfiguration =
  sendOwnedMessage cwConfiguration initSelector

-- | @configuration@ — A CWConfiguration object.
--
-- Returns: A CWConfiguration object.
--
-- Initializes a CWConfiguration object with the properties of an existing CWConfiguration object.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsCWConfiguration cwConfiguration, IsCWConfiguration configuration) => cwConfiguration -> configuration -> IO (Id CWConfiguration)
initWithConfiguration cwConfiguration configuration =
  sendOwnedMessage cwConfiguration initWithConfigurationSelector (toCWConfiguration configuration)

-- | @configuration@ — A CWConfiguration object.
--
-- Returns: A CWConfiguration object.
--
-- Convenience method for getting a CWConfiguration object initialized with the properties of an existing CWConfiguration object.
--
-- ObjC selector: @+ configurationWithConfiguration:@
configurationWithConfiguration :: IsCWConfiguration configuration => configuration -> IO (Id CWConfiguration)
configurationWithConfiguration configuration =
  do
    cls' <- getRequiredClass "CWConfiguration"
    sendClassMessage cls' configurationWithConfigurationSelector (toCWConfiguration configuration)

-- | @configuration@ — The CWConfiguration with which to compare the receiver.
--
-- Returns: YES if the objects are equal, NO otherwise.
--
-- Determine CWConfiguration equality.
--
-- CWConfiguration objects are considered equal if all their corresponding properties are equal.
--
-- ObjC selector: @- isEqualToConfiguration:@
isEqualToConfiguration :: (IsCWConfiguration cwConfiguration, IsCWConfiguration configuration) => cwConfiguration -> configuration -> IO Bool
isEqualToConfiguration cwConfiguration configuration =
  sendMessage cwConfiguration isEqualToConfigurationSelector (toCWConfiguration configuration)

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to require an administrator password to change networks.
--
-- If YES, the user may be prompted to enter an administrator password upon attempting to join a Wi-Fi network. This preference is enforced at the API layer.
--
-- ObjC selector: @- requireAdministratorForAssociation@
requireAdministratorForAssociation :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
requireAdministratorForAssociation cwConfiguration =
  sendMessage cwConfiguration requireAdministratorForAssociationSelector

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to require an administrator password to change the interface power state.
--
-- If YES, the user may be prompted to enter an administrator password upon attempting to turn Wi-Fi on or off. This preference is enforced at the API layer.
--
-- ObjC selector: @- requireAdministratorForPower@
requireAdministratorForPower :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
requireAdministratorForPower cwConfiguration =
  sendMessage cwConfiguration requireAdministratorForPowerSelector

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to require an administrator password to create a computer-to-computer network.
--
-- If YES, the user may be prompted to enter an administrator password upon attempting to create an IBSS network. This preference is enforced at the API layer.
--
-- ObjC selector: @- requireAdministratorForIBSSMode@
requireAdministratorForIBSSMode :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
requireAdministratorForIBSSMode cwConfiguration =
  sendMessage cwConfiguration requireAdministratorForIBSSModeSelector

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to remember all Wi-Fi networks joined unless otherwise specified by the user when joining a particular Wi-Fi network.
--
-- ObjC selector: @- rememberJoinedNetworks@
rememberJoinedNetworks :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
rememberJoinedNetworks cwConfiguration =
  sendMessage cwConfiguration rememberJoinedNetworksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id CWConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CWConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector '[Id CWConfiguration] (Id CWConfiguration)
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @configurationWithConfiguration:@
configurationWithConfigurationSelector :: Selector '[Id CWConfiguration] (Id CWConfiguration)
configurationWithConfigurationSelector = mkSelector "configurationWithConfiguration:"

-- | @Selector@ for @isEqualToConfiguration:@
isEqualToConfigurationSelector :: Selector '[Id CWConfiguration] Bool
isEqualToConfigurationSelector = mkSelector "isEqualToConfiguration:"

-- | @Selector@ for @requireAdministratorForAssociation@
requireAdministratorForAssociationSelector :: Selector '[] Bool
requireAdministratorForAssociationSelector = mkSelector "requireAdministratorForAssociation"

-- | @Selector@ for @requireAdministratorForPower@
requireAdministratorForPowerSelector :: Selector '[] Bool
requireAdministratorForPowerSelector = mkSelector "requireAdministratorForPower"

-- | @Selector@ for @requireAdministratorForIBSSMode@
requireAdministratorForIBSSModeSelector :: Selector '[] Bool
requireAdministratorForIBSSModeSelector = mkSelector "requireAdministratorForIBSSMode"

-- | @Selector@ for @rememberJoinedNetworks@
rememberJoinedNetworksSelector :: Selector '[] Bool
rememberJoinedNetworksSelector = mkSelector "rememberJoinedNetworks"

