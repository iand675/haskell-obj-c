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
  , initSelector
  , initWithConfigurationSelector
  , configurationWithConfigurationSelector
  , isEqualToConfigurationSelector
  , requireAdministratorForAssociationSelector
  , requireAdministratorForPowerSelector
  , requireAdministratorForIBSSModeSelector
  , rememberJoinedNetworksSelector


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

import ObjC.CoreWLAN.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Convenience method for getting a CWConfiguration object.
--
-- ObjC selector: @+ configuration@
configuration :: IO (Id CWConfiguration)
configuration  =
  do
    cls' <- getRequiredClass "CWConfiguration"
    sendClassMsg cls' (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Initializes a CWConfiguration object.
--
-- ObjC selector: @- init@
init_ :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO (Id CWConfiguration)
init_ cwConfiguration  =
  sendMsg cwConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @configuration@ — A CWConfiguration object.
--
-- Returns: A CWConfiguration object.
--
-- Initializes a CWConfiguration object with the properties of an existing CWConfiguration object.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsCWConfiguration cwConfiguration, IsCWConfiguration configuration) => cwConfiguration -> configuration -> IO (Id CWConfiguration)
initWithConfiguration cwConfiguration  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg cwConfiguration (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr configuration $ \raw_configuration ->
      sendClassMsg cls' (mkSelector "configurationWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= retainedObject . castPtr

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
isEqualToConfiguration cwConfiguration  configuration =
withObjCPtr configuration $ \raw_configuration ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwConfiguration (mkSelector "isEqualToConfiguration:") retCULong [argPtr (castPtr raw_configuration :: Ptr ())]

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to require an administrator password to change networks.
--
-- If YES, the user may be prompted to enter an administrator password upon attempting to join a Wi-Fi network. This preference is enforced at the API layer.
--
-- ObjC selector: @- requireAdministratorForAssociation@
requireAdministratorForAssociation :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
requireAdministratorForAssociation cwConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwConfiguration (mkSelector "requireAdministratorForAssociation") retCULong []

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to require an administrator password to change the interface power state.
--
-- If YES, the user may be prompted to enter an administrator password upon attempting to turn Wi-Fi on or off. This preference is enforced at the API layer.
--
-- ObjC selector: @- requireAdministratorForPower@
requireAdministratorForPower :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
requireAdministratorForPower cwConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwConfiguration (mkSelector "requireAdministratorForPower") retCULong []

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to require an administrator password to create a computer-to-computer network.
--
-- If YES, the user may be prompted to enter an administrator password upon attempting to create an IBSS network. This preference is enforced at the API layer.
--
-- ObjC selector: @- requireAdministratorForIBSSMode@
requireAdministratorForIBSSMode :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
requireAdministratorForIBSSMode cwConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwConfiguration (mkSelector "requireAdministratorForIBSSMode") retCULong []

-- | Returns: YES if the preference is enabled, NO otherwise.
--
-- Returns the preference to remember all Wi-Fi networks joined unless otherwise specified by the user when joining a particular Wi-Fi network.
--
-- ObjC selector: @- rememberJoinedNetworks@
rememberJoinedNetworks :: IsCWConfiguration cwConfiguration => cwConfiguration -> IO Bool
rememberJoinedNetworks cwConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwConfiguration (mkSelector "rememberJoinedNetworks") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @configurationWithConfiguration:@
configurationWithConfigurationSelector :: Selector
configurationWithConfigurationSelector = mkSelector "configurationWithConfiguration:"

-- | @Selector@ for @isEqualToConfiguration:@
isEqualToConfigurationSelector :: Selector
isEqualToConfigurationSelector = mkSelector "isEqualToConfiguration:"

-- | @Selector@ for @requireAdministratorForAssociation@
requireAdministratorForAssociationSelector :: Selector
requireAdministratorForAssociationSelector = mkSelector "requireAdministratorForAssociation"

-- | @Selector@ for @requireAdministratorForPower@
requireAdministratorForPowerSelector :: Selector
requireAdministratorForPowerSelector = mkSelector "requireAdministratorForPower"

-- | @Selector@ for @requireAdministratorForIBSSMode@
requireAdministratorForIBSSModeSelector :: Selector
requireAdministratorForIBSSModeSelector = mkSelector "requireAdministratorForIBSSMode"

-- | @Selector@ for @rememberJoinedNetworks@
rememberJoinedNetworksSelector :: Selector
rememberJoinedNetworksSelector = mkSelector "rememberJoinedNetworks"

