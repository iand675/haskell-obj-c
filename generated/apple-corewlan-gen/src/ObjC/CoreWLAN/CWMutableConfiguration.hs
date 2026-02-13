{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Mutable subclass of CWConfiguration.  Use this class for changing configuration settings and/or the preferred networks list.
--
-- To commit configuration changes, use -[CWInterface commitConfiguration:authorization:error:].
--
-- Generated bindings for @CWMutableConfiguration@.
module ObjC.CoreWLAN.CWMutableConfiguration
  ( CWMutableConfiguration
  , IsCWMutableConfiguration(..)
  , requireAdministratorForAssociation
  , setRequireAdministratorForAssociation
  , requireAdministratorForPower
  , setRequireAdministratorForPower
  , requireAdministratorForIBSSMode
  , setRequireAdministratorForIBSSMode
  , rememberJoinedNetworks
  , setRememberJoinedNetworks
  , rememberJoinedNetworksSelector
  , requireAdministratorForAssociationSelector
  , requireAdministratorForIBSSModeSelector
  , requireAdministratorForPowerSelector
  , setRememberJoinedNetworksSelector
  , setRequireAdministratorForAssociationSelector
  , setRequireAdministratorForIBSSModeSelector
  , setRequireAdministratorForPowerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreWLAN.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- requireAdministratorForAssociation@
requireAdministratorForAssociation :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
requireAdministratorForAssociation cwMutableConfiguration =
  sendMessage cwMutableConfiguration requireAdministratorForAssociationSelector

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- setRequireAdministratorForAssociation:@
setRequireAdministratorForAssociation :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRequireAdministratorForAssociation cwMutableConfiguration value =
  sendMessage cwMutableConfiguration setRequireAdministratorForAssociationSelector value

-- | Set the preference to require an administrator password to change the interface power state.
--
-- ObjC selector: @- requireAdministratorForPower@
requireAdministratorForPower :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
requireAdministratorForPower cwMutableConfiguration =
  sendMessage cwMutableConfiguration requireAdministratorForPowerSelector

-- | Set the preference to require an administrator password to change the interface power state.
--
-- ObjC selector: @- setRequireAdministratorForPower:@
setRequireAdministratorForPower :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRequireAdministratorForPower cwMutableConfiguration value =
  sendMessage cwMutableConfiguration setRequireAdministratorForPowerSelector value

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- requireAdministratorForIBSSMode@
requireAdministratorForIBSSMode :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
requireAdministratorForIBSSMode cwMutableConfiguration =
  sendMessage cwMutableConfiguration requireAdministratorForIBSSModeSelector

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- setRequireAdministratorForIBSSMode:@
setRequireAdministratorForIBSSMode :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRequireAdministratorForIBSSMode cwMutableConfiguration value =
  sendMessage cwMutableConfiguration setRequireAdministratorForIBSSModeSelector value

-- | Set the preference to require an administrator password to create a computer-to-computer network.
--
-- ObjC selector: @- rememberJoinedNetworks@
rememberJoinedNetworks :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
rememberJoinedNetworks cwMutableConfiguration =
  sendMessage cwMutableConfiguration rememberJoinedNetworksSelector

-- | Set the preference to require an administrator password to create a computer-to-computer network.
--
-- ObjC selector: @- setRememberJoinedNetworks:@
setRememberJoinedNetworks :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRememberJoinedNetworks cwMutableConfiguration value =
  sendMessage cwMutableConfiguration setRememberJoinedNetworksSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requireAdministratorForAssociation@
requireAdministratorForAssociationSelector :: Selector '[] Bool
requireAdministratorForAssociationSelector = mkSelector "requireAdministratorForAssociation"

-- | @Selector@ for @setRequireAdministratorForAssociation:@
setRequireAdministratorForAssociationSelector :: Selector '[Bool] ()
setRequireAdministratorForAssociationSelector = mkSelector "setRequireAdministratorForAssociation:"

-- | @Selector@ for @requireAdministratorForPower@
requireAdministratorForPowerSelector :: Selector '[] Bool
requireAdministratorForPowerSelector = mkSelector "requireAdministratorForPower"

-- | @Selector@ for @setRequireAdministratorForPower:@
setRequireAdministratorForPowerSelector :: Selector '[Bool] ()
setRequireAdministratorForPowerSelector = mkSelector "setRequireAdministratorForPower:"

-- | @Selector@ for @requireAdministratorForIBSSMode@
requireAdministratorForIBSSModeSelector :: Selector '[] Bool
requireAdministratorForIBSSModeSelector = mkSelector "requireAdministratorForIBSSMode"

-- | @Selector@ for @setRequireAdministratorForIBSSMode:@
setRequireAdministratorForIBSSModeSelector :: Selector '[Bool] ()
setRequireAdministratorForIBSSModeSelector = mkSelector "setRequireAdministratorForIBSSMode:"

-- | @Selector@ for @rememberJoinedNetworks@
rememberJoinedNetworksSelector :: Selector '[] Bool
rememberJoinedNetworksSelector = mkSelector "rememberJoinedNetworks"

-- | @Selector@ for @setRememberJoinedNetworks:@
setRememberJoinedNetworksSelector :: Selector '[Bool] ()
setRememberJoinedNetworksSelector = mkSelector "setRememberJoinedNetworks:"

