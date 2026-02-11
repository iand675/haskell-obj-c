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
  , requireAdministratorForAssociationSelector
  , setRequireAdministratorForAssociationSelector
  , requireAdministratorForPowerSelector
  , setRequireAdministratorForPowerSelector
  , requireAdministratorForIBSSModeSelector
  , setRequireAdministratorForIBSSModeSelector
  , rememberJoinedNetworksSelector
  , setRememberJoinedNetworksSelector


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

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- requireAdministratorForAssociation@
requireAdministratorForAssociation :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
requireAdministratorForAssociation cwMutableConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwMutableConfiguration (mkSelector "requireAdministratorForAssociation") retCULong []

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- setRequireAdministratorForAssociation:@
setRequireAdministratorForAssociation :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRequireAdministratorForAssociation cwMutableConfiguration  value =
  sendMsg cwMutableConfiguration (mkSelector "setRequireAdministratorForAssociation:") retVoid [argCULong (if value then 1 else 0)]

-- | Set the preference to require an administrator password to change the interface power state.
--
-- ObjC selector: @- requireAdministratorForPower@
requireAdministratorForPower :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
requireAdministratorForPower cwMutableConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwMutableConfiguration (mkSelector "requireAdministratorForPower") retCULong []

-- | Set the preference to require an administrator password to change the interface power state.
--
-- ObjC selector: @- setRequireAdministratorForPower:@
setRequireAdministratorForPower :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRequireAdministratorForPower cwMutableConfiguration  value =
  sendMsg cwMutableConfiguration (mkSelector "setRequireAdministratorForPower:") retVoid [argCULong (if value then 1 else 0)]

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- requireAdministratorForIBSSMode@
requireAdministratorForIBSSMode :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
requireAdministratorForIBSSMode cwMutableConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwMutableConfiguration (mkSelector "requireAdministratorForIBSSMode") retCULong []

-- | Set the preference to require an administrator password to change networks.
--
-- ObjC selector: @- setRequireAdministratorForIBSSMode:@
setRequireAdministratorForIBSSMode :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRequireAdministratorForIBSSMode cwMutableConfiguration  value =
  sendMsg cwMutableConfiguration (mkSelector "setRequireAdministratorForIBSSMode:") retVoid [argCULong (if value then 1 else 0)]

-- | Set the preference to require an administrator password to create a computer-to-computer network.
--
-- ObjC selector: @- rememberJoinedNetworks@
rememberJoinedNetworks :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> IO Bool
rememberJoinedNetworks cwMutableConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwMutableConfiguration (mkSelector "rememberJoinedNetworks") retCULong []

-- | Set the preference to require an administrator password to create a computer-to-computer network.
--
-- ObjC selector: @- setRememberJoinedNetworks:@
setRememberJoinedNetworks :: IsCWMutableConfiguration cwMutableConfiguration => cwMutableConfiguration -> Bool -> IO ()
setRememberJoinedNetworks cwMutableConfiguration  value =
  sendMsg cwMutableConfiguration (mkSelector "setRememberJoinedNetworks:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requireAdministratorForAssociation@
requireAdministratorForAssociationSelector :: Selector
requireAdministratorForAssociationSelector = mkSelector "requireAdministratorForAssociation"

-- | @Selector@ for @setRequireAdministratorForAssociation:@
setRequireAdministratorForAssociationSelector :: Selector
setRequireAdministratorForAssociationSelector = mkSelector "setRequireAdministratorForAssociation:"

-- | @Selector@ for @requireAdministratorForPower@
requireAdministratorForPowerSelector :: Selector
requireAdministratorForPowerSelector = mkSelector "requireAdministratorForPower"

-- | @Selector@ for @setRequireAdministratorForPower:@
setRequireAdministratorForPowerSelector :: Selector
setRequireAdministratorForPowerSelector = mkSelector "setRequireAdministratorForPower:"

-- | @Selector@ for @requireAdministratorForIBSSMode@
requireAdministratorForIBSSModeSelector :: Selector
requireAdministratorForIBSSModeSelector = mkSelector "requireAdministratorForIBSSMode"

-- | @Selector@ for @setRequireAdministratorForIBSSMode:@
setRequireAdministratorForIBSSModeSelector :: Selector
setRequireAdministratorForIBSSModeSelector = mkSelector "setRequireAdministratorForIBSSMode:"

-- | @Selector@ for @rememberJoinedNetworks@
rememberJoinedNetworksSelector :: Selector
rememberJoinedNetworksSelector = mkSelector "rememberJoinedNetworks"

-- | @Selector@ for @setRememberJoinedNetworks:@
setRememberJoinedNetworksSelector :: Selector
setRememberJoinedNetworksSelector = mkSelector "setRememberJoinedNetworks:"

