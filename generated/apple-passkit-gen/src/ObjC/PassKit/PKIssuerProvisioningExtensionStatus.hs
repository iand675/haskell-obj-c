{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIssuerProvisioningExtensionStatus@.
module ObjC.PassKit.PKIssuerProvisioningExtensionStatus
  ( PKIssuerProvisioningExtensionStatus
  , IsPKIssuerProvisioningExtensionStatus(..)
  , init_
  , requiresAuthentication
  , setRequiresAuthentication
  , passEntriesAvailable
  , setPassEntriesAvailable
  , remotePassEntriesAvailable
  , setRemotePassEntriesAvailable
  , initSelector
  , passEntriesAvailableSelector
  , remotePassEntriesAvailableSelector
  , requiresAuthenticationSelector
  , setPassEntriesAvailableSelector
  , setRemotePassEntriesAvailableSelector
  , setRequiresAuthenticationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO (Id PKIssuerProvisioningExtensionStatus)
init_ pkIssuerProvisioningExtensionStatus =
  sendOwnedMessage pkIssuerProvisioningExtensionStatus initSelector

-- | @- requiresAuthentication@
requiresAuthentication :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO Bool
requiresAuthentication pkIssuerProvisioningExtensionStatus =
  sendMessage pkIssuerProvisioningExtensionStatus requiresAuthenticationSelector

-- | @- setRequiresAuthentication:@
setRequiresAuthentication :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> Bool -> IO ()
setRequiresAuthentication pkIssuerProvisioningExtensionStatus value =
  sendMessage pkIssuerProvisioningExtensionStatus setRequiresAuthenticationSelector value

-- | @- passEntriesAvailable@
passEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO Bool
passEntriesAvailable pkIssuerProvisioningExtensionStatus =
  sendMessage pkIssuerProvisioningExtensionStatus passEntriesAvailableSelector

-- | @- setPassEntriesAvailable:@
setPassEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> Bool -> IO ()
setPassEntriesAvailable pkIssuerProvisioningExtensionStatus value =
  sendMessage pkIssuerProvisioningExtensionStatus setPassEntriesAvailableSelector value

-- | @- remotePassEntriesAvailable@
remotePassEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO Bool
remotePassEntriesAvailable pkIssuerProvisioningExtensionStatus =
  sendMessage pkIssuerProvisioningExtensionStatus remotePassEntriesAvailableSelector

-- | @- setRemotePassEntriesAvailable:@
setRemotePassEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> Bool -> IO ()
setRemotePassEntriesAvailable pkIssuerProvisioningExtensionStatus value =
  sendMessage pkIssuerProvisioningExtensionStatus setRemotePassEntriesAvailableSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIssuerProvisioningExtensionStatus)
initSelector = mkSelector "init"

-- | @Selector@ for @requiresAuthentication@
requiresAuthenticationSelector :: Selector '[] Bool
requiresAuthenticationSelector = mkSelector "requiresAuthentication"

-- | @Selector@ for @setRequiresAuthentication:@
setRequiresAuthenticationSelector :: Selector '[Bool] ()
setRequiresAuthenticationSelector = mkSelector "setRequiresAuthentication:"

-- | @Selector@ for @passEntriesAvailable@
passEntriesAvailableSelector :: Selector '[] Bool
passEntriesAvailableSelector = mkSelector "passEntriesAvailable"

-- | @Selector@ for @setPassEntriesAvailable:@
setPassEntriesAvailableSelector :: Selector '[Bool] ()
setPassEntriesAvailableSelector = mkSelector "setPassEntriesAvailable:"

-- | @Selector@ for @remotePassEntriesAvailable@
remotePassEntriesAvailableSelector :: Selector '[] Bool
remotePassEntriesAvailableSelector = mkSelector "remotePassEntriesAvailable"

-- | @Selector@ for @setRemotePassEntriesAvailable:@
setRemotePassEntriesAvailableSelector :: Selector '[Bool] ()
setRemotePassEntriesAvailableSelector = mkSelector "setRemotePassEntriesAvailable:"

