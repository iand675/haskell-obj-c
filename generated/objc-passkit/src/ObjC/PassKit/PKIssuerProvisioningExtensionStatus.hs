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
  , requiresAuthenticationSelector
  , setRequiresAuthenticationSelector
  , passEntriesAvailableSelector
  , setPassEntriesAvailableSelector
  , remotePassEntriesAvailableSelector
  , setRemotePassEntriesAvailableSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO (Id PKIssuerProvisioningExtensionStatus)
init_ pkIssuerProvisioningExtensionStatus  =
  sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- requiresAuthentication@
requiresAuthentication :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO Bool
requiresAuthentication pkIssuerProvisioningExtensionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "requiresAuthentication") retCULong []

-- | @- setRequiresAuthentication:@
setRequiresAuthentication :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> Bool -> IO ()
setRequiresAuthentication pkIssuerProvisioningExtensionStatus  value =
  sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "setRequiresAuthentication:") retVoid [argCULong (if value then 1 else 0)]

-- | @- passEntriesAvailable@
passEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO Bool
passEntriesAvailable pkIssuerProvisioningExtensionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "passEntriesAvailable") retCULong []

-- | @- setPassEntriesAvailable:@
setPassEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> Bool -> IO ()
setPassEntriesAvailable pkIssuerProvisioningExtensionStatus  value =
  sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "setPassEntriesAvailable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- remotePassEntriesAvailable@
remotePassEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> IO Bool
remotePassEntriesAvailable pkIssuerProvisioningExtensionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "remotePassEntriesAvailable") retCULong []

-- | @- setRemotePassEntriesAvailable:@
setRemotePassEntriesAvailable :: IsPKIssuerProvisioningExtensionStatus pkIssuerProvisioningExtensionStatus => pkIssuerProvisioningExtensionStatus -> Bool -> IO ()
setRemotePassEntriesAvailable pkIssuerProvisioningExtensionStatus  value =
  sendMsg pkIssuerProvisioningExtensionStatus (mkSelector "setRemotePassEntriesAvailable:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @requiresAuthentication@
requiresAuthenticationSelector :: Selector
requiresAuthenticationSelector = mkSelector "requiresAuthentication"

-- | @Selector@ for @setRequiresAuthentication:@
setRequiresAuthenticationSelector :: Selector
setRequiresAuthenticationSelector = mkSelector "setRequiresAuthentication:"

-- | @Selector@ for @passEntriesAvailable@
passEntriesAvailableSelector :: Selector
passEntriesAvailableSelector = mkSelector "passEntriesAvailable"

-- | @Selector@ for @setPassEntriesAvailable:@
setPassEntriesAvailableSelector :: Selector
setPassEntriesAvailableSelector = mkSelector "setPassEntriesAvailable:"

-- | @Selector@ for @remotePassEntriesAvailable@
remotePassEntriesAvailableSelector :: Selector
remotePassEntriesAvailableSelector = mkSelector "remotePassEntriesAvailable"

-- | @Selector@ for @setRemotePassEntriesAvailable:@
setRemotePassEntriesAvailableSelector :: Selector
setRemotePassEntriesAvailableSelector = mkSelector "setRemotePassEntriesAvailable:"

