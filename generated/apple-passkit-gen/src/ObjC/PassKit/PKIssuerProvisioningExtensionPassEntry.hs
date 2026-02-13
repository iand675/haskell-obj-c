{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIssuerProvisioningExtensionPassEntry@.
module ObjC.PassKit.PKIssuerProvisioningExtensionPassEntry
  ( PKIssuerProvisioningExtensionPassEntry
  , IsPKIssuerProvisioningExtensionPassEntry(..)
  , init_
  , identifier
  , title
  , art
  , artSelector
  , identifierSelector
  , initSelector
  , titleSelector


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
init_ :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Id PKIssuerProvisioningExtensionPassEntry)
init_ pkIssuerProvisioningExtensionPassEntry =
  sendOwnedMessage pkIssuerProvisioningExtensionPassEntry initSelector

-- | @- identifier@
identifier :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Id NSString)
identifier pkIssuerProvisioningExtensionPassEntry =
  sendMessage pkIssuerProvisioningExtensionPassEntry identifierSelector

-- | @- title@
title :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Id NSString)
title pkIssuerProvisioningExtensionPassEntry =
  sendMessage pkIssuerProvisioningExtensionPassEntry titleSelector

-- | @- art@
art :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Ptr ())
art pkIssuerProvisioningExtensionPassEntry =
  sendMessage pkIssuerProvisioningExtensionPassEntry artSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIssuerProvisioningExtensionPassEntry)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @art@
artSelector :: Selector '[] (Ptr ())
artSelector = mkSelector "art"

