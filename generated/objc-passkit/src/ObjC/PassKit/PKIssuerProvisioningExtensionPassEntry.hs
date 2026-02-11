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
  , initSelector
  , identifierSelector
  , titleSelector
  , artSelector


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
init_ :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Id PKIssuerProvisioningExtensionPassEntry)
init_ pkIssuerProvisioningExtensionPassEntry  =
  sendMsg pkIssuerProvisioningExtensionPassEntry (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Id NSString)
identifier pkIssuerProvisioningExtensionPassEntry  =
  sendMsg pkIssuerProvisioningExtensionPassEntry (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Id NSString)
title pkIssuerProvisioningExtensionPassEntry  =
  sendMsg pkIssuerProvisioningExtensionPassEntry (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- art@
art :: IsPKIssuerProvisioningExtensionPassEntry pkIssuerProvisioningExtensionPassEntry => pkIssuerProvisioningExtensionPassEntry -> IO (Ptr ())
art pkIssuerProvisioningExtensionPassEntry  =
  fmap castPtr $ sendMsg pkIssuerProvisioningExtensionPassEntry (mkSelector "art") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @art@
artSelector :: Selector
artSelector = mkSelector "art"

