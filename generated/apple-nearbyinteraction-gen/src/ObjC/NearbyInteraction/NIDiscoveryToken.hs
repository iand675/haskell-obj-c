{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type used to uniquely discover and identify a device in a nearby interaction session.
--
-- Generated bindings for @NIDiscoveryToken@.
module ObjC.NearbyInteraction.NIDiscoveryToken
  ( NIDiscoveryToken
  , IsNIDiscoveryToken(..)
  , init_
  , new
  , deviceCapabilities
  , deviceCapabilitiesSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIDiscoveryToken niDiscoveryToken => niDiscoveryToken -> IO (Id NIDiscoveryToken)
init_ niDiscoveryToken =
  sendOwnedMessage niDiscoveryToken initSelector

-- | @+ new@
new :: IO (Id NIDiscoveryToken)
new  =
  do
    cls' <- getRequiredClass "NIDiscoveryToken"
    sendOwnedClassMessage cls' newSelector

-- | Get the protocol that describes nearby interaction capabilities of the device that generated this token.
--
-- Detailed description on the capability protocol is in NIDeviceCapability.h.
--
-- ObjC selector: @- deviceCapabilities@
deviceCapabilities :: IsNIDiscoveryToken niDiscoveryToken => niDiscoveryToken -> IO RawId
deviceCapabilities niDiscoveryToken =
  sendMessage niDiscoveryToken deviceCapabilitiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NIDiscoveryToken)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NIDiscoveryToken)
newSelector = mkSelector "new"

-- | @Selector@ for @deviceCapabilities@
deviceCapabilitiesSelector :: Selector '[] RawId
deviceCapabilitiesSelector = mkSelector "deviceCapabilities"

