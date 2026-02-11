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
  , initSelector
  , newSelector
  , deviceCapabilitiesSelector


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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIDiscoveryToken niDiscoveryToken => niDiscoveryToken -> IO (Id NIDiscoveryToken)
init_ niDiscoveryToken  =
    sendMsg niDiscoveryToken (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NIDiscoveryToken)
new  =
  do
    cls' <- getRequiredClass "NIDiscoveryToken"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get the protocol that describes nearby interaction capabilities of the device that generated this token.
--
-- Detailed description on the capability protocol is in NIDeviceCapability.h.
--
-- ObjC selector: @- deviceCapabilities@
deviceCapabilities :: IsNIDiscoveryToken niDiscoveryToken => niDiscoveryToken -> IO RawId
deviceCapabilities niDiscoveryToken  =
    fmap (RawId . castPtr) $ sendMsg niDiscoveryToken (mkSelector "deviceCapabilities") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @deviceCapabilities@
deviceCapabilitiesSelector :: Selector
deviceCapabilitiesSelector = mkSelector "deviceCapabilities"

