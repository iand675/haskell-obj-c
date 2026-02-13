{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that identifies a container.
--
-- The identifier is either a UUID or a UUID with additional differentiating bytes. Some network protocols evaluate access based on a user ID when connecting. In this situation, when a file server receives multiple client connections with different user IDs, the server provides different file hierarchies to each. For such systems, represent the container identifier as the UUID associated with the server, followed by four or eight bytes to differentiate connections.
--
-- > Important: Don't subclass this class.
--
-- Generated bindings for @FSContainerIdentifier@.
module ObjC.FSKit.FSContainerIdentifier
  ( FSContainerIdentifier
  , IsFSContainerIdentifier(..)
  , volumeIdentifier
  , volumeIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The volume identifier associated with the container.
--
-- For unary file systems, the volume identifier is the same as the container identifier.
--
-- ObjC selector: @- volumeIdentifier@
volumeIdentifier :: IsFSContainerIdentifier fsContainerIdentifier => fsContainerIdentifier -> IO (Id FSVolumeIdentifier)
volumeIdentifier fsContainerIdentifier =
  sendMessage fsContainerIdentifier volumeIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @volumeIdentifier@
volumeIdentifierSelector :: Selector '[] (Id FSVolumeIdentifier)
volumeIdentifierSelector = mkSelector "volumeIdentifier"

