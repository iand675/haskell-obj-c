{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An identifier to make a virtual machine unique.
--
-- The Mac machine identifier is used by macOS guests to uniquely identify the virtual hardware.
--
-- Two virtual machines running concurrently should not use the same identifier.
--
-- If the virtual machine is serialized to disk, the identifier can be preserved in a binary representation through VZMacMachineIdentifier.dataRepresentation.    The identifier can then be recreated with -[VZMacMachineIdentifier initWithDataRepresentation:] from the binary representation.
--
-- The contents of two identifiers can be compared with -[VZMacMachineIdentifier isEqual:].
--
-- VZMacPlatformConfiguration
--
-- Generated bindings for @VZMacMachineIdentifier@.
module ObjC.Virtualization.VZMacMachineIdentifier
  ( VZMacMachineIdentifier
  , IsVZMacMachineIdentifier(..)
  , init_
  , initWithDataRepresentation
  , dataRepresentation
  , dataRepresentationSelector
  , initSelector
  , initWithDataRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new unique machine identifier.
--
-- ObjC selector: @- init@
init_ :: IsVZMacMachineIdentifier vzMacMachineIdentifier => vzMacMachineIdentifier -> IO (Id VZMacMachineIdentifier)
init_ vzMacMachineIdentifier =
  sendOwnedMessage vzMacMachineIdentifier initSelector

-- | Get the machine identifier described by the specified data representation.
--
-- @dataRepresentation@ — The opaque data representation of the machine identifier to be obtained.
--
-- Returns: A unique identifier identical to the one that generated the dataRepresentation, or nil if the data is invalid.
--
-- See: VZMacMachineIdentifier.dataRepresentation
--
-- ObjC selector: @- initWithDataRepresentation:@
initWithDataRepresentation :: (IsVZMacMachineIdentifier vzMacMachineIdentifier, IsNSData dataRepresentation) => vzMacMachineIdentifier -> dataRepresentation -> IO (Id VZMacMachineIdentifier)
initWithDataRepresentation vzMacMachineIdentifier dataRepresentation =
  sendOwnedMessage vzMacMachineIdentifier initWithDataRepresentationSelector (toNSData dataRepresentation)

-- | Opaque data representation of the machine identifier.
--
-- This can be used to recreate the same machine identifier with -[VZMacMachineIdentifier initWithDataRepresentation:].
--
-- See: -[VZMacMachineIdentifier initWithDataRepresentation:]
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsVZMacMachineIdentifier vzMacMachineIdentifier => vzMacMachineIdentifier -> IO (Id NSData)
dataRepresentation vzMacMachineIdentifier =
  sendMessage vzMacMachineIdentifier dataRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacMachineIdentifier)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataRepresentation:@
initWithDataRepresentationSelector :: Selector '[Id NSData] (Id VZMacMachineIdentifier)
initWithDataRepresentationSelector = mkSelector "initWithDataRepresentation:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

