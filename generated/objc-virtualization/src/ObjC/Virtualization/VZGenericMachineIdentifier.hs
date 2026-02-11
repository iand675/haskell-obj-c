{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An identifier to make a virtual machine unique.
--
-- The generic machine identifier is used by guests to uniquely identify the virtual hardware.
--
-- If the virtual machine is serialized to disk, the identifier can be preserved in a binary representation through VZGenericMachineIdentifier.dataRepresentation.    The identifier can then be recreated with -[VZGenericMachineIdentifier initWithDataRepresentation:] from the binary representation.
--
-- The contents of two identifiers can be compared with -[VZGenericMachineIdentifier isEqual:].
--
-- VZGenericPlatformConfiguration
--
-- Generated bindings for @VZGenericMachineIdentifier@.
module ObjC.Virtualization.VZGenericMachineIdentifier
  ( VZGenericMachineIdentifier
  , IsVZGenericMachineIdentifier(..)
  , init_
  , initWithDataRepresentation
  , dataRepresentation
  , initSelector
  , initWithDataRepresentationSelector
  , dataRepresentationSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new unique machine identifier.
--
-- ObjC selector: @- init@
init_ :: IsVZGenericMachineIdentifier vzGenericMachineIdentifier => vzGenericMachineIdentifier -> IO (Id VZGenericMachineIdentifier)
init_ vzGenericMachineIdentifier  =
  sendMsg vzGenericMachineIdentifier (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get the machine identifier described by the specified data representation.
--
-- @dataRepresentation@ — The opaque data representation of the machine identifier to be obtained.
--
-- Returns: A unique identifier identical to the one that generated the dataRepresentation, or nil if the data is invalid.
--
-- See: VZGenericMachineIdentifier.dataRepresentation
--
-- ObjC selector: @- initWithDataRepresentation:@
initWithDataRepresentation :: (IsVZGenericMachineIdentifier vzGenericMachineIdentifier, IsNSData dataRepresentation) => vzGenericMachineIdentifier -> dataRepresentation -> IO (Id VZGenericMachineIdentifier)
initWithDataRepresentation vzGenericMachineIdentifier  dataRepresentation =
withObjCPtr dataRepresentation $ \raw_dataRepresentation ->
    sendMsg vzGenericMachineIdentifier (mkSelector "initWithDataRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_dataRepresentation :: Ptr ())] >>= ownedObject . castPtr

-- | Opaque data representation of the machine identifier.
--
-- This can be used to recreate the same machine identifier with -[VZGenericMachineIdentifier initWithDataRepresentation:].
--
-- See: -[VZGenericMachineIdentifier initWithDataRepresentation:]
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsVZGenericMachineIdentifier vzGenericMachineIdentifier => vzGenericMachineIdentifier -> IO (Id NSData)
dataRepresentation vzGenericMachineIdentifier  =
  sendMsg vzGenericMachineIdentifier (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataRepresentation:@
initWithDataRepresentationSelector :: Selector
initWithDataRepresentationSelector = mkSelector "initWithDataRepresentation:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

