{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Define a network interface that bridges a physical interface with a virtual machine.
--
-- A bridged interface is shared between the virtual machine and the host system. Both host and virtual machine send and receive packets on the same physical interface    but have distinct network layers.
--
-- VZBridgedNetworkInterface cannot be instantiated directly. A list of supported network interfaces can be obtained using +[VZBridgedNetworkInterface networkInterfaces].
--
-- The VZBridgedNetworkInterface can be used with a VZBridgedNetworkDeviceAttachment to set up a network device VZNetworkDeviceConfiguration.
--
-- VZBridgedNetworkDeviceAttachment
--
-- VZNATNetworkDeviceAttachment
--
-- VZNetworkDeviceConfiguration
--
-- Generated bindings for @VZBridgedNetworkInterface@.
module ObjC.Virtualization.VZBridgedNetworkInterface
  ( VZBridgedNetworkInterface
  , IsVZBridgedNetworkInterface(..)
  , new
  , init_
  , networkInterfaces
  , identifier
  , localizedDisplayName
  , newSelector
  , initSelector
  , networkInterfacesSelector
  , identifierSelector
  , localizedDisplayNameSelector


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

-- | @+ new@
new :: IO (Id VZBridgedNetworkInterface)
new  =
  do
    cls' <- getRequiredClass "VZBridgedNetworkInterface"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZBridgedNetworkInterface vzBridgedNetworkInterface => vzBridgedNetworkInterface -> IO (Id VZBridgedNetworkInterface)
init_ vzBridgedNetworkInterface  =
  sendMsg vzBridgedNetworkInterface (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Return the list of network interfaces available for bridging.
--
-- ObjC selector: @+ networkInterfaces@
networkInterfaces :: IO (Id NSArray)
networkInterfaces  =
  do
    cls' <- getRequiredClass "VZBridgedNetworkInterface"
    sendClassMsg cls' (mkSelector "networkInterfaces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return the unique identifier for this interface. The identifier is the BSD name associated with the interface (e.g. "en0").
--
-- ObjC selector: @- identifier@
identifier :: IsVZBridgedNetworkInterface vzBridgedNetworkInterface => vzBridgedNetworkInterface -> IO (Id NSString)
identifier vzBridgedNetworkInterface  =
  sendMsg vzBridgedNetworkInterface (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return a display name if available (e.g. "Ethernet").
--
-- ObjC selector: @- localizedDisplayName@
localizedDisplayName :: IsVZBridgedNetworkInterface vzBridgedNetworkInterface => vzBridgedNetworkInterface -> IO (Id NSString)
localizedDisplayName vzBridgedNetworkInterface  =
  sendMsg vzBridgedNetworkInterface (mkSelector "localizedDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @networkInterfaces@
networkInterfacesSelector :: Selector
networkInterfacesSelector = mkSelector "networkInterfaces"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @localizedDisplayName@
localizedDisplayNameSelector :: Selector
localizedDisplayNameSelector = mkSelector "localizedDisplayName"

