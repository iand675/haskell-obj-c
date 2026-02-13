{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , initSelector
  , localizedDisplayNameSelector
  , networkInterfacesSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZBridgedNetworkInterface)
new  =
  do
    cls' <- getRequiredClass "VZBridgedNetworkInterface"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZBridgedNetworkInterface vzBridgedNetworkInterface => vzBridgedNetworkInterface -> IO (Id VZBridgedNetworkInterface)
init_ vzBridgedNetworkInterface =
  sendOwnedMessage vzBridgedNetworkInterface initSelector

-- | Return the list of network interfaces available for bridging.
--
-- ObjC selector: @+ networkInterfaces@
networkInterfaces :: IO (Id NSArray)
networkInterfaces  =
  do
    cls' <- getRequiredClass "VZBridgedNetworkInterface"
    sendClassMessage cls' networkInterfacesSelector

-- | Return the unique identifier for this interface. The identifier is the BSD name associated with the interface (e.g. "en0").
--
-- ObjC selector: @- identifier@
identifier :: IsVZBridgedNetworkInterface vzBridgedNetworkInterface => vzBridgedNetworkInterface -> IO (Id NSString)
identifier vzBridgedNetworkInterface =
  sendMessage vzBridgedNetworkInterface identifierSelector

-- | Return a display name if available (e.g. "Ethernet").
--
-- ObjC selector: @- localizedDisplayName@
localizedDisplayName :: IsVZBridgedNetworkInterface vzBridgedNetworkInterface => vzBridgedNetworkInterface -> IO (Id NSString)
localizedDisplayName vzBridgedNetworkInterface =
  sendMessage vzBridgedNetworkInterface localizedDisplayNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZBridgedNetworkInterface)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZBridgedNetworkInterface)
initSelector = mkSelector "init"

-- | @Selector@ for @networkInterfaces@
networkInterfacesSelector :: Selector '[] (Id NSArray)
networkInterfacesSelector = mkSelector "networkInterfaces"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @localizedDisplayName@
localizedDisplayNameSelector :: Selector '[] (Id NSString)
localizedDisplayNameSelector = mkSelector "localizedDisplayName"

