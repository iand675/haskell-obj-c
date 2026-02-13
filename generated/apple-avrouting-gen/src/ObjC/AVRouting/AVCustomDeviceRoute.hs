{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a custom device route.
--
-- Use the value of a route’s ``AVCustomDeviceRoute/networkEndpoint`` or ``AVCustomDeviceRoute/bluetoothIdentifier`` property to establish a connection to a device. Typically, only one of the properties provides a valid value, depending on the type of device. In certain cases, both properties might provide valid values, in which case your app determines which one to use.
--
-- Generated bindings for @AVCustomDeviceRoute@.
module ObjC.AVRouting.AVCustomDeviceRoute
  ( AVCustomDeviceRoute
  , IsAVCustomDeviceRoute(..)
  , networkEndpoint
  , bluetoothIdentifier
  , bluetoothIdentifierSelector
  , networkEndpointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A local or remote endpoint to connect to.
--
-- ObjC selector: @- networkEndpoint@
networkEndpoint :: IsAVCustomDeviceRoute avCustomDeviceRoute => avCustomDeviceRoute -> IO (Id NSObject)
networkEndpoint avCustomDeviceRoute =
  sendMessage avCustomDeviceRoute networkEndpointSelector

-- | An identifier to use to establish a connection to a Bluetooth device.
--
-- ObjC selector: @- bluetoothIdentifier@
bluetoothIdentifier :: IsAVCustomDeviceRoute avCustomDeviceRoute => avCustomDeviceRoute -> IO (Id NSUUID)
bluetoothIdentifier avCustomDeviceRoute =
  sendMessage avCustomDeviceRoute bluetoothIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkEndpoint@
networkEndpointSelector :: Selector '[] (Id NSObject)
networkEndpointSelector = mkSelector "networkEndpoint"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector '[] (Id NSUUID)
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

