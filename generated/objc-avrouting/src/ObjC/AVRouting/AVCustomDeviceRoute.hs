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
  , networkEndpointSelector


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

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A local or remote endpoint to connect to.
--
-- ObjC selector: @- networkEndpoint@
networkEndpoint :: IsAVCustomDeviceRoute avCustomDeviceRoute => avCustomDeviceRoute -> IO (Id NSObject)
networkEndpoint avCustomDeviceRoute  =
  sendMsg avCustomDeviceRoute (mkSelector "networkEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkEndpoint@
networkEndpointSelector :: Selector
networkEndpointSelector = mkSelector "networkEndpoint"

