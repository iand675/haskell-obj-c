{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostIOSource
--
-- The abstract class IOUSBHostPipe and IOUSBHostStream derive from.
--
-- Defines common methods that are shared between IOUSBHostPipe and IOUSBHostStream.
--
-- Generated bindings for @IOUSBHostIOSource@.
module ObjC.IOUSBHost.IOUSBHostIOSource
  ( IOUSBHostIOSource
  , IsIOUSBHostIOSource(..)
  , init_
  , hostInterface
  , deviceAddress
  , endpointAddress
  , deviceAddressSelector
  , endpointAddressSelector
  , hostInterfaceSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOUSBHost.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO (Id IOUSBHostIOSource)
init_ iousbHostIOSource =
  sendOwnedMessage iousbHostIOSource initSelector

-- | Retrieve the source's IOUSBHostInterface
--
-- Returns: IOUSBHostInterface pointer that the IOSource was created from.
--
-- ObjC selector: @- hostInterface@
hostInterface :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO (Id IOUSBHostInterface)
hostInterface iousbHostIOSource =
  sendMessage iousbHostIOSource hostInterfaceSelector

-- | Retrieve the device's address
--
-- Returns: Current address of the device
--
-- ObjC selector: @- deviceAddress@
deviceAddress :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO CULong
deviceAddress iousbHostIOSource =
  sendMessage iousbHostIOSource deviceAddressSelector

-- | Retrieve the IOSource's endpoint address
--
-- Returns: Current address of the endpoint
--
-- ObjC selector: @- endpointAddress@
endpointAddress :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO CULong
endpointAddress iousbHostIOSource =
  sendMessage iousbHostIOSource endpointAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id IOUSBHostIOSource)
initSelector = mkSelector "init"

-- | @Selector@ for @hostInterface@
hostInterfaceSelector :: Selector '[] (Id IOUSBHostInterface)
hostInterfaceSelector = mkSelector "hostInterface"

-- | @Selector@ for @deviceAddress@
deviceAddressSelector :: Selector '[] CULong
deviceAddressSelector = mkSelector "deviceAddress"

-- | @Selector@ for @endpointAddress@
endpointAddressSelector :: Selector '[] CULong
endpointAddressSelector = mkSelector "endpointAddress"

