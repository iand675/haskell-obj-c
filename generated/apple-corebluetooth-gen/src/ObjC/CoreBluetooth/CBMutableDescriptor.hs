{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBMutableDescriptor
--
-- Used to create a local characteristic descriptor, which can be added to the local database via CBPeripheralManager.		Once a descriptor is published, it is cached and can no longer be changed.		Descriptor types are detailed in
--
-- CBUUID.h
--
-- , but only the Characteristic User Description and Characteristic Presentation		Format descriptors are currently supported. The Characteristic Extended Properties and Client Characteristic 		Configuration descriptors will be created automatically upon publication of the parent service, depending on the properties of the characteristic itself.
--
-- Generated bindings for @CBMutableDescriptor@.
module ObjC.CoreBluetooth.CBMutableDescriptor
  ( CBMutableDescriptor
  , IsCBMutableDescriptor(..)
  , initWithType_value
  , initWithType_valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithType:value:
--
-- @UUID@ — The Bluetooth UUID of the descriptor.
--
-- @value@ — The value of the descriptor.
--
-- Returns a decriptor, initialized with a service type and value. The value is required and cannot be updated dynamically					once the parent service has been published.
--
-- ObjC selector: @- initWithType:value:@
initWithType_value :: (IsCBMutableDescriptor cbMutableDescriptor, IsCBUUID uuid) => cbMutableDescriptor -> uuid -> RawId -> IO (Id CBMutableDescriptor)
initWithType_value cbMutableDescriptor uuid value =
  sendOwnedMessage cbMutableDescriptor initWithType_valueSelector (toCBUUID uuid) value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:value:@
initWithType_valueSelector :: Selector '[Id CBUUID, RawId] (Id CBMutableDescriptor)
initWithType_valueSelector = mkSelector "initWithType:value:"

