{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBMutableService
--
-- Used to create a local service or included service, which can be added to the local database via CBPeripheralManager.		Once a service is published, it is cached and can no longer be changed. This class adds write access to all properties in the
--
-- CBService
--
-- class.
--
-- Generated bindings for @CBMutableService@.
module ObjC.CoreBluetooth.CBMutableService
  ( CBMutableService
  , IsCBMutableService(..)
  , initWithType_primary
  , includedServices
  , setIncludedServices
  , characteristics
  , setCharacteristics
  , characteristicsSelector
  , includedServicesSelector
  , initWithType_primarySelector
  , setCharacteristicsSelector
  , setIncludedServicesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithType:primary:
--
-- @UUID@ — The Bluetooth UUID of the service.
--
-- @isPrimary@ — The type of the service (primary or secondary).
--
-- Returns a service, initialized with a service type and UUID.
--
-- ObjC selector: @- initWithType:primary:@
initWithType_primary :: (IsCBMutableService cbMutableService, IsCBUUID uuid) => cbMutableService -> uuid -> Bool -> IO (Id CBMutableService)
initWithType_primary cbMutableService uuid isPrimary =
  sendOwnedMessage cbMutableService initWithType_primarySelector (toCBUUID uuid) isPrimary

-- | @- includedServices@
includedServices :: IsCBMutableService cbMutableService => cbMutableService -> IO (Id NSArray)
includedServices cbMutableService =
  sendMessage cbMutableService includedServicesSelector

-- | @- setIncludedServices:@
setIncludedServices :: (IsCBMutableService cbMutableService, IsNSArray value) => cbMutableService -> value -> IO ()
setIncludedServices cbMutableService value =
  sendMessage cbMutableService setIncludedServicesSelector (toNSArray value)

-- | @- characteristics@
characteristics :: IsCBMutableService cbMutableService => cbMutableService -> IO (Id NSArray)
characteristics cbMutableService =
  sendMessage cbMutableService characteristicsSelector

-- | @- setCharacteristics:@
setCharacteristics :: (IsCBMutableService cbMutableService, IsNSArray value) => cbMutableService -> value -> IO ()
setCharacteristics cbMutableService value =
  sendMessage cbMutableService setCharacteristicsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:primary:@
initWithType_primarySelector :: Selector '[Id CBUUID, Bool] (Id CBMutableService)
initWithType_primarySelector = mkSelector "initWithType:primary:"

-- | @Selector@ for @includedServices@
includedServicesSelector :: Selector '[] (Id NSArray)
includedServicesSelector = mkSelector "includedServices"

-- | @Selector@ for @setIncludedServices:@
setIncludedServicesSelector :: Selector '[Id NSArray] ()
setIncludedServicesSelector = mkSelector "setIncludedServices:"

-- | @Selector@ for @characteristics@
characteristicsSelector :: Selector '[] (Id NSArray)
characteristicsSelector = mkSelector "characteristics"

-- | @Selector@ for @setCharacteristics:@
setCharacteristicsSelector :: Selector '[Id NSArray] ()
setCharacteristicsSelector = mkSelector "setCharacteristics:"

