{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBService
--
-- Represents a peripheral's service or a service's included service.
--
-- Generated bindings for @CBService@.
module ObjC.CoreBluetooth.CBService
  ( CBService
  , IsCBService(..)
  , peripheral
  , isPrimary
  , includedServices
  , characteristics
  , characteristicsSelector
  , includedServicesSelector
  , isPrimarySelector
  , peripheralSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | peripheral
--
-- A back-pointer to the peripheral this service belongs to.
--
-- ObjC selector: @- peripheral@
peripheral :: IsCBService cbService => cbService -> IO (Id CBPeripheral)
peripheral cbService =
  sendMessage cbService peripheralSelector

-- | isPrimary
--
-- The type of the service (primary or secondary).
--
-- ObjC selector: @- isPrimary@
isPrimary :: IsCBService cbService => cbService -> IO Bool
isPrimary cbService =
  sendMessage cbService isPrimarySelector

-- | includedServices
--
-- A list of included CBServices that have so far been discovered in this service.
--
-- ObjC selector: @- includedServices@
includedServices :: IsCBService cbService => cbService -> IO (Id NSArray)
includedServices cbService =
  sendMessage cbService includedServicesSelector

-- | characteristics
--
-- A list of CBCharacteristics that have so far been discovered in this service.
--
-- ObjC selector: @- characteristics@
characteristics :: IsCBService cbService => cbService -> IO (Id NSArray)
characteristics cbService =
  sendMessage cbService characteristicsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peripheral@
peripheralSelector :: Selector '[] (Id CBPeripheral)
peripheralSelector = mkSelector "peripheral"

-- | @Selector@ for @isPrimary@
isPrimarySelector :: Selector '[] Bool
isPrimarySelector = mkSelector "isPrimary"

-- | @Selector@ for @includedServices@
includedServicesSelector :: Selector '[] (Id NSArray)
includedServicesSelector = mkSelector "includedServices"

-- | @Selector@ for @characteristics@
characteristicsSelector :: Selector '[] (Id NSArray)
characteristicsSelector = mkSelector "characteristics"

