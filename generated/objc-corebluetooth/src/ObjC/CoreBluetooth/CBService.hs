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
  , peripheralSelector
  , isPrimarySelector
  , includedServicesSelector
  , characteristicsSelector


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

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | peripheral
--
-- A back-pointer to the peripheral this service belongs to.
--
-- ObjC selector: @- peripheral@
peripheral :: IsCBService cbService => cbService -> IO (Id CBPeripheral)
peripheral cbService  =
  sendMsg cbService (mkSelector "peripheral") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isPrimary
--
-- The type of the service (primary or secondary).
--
-- ObjC selector: @- isPrimary@
isPrimary :: IsCBService cbService => cbService -> IO Bool
isPrimary cbService  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbService (mkSelector "isPrimary") retCULong []

-- | includedServices
--
-- A list of included CBServices that have so far been discovered in this service.
--
-- ObjC selector: @- includedServices@
includedServices :: IsCBService cbService => cbService -> IO (Id NSArray)
includedServices cbService  =
  sendMsg cbService (mkSelector "includedServices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | characteristics
--
-- A list of CBCharacteristics that have so far been discovered in this service.
--
-- ObjC selector: @- characteristics@
characteristics :: IsCBService cbService => cbService -> IO (Id NSArray)
characteristics cbService  =
  sendMsg cbService (mkSelector "characteristics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peripheral@
peripheralSelector :: Selector
peripheralSelector = mkSelector "peripheral"

-- | @Selector@ for @isPrimary@
isPrimarySelector :: Selector
isPrimarySelector = mkSelector "isPrimary"

-- | @Selector@ for @includedServices@
includedServicesSelector :: Selector
includedServicesSelector = mkSelector "includedServices"

-- | @Selector@ for @characteristics@
characteristicsSelector :: Selector
characteristicsSelector = mkSelector "characteristics"

