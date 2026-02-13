{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBDescriptor
--
-- Represents a characteristic's descriptor.
--
-- Generated bindings for @CBDescriptor@.
module ObjC.CoreBluetooth.CBDescriptor
  ( CBDescriptor
  , IsCBDescriptor(..)
  , characteristic
  , value
  , characteristicSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | characteristic
--
-- A back-pointer to the characteristic this descriptor belongs to.
--
-- ObjC selector: @- characteristic@
characteristic :: IsCBDescriptor cbDescriptor => cbDescriptor -> IO (Id CBCharacteristic)
characteristic cbDescriptor =
  sendMessage cbDescriptor characteristicSelector

-- | value
--
-- The value of the descriptor. The corresponding value types for the various descriptors are detailed in
--
-- CBUUID.h
--
-- .
--
-- ObjC selector: @- value@
value :: IsCBDescriptor cbDescriptor => cbDescriptor -> IO RawId
value cbDescriptor =
  sendMessage cbDescriptor valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @characteristic@
characteristicSelector :: Selector '[] (Id CBCharacteristic)
characteristicSelector = mkSelector "characteristic"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

