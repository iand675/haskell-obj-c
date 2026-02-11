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

-- | characteristic
--
-- A back-pointer to the characteristic this descriptor belongs to.
--
-- ObjC selector: @- characteristic@
characteristic :: IsCBDescriptor cbDescriptor => cbDescriptor -> IO (Id CBCharacteristic)
characteristic cbDescriptor  =
  sendMsg cbDescriptor (mkSelector "characteristic") (retPtr retVoid) [] >>= retainedObject . castPtr

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
value cbDescriptor  =
  fmap (RawId . castPtr) $ sendMsg cbDescriptor (mkSelector "value") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @characteristic@
characteristicSelector :: Selector
characteristicSelector = mkSelector "characteristic"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

