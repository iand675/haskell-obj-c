{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBCentral
--
-- Represents a remote central.
--
-- Generated bindings for @CBCentral@.
module ObjC.CoreBluetooth.CBCentral
  ( CBCentral
  , IsCBCentral(..)
  , maximumUpdateValueLength
  , maximumUpdateValueLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | maximumUpdateValueLength
--
-- The maximum amount of data, in bytes, that can be received by the central in a single notification or indication.
--
-- See: updateValue:forCharacteristic:onSubscribedCentrals:
--
-- ObjC selector: @- maximumUpdateValueLength@
maximumUpdateValueLength :: IsCBCentral cbCentral => cbCentral -> IO CULong
maximumUpdateValueLength cbCentral =
  sendMessage cbCentral maximumUpdateValueLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maximumUpdateValueLength@
maximumUpdateValueLengthSelector :: Selector '[] CULong
maximumUpdateValueLengthSelector = mkSelector "maximumUpdateValueLength"

