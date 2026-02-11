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

-- | maximumUpdateValueLength
--
-- The maximum amount of data, in bytes, that can be received by the central in a single notification or indication.
--
-- See: updateValue:forCharacteristic:onSubscribedCentrals:
--
-- ObjC selector: @- maximumUpdateValueLength@
maximumUpdateValueLength :: IsCBCentral cbCentral => cbCentral -> IO CULong
maximumUpdateValueLength cbCentral  =
  sendMsg cbCentral (mkSelector "maximumUpdateValueLength") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maximumUpdateValueLength@
maximumUpdateValueLengthSelector :: Selector
maximumUpdateValueLengthSelector = mkSelector "maximumUpdateValueLength"

