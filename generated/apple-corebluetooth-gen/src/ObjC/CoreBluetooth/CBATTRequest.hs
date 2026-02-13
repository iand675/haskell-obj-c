{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBATTRequest
--
-- Represents a read or write request from a central.
--
-- Generated bindings for @CBATTRequest@.
module ObjC.CoreBluetooth.CBATTRequest
  ( CBATTRequest
  , IsCBATTRequest(..)
  , init_
  , central
  , characteristic
  , offset
  , value
  , setValue
  , centralSelector
  , characteristicSelector
  , initSelector
  , offsetSelector
  , setValueSelector
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

-- | @- init@
init_ :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id CBATTRequest)
init_ cbattRequest =
  sendOwnedMessage cbattRequest initSelector

-- | central
--
-- The central that originated the request.
--
-- ObjC selector: @- central@
central :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id CBCentral)
central cbattRequest =
  sendMessage cbattRequest centralSelector

-- | characteristic
--
-- The characteristic whose value will be read or written.
--
-- ObjC selector: @- characteristic@
characteristic :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id CBCharacteristic)
characteristic cbattRequest =
  sendMessage cbattRequest characteristicSelector

-- | offset
--
-- The zero-based index of the first byte for the read or write.
--
-- ObjC selector: @- offset@
offset :: IsCBATTRequest cbattRequest => cbattRequest -> IO CULong
offset cbattRequest =
  sendMessage cbattRequest offsetSelector

-- | value
--
-- The data being read or written.				For read requests, value will be nil and should be set before responding via
--
-- respondToRequest:withResult:
--
-- .				For write requests, value will contain the data to be written.
--
-- ObjC selector: @- value@
value :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id NSData)
value cbattRequest =
  sendMessage cbattRequest valueSelector

-- | value
--
-- The data being read or written.				For read requests, value will be nil and should be set before responding via
--
-- respondToRequest:withResult:
--
-- .				For write requests, value will contain the data to be written.
--
-- ObjC selector: @- setValue:@
setValue :: (IsCBATTRequest cbattRequest, IsNSData value) => cbattRequest -> value -> IO ()
setValue cbattRequest value =
  sendMessage cbattRequest setValueSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CBATTRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @central@
centralSelector :: Selector '[] (Id CBCentral)
centralSelector = mkSelector "central"

-- | @Selector@ for @characteristic@
characteristicSelector :: Selector '[] (Id CBCharacteristic)
characteristicSelector = mkSelector "characteristic"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSData)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSData] ()
setValueSelector = mkSelector "setValue:"

