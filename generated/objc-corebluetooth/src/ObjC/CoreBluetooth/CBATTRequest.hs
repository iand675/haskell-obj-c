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
  , initSelector
  , centralSelector
  , characteristicSelector
  , offsetSelector
  , valueSelector
  , setValueSelector


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

-- | @- init@
init_ :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id CBATTRequest)
init_ cbattRequest  =
  sendMsg cbattRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | central
--
-- The central that originated the request.
--
-- ObjC selector: @- central@
central :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id CBCentral)
central cbattRequest  =
  sendMsg cbattRequest (mkSelector "central") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | characteristic
--
-- The characteristic whose value will be read or written.
--
-- ObjC selector: @- characteristic@
characteristic :: IsCBATTRequest cbattRequest => cbattRequest -> IO (Id CBCharacteristic)
characteristic cbattRequest  =
  sendMsg cbattRequest (mkSelector "characteristic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | offset
--
-- The zero-based index of the first byte for the read or write.
--
-- ObjC selector: @- offset@
offset :: IsCBATTRequest cbattRequest => cbattRequest -> IO CULong
offset cbattRequest  =
  sendMsg cbattRequest (mkSelector "offset") retCULong []

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
value cbattRequest  =
  sendMsg cbattRequest (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setValue cbattRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg cbattRequest (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @central@
centralSelector :: Selector
centralSelector = mkSelector "central"

-- | @Selector@ for @characteristic@
characteristicSelector :: Selector
characteristicSelector = mkSelector "characteristic"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

