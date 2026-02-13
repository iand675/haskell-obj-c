{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKSimpleTLVRecord implements Simple-TLV encoding according to ISO7816-4. Tag is number in range <1..254> encoded as single byte, length is either single byte specifying length 0-254 or 3 bytes encoded as 0xff followed by 2 bytes of big-endian encoded number.
--
-- Generated bindings for @TKSimpleTLVRecord@.
module ObjC.CryptoTokenKit.TKSimpleTLVRecord
  ( TKSimpleTLVRecord
  , IsTKSimpleTLVRecord(..)
  , initWithTag_value
  , initWithTag_valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates TLV record with specified tag and value.
--
-- @tag@ — Tag value for the new record.
--
-- @value@ — Value for the new record.
--
-- Returns: Newly created TLV record.
--
-- ObjC selector: @- initWithTag:value:@
initWithTag_value :: (IsTKSimpleTLVRecord tkSimpleTLVRecord, IsNSData value) => tkSimpleTLVRecord -> CUChar -> value -> IO (Id TKSimpleTLVRecord)
initWithTag_value tkSimpleTLVRecord tag value =
  sendOwnedMessage tkSimpleTLVRecord initWithTag_valueSelector tag (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:value:@
initWithTag_valueSelector :: Selector '[CUChar, Id NSData] (Id TKSimpleTLVRecord)
initWithTag_valueSelector = mkSelector "initWithTag:value:"

