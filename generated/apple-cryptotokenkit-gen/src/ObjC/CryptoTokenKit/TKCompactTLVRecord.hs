{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKCompactTLVRecord implements Compact-TLV encoding according to ISO7816-4 Tag is number in range <0..15> encoded as high 4 bits of initial byte, length is number in range <0..15> encoded as low 4 bits of initial byte.  Value immediatelly follows leading byte.
--
-- Generated bindings for @TKCompactTLVRecord@.
module ObjC.CryptoTokenKit.TKCompactTLVRecord
  ( TKCompactTLVRecord
  , IsTKCompactTLVRecord(..)
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
initWithTag_value :: (IsTKCompactTLVRecord tkCompactTLVRecord, IsNSData value) => tkCompactTLVRecord -> CUChar -> value -> IO (Id TKCompactTLVRecord)
initWithTag_value tkCompactTLVRecord tag value =
  sendOwnedMessage tkCompactTLVRecord initWithTag_valueSelector tag (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:value:@
initWithTag_valueSelector :: Selector '[CUChar, Id NSData] (Id TKCompactTLVRecord)
initWithTag_valueSelector = mkSelector "initWithTag:value:"

