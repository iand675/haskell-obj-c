{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKBERTLVRecord implements encoding using BER-TLV encoding rules. It is able to parse BER-encoded data and always produces DER-encoded data. No interpretation of tag values is made, all values are treated only as NSData irrespective of the tag.
--
-- Generated bindings for @TKBERTLVRecord@.
module ObjC.CryptoTokenKit.TKBERTLVRecord
  ( TKBERTLVRecord
  , IsTKBERTLVRecord(..)
  , dataForTag
  , initWithTag_value
  , initWithTag_records
  , dataForTagSelector
  , initWithTag_recordsSelector
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

-- | Encodes tag using BER-TLV tag encoding rules.
--
-- @tag@ — Tag value to encode
--
-- Returns: Binary block containing encoded tag value.
--
-- ObjC selector: @+ dataForTag:@
dataForTag :: CULong -> IO (Id NSData)
dataForTag tag =
  do
    cls' <- getRequiredClass "TKBERTLVRecord"
    sendClassMessage cls' dataForTagSelector tag

-- | Creates TLV record with specified tag and value.
--
-- @tag@ — Tag value for the new record.
--
-- @value@ — Value for the new record.
--
-- Returns: Newly created TLV record.
--
-- ObjC selector: @- initWithTag:value:@
initWithTag_value :: (IsTKBERTLVRecord tkbertlvRecord, IsNSData value) => tkbertlvRecord -> CULong -> value -> IO (Id TKBERTLVRecord)
initWithTag_value tkbertlvRecord tag value =
  sendOwnedMessage tkbertlvRecord initWithTag_valueSelector tag (toNSData value)

-- | Creates TKBERTLVRecord with specified tag and array of children TKTLVRecord instances as subrecords.
--
-- @tag@ — Tag value for the new record.
--
-- @records@ — Array of TKTLVRecord instances serving as subrecords of this record.
--
-- Returns: Newly created TLV record.
--
-- ObjC selector: @- initWithTag:records:@
initWithTag_records :: (IsTKBERTLVRecord tkbertlvRecord, IsNSArray records) => tkbertlvRecord -> CULong -> records -> IO (Id TKBERTLVRecord)
initWithTag_records tkbertlvRecord tag records =
  sendOwnedMessage tkbertlvRecord initWithTag_recordsSelector tag (toNSArray records)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dataForTag:@
dataForTagSelector :: Selector '[CULong] (Id NSData)
dataForTagSelector = mkSelector "dataForTag:"

-- | @Selector@ for @initWithTag:value:@
initWithTag_valueSelector :: Selector '[CULong, Id NSData] (Id TKBERTLVRecord)
initWithTag_valueSelector = mkSelector "initWithTag:value:"

-- | @Selector@ for @initWithTag:records:@
initWithTag_recordsSelector :: Selector '[CULong, Id NSArray] (Id TKBERTLVRecord)
initWithTag_recordsSelector = mkSelector "initWithTag:records:"

