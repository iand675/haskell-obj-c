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
  , initWithTag_valueSelector
  , initWithTag_recordsSelector


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
    sendClassMsg cls' (mkSelector "dataForTag:") (retPtr retVoid) [argCULong (fromIntegral tag)] >>= retainedObject . castPtr

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
initWithTag_value tkbertlvRecord  tag value =
withObjCPtr value $ \raw_value ->
    sendMsg tkbertlvRecord (mkSelector "initWithTag:value:") (retPtr retVoid) [argCULong (fromIntegral tag), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

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
initWithTag_records tkbertlvRecord  tag records =
withObjCPtr records $ \raw_records ->
    sendMsg tkbertlvRecord (mkSelector "initWithTag:records:") (retPtr retVoid) [argCULong (fromIntegral tag), argPtr (castPtr raw_records :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dataForTag:@
dataForTagSelector :: Selector
dataForTagSelector = mkSelector "dataForTag:"

-- | @Selector@ for @initWithTag:value:@
initWithTag_valueSelector :: Selector
initWithTag_valueSelector = mkSelector "initWithTag:value:"

-- | @Selector@ for @initWithTag:records:@
initWithTag_recordsSelector :: Selector
initWithTag_recordsSelector = mkSelector "initWithTag:records:"

