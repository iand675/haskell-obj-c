{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class representing Tag-Length-Value record. Every record has its tag and binary value represented as NSData instance.  Allows retrieving record's tag, value (as NSData object) and binary representation of the record. Existing subclasses implement assorted encodings - TKBERTLVRecord, TKSimpleTLVRecord and TKCompactTLVRecord.
--
-- Generated bindings for @TKTLVRecord@.
module ObjC.CryptoTokenKit.TKTLVRecord
  ( TKTLVRecord
  , IsTKTLVRecord(..)
  , recordFromData
  , sequenceOfRecordsFromData
  , init_
  , tag
  , value
  , data_
  , recordFromDataSelector
  , sequenceOfRecordsFromDataSelector
  , initSelector
  , tagSelector
  , valueSelector
  , dataSelector


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

-- | Parses TLV record from data block
--
-- @data@ — Data block containing serialized form of TLV record.
--
-- Returns: newly parsed record instance or nil if data do not represent valid record.
--
-- ObjC selector: @+ recordFromData:@
recordFromData :: IsNSData data_ => data_ -> IO (Id TKTLVRecord)
recordFromData data_ =
  do
    cls' <- getRequiredClass "TKTLVRecord"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "recordFromData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | Parses sequence of TLV records from data block. The amount of records is determined by the length of input data block.
--
-- @data@ — Data block containing zero or more serialized forms of TLV record.
--
-- Returns: An array of TLV record instances parsed from input data block or nil if data do not form valid TLV record sequence.
--
-- ObjC selector: @+ sequenceOfRecordsFromData:@
sequenceOfRecordsFromData :: IsNSData data_ => data_ -> IO (Id NSArray)
sequenceOfRecordsFromData data_ =
  do
    cls' <- getRequiredClass "TKTLVRecord"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "sequenceOfRecordsFromData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO (Id TKTLVRecord)
init_ tktlvRecord  =
  sendMsg tktlvRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Tag value of the record.
--
-- ObjC selector: @- tag@
tag :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO CULong
tag tktlvRecord  =
  sendMsg tktlvRecord (mkSelector "tag") retCULong []

-- | Value field of the record.
--
-- ObjC selector: @- value@
value :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO (Id NSData)
value tktlvRecord  =
  sendMsg tktlvRecord (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Data object containing whole encoded record, including tag, length and value.
--
-- ObjC selector: @- data@
data_ :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO (Id NSData)
data_ tktlvRecord  =
  sendMsg tktlvRecord (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recordFromData:@
recordFromDataSelector :: Selector
recordFromDataSelector = mkSelector "recordFromData:"

-- | @Selector@ for @sequenceOfRecordsFromData:@
sequenceOfRecordsFromDataSelector :: Selector
sequenceOfRecordsFromDataSelector = mkSelector "sequenceOfRecordsFromData:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

