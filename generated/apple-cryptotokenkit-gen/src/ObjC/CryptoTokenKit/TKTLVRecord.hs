{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , initSelector
  , recordFromDataSelector
  , sequenceOfRecordsFromDataSelector
  , tagSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' recordFromDataSelector (toNSData data_)

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
    sendClassMessage cls' sequenceOfRecordsFromDataSelector (toNSData data_)

-- | @- init@
init_ :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO (Id TKTLVRecord)
init_ tktlvRecord =
  sendOwnedMessage tktlvRecord initSelector

-- | Tag value of the record.
--
-- ObjC selector: @- tag@
tag :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO CULong
tag tktlvRecord =
  sendMessage tktlvRecord tagSelector

-- | Value field of the record.
--
-- ObjC selector: @- value@
value :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO (Id NSData)
value tktlvRecord =
  sendMessage tktlvRecord valueSelector

-- | Data object containing whole encoded record, including tag, length and value.
--
-- ObjC selector: @- data@
data_ :: IsTKTLVRecord tktlvRecord => tktlvRecord -> IO (Id NSData)
data_ tktlvRecord =
  sendMessage tktlvRecord dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recordFromData:@
recordFromDataSelector :: Selector '[Id NSData] (Id TKTLVRecord)
recordFromDataSelector = mkSelector "recordFromData:"

-- | @Selector@ for @sequenceOfRecordsFromData:@
sequenceOfRecordsFromDataSelector :: Selector '[Id NSData] (Id NSArray)
sequenceOfRecordsFromDataSelector = mkSelector "sequenceOfRecordsFromData:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTLVRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CULong
tagSelector = mkSelector "tag"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSData)
valueSelector = mkSelector "value"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

