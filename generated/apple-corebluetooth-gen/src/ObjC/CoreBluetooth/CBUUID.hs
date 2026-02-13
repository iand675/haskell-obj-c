{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBUUID
--
-- A 16-bit, 32-bit, or 128-bit Bluetooth UUID.      16-bit and 32-bit UUIDs are implicitly pre-filled with the Bluetooth Base UUID.
--
-- Generated bindings for @CBUUID@.
module ObjC.CoreBluetooth.CBUUID
  ( CBUUID
  , IsCBUUID(..)
  , uuidWithString
  , uuidWithData
  , uuidWithCFUUID
  , uuidWithNSUUID
  , data_
  , uuidString
  , dataSelector
  , uuidStringSelector
  , uuidWithCFUUIDSelector
  , uuidWithDataSelector
  , uuidWithNSUUIDSelector
  , uuidWithStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | UUIDWithString:
--
-- Creates a CBUUID with a 16-bit, 32-bit, or 128-bit UUID string representation.      The expected format for 128-bit UUIDs is a string punctuated by hyphens, for example 68753A44-4D6F-1226-9C60-0050E4C00067.
--
-- ObjC selector: @+ UUIDWithString:@
uuidWithString :: IsNSString theString => theString -> IO (Id CBUUID)
uuidWithString theString =
  do
    cls' <- getRequiredClass "CBUUID"
    sendClassMessage cls' uuidWithStringSelector (toNSString theString)

-- | UUIDWithData:
--
-- Creates a CBUUID with a 16-bit, 32-bit, or 128-bit UUID data container.
--
-- ObjC selector: @+ UUIDWithData:@
uuidWithData :: IsNSData theData => theData -> IO (Id CBUUID)
uuidWithData theData =
  do
    cls' <- getRequiredClass "CBUUID"
    sendClassMessage cls' uuidWithDataSelector (toNSData theData)

-- | UUIDWithCFUUID:
--
-- Creates a CBUUID with a CFUUIDRef.
--
-- ObjC selector: @+ UUIDWithCFUUID:@
uuidWithCFUUID :: RawId -> IO (Id CBUUID)
uuidWithCFUUID theUUID =
  do
    cls' <- getRequiredClass "CBUUID"
    sendClassMessage cls' uuidWithCFUUIDSelector theUUID

-- | UUIDWithNSUUID:
--
-- Creates a CBUUID with an NSUUID.
--
-- ObjC selector: @+ UUIDWithNSUUID:@
uuidWithNSUUID :: IsNSUUID theUUID => theUUID -> IO (Id CBUUID)
uuidWithNSUUID theUUID =
  do
    cls' <- getRequiredClass "CBUUID"
    sendClassMessage cls' uuidWithNSUUIDSelector (toNSUUID theUUID)

-- | data
--
-- The UUID as NSData.
--
-- ObjC selector: @- data@
data_ :: IsCBUUID cbuuid => cbuuid -> IO (Id NSData)
data_ cbuuid =
  sendMessage cbuuid dataSelector

-- | UUIDString
--
-- The UUID as NSString.
--
-- ObjC selector: @- UUIDString@
uuidString :: IsCBUUID cbuuid => cbuuid -> IO RawId
uuidString cbuuid =
  sendMessage cbuuid uuidStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @UUIDWithString:@
uuidWithStringSelector :: Selector '[Id NSString] (Id CBUUID)
uuidWithStringSelector = mkSelector "UUIDWithString:"

-- | @Selector@ for @UUIDWithData:@
uuidWithDataSelector :: Selector '[Id NSData] (Id CBUUID)
uuidWithDataSelector = mkSelector "UUIDWithData:"

-- | @Selector@ for @UUIDWithCFUUID:@
uuidWithCFUUIDSelector :: Selector '[RawId] (Id CBUUID)
uuidWithCFUUIDSelector = mkSelector "UUIDWithCFUUID:"

-- | @Selector@ for @UUIDWithNSUUID:@
uuidWithNSUUIDSelector :: Selector '[Id NSUUID] (Id CBUUID)
uuidWithNSUUIDSelector = mkSelector "UUIDWithNSUUID:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector '[] RawId
uuidStringSelector = mkSelector "UUIDString"

