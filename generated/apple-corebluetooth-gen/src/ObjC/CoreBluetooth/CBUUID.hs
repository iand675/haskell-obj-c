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
  , uuidWithStringSelector
  , uuidWithDataSelector
  , uuidWithCFUUIDSelector
  , uuidWithNSUUIDSelector
  , dataSelector
  , uuidStringSelector


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

-- | UUIDWithString:
--
-- Creates a CBUUID with a 16-bit, 32-bit, or 128-bit UUID string representation.      The expected format for 128-bit UUIDs is a string punctuated by hyphens, for example 68753A44-4D6F-1226-9C60-0050E4C00067.
--
-- ObjC selector: @+ UUIDWithString:@
uuidWithString :: IsNSString theString => theString -> IO (Id CBUUID)
uuidWithString theString =
  do
    cls' <- getRequiredClass "CBUUID"
    withObjCPtr theString $ \raw_theString ->
      sendClassMsg cls' (mkSelector "UUIDWithString:") (retPtr retVoid) [argPtr (castPtr raw_theString :: Ptr ())] >>= retainedObject . castPtr

-- | UUIDWithData:
--
-- Creates a CBUUID with a 16-bit, 32-bit, or 128-bit UUID data container.
--
-- ObjC selector: @+ UUIDWithData:@
uuidWithData :: IsNSData theData => theData -> IO (Id CBUUID)
uuidWithData theData =
  do
    cls' <- getRequiredClass "CBUUID"
    withObjCPtr theData $ \raw_theData ->
      sendClassMsg cls' (mkSelector "UUIDWithData:") (retPtr retVoid) [argPtr (castPtr raw_theData :: Ptr ())] >>= retainedObject . castPtr

-- | UUIDWithCFUUID:
--
-- Creates a CBUUID with a CFUUIDRef.
--
-- ObjC selector: @+ UUIDWithCFUUID:@
uuidWithCFUUID :: RawId -> IO (Id CBUUID)
uuidWithCFUUID theUUID =
  do
    cls' <- getRequiredClass "CBUUID"
    sendClassMsg cls' (mkSelector "UUIDWithCFUUID:") (retPtr retVoid) [argPtr (castPtr (unRawId theUUID) :: Ptr ())] >>= retainedObject . castPtr

-- | UUIDWithNSUUID:
--
-- Creates a CBUUID with an NSUUID.
--
-- ObjC selector: @+ UUIDWithNSUUID:@
uuidWithNSUUID :: IsNSUUID theUUID => theUUID -> IO (Id CBUUID)
uuidWithNSUUID theUUID =
  do
    cls' <- getRequiredClass "CBUUID"
    withObjCPtr theUUID $ \raw_theUUID ->
      sendClassMsg cls' (mkSelector "UUIDWithNSUUID:") (retPtr retVoid) [argPtr (castPtr raw_theUUID :: Ptr ())] >>= retainedObject . castPtr

-- | data
--
-- The UUID as NSData.
--
-- ObjC selector: @- data@
data_ :: IsCBUUID cbuuid => cbuuid -> IO (Id NSData)
data_ cbuuid  =
    sendMsg cbuuid (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | UUIDString
--
-- The UUID as NSString.
--
-- ObjC selector: @- UUIDString@
uuidString :: IsCBUUID cbuuid => cbuuid -> IO RawId
uuidString cbuuid  =
    fmap (RawId . castPtr) $ sendMsg cbuuid (mkSelector "UUIDString") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @UUIDWithString:@
uuidWithStringSelector :: Selector
uuidWithStringSelector = mkSelector "UUIDWithString:"

-- | @Selector@ for @UUIDWithData:@
uuidWithDataSelector :: Selector
uuidWithDataSelector = mkSelector "UUIDWithData:"

-- | @Selector@ for @UUIDWithCFUUID:@
uuidWithCFUUIDSelector :: Selector
uuidWithCFUUIDSelector = mkSelector "UUIDWithCFUUID:"

-- | @Selector@ for @UUIDWithNSUUID:@
uuidWithNSUUIDSelector :: Selector
uuidWithNSUUIDSelector = mkSelector "UUIDWithNSUUID:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector
uuidStringSelector = mkSelector "UUIDString"

