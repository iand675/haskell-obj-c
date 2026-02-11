{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionDataRecord`` object represents a record of stored data for a specific web extension context.
--
-- Contains properties and methods to query the data types and sizes.
--
-- Generated bindings for @WKWebExtensionDataRecord@.
module ObjC.WebKit.WKWebExtensionDataRecord
  ( WKWebExtensionDataRecord
  , IsWKWebExtensionDataRecord(..)
  , new
  , init_
  , sizeInBytesOfTypes
  , displayName
  , uniqueIdentifier
  , containedDataTypes
  , errors
  , totalSizeInBytes
  , newSelector
  , initSelector
  , sizeInBytesOfTypesSelector
  , displayNameSelector
  , uniqueIdentifierSelector
  , containedDataTypesSelector
  , errorsSelector
  , totalSizeInBytesSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionDataRecord)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionDataRecord"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id WKWebExtensionDataRecord)
init_ wkWebExtensionDataRecord  =
  sendMsg wkWebExtensionDataRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Retrieves the size in bytes of the specific data types in this data record.
--
-- @dataTypes@ — The set of data types to measure the size for.
--
-- Returns: The total size of the specified data types.
--
-- totalSizeInBytes
--
-- ObjC selector: @- sizeInBytesOfTypes:@
sizeInBytesOfTypes :: (IsWKWebExtensionDataRecord wkWebExtensionDataRecord, IsNSSet dataTypes) => wkWebExtensionDataRecord -> dataTypes -> IO CULong
sizeInBytesOfTypes wkWebExtensionDataRecord  dataTypes =
withObjCPtr dataTypes $ \raw_dataTypes ->
    sendMsg wkWebExtensionDataRecord (mkSelector "sizeInBytesOfTypes:") retCULong [argPtr (castPtr raw_dataTypes :: Ptr ())]

-- | The display name for the web extension to which this data record belongs.
--
-- ObjC selector: @- displayName@
displayName :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSString)
displayName wkWebExtensionDataRecord  =
  sendMsg wkWebExtensionDataRecord (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Unique identifier for the web extension context to which this data record belongs.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSString)
uniqueIdentifier wkWebExtensionDataRecord  =
  sendMsg wkWebExtensionDataRecord (mkSelector "uniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of data types contained in this data record.
--
-- ObjC selector: @- containedDataTypes@
containedDataTypes :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSSet)
containedDataTypes wkWebExtensionDataRecord  =
  sendMsg wkWebExtensionDataRecord (mkSelector "containedDataTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of errors that may have occurred when either calculating or deleting storage.
--
-- ObjC selector: @- errors@
errors :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSArray)
errors wkWebExtensionDataRecord  =
  sendMsg wkWebExtensionDataRecord (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The total size in bytes of all data types contained in this data record.
--
-- sizeInBytesOfTypes:
--
-- ObjC selector: @- totalSizeInBytes@
totalSizeInBytes :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO CULong
totalSizeInBytes wkWebExtensionDataRecord  =
  sendMsg wkWebExtensionDataRecord (mkSelector "totalSizeInBytes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sizeInBytesOfTypes:@
sizeInBytesOfTypesSelector :: Selector
sizeInBytesOfTypesSelector = mkSelector "sizeInBytesOfTypes:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @containedDataTypes@
containedDataTypesSelector :: Selector
containedDataTypesSelector = mkSelector "containedDataTypes"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @totalSizeInBytes@
totalSizeInBytesSelector :: Selector
totalSizeInBytesSelector = mkSelector "totalSizeInBytes"

