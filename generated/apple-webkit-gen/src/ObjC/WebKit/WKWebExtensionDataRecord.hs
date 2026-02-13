{-# LANGUAGE DataKinds #-}
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
  , containedDataTypesSelector
  , displayNameSelector
  , errorsSelector
  , initSelector
  , newSelector
  , sizeInBytesOfTypesSelector
  , totalSizeInBytesSelector
  , uniqueIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionDataRecord)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionDataRecord"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id WKWebExtensionDataRecord)
init_ wkWebExtensionDataRecord =
  sendOwnedMessage wkWebExtensionDataRecord initSelector

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
sizeInBytesOfTypes wkWebExtensionDataRecord dataTypes =
  sendMessage wkWebExtensionDataRecord sizeInBytesOfTypesSelector (toNSSet dataTypes)

-- | The display name for the web extension to which this data record belongs.
--
-- ObjC selector: @- displayName@
displayName :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSString)
displayName wkWebExtensionDataRecord =
  sendMessage wkWebExtensionDataRecord displayNameSelector

-- | Unique identifier for the web extension context to which this data record belongs.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSString)
uniqueIdentifier wkWebExtensionDataRecord =
  sendMessage wkWebExtensionDataRecord uniqueIdentifierSelector

-- | The set of data types contained in this data record.
--
-- ObjC selector: @- containedDataTypes@
containedDataTypes :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSSet)
containedDataTypes wkWebExtensionDataRecord =
  sendMessage wkWebExtensionDataRecord containedDataTypesSelector

-- | An array of errors that may have occurred when either calculating or deleting storage.
--
-- ObjC selector: @- errors@
errors :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO (Id NSArray)
errors wkWebExtensionDataRecord =
  sendMessage wkWebExtensionDataRecord errorsSelector

-- | The total size in bytes of all data types contained in this data record.
--
-- sizeInBytesOfTypes:
--
-- ObjC selector: @- totalSizeInBytes@
totalSizeInBytes :: IsWKWebExtensionDataRecord wkWebExtensionDataRecord => wkWebExtensionDataRecord -> IO CULong
totalSizeInBytes wkWebExtensionDataRecord =
  sendMessage wkWebExtensionDataRecord totalSizeInBytesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionDataRecord)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionDataRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @sizeInBytesOfTypes:@
sizeInBytesOfTypesSelector :: Selector '[Id NSSet] CULong
sizeInBytesOfTypesSelector = mkSelector "sizeInBytesOfTypes:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSString)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @containedDataTypes@
containedDataTypesSelector :: Selector '[] (Id NSSet)
containedDataTypesSelector = mkSelector "containedDataTypes"

-- | @Selector@ for @errors@
errorsSelector :: Selector '[] (Id NSArray)
errorsSelector = mkSelector "errors"

-- | @Selector@ for @totalSizeInBytes@
totalSizeInBytesSelector :: Selector '[] CULong
totalSizeInBytesSelector = mkSelector "totalSizeInBytes"

