{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABMultiValue@.
module ObjC.AddressBook.ABMultiValue
  ( ABMultiValue
  , IsABMultiValue(..)
  , count
  , valueAtIndex
  , labelAtIndex
  , identifierAtIndex
  , indexForIdentifier
  , primaryIdentifier
  , propertyType
  , valueForIdentifier
  , labelForIdentifier
  , countSelector
  , identifierAtIndexSelector
  , indexForIdentifierSelector
  , labelAtIndexSelector
  , labelForIdentifierSelector
  , primaryIdentifierSelector
  , propertyTypeSelector
  , valueAtIndexSelector
  , valueForIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- count@
count :: IsABMultiValue abMultiValue => abMultiValue -> IO CULong
count abMultiValue =
  sendMessage abMultiValue countSelector

-- | @- valueAtIndex:@
valueAtIndex :: IsABMultiValue abMultiValue => abMultiValue -> CULong -> IO RawId
valueAtIndex abMultiValue index =
  sendMessage abMultiValue valueAtIndexSelector index

-- | @- labelAtIndex:@
labelAtIndex :: IsABMultiValue abMultiValue => abMultiValue -> CULong -> IO (Id NSString)
labelAtIndex abMultiValue index =
  sendMessage abMultiValue labelAtIndexSelector index

-- | @- identifierAtIndex:@
identifierAtIndex :: IsABMultiValue abMultiValue => abMultiValue -> CULong -> IO (Id NSString)
identifierAtIndex abMultiValue index =
  sendMessage abMultiValue identifierAtIndexSelector index

-- | @- indexForIdentifier:@
indexForIdentifier :: (IsABMultiValue abMultiValue, IsNSString identifier) => abMultiValue -> identifier -> IO CULong
indexForIdentifier abMultiValue identifier =
  sendMessage abMultiValue indexForIdentifierSelector (toNSString identifier)

-- | @- primaryIdentifier@
primaryIdentifier :: IsABMultiValue abMultiValue => abMultiValue -> IO (Id NSString)
primaryIdentifier abMultiValue =
  sendMessage abMultiValue primaryIdentifierSelector

-- | @- propertyType@
propertyType :: IsABMultiValue abMultiValue => abMultiValue -> IO CLong
propertyType abMultiValue =
  sendMessage abMultiValue propertyTypeSelector

-- | @- valueForIdentifier:@
valueForIdentifier :: (IsABMultiValue abMultiValue, IsNSString identifier) => abMultiValue -> identifier -> IO RawId
valueForIdentifier abMultiValue identifier =
  sendMessage abMultiValue valueForIdentifierSelector (toNSString identifier)

-- | @- labelForIdentifier:@
labelForIdentifier :: (IsABMultiValue abMultiValue, IsNSString identifier) => abMultiValue -> identifier -> IO RawId
labelForIdentifier abMultiValue identifier =
  sendMessage abMultiValue labelForIdentifierSelector (toNSString identifier)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @valueAtIndex:@
valueAtIndexSelector :: Selector '[CULong] RawId
valueAtIndexSelector = mkSelector "valueAtIndex:"

-- | @Selector@ for @labelAtIndex:@
labelAtIndexSelector :: Selector '[CULong] (Id NSString)
labelAtIndexSelector = mkSelector "labelAtIndex:"

-- | @Selector@ for @identifierAtIndex:@
identifierAtIndexSelector :: Selector '[CULong] (Id NSString)
identifierAtIndexSelector = mkSelector "identifierAtIndex:"

-- | @Selector@ for @indexForIdentifier:@
indexForIdentifierSelector :: Selector '[Id NSString] CULong
indexForIdentifierSelector = mkSelector "indexForIdentifier:"

-- | @Selector@ for @primaryIdentifier@
primaryIdentifierSelector :: Selector '[] (Id NSString)
primaryIdentifierSelector = mkSelector "primaryIdentifier"

-- | @Selector@ for @propertyType@
propertyTypeSelector :: Selector '[] CLong
propertyTypeSelector = mkSelector "propertyType"

-- | @Selector@ for @valueForIdentifier:@
valueForIdentifierSelector :: Selector '[Id NSString] RawId
valueForIdentifierSelector = mkSelector "valueForIdentifier:"

-- | @Selector@ for @labelForIdentifier:@
labelForIdentifierSelector :: Selector '[Id NSString] RawId
labelForIdentifierSelector = mkSelector "labelForIdentifier:"

