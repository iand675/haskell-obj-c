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
  , valueAtIndexSelector
  , labelAtIndexSelector
  , identifierAtIndexSelector
  , indexForIdentifierSelector
  , primaryIdentifierSelector
  , propertyTypeSelector
  , valueForIdentifierSelector
  , labelForIdentifierSelector


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

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- count@
count :: IsABMultiValue abMultiValue => abMultiValue -> IO CULong
count abMultiValue  =
  sendMsg abMultiValue (mkSelector "count") retCULong []

-- | @- valueAtIndex:@
valueAtIndex :: IsABMultiValue abMultiValue => abMultiValue -> CULong -> IO RawId
valueAtIndex abMultiValue  index =
  fmap (RawId . castPtr) $ sendMsg abMultiValue (mkSelector "valueAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- labelAtIndex:@
labelAtIndex :: IsABMultiValue abMultiValue => abMultiValue -> CULong -> IO (Id NSString)
labelAtIndex abMultiValue  index =
  sendMsg abMultiValue (mkSelector "labelAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- identifierAtIndex:@
identifierAtIndex :: IsABMultiValue abMultiValue => abMultiValue -> CULong -> IO (Id NSString)
identifierAtIndex abMultiValue  index =
  sendMsg abMultiValue (mkSelector "identifierAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- indexForIdentifier:@
indexForIdentifier :: (IsABMultiValue abMultiValue, IsNSString identifier) => abMultiValue -> identifier -> IO CULong
indexForIdentifier abMultiValue  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg abMultiValue (mkSelector "indexForIdentifier:") retCULong [argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- primaryIdentifier@
primaryIdentifier :: IsABMultiValue abMultiValue => abMultiValue -> IO (Id NSString)
primaryIdentifier abMultiValue  =
  sendMsg abMultiValue (mkSelector "primaryIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- propertyType@
propertyType :: IsABMultiValue abMultiValue => abMultiValue -> IO CLong
propertyType abMultiValue  =
  sendMsg abMultiValue (mkSelector "propertyType") retCLong []

-- | @- valueForIdentifier:@
valueForIdentifier :: (IsABMultiValue abMultiValue, IsNSString identifier) => abMultiValue -> identifier -> IO RawId
valueForIdentifier abMultiValue  identifier =
withObjCPtr identifier $ \raw_identifier ->
    fmap (RawId . castPtr) $ sendMsg abMultiValue (mkSelector "valueForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- labelForIdentifier:@
labelForIdentifier :: (IsABMultiValue abMultiValue, IsNSString identifier) => abMultiValue -> identifier -> IO RawId
labelForIdentifier abMultiValue  identifier =
withObjCPtr identifier $ \raw_identifier ->
    fmap (RawId . castPtr) $ sendMsg abMultiValue (mkSelector "labelForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @valueAtIndex:@
valueAtIndexSelector :: Selector
valueAtIndexSelector = mkSelector "valueAtIndex:"

-- | @Selector@ for @labelAtIndex:@
labelAtIndexSelector :: Selector
labelAtIndexSelector = mkSelector "labelAtIndex:"

-- | @Selector@ for @identifierAtIndex:@
identifierAtIndexSelector :: Selector
identifierAtIndexSelector = mkSelector "identifierAtIndex:"

-- | @Selector@ for @indexForIdentifier:@
indexForIdentifierSelector :: Selector
indexForIdentifierSelector = mkSelector "indexForIdentifier:"

-- | @Selector@ for @primaryIdentifier@
primaryIdentifierSelector :: Selector
primaryIdentifierSelector = mkSelector "primaryIdentifier"

-- | @Selector@ for @propertyType@
propertyTypeSelector :: Selector
propertyTypeSelector = mkSelector "propertyType"

-- | @Selector@ for @valueForIdentifier:@
valueForIdentifierSelector :: Selector
valueForIdentifierSelector = mkSelector "valueForIdentifier:"

-- | @Selector@ for @labelForIdentifier:@
labelForIdentifierSelector :: Selector
labelForIdentifierSelector = mkSelector "labelForIdentifier:"

