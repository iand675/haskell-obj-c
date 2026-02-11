{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABRecord@.
module ObjC.AddressBook.ABRecord
  ( ABRecord
  , IsABRecord(..)
  , init_
  , initWithAddressBook
  , valueForProperty
  , setValue_forProperty_error
  , setValue_forProperty
  , removeValueForProperty
  , isReadOnly
  , uniqueId
  , displayName
  , initSelector
  , initWithAddressBookSelector
  , valueForPropertySelector
  , setValue_forProperty_errorSelector
  , setValue_forPropertySelector
  , removeValueForPropertySelector
  , isReadOnlySelector
  , uniqueIdSelector
  , displayNameSelector


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

-- | @- init@
init_ :: IsABRecord abRecord => abRecord -> IO RawId
init_ abRecord  =
  fmap (RawId . castPtr) $ sendMsg abRecord (mkSelector "init") (retPtr retVoid) []

-- | @- initWithAddressBook:@
initWithAddressBook :: (IsABRecord abRecord, IsABAddressBook addressBook) => abRecord -> addressBook -> IO RawId
initWithAddressBook abRecord  addressBook =
withObjCPtr addressBook $ \raw_addressBook ->
    fmap (RawId . castPtr) $ sendMsg abRecord (mkSelector "initWithAddressBook:") (retPtr retVoid) [argPtr (castPtr raw_addressBook :: Ptr ())]

-- | @- valueForProperty:@
valueForProperty :: (IsABRecord abRecord, IsNSString property) => abRecord -> property -> IO RawId
valueForProperty abRecord  property =
withObjCPtr property $ \raw_property ->
    fmap (RawId . castPtr) $ sendMsg abRecord (mkSelector "valueForProperty:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ())]

-- | @- setValue:forProperty:error:@
setValue_forProperty_error :: (IsABRecord abRecord, IsNSString property, IsNSError error_) => abRecord -> RawId -> property -> error_ -> IO Bool
setValue_forProperty_error abRecord  value property error_ =
withObjCPtr property $ \raw_property ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg abRecord (mkSelector "setValue:forProperty:error:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- setValue:forProperty:@
setValue_forProperty :: (IsABRecord abRecord, IsNSString property) => abRecord -> RawId -> property -> IO Bool
setValue_forProperty abRecord  value property =
withObjCPtr property $ \raw_property ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abRecord (mkSelector "setValue:forProperty:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())]

-- | @- removeValueForProperty:@
removeValueForProperty :: (IsABRecord abRecord, IsNSString property) => abRecord -> property -> IO Bool
removeValueForProperty abRecord  property =
withObjCPtr property $ \raw_property ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abRecord (mkSelector "removeValueForProperty:") retCULong [argPtr (castPtr raw_property :: Ptr ())]

-- | @- isReadOnly@
isReadOnly :: IsABRecord abRecord => abRecord -> IO Bool
isReadOnly abRecord  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg abRecord (mkSelector "isReadOnly") retCULong []

-- | @- uniqueId@
uniqueId :: IsABRecord abRecord => abRecord -> IO (Id NSString)
uniqueId abRecord  =
  sendMsg abRecord (mkSelector "uniqueId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsABRecord abRecord => abRecord -> IO (Id NSString)
displayName abRecord  =
  sendMsg abRecord (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAddressBook:@
initWithAddressBookSelector :: Selector
initWithAddressBookSelector = mkSelector "initWithAddressBook:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @setValue:forProperty:error:@
setValue_forProperty_errorSelector :: Selector
setValue_forProperty_errorSelector = mkSelector "setValue:forProperty:error:"

-- | @Selector@ for @setValue:forProperty:@
setValue_forPropertySelector :: Selector
setValue_forPropertySelector = mkSelector "setValue:forProperty:"

-- | @Selector@ for @removeValueForProperty:@
removeValueForPropertySelector :: Selector
removeValueForPropertySelector = mkSelector "removeValueForProperty:"

-- | @Selector@ for @isReadOnly@
isReadOnlySelector :: Selector
isReadOnlySelector = mkSelector "isReadOnly"

-- | @Selector@ for @uniqueId@
uniqueIdSelector :: Selector
uniqueIdSelector = mkSelector "uniqueId"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

