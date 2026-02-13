{-# LANGUAGE DataKinds #-}
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
  , displayNameSelector
  , initSelector
  , initWithAddressBookSelector
  , isReadOnlySelector
  , removeValueForPropertySelector
  , setValue_forPropertySelector
  , setValue_forProperty_errorSelector
  , uniqueIdSelector
  , valueForPropertySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsABRecord abRecord => abRecord -> IO RawId
init_ abRecord =
  sendOwnedMessage abRecord initSelector

-- | @- initWithAddressBook:@
initWithAddressBook :: (IsABRecord abRecord, IsABAddressBook addressBook) => abRecord -> addressBook -> IO RawId
initWithAddressBook abRecord addressBook =
  sendOwnedMessage abRecord initWithAddressBookSelector (toABAddressBook addressBook)

-- | @- valueForProperty:@
valueForProperty :: (IsABRecord abRecord, IsNSString property) => abRecord -> property -> IO RawId
valueForProperty abRecord property =
  sendMessage abRecord valueForPropertySelector (toNSString property)

-- | @- setValue:forProperty:error:@
setValue_forProperty_error :: (IsABRecord abRecord, IsNSString property, IsNSError error_) => abRecord -> RawId -> property -> error_ -> IO Bool
setValue_forProperty_error abRecord value property error_ =
  sendMessage abRecord setValue_forProperty_errorSelector value (toNSString property) (toNSError error_)

-- | @- setValue:forProperty:@
setValue_forProperty :: (IsABRecord abRecord, IsNSString property) => abRecord -> RawId -> property -> IO Bool
setValue_forProperty abRecord value property =
  sendMessage abRecord setValue_forPropertySelector value (toNSString property)

-- | @- removeValueForProperty:@
removeValueForProperty :: (IsABRecord abRecord, IsNSString property) => abRecord -> property -> IO Bool
removeValueForProperty abRecord property =
  sendMessage abRecord removeValueForPropertySelector (toNSString property)

-- | @- isReadOnly@
isReadOnly :: IsABRecord abRecord => abRecord -> IO Bool
isReadOnly abRecord =
  sendMessage abRecord isReadOnlySelector

-- | @- uniqueId@
uniqueId :: IsABRecord abRecord => abRecord -> IO (Id NSString)
uniqueId abRecord =
  sendMessage abRecord uniqueIdSelector

-- | @- displayName@
displayName :: IsABRecord abRecord => abRecord -> IO (Id NSString)
displayName abRecord =
  sendMessage abRecord displayNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAddressBook:@
initWithAddressBookSelector :: Selector '[Id ABAddressBook] RawId
initWithAddressBookSelector = mkSelector "initWithAddressBook:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector '[Id NSString] RawId
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @setValue:forProperty:error:@
setValue_forProperty_errorSelector :: Selector '[RawId, Id NSString, Id NSError] Bool
setValue_forProperty_errorSelector = mkSelector "setValue:forProperty:error:"

-- | @Selector@ for @setValue:forProperty:@
setValue_forPropertySelector :: Selector '[RawId, Id NSString] Bool
setValue_forPropertySelector = mkSelector "setValue:forProperty:"

-- | @Selector@ for @removeValueForProperty:@
removeValueForPropertySelector :: Selector '[Id NSString] Bool
removeValueForPropertySelector = mkSelector "removeValueForProperty:"

-- | @Selector@ for @isReadOnly@
isReadOnlySelector :: Selector '[] Bool
isReadOnlySelector = mkSelector "isReadOnly"

-- | @Selector@ for @uniqueId@
uniqueIdSelector :: Selector '[] (Id NSString)
uniqueIdSelector = mkSelector "uniqueId"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

