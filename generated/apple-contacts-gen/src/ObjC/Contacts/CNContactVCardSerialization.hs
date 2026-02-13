{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contact vCard support.
--
-- This converts between a contact and its vCard representation.
--
-- Generated bindings for @CNContactVCardSerialization@.
module ObjC.Contacts.CNContactVCardSerialization
  ( CNContactVCardSerialization
  , IsCNContactVCardSerialization(..)
  , descriptorForRequiredKeys
  , dataWithContacts_error
  , contactsWithData_error
  , contactsWithData_errorSelector
  , dataWithContacts_errorSelector
  , descriptorForRequiredKeysSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Descriptor for all contact keys required by vCard serialization
--
-- This descriptor must be passed to the fetch request if the returned              contacts are to be serialized with dataWithContacts:error:.
--
-- ObjC selector: @+ descriptorForRequiredKeys@
descriptorForRequiredKeys :: IO RawId
descriptorForRequiredKeys  =
  do
    cls' <- getRequiredClass "CNContactVCardSerialization"
    sendClassMessage cls' descriptorForRequiredKeysSelector

-- | Serialize contacts to data.
--
-- The contacts to be serialized must have been fetched with              @+descriptorForRequiredKeys.@
--
-- @contacts@ — The contacts to serialize.
--
-- @error@ — An optional outparameter. If the serialization fails, this will be set.
--
-- The encoded data. If the serialization fails, this will be @nil.@
--
-- ObjC selector: @+ dataWithContacts:error:@
dataWithContacts_error :: (IsNSArray contacts, IsNSError error_) => contacts -> error_ -> IO (Id NSData)
dataWithContacts_error contacts error_ =
  do
    cls' <- getRequiredClass "CNContactVCardSerialization"
    sendClassMessage cls' dataWithContacts_errorSelector (toNSArray contacts) (toNSError error_)

-- | Parse data into contacts.
--
-- @data@ — The data to parse.
--
-- @error@ — An optional outparameter. If the parsing fails, this will be set.
--
-- The parsed contacts. If the parsing fails, this will be @nil.@
--
-- ObjC selector: @+ contactsWithData:error:@
contactsWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO (Id NSArray)
contactsWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "CNContactVCardSerialization"
    sendClassMessage cls' contactsWithData_errorSelector (toNSData data_) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorForRequiredKeys@
descriptorForRequiredKeysSelector :: Selector '[] RawId
descriptorForRequiredKeysSelector = mkSelector "descriptorForRequiredKeys"

-- | @Selector@ for @dataWithContacts:error:@
dataWithContacts_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSData)
dataWithContacts_errorSelector = mkSelector "dataWithContacts:error:"

-- | @Selector@ for @contactsWithData:error:@
contactsWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id NSArray)
contactsWithData_errorSelector = mkSelector "contactsWithData:error:"

