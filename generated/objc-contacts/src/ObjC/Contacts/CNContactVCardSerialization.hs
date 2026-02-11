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
  , descriptorForRequiredKeysSelector
  , dataWithContacts_errorSelector
  , contactsWithData_errorSelector


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
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "descriptorForRequiredKeys") (retPtr retVoid) []

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
    withObjCPtr contacts $ \raw_contacts ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "dataWithContacts:error:") (retPtr retVoid) [argPtr (castPtr raw_contacts :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "contactsWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorForRequiredKeys@
descriptorForRequiredKeysSelector :: Selector
descriptorForRequiredKeysSelector = mkSelector "descriptorForRequiredKeys"

-- | @Selector@ for @dataWithContacts:error:@
dataWithContacts_errorSelector :: Selector
dataWithContacts_errorSelector = mkSelector "dataWithContacts:error:"

-- | @Selector@ for @contactsWithData:error:@
contactsWithData_errorSelector :: Selector
contactsWithData_errorSelector = mkSelector "contactsWithData:error:"

