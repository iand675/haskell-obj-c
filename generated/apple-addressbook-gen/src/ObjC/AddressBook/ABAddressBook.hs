{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABAddressBook@.
module ObjC.AddressBook.ABAddressBook
  ( ABAddressBook
  , IsABAddressBook(..)
  , sharedAddressBook
  , addressBook
  , recordsMatchingSearchElement
  , save
  , saveAndReturnError
  , hasUnsavedChanges
  , me
  , setMe
  , recordForUniqueId
  , addRecord_error
  , addRecord
  , removeRecord_error
  , removeRecord
  , people
  , groups
  , recordClassFromUniqueId
  , formattedAddressFromDictionary
  , defaultCountryCode
  , defaultNameOrdering
  , addRecordSelector
  , addRecord_errorSelector
  , addressBookSelector
  , defaultCountryCodeSelector
  , defaultNameOrderingSelector
  , formattedAddressFromDictionarySelector
  , groupsSelector
  , hasUnsavedChangesSelector
  , meSelector
  , peopleSelector
  , recordClassFromUniqueIdSelector
  , recordForUniqueIdSelector
  , recordsMatchingSearchElementSelector
  , removeRecordSelector
  , removeRecord_errorSelector
  , saveAndReturnErrorSelector
  , saveSelector
  , setMeSelector
  , sharedAddressBookSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedAddressBook@
sharedAddressBook :: IO (Id ABAddressBook)
sharedAddressBook  =
  do
    cls' <- getRequiredClass "ABAddressBook"
    sendClassMessage cls' sharedAddressBookSelector

-- | @+ addressBook@
addressBook :: IO (Id ABAddressBook)
addressBook  =
  do
    cls' <- getRequiredClass "ABAddressBook"
    sendClassMessage cls' addressBookSelector

-- | @- recordsMatchingSearchElement:@
recordsMatchingSearchElement :: (IsABAddressBook abAddressBook, IsABSearchElement search) => abAddressBook -> search -> IO (Id NSArray)
recordsMatchingSearchElement abAddressBook search =
  sendMessage abAddressBook recordsMatchingSearchElementSelector (toABSearchElement search)

-- | @- save@
save :: IsABAddressBook abAddressBook => abAddressBook -> IO Bool
save abAddressBook =
  sendMessage abAddressBook saveSelector

-- | @- saveAndReturnError:@
saveAndReturnError :: (IsABAddressBook abAddressBook, IsNSError error_) => abAddressBook -> error_ -> IO Bool
saveAndReturnError abAddressBook error_ =
  sendMessage abAddressBook saveAndReturnErrorSelector (toNSError error_)

-- | @- hasUnsavedChanges@
hasUnsavedChanges :: IsABAddressBook abAddressBook => abAddressBook -> IO Bool
hasUnsavedChanges abAddressBook =
  sendMessage abAddressBook hasUnsavedChangesSelector

-- | @- me@
me :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id ABPerson)
me abAddressBook =
  sendMessage abAddressBook meSelector

-- | @- setMe:@
setMe :: (IsABAddressBook abAddressBook, IsABPerson moi) => abAddressBook -> moi -> IO ()
setMe abAddressBook moi =
  sendMessage abAddressBook setMeSelector (toABPerson moi)

-- | @- recordForUniqueId:@
recordForUniqueId :: (IsABAddressBook abAddressBook, IsNSString uniqueId) => abAddressBook -> uniqueId -> IO (Id ABRecord)
recordForUniqueId abAddressBook uniqueId =
  sendMessage abAddressBook recordForUniqueIdSelector (toNSString uniqueId)

-- | @- addRecord:error:@
addRecord_error :: (IsABAddressBook abAddressBook, IsABRecord record, IsNSError error_) => abAddressBook -> record -> error_ -> IO Bool
addRecord_error abAddressBook record error_ =
  sendMessage abAddressBook addRecord_errorSelector (toABRecord record) (toNSError error_)

-- | @- addRecord:@
addRecord :: (IsABAddressBook abAddressBook, IsABRecord record) => abAddressBook -> record -> IO Bool
addRecord abAddressBook record =
  sendMessage abAddressBook addRecordSelector (toABRecord record)

-- | @- removeRecord:error:@
removeRecord_error :: (IsABAddressBook abAddressBook, IsABRecord record, IsNSError error_) => abAddressBook -> record -> error_ -> IO Bool
removeRecord_error abAddressBook record error_ =
  sendMessage abAddressBook removeRecord_errorSelector (toABRecord record) (toNSError error_)

-- | @- removeRecord:@
removeRecord :: (IsABAddressBook abAddressBook, IsABRecord record) => abAddressBook -> record -> IO Bool
removeRecord abAddressBook record =
  sendMessage abAddressBook removeRecordSelector (toABRecord record)

-- | @- people@
people :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id NSArray)
people abAddressBook =
  sendMessage abAddressBook peopleSelector

-- | @- groups@
groups :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id NSArray)
groups abAddressBook =
  sendMessage abAddressBook groupsSelector

-- | @- recordClassFromUniqueId:@
recordClassFromUniqueId :: (IsABAddressBook abAddressBook, IsNSString uniqueId) => abAddressBook -> uniqueId -> IO (Id NSString)
recordClassFromUniqueId abAddressBook uniqueId =
  sendMessage abAddressBook recordClassFromUniqueIdSelector (toNSString uniqueId)

-- | @- formattedAddressFromDictionary:@
formattedAddressFromDictionary :: (IsABAddressBook abAddressBook, IsNSDictionary address) => abAddressBook -> address -> IO (Id NSAttributedString)
formattedAddressFromDictionary abAddressBook address =
  sendMessage abAddressBook formattedAddressFromDictionarySelector (toNSDictionary address)

-- | @- defaultCountryCode@
defaultCountryCode :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id NSString)
defaultCountryCode abAddressBook =
  sendMessage abAddressBook defaultCountryCodeSelector

-- | @- defaultNameOrdering@
defaultNameOrdering :: IsABAddressBook abAddressBook => abAddressBook -> IO CLong
defaultNameOrdering abAddressBook =
  sendMessage abAddressBook defaultNameOrderingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAddressBook@
sharedAddressBookSelector :: Selector '[] (Id ABAddressBook)
sharedAddressBookSelector = mkSelector "sharedAddressBook"

-- | @Selector@ for @addressBook@
addressBookSelector :: Selector '[] (Id ABAddressBook)
addressBookSelector = mkSelector "addressBook"

-- | @Selector@ for @recordsMatchingSearchElement:@
recordsMatchingSearchElementSelector :: Selector '[Id ABSearchElement] (Id NSArray)
recordsMatchingSearchElementSelector = mkSelector "recordsMatchingSearchElement:"

-- | @Selector@ for @save@
saveSelector :: Selector '[] Bool
saveSelector = mkSelector "save"

-- | @Selector@ for @saveAndReturnError:@
saveAndReturnErrorSelector :: Selector '[Id NSError] Bool
saveAndReturnErrorSelector = mkSelector "saveAndReturnError:"

-- | @Selector@ for @hasUnsavedChanges@
hasUnsavedChangesSelector :: Selector '[] Bool
hasUnsavedChangesSelector = mkSelector "hasUnsavedChanges"

-- | @Selector@ for @me@
meSelector :: Selector '[] (Id ABPerson)
meSelector = mkSelector "me"

-- | @Selector@ for @setMe:@
setMeSelector :: Selector '[Id ABPerson] ()
setMeSelector = mkSelector "setMe:"

-- | @Selector@ for @recordForUniqueId:@
recordForUniqueIdSelector :: Selector '[Id NSString] (Id ABRecord)
recordForUniqueIdSelector = mkSelector "recordForUniqueId:"

-- | @Selector@ for @addRecord:error:@
addRecord_errorSelector :: Selector '[Id ABRecord, Id NSError] Bool
addRecord_errorSelector = mkSelector "addRecord:error:"

-- | @Selector@ for @addRecord:@
addRecordSelector :: Selector '[Id ABRecord] Bool
addRecordSelector = mkSelector "addRecord:"

-- | @Selector@ for @removeRecord:error:@
removeRecord_errorSelector :: Selector '[Id ABRecord, Id NSError] Bool
removeRecord_errorSelector = mkSelector "removeRecord:error:"

-- | @Selector@ for @removeRecord:@
removeRecordSelector :: Selector '[Id ABRecord] Bool
removeRecordSelector = mkSelector "removeRecord:"

-- | @Selector@ for @people@
peopleSelector :: Selector '[] (Id NSArray)
peopleSelector = mkSelector "people"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] (Id NSArray)
groupsSelector = mkSelector "groups"

-- | @Selector@ for @recordClassFromUniqueId:@
recordClassFromUniqueIdSelector :: Selector '[Id NSString] (Id NSString)
recordClassFromUniqueIdSelector = mkSelector "recordClassFromUniqueId:"

-- | @Selector@ for @formattedAddressFromDictionary:@
formattedAddressFromDictionarySelector :: Selector '[Id NSDictionary] (Id NSAttributedString)
formattedAddressFromDictionarySelector = mkSelector "formattedAddressFromDictionary:"

-- | @Selector@ for @defaultCountryCode@
defaultCountryCodeSelector :: Selector '[] (Id NSString)
defaultCountryCodeSelector = mkSelector "defaultCountryCode"

-- | @Selector@ for @defaultNameOrdering@
defaultNameOrderingSelector :: Selector '[] CLong
defaultNameOrderingSelector = mkSelector "defaultNameOrdering"

