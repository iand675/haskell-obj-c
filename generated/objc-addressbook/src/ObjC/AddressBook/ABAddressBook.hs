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
  , sharedAddressBookSelector
  , addressBookSelector
  , recordsMatchingSearchElementSelector
  , saveSelector
  , saveAndReturnErrorSelector
  , hasUnsavedChangesSelector
  , meSelector
  , setMeSelector
  , recordForUniqueIdSelector
  , addRecord_errorSelector
  , addRecordSelector
  , removeRecord_errorSelector
  , removeRecordSelector
  , peopleSelector
  , groupsSelector
  , recordClassFromUniqueIdSelector
  , formattedAddressFromDictionarySelector
  , defaultCountryCodeSelector
  , defaultNameOrderingSelector


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

-- | @+ sharedAddressBook@
sharedAddressBook :: IO (Id ABAddressBook)
sharedAddressBook  =
  do
    cls' <- getRequiredClass "ABAddressBook"
    sendClassMsg cls' (mkSelector "sharedAddressBook") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ addressBook@
addressBook :: IO (Id ABAddressBook)
addressBook  =
  do
    cls' <- getRequiredClass "ABAddressBook"
    sendClassMsg cls' (mkSelector "addressBook") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recordsMatchingSearchElement:@
recordsMatchingSearchElement :: (IsABAddressBook abAddressBook, IsABSearchElement search) => abAddressBook -> search -> IO (Id NSArray)
recordsMatchingSearchElement abAddressBook  search =
withObjCPtr search $ \raw_search ->
    sendMsg abAddressBook (mkSelector "recordsMatchingSearchElement:") (retPtr retVoid) [argPtr (castPtr raw_search :: Ptr ())] >>= retainedObject . castPtr

-- | @- save@
save :: IsABAddressBook abAddressBook => abAddressBook -> IO Bool
save abAddressBook  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "save") retCULong []

-- | @- saveAndReturnError:@
saveAndReturnError :: (IsABAddressBook abAddressBook, IsNSError error_) => abAddressBook -> error_ -> IO Bool
saveAndReturnError abAddressBook  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "saveAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- hasUnsavedChanges@
hasUnsavedChanges :: IsABAddressBook abAddressBook => abAddressBook -> IO Bool
hasUnsavedChanges abAddressBook  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "hasUnsavedChanges") retCULong []

-- | @- me@
me :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id ABPerson)
me abAddressBook  =
  sendMsg abAddressBook (mkSelector "me") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMe:@
setMe :: (IsABAddressBook abAddressBook, IsABPerson moi) => abAddressBook -> moi -> IO ()
setMe abAddressBook  moi =
withObjCPtr moi $ \raw_moi ->
    sendMsg abAddressBook (mkSelector "setMe:") retVoid [argPtr (castPtr raw_moi :: Ptr ())]

-- | @- recordForUniqueId:@
recordForUniqueId :: (IsABAddressBook abAddressBook, IsNSString uniqueId) => abAddressBook -> uniqueId -> IO (Id ABRecord)
recordForUniqueId abAddressBook  uniqueId =
withObjCPtr uniqueId $ \raw_uniqueId ->
    sendMsg abAddressBook (mkSelector "recordForUniqueId:") (retPtr retVoid) [argPtr (castPtr raw_uniqueId :: Ptr ())] >>= retainedObject . castPtr

-- | @- addRecord:error:@
addRecord_error :: (IsABAddressBook abAddressBook, IsABRecord record, IsNSError error_) => abAddressBook -> record -> error_ -> IO Bool
addRecord_error abAddressBook  record error_ =
withObjCPtr record $ \raw_record ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "addRecord:error:") retCULong [argPtr (castPtr raw_record :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- addRecord:@
addRecord :: (IsABAddressBook abAddressBook, IsABRecord record) => abAddressBook -> record -> IO Bool
addRecord abAddressBook  record =
withObjCPtr record $ \raw_record ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "addRecord:") retCULong [argPtr (castPtr raw_record :: Ptr ())]

-- | @- removeRecord:error:@
removeRecord_error :: (IsABAddressBook abAddressBook, IsABRecord record, IsNSError error_) => abAddressBook -> record -> error_ -> IO Bool
removeRecord_error abAddressBook  record error_ =
withObjCPtr record $ \raw_record ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "removeRecord:error:") retCULong [argPtr (castPtr raw_record :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeRecord:@
removeRecord :: (IsABAddressBook abAddressBook, IsABRecord record) => abAddressBook -> record -> IO Bool
removeRecord abAddressBook  record =
withObjCPtr record $ \raw_record ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abAddressBook (mkSelector "removeRecord:") retCULong [argPtr (castPtr raw_record :: Ptr ())]

-- | @- people@
people :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id NSArray)
people abAddressBook  =
  sendMsg abAddressBook (mkSelector "people") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groups@
groups :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id NSArray)
groups abAddressBook  =
  sendMsg abAddressBook (mkSelector "groups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recordClassFromUniqueId:@
recordClassFromUniqueId :: (IsABAddressBook abAddressBook, IsNSString uniqueId) => abAddressBook -> uniqueId -> IO (Id NSString)
recordClassFromUniqueId abAddressBook  uniqueId =
withObjCPtr uniqueId $ \raw_uniqueId ->
    sendMsg abAddressBook (mkSelector "recordClassFromUniqueId:") (retPtr retVoid) [argPtr (castPtr raw_uniqueId :: Ptr ())] >>= retainedObject . castPtr

-- | @- formattedAddressFromDictionary:@
formattedAddressFromDictionary :: (IsABAddressBook abAddressBook, IsNSDictionary address) => abAddressBook -> address -> IO (Id NSAttributedString)
formattedAddressFromDictionary abAddressBook  address =
withObjCPtr address $ \raw_address ->
    sendMsg abAddressBook (mkSelector "formattedAddressFromDictionary:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ())] >>= retainedObject . castPtr

-- | @- defaultCountryCode@
defaultCountryCode :: IsABAddressBook abAddressBook => abAddressBook -> IO (Id NSString)
defaultCountryCode abAddressBook  =
  sendMsg abAddressBook (mkSelector "defaultCountryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defaultNameOrdering@
defaultNameOrdering :: IsABAddressBook abAddressBook => abAddressBook -> IO CLong
defaultNameOrdering abAddressBook  =
  sendMsg abAddressBook (mkSelector "defaultNameOrdering") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAddressBook@
sharedAddressBookSelector :: Selector
sharedAddressBookSelector = mkSelector "sharedAddressBook"

-- | @Selector@ for @addressBook@
addressBookSelector :: Selector
addressBookSelector = mkSelector "addressBook"

-- | @Selector@ for @recordsMatchingSearchElement:@
recordsMatchingSearchElementSelector :: Selector
recordsMatchingSearchElementSelector = mkSelector "recordsMatchingSearchElement:"

-- | @Selector@ for @save@
saveSelector :: Selector
saveSelector = mkSelector "save"

-- | @Selector@ for @saveAndReturnError:@
saveAndReturnErrorSelector :: Selector
saveAndReturnErrorSelector = mkSelector "saveAndReturnError:"

-- | @Selector@ for @hasUnsavedChanges@
hasUnsavedChangesSelector :: Selector
hasUnsavedChangesSelector = mkSelector "hasUnsavedChanges"

-- | @Selector@ for @me@
meSelector :: Selector
meSelector = mkSelector "me"

-- | @Selector@ for @setMe:@
setMeSelector :: Selector
setMeSelector = mkSelector "setMe:"

-- | @Selector@ for @recordForUniqueId:@
recordForUniqueIdSelector :: Selector
recordForUniqueIdSelector = mkSelector "recordForUniqueId:"

-- | @Selector@ for @addRecord:error:@
addRecord_errorSelector :: Selector
addRecord_errorSelector = mkSelector "addRecord:error:"

-- | @Selector@ for @addRecord:@
addRecordSelector :: Selector
addRecordSelector = mkSelector "addRecord:"

-- | @Selector@ for @removeRecord:error:@
removeRecord_errorSelector :: Selector
removeRecord_errorSelector = mkSelector "removeRecord:error:"

-- | @Selector@ for @removeRecord:@
removeRecordSelector :: Selector
removeRecordSelector = mkSelector "removeRecord:"

-- | @Selector@ for @people@
peopleSelector :: Selector
peopleSelector = mkSelector "people"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @recordClassFromUniqueId:@
recordClassFromUniqueIdSelector :: Selector
recordClassFromUniqueIdSelector = mkSelector "recordClassFromUniqueId:"

-- | @Selector@ for @formattedAddressFromDictionary:@
formattedAddressFromDictionarySelector :: Selector
formattedAddressFromDictionarySelector = mkSelector "formattedAddressFromDictionary:"

-- | @Selector@ for @defaultCountryCode@
defaultCountryCodeSelector :: Selector
defaultCountryCodeSelector = mkSelector "defaultCountryCode"

-- | @Selector@ for @defaultNameOrdering@
defaultNameOrderingSelector :: Selector
defaultNameOrderingSelector = mkSelector "defaultNameOrdering"

