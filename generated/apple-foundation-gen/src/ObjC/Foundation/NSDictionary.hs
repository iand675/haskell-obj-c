{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Dictionary	***************
--
-- Generated bindings for @NSDictionary@.
module ObjC.Foundation.NSDictionary
  ( NSDictionary
  , IsNSDictionary(..)
  , objectForKey
  , keyEnumerator
  , init_
  , initWithObjects_forKeys_count
  , initWithCoder
  , valueForKey
  , fileSize
  , fileModificationDate
  , fileType
  , filePosixPermissions
  , fileOwnerAccountName
  , fileGroupOwnerAccountName
  , fileSystemNumber
  , fileSystemFileNumber
  , fileExtensionHidden
  , fileHFSCreatorCode
  , fileHFSTypeCode
  , fileIsImmutable
  , fileIsAppendOnly
  , fileCreationDate
  , fileOwnerAccountID
  , fileGroupOwnerAccountID
  , countByEnumeratingWithState_objects_count
  , sharedKeySetForKeys
  , dictionary
  , dictionaryWithObject_forKey
  , dictionaryWithObjects_forKeys_count
  , dictionaryWithObjectsAndKeys
  , dictionaryWithDictionary
  , dictionaryWithObjects_forKeys
  , initWithObjectsAndKeys
  , initWithDictionary
  , initWithDictionary_copyItems
  , initWithObjects_forKeys
  , initWithContentsOfURL_error
  , dictionaryWithContentsOfURL_error
  , getObjects_andKeys
  , dictionaryWithContentsOfFile
  , dictionaryWithContentsOfURL
  , initWithContentsOfFile
  , initWithContentsOfURL
  , writeToFile_atomically
  , writeToURL_atomically
  , allKeysForObject
  , descriptionWithLocale
  , descriptionWithLocale_indent
  , isEqualToDictionary
  , objectEnumerator
  , objectsForKeys_notFoundMarker
  , writeToURL_error
  , keysSortedByValueUsingSelector
  , getObjects_andKeys_count
  , objectForKeyedSubscript
  , enumerateKeysAndObjectsUsingBlock
  , enumerateKeysAndObjectsWithOptions_usingBlock
  , keysSortedByValueUsingComparator
  , keysSortedByValueWithOptions_usingComparator
  , keysOfEntriesPassingTest
  , keysOfEntriesWithOptions_passingTest
  , count
  , allKeys
  , allValues
  , description
  , descriptionInStringsFileFormat
  , allKeysForObjectSelector
  , allKeysSelector
  , allValuesSelector
  , countByEnumeratingWithState_objects_countSelector
  , countSelector
  , descriptionInStringsFileFormatSelector
  , descriptionSelector
  , descriptionWithLocaleSelector
  , descriptionWithLocale_indentSelector
  , dictionarySelector
  , dictionaryWithContentsOfFileSelector
  , dictionaryWithContentsOfURLSelector
  , dictionaryWithContentsOfURL_errorSelector
  , dictionaryWithDictionarySelector
  , dictionaryWithObject_forKeySelector
  , dictionaryWithObjectsAndKeysSelector
  , dictionaryWithObjects_forKeysSelector
  , dictionaryWithObjects_forKeys_countSelector
  , enumerateKeysAndObjectsUsingBlockSelector
  , enumerateKeysAndObjectsWithOptions_usingBlockSelector
  , fileCreationDateSelector
  , fileExtensionHiddenSelector
  , fileGroupOwnerAccountIDSelector
  , fileGroupOwnerAccountNameSelector
  , fileHFSCreatorCodeSelector
  , fileHFSTypeCodeSelector
  , fileIsAppendOnlySelector
  , fileIsImmutableSelector
  , fileModificationDateSelector
  , fileOwnerAccountIDSelector
  , fileOwnerAccountNameSelector
  , filePosixPermissionsSelector
  , fileSizeSelector
  , fileSystemFileNumberSelector
  , fileSystemNumberSelector
  , fileTypeSelector
  , getObjects_andKeysSelector
  , getObjects_andKeys_countSelector
  , initSelector
  , initWithCoderSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , initWithContentsOfURL_errorSelector
  , initWithDictionarySelector
  , initWithDictionary_copyItemsSelector
  , initWithObjectsAndKeysSelector
  , initWithObjects_forKeysSelector
  , initWithObjects_forKeys_countSelector
  , isEqualToDictionarySelector
  , keyEnumeratorSelector
  , keysOfEntriesPassingTestSelector
  , keysOfEntriesWithOptions_passingTestSelector
  , keysSortedByValueUsingComparatorSelector
  , keysSortedByValueUsingSelectorSelector
  , keysSortedByValueWithOptions_usingComparatorSelector
  , objectEnumeratorSelector
  , objectForKeySelector
  , objectForKeyedSubscriptSelector
  , objectsForKeys_notFoundMarkerSelector
  , sharedKeySetForKeysSelector
  , valueForKeySelector
  , writeToFile_atomicallySelector
  , writeToURL_atomicallySelector
  , writeToURL_errorSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse
  , NSSortOptions(NSSortOptions)
  , pattern NSSortConcurrent
  , pattern NSSortStable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- objectForKey:@
objectForKey :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO RawId
objectForKey nsDictionary aKey =
  sendMessage nsDictionary objectForKeySelector aKey

-- | @- keyEnumerator@
keyEnumerator :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSEnumerator)
keyEnumerator nsDictionary =
  sendMessage nsDictionary keyEnumeratorSelector

-- | @- init@
init_ :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSDictionary)
init_ nsDictionary =
  sendOwnedMessage nsDictionary initSelector

-- | @- initWithObjects:forKeys:count:@
initWithObjects_forKeys_count :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> RawId -> CULong -> IO (Id NSDictionary)
initWithObjects_forKeys_count nsDictionary objects keys cnt =
  sendOwnedMessage nsDictionary initWithObjects_forKeys_countSelector objects keys cnt

-- | @- initWithCoder:@
initWithCoder :: (IsNSDictionary nsDictionary, IsNSCoder coder) => nsDictionary -> coder -> IO (Id NSDictionary)
initWithCoder nsDictionary coder =
  sendOwnedMessage nsDictionary initWithCoderSelector (toNSCoder coder)

-- | @- valueForKey:@
valueForKey :: (IsNSDictionary nsDictionary, IsNSString key) => nsDictionary -> key -> IO RawId
valueForKey nsDictionary key =
  sendMessage nsDictionary valueForKeySelector (toNSString key)

-- | @- fileSize@
fileSize :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
fileSize nsDictionary =
  sendMessage nsDictionary fileSizeSelector

-- | @- fileModificationDate@
fileModificationDate :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSDate)
fileModificationDate nsDictionary =
  sendMessage nsDictionary fileModificationDateSelector

-- | @- fileType@
fileType :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
fileType nsDictionary =
  sendMessage nsDictionary fileTypeSelector

-- | @- filePosixPermissions@
filePosixPermissions :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
filePosixPermissions nsDictionary =
  sendMessage nsDictionary filePosixPermissionsSelector

-- | @- fileOwnerAccountName@
fileOwnerAccountName :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
fileOwnerAccountName nsDictionary =
  sendMessage nsDictionary fileOwnerAccountNameSelector

-- | @- fileGroupOwnerAccountName@
fileGroupOwnerAccountName :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
fileGroupOwnerAccountName nsDictionary =
  sendMessage nsDictionary fileGroupOwnerAccountNameSelector

-- | @- fileSystemNumber@
fileSystemNumber :: IsNSDictionary nsDictionary => nsDictionary -> IO CLong
fileSystemNumber nsDictionary =
  sendMessage nsDictionary fileSystemNumberSelector

-- | @- fileSystemFileNumber@
fileSystemFileNumber :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
fileSystemFileNumber nsDictionary =
  sendMessage nsDictionary fileSystemFileNumberSelector

-- | @- fileExtensionHidden@
fileExtensionHidden :: IsNSDictionary nsDictionary => nsDictionary -> IO Bool
fileExtensionHidden nsDictionary =
  sendMessage nsDictionary fileExtensionHiddenSelector

-- | @- fileHFSCreatorCode@
fileHFSCreatorCode :: IsNSDictionary nsDictionary => nsDictionary -> IO CUInt
fileHFSCreatorCode nsDictionary =
  sendMessage nsDictionary fileHFSCreatorCodeSelector

-- | @- fileHFSTypeCode@
fileHFSTypeCode :: IsNSDictionary nsDictionary => nsDictionary -> IO CUInt
fileHFSTypeCode nsDictionary =
  sendMessage nsDictionary fileHFSTypeCodeSelector

-- | @- fileIsImmutable@
fileIsImmutable :: IsNSDictionary nsDictionary => nsDictionary -> IO Bool
fileIsImmutable nsDictionary =
  sendMessage nsDictionary fileIsImmutableSelector

-- | @- fileIsAppendOnly@
fileIsAppendOnly :: IsNSDictionary nsDictionary => nsDictionary -> IO Bool
fileIsAppendOnly nsDictionary =
  sendMessage nsDictionary fileIsAppendOnlySelector

-- | @- fileCreationDate@
fileCreationDate :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSDate)
fileCreationDate nsDictionary =
  sendMessage nsDictionary fileCreationDateSelector

-- | @- fileOwnerAccountID@
fileOwnerAccountID :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSNumber)
fileOwnerAccountID nsDictionary =
  sendMessage nsDictionary fileOwnerAccountIDSelector

-- | @- fileGroupOwnerAccountID@
fileGroupOwnerAccountID :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSNumber)
fileGroupOwnerAccountID nsDictionary =
  sendMessage nsDictionary fileGroupOwnerAccountIDSelector

-- | @- countByEnumeratingWithState:objects:count:@
countByEnumeratingWithState_objects_count :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> RawId -> CULong -> IO CULong
countByEnumeratingWithState_objects_count nsDictionary state buffer len =
  sendMessage nsDictionary countByEnumeratingWithState_objects_countSelector state buffer len

-- | @+ sharedKeySetForKeys:@
sharedKeySetForKeys :: IsNSArray keys => keys -> IO RawId
sharedKeySetForKeys keys =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' sharedKeySetForKeysSelector (toNSArray keys)

-- | @+ dictionary@
dictionary :: IO (Id NSDictionary)
dictionary  =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionarySelector

-- | @+ dictionaryWithObject:forKey:@
dictionaryWithObject_forKey :: RawId -> RawId -> IO (Id NSDictionary)
dictionaryWithObject_forKey object key =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithObject_forKeySelector object key

-- | @+ dictionaryWithObjects:forKeys:count:@
dictionaryWithObjects_forKeys_count :: RawId -> RawId -> CULong -> IO (Id NSDictionary)
dictionaryWithObjects_forKeys_count objects keys cnt =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithObjects_forKeys_countSelector objects keys cnt

-- | @+ dictionaryWithObjectsAndKeys:@
dictionaryWithObjectsAndKeys :: RawId -> IO (Id NSDictionary)
dictionaryWithObjectsAndKeys firstObject =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithObjectsAndKeysSelector firstObject

-- | @+ dictionaryWithDictionary:@
dictionaryWithDictionary :: IsNSDictionary dict => dict -> IO (Id NSDictionary)
dictionaryWithDictionary dict =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithDictionarySelector (toNSDictionary dict)

-- | @+ dictionaryWithObjects:forKeys:@
dictionaryWithObjects_forKeys :: (IsNSArray objects, IsNSArray keys) => objects -> keys -> IO (Id NSDictionary)
dictionaryWithObjects_forKeys objects keys =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithObjects_forKeysSelector (toNSArray objects) (toNSArray keys)

-- | @- initWithObjectsAndKeys:@
initWithObjectsAndKeys :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO (Id NSDictionary)
initWithObjectsAndKeys nsDictionary firstObject =
  sendOwnedMessage nsDictionary initWithObjectsAndKeysSelector firstObject

-- | @- initWithDictionary:@
initWithDictionary :: (IsNSDictionary nsDictionary, IsNSDictionary otherDictionary) => nsDictionary -> otherDictionary -> IO (Id NSDictionary)
initWithDictionary nsDictionary otherDictionary =
  sendOwnedMessage nsDictionary initWithDictionarySelector (toNSDictionary otherDictionary)

-- | @- initWithDictionary:copyItems:@
initWithDictionary_copyItems :: (IsNSDictionary nsDictionary, IsNSDictionary otherDictionary) => nsDictionary -> otherDictionary -> Bool -> IO (Id NSDictionary)
initWithDictionary_copyItems nsDictionary otherDictionary flag =
  sendOwnedMessage nsDictionary initWithDictionary_copyItemsSelector (toNSDictionary otherDictionary) flag

-- | @- initWithObjects:forKeys:@
initWithObjects_forKeys :: (IsNSDictionary nsDictionary, IsNSArray objects, IsNSArray keys) => nsDictionary -> objects -> keys -> IO (Id NSDictionary)
initWithObjects_forKeys nsDictionary objects keys =
  sendOwnedMessage nsDictionary initWithObjects_forKeysSelector (toNSArray objects) (toNSArray keys)

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNSDictionary nsDictionary, IsNSURL url, IsNSError error_) => nsDictionary -> url -> error_ -> IO (Id NSDictionary)
initWithContentsOfURL_error nsDictionary url error_ =
  sendOwnedMessage nsDictionary initWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @+ dictionaryWithContentsOfURL:error:@
dictionaryWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSDictionary)
dictionaryWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | This method is unsafe because it could potentially cause buffer overruns. You should use -getObjects:andKeys:count:
--
-- ObjC selector: @- getObjects:andKeys:@
getObjects_andKeys :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> RawId -> IO ()
getObjects_andKeys nsDictionary objects keys =
  sendMessage nsDictionary getObjects_andKeysSelector objects keys

-- | @+ dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFile :: IsNSString path => path -> IO (Id NSDictionary)
dictionaryWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithContentsOfFileSelector (toNSString path)

-- | @+ dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURL :: IsNSURL url => url -> IO (Id NSDictionary)
dictionaryWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMessage cls' dictionaryWithContentsOfURLSelector (toNSURL url)

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSDictionary nsDictionary, IsNSString path) => nsDictionary -> path -> IO (Id NSDictionary)
initWithContentsOfFile nsDictionary path =
  sendOwnedMessage nsDictionary initWithContentsOfFileSelector (toNSString path)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSDictionary nsDictionary, IsNSURL url) => nsDictionary -> url -> IO (Id NSDictionary)
initWithContentsOfURL nsDictionary url =
  sendOwnedMessage nsDictionary initWithContentsOfURLSelector (toNSURL url)

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSDictionary nsDictionary, IsNSString path) => nsDictionary -> path -> Bool -> IO Bool
writeToFile_atomically nsDictionary path useAuxiliaryFile =
  sendMessage nsDictionary writeToFile_atomicallySelector (toNSString path) useAuxiliaryFile

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSDictionary nsDictionary, IsNSURL url) => nsDictionary -> url -> Bool -> IO Bool
writeToURL_atomically nsDictionary url atomically =
  sendMessage nsDictionary writeToURL_atomicallySelector (toNSURL url) atomically

-- | @- allKeysForObject:@
allKeysForObject :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO (Id NSArray)
allKeysForObject nsDictionary anObject =
  sendMessage nsDictionary allKeysForObjectSelector anObject

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO (Id NSString)
descriptionWithLocale nsDictionary locale =
  sendMessage nsDictionary descriptionWithLocaleSelector locale

-- | @- descriptionWithLocale:indent:@
descriptionWithLocale_indent :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> CULong -> IO (Id NSString)
descriptionWithLocale_indent nsDictionary locale level =
  sendMessage nsDictionary descriptionWithLocale_indentSelector locale level

-- | @- isEqualToDictionary:@
isEqualToDictionary :: (IsNSDictionary nsDictionary, IsNSDictionary otherDictionary) => nsDictionary -> otherDictionary -> IO Bool
isEqualToDictionary nsDictionary otherDictionary =
  sendMessage nsDictionary isEqualToDictionarySelector (toNSDictionary otherDictionary)

-- | @- objectEnumerator@
objectEnumerator :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSEnumerator)
objectEnumerator nsDictionary =
  sendMessage nsDictionary objectEnumeratorSelector

-- | @- objectsForKeys:notFoundMarker:@
objectsForKeys_notFoundMarker :: (IsNSDictionary nsDictionary, IsNSArray keys) => nsDictionary -> keys -> RawId -> IO (Id NSArray)
objectsForKeys_notFoundMarker nsDictionary keys marker =
  sendMessage nsDictionary objectsForKeys_notFoundMarkerSelector (toNSArray keys) marker

-- | @- writeToURL:error:@
writeToURL_error :: (IsNSDictionary nsDictionary, IsNSURL url, IsNSError error_) => nsDictionary -> url -> error_ -> IO Bool
writeToURL_error nsDictionary url error_ =
  sendMessage nsDictionary writeToURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- keysSortedByValueUsingSelector:@
keysSortedByValueUsingSelector :: IsNSDictionary nsDictionary => nsDictionary -> Sel -> IO (Id NSArray)
keysSortedByValueUsingSelector nsDictionary comparator =
  sendMessage nsDictionary keysSortedByValueUsingSelectorSelector comparator

-- | @- getObjects:andKeys:count:@
getObjects_andKeys_count :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> RawId -> CULong -> IO ()
getObjects_andKeys_count nsDictionary objects keys count =
  sendMessage nsDictionary getObjects_andKeys_countSelector objects keys count

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO RawId
objectForKeyedSubscript nsDictionary key =
  sendMessage nsDictionary objectForKeyedSubscriptSelector key

-- | @- enumerateKeysAndObjectsUsingBlock:@
enumerateKeysAndObjectsUsingBlock :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> IO ()
enumerateKeysAndObjectsUsingBlock nsDictionary block =
  sendMessage nsDictionary enumerateKeysAndObjectsUsingBlockSelector block

-- | @- enumerateKeysAndObjectsWithOptions:usingBlock:@
enumerateKeysAndObjectsWithOptions_usingBlock :: IsNSDictionary nsDictionary => nsDictionary -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateKeysAndObjectsWithOptions_usingBlock nsDictionary opts block =
  sendMessage nsDictionary enumerateKeysAndObjectsWithOptions_usingBlockSelector opts block

-- | @- keysSortedByValueUsingComparator:@
keysSortedByValueUsingComparator :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> IO (Id NSArray)
keysSortedByValueUsingComparator nsDictionary cmptr =
  sendMessage nsDictionary keysSortedByValueUsingComparatorSelector cmptr

-- | @- keysSortedByValueWithOptions:usingComparator:@
keysSortedByValueWithOptions_usingComparator :: IsNSDictionary nsDictionary => nsDictionary -> NSSortOptions -> Ptr () -> IO (Id NSArray)
keysSortedByValueWithOptions_usingComparator nsDictionary opts cmptr =
  sendMessage nsDictionary keysSortedByValueWithOptions_usingComparatorSelector opts cmptr

-- | @- keysOfEntriesPassingTest:@
keysOfEntriesPassingTest :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> IO (Id NSSet)
keysOfEntriesPassingTest nsDictionary predicate =
  sendMessage nsDictionary keysOfEntriesPassingTestSelector predicate

-- | @- keysOfEntriesWithOptions:passingTest:@
keysOfEntriesWithOptions_passingTest :: IsNSDictionary nsDictionary => nsDictionary -> NSEnumerationOptions -> Ptr () -> IO (Id NSSet)
keysOfEntriesWithOptions_passingTest nsDictionary opts predicate =
  sendMessage nsDictionary keysOfEntriesWithOptions_passingTestSelector opts predicate

-- | @- count@
count :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
count nsDictionary =
  sendMessage nsDictionary countSelector

-- | @- allKeys@
allKeys :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSArray)
allKeys nsDictionary =
  sendMessage nsDictionary allKeysSelector

-- | @- allValues@
allValues :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSArray)
allValues nsDictionary =
  sendMessage nsDictionary allValuesSelector

-- | @- description@
description :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
description nsDictionary =
  sendMessage nsDictionary descriptionSelector

-- | @- descriptionInStringsFileFormat@
descriptionInStringsFileFormat :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
descriptionInStringsFileFormat nsDictionary =
  sendMessage nsDictionary descriptionInStringsFileFormatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[RawId] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @keyEnumerator@
keyEnumeratorSelector :: Selector '[] (Id NSEnumerator)
keyEnumeratorSelector = mkSelector "keyEnumerator"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDictionary)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:forKeys:count:@
initWithObjects_forKeys_countSelector :: Selector '[RawId, RawId, CULong] (Id NSDictionary)
initWithObjects_forKeys_countSelector = mkSelector "initWithObjects:forKeys:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSDictionary)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector '[] CULong
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @fileModificationDate@
fileModificationDateSelector :: Selector '[] (Id NSDate)
fileModificationDateSelector = mkSelector "fileModificationDate"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector '[] (Id NSString)
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @filePosixPermissions@
filePosixPermissionsSelector :: Selector '[] CULong
filePosixPermissionsSelector = mkSelector "filePosixPermissions"

-- | @Selector@ for @fileOwnerAccountName@
fileOwnerAccountNameSelector :: Selector '[] (Id NSString)
fileOwnerAccountNameSelector = mkSelector "fileOwnerAccountName"

-- | @Selector@ for @fileGroupOwnerAccountName@
fileGroupOwnerAccountNameSelector :: Selector '[] (Id NSString)
fileGroupOwnerAccountNameSelector = mkSelector "fileGroupOwnerAccountName"

-- | @Selector@ for @fileSystemNumber@
fileSystemNumberSelector :: Selector '[] CLong
fileSystemNumberSelector = mkSelector "fileSystemNumber"

-- | @Selector@ for @fileSystemFileNumber@
fileSystemFileNumberSelector :: Selector '[] CULong
fileSystemFileNumberSelector = mkSelector "fileSystemFileNumber"

-- | @Selector@ for @fileExtensionHidden@
fileExtensionHiddenSelector :: Selector '[] Bool
fileExtensionHiddenSelector = mkSelector "fileExtensionHidden"

-- | @Selector@ for @fileHFSCreatorCode@
fileHFSCreatorCodeSelector :: Selector '[] CUInt
fileHFSCreatorCodeSelector = mkSelector "fileHFSCreatorCode"

-- | @Selector@ for @fileHFSTypeCode@
fileHFSTypeCodeSelector :: Selector '[] CUInt
fileHFSTypeCodeSelector = mkSelector "fileHFSTypeCode"

-- | @Selector@ for @fileIsImmutable@
fileIsImmutableSelector :: Selector '[] Bool
fileIsImmutableSelector = mkSelector "fileIsImmutable"

-- | @Selector@ for @fileIsAppendOnly@
fileIsAppendOnlySelector :: Selector '[] Bool
fileIsAppendOnlySelector = mkSelector "fileIsAppendOnly"

-- | @Selector@ for @fileCreationDate@
fileCreationDateSelector :: Selector '[] (Id NSDate)
fileCreationDateSelector = mkSelector "fileCreationDate"

-- | @Selector@ for @fileOwnerAccountID@
fileOwnerAccountIDSelector :: Selector '[] (Id NSNumber)
fileOwnerAccountIDSelector = mkSelector "fileOwnerAccountID"

-- | @Selector@ for @fileGroupOwnerAccountID@
fileGroupOwnerAccountIDSelector :: Selector '[] (Id NSNumber)
fileGroupOwnerAccountIDSelector = mkSelector "fileGroupOwnerAccountID"

-- | @Selector@ for @countByEnumeratingWithState:objects:count:@
countByEnumeratingWithState_objects_countSelector :: Selector '[Ptr (), RawId, CULong] CULong
countByEnumeratingWithState_objects_countSelector = mkSelector "countByEnumeratingWithState:objects:count:"

-- | @Selector@ for @sharedKeySetForKeys:@
sharedKeySetForKeysSelector :: Selector '[Id NSArray] RawId
sharedKeySetForKeysSelector = mkSelector "sharedKeySetForKeys:"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector '[] (Id NSDictionary)
dictionarySelector = mkSelector "dictionary"

-- | @Selector@ for @dictionaryWithObject:forKey:@
dictionaryWithObject_forKeySelector :: Selector '[RawId, RawId] (Id NSDictionary)
dictionaryWithObject_forKeySelector = mkSelector "dictionaryWithObject:forKey:"

-- | @Selector@ for @dictionaryWithObjects:forKeys:count:@
dictionaryWithObjects_forKeys_countSelector :: Selector '[RawId, RawId, CULong] (Id NSDictionary)
dictionaryWithObjects_forKeys_countSelector = mkSelector "dictionaryWithObjects:forKeys:count:"

-- | @Selector@ for @dictionaryWithObjectsAndKeys:@
dictionaryWithObjectsAndKeysSelector :: Selector '[RawId] (Id NSDictionary)
dictionaryWithObjectsAndKeysSelector = mkSelector "dictionaryWithObjectsAndKeys:"

-- | @Selector@ for @dictionaryWithDictionary:@
dictionaryWithDictionarySelector :: Selector '[Id NSDictionary] (Id NSDictionary)
dictionaryWithDictionarySelector = mkSelector "dictionaryWithDictionary:"

-- | @Selector@ for @dictionaryWithObjects:forKeys:@
dictionaryWithObjects_forKeysSelector :: Selector '[Id NSArray, Id NSArray] (Id NSDictionary)
dictionaryWithObjects_forKeysSelector = mkSelector "dictionaryWithObjects:forKeys:"

-- | @Selector@ for @initWithObjectsAndKeys:@
initWithObjectsAndKeysSelector :: Selector '[RawId] (Id NSDictionary)
initWithObjectsAndKeysSelector = mkSelector "initWithObjectsAndKeys:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id NSDictionary)
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @initWithDictionary:copyItems:@
initWithDictionary_copyItemsSelector :: Selector '[Id NSDictionary, Bool] (Id NSDictionary)
initWithDictionary_copyItemsSelector = mkSelector "initWithDictionary:copyItems:"

-- | @Selector@ for @initWithObjects:forKeys:@
initWithObjects_forKeysSelector :: Selector '[Id NSArray, Id NSArray] (Id NSDictionary)
initWithObjects_forKeysSelector = mkSelector "initWithObjects:forKeys:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSDictionary)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @dictionaryWithContentsOfURL:error:@
dictionaryWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSDictionary)
dictionaryWithContentsOfURL_errorSelector = mkSelector "dictionaryWithContentsOfURL:error:"

-- | @Selector@ for @getObjects:andKeys:@
getObjects_andKeysSelector :: Selector '[RawId, RawId] ()
getObjects_andKeysSelector = mkSelector "getObjects:andKeys:"

-- | @Selector@ for @dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSDictionary)
dictionaryWithContentsOfFileSelector = mkSelector "dictionaryWithContentsOfFile:"

-- | @Selector@ for @dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSDictionary)
dictionaryWithContentsOfURLSelector = mkSelector "dictionaryWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSDictionary)
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSDictionary)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector '[Id NSString, Bool] Bool
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector '[Id NSURL, Bool] Bool
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @allKeysForObject:@
allKeysForObjectSelector :: Selector '[RawId] (Id NSArray)
allKeysForObjectSelector = mkSelector "allKeysForObject:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @descriptionWithLocale:indent:@
descriptionWithLocale_indentSelector :: Selector '[RawId, CULong] (Id NSString)
descriptionWithLocale_indentSelector = mkSelector "descriptionWithLocale:indent:"

-- | @Selector@ for @isEqualToDictionary:@
isEqualToDictionarySelector :: Selector '[Id NSDictionary] Bool
isEqualToDictionarySelector = mkSelector "isEqualToDictionary:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @objectsForKeys:notFoundMarker:@
objectsForKeys_notFoundMarkerSelector :: Selector '[Id NSArray, RawId] (Id NSArray)
objectsForKeys_notFoundMarkerSelector = mkSelector "objectsForKeys:notFoundMarker:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @keysSortedByValueUsingSelector:@
keysSortedByValueUsingSelectorSelector :: Selector '[Sel] (Id NSArray)
keysSortedByValueUsingSelectorSelector = mkSelector "keysSortedByValueUsingSelector:"

-- | @Selector@ for @getObjects:andKeys:count:@
getObjects_andKeys_countSelector :: Selector '[RawId, RawId, CULong] ()
getObjects_andKeys_countSelector = mkSelector "getObjects:andKeys:count:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[RawId] RawId
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @enumerateKeysAndObjectsUsingBlock:@
enumerateKeysAndObjectsUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateKeysAndObjectsUsingBlockSelector = mkSelector "enumerateKeysAndObjectsUsingBlock:"

-- | @Selector@ for @enumerateKeysAndObjectsWithOptions:usingBlock:@
enumerateKeysAndObjectsWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateKeysAndObjectsWithOptions_usingBlockSelector = mkSelector "enumerateKeysAndObjectsWithOptions:usingBlock:"

-- | @Selector@ for @keysSortedByValueUsingComparator:@
keysSortedByValueUsingComparatorSelector :: Selector '[Ptr ()] (Id NSArray)
keysSortedByValueUsingComparatorSelector = mkSelector "keysSortedByValueUsingComparator:"

-- | @Selector@ for @keysSortedByValueWithOptions:usingComparator:@
keysSortedByValueWithOptions_usingComparatorSelector :: Selector '[NSSortOptions, Ptr ()] (Id NSArray)
keysSortedByValueWithOptions_usingComparatorSelector = mkSelector "keysSortedByValueWithOptions:usingComparator:"

-- | @Selector@ for @keysOfEntriesPassingTest:@
keysOfEntriesPassingTestSelector :: Selector '[Ptr ()] (Id NSSet)
keysOfEntriesPassingTestSelector = mkSelector "keysOfEntriesPassingTest:"

-- | @Selector@ for @keysOfEntriesWithOptions:passingTest:@
keysOfEntriesWithOptions_passingTestSelector :: Selector '[NSEnumerationOptions, Ptr ()] (Id NSSet)
keysOfEntriesWithOptions_passingTestSelector = mkSelector "keysOfEntriesWithOptions:passingTest:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector '[] (Id NSArray)
allKeysSelector = mkSelector "allKeys"

-- | @Selector@ for @allValues@
allValuesSelector :: Selector '[] (Id NSArray)
allValuesSelector = mkSelector "allValues"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @descriptionInStringsFileFormat@
descriptionInStringsFileFormatSelector :: Selector '[] (Id NSString)
descriptionInStringsFileFormatSelector = mkSelector "descriptionInStringsFileFormat"

