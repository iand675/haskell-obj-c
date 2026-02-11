{-# LANGUAGE PatternSynonyms #-}
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
  , objectForKeySelector
  , keyEnumeratorSelector
  , initSelector
  , initWithObjects_forKeys_countSelector
  , initWithCoderSelector
  , valueForKeySelector
  , fileSizeSelector
  , fileModificationDateSelector
  , fileTypeSelector
  , filePosixPermissionsSelector
  , fileOwnerAccountNameSelector
  , fileGroupOwnerAccountNameSelector
  , fileSystemNumberSelector
  , fileSystemFileNumberSelector
  , fileExtensionHiddenSelector
  , fileHFSCreatorCodeSelector
  , fileHFSTypeCodeSelector
  , fileIsImmutableSelector
  , fileIsAppendOnlySelector
  , fileCreationDateSelector
  , fileOwnerAccountIDSelector
  , fileGroupOwnerAccountIDSelector
  , countByEnumeratingWithState_objects_countSelector
  , sharedKeySetForKeysSelector
  , dictionarySelector
  , dictionaryWithObject_forKeySelector
  , dictionaryWithObjects_forKeys_countSelector
  , dictionaryWithObjectsAndKeysSelector
  , dictionaryWithDictionarySelector
  , dictionaryWithObjects_forKeysSelector
  , initWithObjectsAndKeysSelector
  , initWithDictionarySelector
  , initWithDictionary_copyItemsSelector
  , initWithObjects_forKeysSelector
  , initWithContentsOfURL_errorSelector
  , dictionaryWithContentsOfURL_errorSelector
  , getObjects_andKeysSelector
  , dictionaryWithContentsOfFileSelector
  , dictionaryWithContentsOfURLSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , writeToFile_atomicallySelector
  , writeToURL_atomicallySelector
  , allKeysForObjectSelector
  , descriptionWithLocaleSelector
  , descriptionWithLocale_indentSelector
  , isEqualToDictionarySelector
  , objectEnumeratorSelector
  , objectsForKeys_notFoundMarkerSelector
  , writeToURL_errorSelector
  , keysSortedByValueUsingSelectorSelector
  , getObjects_andKeys_countSelector
  , objectForKeyedSubscriptSelector
  , enumerateKeysAndObjectsUsingBlockSelector
  , enumerateKeysAndObjectsWithOptions_usingBlockSelector
  , keysSortedByValueUsingComparatorSelector
  , keysSortedByValueWithOptions_usingComparatorSelector
  , keysOfEntriesPassingTestSelector
  , keysOfEntriesWithOptions_passingTestSelector
  , countSelector
  , allKeysSelector
  , allValuesSelector
  , descriptionSelector
  , descriptionInStringsFileFormatSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse
  , NSSortOptions(NSSortOptions)
  , pattern NSSortConcurrent
  , pattern NSSortStable

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- objectForKey:@
objectForKey :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO RawId
objectForKey nsDictionary  aKey =
  fmap (RawId . castPtr) $ sendMsg nsDictionary (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr (unRawId aKey) :: Ptr ())]

-- | @- keyEnumerator@
keyEnumerator :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSEnumerator)
keyEnumerator nsDictionary  =
  sendMsg nsDictionary (mkSelector "keyEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSDictionary)
init_ nsDictionary  =
  sendMsg nsDictionary (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithObjects:forKeys:count:@
initWithObjects_forKeys_count :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> RawId -> CULong -> IO (Id NSDictionary)
initWithObjects_forKeys_count nsDictionary  objects keys cnt =
  sendMsg nsDictionary (mkSelector "initWithObjects:forKeys:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argPtr (castPtr (unRawId keys) :: Ptr ()), argCULong (fromIntegral cnt)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSDictionary nsDictionary, IsNSCoder coder) => nsDictionary -> coder -> IO (Id NSDictionary)
initWithCoder nsDictionary  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsDictionary (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- valueForKey:@
valueForKey :: (IsNSDictionary nsDictionary, IsNSString key) => nsDictionary -> key -> IO RawId
valueForKey nsDictionary  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsDictionary (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- fileSize@
fileSize :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
fileSize nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileSize") retCULong []

-- | @- fileModificationDate@
fileModificationDate :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSDate)
fileModificationDate nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileType@
fileType :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
fileType nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- filePosixPermissions@
filePosixPermissions :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
filePosixPermissions nsDictionary  =
  sendMsg nsDictionary (mkSelector "filePosixPermissions") retCULong []

-- | @- fileOwnerAccountName@
fileOwnerAccountName :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
fileOwnerAccountName nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileOwnerAccountName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileGroupOwnerAccountName@
fileGroupOwnerAccountName :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
fileGroupOwnerAccountName nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileGroupOwnerAccountName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileSystemNumber@
fileSystemNumber :: IsNSDictionary nsDictionary => nsDictionary -> IO CLong
fileSystemNumber nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileSystemNumber") retCLong []

-- | @- fileSystemFileNumber@
fileSystemFileNumber :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
fileSystemFileNumber nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileSystemFileNumber") retCULong []

-- | @- fileExtensionHidden@
fileExtensionHidden :: IsNSDictionary nsDictionary => nsDictionary -> IO Bool
fileExtensionHidden nsDictionary  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "fileExtensionHidden") retCULong []

-- | @- fileHFSCreatorCode@
fileHFSCreatorCode :: IsNSDictionary nsDictionary => nsDictionary -> IO CUInt
fileHFSCreatorCode nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileHFSCreatorCode") retCUInt []

-- | @- fileHFSTypeCode@
fileHFSTypeCode :: IsNSDictionary nsDictionary => nsDictionary -> IO CUInt
fileHFSTypeCode nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileHFSTypeCode") retCUInt []

-- | @- fileIsImmutable@
fileIsImmutable :: IsNSDictionary nsDictionary => nsDictionary -> IO Bool
fileIsImmutable nsDictionary  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "fileIsImmutable") retCULong []

-- | @- fileIsAppendOnly@
fileIsAppendOnly :: IsNSDictionary nsDictionary => nsDictionary -> IO Bool
fileIsAppendOnly nsDictionary  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "fileIsAppendOnly") retCULong []

-- | @- fileCreationDate@
fileCreationDate :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSDate)
fileCreationDate nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileCreationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileOwnerAccountID@
fileOwnerAccountID :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSNumber)
fileOwnerAccountID nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileOwnerAccountID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileGroupOwnerAccountID@
fileGroupOwnerAccountID :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSNumber)
fileGroupOwnerAccountID nsDictionary  =
  sendMsg nsDictionary (mkSelector "fileGroupOwnerAccountID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- countByEnumeratingWithState:objects:count:@
countByEnumeratingWithState_objects_count :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> RawId -> CULong -> IO CULong
countByEnumeratingWithState_objects_count nsDictionary  state buffer len =
  sendMsg nsDictionary (mkSelector "countByEnumeratingWithState:objects:count:") retCULong [argPtr state, argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong (fromIntegral len)]

-- | @+ sharedKeySetForKeys:@
sharedKeySetForKeys :: IsNSArray keys => keys -> IO RawId
sharedKeySetForKeys keys =
  do
    cls' <- getRequiredClass "NSDictionary"
    withObjCPtr keys $ \raw_keys ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "sharedKeySetForKeys:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ())]

-- | @+ dictionary@
dictionary :: IO (Id NSDictionary)
dictionary  =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMsg cls' (mkSelector "dictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dictionaryWithObject:forKey:@
dictionaryWithObject_forKey :: RawId -> RawId -> IO (Id NSDictionary)
dictionaryWithObject_forKey object key =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMsg cls' (mkSelector "dictionaryWithObject:forKey:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dictionaryWithObjects:forKeys:count:@
dictionaryWithObjects_forKeys_count :: RawId -> RawId -> CULong -> IO (Id NSDictionary)
dictionaryWithObjects_forKeys_count objects keys cnt =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMsg cls' (mkSelector "dictionaryWithObjects:forKeys:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argPtr (castPtr (unRawId keys) :: Ptr ()), argCULong (fromIntegral cnt)] >>= retainedObject . castPtr

-- | @+ dictionaryWithObjectsAndKeys:@
dictionaryWithObjectsAndKeys :: RawId -> IO (Id NSDictionary)
dictionaryWithObjectsAndKeys firstObject =
  do
    cls' <- getRequiredClass "NSDictionary"
    sendClassMsg cls' (mkSelector "dictionaryWithObjectsAndKeys:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dictionaryWithDictionary:@
dictionaryWithDictionary :: IsNSDictionary dict => dict -> IO (Id NSDictionary)
dictionaryWithDictionary dict =
  do
    cls' <- getRequiredClass "NSDictionary"
    withObjCPtr dict $ \raw_dict ->
      sendClassMsg cls' (mkSelector "dictionaryWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dictionaryWithObjects:forKeys:@
dictionaryWithObjects_forKeys :: (IsNSArray objects, IsNSArray keys) => objects -> keys -> IO (Id NSDictionary)
dictionaryWithObjects_forKeys objects keys =
  do
    cls' <- getRequiredClass "NSDictionary"
    withObjCPtr objects $ \raw_objects ->
      withObjCPtr keys $ \raw_keys ->
        sendClassMsg cls' (mkSelector "dictionaryWithObjects:forKeys:") (retPtr retVoid) [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_keys :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithObjectsAndKeys:@
initWithObjectsAndKeys :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO (Id NSDictionary)
initWithObjectsAndKeys nsDictionary  firstObject =
  sendMsg nsDictionary (mkSelector "initWithObjectsAndKeys:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObject) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDictionary:@
initWithDictionary :: (IsNSDictionary nsDictionary, IsNSDictionary otherDictionary) => nsDictionary -> otherDictionary -> IO (Id NSDictionary)
initWithDictionary nsDictionary  otherDictionary =
withObjCPtr otherDictionary $ \raw_otherDictionary ->
    sendMsg nsDictionary (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_otherDictionary :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDictionary:copyItems:@
initWithDictionary_copyItems :: (IsNSDictionary nsDictionary, IsNSDictionary otherDictionary) => nsDictionary -> otherDictionary -> Bool -> IO (Id NSDictionary)
initWithDictionary_copyItems nsDictionary  otherDictionary flag =
withObjCPtr otherDictionary $ \raw_otherDictionary ->
    sendMsg nsDictionary (mkSelector "initWithDictionary:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_otherDictionary :: Ptr ()), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithObjects:forKeys:@
initWithObjects_forKeys :: (IsNSDictionary nsDictionary, IsNSArray objects, IsNSArray keys) => nsDictionary -> objects -> keys -> IO (Id NSDictionary)
initWithObjects_forKeys nsDictionary  objects keys =
withObjCPtr objects $ \raw_objects ->
  withObjCPtr keys $ \raw_keys ->
      sendMsg nsDictionary (mkSelector "initWithObjects:forKeys:") (retPtr retVoid) [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_keys :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNSDictionary nsDictionary, IsNSURL url, IsNSError error_) => nsDictionary -> url -> error_ -> IO (Id NSDictionary)
initWithContentsOfURL_error nsDictionary  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsDictionary (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ dictionaryWithContentsOfURL:error:@
dictionaryWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSDictionary)
dictionaryWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NSDictionary"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "dictionaryWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | This method is unsafe because it could potentially cause buffer overruns. You should use -getObjects:andKeys:count:
--
-- ObjC selector: @- getObjects:andKeys:@
getObjects_andKeys :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> RawId -> IO ()
getObjects_andKeys nsDictionary  objects keys =
  sendMsg nsDictionary (mkSelector "getObjects:andKeys:") retVoid [argPtr (castPtr (unRawId objects) :: Ptr ()), argPtr (castPtr (unRawId keys) :: Ptr ())]

-- | @+ dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFile :: IsNSString path => path -> IO (Id NSDictionary)
dictionaryWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSDictionary"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "dictionaryWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURL :: IsNSURL url => url -> IO (Id NSDictionary)
dictionaryWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSDictionary"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "dictionaryWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSDictionary nsDictionary, IsNSString path) => nsDictionary -> path -> IO (Id NSDictionary)
initWithContentsOfFile nsDictionary  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsDictionary (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSDictionary nsDictionary, IsNSURL url) => nsDictionary -> url -> IO (Id NSDictionary)
initWithContentsOfURL nsDictionary  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsDictionary (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSDictionary nsDictionary, IsNSString path) => nsDictionary -> path -> Bool -> IO Bool
writeToFile_atomically nsDictionary  path useAuxiliaryFile =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "writeToFile:atomically:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if useAuxiliaryFile then 1 else 0)]

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSDictionary nsDictionary, IsNSURL url) => nsDictionary -> url -> Bool -> IO Bool
writeToURL_atomically nsDictionary  url atomically =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "writeToURL:atomically:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (if atomically then 1 else 0)]

-- | @- allKeysForObject:@
allKeysForObject :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO (Id NSArray)
allKeysForObject nsDictionary  anObject =
  sendMsg nsDictionary (mkSelector "allKeysForObject:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO (Id NSString)
descriptionWithLocale nsDictionary  locale =
  sendMsg nsDictionary (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithLocale:indent:@
descriptionWithLocale_indent :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> CULong -> IO (Id NSString)
descriptionWithLocale_indent nsDictionary  locale level =
  sendMsg nsDictionary (mkSelector "descriptionWithLocale:indent:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ()), argCULong (fromIntegral level)] >>= retainedObject . castPtr

-- | @- isEqualToDictionary:@
isEqualToDictionary :: (IsNSDictionary nsDictionary, IsNSDictionary otherDictionary) => nsDictionary -> otherDictionary -> IO Bool
isEqualToDictionary nsDictionary  otherDictionary =
withObjCPtr otherDictionary $ \raw_otherDictionary ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "isEqualToDictionary:") retCULong [argPtr (castPtr raw_otherDictionary :: Ptr ())]

-- | @- objectEnumerator@
objectEnumerator :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSEnumerator)
objectEnumerator nsDictionary  =
  sendMsg nsDictionary (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectsForKeys:notFoundMarker:@
objectsForKeys_notFoundMarker :: (IsNSDictionary nsDictionary, IsNSArray keys) => nsDictionary -> keys -> RawId -> IO (Id NSArray)
objectsForKeys_notFoundMarker nsDictionary  keys marker =
withObjCPtr keys $ \raw_keys ->
    sendMsg nsDictionary (mkSelector "objectsForKeys:notFoundMarker:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr (unRawId marker) :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToURL:error:@
writeToURL_error :: (IsNSDictionary nsDictionary, IsNSURL url, IsNSError error_) => nsDictionary -> url -> error_ -> IO Bool
writeToURL_error nsDictionary  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionary (mkSelector "writeToURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- keysSortedByValueUsingSelector:@
keysSortedByValueUsingSelector :: IsNSDictionary nsDictionary => nsDictionary -> Selector -> IO (Id NSArray)
keysSortedByValueUsingSelector nsDictionary  comparator =
  sendMsg nsDictionary (mkSelector "keysSortedByValueUsingSelector:") (retPtr retVoid) [argPtr (unSelector comparator)] >>= retainedObject . castPtr

-- | @- getObjects:andKeys:count:@
getObjects_andKeys_count :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> RawId -> CULong -> IO ()
getObjects_andKeys_count nsDictionary  objects keys count =
  sendMsg nsDictionary (mkSelector "getObjects:andKeys:count:") retVoid [argPtr (castPtr (unRawId objects) :: Ptr ()), argPtr (castPtr (unRawId keys) :: Ptr ()), argCULong (fromIntegral count)]

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: IsNSDictionary nsDictionary => nsDictionary -> RawId -> IO RawId
objectForKeyedSubscript nsDictionary  key =
  fmap (RawId . castPtr) $ sendMsg nsDictionary (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- enumerateKeysAndObjectsUsingBlock:@
enumerateKeysAndObjectsUsingBlock :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> IO ()
enumerateKeysAndObjectsUsingBlock nsDictionary  block =
  sendMsg nsDictionary (mkSelector "enumerateKeysAndObjectsUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateKeysAndObjectsWithOptions:usingBlock:@
enumerateKeysAndObjectsWithOptions_usingBlock :: IsNSDictionary nsDictionary => nsDictionary -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateKeysAndObjectsWithOptions_usingBlock nsDictionary  opts block =
  sendMsg nsDictionary (mkSelector "enumerateKeysAndObjectsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- keysSortedByValueUsingComparator:@
keysSortedByValueUsingComparator :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> IO (Id NSArray)
keysSortedByValueUsingComparator nsDictionary  cmptr =
  sendMsg nsDictionary (mkSelector "keysSortedByValueUsingComparator:") (retPtr retVoid) [argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- keysSortedByValueWithOptions:usingComparator:@
keysSortedByValueWithOptions_usingComparator :: IsNSDictionary nsDictionary => nsDictionary -> NSSortOptions -> Ptr () -> IO (Id NSArray)
keysSortedByValueWithOptions_usingComparator nsDictionary  opts cmptr =
  sendMsg nsDictionary (mkSelector "keysSortedByValueWithOptions:usingComparator:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- keysOfEntriesPassingTest:@
keysOfEntriesPassingTest :: IsNSDictionary nsDictionary => nsDictionary -> Ptr () -> IO (Id NSSet)
keysOfEntriesPassingTest nsDictionary  predicate =
  sendMsg nsDictionary (mkSelector "keysOfEntriesPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- keysOfEntriesWithOptions:passingTest:@
keysOfEntriesWithOptions_passingTest :: IsNSDictionary nsDictionary => nsDictionary -> NSEnumerationOptions -> Ptr () -> IO (Id NSSet)
keysOfEntriesWithOptions_passingTest nsDictionary  opts predicate =
  sendMsg nsDictionary (mkSelector "keysOfEntriesWithOptions:passingTest:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- count@
count :: IsNSDictionary nsDictionary => nsDictionary -> IO CULong
count nsDictionary  =
  sendMsg nsDictionary (mkSelector "count") retCULong []

-- | @- allKeys@
allKeys :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSArray)
allKeys nsDictionary  =
  sendMsg nsDictionary (mkSelector "allKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allValues@
allValues :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSArray)
allValues nsDictionary  =
  sendMsg nsDictionary (mkSelector "allValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- description@
description :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
description nsDictionary  =
  sendMsg nsDictionary (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- descriptionInStringsFileFormat@
descriptionInStringsFileFormat :: IsNSDictionary nsDictionary => nsDictionary -> IO (Id NSString)
descriptionInStringsFileFormat nsDictionary  =
  sendMsg nsDictionary (mkSelector "descriptionInStringsFileFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @keyEnumerator@
keyEnumeratorSelector :: Selector
keyEnumeratorSelector = mkSelector "keyEnumerator"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:forKeys:count:@
initWithObjects_forKeys_countSelector :: Selector
initWithObjects_forKeys_countSelector = mkSelector "initWithObjects:forKeys:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @fileModificationDate@
fileModificationDateSelector :: Selector
fileModificationDateSelector = mkSelector "fileModificationDate"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @filePosixPermissions@
filePosixPermissionsSelector :: Selector
filePosixPermissionsSelector = mkSelector "filePosixPermissions"

-- | @Selector@ for @fileOwnerAccountName@
fileOwnerAccountNameSelector :: Selector
fileOwnerAccountNameSelector = mkSelector "fileOwnerAccountName"

-- | @Selector@ for @fileGroupOwnerAccountName@
fileGroupOwnerAccountNameSelector :: Selector
fileGroupOwnerAccountNameSelector = mkSelector "fileGroupOwnerAccountName"

-- | @Selector@ for @fileSystemNumber@
fileSystemNumberSelector :: Selector
fileSystemNumberSelector = mkSelector "fileSystemNumber"

-- | @Selector@ for @fileSystemFileNumber@
fileSystemFileNumberSelector :: Selector
fileSystemFileNumberSelector = mkSelector "fileSystemFileNumber"

-- | @Selector@ for @fileExtensionHidden@
fileExtensionHiddenSelector :: Selector
fileExtensionHiddenSelector = mkSelector "fileExtensionHidden"

-- | @Selector@ for @fileHFSCreatorCode@
fileHFSCreatorCodeSelector :: Selector
fileHFSCreatorCodeSelector = mkSelector "fileHFSCreatorCode"

-- | @Selector@ for @fileHFSTypeCode@
fileHFSTypeCodeSelector :: Selector
fileHFSTypeCodeSelector = mkSelector "fileHFSTypeCode"

-- | @Selector@ for @fileIsImmutable@
fileIsImmutableSelector :: Selector
fileIsImmutableSelector = mkSelector "fileIsImmutable"

-- | @Selector@ for @fileIsAppendOnly@
fileIsAppendOnlySelector :: Selector
fileIsAppendOnlySelector = mkSelector "fileIsAppendOnly"

-- | @Selector@ for @fileCreationDate@
fileCreationDateSelector :: Selector
fileCreationDateSelector = mkSelector "fileCreationDate"

-- | @Selector@ for @fileOwnerAccountID@
fileOwnerAccountIDSelector :: Selector
fileOwnerAccountIDSelector = mkSelector "fileOwnerAccountID"

-- | @Selector@ for @fileGroupOwnerAccountID@
fileGroupOwnerAccountIDSelector :: Selector
fileGroupOwnerAccountIDSelector = mkSelector "fileGroupOwnerAccountID"

-- | @Selector@ for @countByEnumeratingWithState:objects:count:@
countByEnumeratingWithState_objects_countSelector :: Selector
countByEnumeratingWithState_objects_countSelector = mkSelector "countByEnumeratingWithState:objects:count:"

-- | @Selector@ for @sharedKeySetForKeys:@
sharedKeySetForKeysSelector :: Selector
sharedKeySetForKeysSelector = mkSelector "sharedKeySetForKeys:"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector
dictionarySelector = mkSelector "dictionary"

-- | @Selector@ for @dictionaryWithObject:forKey:@
dictionaryWithObject_forKeySelector :: Selector
dictionaryWithObject_forKeySelector = mkSelector "dictionaryWithObject:forKey:"

-- | @Selector@ for @dictionaryWithObjects:forKeys:count:@
dictionaryWithObjects_forKeys_countSelector :: Selector
dictionaryWithObjects_forKeys_countSelector = mkSelector "dictionaryWithObjects:forKeys:count:"

-- | @Selector@ for @dictionaryWithObjectsAndKeys:@
dictionaryWithObjectsAndKeysSelector :: Selector
dictionaryWithObjectsAndKeysSelector = mkSelector "dictionaryWithObjectsAndKeys:"

-- | @Selector@ for @dictionaryWithDictionary:@
dictionaryWithDictionarySelector :: Selector
dictionaryWithDictionarySelector = mkSelector "dictionaryWithDictionary:"

-- | @Selector@ for @dictionaryWithObjects:forKeys:@
dictionaryWithObjects_forKeysSelector :: Selector
dictionaryWithObjects_forKeysSelector = mkSelector "dictionaryWithObjects:forKeys:"

-- | @Selector@ for @initWithObjectsAndKeys:@
initWithObjectsAndKeysSelector :: Selector
initWithObjectsAndKeysSelector = mkSelector "initWithObjectsAndKeys:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @initWithDictionary:copyItems:@
initWithDictionary_copyItemsSelector :: Selector
initWithDictionary_copyItemsSelector = mkSelector "initWithDictionary:copyItems:"

-- | @Selector@ for @initWithObjects:forKeys:@
initWithObjects_forKeysSelector :: Selector
initWithObjects_forKeysSelector = mkSelector "initWithObjects:forKeys:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @dictionaryWithContentsOfURL:error:@
dictionaryWithContentsOfURL_errorSelector :: Selector
dictionaryWithContentsOfURL_errorSelector = mkSelector "dictionaryWithContentsOfURL:error:"

-- | @Selector@ for @getObjects:andKeys:@
getObjects_andKeysSelector :: Selector
getObjects_andKeysSelector = mkSelector "getObjects:andKeys:"

-- | @Selector@ for @dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFileSelector :: Selector
dictionaryWithContentsOfFileSelector = mkSelector "dictionaryWithContentsOfFile:"

-- | @Selector@ for @dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURLSelector :: Selector
dictionaryWithContentsOfURLSelector = mkSelector "dictionaryWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @allKeysForObject:@
allKeysForObjectSelector :: Selector
allKeysForObjectSelector = mkSelector "allKeysForObject:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @descriptionWithLocale:indent:@
descriptionWithLocale_indentSelector :: Selector
descriptionWithLocale_indentSelector = mkSelector "descriptionWithLocale:indent:"

-- | @Selector@ for @isEqualToDictionary:@
isEqualToDictionarySelector :: Selector
isEqualToDictionarySelector = mkSelector "isEqualToDictionary:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @objectsForKeys:notFoundMarker:@
objectsForKeys_notFoundMarkerSelector :: Selector
objectsForKeys_notFoundMarkerSelector = mkSelector "objectsForKeys:notFoundMarker:"

-- | @Selector@ for @writeToURL:error:@
writeToURL_errorSelector :: Selector
writeToURL_errorSelector = mkSelector "writeToURL:error:"

-- | @Selector@ for @keysSortedByValueUsingSelector:@
keysSortedByValueUsingSelectorSelector :: Selector
keysSortedByValueUsingSelectorSelector = mkSelector "keysSortedByValueUsingSelector:"

-- | @Selector@ for @getObjects:andKeys:count:@
getObjects_andKeys_countSelector :: Selector
getObjects_andKeys_countSelector = mkSelector "getObjects:andKeys:count:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @enumerateKeysAndObjectsUsingBlock:@
enumerateKeysAndObjectsUsingBlockSelector :: Selector
enumerateKeysAndObjectsUsingBlockSelector = mkSelector "enumerateKeysAndObjectsUsingBlock:"

-- | @Selector@ for @enumerateKeysAndObjectsWithOptions:usingBlock:@
enumerateKeysAndObjectsWithOptions_usingBlockSelector :: Selector
enumerateKeysAndObjectsWithOptions_usingBlockSelector = mkSelector "enumerateKeysAndObjectsWithOptions:usingBlock:"

-- | @Selector@ for @keysSortedByValueUsingComparator:@
keysSortedByValueUsingComparatorSelector :: Selector
keysSortedByValueUsingComparatorSelector = mkSelector "keysSortedByValueUsingComparator:"

-- | @Selector@ for @keysSortedByValueWithOptions:usingComparator:@
keysSortedByValueWithOptions_usingComparatorSelector :: Selector
keysSortedByValueWithOptions_usingComparatorSelector = mkSelector "keysSortedByValueWithOptions:usingComparator:"

-- | @Selector@ for @keysOfEntriesPassingTest:@
keysOfEntriesPassingTestSelector :: Selector
keysOfEntriesPassingTestSelector = mkSelector "keysOfEntriesPassingTest:"

-- | @Selector@ for @keysOfEntriesWithOptions:passingTest:@
keysOfEntriesWithOptions_passingTestSelector :: Selector
keysOfEntriesWithOptions_passingTestSelector = mkSelector "keysOfEntriesWithOptions:passingTest:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector
allKeysSelector = mkSelector "allKeys"

-- | @Selector@ for @allValues@
allValuesSelector :: Selector
allValuesSelector = mkSelector "allValues"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @descriptionInStringsFileFormat@
descriptionInStringsFileFormatSelector :: Selector
descriptionInStringsFileFormatSelector = mkSelector "descriptionInStringsFileFormat"

