{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUbiquitousKeyValueStore@.
module ObjC.Foundation.NSUbiquitousKeyValueStore
  ( NSUbiquitousKeyValueStore
  , IsNSUbiquitousKeyValueStore(..)
  , objectForKey
  , setObject_forKey
  , removeObjectForKey
  , stringForKey
  , arrayForKey
  , dictionaryForKey
  , dataForKey
  , longLongForKey
  , doubleForKey
  , boolForKey
  , setString_forKey
  , setData_forKey
  , setArray_forKey
  , setDictionary_forKey
  , setLongLong_forKey
  , setDouble_forKey
  , setBool_forKey
  , synchronize
  , defaultStore
  , dictionaryRepresentation
  , arrayForKeySelector
  , boolForKeySelector
  , dataForKeySelector
  , defaultStoreSelector
  , dictionaryForKeySelector
  , dictionaryRepresentationSelector
  , doubleForKeySelector
  , longLongForKeySelector
  , objectForKeySelector
  , removeObjectForKeySelector
  , setArray_forKeySelector
  , setBool_forKeySelector
  , setData_forKeySelector
  , setDictionary_forKeySelector
  , setDouble_forKeySelector
  , setLongLong_forKeySelector
  , setObject_forKeySelector
  , setString_forKeySelector
  , stringForKeySelector
  , synchronizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- objectForKey:@
objectForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO RawId
objectForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore objectForKeySelector (toNSString aKey)

-- | @- setObject:forKey:@
setObject_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> RawId -> aKey -> IO ()
setObject_forKey nsUbiquitousKeyValueStore anObject aKey =
  sendMessage nsUbiquitousKeyValueStore setObject_forKeySelector anObject (toNSString aKey)

-- | @- removeObjectForKey:@
removeObjectForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO ()
removeObjectForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore removeObjectForKeySelector (toNSString aKey)

-- | @- stringForKey:@
stringForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSString)
stringForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore stringForKeySelector (toNSString aKey)

-- | @- arrayForKey:@
arrayForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSArray)
arrayForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore arrayForKeySelector (toNSString aKey)

-- | @- dictionaryForKey:@
dictionaryForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSDictionary)
dictionaryForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore dictionaryForKeySelector (toNSString aKey)

-- | @- dataForKey:@
dataForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSData)
dataForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore dataForKeySelector (toNSString aKey)

-- | @- longLongForKey:@
longLongForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO CLong
longLongForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore longLongForKeySelector (toNSString aKey)

-- | @- doubleForKey:@
doubleForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO CDouble
doubleForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore doubleForKeySelector (toNSString aKey)

-- | @- boolForKey:@
boolForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO Bool
boolForKey nsUbiquitousKeyValueStore aKey =
  sendMessage nsUbiquitousKeyValueStore boolForKeySelector (toNSString aKey)

-- | @- setString:forKey:@
setString_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aString, IsNSString aKey) => nsUbiquitousKeyValueStore -> aString -> aKey -> IO ()
setString_forKey nsUbiquitousKeyValueStore aString aKey =
  sendMessage nsUbiquitousKeyValueStore setString_forKeySelector (toNSString aString) (toNSString aKey)

-- | @- setData:forKey:@
setData_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSData aData, IsNSString aKey) => nsUbiquitousKeyValueStore -> aData -> aKey -> IO ()
setData_forKey nsUbiquitousKeyValueStore aData aKey =
  sendMessage nsUbiquitousKeyValueStore setData_forKeySelector (toNSData aData) (toNSString aKey)

-- | @- setArray:forKey:@
setArray_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSArray anArray, IsNSString aKey) => nsUbiquitousKeyValueStore -> anArray -> aKey -> IO ()
setArray_forKey nsUbiquitousKeyValueStore anArray aKey =
  sendMessage nsUbiquitousKeyValueStore setArray_forKeySelector (toNSArray anArray) (toNSString aKey)

-- | @- setDictionary:forKey:@
setDictionary_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSDictionary aDictionary, IsNSString aKey) => nsUbiquitousKeyValueStore -> aDictionary -> aKey -> IO ()
setDictionary_forKey nsUbiquitousKeyValueStore aDictionary aKey =
  sendMessage nsUbiquitousKeyValueStore setDictionary_forKeySelector (toNSDictionary aDictionary) (toNSString aKey)

-- | @- setLongLong:forKey:@
setLongLong_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> CLong -> aKey -> IO ()
setLongLong_forKey nsUbiquitousKeyValueStore value aKey =
  sendMessage nsUbiquitousKeyValueStore setLongLong_forKeySelector value (toNSString aKey)

-- | @- setDouble:forKey:@
setDouble_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> CDouble -> aKey -> IO ()
setDouble_forKey nsUbiquitousKeyValueStore value aKey =
  sendMessage nsUbiquitousKeyValueStore setDouble_forKeySelector value (toNSString aKey)

-- | @- setBool:forKey:@
setBool_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> Bool -> aKey -> IO ()
setBool_forKey nsUbiquitousKeyValueStore value aKey =
  sendMessage nsUbiquitousKeyValueStore setBool_forKeySelector value (toNSString aKey)

-- | @- synchronize@
synchronize :: IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore => nsUbiquitousKeyValueStore -> IO Bool
synchronize nsUbiquitousKeyValueStore =
  sendMessage nsUbiquitousKeyValueStore synchronizeSelector

-- | @+ defaultStore@
defaultStore :: IO (Id NSUbiquitousKeyValueStore)
defaultStore  =
  do
    cls' <- getRequiredClass "NSUbiquitousKeyValueStore"
    sendClassMessage cls' defaultStoreSelector

-- | @- dictionaryRepresentation@
dictionaryRepresentation :: IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore => nsUbiquitousKeyValueStore -> IO (Id NSDictionary)
dictionaryRepresentation nsUbiquitousKeyValueStore =
  sendMessage nsUbiquitousKeyValueStore dictionaryRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[Id NSString] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector '[RawId, Id NSString] ()
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector '[Id NSString] ()
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @stringForKey:@
stringForKeySelector :: Selector '[Id NSString] (Id NSString)
stringForKeySelector = mkSelector "stringForKey:"

-- | @Selector@ for @arrayForKey:@
arrayForKeySelector :: Selector '[Id NSString] (Id NSArray)
arrayForKeySelector = mkSelector "arrayForKey:"

-- | @Selector@ for @dictionaryForKey:@
dictionaryForKeySelector :: Selector '[Id NSString] (Id NSDictionary)
dictionaryForKeySelector = mkSelector "dictionaryForKey:"

-- | @Selector@ for @dataForKey:@
dataForKeySelector :: Selector '[Id NSString] (Id NSData)
dataForKeySelector = mkSelector "dataForKey:"

-- | @Selector@ for @longLongForKey:@
longLongForKeySelector :: Selector '[Id NSString] CLong
longLongForKeySelector = mkSelector "longLongForKey:"

-- | @Selector@ for @doubleForKey:@
doubleForKeySelector :: Selector '[Id NSString] CDouble
doubleForKeySelector = mkSelector "doubleForKey:"

-- | @Selector@ for @boolForKey:@
boolForKeySelector :: Selector '[Id NSString] Bool
boolForKeySelector = mkSelector "boolForKey:"

-- | @Selector@ for @setString:forKey:@
setString_forKeySelector :: Selector '[Id NSString, Id NSString] ()
setString_forKeySelector = mkSelector "setString:forKey:"

-- | @Selector@ for @setData:forKey:@
setData_forKeySelector :: Selector '[Id NSData, Id NSString] ()
setData_forKeySelector = mkSelector "setData:forKey:"

-- | @Selector@ for @setArray:forKey:@
setArray_forKeySelector :: Selector '[Id NSArray, Id NSString] ()
setArray_forKeySelector = mkSelector "setArray:forKey:"

-- | @Selector@ for @setDictionary:forKey:@
setDictionary_forKeySelector :: Selector '[Id NSDictionary, Id NSString] ()
setDictionary_forKeySelector = mkSelector "setDictionary:forKey:"

-- | @Selector@ for @setLongLong:forKey:@
setLongLong_forKeySelector :: Selector '[CLong, Id NSString] ()
setLongLong_forKeySelector = mkSelector "setLongLong:forKey:"

-- | @Selector@ for @setDouble:forKey:@
setDouble_forKeySelector :: Selector '[CDouble, Id NSString] ()
setDouble_forKeySelector = mkSelector "setDouble:forKey:"

-- | @Selector@ for @setBool:forKey:@
setBool_forKeySelector :: Selector '[Bool, Id NSString] ()
setBool_forKeySelector = mkSelector "setBool:forKey:"

-- | @Selector@ for @synchronize@
synchronizeSelector :: Selector '[] Bool
synchronizeSelector = mkSelector "synchronize"

-- | @Selector@ for @defaultStore@
defaultStoreSelector :: Selector '[] (Id NSUbiquitousKeyValueStore)
defaultStoreSelector = mkSelector "defaultStore"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

