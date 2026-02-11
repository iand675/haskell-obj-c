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
  , objectForKeySelector
  , setObject_forKeySelector
  , removeObjectForKeySelector
  , stringForKeySelector
  , arrayForKeySelector
  , dictionaryForKeySelector
  , dataForKeySelector
  , longLongForKeySelector
  , doubleForKeySelector
  , boolForKeySelector
  , setString_forKeySelector
  , setData_forKeySelector
  , setArray_forKeySelector
  , setDictionary_forKeySelector
  , setLongLong_forKeySelector
  , setDouble_forKeySelector
  , setBool_forKeySelector
  , synchronizeSelector
  , defaultStoreSelector
  , dictionaryRepresentationSelector


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

-- | @- objectForKey:@
objectForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO RawId
objectForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    fmap (RawId . castPtr) $ sendMsg nsUbiquitousKeyValueStore (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setObject:forKey:@
setObject_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> RawId -> aKey -> IO ()
setObject_forKey nsUbiquitousKeyValueStore  anObject aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "setObject:forKey:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- removeObjectForKey:@
removeObjectForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO ()
removeObjectForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "removeObjectForKey:") retVoid [argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- stringForKey:@
stringForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSString)
stringForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "stringForKey:") (retPtr retVoid) [argPtr (castPtr raw_aKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- arrayForKey:@
arrayForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSArray)
arrayForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "arrayForKey:") (retPtr retVoid) [argPtr (castPtr raw_aKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- dictionaryForKey:@
dictionaryForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSDictionary)
dictionaryForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "dictionaryForKey:") (retPtr retVoid) [argPtr (castPtr raw_aKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataForKey:@
dataForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO (Id NSData)
dataForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "dataForKey:") (retPtr retVoid) [argPtr (castPtr raw_aKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- longLongForKey:@
longLongForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO CLong
longLongForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "longLongForKey:") retCLong [argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- doubleForKey:@
doubleForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO CDouble
doubleForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "doubleForKey:") retCDouble [argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- boolForKey:@
boolForKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> aKey -> IO Bool
boolForKey nsUbiquitousKeyValueStore  aKey =
withObjCPtr aKey $ \raw_aKey ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUbiquitousKeyValueStore (mkSelector "boolForKey:") retCULong [argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setString:forKey:@
setString_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aString, IsNSString aKey) => nsUbiquitousKeyValueStore -> aString -> aKey -> IO ()
setString_forKey nsUbiquitousKeyValueStore  aString aKey =
withObjCPtr aString $ \raw_aString ->
  withObjCPtr aKey $ \raw_aKey ->
      sendMsg nsUbiquitousKeyValueStore (mkSelector "setString:forKey:") retVoid [argPtr (castPtr raw_aString :: Ptr ()), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setData:forKey:@
setData_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSData aData, IsNSString aKey) => nsUbiquitousKeyValueStore -> aData -> aKey -> IO ()
setData_forKey nsUbiquitousKeyValueStore  aData aKey =
withObjCPtr aData $ \raw_aData ->
  withObjCPtr aKey $ \raw_aKey ->
      sendMsg nsUbiquitousKeyValueStore (mkSelector "setData:forKey:") retVoid [argPtr (castPtr raw_aData :: Ptr ()), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setArray:forKey:@
setArray_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSArray anArray, IsNSString aKey) => nsUbiquitousKeyValueStore -> anArray -> aKey -> IO ()
setArray_forKey nsUbiquitousKeyValueStore  anArray aKey =
withObjCPtr anArray $ \raw_anArray ->
  withObjCPtr aKey $ \raw_aKey ->
      sendMsg nsUbiquitousKeyValueStore (mkSelector "setArray:forKey:") retVoid [argPtr (castPtr raw_anArray :: Ptr ()), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setDictionary:forKey:@
setDictionary_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSDictionary aDictionary, IsNSString aKey) => nsUbiquitousKeyValueStore -> aDictionary -> aKey -> IO ()
setDictionary_forKey nsUbiquitousKeyValueStore  aDictionary aKey =
withObjCPtr aDictionary $ \raw_aDictionary ->
  withObjCPtr aKey $ \raw_aKey ->
      sendMsg nsUbiquitousKeyValueStore (mkSelector "setDictionary:forKey:") retVoid [argPtr (castPtr raw_aDictionary :: Ptr ()), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setLongLong:forKey:@
setLongLong_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> CLong -> aKey -> IO ()
setLongLong_forKey nsUbiquitousKeyValueStore  value aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "setLongLong:forKey:") retVoid [argCLong (fromIntegral value), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setDouble:forKey:@
setDouble_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> CDouble -> aKey -> IO ()
setDouble_forKey nsUbiquitousKeyValueStore  value aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "setDouble:forKey:") retVoid [argCDouble (fromIntegral value), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- setBool:forKey:@
setBool_forKey :: (IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore, IsNSString aKey) => nsUbiquitousKeyValueStore -> Bool -> aKey -> IO ()
setBool_forKey nsUbiquitousKeyValueStore  value aKey =
withObjCPtr aKey $ \raw_aKey ->
    sendMsg nsUbiquitousKeyValueStore (mkSelector "setBool:forKey:") retVoid [argCULong (if value then 1 else 0), argPtr (castPtr raw_aKey :: Ptr ())]

-- | @- synchronize@
synchronize :: IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore => nsUbiquitousKeyValueStore -> IO Bool
synchronize nsUbiquitousKeyValueStore  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUbiquitousKeyValueStore (mkSelector "synchronize") retCULong []

-- | @+ defaultStore@
defaultStore :: IO (Id NSUbiquitousKeyValueStore)
defaultStore  =
  do
    cls' <- getRequiredClass "NSUbiquitousKeyValueStore"
    sendClassMsg cls' (mkSelector "defaultStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dictionaryRepresentation@
dictionaryRepresentation :: IsNSUbiquitousKeyValueStore nsUbiquitousKeyValueStore => nsUbiquitousKeyValueStore -> IO (Id NSDictionary)
dictionaryRepresentation nsUbiquitousKeyValueStore  =
  sendMsg nsUbiquitousKeyValueStore (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @stringForKey:@
stringForKeySelector :: Selector
stringForKeySelector = mkSelector "stringForKey:"

-- | @Selector@ for @arrayForKey:@
arrayForKeySelector :: Selector
arrayForKeySelector = mkSelector "arrayForKey:"

-- | @Selector@ for @dictionaryForKey:@
dictionaryForKeySelector :: Selector
dictionaryForKeySelector = mkSelector "dictionaryForKey:"

-- | @Selector@ for @dataForKey:@
dataForKeySelector :: Selector
dataForKeySelector = mkSelector "dataForKey:"

-- | @Selector@ for @longLongForKey:@
longLongForKeySelector :: Selector
longLongForKeySelector = mkSelector "longLongForKey:"

-- | @Selector@ for @doubleForKey:@
doubleForKeySelector :: Selector
doubleForKeySelector = mkSelector "doubleForKey:"

-- | @Selector@ for @boolForKey:@
boolForKeySelector :: Selector
boolForKeySelector = mkSelector "boolForKey:"

-- | @Selector@ for @setString:forKey:@
setString_forKeySelector :: Selector
setString_forKeySelector = mkSelector "setString:forKey:"

-- | @Selector@ for @setData:forKey:@
setData_forKeySelector :: Selector
setData_forKeySelector = mkSelector "setData:forKey:"

-- | @Selector@ for @setArray:forKey:@
setArray_forKeySelector :: Selector
setArray_forKeySelector = mkSelector "setArray:forKey:"

-- | @Selector@ for @setDictionary:forKey:@
setDictionary_forKeySelector :: Selector
setDictionary_forKeySelector = mkSelector "setDictionary:forKey:"

-- | @Selector@ for @setLongLong:forKey:@
setLongLong_forKeySelector :: Selector
setLongLong_forKeySelector = mkSelector "setLongLong:forKey:"

-- | @Selector@ for @setDouble:forKey:@
setDouble_forKeySelector :: Selector
setDouble_forKeySelector = mkSelector "setDouble:forKey:"

-- | @Selector@ for @setBool:forKey:@
setBool_forKeySelector :: Selector
setBool_forKeySelector = mkSelector "setBool:forKey:"

-- | @Selector@ for @synchronize@
synchronizeSelector :: Selector
synchronizeSelector = mkSelector "synchronize"

-- | @Selector@ for @defaultStore@
defaultStoreSelector :: Selector
defaultStoreSelector = mkSelector "defaultStore"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

