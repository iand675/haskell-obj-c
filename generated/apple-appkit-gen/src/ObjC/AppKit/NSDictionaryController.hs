{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDictionaryController@.
module ObjC.AppKit.NSDictionaryController
  ( NSDictionaryController
  , IsNSDictionaryController(..)
  , newObject
  , initialKey
  , setInitialKey
  , initialValue
  , setInitialValue
  , includedKeys
  , setIncludedKeys
  , excludedKeys
  , setExcludedKeys
  , localizedKeyDictionary
  , setLocalizedKeyDictionary
  , localizedKeyTable
  , setLocalizedKeyTable
  , excludedKeysSelector
  , includedKeysSelector
  , initialKeySelector
  , initialValueSelector
  , localizedKeyDictionarySelector
  , localizedKeyTableSelector
  , newObjectSelector
  , setExcludedKeysSelector
  , setIncludedKeysSelector
  , setInitialKeySelector
  , setInitialValueSelector
  , setLocalizedKeyDictionarySelector
  , setLocalizedKeyTableSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- newObject@
newObject :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSDictionaryControllerKeyValuePair)
newObject nsDictionaryController =
  sendOwnedMessage nsDictionaryController newObjectSelector

-- | @- initialKey@
initialKey :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSString)
initialKey nsDictionaryController =
  sendOwnedMessage nsDictionaryController initialKeySelector

-- | @- setInitialKey:@
setInitialKey :: (IsNSDictionaryController nsDictionaryController, IsNSString value) => nsDictionaryController -> value -> IO ()
setInitialKey nsDictionaryController value =
  sendMessage nsDictionaryController setInitialKeySelector (toNSString value)

-- | @- initialValue@
initialValue :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO RawId
initialValue nsDictionaryController =
  sendOwnedMessage nsDictionaryController initialValueSelector

-- | @- setInitialValue:@
setInitialValue :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> RawId -> IO ()
setInitialValue nsDictionaryController value =
  sendMessage nsDictionaryController setInitialValueSelector value

-- | @- includedKeys@
includedKeys :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSArray)
includedKeys nsDictionaryController =
  sendMessage nsDictionaryController includedKeysSelector

-- | @- setIncludedKeys:@
setIncludedKeys :: (IsNSDictionaryController nsDictionaryController, IsNSArray value) => nsDictionaryController -> value -> IO ()
setIncludedKeys nsDictionaryController value =
  sendMessage nsDictionaryController setIncludedKeysSelector (toNSArray value)

-- | @- excludedKeys@
excludedKeys :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSArray)
excludedKeys nsDictionaryController =
  sendMessage nsDictionaryController excludedKeysSelector

-- | @- setExcludedKeys:@
setExcludedKeys :: (IsNSDictionaryController nsDictionaryController, IsNSArray value) => nsDictionaryController -> value -> IO ()
setExcludedKeys nsDictionaryController value =
  sendMessage nsDictionaryController setExcludedKeysSelector (toNSArray value)

-- | @- localizedKeyDictionary@
localizedKeyDictionary :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSDictionary)
localizedKeyDictionary nsDictionaryController =
  sendMessage nsDictionaryController localizedKeyDictionarySelector

-- | @- setLocalizedKeyDictionary:@
setLocalizedKeyDictionary :: (IsNSDictionaryController nsDictionaryController, IsNSDictionary value) => nsDictionaryController -> value -> IO ()
setLocalizedKeyDictionary nsDictionaryController value =
  sendMessage nsDictionaryController setLocalizedKeyDictionarySelector (toNSDictionary value)

-- | @- localizedKeyTable@
localizedKeyTable :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSString)
localizedKeyTable nsDictionaryController =
  sendMessage nsDictionaryController localizedKeyTableSelector

-- | @- setLocalizedKeyTable:@
setLocalizedKeyTable :: (IsNSDictionaryController nsDictionaryController, IsNSString value) => nsDictionaryController -> value -> IO ()
setLocalizedKeyTable nsDictionaryController value =
  sendMessage nsDictionaryController setLocalizedKeyTableSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newObject@
newObjectSelector :: Selector '[] (Id NSDictionaryControllerKeyValuePair)
newObjectSelector = mkSelector "newObject"

-- | @Selector@ for @initialKey@
initialKeySelector :: Selector '[] (Id NSString)
initialKeySelector = mkSelector "initialKey"

-- | @Selector@ for @setInitialKey:@
setInitialKeySelector :: Selector '[Id NSString] ()
setInitialKeySelector = mkSelector "setInitialKey:"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector '[] RawId
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @setInitialValue:@
setInitialValueSelector :: Selector '[RawId] ()
setInitialValueSelector = mkSelector "setInitialValue:"

-- | @Selector@ for @includedKeys@
includedKeysSelector :: Selector '[] (Id NSArray)
includedKeysSelector = mkSelector "includedKeys"

-- | @Selector@ for @setIncludedKeys:@
setIncludedKeysSelector :: Selector '[Id NSArray] ()
setIncludedKeysSelector = mkSelector "setIncludedKeys:"

-- | @Selector@ for @excludedKeys@
excludedKeysSelector :: Selector '[] (Id NSArray)
excludedKeysSelector = mkSelector "excludedKeys"

-- | @Selector@ for @setExcludedKeys:@
setExcludedKeysSelector :: Selector '[Id NSArray] ()
setExcludedKeysSelector = mkSelector "setExcludedKeys:"

-- | @Selector@ for @localizedKeyDictionary@
localizedKeyDictionarySelector :: Selector '[] (Id NSDictionary)
localizedKeyDictionarySelector = mkSelector "localizedKeyDictionary"

-- | @Selector@ for @setLocalizedKeyDictionary:@
setLocalizedKeyDictionarySelector :: Selector '[Id NSDictionary] ()
setLocalizedKeyDictionarySelector = mkSelector "setLocalizedKeyDictionary:"

-- | @Selector@ for @localizedKeyTable@
localizedKeyTableSelector :: Selector '[] (Id NSString)
localizedKeyTableSelector = mkSelector "localizedKeyTable"

-- | @Selector@ for @setLocalizedKeyTable:@
setLocalizedKeyTableSelector :: Selector '[Id NSString] ()
setLocalizedKeyTableSelector = mkSelector "setLocalizedKeyTable:"

