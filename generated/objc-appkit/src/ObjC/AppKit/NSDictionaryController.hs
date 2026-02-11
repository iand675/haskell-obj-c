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
  , newObjectSelector
  , initialKeySelector
  , setInitialKeySelector
  , initialValueSelector
  , setInitialValueSelector
  , includedKeysSelector
  , setIncludedKeysSelector
  , excludedKeysSelector
  , setExcludedKeysSelector
  , localizedKeyDictionarySelector
  , setLocalizedKeyDictionarySelector
  , localizedKeyTableSelector
  , setLocalizedKeyTableSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- newObject@
newObject :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSDictionaryControllerKeyValuePair)
newObject nsDictionaryController  =
  sendMsg nsDictionaryController (mkSelector "newObject") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initialKey@
initialKey :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSString)
initialKey nsDictionaryController  =
  sendMsg nsDictionaryController (mkSelector "initialKey") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialKey:@
setInitialKey :: (IsNSDictionaryController nsDictionaryController, IsNSString value) => nsDictionaryController -> value -> IO ()
setInitialKey nsDictionaryController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryController (mkSelector "setInitialKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- initialValue@
initialValue :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO RawId
initialValue nsDictionaryController  =
  fmap (RawId . castPtr) $ sendMsg nsDictionaryController (mkSelector "initialValue") (retPtr retVoid) []

-- | @- setInitialValue:@
setInitialValue :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> RawId -> IO ()
setInitialValue nsDictionaryController  value =
  sendMsg nsDictionaryController (mkSelector "setInitialValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- includedKeys@
includedKeys :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSArray)
includedKeys nsDictionaryController  =
  sendMsg nsDictionaryController (mkSelector "includedKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIncludedKeys:@
setIncludedKeys :: (IsNSDictionaryController nsDictionaryController, IsNSArray value) => nsDictionaryController -> value -> IO ()
setIncludedKeys nsDictionaryController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryController (mkSelector "setIncludedKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- excludedKeys@
excludedKeys :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSArray)
excludedKeys nsDictionaryController  =
  sendMsg nsDictionaryController (mkSelector "excludedKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExcludedKeys:@
setExcludedKeys :: (IsNSDictionaryController nsDictionaryController, IsNSArray value) => nsDictionaryController -> value -> IO ()
setExcludedKeys nsDictionaryController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryController (mkSelector "setExcludedKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizedKeyDictionary@
localizedKeyDictionary :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSDictionary)
localizedKeyDictionary nsDictionaryController  =
  sendMsg nsDictionaryController (mkSelector "localizedKeyDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedKeyDictionary:@
setLocalizedKeyDictionary :: (IsNSDictionaryController nsDictionaryController, IsNSDictionary value) => nsDictionaryController -> value -> IO ()
setLocalizedKeyDictionary nsDictionaryController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryController (mkSelector "setLocalizedKeyDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizedKeyTable@
localizedKeyTable :: IsNSDictionaryController nsDictionaryController => nsDictionaryController -> IO (Id NSString)
localizedKeyTable nsDictionaryController  =
  sendMsg nsDictionaryController (mkSelector "localizedKeyTable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedKeyTable:@
setLocalizedKeyTable :: (IsNSDictionaryController nsDictionaryController, IsNSString value) => nsDictionaryController -> value -> IO ()
setLocalizedKeyTable nsDictionaryController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryController (mkSelector "setLocalizedKeyTable:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newObject@
newObjectSelector :: Selector
newObjectSelector = mkSelector "newObject"

-- | @Selector@ for @initialKey@
initialKeySelector :: Selector
initialKeySelector = mkSelector "initialKey"

-- | @Selector@ for @setInitialKey:@
setInitialKeySelector :: Selector
setInitialKeySelector = mkSelector "setInitialKey:"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @setInitialValue:@
setInitialValueSelector :: Selector
setInitialValueSelector = mkSelector "setInitialValue:"

-- | @Selector@ for @includedKeys@
includedKeysSelector :: Selector
includedKeysSelector = mkSelector "includedKeys"

-- | @Selector@ for @setIncludedKeys:@
setIncludedKeysSelector :: Selector
setIncludedKeysSelector = mkSelector "setIncludedKeys:"

-- | @Selector@ for @excludedKeys@
excludedKeysSelector :: Selector
excludedKeysSelector = mkSelector "excludedKeys"

-- | @Selector@ for @setExcludedKeys:@
setExcludedKeysSelector :: Selector
setExcludedKeysSelector = mkSelector "setExcludedKeys:"

-- | @Selector@ for @localizedKeyDictionary@
localizedKeyDictionarySelector :: Selector
localizedKeyDictionarySelector = mkSelector "localizedKeyDictionary"

-- | @Selector@ for @setLocalizedKeyDictionary:@
setLocalizedKeyDictionarySelector :: Selector
setLocalizedKeyDictionarySelector = mkSelector "setLocalizedKeyDictionary:"

-- | @Selector@ for @localizedKeyTable@
localizedKeyTableSelector :: Selector
localizedKeyTableSelector = mkSelector "localizedKeyTable"

-- | @Selector@ for @setLocalizedKeyTable:@
setLocalizedKeyTableSelector :: Selector
setLocalizedKeyTableSelector = mkSelector "setLocalizedKeyTable:"

