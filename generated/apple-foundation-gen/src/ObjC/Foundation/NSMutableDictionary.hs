{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Mutable Dictionary	***************
--
-- Generated bindings for @NSMutableDictionary@.
module ObjC.Foundation.NSMutableDictionary
  ( NSMutableDictionary
  , IsNSMutableDictionary(..)
  , removeObjectForKey
  , setObject_forKey
  , init_
  , initWithCapacity
  , initWithCoder
  , setValue_forKey
  , dictionaryWithSharedKeySet
  , dictionaryWithCapacity
  , dictionaryWithContentsOfFile
  , dictionaryWithContentsOfURL
  , initWithContentsOfFile
  , initWithContentsOfURL
  , addEntriesFromDictionary
  , removeAllObjects
  , removeObjectsForKeys
  , setDictionary
  , setObject_forKeyedSubscript
  , addEntriesFromDictionarySelector
  , dictionaryWithCapacitySelector
  , dictionaryWithContentsOfFileSelector
  , dictionaryWithContentsOfURLSelector
  , dictionaryWithSharedKeySetSelector
  , initSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , removeAllObjectsSelector
  , removeObjectForKeySelector
  , removeObjectsForKeysSelector
  , setDictionarySelector
  , setObject_forKeySelector
  , setObject_forKeyedSubscriptSelector
  , setValue_forKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- removeObjectForKey:@
removeObjectForKey :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> RawId -> IO ()
removeObjectForKey nsMutableDictionary aKey =
  sendMessage nsMutableDictionary removeObjectForKeySelector aKey

-- | @- setObject:forKey:@
setObject_forKey :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> RawId -> RawId -> IO ()
setObject_forKey nsMutableDictionary anObject aKey =
  sendMessage nsMutableDictionary setObject_forKeySelector anObject aKey

-- | @- init@
init_ :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> IO (Id NSMutableDictionary)
init_ nsMutableDictionary =
  sendOwnedMessage nsMutableDictionary initSelector

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CULong -> IO (Id NSMutableDictionary)
initWithCapacity nsMutableDictionary numItems =
  sendOwnedMessage nsMutableDictionary initWithCapacitySelector numItems

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableDictionary nsMutableDictionary, IsNSCoder coder) => nsMutableDictionary -> coder -> IO (Id NSMutableDictionary)
initWithCoder nsMutableDictionary coder =
  sendOwnedMessage nsMutableDictionary initWithCoderSelector (toNSCoder coder)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSMutableDictionary nsMutableDictionary, IsNSString key) => nsMutableDictionary -> RawId -> key -> IO ()
setValue_forKey nsMutableDictionary value key =
  sendMessage nsMutableDictionary setValue_forKeySelector value (toNSString key)

-- | @+ dictionaryWithSharedKeySet:@
dictionaryWithSharedKeySet :: RawId -> IO (Id NSMutableDictionary)
dictionaryWithSharedKeySet keyset =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' dictionaryWithSharedKeySetSelector keyset

-- | @+ dictionaryWithCapacity:@
dictionaryWithCapacity :: CULong -> IO (Id NSMutableDictionary)
dictionaryWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' dictionaryWithCapacitySelector numItems

-- | @+ dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFile :: IsNSString path => path -> IO (Id NSMutableDictionary)
dictionaryWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' dictionaryWithContentsOfFileSelector (toNSString path)

-- | @+ dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURL :: IsNSURL url => url -> IO (Id NSMutableDictionary)
dictionaryWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' dictionaryWithContentsOfURLSelector (toNSURL url)

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSMutableDictionary nsMutableDictionary, IsNSString path) => nsMutableDictionary -> path -> IO (Id NSMutableDictionary)
initWithContentsOfFile nsMutableDictionary path =
  sendOwnedMessage nsMutableDictionary initWithContentsOfFileSelector (toNSString path)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSMutableDictionary nsMutableDictionary, IsNSURL url) => nsMutableDictionary -> url -> IO (Id NSMutableDictionary)
initWithContentsOfURL nsMutableDictionary url =
  sendOwnedMessage nsMutableDictionary initWithContentsOfURLSelector (toNSURL url)

-- | @- addEntriesFromDictionary:@
addEntriesFromDictionary :: (IsNSMutableDictionary nsMutableDictionary, IsNSDictionary otherDictionary) => nsMutableDictionary -> otherDictionary -> IO ()
addEntriesFromDictionary nsMutableDictionary otherDictionary =
  sendMessage nsMutableDictionary addEntriesFromDictionarySelector (toNSDictionary otherDictionary)

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> IO ()
removeAllObjects nsMutableDictionary =
  sendMessage nsMutableDictionary removeAllObjectsSelector

-- | @- removeObjectsForKeys:@
removeObjectsForKeys :: (IsNSMutableDictionary nsMutableDictionary, IsNSArray keyArray) => nsMutableDictionary -> keyArray -> IO ()
removeObjectsForKeys nsMutableDictionary keyArray =
  sendMessage nsMutableDictionary removeObjectsForKeysSelector (toNSArray keyArray)

-- | @- setDictionary:@
setDictionary :: (IsNSMutableDictionary nsMutableDictionary, IsNSDictionary otherDictionary) => nsMutableDictionary -> otherDictionary -> IO ()
setDictionary nsMutableDictionary otherDictionary =
  sendMessage nsMutableDictionary setDictionarySelector (toNSDictionary otherDictionary)

-- | @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> RawId -> RawId -> IO ()
setObject_forKeyedSubscript nsMutableDictionary obj_ key =
  sendMessage nsMutableDictionary setObject_forKeyedSubscriptSelector obj_ key

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector '[RawId] ()
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector '[RawId, RawId] ()
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMutableDictionary)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSMutableDictionary)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMutableDictionary)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @dictionaryWithSharedKeySet:@
dictionaryWithSharedKeySetSelector :: Selector '[RawId] (Id NSMutableDictionary)
dictionaryWithSharedKeySetSelector = mkSelector "dictionaryWithSharedKeySet:"

-- | @Selector@ for @dictionaryWithCapacity:@
dictionaryWithCapacitySelector :: Selector '[CULong] (Id NSMutableDictionary)
dictionaryWithCapacitySelector = mkSelector "dictionaryWithCapacity:"

-- | @Selector@ for @dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSMutableDictionary)
dictionaryWithContentsOfFileSelector = mkSelector "dictionaryWithContentsOfFile:"

-- | @Selector@ for @dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSMutableDictionary)
dictionaryWithContentsOfURLSelector = mkSelector "dictionaryWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSMutableDictionary)
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSMutableDictionary)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @addEntriesFromDictionary:@
addEntriesFromDictionarySelector :: Selector '[Id NSDictionary] ()
addEntriesFromDictionarySelector = mkSelector "addEntriesFromDictionary:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @removeObjectsForKeys:@
removeObjectsForKeysSelector :: Selector '[Id NSArray] ()
removeObjectsForKeysSelector = mkSelector "removeObjectsForKeys:"

-- | @Selector@ for @setDictionary:@
setDictionarySelector :: Selector '[Id NSDictionary] ()
setDictionarySelector = mkSelector "setDictionary:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector '[RawId, RawId] ()
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

