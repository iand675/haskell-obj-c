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
  , removeObjectForKeySelector
  , setObject_forKeySelector
  , initSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , setValue_forKeySelector
  , dictionaryWithSharedKeySetSelector
  , dictionaryWithCapacitySelector
  , dictionaryWithContentsOfFileSelector
  , dictionaryWithContentsOfURLSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , addEntriesFromDictionarySelector
  , removeAllObjectsSelector
  , removeObjectsForKeysSelector
  , setDictionarySelector
  , setObject_forKeyedSubscriptSelector


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

-- | @- removeObjectForKey:@
removeObjectForKey :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> RawId -> IO ()
removeObjectForKey nsMutableDictionary  aKey =
  sendMsg nsMutableDictionary (mkSelector "removeObjectForKey:") retVoid [argPtr (castPtr (unRawId aKey) :: Ptr ())]

-- | @- setObject:forKey:@
setObject_forKey :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> RawId -> RawId -> IO ()
setObject_forKey nsMutableDictionary  anObject aKey =
  sendMsg nsMutableDictionary (mkSelector "setObject:forKey:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr (unRawId aKey) :: Ptr ())]

-- | @- init@
init_ :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> IO (Id NSMutableDictionary)
init_ nsMutableDictionary  =
  sendMsg nsMutableDictionary (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CULong -> IO (Id NSMutableDictionary)
initWithCapacity nsMutableDictionary  numItems =
  sendMsg nsMutableDictionary (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableDictionary nsMutableDictionary, IsNSCoder coder) => nsMutableDictionary -> coder -> IO (Id NSMutableDictionary)
initWithCoder nsMutableDictionary  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsMutableDictionary (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSMutableDictionary nsMutableDictionary, IsNSString key) => nsMutableDictionary -> RawId -> key -> IO ()
setValue_forKey nsMutableDictionary  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsMutableDictionary (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @+ dictionaryWithSharedKeySet:@
dictionaryWithSharedKeySet :: RawId -> IO (Id NSMutableDictionary)
dictionaryWithSharedKeySet keyset =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMsg cls' (mkSelector "dictionaryWithSharedKeySet:") (retPtr retVoid) [argPtr (castPtr (unRawId keyset) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dictionaryWithCapacity:@
dictionaryWithCapacity :: CULong -> IO (Id NSMutableDictionary)
dictionaryWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMsg cls' (mkSelector "dictionaryWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= retainedObject . castPtr

-- | @+ dictionaryWithContentsOfFile:@
dictionaryWithContentsOfFile :: IsNSString path => path -> IO (Id NSMutableDictionary)
dictionaryWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "dictionaryWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dictionaryWithContentsOfURL:@
dictionaryWithContentsOfURL :: IsNSURL url => url -> IO (Id NSMutableDictionary)
dictionaryWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "dictionaryWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSMutableDictionary nsMutableDictionary, IsNSString path) => nsMutableDictionary -> path -> IO (Id NSMutableDictionary)
initWithContentsOfFile nsMutableDictionary  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsMutableDictionary (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSMutableDictionary nsMutableDictionary, IsNSURL url) => nsMutableDictionary -> url -> IO (Id NSMutableDictionary)
initWithContentsOfURL nsMutableDictionary  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsMutableDictionary (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- addEntriesFromDictionary:@
addEntriesFromDictionary :: (IsNSMutableDictionary nsMutableDictionary, IsNSDictionary otherDictionary) => nsMutableDictionary -> otherDictionary -> IO ()
addEntriesFromDictionary nsMutableDictionary  otherDictionary =
withObjCPtr otherDictionary $ \raw_otherDictionary ->
    sendMsg nsMutableDictionary (mkSelector "addEntriesFromDictionary:") retVoid [argPtr (castPtr raw_otherDictionary :: Ptr ())]

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> IO ()
removeAllObjects nsMutableDictionary  =
  sendMsg nsMutableDictionary (mkSelector "removeAllObjects") retVoid []

-- | @- removeObjectsForKeys:@
removeObjectsForKeys :: (IsNSMutableDictionary nsMutableDictionary, IsNSArray keyArray) => nsMutableDictionary -> keyArray -> IO ()
removeObjectsForKeys nsMutableDictionary  keyArray =
withObjCPtr keyArray $ \raw_keyArray ->
    sendMsg nsMutableDictionary (mkSelector "removeObjectsForKeys:") retVoid [argPtr (castPtr raw_keyArray :: Ptr ())]

-- | @- setDictionary:@
setDictionary :: (IsNSMutableDictionary nsMutableDictionary, IsNSDictionary otherDictionary) => nsMutableDictionary -> otherDictionary -> IO ()
setDictionary nsMutableDictionary  otherDictionary =
withObjCPtr otherDictionary $ \raw_otherDictionary ->
    sendMsg nsMutableDictionary (mkSelector "setDictionary:") retVoid [argPtr (castPtr raw_otherDictionary :: Ptr ())]

-- | @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> RawId -> RawId -> IO ()
setObject_forKeyedSubscript nsMutableDictionary  obj_ key =
  sendMsg nsMutableDictionary (mkSelector "setObject:forKeyedSubscript:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @dictionaryWithSharedKeySet:@
dictionaryWithSharedKeySetSelector :: Selector
dictionaryWithSharedKeySetSelector = mkSelector "dictionaryWithSharedKeySet:"

-- | @Selector@ for @dictionaryWithCapacity:@
dictionaryWithCapacitySelector :: Selector
dictionaryWithCapacitySelector = mkSelector "dictionaryWithCapacity:"

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

-- | @Selector@ for @addEntriesFromDictionary:@
addEntriesFromDictionarySelector :: Selector
addEntriesFromDictionarySelector = mkSelector "addEntriesFromDictionary:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @removeObjectsForKeys:@
removeObjectsForKeysSelector :: Selector
removeObjectsForKeysSelector = mkSelector "removeObjectsForKeys:"

-- | @Selector@ for @setDictionary:@
setDictionarySelector :: Selector
setDictionarySelector = mkSelector "setDictionary:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

