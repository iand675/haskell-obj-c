{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAtomicStoreCacheNode@.
module ObjC.CoreData.NSAtomicStoreCacheNode
  ( NSAtomicStoreCacheNode
  , IsNSAtomicStoreCacheNode(..)
  , initWithObjectID
  , valueForKey
  , setValue_forKey
  , objectID
  , propertyCache
  , setPropertyCache
  , initWithObjectIDSelector
  , valueForKeySelector
  , setValue_forKeySelector
  , objectIDSelector
  , propertyCacheSelector
  , setPropertyCacheSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithObjectID:@
initWithObjectID :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSManagedObjectID moid) => nsAtomicStoreCacheNode -> moid -> IO (Id NSAtomicStoreCacheNode)
initWithObjectID nsAtomicStoreCacheNode  moid =
withObjCPtr moid $ \raw_moid ->
    sendMsg nsAtomicStoreCacheNode (mkSelector "initWithObjectID:") (retPtr retVoid) [argPtr (castPtr raw_moid :: Ptr ())] >>= ownedObject . castPtr

-- | @- valueForKey:@
valueForKey :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSString key) => nsAtomicStoreCacheNode -> key -> IO RawId
valueForKey nsAtomicStoreCacheNode  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsAtomicStoreCacheNode (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSString key) => nsAtomicStoreCacheNode -> RawId -> key -> IO ()
setValue_forKey nsAtomicStoreCacheNode  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsAtomicStoreCacheNode (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- objectID@
objectID :: IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode => nsAtomicStoreCacheNode -> IO (Id NSManagedObjectID)
objectID nsAtomicStoreCacheNode  =
  sendMsg nsAtomicStoreCacheNode (mkSelector "objectID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- propertyCache@
propertyCache :: IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode => nsAtomicStoreCacheNode -> IO (Id NSMutableDictionary)
propertyCache nsAtomicStoreCacheNode  =
  sendMsg nsAtomicStoreCacheNode (mkSelector "propertyCache") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPropertyCache:@
setPropertyCache :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSMutableDictionary value) => nsAtomicStoreCacheNode -> value -> IO ()
setPropertyCache nsAtomicStoreCacheNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAtomicStoreCacheNode (mkSelector "setPropertyCache:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @propertyCache@
propertyCacheSelector :: Selector
propertyCacheSelector = mkSelector "propertyCache"

-- | @Selector@ for @setPropertyCache:@
setPropertyCacheSelector :: Selector
setPropertyCacheSelector = mkSelector "setPropertyCache:"

