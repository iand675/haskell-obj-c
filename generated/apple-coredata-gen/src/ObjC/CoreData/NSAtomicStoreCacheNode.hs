{-# LANGUAGE DataKinds #-}
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
  , objectIDSelector
  , propertyCacheSelector
  , setPropertyCacheSelector
  , setValue_forKeySelector
  , valueForKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithObjectID:@
initWithObjectID :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSManagedObjectID moid) => nsAtomicStoreCacheNode -> moid -> IO (Id NSAtomicStoreCacheNode)
initWithObjectID nsAtomicStoreCacheNode moid =
  sendOwnedMessage nsAtomicStoreCacheNode initWithObjectIDSelector (toNSManagedObjectID moid)

-- | @- valueForKey:@
valueForKey :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSString key) => nsAtomicStoreCacheNode -> key -> IO RawId
valueForKey nsAtomicStoreCacheNode key =
  sendMessage nsAtomicStoreCacheNode valueForKeySelector (toNSString key)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSString key) => nsAtomicStoreCacheNode -> RawId -> key -> IO ()
setValue_forKey nsAtomicStoreCacheNode value key =
  sendMessage nsAtomicStoreCacheNode setValue_forKeySelector value (toNSString key)

-- | @- objectID@
objectID :: IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode => nsAtomicStoreCacheNode -> IO (Id NSManagedObjectID)
objectID nsAtomicStoreCacheNode =
  sendMessage nsAtomicStoreCacheNode objectIDSelector

-- | @- propertyCache@
propertyCache :: IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode => nsAtomicStoreCacheNode -> IO (Id NSMutableDictionary)
propertyCache nsAtomicStoreCacheNode =
  sendMessage nsAtomicStoreCacheNode propertyCacheSelector

-- | @- setPropertyCache:@
setPropertyCache :: (IsNSAtomicStoreCacheNode nsAtomicStoreCacheNode, IsNSMutableDictionary value) => nsAtomicStoreCacheNode -> value -> IO ()
setPropertyCache nsAtomicStoreCacheNode value =
  sendMessage nsAtomicStoreCacheNode setPropertyCacheSelector (toNSMutableDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector '[Id NSManagedObjectID] (Id NSAtomicStoreCacheNode)
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector '[] (Id NSManagedObjectID)
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @propertyCache@
propertyCacheSelector :: Selector '[] (Id NSMutableDictionary)
propertyCacheSelector = mkSelector "propertyCache"

-- | @Selector@ for @setPropertyCache:@
setPropertyCacheSelector :: Selector '[Id NSMutableDictionary] ()
setPropertyCacheSelector = mkSelector "setPropertyCache:"

