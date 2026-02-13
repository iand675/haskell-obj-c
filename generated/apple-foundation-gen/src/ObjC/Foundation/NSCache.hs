{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCache@.
module ObjC.Foundation.NSCache
  ( NSCache
  , IsNSCache(..)
  , objectForKey
  , setObject_forKey
  , setObject_forKey_cost
  , removeObjectForKey
  , removeAllObjects
  , name
  , setName
  , delegate
  , setDelegate
  , totalCostLimit
  , setTotalCostLimit
  , countLimit
  , setCountLimit
  , evictsObjectsWithDiscardedContent
  , setEvictsObjectsWithDiscardedContent
  , countLimitSelector
  , delegateSelector
  , evictsObjectsWithDiscardedContentSelector
  , nameSelector
  , objectForKeySelector
  , removeAllObjectsSelector
  , removeObjectForKeySelector
  , setCountLimitSelector
  , setDelegateSelector
  , setEvictsObjectsWithDiscardedContentSelector
  , setNameSelector
  , setObject_forKeySelector
  , setObject_forKey_costSelector
  , setTotalCostLimitSelector
  , totalCostLimitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- objectForKey:@
objectForKey :: IsNSCache nsCache => nsCache -> RawId -> IO RawId
objectForKey nsCache key =
  sendMessage nsCache objectForKeySelector key

-- | @- setObject:forKey:@
setObject_forKey :: IsNSCache nsCache => nsCache -> RawId -> RawId -> IO ()
setObject_forKey nsCache obj_ key =
  sendMessage nsCache setObject_forKeySelector obj_ key

-- | @- setObject:forKey:cost:@
setObject_forKey_cost :: IsNSCache nsCache => nsCache -> RawId -> RawId -> CULong -> IO ()
setObject_forKey_cost nsCache obj_ key g =
  sendMessage nsCache setObject_forKey_costSelector obj_ key g

-- | @- removeObjectForKey:@
removeObjectForKey :: IsNSCache nsCache => nsCache -> RawId -> IO ()
removeObjectForKey nsCache key =
  sendMessage nsCache removeObjectForKeySelector key

-- | @- removeAllObjects@
removeAllObjects :: IsNSCache nsCache => nsCache -> IO ()
removeAllObjects nsCache =
  sendMessage nsCache removeAllObjectsSelector

-- | @- name@
name :: IsNSCache nsCache => nsCache -> IO (Id NSString)
name nsCache =
  sendMessage nsCache nameSelector

-- | @- setName:@
setName :: (IsNSCache nsCache, IsNSString value) => nsCache -> value -> IO ()
setName nsCache value =
  sendMessage nsCache setNameSelector (toNSString value)

-- | @- delegate@
delegate :: IsNSCache nsCache => nsCache -> IO RawId
delegate nsCache =
  sendMessage nsCache delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSCache nsCache => nsCache -> RawId -> IO ()
setDelegate nsCache value =
  sendMessage nsCache setDelegateSelector value

-- | @- totalCostLimit@
totalCostLimit :: IsNSCache nsCache => nsCache -> IO CULong
totalCostLimit nsCache =
  sendMessage nsCache totalCostLimitSelector

-- | @- setTotalCostLimit:@
setTotalCostLimit :: IsNSCache nsCache => nsCache -> CULong -> IO ()
setTotalCostLimit nsCache value =
  sendMessage nsCache setTotalCostLimitSelector value

-- | @- countLimit@
countLimit :: IsNSCache nsCache => nsCache -> IO CULong
countLimit nsCache =
  sendMessage nsCache countLimitSelector

-- | @- setCountLimit:@
setCountLimit :: IsNSCache nsCache => nsCache -> CULong -> IO ()
setCountLimit nsCache value =
  sendMessage nsCache setCountLimitSelector value

-- | @- evictsObjectsWithDiscardedContent@
evictsObjectsWithDiscardedContent :: IsNSCache nsCache => nsCache -> IO Bool
evictsObjectsWithDiscardedContent nsCache =
  sendMessage nsCache evictsObjectsWithDiscardedContentSelector

-- | @- setEvictsObjectsWithDiscardedContent:@
setEvictsObjectsWithDiscardedContent :: IsNSCache nsCache => nsCache -> Bool -> IO ()
setEvictsObjectsWithDiscardedContent nsCache value =
  sendMessage nsCache setEvictsObjectsWithDiscardedContentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[RawId] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector '[RawId, RawId] ()
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @setObject:forKey:cost:@
setObject_forKey_costSelector :: Selector '[RawId, RawId, CULong] ()
setObject_forKey_costSelector = mkSelector "setObject:forKey:cost:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector '[RawId] ()
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @totalCostLimit@
totalCostLimitSelector :: Selector '[] CULong
totalCostLimitSelector = mkSelector "totalCostLimit"

-- | @Selector@ for @setTotalCostLimit:@
setTotalCostLimitSelector :: Selector '[CULong] ()
setTotalCostLimitSelector = mkSelector "setTotalCostLimit:"

-- | @Selector@ for @countLimit@
countLimitSelector :: Selector '[] CULong
countLimitSelector = mkSelector "countLimit"

-- | @Selector@ for @setCountLimit:@
setCountLimitSelector :: Selector '[CULong] ()
setCountLimitSelector = mkSelector "setCountLimit:"

-- | @Selector@ for @evictsObjectsWithDiscardedContent@
evictsObjectsWithDiscardedContentSelector :: Selector '[] Bool
evictsObjectsWithDiscardedContentSelector = mkSelector "evictsObjectsWithDiscardedContent"

-- | @Selector@ for @setEvictsObjectsWithDiscardedContent:@
setEvictsObjectsWithDiscardedContentSelector :: Selector '[Bool] ()
setEvictsObjectsWithDiscardedContentSelector = mkSelector "setEvictsObjectsWithDiscardedContent:"

