{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Mutable Set	***************
--
-- Generated bindings for @NSMutableSet@.
module ObjC.Foundation.NSMutableSet
  ( NSMutableSet
  , IsNSMutableSet(..)
  , addObject
  , removeObject
  , initWithCoder
  , init_
  , initWithCapacity
  , filterUsingPredicate
  , setWithCapacity
  , addObjectsFromArray
  , intersectSet
  , minusSet
  , removeAllObjects
  , unionSet
  , setSet
  , addObjectSelector
  , addObjectsFromArraySelector
  , filterUsingPredicateSelector
  , initSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , intersectSetSelector
  , minusSetSelector
  , removeAllObjectsSelector
  , removeObjectSelector
  , setSetSelector
  , setWithCapacitySelector
  , unionSetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- addObject:@
addObject :: IsNSMutableSet nsMutableSet => nsMutableSet -> RawId -> IO ()
addObject nsMutableSet object =
  sendMessage nsMutableSet addObjectSelector object

-- | @- removeObject:@
removeObject :: IsNSMutableSet nsMutableSet => nsMutableSet -> RawId -> IO ()
removeObject nsMutableSet object =
  sendMessage nsMutableSet removeObjectSelector object

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableSet nsMutableSet, IsNSCoder coder) => nsMutableSet -> coder -> IO (Id NSMutableSet)
initWithCoder nsMutableSet coder =
  sendOwnedMessage nsMutableSet initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSMutableSet nsMutableSet => nsMutableSet -> IO (Id NSMutableSet)
init_ nsMutableSet =
  sendOwnedMessage nsMutableSet initSelector

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableSet nsMutableSet => nsMutableSet -> CULong -> IO (Id NSMutableSet)
initWithCapacity nsMutableSet numItems =
  sendOwnedMessage nsMutableSet initWithCapacitySelector numItems

-- | @- filterUsingPredicate:@
filterUsingPredicate :: (IsNSMutableSet nsMutableSet, IsNSPredicate predicate) => nsMutableSet -> predicate -> IO ()
filterUsingPredicate nsMutableSet predicate =
  sendMessage nsMutableSet filterUsingPredicateSelector (toNSPredicate predicate)

-- | @+ setWithCapacity:@
setWithCapacity :: CULong -> IO (Id NSMutableSet)
setWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableSet"
    sendClassMessage cls' setWithCapacitySelector numItems

-- | @- addObjectsFromArray:@
addObjectsFromArray :: (IsNSMutableSet nsMutableSet, IsNSArray array) => nsMutableSet -> array -> IO ()
addObjectsFromArray nsMutableSet array =
  sendMessage nsMutableSet addObjectsFromArraySelector (toNSArray array)

-- | @- intersectSet:@
intersectSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
intersectSet nsMutableSet otherSet =
  sendMessage nsMutableSet intersectSetSelector (toNSSet otherSet)

-- | @- minusSet:@
minusSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
minusSet nsMutableSet otherSet =
  sendMessage nsMutableSet minusSetSelector (toNSSet otherSet)

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableSet nsMutableSet => nsMutableSet -> IO ()
removeAllObjects nsMutableSet =
  sendMessage nsMutableSet removeAllObjectsSelector

-- | @- unionSet:@
unionSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
unionSet nsMutableSet otherSet =
  sendMessage nsMutableSet unionSetSelector (toNSSet otherSet)

-- | @- setSet:@
setSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
setSet nsMutableSet otherSet =
  sendMessage nsMutableSet setSetSelector (toNSSet otherSet)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMutableSet)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMutableSet)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSMutableSet)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @filterUsingPredicate:@
filterUsingPredicateSelector :: Selector '[Id NSPredicate] ()
filterUsingPredicateSelector = mkSelector "filterUsingPredicate:"

-- | @Selector@ for @setWithCapacity:@
setWithCapacitySelector :: Selector '[CULong] (Id NSMutableSet)
setWithCapacitySelector = mkSelector "setWithCapacity:"

-- | @Selector@ for @addObjectsFromArray:@
addObjectsFromArraySelector :: Selector '[Id NSArray] ()
addObjectsFromArraySelector = mkSelector "addObjectsFromArray:"

-- | @Selector@ for @intersectSet:@
intersectSetSelector :: Selector '[Id NSSet] ()
intersectSetSelector = mkSelector "intersectSet:"

-- | @Selector@ for @minusSet:@
minusSetSelector :: Selector '[Id NSSet] ()
minusSetSelector = mkSelector "minusSet:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @unionSet:@
unionSetSelector :: Selector '[Id NSSet] ()
unionSetSelector = mkSelector "unionSet:"

-- | @Selector@ for @setSet:@
setSetSelector :: Selector '[Id NSSet] ()
setSetSelector = mkSelector "setSet:"

