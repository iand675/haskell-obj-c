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
  , removeObjectSelector
  , initWithCoderSelector
  , initSelector
  , initWithCapacitySelector
  , filterUsingPredicateSelector
  , setWithCapacitySelector
  , addObjectsFromArraySelector
  , intersectSetSelector
  , minusSetSelector
  , removeAllObjectsSelector
  , unionSetSelector
  , setSetSelector


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

-- | @- addObject:@
addObject :: IsNSMutableSet nsMutableSet => nsMutableSet -> RawId -> IO ()
addObject nsMutableSet  object =
  sendMsg nsMutableSet (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeObject:@
removeObject :: IsNSMutableSet nsMutableSet => nsMutableSet -> RawId -> IO ()
removeObject nsMutableSet  object =
  sendMsg nsMutableSet (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- initWithCoder:@
initWithCoder :: (IsNSMutableSet nsMutableSet, IsNSCoder coder) => nsMutableSet -> coder -> IO (Id NSMutableSet)
initWithCoder nsMutableSet  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsMutableSet (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSMutableSet nsMutableSet => nsMutableSet -> IO (Id NSMutableSet)
init_ nsMutableSet  =
  sendMsg nsMutableSet (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableSet nsMutableSet => nsMutableSet -> CULong -> IO (Id NSMutableSet)
initWithCapacity nsMutableSet  numItems =
  sendMsg nsMutableSet (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= ownedObject . castPtr

-- | @- filterUsingPredicate:@
filterUsingPredicate :: (IsNSMutableSet nsMutableSet, IsNSPredicate predicate) => nsMutableSet -> predicate -> IO ()
filterUsingPredicate nsMutableSet  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsMutableSet (mkSelector "filterUsingPredicate:") retVoid [argPtr (castPtr raw_predicate :: Ptr ())]

-- | @+ setWithCapacity:@
setWithCapacity :: CULong -> IO (Id NSMutableSet)
setWithCapacity numItems =
  do
    cls' <- getRequiredClass "NSMutableSet"
    sendClassMsg cls' (mkSelector "setWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= retainedObject . castPtr

-- | @- addObjectsFromArray:@
addObjectsFromArray :: (IsNSMutableSet nsMutableSet, IsNSArray array) => nsMutableSet -> array -> IO ()
addObjectsFromArray nsMutableSet  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsMutableSet (mkSelector "addObjectsFromArray:") retVoid [argPtr (castPtr raw_array :: Ptr ())]

-- | @- intersectSet:@
intersectSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
intersectSet nsMutableSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    sendMsg nsMutableSet (mkSelector "intersectSet:") retVoid [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- minusSet:@
minusSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
minusSet nsMutableSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    sendMsg nsMutableSet (mkSelector "minusSet:") retVoid [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- removeAllObjects@
removeAllObjects :: IsNSMutableSet nsMutableSet => nsMutableSet -> IO ()
removeAllObjects nsMutableSet  =
  sendMsg nsMutableSet (mkSelector "removeAllObjects") retVoid []

-- | @- unionSet:@
unionSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
unionSet nsMutableSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    sendMsg nsMutableSet (mkSelector "unionSet:") retVoid [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- setSet:@
setSet :: (IsNSMutableSet nsMutableSet, IsNSSet otherSet) => nsMutableSet -> otherSet -> IO ()
setSet nsMutableSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    sendMsg nsMutableSet (mkSelector "setSet:") retVoid [argPtr (castPtr raw_otherSet :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @filterUsingPredicate:@
filterUsingPredicateSelector :: Selector
filterUsingPredicateSelector = mkSelector "filterUsingPredicate:"

-- | @Selector@ for @setWithCapacity:@
setWithCapacitySelector :: Selector
setWithCapacitySelector = mkSelector "setWithCapacity:"

-- | @Selector@ for @addObjectsFromArray:@
addObjectsFromArraySelector :: Selector
addObjectsFromArraySelector = mkSelector "addObjectsFromArray:"

-- | @Selector@ for @intersectSet:@
intersectSetSelector :: Selector
intersectSetSelector = mkSelector "intersectSet:"

-- | @Selector@ for @minusSet:@
minusSetSelector :: Selector
minusSetSelector = mkSelector "minusSet:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @unionSet:@
unionSetSelector :: Selector
unionSetSelector = mkSelector "unionSet:"

-- | @Selector@ for @setSet:@
setSetSelector :: Selector
setSetSelector = mkSelector "setSet:"

