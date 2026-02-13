{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Counted Set	***************
--
-- Generated bindings for @NSCountedSet@.
module ObjC.Foundation.NSCountedSet
  ( NSCountedSet
  , IsNSCountedSet(..)
  , initWithCapacity
  , initWithArray
  , initWithSet
  , countForObject
  , objectEnumerator
  , addObject
  , removeObject
  , addObjectSelector
  , countForObjectSelector
  , initWithArraySelector
  , initWithCapacitySelector
  , initWithSetSelector
  , objectEnumeratorSelector
  , removeObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCapacity:@
initWithCapacity :: IsNSCountedSet nsCountedSet => nsCountedSet -> CULong -> IO (Id NSCountedSet)
initWithCapacity nsCountedSet numItems =
  sendOwnedMessage nsCountedSet initWithCapacitySelector numItems

-- | @- initWithArray:@
initWithArray :: (IsNSCountedSet nsCountedSet, IsNSArray array) => nsCountedSet -> array -> IO (Id NSCountedSet)
initWithArray nsCountedSet array =
  sendOwnedMessage nsCountedSet initWithArraySelector (toNSArray array)

-- | @- initWithSet:@
initWithSet :: (IsNSCountedSet nsCountedSet, IsNSSet set) => nsCountedSet -> set -> IO (Id NSCountedSet)
initWithSet nsCountedSet set =
  sendOwnedMessage nsCountedSet initWithSetSelector (toNSSet set)

-- | @- countForObject:@
countForObject :: IsNSCountedSet nsCountedSet => nsCountedSet -> RawId -> IO CULong
countForObject nsCountedSet object =
  sendMessage nsCountedSet countForObjectSelector object

-- | @- objectEnumerator@
objectEnumerator :: IsNSCountedSet nsCountedSet => nsCountedSet -> IO (Id NSEnumerator)
objectEnumerator nsCountedSet =
  sendMessage nsCountedSet objectEnumeratorSelector

-- | @- addObject:@
addObject :: IsNSCountedSet nsCountedSet => nsCountedSet -> RawId -> IO ()
addObject nsCountedSet object =
  sendMessage nsCountedSet addObjectSelector object

-- | @- removeObject:@
removeObject :: IsNSCountedSet nsCountedSet => nsCountedSet -> RawId -> IO ()
removeObject nsCountedSet object =
  sendMessage nsCountedSet removeObjectSelector object

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSCountedSet)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector '[Id NSArray] (Id NSCountedSet)
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @initWithSet:@
initWithSetSelector :: Selector '[Id NSSet] (Id NSCountedSet)
initWithSetSelector = mkSelector "initWithSet:"

-- | @Selector@ for @countForObject:@
countForObjectSelector :: Selector '[RawId] CULong
countForObjectSelector = mkSelector "countForObject:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

