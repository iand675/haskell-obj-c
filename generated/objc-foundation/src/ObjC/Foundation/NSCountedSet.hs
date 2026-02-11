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
  , initWithCapacitySelector
  , initWithArraySelector
  , initWithSetSelector
  , countForObjectSelector
  , objectEnumeratorSelector
  , addObjectSelector
  , removeObjectSelector


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

-- | @- initWithCapacity:@
initWithCapacity :: IsNSCountedSet nsCountedSet => nsCountedSet -> CULong -> IO (Id NSCountedSet)
initWithCapacity nsCountedSet  numItems =
  sendMsg nsCountedSet (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= ownedObject . castPtr

-- | @- initWithArray:@
initWithArray :: (IsNSCountedSet nsCountedSet, IsNSArray array) => nsCountedSet -> array -> IO (Id NSCountedSet)
initWithArray nsCountedSet  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsCountedSet (mkSelector "initWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSet:@
initWithSet :: (IsNSCountedSet nsCountedSet, IsNSSet set) => nsCountedSet -> set -> IO (Id NSCountedSet)
initWithSet nsCountedSet  set =
withObjCPtr set $ \raw_set ->
    sendMsg nsCountedSet (mkSelector "initWithSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= ownedObject . castPtr

-- | @- countForObject:@
countForObject :: IsNSCountedSet nsCountedSet => nsCountedSet -> RawId -> IO CULong
countForObject nsCountedSet  object =
  sendMsg nsCountedSet (mkSelector "countForObject:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- objectEnumerator@
objectEnumerator :: IsNSCountedSet nsCountedSet => nsCountedSet -> IO (Id NSEnumerator)
objectEnumerator nsCountedSet  =
  sendMsg nsCountedSet (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addObject:@
addObject :: IsNSCountedSet nsCountedSet => nsCountedSet -> RawId -> IO ()
addObject nsCountedSet  object =
  sendMsg nsCountedSet (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeObject:@
removeObject :: IsNSCountedSet nsCountedSet => nsCountedSet -> RawId -> IO ()
removeObject nsCountedSet  object =
  sendMsg nsCountedSet (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @initWithSet:@
initWithSetSelector :: Selector
initWithSetSelector = mkSelector "initWithSet:"

-- | @Selector@ for @countForObject:@
countForObjectSelector :: Selector
countForObjectSelector = mkSelector "countForObject:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

