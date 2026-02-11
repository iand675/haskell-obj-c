{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSHashTable@.
module ObjC.Foundation.NSHashTable
  ( NSHashTable
  , IsNSHashTable(..)
  , initWithOptions_capacity
  , initWithPointerFunctions_capacity
  , hashTableWithOptions
  , hashTableWithWeakObjects
  , weakObjectsHashTable
  , member
  , objectEnumerator
  , addObject
  , removeObject
  , removeAllObjects
  , containsObject
  , intersectsHashTable
  , isEqualToHashTable
  , isSubsetOfHashTable
  , intersectHashTable
  , unionHashTable
  , minusHashTable
  , pointerFunctions
  , count
  , allObjects
  , anyObject
  , setRepresentation
  , initWithOptions_capacitySelector
  , initWithPointerFunctions_capacitySelector
  , hashTableWithOptionsSelector
  , hashTableWithWeakObjectsSelector
  , weakObjectsHashTableSelector
  , memberSelector
  , objectEnumeratorSelector
  , addObjectSelector
  , removeObjectSelector
  , removeAllObjectsSelector
  , containsObjectSelector
  , intersectsHashTableSelector
  , isEqualToHashTableSelector
  , isSubsetOfHashTableSelector
  , intersectHashTableSelector
  , unionHashTableSelector
  , minusHashTableSelector
  , pointerFunctionsSelector
  , countSelector
  , allObjectsSelector
  , anyObjectSelector
  , setRepresentationSelector

  -- * Enum types
  , NSPointerFunctionsOptions(NSPointerFunctionsOptions)
  , pattern NSPointerFunctionsStrongMemory
  , pattern NSPointerFunctionsZeroingWeakMemory
  , pattern NSPointerFunctionsOpaqueMemory
  , pattern NSPointerFunctionsMallocMemory
  , pattern NSPointerFunctionsMachVirtualMemory
  , pattern NSPointerFunctionsWeakMemory
  , pattern NSPointerFunctionsObjectPersonality
  , pattern NSPointerFunctionsOpaquePersonality
  , pattern NSPointerFunctionsObjectPointerPersonality
  , pattern NSPointerFunctionsCStringPersonality
  , pattern NSPointerFunctionsStructPersonality
  , pattern NSPointerFunctionsIntegerPersonality
  , pattern NSPointerFunctionsCopyIn

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
import ObjC.Foundation.Internal.Enums

-- | @- initWithOptions:capacity:@
initWithOptions_capacity :: IsNSHashTable nsHashTable => nsHashTable -> NSPointerFunctionsOptions -> CULong -> IO (Id NSHashTable)
initWithOptions_capacity nsHashTable  options initialCapacity =
  sendMsg nsHashTable (mkSelector "initWithOptions:capacity:") (retPtr retVoid) [argCULong (coerce options), argCULong (fromIntegral initialCapacity)] >>= ownedObject . castPtr

-- | @- initWithPointerFunctions:capacity:@
initWithPointerFunctions_capacity :: (IsNSHashTable nsHashTable, IsNSPointerFunctions functions) => nsHashTable -> functions -> CULong -> IO (Id NSHashTable)
initWithPointerFunctions_capacity nsHashTable  functions initialCapacity =
withObjCPtr functions $ \raw_functions ->
    sendMsg nsHashTable (mkSelector "initWithPointerFunctions:capacity:") (retPtr retVoid) [argPtr (castPtr raw_functions :: Ptr ()), argCULong (fromIntegral initialCapacity)] >>= ownedObject . castPtr

-- | @+ hashTableWithOptions:@
hashTableWithOptions :: NSPointerFunctionsOptions -> IO (Id NSHashTable)
hashTableWithOptions options =
  do
    cls' <- getRequiredClass "NSHashTable"
    sendClassMsg cls' (mkSelector "hashTableWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ hashTableWithWeakObjects@
hashTableWithWeakObjects :: IO RawId
hashTableWithWeakObjects  =
  do
    cls' <- getRequiredClass "NSHashTable"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "hashTableWithWeakObjects") (retPtr retVoid) []

-- | @+ weakObjectsHashTable@
weakObjectsHashTable :: IO (Id NSHashTable)
weakObjectsHashTable  =
  do
    cls' <- getRequiredClass "NSHashTable"
    sendClassMsg cls' (mkSelector "weakObjectsHashTable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- member:@
member :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO RawId
member nsHashTable  object =
  fmap (RawId . castPtr) $ sendMsg nsHashTable (mkSelector "member:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- objectEnumerator@
objectEnumerator :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSEnumerator)
objectEnumerator nsHashTable  =
  sendMsg nsHashTable (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addObject:@
addObject :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO ()
addObject nsHashTable  object =
  sendMsg nsHashTable (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeObject:@
removeObject :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO ()
removeObject nsHashTable  object =
  sendMsg nsHashTable (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeAllObjects@
removeAllObjects :: IsNSHashTable nsHashTable => nsHashTable -> IO ()
removeAllObjects nsHashTable  =
  sendMsg nsHashTable (mkSelector "removeAllObjects") retVoid []

-- | @- containsObject:@
containsObject :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO Bool
containsObject nsHashTable  anObject =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHashTable (mkSelector "containsObject:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- intersectsHashTable:@
intersectsHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO Bool
intersectsHashTable nsHashTable  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHashTable (mkSelector "intersectsHashTable:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- isEqualToHashTable:@
isEqualToHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO Bool
isEqualToHashTable nsHashTable  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHashTable (mkSelector "isEqualToHashTable:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- isSubsetOfHashTable:@
isSubsetOfHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO Bool
isSubsetOfHashTable nsHashTable  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHashTable (mkSelector "isSubsetOfHashTable:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- intersectHashTable:@
intersectHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO ()
intersectHashTable nsHashTable  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsHashTable (mkSelector "intersectHashTable:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- unionHashTable:@
unionHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO ()
unionHashTable nsHashTable  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsHashTable (mkSelector "unionHashTable:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- minusHashTable:@
minusHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO ()
minusHashTable nsHashTable  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsHashTable (mkSelector "minusHashTable:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- pointerFunctions@
pointerFunctions :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSPointerFunctions)
pointerFunctions nsHashTable  =
  sendMsg nsHashTable (mkSelector "pointerFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- count@
count :: IsNSHashTable nsHashTable => nsHashTable -> IO CULong
count nsHashTable  =
  sendMsg nsHashTable (mkSelector "count") retCULong []

-- | @- allObjects@
allObjects :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSArray)
allObjects nsHashTable  =
  sendMsg nsHashTable (mkSelector "allObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- anyObject@
anyObject :: IsNSHashTable nsHashTable => nsHashTable -> IO RawId
anyObject nsHashTable  =
  fmap (RawId . castPtr) $ sendMsg nsHashTable (mkSelector "anyObject") (retPtr retVoid) []

-- | @- setRepresentation@
setRepresentation :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSSet)
setRepresentation nsHashTable  =
  sendMsg nsHashTable (mkSelector "setRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:capacity:@
initWithOptions_capacitySelector :: Selector
initWithOptions_capacitySelector = mkSelector "initWithOptions:capacity:"

-- | @Selector@ for @initWithPointerFunctions:capacity:@
initWithPointerFunctions_capacitySelector :: Selector
initWithPointerFunctions_capacitySelector = mkSelector "initWithPointerFunctions:capacity:"

-- | @Selector@ for @hashTableWithOptions:@
hashTableWithOptionsSelector :: Selector
hashTableWithOptionsSelector = mkSelector "hashTableWithOptions:"

-- | @Selector@ for @hashTableWithWeakObjects@
hashTableWithWeakObjectsSelector :: Selector
hashTableWithWeakObjectsSelector = mkSelector "hashTableWithWeakObjects"

-- | @Selector@ for @weakObjectsHashTable@
weakObjectsHashTableSelector :: Selector
weakObjectsHashTableSelector = mkSelector "weakObjectsHashTable"

-- | @Selector@ for @member:@
memberSelector :: Selector
memberSelector = mkSelector "member:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @intersectsHashTable:@
intersectsHashTableSelector :: Selector
intersectsHashTableSelector = mkSelector "intersectsHashTable:"

-- | @Selector@ for @isEqualToHashTable:@
isEqualToHashTableSelector :: Selector
isEqualToHashTableSelector = mkSelector "isEqualToHashTable:"

-- | @Selector@ for @isSubsetOfHashTable:@
isSubsetOfHashTableSelector :: Selector
isSubsetOfHashTableSelector = mkSelector "isSubsetOfHashTable:"

-- | @Selector@ for @intersectHashTable:@
intersectHashTableSelector :: Selector
intersectHashTableSelector = mkSelector "intersectHashTable:"

-- | @Selector@ for @unionHashTable:@
unionHashTableSelector :: Selector
unionHashTableSelector = mkSelector "unionHashTable:"

-- | @Selector@ for @minusHashTable:@
minusHashTableSelector :: Selector
minusHashTableSelector = mkSelector "minusHashTable:"

-- | @Selector@ for @pointerFunctions@
pointerFunctionsSelector :: Selector
pointerFunctionsSelector = mkSelector "pointerFunctions"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector
allObjectsSelector = mkSelector "allObjects"

-- | @Selector@ for @anyObject@
anyObjectSelector :: Selector
anyObjectSelector = mkSelector "anyObject"

-- | @Selector@ for @setRepresentation@
setRepresentationSelector :: Selector
setRepresentationSelector = mkSelector "setRepresentation"

