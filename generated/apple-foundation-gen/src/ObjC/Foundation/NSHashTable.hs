{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addObjectSelector
  , allObjectsSelector
  , anyObjectSelector
  , containsObjectSelector
  , countSelector
  , hashTableWithOptionsSelector
  , hashTableWithWeakObjectsSelector
  , initWithOptions_capacitySelector
  , initWithPointerFunctions_capacitySelector
  , intersectHashTableSelector
  , intersectsHashTableSelector
  , isEqualToHashTableSelector
  , isSubsetOfHashTableSelector
  , memberSelector
  , minusHashTableSelector
  , objectEnumeratorSelector
  , pointerFunctionsSelector
  , removeAllObjectsSelector
  , removeObjectSelector
  , setRepresentationSelector
  , unionHashTableSelector
  , weakObjectsHashTableSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithOptions:capacity:@
initWithOptions_capacity :: IsNSHashTable nsHashTable => nsHashTable -> NSPointerFunctionsOptions -> CULong -> IO (Id NSHashTable)
initWithOptions_capacity nsHashTable options initialCapacity =
  sendOwnedMessage nsHashTable initWithOptions_capacitySelector options initialCapacity

-- | @- initWithPointerFunctions:capacity:@
initWithPointerFunctions_capacity :: (IsNSHashTable nsHashTable, IsNSPointerFunctions functions) => nsHashTable -> functions -> CULong -> IO (Id NSHashTable)
initWithPointerFunctions_capacity nsHashTable functions initialCapacity =
  sendOwnedMessage nsHashTable initWithPointerFunctions_capacitySelector (toNSPointerFunctions functions) initialCapacity

-- | @+ hashTableWithOptions:@
hashTableWithOptions :: NSPointerFunctionsOptions -> IO (Id NSHashTable)
hashTableWithOptions options =
  do
    cls' <- getRequiredClass "NSHashTable"
    sendClassMessage cls' hashTableWithOptionsSelector options

-- | @+ hashTableWithWeakObjects@
hashTableWithWeakObjects :: IO RawId
hashTableWithWeakObjects  =
  do
    cls' <- getRequiredClass "NSHashTable"
    sendClassMessage cls' hashTableWithWeakObjectsSelector

-- | @+ weakObjectsHashTable@
weakObjectsHashTable :: IO (Id NSHashTable)
weakObjectsHashTable  =
  do
    cls' <- getRequiredClass "NSHashTable"
    sendClassMessage cls' weakObjectsHashTableSelector

-- | @- member:@
member :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO RawId
member nsHashTable object =
  sendMessage nsHashTable memberSelector object

-- | @- objectEnumerator@
objectEnumerator :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSEnumerator)
objectEnumerator nsHashTable =
  sendMessage nsHashTable objectEnumeratorSelector

-- | @- addObject:@
addObject :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO ()
addObject nsHashTable object =
  sendMessage nsHashTable addObjectSelector object

-- | @- removeObject:@
removeObject :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO ()
removeObject nsHashTable object =
  sendMessage nsHashTable removeObjectSelector object

-- | @- removeAllObjects@
removeAllObjects :: IsNSHashTable nsHashTable => nsHashTable -> IO ()
removeAllObjects nsHashTable =
  sendMessage nsHashTable removeAllObjectsSelector

-- | @- containsObject:@
containsObject :: IsNSHashTable nsHashTable => nsHashTable -> RawId -> IO Bool
containsObject nsHashTable anObject =
  sendMessage nsHashTable containsObjectSelector anObject

-- | @- intersectsHashTable:@
intersectsHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO Bool
intersectsHashTable nsHashTable other =
  sendMessage nsHashTable intersectsHashTableSelector (toNSHashTable other)

-- | @- isEqualToHashTable:@
isEqualToHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO Bool
isEqualToHashTable nsHashTable other =
  sendMessage nsHashTable isEqualToHashTableSelector (toNSHashTable other)

-- | @- isSubsetOfHashTable:@
isSubsetOfHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO Bool
isSubsetOfHashTable nsHashTable other =
  sendMessage nsHashTable isSubsetOfHashTableSelector (toNSHashTable other)

-- | @- intersectHashTable:@
intersectHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO ()
intersectHashTable nsHashTable other =
  sendMessage nsHashTable intersectHashTableSelector (toNSHashTable other)

-- | @- unionHashTable:@
unionHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO ()
unionHashTable nsHashTable other =
  sendMessage nsHashTable unionHashTableSelector (toNSHashTable other)

-- | @- minusHashTable:@
minusHashTable :: (IsNSHashTable nsHashTable, IsNSHashTable other) => nsHashTable -> other -> IO ()
minusHashTable nsHashTable other =
  sendMessage nsHashTable minusHashTableSelector (toNSHashTable other)

-- | @- pointerFunctions@
pointerFunctions :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSPointerFunctions)
pointerFunctions nsHashTable =
  sendMessage nsHashTable pointerFunctionsSelector

-- | @- count@
count :: IsNSHashTable nsHashTable => nsHashTable -> IO CULong
count nsHashTable =
  sendMessage nsHashTable countSelector

-- | @- allObjects@
allObjects :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSArray)
allObjects nsHashTable =
  sendMessage nsHashTable allObjectsSelector

-- | @- anyObject@
anyObject :: IsNSHashTable nsHashTable => nsHashTable -> IO RawId
anyObject nsHashTable =
  sendMessage nsHashTable anyObjectSelector

-- | @- setRepresentation@
setRepresentation :: IsNSHashTable nsHashTable => nsHashTable -> IO (Id NSSet)
setRepresentation nsHashTable =
  sendMessage nsHashTable setRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:capacity:@
initWithOptions_capacitySelector :: Selector '[NSPointerFunctionsOptions, CULong] (Id NSHashTable)
initWithOptions_capacitySelector = mkSelector "initWithOptions:capacity:"

-- | @Selector@ for @initWithPointerFunctions:capacity:@
initWithPointerFunctions_capacitySelector :: Selector '[Id NSPointerFunctions, CULong] (Id NSHashTable)
initWithPointerFunctions_capacitySelector = mkSelector "initWithPointerFunctions:capacity:"

-- | @Selector@ for @hashTableWithOptions:@
hashTableWithOptionsSelector :: Selector '[NSPointerFunctionsOptions] (Id NSHashTable)
hashTableWithOptionsSelector = mkSelector "hashTableWithOptions:"

-- | @Selector@ for @hashTableWithWeakObjects@
hashTableWithWeakObjectsSelector :: Selector '[] RawId
hashTableWithWeakObjectsSelector = mkSelector "hashTableWithWeakObjects"

-- | @Selector@ for @weakObjectsHashTable@
weakObjectsHashTableSelector :: Selector '[] (Id NSHashTable)
weakObjectsHashTableSelector = mkSelector "weakObjectsHashTable"

-- | @Selector@ for @member:@
memberSelector :: Selector '[RawId] RawId
memberSelector = mkSelector "member:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector '[RawId] Bool
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @intersectsHashTable:@
intersectsHashTableSelector :: Selector '[Id NSHashTable] Bool
intersectsHashTableSelector = mkSelector "intersectsHashTable:"

-- | @Selector@ for @isEqualToHashTable:@
isEqualToHashTableSelector :: Selector '[Id NSHashTable] Bool
isEqualToHashTableSelector = mkSelector "isEqualToHashTable:"

-- | @Selector@ for @isSubsetOfHashTable:@
isSubsetOfHashTableSelector :: Selector '[Id NSHashTable] Bool
isSubsetOfHashTableSelector = mkSelector "isSubsetOfHashTable:"

-- | @Selector@ for @intersectHashTable:@
intersectHashTableSelector :: Selector '[Id NSHashTable] ()
intersectHashTableSelector = mkSelector "intersectHashTable:"

-- | @Selector@ for @unionHashTable:@
unionHashTableSelector :: Selector '[Id NSHashTable] ()
unionHashTableSelector = mkSelector "unionHashTable:"

-- | @Selector@ for @minusHashTable:@
minusHashTableSelector :: Selector '[Id NSHashTable] ()
minusHashTableSelector = mkSelector "minusHashTable:"

-- | @Selector@ for @pointerFunctions@
pointerFunctionsSelector :: Selector '[] (Id NSPointerFunctions)
pointerFunctionsSelector = mkSelector "pointerFunctions"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector '[] (Id NSArray)
allObjectsSelector = mkSelector "allObjects"

-- | @Selector@ for @anyObject@
anyObjectSelector :: Selector '[] RawId
anyObjectSelector = mkSelector "anyObject"

-- | @Selector@ for @setRepresentation@
setRepresentationSelector :: Selector '[] (Id NSSet)
setRepresentationSelector = mkSelector "setRepresentation"

