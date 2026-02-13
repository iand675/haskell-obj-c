{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMapTable@.
module ObjC.Foundation.NSMapTable
  ( NSMapTable
  , IsNSMapTable(..)
  , initWithKeyOptions_valueOptions_capacity
  , initWithKeyPointerFunctions_valuePointerFunctions_capacity
  , mapTableWithKeyOptions_valueOptions
  , mapTableWithStrongToStrongObjects
  , mapTableWithWeakToStrongObjects
  , mapTableWithStrongToWeakObjects
  , mapTableWithWeakToWeakObjects
  , strongToStrongObjectsMapTable
  , weakToStrongObjectsMapTable
  , strongToWeakObjectsMapTable
  , weakToWeakObjectsMapTable
  , objectForKey
  , removeObjectForKey
  , setObject_forKey
  , keyEnumerator
  , objectEnumerator
  , removeAllObjects
  , dictionaryRepresentation
  , keyPointerFunctions
  , valuePointerFunctions
  , count
  , countSelector
  , dictionaryRepresentationSelector
  , initWithKeyOptions_valueOptions_capacitySelector
  , initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector
  , keyEnumeratorSelector
  , keyPointerFunctionsSelector
  , mapTableWithKeyOptions_valueOptionsSelector
  , mapTableWithStrongToStrongObjectsSelector
  , mapTableWithStrongToWeakObjectsSelector
  , mapTableWithWeakToStrongObjectsSelector
  , mapTableWithWeakToWeakObjectsSelector
  , objectEnumeratorSelector
  , objectForKeySelector
  , removeAllObjectsSelector
  , removeObjectForKeySelector
  , setObject_forKeySelector
  , strongToStrongObjectsMapTableSelector
  , strongToWeakObjectsMapTableSelector
  , valuePointerFunctionsSelector
  , weakToStrongObjectsMapTableSelector
  , weakToWeakObjectsMapTableSelector

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

-- | @- initWithKeyOptions:valueOptions:capacity:@
initWithKeyOptions_valueOptions_capacity :: IsNSMapTable nsMapTable => nsMapTable -> NSPointerFunctionsOptions -> NSPointerFunctionsOptions -> CULong -> IO (Id NSMapTable)
initWithKeyOptions_valueOptions_capacity nsMapTable keyOptions valueOptions initialCapacity =
  sendOwnedMessage nsMapTable initWithKeyOptions_valueOptions_capacitySelector keyOptions valueOptions initialCapacity

-- | @- initWithKeyPointerFunctions:valuePointerFunctions:capacity:@
initWithKeyPointerFunctions_valuePointerFunctions_capacity :: (IsNSMapTable nsMapTable, IsNSPointerFunctions keyFunctions, IsNSPointerFunctions valueFunctions) => nsMapTable -> keyFunctions -> valueFunctions -> CULong -> IO (Id NSMapTable)
initWithKeyPointerFunctions_valuePointerFunctions_capacity nsMapTable keyFunctions valueFunctions initialCapacity =
  sendOwnedMessage nsMapTable initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector (toNSPointerFunctions keyFunctions) (toNSPointerFunctions valueFunctions) initialCapacity

-- | @+ mapTableWithKeyOptions:valueOptions:@
mapTableWithKeyOptions_valueOptions :: NSPointerFunctionsOptions -> NSPointerFunctionsOptions -> IO (Id NSMapTable)
mapTableWithKeyOptions_valueOptions keyOptions valueOptions =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' mapTableWithKeyOptions_valueOptionsSelector keyOptions valueOptions

-- | @+ mapTableWithStrongToStrongObjects@
mapTableWithStrongToStrongObjects :: IO RawId
mapTableWithStrongToStrongObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' mapTableWithStrongToStrongObjectsSelector

-- | @+ mapTableWithWeakToStrongObjects@
mapTableWithWeakToStrongObjects :: IO RawId
mapTableWithWeakToStrongObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' mapTableWithWeakToStrongObjectsSelector

-- | @+ mapTableWithStrongToWeakObjects@
mapTableWithStrongToWeakObjects :: IO RawId
mapTableWithStrongToWeakObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' mapTableWithStrongToWeakObjectsSelector

-- | @+ mapTableWithWeakToWeakObjects@
mapTableWithWeakToWeakObjects :: IO RawId
mapTableWithWeakToWeakObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' mapTableWithWeakToWeakObjectsSelector

-- | @+ strongToStrongObjectsMapTable@
strongToStrongObjectsMapTable :: IO (Id NSMapTable)
strongToStrongObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' strongToStrongObjectsMapTableSelector

-- | @+ weakToStrongObjectsMapTable@
weakToStrongObjectsMapTable :: IO (Id NSMapTable)
weakToStrongObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' weakToStrongObjectsMapTableSelector

-- | @+ strongToWeakObjectsMapTable@
strongToWeakObjectsMapTable :: IO (Id NSMapTable)
strongToWeakObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' strongToWeakObjectsMapTableSelector

-- | @+ weakToWeakObjectsMapTable@
weakToWeakObjectsMapTable :: IO (Id NSMapTable)
weakToWeakObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMessage cls' weakToWeakObjectsMapTableSelector

-- | @- objectForKey:@
objectForKey :: IsNSMapTable nsMapTable => nsMapTable -> RawId -> IO RawId
objectForKey nsMapTable aKey =
  sendMessage nsMapTable objectForKeySelector aKey

-- | @- removeObjectForKey:@
removeObjectForKey :: IsNSMapTable nsMapTable => nsMapTable -> RawId -> IO ()
removeObjectForKey nsMapTable aKey =
  sendMessage nsMapTable removeObjectForKeySelector aKey

-- | @- setObject:forKey:@
setObject_forKey :: IsNSMapTable nsMapTable => nsMapTable -> RawId -> RawId -> IO ()
setObject_forKey nsMapTable anObject aKey =
  sendMessage nsMapTable setObject_forKeySelector anObject aKey

-- | @- keyEnumerator@
keyEnumerator :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSEnumerator)
keyEnumerator nsMapTable =
  sendMessage nsMapTable keyEnumeratorSelector

-- | @- objectEnumerator@
objectEnumerator :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSEnumerator)
objectEnumerator nsMapTable =
  sendMessage nsMapTable objectEnumeratorSelector

-- | @- removeAllObjects@
removeAllObjects :: IsNSMapTable nsMapTable => nsMapTable -> IO ()
removeAllObjects nsMapTable =
  sendMessage nsMapTable removeAllObjectsSelector

-- | @- dictionaryRepresentation@
dictionaryRepresentation :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSDictionary)
dictionaryRepresentation nsMapTable =
  sendMessage nsMapTable dictionaryRepresentationSelector

-- | @- keyPointerFunctions@
keyPointerFunctions :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSPointerFunctions)
keyPointerFunctions nsMapTable =
  sendMessage nsMapTable keyPointerFunctionsSelector

-- | @- valuePointerFunctions@
valuePointerFunctions :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSPointerFunctions)
valuePointerFunctions nsMapTable =
  sendMessage nsMapTable valuePointerFunctionsSelector

-- | @- count@
count :: IsNSMapTable nsMapTable => nsMapTable -> IO CULong
count nsMapTable =
  sendMessage nsMapTable countSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKeyOptions:valueOptions:capacity:@
initWithKeyOptions_valueOptions_capacitySelector :: Selector '[NSPointerFunctionsOptions, NSPointerFunctionsOptions, CULong] (Id NSMapTable)
initWithKeyOptions_valueOptions_capacitySelector = mkSelector "initWithKeyOptions:valueOptions:capacity:"

-- | @Selector@ for @initWithKeyPointerFunctions:valuePointerFunctions:capacity:@
initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector :: Selector '[Id NSPointerFunctions, Id NSPointerFunctions, CULong] (Id NSMapTable)
initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector = mkSelector "initWithKeyPointerFunctions:valuePointerFunctions:capacity:"

-- | @Selector@ for @mapTableWithKeyOptions:valueOptions:@
mapTableWithKeyOptions_valueOptionsSelector :: Selector '[NSPointerFunctionsOptions, NSPointerFunctionsOptions] (Id NSMapTable)
mapTableWithKeyOptions_valueOptionsSelector = mkSelector "mapTableWithKeyOptions:valueOptions:"

-- | @Selector@ for @mapTableWithStrongToStrongObjects@
mapTableWithStrongToStrongObjectsSelector :: Selector '[] RawId
mapTableWithStrongToStrongObjectsSelector = mkSelector "mapTableWithStrongToStrongObjects"

-- | @Selector@ for @mapTableWithWeakToStrongObjects@
mapTableWithWeakToStrongObjectsSelector :: Selector '[] RawId
mapTableWithWeakToStrongObjectsSelector = mkSelector "mapTableWithWeakToStrongObjects"

-- | @Selector@ for @mapTableWithStrongToWeakObjects@
mapTableWithStrongToWeakObjectsSelector :: Selector '[] RawId
mapTableWithStrongToWeakObjectsSelector = mkSelector "mapTableWithStrongToWeakObjects"

-- | @Selector@ for @mapTableWithWeakToWeakObjects@
mapTableWithWeakToWeakObjectsSelector :: Selector '[] RawId
mapTableWithWeakToWeakObjectsSelector = mkSelector "mapTableWithWeakToWeakObjects"

-- | @Selector@ for @strongToStrongObjectsMapTable@
strongToStrongObjectsMapTableSelector :: Selector '[] (Id NSMapTable)
strongToStrongObjectsMapTableSelector = mkSelector "strongToStrongObjectsMapTable"

-- | @Selector@ for @weakToStrongObjectsMapTable@
weakToStrongObjectsMapTableSelector :: Selector '[] (Id NSMapTable)
weakToStrongObjectsMapTableSelector = mkSelector "weakToStrongObjectsMapTable"

-- | @Selector@ for @strongToWeakObjectsMapTable@
strongToWeakObjectsMapTableSelector :: Selector '[] (Id NSMapTable)
strongToWeakObjectsMapTableSelector = mkSelector "strongToWeakObjectsMapTable"

-- | @Selector@ for @weakToWeakObjectsMapTable@
weakToWeakObjectsMapTableSelector :: Selector '[] (Id NSMapTable)
weakToWeakObjectsMapTableSelector = mkSelector "weakToWeakObjectsMapTable"

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[RawId] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector '[RawId] ()
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector '[RawId, RawId] ()
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @keyEnumerator@
keyEnumeratorSelector :: Selector '[] (Id NSEnumerator)
keyEnumeratorSelector = mkSelector "keyEnumerator"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector '[] ()
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @keyPointerFunctions@
keyPointerFunctionsSelector :: Selector '[] (Id NSPointerFunctions)
keyPointerFunctionsSelector = mkSelector "keyPointerFunctions"

-- | @Selector@ for @valuePointerFunctions@
valuePointerFunctionsSelector :: Selector '[] (Id NSPointerFunctions)
valuePointerFunctionsSelector = mkSelector "valuePointerFunctions"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

