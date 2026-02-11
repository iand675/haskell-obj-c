{-# LANGUAGE PatternSynonyms #-}
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
  , initWithKeyOptions_valueOptions_capacitySelector
  , initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector
  , mapTableWithKeyOptions_valueOptionsSelector
  , mapTableWithStrongToStrongObjectsSelector
  , mapTableWithWeakToStrongObjectsSelector
  , mapTableWithStrongToWeakObjectsSelector
  , mapTableWithWeakToWeakObjectsSelector
  , strongToStrongObjectsMapTableSelector
  , weakToStrongObjectsMapTableSelector
  , strongToWeakObjectsMapTableSelector
  , weakToWeakObjectsMapTableSelector
  , objectForKeySelector
  , removeObjectForKeySelector
  , setObject_forKeySelector
  , keyEnumeratorSelector
  , objectEnumeratorSelector
  , removeAllObjectsSelector
  , dictionaryRepresentationSelector
  , keyPointerFunctionsSelector
  , valuePointerFunctionsSelector
  , countSelector

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

-- | @- initWithKeyOptions:valueOptions:capacity:@
initWithKeyOptions_valueOptions_capacity :: IsNSMapTable nsMapTable => nsMapTable -> NSPointerFunctionsOptions -> NSPointerFunctionsOptions -> CULong -> IO (Id NSMapTable)
initWithKeyOptions_valueOptions_capacity nsMapTable  keyOptions valueOptions initialCapacity =
  sendMsg nsMapTable (mkSelector "initWithKeyOptions:valueOptions:capacity:") (retPtr retVoid) [argCULong (coerce keyOptions), argCULong (coerce valueOptions), argCULong (fromIntegral initialCapacity)] >>= ownedObject . castPtr

-- | @- initWithKeyPointerFunctions:valuePointerFunctions:capacity:@
initWithKeyPointerFunctions_valuePointerFunctions_capacity :: (IsNSMapTable nsMapTable, IsNSPointerFunctions keyFunctions, IsNSPointerFunctions valueFunctions) => nsMapTable -> keyFunctions -> valueFunctions -> CULong -> IO (Id NSMapTable)
initWithKeyPointerFunctions_valuePointerFunctions_capacity nsMapTable  keyFunctions valueFunctions initialCapacity =
withObjCPtr keyFunctions $ \raw_keyFunctions ->
  withObjCPtr valueFunctions $ \raw_valueFunctions ->
      sendMsg nsMapTable (mkSelector "initWithKeyPointerFunctions:valuePointerFunctions:capacity:") (retPtr retVoid) [argPtr (castPtr raw_keyFunctions :: Ptr ()), argPtr (castPtr raw_valueFunctions :: Ptr ()), argCULong (fromIntegral initialCapacity)] >>= ownedObject . castPtr

-- | @+ mapTableWithKeyOptions:valueOptions:@
mapTableWithKeyOptions_valueOptions :: NSPointerFunctionsOptions -> NSPointerFunctionsOptions -> IO (Id NSMapTable)
mapTableWithKeyOptions_valueOptions keyOptions valueOptions =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMsg cls' (mkSelector "mapTableWithKeyOptions:valueOptions:") (retPtr retVoid) [argCULong (coerce keyOptions), argCULong (coerce valueOptions)] >>= retainedObject . castPtr

-- | @+ mapTableWithStrongToStrongObjects@
mapTableWithStrongToStrongObjects :: IO RawId
mapTableWithStrongToStrongObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "mapTableWithStrongToStrongObjects") (retPtr retVoid) []

-- | @+ mapTableWithWeakToStrongObjects@
mapTableWithWeakToStrongObjects :: IO RawId
mapTableWithWeakToStrongObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "mapTableWithWeakToStrongObjects") (retPtr retVoid) []

-- | @+ mapTableWithStrongToWeakObjects@
mapTableWithStrongToWeakObjects :: IO RawId
mapTableWithStrongToWeakObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "mapTableWithStrongToWeakObjects") (retPtr retVoid) []

-- | @+ mapTableWithWeakToWeakObjects@
mapTableWithWeakToWeakObjects :: IO RawId
mapTableWithWeakToWeakObjects  =
  do
    cls' <- getRequiredClass "NSMapTable"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "mapTableWithWeakToWeakObjects") (retPtr retVoid) []

-- | @+ strongToStrongObjectsMapTable@
strongToStrongObjectsMapTable :: IO (Id NSMapTable)
strongToStrongObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMsg cls' (mkSelector "strongToStrongObjectsMapTable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ weakToStrongObjectsMapTable@
weakToStrongObjectsMapTable :: IO (Id NSMapTable)
weakToStrongObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMsg cls' (mkSelector "weakToStrongObjectsMapTable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ strongToWeakObjectsMapTable@
strongToWeakObjectsMapTable :: IO (Id NSMapTable)
strongToWeakObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMsg cls' (mkSelector "strongToWeakObjectsMapTable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ weakToWeakObjectsMapTable@
weakToWeakObjectsMapTable :: IO (Id NSMapTable)
weakToWeakObjectsMapTable  =
  do
    cls' <- getRequiredClass "NSMapTable"
    sendClassMsg cls' (mkSelector "weakToWeakObjectsMapTable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectForKey:@
objectForKey :: IsNSMapTable nsMapTable => nsMapTable -> RawId -> IO RawId
objectForKey nsMapTable  aKey =
  fmap (RawId . castPtr) $ sendMsg nsMapTable (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr (unRawId aKey) :: Ptr ())]

-- | @- removeObjectForKey:@
removeObjectForKey :: IsNSMapTable nsMapTable => nsMapTable -> RawId -> IO ()
removeObjectForKey nsMapTable  aKey =
  sendMsg nsMapTable (mkSelector "removeObjectForKey:") retVoid [argPtr (castPtr (unRawId aKey) :: Ptr ())]

-- | @- setObject:forKey:@
setObject_forKey :: IsNSMapTable nsMapTable => nsMapTable -> RawId -> RawId -> IO ()
setObject_forKey nsMapTable  anObject aKey =
  sendMsg nsMapTable (mkSelector "setObject:forKey:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr (unRawId aKey) :: Ptr ())]

-- | @- keyEnumerator@
keyEnumerator :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSEnumerator)
keyEnumerator nsMapTable  =
  sendMsg nsMapTable (mkSelector "keyEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectEnumerator@
objectEnumerator :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSEnumerator)
objectEnumerator nsMapTable  =
  sendMsg nsMapTable (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- removeAllObjects@
removeAllObjects :: IsNSMapTable nsMapTable => nsMapTable -> IO ()
removeAllObjects nsMapTable  =
  sendMsg nsMapTable (mkSelector "removeAllObjects") retVoid []

-- | @- dictionaryRepresentation@
dictionaryRepresentation :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSDictionary)
dictionaryRepresentation nsMapTable  =
  sendMsg nsMapTable (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- keyPointerFunctions@
keyPointerFunctions :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSPointerFunctions)
keyPointerFunctions nsMapTable  =
  sendMsg nsMapTable (mkSelector "keyPointerFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valuePointerFunctions@
valuePointerFunctions :: IsNSMapTable nsMapTable => nsMapTable -> IO (Id NSPointerFunctions)
valuePointerFunctions nsMapTable  =
  sendMsg nsMapTable (mkSelector "valuePointerFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- count@
count :: IsNSMapTable nsMapTable => nsMapTable -> IO CULong
count nsMapTable  =
  sendMsg nsMapTable (mkSelector "count") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKeyOptions:valueOptions:capacity:@
initWithKeyOptions_valueOptions_capacitySelector :: Selector
initWithKeyOptions_valueOptions_capacitySelector = mkSelector "initWithKeyOptions:valueOptions:capacity:"

-- | @Selector@ for @initWithKeyPointerFunctions:valuePointerFunctions:capacity:@
initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector :: Selector
initWithKeyPointerFunctions_valuePointerFunctions_capacitySelector = mkSelector "initWithKeyPointerFunctions:valuePointerFunctions:capacity:"

-- | @Selector@ for @mapTableWithKeyOptions:valueOptions:@
mapTableWithKeyOptions_valueOptionsSelector :: Selector
mapTableWithKeyOptions_valueOptionsSelector = mkSelector "mapTableWithKeyOptions:valueOptions:"

-- | @Selector@ for @mapTableWithStrongToStrongObjects@
mapTableWithStrongToStrongObjectsSelector :: Selector
mapTableWithStrongToStrongObjectsSelector = mkSelector "mapTableWithStrongToStrongObjects"

-- | @Selector@ for @mapTableWithWeakToStrongObjects@
mapTableWithWeakToStrongObjectsSelector :: Selector
mapTableWithWeakToStrongObjectsSelector = mkSelector "mapTableWithWeakToStrongObjects"

-- | @Selector@ for @mapTableWithStrongToWeakObjects@
mapTableWithStrongToWeakObjectsSelector :: Selector
mapTableWithStrongToWeakObjectsSelector = mkSelector "mapTableWithStrongToWeakObjects"

-- | @Selector@ for @mapTableWithWeakToWeakObjects@
mapTableWithWeakToWeakObjectsSelector :: Selector
mapTableWithWeakToWeakObjectsSelector = mkSelector "mapTableWithWeakToWeakObjects"

-- | @Selector@ for @strongToStrongObjectsMapTable@
strongToStrongObjectsMapTableSelector :: Selector
strongToStrongObjectsMapTableSelector = mkSelector "strongToStrongObjectsMapTable"

-- | @Selector@ for @weakToStrongObjectsMapTable@
weakToStrongObjectsMapTableSelector :: Selector
weakToStrongObjectsMapTableSelector = mkSelector "weakToStrongObjectsMapTable"

-- | @Selector@ for @strongToWeakObjectsMapTable@
strongToWeakObjectsMapTableSelector :: Selector
strongToWeakObjectsMapTableSelector = mkSelector "strongToWeakObjectsMapTable"

-- | @Selector@ for @weakToWeakObjectsMapTable@
weakToWeakObjectsMapTableSelector :: Selector
weakToWeakObjectsMapTableSelector = mkSelector "weakToWeakObjectsMapTable"

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @keyEnumerator@
keyEnumeratorSelector :: Selector
keyEnumeratorSelector = mkSelector "keyEnumerator"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @keyPointerFunctions@
keyPointerFunctionsSelector :: Selector
keyPointerFunctionsSelector = mkSelector "keyPointerFunctions"

-- | @Selector@ for @valuePointerFunctions@
valuePointerFunctionsSelector :: Selector
valuePointerFunctionsSelector = mkSelector "valuePointerFunctions"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

