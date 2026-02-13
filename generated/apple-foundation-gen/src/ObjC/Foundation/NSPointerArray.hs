{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPointerArray@.
module ObjC.Foundation.NSPointerArray
  ( NSPointerArray
  , IsNSPointerArray(..)
  , initWithOptions
  , initWithPointerFunctions
  , pointerArrayWithOptions
  , pointerArrayWithPointerFunctions
  , pointerAtIndex
  , addPointer
  , removePointerAtIndex
  , insertPointer_atIndex
  , replacePointerAtIndex_withPointer
  , compact
  , pointerArrayWithStrongObjects
  , pointerArrayWithWeakObjects
  , strongObjectsPointerArray
  , weakObjectsPointerArray
  , pointerFunctions
  , count
  , setCount
  , allObjects
  , addPointerSelector
  , allObjectsSelector
  , compactSelector
  , countSelector
  , initWithOptionsSelector
  , initWithPointerFunctionsSelector
  , insertPointer_atIndexSelector
  , pointerArrayWithOptionsSelector
  , pointerArrayWithPointerFunctionsSelector
  , pointerArrayWithStrongObjectsSelector
  , pointerArrayWithWeakObjectsSelector
  , pointerAtIndexSelector
  , pointerFunctionsSelector
  , removePointerAtIndexSelector
  , replacePointerAtIndex_withPointerSelector
  , setCountSelector
  , strongObjectsPointerArraySelector
  , weakObjectsPointerArraySelector

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

-- | @- initWithOptions:@
initWithOptions :: IsNSPointerArray nsPointerArray => nsPointerArray -> NSPointerFunctionsOptions -> IO (Id NSPointerArray)
initWithOptions nsPointerArray options =
  sendOwnedMessage nsPointerArray initWithOptionsSelector options

-- | @- initWithPointerFunctions:@
initWithPointerFunctions :: (IsNSPointerArray nsPointerArray, IsNSPointerFunctions functions) => nsPointerArray -> functions -> IO (Id NSPointerArray)
initWithPointerFunctions nsPointerArray functions =
  sendOwnedMessage nsPointerArray initWithPointerFunctionsSelector (toNSPointerFunctions functions)

-- | @+ pointerArrayWithOptions:@
pointerArrayWithOptions :: NSPointerFunctionsOptions -> IO (Id NSPointerArray)
pointerArrayWithOptions options =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMessage cls' pointerArrayWithOptionsSelector options

-- | @+ pointerArrayWithPointerFunctions:@
pointerArrayWithPointerFunctions :: IsNSPointerFunctions functions => functions -> IO (Id NSPointerArray)
pointerArrayWithPointerFunctions functions =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMessage cls' pointerArrayWithPointerFunctionsSelector (toNSPointerFunctions functions)

-- | @- pointerAtIndex:@
pointerAtIndex :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> IO (Ptr ())
pointerAtIndex nsPointerArray index =
  sendMessage nsPointerArray pointerAtIndexSelector index

-- | @- addPointer:@
addPointer :: IsNSPointerArray nsPointerArray => nsPointerArray -> Ptr () -> IO ()
addPointer nsPointerArray pointer =
  sendMessage nsPointerArray addPointerSelector pointer

-- | @- removePointerAtIndex:@
removePointerAtIndex :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> IO ()
removePointerAtIndex nsPointerArray index =
  sendMessage nsPointerArray removePointerAtIndexSelector index

-- | @- insertPointer:atIndex:@
insertPointer_atIndex :: IsNSPointerArray nsPointerArray => nsPointerArray -> Ptr () -> CULong -> IO ()
insertPointer_atIndex nsPointerArray item index =
  sendMessage nsPointerArray insertPointer_atIndexSelector item index

-- | @- replacePointerAtIndex:withPointer:@
replacePointerAtIndex_withPointer :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> Ptr () -> IO ()
replacePointerAtIndex_withPointer nsPointerArray index item =
  sendMessage nsPointerArray replacePointerAtIndex_withPointerSelector index item

-- | @- compact@
compact :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO ()
compact nsPointerArray =
  sendMessage nsPointerArray compactSelector

-- | @+ pointerArrayWithStrongObjects@
pointerArrayWithStrongObjects :: IO RawId
pointerArrayWithStrongObjects  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMessage cls' pointerArrayWithStrongObjectsSelector

-- | @+ pointerArrayWithWeakObjects@
pointerArrayWithWeakObjects :: IO RawId
pointerArrayWithWeakObjects  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMessage cls' pointerArrayWithWeakObjectsSelector

-- | @+ strongObjectsPointerArray@
strongObjectsPointerArray :: IO (Id NSPointerArray)
strongObjectsPointerArray  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMessage cls' strongObjectsPointerArraySelector

-- | @+ weakObjectsPointerArray@
weakObjectsPointerArray :: IO (Id NSPointerArray)
weakObjectsPointerArray  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMessage cls' weakObjectsPointerArraySelector

-- | @- pointerFunctions@
pointerFunctions :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO (Id NSPointerFunctions)
pointerFunctions nsPointerArray =
  sendMessage nsPointerArray pointerFunctionsSelector

-- | @- count@
count :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO CULong
count nsPointerArray =
  sendMessage nsPointerArray countSelector

-- | @- setCount:@
setCount :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> IO ()
setCount nsPointerArray value =
  sendMessage nsPointerArray setCountSelector value

-- | @- allObjects@
allObjects :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO (Id NSArray)
allObjects nsPointerArray =
  sendMessage nsPointerArray allObjectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector '[NSPointerFunctionsOptions] (Id NSPointerArray)
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @initWithPointerFunctions:@
initWithPointerFunctionsSelector :: Selector '[Id NSPointerFunctions] (Id NSPointerArray)
initWithPointerFunctionsSelector = mkSelector "initWithPointerFunctions:"

-- | @Selector@ for @pointerArrayWithOptions:@
pointerArrayWithOptionsSelector :: Selector '[NSPointerFunctionsOptions] (Id NSPointerArray)
pointerArrayWithOptionsSelector = mkSelector "pointerArrayWithOptions:"

-- | @Selector@ for @pointerArrayWithPointerFunctions:@
pointerArrayWithPointerFunctionsSelector :: Selector '[Id NSPointerFunctions] (Id NSPointerArray)
pointerArrayWithPointerFunctionsSelector = mkSelector "pointerArrayWithPointerFunctions:"

-- | @Selector@ for @pointerAtIndex:@
pointerAtIndexSelector :: Selector '[CULong] (Ptr ())
pointerAtIndexSelector = mkSelector "pointerAtIndex:"

-- | @Selector@ for @addPointer:@
addPointerSelector :: Selector '[Ptr ()] ()
addPointerSelector = mkSelector "addPointer:"

-- | @Selector@ for @removePointerAtIndex:@
removePointerAtIndexSelector :: Selector '[CULong] ()
removePointerAtIndexSelector = mkSelector "removePointerAtIndex:"

-- | @Selector@ for @insertPointer:atIndex:@
insertPointer_atIndexSelector :: Selector '[Ptr (), CULong] ()
insertPointer_atIndexSelector = mkSelector "insertPointer:atIndex:"

-- | @Selector@ for @replacePointerAtIndex:withPointer:@
replacePointerAtIndex_withPointerSelector :: Selector '[CULong, Ptr ()] ()
replacePointerAtIndex_withPointerSelector = mkSelector "replacePointerAtIndex:withPointer:"

-- | @Selector@ for @compact@
compactSelector :: Selector '[] ()
compactSelector = mkSelector "compact"

-- | @Selector@ for @pointerArrayWithStrongObjects@
pointerArrayWithStrongObjectsSelector :: Selector '[] RawId
pointerArrayWithStrongObjectsSelector = mkSelector "pointerArrayWithStrongObjects"

-- | @Selector@ for @pointerArrayWithWeakObjects@
pointerArrayWithWeakObjectsSelector :: Selector '[] RawId
pointerArrayWithWeakObjectsSelector = mkSelector "pointerArrayWithWeakObjects"

-- | @Selector@ for @strongObjectsPointerArray@
strongObjectsPointerArraySelector :: Selector '[] (Id NSPointerArray)
strongObjectsPointerArraySelector = mkSelector "strongObjectsPointerArray"

-- | @Selector@ for @weakObjectsPointerArray@
weakObjectsPointerArraySelector :: Selector '[] (Id NSPointerArray)
weakObjectsPointerArraySelector = mkSelector "weakObjectsPointerArray"

-- | @Selector@ for @pointerFunctions@
pointerFunctionsSelector :: Selector '[] (Id NSPointerFunctions)
pointerFunctionsSelector = mkSelector "pointerFunctions"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector '[CULong] ()
setCountSelector = mkSelector "setCount:"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector '[] (Id NSArray)
allObjectsSelector = mkSelector "allObjects"

