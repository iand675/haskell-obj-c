{-# LANGUAGE PatternSynonyms #-}
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
  , initWithOptionsSelector
  , initWithPointerFunctionsSelector
  , pointerArrayWithOptionsSelector
  , pointerArrayWithPointerFunctionsSelector
  , pointerAtIndexSelector
  , addPointerSelector
  , removePointerAtIndexSelector
  , insertPointer_atIndexSelector
  , replacePointerAtIndex_withPointerSelector
  , compactSelector
  , pointerArrayWithStrongObjectsSelector
  , pointerArrayWithWeakObjectsSelector
  , strongObjectsPointerArraySelector
  , weakObjectsPointerArraySelector
  , pointerFunctionsSelector
  , countSelector
  , setCountSelector
  , allObjectsSelector

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

-- | @- initWithOptions:@
initWithOptions :: IsNSPointerArray nsPointerArray => nsPointerArray -> NSPointerFunctionsOptions -> IO (Id NSPointerArray)
initWithOptions nsPointerArray  options =
  sendMsg nsPointerArray (mkSelector "initWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- initWithPointerFunctions:@
initWithPointerFunctions :: (IsNSPointerArray nsPointerArray, IsNSPointerFunctions functions) => nsPointerArray -> functions -> IO (Id NSPointerArray)
initWithPointerFunctions nsPointerArray  functions =
withObjCPtr functions $ \raw_functions ->
    sendMsg nsPointerArray (mkSelector "initWithPointerFunctions:") (retPtr retVoid) [argPtr (castPtr raw_functions :: Ptr ())] >>= ownedObject . castPtr

-- | @+ pointerArrayWithOptions:@
pointerArrayWithOptions :: NSPointerFunctionsOptions -> IO (Id NSPointerArray)
pointerArrayWithOptions options =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMsg cls' (mkSelector "pointerArrayWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ pointerArrayWithPointerFunctions:@
pointerArrayWithPointerFunctions :: IsNSPointerFunctions functions => functions -> IO (Id NSPointerArray)
pointerArrayWithPointerFunctions functions =
  do
    cls' <- getRequiredClass "NSPointerArray"
    withObjCPtr functions $ \raw_functions ->
      sendClassMsg cls' (mkSelector "pointerArrayWithPointerFunctions:") (retPtr retVoid) [argPtr (castPtr raw_functions :: Ptr ())] >>= retainedObject . castPtr

-- | @- pointerAtIndex:@
pointerAtIndex :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> IO (Ptr ())
pointerAtIndex nsPointerArray  index =
  fmap castPtr $ sendMsg nsPointerArray (mkSelector "pointerAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- addPointer:@
addPointer :: IsNSPointerArray nsPointerArray => nsPointerArray -> Ptr () -> IO ()
addPointer nsPointerArray  pointer =
  sendMsg nsPointerArray (mkSelector "addPointer:") retVoid [argPtr pointer]

-- | @- removePointerAtIndex:@
removePointerAtIndex :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> IO ()
removePointerAtIndex nsPointerArray  index =
  sendMsg nsPointerArray (mkSelector "removePointerAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | @- insertPointer:atIndex:@
insertPointer_atIndex :: IsNSPointerArray nsPointerArray => nsPointerArray -> Ptr () -> CULong -> IO ()
insertPointer_atIndex nsPointerArray  item index =
  sendMsg nsPointerArray (mkSelector "insertPointer:atIndex:") retVoid [argPtr item, argCULong (fromIntegral index)]

-- | @- replacePointerAtIndex:withPointer:@
replacePointerAtIndex_withPointer :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> Ptr () -> IO ()
replacePointerAtIndex_withPointer nsPointerArray  index item =
  sendMsg nsPointerArray (mkSelector "replacePointerAtIndex:withPointer:") retVoid [argCULong (fromIntegral index), argPtr item]

-- | @- compact@
compact :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO ()
compact nsPointerArray  =
  sendMsg nsPointerArray (mkSelector "compact") retVoid []

-- | @+ pointerArrayWithStrongObjects@
pointerArrayWithStrongObjects :: IO RawId
pointerArrayWithStrongObjects  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "pointerArrayWithStrongObjects") (retPtr retVoid) []

-- | @+ pointerArrayWithWeakObjects@
pointerArrayWithWeakObjects :: IO RawId
pointerArrayWithWeakObjects  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "pointerArrayWithWeakObjects") (retPtr retVoid) []

-- | @+ strongObjectsPointerArray@
strongObjectsPointerArray :: IO (Id NSPointerArray)
strongObjectsPointerArray  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMsg cls' (mkSelector "strongObjectsPointerArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ weakObjectsPointerArray@
weakObjectsPointerArray :: IO (Id NSPointerArray)
weakObjectsPointerArray  =
  do
    cls' <- getRequiredClass "NSPointerArray"
    sendClassMsg cls' (mkSelector "weakObjectsPointerArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pointerFunctions@
pointerFunctions :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO (Id NSPointerFunctions)
pointerFunctions nsPointerArray  =
  sendMsg nsPointerArray (mkSelector "pointerFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- count@
count :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO CULong
count nsPointerArray  =
  sendMsg nsPointerArray (mkSelector "count") retCULong []

-- | @- setCount:@
setCount :: IsNSPointerArray nsPointerArray => nsPointerArray -> CULong -> IO ()
setCount nsPointerArray  value =
  sendMsg nsPointerArray (mkSelector "setCount:") retVoid [argCULong (fromIntegral value)]

-- | @- allObjects@
allObjects :: IsNSPointerArray nsPointerArray => nsPointerArray -> IO (Id NSArray)
allObjects nsPointerArray  =
  sendMsg nsPointerArray (mkSelector "allObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @initWithPointerFunctions:@
initWithPointerFunctionsSelector :: Selector
initWithPointerFunctionsSelector = mkSelector "initWithPointerFunctions:"

-- | @Selector@ for @pointerArrayWithOptions:@
pointerArrayWithOptionsSelector :: Selector
pointerArrayWithOptionsSelector = mkSelector "pointerArrayWithOptions:"

-- | @Selector@ for @pointerArrayWithPointerFunctions:@
pointerArrayWithPointerFunctionsSelector :: Selector
pointerArrayWithPointerFunctionsSelector = mkSelector "pointerArrayWithPointerFunctions:"

-- | @Selector@ for @pointerAtIndex:@
pointerAtIndexSelector :: Selector
pointerAtIndexSelector = mkSelector "pointerAtIndex:"

-- | @Selector@ for @addPointer:@
addPointerSelector :: Selector
addPointerSelector = mkSelector "addPointer:"

-- | @Selector@ for @removePointerAtIndex:@
removePointerAtIndexSelector :: Selector
removePointerAtIndexSelector = mkSelector "removePointerAtIndex:"

-- | @Selector@ for @insertPointer:atIndex:@
insertPointer_atIndexSelector :: Selector
insertPointer_atIndexSelector = mkSelector "insertPointer:atIndex:"

-- | @Selector@ for @replacePointerAtIndex:withPointer:@
replacePointerAtIndex_withPointerSelector :: Selector
replacePointerAtIndex_withPointerSelector = mkSelector "replacePointerAtIndex:withPointer:"

-- | @Selector@ for @compact@
compactSelector :: Selector
compactSelector = mkSelector "compact"

-- | @Selector@ for @pointerArrayWithStrongObjects@
pointerArrayWithStrongObjectsSelector :: Selector
pointerArrayWithStrongObjectsSelector = mkSelector "pointerArrayWithStrongObjects"

-- | @Selector@ for @pointerArrayWithWeakObjects@
pointerArrayWithWeakObjectsSelector :: Selector
pointerArrayWithWeakObjectsSelector = mkSelector "pointerArrayWithWeakObjects"

-- | @Selector@ for @strongObjectsPointerArray@
strongObjectsPointerArraySelector :: Selector
strongObjectsPointerArraySelector = mkSelector "strongObjectsPointerArray"

-- | @Selector@ for @weakObjectsPointerArray@
weakObjectsPointerArraySelector :: Selector
weakObjectsPointerArraySelector = mkSelector "weakObjectsPointerArray"

-- | @Selector@ for @pointerFunctions@
pointerFunctionsSelector :: Selector
pointerFunctionsSelector = mkSelector "pointerFunctions"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector
setCountSelector = mkSelector "setCount:"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector
allObjectsSelector = mkSelector "allObjects"

