{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPointerFunctions@.
module ObjC.Foundation.NSPointerFunctions
  ( NSPointerFunctions
  , IsNSPointerFunctions(..)
  , initWithOptions
  , pointerFunctionsWithOptions
  , hashFunction
  , setHashFunction
  , isEqualFunction
  , setIsEqualFunction
  , sizeFunction
  , setSizeFunction
  , descriptionFunction
  , setDescriptionFunction
  , relinquishFunction
  , setRelinquishFunction
  , acquireFunction
  , setAcquireFunction
  , usesStrongWriteBarrier
  , setUsesStrongWriteBarrier
  , usesWeakReadAndWriteBarriers
  , setUsesWeakReadAndWriteBarriers
  , acquireFunctionSelector
  , descriptionFunctionSelector
  , hashFunctionSelector
  , initWithOptionsSelector
  , isEqualFunctionSelector
  , pointerFunctionsWithOptionsSelector
  , relinquishFunctionSelector
  , setAcquireFunctionSelector
  , setDescriptionFunctionSelector
  , setHashFunctionSelector
  , setIsEqualFunctionSelector
  , setRelinquishFunctionSelector
  , setSizeFunctionSelector
  , setUsesStrongWriteBarrierSelector
  , setUsesWeakReadAndWriteBarriersSelector
  , sizeFunctionSelector
  , usesStrongWriteBarrierSelector
  , usesWeakReadAndWriteBarriersSelector

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
initWithOptions :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> NSPointerFunctionsOptions -> IO (Id NSPointerFunctions)
initWithOptions nsPointerFunctions options =
  sendOwnedMessage nsPointerFunctions initWithOptionsSelector options

-- | @+ pointerFunctionsWithOptions:@
pointerFunctionsWithOptions :: NSPointerFunctionsOptions -> IO (Id NSPointerFunctions)
pointerFunctionsWithOptions options =
  do
    cls' <- getRequiredClass "NSPointerFunctions"
    sendClassMessage cls' pointerFunctionsWithOptionsSelector options

-- | @- hashFunction@
hashFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
hashFunction nsPointerFunctions =
  sendMessage nsPointerFunctions hashFunctionSelector

-- | @- setHashFunction:@
setHashFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setHashFunction nsPointerFunctions value =
  sendMessage nsPointerFunctions setHashFunctionSelector value

-- | @- isEqualFunction@
isEqualFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
isEqualFunction nsPointerFunctions =
  sendMessage nsPointerFunctions isEqualFunctionSelector

-- | @- setIsEqualFunction:@
setIsEqualFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setIsEqualFunction nsPointerFunctions value =
  sendMessage nsPointerFunctions setIsEqualFunctionSelector value

-- | @- sizeFunction@
sizeFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
sizeFunction nsPointerFunctions =
  sendMessage nsPointerFunctions sizeFunctionSelector

-- | @- setSizeFunction:@
setSizeFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setSizeFunction nsPointerFunctions value =
  sendMessage nsPointerFunctions setSizeFunctionSelector value

-- | @- descriptionFunction@
descriptionFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
descriptionFunction nsPointerFunctions =
  sendMessage nsPointerFunctions descriptionFunctionSelector

-- | @- setDescriptionFunction:@
setDescriptionFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setDescriptionFunction nsPointerFunctions value =
  sendMessage nsPointerFunctions setDescriptionFunctionSelector value

-- | @- relinquishFunction@
relinquishFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
relinquishFunction nsPointerFunctions =
  sendMessage nsPointerFunctions relinquishFunctionSelector

-- | @- setRelinquishFunction:@
setRelinquishFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setRelinquishFunction nsPointerFunctions value =
  sendMessage nsPointerFunctions setRelinquishFunctionSelector value

-- | @- acquireFunction@
acquireFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
acquireFunction nsPointerFunctions =
  sendMessage nsPointerFunctions acquireFunctionSelector

-- | @- setAcquireFunction:@
setAcquireFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setAcquireFunction nsPointerFunctions value =
  sendMessage nsPointerFunctions setAcquireFunctionSelector value

-- | @- usesStrongWriteBarrier@
usesStrongWriteBarrier :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO Bool
usesStrongWriteBarrier nsPointerFunctions =
  sendMessage nsPointerFunctions usesStrongWriteBarrierSelector

-- | @- setUsesStrongWriteBarrier:@
setUsesStrongWriteBarrier :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Bool -> IO ()
setUsesStrongWriteBarrier nsPointerFunctions value =
  sendMessage nsPointerFunctions setUsesStrongWriteBarrierSelector value

-- | @- usesWeakReadAndWriteBarriers@
usesWeakReadAndWriteBarriers :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO Bool
usesWeakReadAndWriteBarriers nsPointerFunctions =
  sendMessage nsPointerFunctions usesWeakReadAndWriteBarriersSelector

-- | @- setUsesWeakReadAndWriteBarriers:@
setUsesWeakReadAndWriteBarriers :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Bool -> IO ()
setUsesWeakReadAndWriteBarriers nsPointerFunctions value =
  sendMessage nsPointerFunctions setUsesWeakReadAndWriteBarriersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector '[NSPointerFunctionsOptions] (Id NSPointerFunctions)
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @pointerFunctionsWithOptions:@
pointerFunctionsWithOptionsSelector :: Selector '[NSPointerFunctionsOptions] (Id NSPointerFunctions)
pointerFunctionsWithOptionsSelector = mkSelector "pointerFunctionsWithOptions:"

-- | @Selector@ for @hashFunction@
hashFunctionSelector :: Selector '[] (Ptr ())
hashFunctionSelector = mkSelector "hashFunction"

-- | @Selector@ for @setHashFunction:@
setHashFunctionSelector :: Selector '[Ptr ()] ()
setHashFunctionSelector = mkSelector "setHashFunction:"

-- | @Selector@ for @isEqualFunction@
isEqualFunctionSelector :: Selector '[] (Ptr ())
isEqualFunctionSelector = mkSelector "isEqualFunction"

-- | @Selector@ for @setIsEqualFunction:@
setIsEqualFunctionSelector :: Selector '[Ptr ()] ()
setIsEqualFunctionSelector = mkSelector "setIsEqualFunction:"

-- | @Selector@ for @sizeFunction@
sizeFunctionSelector :: Selector '[] (Ptr ())
sizeFunctionSelector = mkSelector "sizeFunction"

-- | @Selector@ for @setSizeFunction:@
setSizeFunctionSelector :: Selector '[Ptr ()] ()
setSizeFunctionSelector = mkSelector "setSizeFunction:"

-- | @Selector@ for @descriptionFunction@
descriptionFunctionSelector :: Selector '[] (Ptr ())
descriptionFunctionSelector = mkSelector "descriptionFunction"

-- | @Selector@ for @setDescriptionFunction:@
setDescriptionFunctionSelector :: Selector '[Ptr ()] ()
setDescriptionFunctionSelector = mkSelector "setDescriptionFunction:"

-- | @Selector@ for @relinquishFunction@
relinquishFunctionSelector :: Selector '[] (Ptr ())
relinquishFunctionSelector = mkSelector "relinquishFunction"

-- | @Selector@ for @setRelinquishFunction:@
setRelinquishFunctionSelector :: Selector '[Ptr ()] ()
setRelinquishFunctionSelector = mkSelector "setRelinquishFunction:"

-- | @Selector@ for @acquireFunction@
acquireFunctionSelector :: Selector '[] (Ptr ())
acquireFunctionSelector = mkSelector "acquireFunction"

-- | @Selector@ for @setAcquireFunction:@
setAcquireFunctionSelector :: Selector '[Ptr ()] ()
setAcquireFunctionSelector = mkSelector "setAcquireFunction:"

-- | @Selector@ for @usesStrongWriteBarrier@
usesStrongWriteBarrierSelector :: Selector '[] Bool
usesStrongWriteBarrierSelector = mkSelector "usesStrongWriteBarrier"

-- | @Selector@ for @setUsesStrongWriteBarrier:@
setUsesStrongWriteBarrierSelector :: Selector '[Bool] ()
setUsesStrongWriteBarrierSelector = mkSelector "setUsesStrongWriteBarrier:"

-- | @Selector@ for @usesWeakReadAndWriteBarriers@
usesWeakReadAndWriteBarriersSelector :: Selector '[] Bool
usesWeakReadAndWriteBarriersSelector = mkSelector "usesWeakReadAndWriteBarriers"

-- | @Selector@ for @setUsesWeakReadAndWriteBarriers:@
setUsesWeakReadAndWriteBarriersSelector :: Selector '[Bool] ()
setUsesWeakReadAndWriteBarriersSelector = mkSelector "setUsesWeakReadAndWriteBarriers:"

