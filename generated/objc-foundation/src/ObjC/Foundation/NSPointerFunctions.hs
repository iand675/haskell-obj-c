{-# LANGUAGE PatternSynonyms #-}
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
  , initWithOptionsSelector
  , pointerFunctionsWithOptionsSelector
  , hashFunctionSelector
  , setHashFunctionSelector
  , isEqualFunctionSelector
  , setIsEqualFunctionSelector
  , sizeFunctionSelector
  , setSizeFunctionSelector
  , descriptionFunctionSelector
  , setDescriptionFunctionSelector
  , relinquishFunctionSelector
  , setRelinquishFunctionSelector
  , acquireFunctionSelector
  , setAcquireFunctionSelector
  , usesStrongWriteBarrierSelector
  , setUsesStrongWriteBarrierSelector
  , usesWeakReadAndWriteBarriersSelector
  , setUsesWeakReadAndWriteBarriersSelector

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
initWithOptions :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> NSPointerFunctionsOptions -> IO (Id NSPointerFunctions)
initWithOptions nsPointerFunctions  options =
  sendMsg nsPointerFunctions (mkSelector "initWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= ownedObject . castPtr

-- | @+ pointerFunctionsWithOptions:@
pointerFunctionsWithOptions :: NSPointerFunctionsOptions -> IO (Id NSPointerFunctions)
pointerFunctionsWithOptions options =
  do
    cls' <- getRequiredClass "NSPointerFunctions"
    sendClassMsg cls' (mkSelector "pointerFunctionsWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- hashFunction@
hashFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
hashFunction nsPointerFunctions  =
  fmap castPtr $ sendMsg nsPointerFunctions (mkSelector "hashFunction") (retPtr retVoid) []

-- | @- setHashFunction:@
setHashFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setHashFunction nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setHashFunction:") retVoid [argPtr value]

-- | @- isEqualFunction@
isEqualFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
isEqualFunction nsPointerFunctions  =
  fmap castPtr $ sendMsg nsPointerFunctions (mkSelector "isEqualFunction") (retPtr retVoid) []

-- | @- setIsEqualFunction:@
setIsEqualFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setIsEqualFunction nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setIsEqualFunction:") retVoid [argPtr value]

-- | @- sizeFunction@
sizeFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
sizeFunction nsPointerFunctions  =
  fmap castPtr $ sendMsg nsPointerFunctions (mkSelector "sizeFunction") (retPtr retVoid) []

-- | @- setSizeFunction:@
setSizeFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setSizeFunction nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setSizeFunction:") retVoid [argPtr value]

-- | @- descriptionFunction@
descriptionFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
descriptionFunction nsPointerFunctions  =
  fmap castPtr $ sendMsg nsPointerFunctions (mkSelector "descriptionFunction") (retPtr retVoid) []

-- | @- setDescriptionFunction:@
setDescriptionFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setDescriptionFunction nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setDescriptionFunction:") retVoid [argPtr value]

-- | @- relinquishFunction@
relinquishFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
relinquishFunction nsPointerFunctions  =
  fmap castPtr $ sendMsg nsPointerFunctions (mkSelector "relinquishFunction") (retPtr retVoid) []

-- | @- setRelinquishFunction:@
setRelinquishFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setRelinquishFunction nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setRelinquishFunction:") retVoid [argPtr value]

-- | @- acquireFunction@
acquireFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO (Ptr ())
acquireFunction nsPointerFunctions  =
  fmap castPtr $ sendMsg nsPointerFunctions (mkSelector "acquireFunction") (retPtr retVoid) []

-- | @- setAcquireFunction:@
setAcquireFunction :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Ptr () -> IO ()
setAcquireFunction nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setAcquireFunction:") retVoid [argPtr value]

-- | @- usesStrongWriteBarrier@
usesStrongWriteBarrier :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO Bool
usesStrongWriteBarrier nsPointerFunctions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPointerFunctions (mkSelector "usesStrongWriteBarrier") retCULong []

-- | @- setUsesStrongWriteBarrier:@
setUsesStrongWriteBarrier :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Bool -> IO ()
setUsesStrongWriteBarrier nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setUsesStrongWriteBarrier:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesWeakReadAndWriteBarriers@
usesWeakReadAndWriteBarriers :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> IO Bool
usesWeakReadAndWriteBarriers nsPointerFunctions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPointerFunctions (mkSelector "usesWeakReadAndWriteBarriers") retCULong []

-- | @- setUsesWeakReadAndWriteBarriers:@
setUsesWeakReadAndWriteBarriers :: IsNSPointerFunctions nsPointerFunctions => nsPointerFunctions -> Bool -> IO ()
setUsesWeakReadAndWriteBarriers nsPointerFunctions  value =
  sendMsg nsPointerFunctions (mkSelector "setUsesWeakReadAndWriteBarriers:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @pointerFunctionsWithOptions:@
pointerFunctionsWithOptionsSelector :: Selector
pointerFunctionsWithOptionsSelector = mkSelector "pointerFunctionsWithOptions:"

-- | @Selector@ for @hashFunction@
hashFunctionSelector :: Selector
hashFunctionSelector = mkSelector "hashFunction"

-- | @Selector@ for @setHashFunction:@
setHashFunctionSelector :: Selector
setHashFunctionSelector = mkSelector "setHashFunction:"

-- | @Selector@ for @isEqualFunction@
isEqualFunctionSelector :: Selector
isEqualFunctionSelector = mkSelector "isEqualFunction"

-- | @Selector@ for @setIsEqualFunction:@
setIsEqualFunctionSelector :: Selector
setIsEqualFunctionSelector = mkSelector "setIsEqualFunction:"

-- | @Selector@ for @sizeFunction@
sizeFunctionSelector :: Selector
sizeFunctionSelector = mkSelector "sizeFunction"

-- | @Selector@ for @setSizeFunction:@
setSizeFunctionSelector :: Selector
setSizeFunctionSelector = mkSelector "setSizeFunction:"

-- | @Selector@ for @descriptionFunction@
descriptionFunctionSelector :: Selector
descriptionFunctionSelector = mkSelector "descriptionFunction"

-- | @Selector@ for @setDescriptionFunction:@
setDescriptionFunctionSelector :: Selector
setDescriptionFunctionSelector = mkSelector "setDescriptionFunction:"

-- | @Selector@ for @relinquishFunction@
relinquishFunctionSelector :: Selector
relinquishFunctionSelector = mkSelector "relinquishFunction"

-- | @Selector@ for @setRelinquishFunction:@
setRelinquishFunctionSelector :: Selector
setRelinquishFunctionSelector = mkSelector "setRelinquishFunction:"

-- | @Selector@ for @acquireFunction@
acquireFunctionSelector :: Selector
acquireFunctionSelector = mkSelector "acquireFunction"

-- | @Selector@ for @setAcquireFunction:@
setAcquireFunctionSelector :: Selector
setAcquireFunctionSelector = mkSelector "setAcquireFunction:"

-- | @Selector@ for @usesStrongWriteBarrier@
usesStrongWriteBarrierSelector :: Selector
usesStrongWriteBarrierSelector = mkSelector "usesStrongWriteBarrier"

-- | @Selector@ for @setUsesStrongWriteBarrier:@
setUsesStrongWriteBarrierSelector :: Selector
setUsesStrongWriteBarrierSelector = mkSelector "setUsesStrongWriteBarrier:"

-- | @Selector@ for @usesWeakReadAndWriteBarriers@
usesWeakReadAndWriteBarriersSelector :: Selector
usesWeakReadAndWriteBarriersSelector = mkSelector "usesWeakReadAndWriteBarriers"

-- | @Selector@ for @setUsesWeakReadAndWriteBarriers:@
setUsesWeakReadAndWriteBarriersSelector :: Selector
setUsesWeakReadAndWriteBarriersSelector = mkSelector "setUsesWeakReadAndWriteBarriers:"

