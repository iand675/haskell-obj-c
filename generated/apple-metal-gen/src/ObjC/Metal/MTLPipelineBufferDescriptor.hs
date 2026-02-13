{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLPipelineBufferDescriptor@.
module ObjC.Metal.MTLPipelineBufferDescriptor
  ( MTLPipelineBufferDescriptor
  , IsMTLPipelineBufferDescriptor(..)
  , mutability
  , setMutability
  , mutabilitySelector
  , setMutabilitySelector

  -- * Enum types
  , MTLMutability(MTLMutability)
  , pattern MTLMutabilityDefault
  , pattern MTLMutabilityMutable
  , pattern MTLMutabilityImmutable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Buffer mutability. Defaults to MTLMutabilityDefault: mutable for standard buffers, immutable for argument buffers
--
-- ObjC selector: @- mutability@
mutability :: IsMTLPipelineBufferDescriptor mtlPipelineBufferDescriptor => mtlPipelineBufferDescriptor -> IO MTLMutability
mutability mtlPipelineBufferDescriptor =
  sendMessage mtlPipelineBufferDescriptor mutabilitySelector

-- | Buffer mutability. Defaults to MTLMutabilityDefault: mutable for standard buffers, immutable for argument buffers
--
-- ObjC selector: @- setMutability:@
setMutability :: IsMTLPipelineBufferDescriptor mtlPipelineBufferDescriptor => mtlPipelineBufferDescriptor -> MTLMutability -> IO ()
setMutability mtlPipelineBufferDescriptor value =
  sendMessage mtlPipelineBufferDescriptor setMutabilitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mutability@
mutabilitySelector :: Selector '[] MTLMutability
mutabilitySelector = mkSelector "mutability"

-- | @Selector@ for @setMutability:@
setMutabilitySelector :: Selector '[MTLMutability] ()
setMutabilitySelector = mkSelector "setMutability:"

