{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLComputePipelineReflection@.
module ObjC.Metal.MTLComputePipelineReflection
  ( MTLComputePipelineReflection
  , IsMTLComputePipelineReflection(..)
  , bindings
  , arguments
  , argumentsSelector
  , bindingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- bindings@
bindings :: IsMTLComputePipelineReflection mtlComputePipelineReflection => mtlComputePipelineReflection -> IO (Id NSArray)
bindings mtlComputePipelineReflection =
  sendMessage mtlComputePipelineReflection bindingsSelector

-- | @- arguments@
arguments :: IsMTLComputePipelineReflection mtlComputePipelineReflection => mtlComputePipelineReflection -> IO (Id NSArray)
arguments mtlComputePipelineReflection =
  sendMessage mtlComputePipelineReflection argumentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindings@
bindingsSelector :: Selector '[] (Id NSArray)
bindingsSelector = mkSelector "bindings"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

