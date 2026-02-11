{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLComputePipelineReflection@.
module ObjC.Metal.MTLComputePipelineReflection
  ( MTLComputePipelineReflection
  , IsMTLComputePipelineReflection(..)
  , bindings
  , arguments
  , bindingsSelector
  , argumentsSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- bindings@
bindings :: IsMTLComputePipelineReflection mtlComputePipelineReflection => mtlComputePipelineReflection -> IO (Id NSArray)
bindings mtlComputePipelineReflection  =
    sendMsg mtlComputePipelineReflection (mkSelector "bindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arguments@
arguments :: IsMTLComputePipelineReflection mtlComputePipelineReflection => mtlComputePipelineReflection -> IO (Id NSArray)
arguments mtlComputePipelineReflection  =
    sendMsg mtlComputePipelineReflection (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindings@
bindingsSelector :: Selector
bindingsSelector = mkSelector "bindings"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

