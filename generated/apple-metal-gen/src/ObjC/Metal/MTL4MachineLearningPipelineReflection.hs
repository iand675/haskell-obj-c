{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents reflection information for a machine learning pipeline state.
--
-- Generated bindings for @MTL4MachineLearningPipelineReflection@.
module ObjC.Metal.MTL4MachineLearningPipelineReflection
  ( MTL4MachineLearningPipelineReflection
  , IsMTL4MachineLearningPipelineReflection(..)
  , bindings
  , bindingsSelector


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

-- | Describes every input and output of the pipeline.
--
-- ObjC selector: @- bindings@
bindings :: IsMTL4MachineLearningPipelineReflection mtL4MachineLearningPipelineReflection => mtL4MachineLearningPipelineReflection -> IO (Id NSArray)
bindings mtL4MachineLearningPipelineReflection  =
    sendMsg mtL4MachineLearningPipelineReflection (mkSelector "bindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindings@
bindingsSelector :: Selector
bindingsSelector = mkSelector "bindings"

