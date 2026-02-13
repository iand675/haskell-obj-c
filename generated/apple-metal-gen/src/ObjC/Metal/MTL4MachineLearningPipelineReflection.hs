{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Describes every input and output of the pipeline.
--
-- ObjC selector: @- bindings@
bindings :: IsMTL4MachineLearningPipelineReflection mtL4MachineLearningPipelineReflection => mtL4MachineLearningPipelineReflection -> IO (Id NSArray)
bindings mtL4MachineLearningPipelineReflection =
  sendMessage mtL4MachineLearningPipelineReflection bindingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindings@
bindingsSelector :: Selector '[] (Id NSArray)
bindingsSelector = mkSelector "bindings"

