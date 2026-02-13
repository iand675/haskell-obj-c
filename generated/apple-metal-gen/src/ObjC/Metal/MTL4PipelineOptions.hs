{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides options controlling how to compile a pipeline state.
--
-- You provide these options through the ``MTL4PipelineDescriptor`` class at compilation time.
--
-- Generated bindings for @MTL4PipelineOptions@.
module ObjC.Metal.MTL4PipelineOptions
  ( MTL4PipelineOptions
  , IsMTL4PipelineOptions(..)
  , shaderValidation
  , setShaderValidation
  , shaderReflection
  , setShaderReflection
  , setShaderReflectionSelector
  , setShaderValidationSelector
  , shaderReflectionSelector
  , shaderValidationSelector

  -- * Enum types
  , MTL4ShaderReflection(MTL4ShaderReflection)
  , pattern MTL4ShaderReflectionNone
  , pattern MTL4ShaderReflectionBindingInfo
  , pattern MTL4ShaderReflectionBufferTypeInfo
  , MTLShaderValidation(MTLShaderValidation)
  , pattern MTLShaderValidationDefault
  , pattern MTLShaderValidationEnabled
  , pattern MTLShaderValidationDisabled

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

-- | Controls whether to enable or disable Metal Shader Validation for the pipeline.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> IO MTLShaderValidation
shaderValidation mtL4PipelineOptions =
  sendMessage mtL4PipelineOptions shaderValidationSelector

-- | Controls whether to enable or disable Metal Shader Validation for the pipeline.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> MTLShaderValidation -> IO ()
setShaderValidation mtL4PipelineOptions value =
  sendMessage mtL4PipelineOptions setShaderValidationSelector value

-- | Controls whether to include Metal shader reflection in this pipeline.
--
-- ObjC selector: @- shaderReflection@
shaderReflection :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> IO MTL4ShaderReflection
shaderReflection mtL4PipelineOptions =
  sendMessage mtL4PipelineOptions shaderReflectionSelector

-- | Controls whether to include Metal shader reflection in this pipeline.
--
-- ObjC selector: @- setShaderReflection:@
setShaderReflection :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> MTL4ShaderReflection -> IO ()
setShaderReflection mtL4PipelineOptions value =
  sendMessage mtL4PipelineOptions setShaderReflectionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector '[] MTLShaderValidation
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector '[MTLShaderValidation] ()
setShaderValidationSelector = mkSelector "setShaderValidation:"

-- | @Selector@ for @shaderReflection@
shaderReflectionSelector :: Selector '[] MTL4ShaderReflection
shaderReflectionSelector = mkSelector "shaderReflection"

-- | @Selector@ for @setShaderReflection:@
setShaderReflectionSelector :: Selector '[MTL4ShaderReflection] ()
setShaderReflectionSelector = mkSelector "setShaderReflection:"

