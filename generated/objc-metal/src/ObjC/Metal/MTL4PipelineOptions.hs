{-# LANGUAGE PatternSynonyms #-}
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
  , shaderValidationSelector
  , setShaderValidationSelector
  , shaderReflectionSelector
  , setShaderReflectionSelector

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
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Controls whether to enable or disable Metal Shader Validation for the pipeline.
--
-- ObjC selector: @- shaderValidation@
shaderValidation :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> IO MTLShaderValidation
shaderValidation mtL4PipelineOptions  =
  fmap (coerce :: CLong -> MTLShaderValidation) $ sendMsg mtL4PipelineOptions (mkSelector "shaderValidation") retCLong []

-- | Controls whether to enable or disable Metal Shader Validation for the pipeline.
--
-- ObjC selector: @- setShaderValidation:@
setShaderValidation :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> MTLShaderValidation -> IO ()
setShaderValidation mtL4PipelineOptions  value =
  sendMsg mtL4PipelineOptions (mkSelector "setShaderValidation:") retVoid [argCLong (coerce value)]

-- | Controls whether to include Metal shader reflection in this pipeline.
--
-- ObjC selector: @- shaderReflection@
shaderReflection :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> IO MTL4ShaderReflection
shaderReflection mtL4PipelineOptions  =
  fmap (coerce :: CULong -> MTL4ShaderReflection) $ sendMsg mtL4PipelineOptions (mkSelector "shaderReflection") retCULong []

-- | Controls whether to include Metal shader reflection in this pipeline.
--
-- ObjC selector: @- setShaderReflection:@
setShaderReflection :: IsMTL4PipelineOptions mtL4PipelineOptions => mtL4PipelineOptions -> MTL4ShaderReflection -> IO ()
setShaderReflection mtL4PipelineOptions  value =
  sendMsg mtL4PipelineOptions (mkSelector "setShaderReflection:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shaderValidation@
shaderValidationSelector :: Selector
shaderValidationSelector = mkSelector "shaderValidation"

-- | @Selector@ for @setShaderValidation:@
setShaderValidationSelector :: Selector
setShaderValidationSelector = mkSelector "setShaderValidation:"

-- | @Selector@ for @shaderReflection@
shaderReflectionSelector :: Selector
shaderReflectionSelector = mkSelector "shaderReflection"

-- | @Selector@ for @setShaderReflection:@
setShaderReflectionSelector :: Selector
setShaderReflectionSelector = mkSelector "setShaderReflection:"

