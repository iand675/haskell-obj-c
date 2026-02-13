{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties to create a pipeline data set serializer.
--
-- Generated bindings for @MTL4PipelineDataSetSerializerDescriptor@.
module ObjC.Metal.MTL4PipelineDataSetSerializerDescriptor
  ( MTL4PipelineDataSetSerializerDescriptor
  , IsMTL4PipelineDataSetSerializerDescriptor(..)
  , configuration
  , setConfiguration
  , configurationSelector
  , setConfigurationSelector

  -- * Enum types
  , MTL4PipelineDataSetSerializerConfiguration(MTL4PipelineDataSetSerializerConfiguration)
  , pattern MTL4PipelineDataSetSerializerConfigurationCaptureDescriptors
  , pattern MTL4PipelineDataSetSerializerConfigurationCaptureBinaries

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

-- | Specifies the configuration of the serialization process.
--
-- The configuration of the serialization process determines the mechanisms you use to serialize pipeline data sets.
--
-- When this configuration contains ``MTL4PipelineDataSetSerializerConfigurationCaptureDescriptors``, use ``serializeAsPipelinesScriptWithError:`` to serialize pipeline scripts.
--
-- If this option contains ``MTL4PipelineDataSetSerializerConfigurationCaptureBinaries``, the serializer can additionally serialize to a binary archive by calling ``serializeAsArchiveAndFlushToURL:error::``.
--
-- ObjC selector: @- configuration@
configuration :: IsMTL4PipelineDataSetSerializerDescriptor mtL4PipelineDataSetSerializerDescriptor => mtL4PipelineDataSetSerializerDescriptor -> IO MTL4PipelineDataSetSerializerConfiguration
configuration mtL4PipelineDataSetSerializerDescriptor =
  sendMessage mtL4PipelineDataSetSerializerDescriptor configurationSelector

-- | Specifies the configuration of the serialization process.
--
-- The configuration of the serialization process determines the mechanisms you use to serialize pipeline data sets.
--
-- When this configuration contains ``MTL4PipelineDataSetSerializerConfigurationCaptureDescriptors``, use ``serializeAsPipelinesScriptWithError:`` to serialize pipeline scripts.
--
-- If this option contains ``MTL4PipelineDataSetSerializerConfigurationCaptureBinaries``, the serializer can additionally serialize to a binary archive by calling ``serializeAsArchiveAndFlushToURL:error::``.
--
-- ObjC selector: @- setConfiguration:@
setConfiguration :: IsMTL4PipelineDataSetSerializerDescriptor mtL4PipelineDataSetSerializerDescriptor => mtL4PipelineDataSetSerializerDescriptor -> MTL4PipelineDataSetSerializerConfiguration -> IO ()
setConfiguration mtL4PipelineDataSetSerializerDescriptor value =
  sendMessage mtL4PipelineDataSetSerializerDescriptor setConfigurationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] MTL4PipelineDataSetSerializerConfiguration
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector '[MTL4PipelineDataSetSerializerConfiguration] ()
setConfigurationSelector = mkSelector "setConfiguration:"

