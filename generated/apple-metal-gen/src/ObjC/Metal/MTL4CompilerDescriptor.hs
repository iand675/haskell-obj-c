{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties for creating a compiler context.
--
-- Generated bindings for @MTL4CompilerDescriptor@.
module ObjC.Metal.MTL4CompilerDescriptor
  ( MTL4CompilerDescriptor
  , IsMTL4CompilerDescriptor(..)
  , label
  , setLabel
  , pipelineDataSetSerializer
  , setPipelineDataSetSerializer
  , labelSelector
  , pipelineDataSetSerializerSelector
  , setLabelSelector
  , setPipelineDataSetSerializerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Assigns an optional descriptor label to the compiler for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4CompilerDescriptor mtL4CompilerDescriptor => mtL4CompilerDescriptor -> IO (Id NSString)
label mtL4CompilerDescriptor =
  sendMessage mtL4CompilerDescriptor labelSelector

-- | Assigns an optional descriptor label to the compiler for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4CompilerDescriptor mtL4CompilerDescriptor, IsNSString value) => mtL4CompilerDescriptor -> value -> IO ()
setLabel mtL4CompilerDescriptor value =
  sendMessage mtL4CompilerDescriptor setLabelSelector (toNSString value)

-- | Assigns a pipeline data set serializer into which this compiler stores data for all pipelines it creates.
--
-- ObjC selector: @- pipelineDataSetSerializer@
pipelineDataSetSerializer :: IsMTL4CompilerDescriptor mtL4CompilerDescriptor => mtL4CompilerDescriptor -> IO RawId
pipelineDataSetSerializer mtL4CompilerDescriptor =
  sendMessage mtL4CompilerDescriptor pipelineDataSetSerializerSelector

-- | Assigns a pipeline data set serializer into which this compiler stores data for all pipelines it creates.
--
-- ObjC selector: @- setPipelineDataSetSerializer:@
setPipelineDataSetSerializer :: IsMTL4CompilerDescriptor mtL4CompilerDescriptor => mtL4CompilerDescriptor -> RawId -> IO ()
setPipelineDataSetSerializer mtL4CompilerDescriptor value =
  sendMessage mtL4CompilerDescriptor setPipelineDataSetSerializerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @pipelineDataSetSerializer@
pipelineDataSetSerializerSelector :: Selector '[] RawId
pipelineDataSetSerializerSelector = mkSelector "pipelineDataSetSerializer"

-- | @Selector@ for @setPipelineDataSetSerializer:@
setPipelineDataSetSerializerSelector :: Selector '[RawId] ()
setPipelineDataSetSerializerSelector = mkSelector "setPipelineDataSetSerializer:"

