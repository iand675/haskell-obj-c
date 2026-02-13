{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base type for descriptors you use for building pipeline state objects.
--
-- Generated bindings for @MTL4PipelineDescriptor@.
module ObjC.Metal.MTL4PipelineDescriptor
  ( MTL4PipelineDescriptor
  , IsMTL4PipelineDescriptor(..)
  , label
  , setLabel
  , options
  , setOptions
  , labelSelector
  , optionsSelector
  , setLabelSelector
  , setOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Assigns an optional string that uniquely identifies a pipeline descriptor.
--
-- After you provide this label, you can use it to look up a pipeline state object by name in a binary archive.
--
-- ObjC selector: @- label@
label :: IsMTL4PipelineDescriptor mtL4PipelineDescriptor => mtL4PipelineDescriptor -> IO (Id NSString)
label mtL4PipelineDescriptor =
  sendMessage mtL4PipelineDescriptor labelSelector

-- | Assigns an optional string that uniquely identifies a pipeline descriptor.
--
-- After you provide this label, you can use it to look up a pipeline state object by name in a binary archive.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4PipelineDescriptor mtL4PipelineDescriptor, IsNSString value) => mtL4PipelineDescriptor -> value -> IO ()
setLabel mtL4PipelineDescriptor value =
  sendMessage mtL4PipelineDescriptor setLabelSelector (toNSString value)

-- | Provides compile-time options when you build the pipeline.
--
-- ObjC selector: @- options@
options :: IsMTL4PipelineDescriptor mtL4PipelineDescriptor => mtL4PipelineDescriptor -> IO (Id MTL4PipelineOptions)
options mtL4PipelineDescriptor =
  sendMessage mtL4PipelineDescriptor optionsSelector

-- | Provides compile-time options when you build the pipeline.
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsMTL4PipelineDescriptor mtL4PipelineDescriptor, IsMTL4PipelineOptions value) => mtL4PipelineDescriptor -> value -> IO ()
setOptions mtL4PipelineDescriptor value =
  sendMessage mtL4PipelineDescriptor setOptionsSelector (toMTL4PipelineOptions value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id MTL4PipelineOptions)
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[Id MTL4PipelineOptions] ()
setOptionsSelector = mkSelector "setOptions:"

