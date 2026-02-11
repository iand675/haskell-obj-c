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
  , setLabelSelector
  , optionsSelector
  , setOptionsSelector


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

-- | Assigns an optional string that uniquely identifies a pipeline descriptor.
--
-- After you provide this label, you can use it to look up a pipeline state object by name in a binary archive.
--
-- ObjC selector: @- label@
label :: IsMTL4PipelineDescriptor mtL4PipelineDescriptor => mtL4PipelineDescriptor -> IO (Id NSString)
label mtL4PipelineDescriptor  =
  sendMsg mtL4PipelineDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional string that uniquely identifies a pipeline descriptor.
--
-- After you provide this label, you can use it to look up a pipeline state object by name in a binary archive.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4PipelineDescriptor mtL4PipelineDescriptor, IsNSString value) => mtL4PipelineDescriptor -> value -> IO ()
setLabel mtL4PipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4PipelineDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides compile-time options when you build the pipeline.
--
-- ObjC selector: @- options@
options :: IsMTL4PipelineDescriptor mtL4PipelineDescriptor => mtL4PipelineDescriptor -> IO (Id MTL4PipelineOptions)
options mtL4PipelineDescriptor  =
  sendMsg mtL4PipelineDescriptor (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides compile-time options when you build the pipeline.
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsMTL4PipelineDescriptor mtL4PipelineDescriptor, IsMTL4PipelineOptions value) => mtL4PipelineDescriptor -> value -> IO ()
setOptions mtL4PipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4PipelineDescriptor (mkSelector "setOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

