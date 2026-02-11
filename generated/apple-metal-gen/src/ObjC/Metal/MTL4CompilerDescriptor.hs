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
  , setLabelSelector
  , pipelineDataSetSerializerSelector
  , setPipelineDataSetSerializerSelector


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

-- | Assigns an optional descriptor label to the compiler for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4CompilerDescriptor mtL4CompilerDescriptor => mtL4CompilerDescriptor -> IO (Id NSString)
label mtL4CompilerDescriptor  =
    sendMsg mtL4CompilerDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional descriptor label to the compiler for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4CompilerDescriptor mtL4CompilerDescriptor, IsNSString value) => mtL4CompilerDescriptor -> value -> IO ()
setLabel mtL4CompilerDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4CompilerDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns a pipeline data set serializer into which this compiler stores data for all pipelines it creates.
--
-- ObjC selector: @- pipelineDataSetSerializer@
pipelineDataSetSerializer :: IsMTL4CompilerDescriptor mtL4CompilerDescriptor => mtL4CompilerDescriptor -> IO RawId
pipelineDataSetSerializer mtL4CompilerDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtL4CompilerDescriptor (mkSelector "pipelineDataSetSerializer") (retPtr retVoid) []

-- | Assigns a pipeline data set serializer into which this compiler stores data for all pipelines it creates.
--
-- ObjC selector: @- setPipelineDataSetSerializer:@
setPipelineDataSetSerializer :: IsMTL4CompilerDescriptor mtL4CompilerDescriptor => mtL4CompilerDescriptor -> RawId -> IO ()
setPipelineDataSetSerializer mtL4CompilerDescriptor  value =
    sendMsg mtL4CompilerDescriptor (mkSelector "setPipelineDataSetSerializer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @pipelineDataSetSerializer@
pipelineDataSetSerializerSelector :: Selector
pipelineDataSetSerializerSelector = mkSelector "pipelineDataSetSerializer"

-- | @Selector@ for @setPipelineDataSetSerializer:@
setPipelineDataSetSerializerSelector :: Selector
setPipelineDataSetSerializerSelector = mkSelector "setPipelineDataSetSerializer:"

