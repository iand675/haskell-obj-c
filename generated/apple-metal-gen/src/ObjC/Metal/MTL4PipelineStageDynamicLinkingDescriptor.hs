{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties to drive the dynamic linking process of a pipeline stage.
--
-- Generated bindings for @MTL4PipelineStageDynamicLinkingDescriptor@.
module ObjC.Metal.MTL4PipelineStageDynamicLinkingDescriptor
  ( MTL4PipelineStageDynamicLinkingDescriptor
  , IsMTL4PipelineStageDynamicLinkingDescriptor(..)
  , maxCallStackDepth
  , setMaxCallStackDepth
  , binaryLinkedFunctions
  , setBinaryLinkedFunctions
  , preloadedLibraries
  , setPreloadedLibraries
  , maxCallStackDepthSelector
  , setMaxCallStackDepthSelector
  , binaryLinkedFunctionsSelector
  , setBinaryLinkedFunctionsSelector
  , preloadedLibrariesSelector
  , setPreloadedLibrariesSelector


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

-- | Limits the maximum depth of the call stack for indirect function calls in the pipeline stage function.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO CULong
maxCallStackDepth mtL4PipelineStageDynamicLinkingDescriptor  =
    sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "maxCallStackDepth") retCULong []

-- | Limits the maximum depth of the call stack for indirect function calls in the pipeline stage function.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtL4PipelineStageDynamicLinkingDescriptor  value =
    sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "setMaxCallStackDepth:") retVoid [argCULong value]

-- | Provides the array of binary functions to link.
--
-- Binary functions are shader functions that you compile from Metal IR to machine code ahead of time using instances of ``MTL4Compiler``.
--
-- ObjC selector: @- binaryLinkedFunctions@
binaryLinkedFunctions :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO (Id NSArray)
binaryLinkedFunctions mtL4PipelineStageDynamicLinkingDescriptor  =
    sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "binaryLinkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the array of binary functions to link.
--
-- Binary functions are shader functions that you compile from Metal IR to machine code ahead of time using instances of ``MTL4Compiler``.
--
-- ObjC selector: @- setBinaryLinkedFunctions:@
setBinaryLinkedFunctions :: (IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor, IsNSArray value) => mtL4PipelineStageDynamicLinkingDescriptor -> value -> IO ()
setBinaryLinkedFunctions mtL4PipelineStageDynamicLinkingDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "setBinaryLinkedFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an array of dynamic libraries the compiler loads when it builds the pipeline.
--
-- ObjC selector: @- preloadedLibraries@
preloadedLibraries :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO (Id NSArray)
preloadedLibraries mtL4PipelineStageDynamicLinkingDescriptor  =
    sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "preloadedLibraries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of dynamic libraries the compiler loads when it builds the pipeline.
--
-- ObjC selector: @- setPreloadedLibraries:@
setPreloadedLibraries :: (IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor, IsNSArray value) => mtL4PipelineStageDynamicLinkingDescriptor -> value -> IO ()
setPreloadedLibraries mtL4PipelineStageDynamicLinkingDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4PipelineStageDynamicLinkingDescriptor (mkSelector "setPreloadedLibraries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCallStackDepth@
maxCallStackDepthSelector :: Selector
maxCallStackDepthSelector = mkSelector "maxCallStackDepth"

-- | @Selector@ for @setMaxCallStackDepth:@
setMaxCallStackDepthSelector :: Selector
setMaxCallStackDepthSelector = mkSelector "setMaxCallStackDepth:"

-- | @Selector@ for @binaryLinkedFunctions@
binaryLinkedFunctionsSelector :: Selector
binaryLinkedFunctionsSelector = mkSelector "binaryLinkedFunctions"

-- | @Selector@ for @setBinaryLinkedFunctions:@
setBinaryLinkedFunctionsSelector :: Selector
setBinaryLinkedFunctionsSelector = mkSelector "setBinaryLinkedFunctions:"

-- | @Selector@ for @preloadedLibraries@
preloadedLibrariesSelector :: Selector
preloadedLibrariesSelector = mkSelector "preloadedLibraries"

-- | @Selector@ for @setPreloadedLibraries:@
setPreloadedLibrariesSelector :: Selector
setPreloadedLibrariesSelector = mkSelector "setPreloadedLibraries:"

