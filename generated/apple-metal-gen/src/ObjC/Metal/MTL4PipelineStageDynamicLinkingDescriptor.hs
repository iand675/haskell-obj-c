{-# LANGUAGE DataKinds #-}
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
  , binaryLinkedFunctionsSelector
  , maxCallStackDepthSelector
  , preloadedLibrariesSelector
  , setBinaryLinkedFunctionsSelector
  , setMaxCallStackDepthSelector
  , setPreloadedLibrariesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Limits the maximum depth of the call stack for indirect function calls in the pipeline stage function.
--
-- ObjC selector: @- maxCallStackDepth@
maxCallStackDepth :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO CULong
maxCallStackDepth mtL4PipelineStageDynamicLinkingDescriptor =
  sendMessage mtL4PipelineStageDynamicLinkingDescriptor maxCallStackDepthSelector

-- | Limits the maximum depth of the call stack for indirect function calls in the pipeline stage function.
--
-- ObjC selector: @- setMaxCallStackDepth:@
setMaxCallStackDepth :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> CULong -> IO ()
setMaxCallStackDepth mtL4PipelineStageDynamicLinkingDescriptor value =
  sendMessage mtL4PipelineStageDynamicLinkingDescriptor setMaxCallStackDepthSelector value

-- | Provides the array of binary functions to link.
--
-- Binary functions are shader functions that you compile from Metal IR to machine code ahead of time using instances of ``MTL4Compiler``.
--
-- ObjC selector: @- binaryLinkedFunctions@
binaryLinkedFunctions :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO (Id NSArray)
binaryLinkedFunctions mtL4PipelineStageDynamicLinkingDescriptor =
  sendMessage mtL4PipelineStageDynamicLinkingDescriptor binaryLinkedFunctionsSelector

-- | Provides the array of binary functions to link.
--
-- Binary functions are shader functions that you compile from Metal IR to machine code ahead of time using instances of ``MTL4Compiler``.
--
-- ObjC selector: @- setBinaryLinkedFunctions:@
setBinaryLinkedFunctions :: (IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor, IsNSArray value) => mtL4PipelineStageDynamicLinkingDescriptor -> value -> IO ()
setBinaryLinkedFunctions mtL4PipelineStageDynamicLinkingDescriptor value =
  sendMessage mtL4PipelineStageDynamicLinkingDescriptor setBinaryLinkedFunctionsSelector (toNSArray value)

-- | Provides an array of dynamic libraries the compiler loads when it builds the pipeline.
--
-- ObjC selector: @- preloadedLibraries@
preloadedLibraries :: IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor => mtL4PipelineStageDynamicLinkingDescriptor -> IO (Id NSArray)
preloadedLibraries mtL4PipelineStageDynamicLinkingDescriptor =
  sendMessage mtL4PipelineStageDynamicLinkingDescriptor preloadedLibrariesSelector

-- | Provides an array of dynamic libraries the compiler loads when it builds the pipeline.
--
-- ObjC selector: @- setPreloadedLibraries:@
setPreloadedLibraries :: (IsMTL4PipelineStageDynamicLinkingDescriptor mtL4PipelineStageDynamicLinkingDescriptor, IsNSArray value) => mtL4PipelineStageDynamicLinkingDescriptor -> value -> IO ()
setPreloadedLibraries mtL4PipelineStageDynamicLinkingDescriptor value =
  sendMessage mtL4PipelineStageDynamicLinkingDescriptor setPreloadedLibrariesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxCallStackDepth@
maxCallStackDepthSelector :: Selector '[] CULong
maxCallStackDepthSelector = mkSelector "maxCallStackDepth"

-- | @Selector@ for @setMaxCallStackDepth:@
setMaxCallStackDepthSelector :: Selector '[CULong] ()
setMaxCallStackDepthSelector = mkSelector "setMaxCallStackDepth:"

-- | @Selector@ for @binaryLinkedFunctions@
binaryLinkedFunctionsSelector :: Selector '[] (Id NSArray)
binaryLinkedFunctionsSelector = mkSelector "binaryLinkedFunctions"

-- | @Selector@ for @setBinaryLinkedFunctions:@
setBinaryLinkedFunctionsSelector :: Selector '[Id NSArray] ()
setBinaryLinkedFunctionsSelector = mkSelector "setBinaryLinkedFunctions:"

-- | @Selector@ for @preloadedLibraries@
preloadedLibrariesSelector :: Selector '[] (Id NSArray)
preloadedLibrariesSelector = mkSelector "preloadedLibraries"

-- | @Selector@ for @setPreloadedLibraries:@
setPreloadedLibrariesSelector :: Selector '[Id NSArray] ()
setPreloadedLibrariesSelector = mkSelector "setPreloadedLibraries:"

