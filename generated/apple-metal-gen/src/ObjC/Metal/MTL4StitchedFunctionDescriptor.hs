{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties that describe a shader function suitable for stitching.
--
-- Generated bindings for @MTL4StitchedFunctionDescriptor@.
module ObjC.Metal.MTL4StitchedFunctionDescriptor
  ( MTL4StitchedFunctionDescriptor
  , IsMTL4StitchedFunctionDescriptor(..)
  , functionGraph
  , setFunctionGraph
  , functionDescriptors
  , setFunctionDescriptors
  , functionDescriptorsSelector
  , functionGraphSelector
  , setFunctionDescriptorsSelector
  , setFunctionGraphSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sets the graph representing how to stitch functions together.
--
-- ObjC selector: @- functionGraph@
functionGraph :: IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor => mtL4StitchedFunctionDescriptor -> IO (Id MTLFunctionStitchingGraph)
functionGraph mtL4StitchedFunctionDescriptor =
  sendMessage mtL4StitchedFunctionDescriptor functionGraphSelector

-- | Sets the graph representing how to stitch functions together.
--
-- ObjC selector: @- setFunctionGraph:@
setFunctionGraph :: (IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor, IsMTLFunctionStitchingGraph value) => mtL4StitchedFunctionDescriptor -> value -> IO ()
setFunctionGraph mtL4StitchedFunctionDescriptor value =
  sendMessage mtL4StitchedFunctionDescriptor setFunctionGraphSelector (toMTLFunctionStitchingGraph value)

-- | Configures an array of function descriptors with references to functions that contribute to the stitching process.
--
-- ObjC selector: @- functionDescriptors@
functionDescriptors :: IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor => mtL4StitchedFunctionDescriptor -> IO (Id NSArray)
functionDescriptors mtL4StitchedFunctionDescriptor =
  sendMessage mtL4StitchedFunctionDescriptor functionDescriptorsSelector

-- | Configures an array of function descriptors with references to functions that contribute to the stitching process.
--
-- ObjC selector: @- setFunctionDescriptors:@
setFunctionDescriptors :: (IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor, IsNSArray value) => mtL4StitchedFunctionDescriptor -> value -> IO ()
setFunctionDescriptors mtL4StitchedFunctionDescriptor value =
  sendMessage mtL4StitchedFunctionDescriptor setFunctionDescriptorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionGraph@
functionGraphSelector :: Selector '[] (Id MTLFunctionStitchingGraph)
functionGraphSelector = mkSelector "functionGraph"

-- | @Selector@ for @setFunctionGraph:@
setFunctionGraphSelector :: Selector '[Id MTLFunctionStitchingGraph] ()
setFunctionGraphSelector = mkSelector "setFunctionGraph:"

-- | @Selector@ for @functionDescriptors@
functionDescriptorsSelector :: Selector '[] (Id NSArray)
functionDescriptorsSelector = mkSelector "functionDescriptors"

-- | @Selector@ for @setFunctionDescriptors:@
setFunctionDescriptorsSelector :: Selector '[Id NSArray] ()
setFunctionDescriptorsSelector = mkSelector "setFunctionDescriptors:"

