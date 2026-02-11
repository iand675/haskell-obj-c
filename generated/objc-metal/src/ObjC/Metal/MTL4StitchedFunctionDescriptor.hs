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
  , functionGraphSelector
  , setFunctionGraphSelector
  , functionDescriptorsSelector
  , setFunctionDescriptorsSelector


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

-- | Sets the graph representing how to stitch functions together.
--
-- ObjC selector: @- functionGraph@
functionGraph :: IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor => mtL4StitchedFunctionDescriptor -> IO (Id MTLFunctionStitchingGraph)
functionGraph mtL4StitchedFunctionDescriptor  =
  sendMsg mtL4StitchedFunctionDescriptor (mkSelector "functionGraph") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the graph representing how to stitch functions together.
--
-- ObjC selector: @- setFunctionGraph:@
setFunctionGraph :: (IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor, IsMTLFunctionStitchingGraph value) => mtL4StitchedFunctionDescriptor -> value -> IO ()
setFunctionGraph mtL4StitchedFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4StitchedFunctionDescriptor (mkSelector "setFunctionGraph:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configures an array of function descriptors with references to functions that contribute to the stitching process.
--
-- ObjC selector: @- functionDescriptors@
functionDescriptors :: IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor => mtL4StitchedFunctionDescriptor -> IO (Id NSArray)
functionDescriptors mtL4StitchedFunctionDescriptor  =
  sendMsg mtL4StitchedFunctionDescriptor (mkSelector "functionDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures an array of function descriptors with references to functions that contribute to the stitching process.
--
-- ObjC selector: @- setFunctionDescriptors:@
setFunctionDescriptors :: (IsMTL4StitchedFunctionDescriptor mtL4StitchedFunctionDescriptor, IsNSArray value) => mtL4StitchedFunctionDescriptor -> value -> IO ()
setFunctionDescriptors mtL4StitchedFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4StitchedFunctionDescriptor (mkSelector "setFunctionDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionGraph@
functionGraphSelector :: Selector
functionGraphSelector = mkSelector "functionGraph"

-- | @Selector@ for @setFunctionGraph:@
setFunctionGraphSelector :: Selector
setFunctionGraphSelector = mkSelector "setFunctionGraph:"

-- | @Selector@ for @functionDescriptors@
functionDescriptorsSelector :: Selector
functionDescriptorsSelector = mkSelector "functionDescriptors"

-- | @Selector@ for @setFunctionDescriptors:@
setFunctionDescriptorsSelector :: Selector
setFunctionDescriptorsSelector = mkSelector "setFunctionDescriptors:"

