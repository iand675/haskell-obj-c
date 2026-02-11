{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbolic representation of a compute operation.
--
-- @NSCopy@ will take a refrence, this is so @NSDictionary@ can work with the tensor. All operations are created, owned and destroyed by the graph.
--
-- Generated bindings for @MPSGraphOperation@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphOperation
  ( MPSGraphOperation
  , IsMPSGraphOperation(..)
  , init_
  , inputTensors
  , outputTensors
  , controlDependencies
  , graph
  , name
  , initSelector
  , inputTensorsSelector
  , outputTensorsSelector
  , controlDependenciesSelector
  , graphSelector
  , nameSelector


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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable, please utilize graph methods to create and initialize operations.
--
-- ObjC selector: @- init@
init_ :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id MPSGraphOperation)
init_ mpsGraphOperation  =
  sendMsg mpsGraphOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The input tensors of the operation.
--
-- ObjC selector: @- inputTensors@
inputTensors :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSArray)
inputTensors mpsGraphOperation  =
  sendMsg mpsGraphOperation (mkSelector "inputTensors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The output tensors of the operation.
--
-- ObjC selector: @- outputTensors@
outputTensors :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSArray)
outputTensors mpsGraphOperation  =
  sendMsg mpsGraphOperation (mkSelector "outputTensors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of operations guaranteed to execute before this operation.
--
-- ObjC selector: @- controlDependencies@
controlDependencies :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSArray)
controlDependencies mpsGraphOperation  =
  sendMsg mpsGraphOperation (mkSelector "controlDependencies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The graph on which the operation is defined.
--
-- ObjC selector: @- graph@
graph :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id MPSGraph)
graph mpsGraphOperation  =
  sendMsg mpsGraphOperation (mkSelector "graph") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of the operation.
--
-- ObjC selector: @- name@
name :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSString)
name mpsGraphOperation  =
  sendMsg mpsGraphOperation (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @inputTensors@
inputTensorsSelector :: Selector
inputTensorsSelector = mkSelector "inputTensors"

-- | @Selector@ for @outputTensors@
outputTensorsSelector :: Selector
outputTensorsSelector = mkSelector "outputTensors"

-- | @Selector@ for @controlDependencies@
controlDependenciesSelector :: Selector
controlDependenciesSelector = mkSelector "controlDependencies"

-- | @Selector@ for @graph@
graphSelector :: Selector
graphSelector = mkSelector "graph"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

