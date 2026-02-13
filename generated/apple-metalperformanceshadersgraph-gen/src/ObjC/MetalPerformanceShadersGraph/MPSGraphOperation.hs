{-# LANGUAGE DataKinds #-}
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
  , controlDependenciesSelector
  , graphSelector
  , initSelector
  , inputTensorsSelector
  , nameSelector
  , outputTensorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable, please utilize graph methods to create and initialize operations.
--
-- ObjC selector: @- init@
init_ :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id MPSGraphOperation)
init_ mpsGraphOperation =
  sendOwnedMessage mpsGraphOperation initSelector

-- | The input tensors of the operation.
--
-- ObjC selector: @- inputTensors@
inputTensors :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSArray)
inputTensors mpsGraphOperation =
  sendMessage mpsGraphOperation inputTensorsSelector

-- | The output tensors of the operation.
--
-- ObjC selector: @- outputTensors@
outputTensors :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSArray)
outputTensors mpsGraphOperation =
  sendMessage mpsGraphOperation outputTensorsSelector

-- | The set of operations guaranteed to execute before this operation.
--
-- ObjC selector: @- controlDependencies@
controlDependencies :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSArray)
controlDependencies mpsGraphOperation =
  sendMessage mpsGraphOperation controlDependenciesSelector

-- | The graph on which the operation is defined.
--
-- ObjC selector: @- graph@
graph :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id MPSGraph)
graph mpsGraphOperation =
  sendMessage mpsGraphOperation graphSelector

-- | Name of the operation.
--
-- ObjC selector: @- name@
name :: IsMPSGraphOperation mpsGraphOperation => mpsGraphOperation -> IO (Id NSString)
name mpsGraphOperation =
  sendMessage mpsGraphOperation nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSGraphOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @inputTensors@
inputTensorsSelector :: Selector '[] (Id NSArray)
inputTensorsSelector = mkSelector "inputTensors"

-- | @Selector@ for @outputTensors@
outputTensorsSelector :: Selector '[] (Id NSArray)
outputTensorsSelector = mkSelector "outputTensors"

-- | @Selector@ for @controlDependencies@
controlDependenciesSelector :: Selector '[] (Id NSArray)
controlDependenciesSelector = mkSelector "controlDependencies"

-- | @Selector@ for @graph@
graphSelector :: Selector '[] (Id MPSGraph)
graphSelector = mkSelector "graph"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

