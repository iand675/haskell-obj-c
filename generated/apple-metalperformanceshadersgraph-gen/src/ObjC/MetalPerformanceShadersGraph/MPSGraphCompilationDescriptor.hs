{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that consists of all the levers for compiling graphs.
--
-- Generated bindings for @MPSGraphCompilationDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphCompilationDescriptor
  ( MPSGraphCompilationDescriptor
  , IsMPSGraphCompilationDescriptor(..)
  , disableTypeInference
  , optimizationLevel
  , setOptimizationLevel
  , waitForCompilationCompletion
  , setWaitForCompilationCompletion
  , compilationCompletionHandler
  , setCompilationCompletionHandler
  , dispatchQueue
  , setDispatchQueue
  , optimizationProfile
  , setOptimizationProfile
  , callables
  , setCallables
  , reducedPrecisionFastMath
  , setReducedPrecisionFastMath
  , callablesSelector
  , compilationCompletionHandlerSelector
  , disableTypeInferenceSelector
  , dispatchQueueSelector
  , optimizationLevelSelector
  , optimizationProfileSelector
  , reducedPrecisionFastMathSelector
  , setCallablesSelector
  , setCompilationCompletionHandlerSelector
  , setDispatchQueueSelector
  , setOptimizationLevelSelector
  , setOptimizationProfileSelector
  , setReducedPrecisionFastMathSelector
  , setWaitForCompilationCompletionSelector
  , waitForCompilationCompletionSelector

  -- * Enum types
  , MPSGraphOptimization(MPSGraphOptimization)
  , pattern MPSGraphOptimizationLevel0
  , pattern MPSGraphOptimizationLevel1
  , MPSGraphOptimizationProfile(MPSGraphOptimizationProfile)
  , pattern MPSGraphOptimizationProfilePerformance
  , pattern MPSGraphOptimizationProfilePowerEfficiency
  , MPSGraphReducedPrecisionFastMath(MPSGraphReducedPrecisionFastMath)
  , pattern MPSGraphReducedPrecisionFastMathNone
  , pattern MPSGraphReducedPrecisionFastMathAllowFP16Conv2DWinogradTransformIntermediate
  , pattern MPSGraphReducedPrecisionFastMathAllowFP16Intermediates
  , pattern MPSGraphReducedPrecisionFastMathDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Turns off type inference and relies on type inference during runtime.
--
-- ObjC selector: @- disableTypeInference@
disableTypeInference :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO ()
disableTypeInference mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor disableTypeInferenceSelector

-- | The optimization level for the graph execution, default is MPSGraphOptimizationLevel1.
--
-- ObjC selector: @- optimizationLevel@
optimizationLevel :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO MPSGraphOptimization
optimizationLevel mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor optimizationLevelSelector

-- | The optimization level for the graph execution, default is MPSGraphOptimizationLevel1.
--
-- ObjC selector: @- setOptimizationLevel:@
setOptimizationLevel :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> MPSGraphOptimization -> IO ()
setOptimizationLevel mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setOptimizationLevelSelector value

-- | Flag that makes the compile or specialize call blocking till the entire compilation is complete, defaults to NO.
--
-- ObjC selector: @- waitForCompilationCompletion@
waitForCompilationCompletion :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO Bool
waitForCompilationCompletion mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor waitForCompilationCompletionSelector

-- | Flag that makes the compile or specialize call blocking till the entire compilation is complete, defaults to NO.
--
-- ObjC selector: @- setWaitForCompilationCompletion:@
setWaitForCompilationCompletion :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> Bool -> IO ()
setWaitForCompilationCompletion mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setWaitForCompilationCompletionSelector value

-- | The handler that the graph calls when the compilation completes.
--
-- Default value is nil.
--
-- ObjC selector: @- compilationCompletionHandler@
compilationCompletionHandler :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO (Ptr ())
compilationCompletionHandler mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor compilationCompletionHandlerSelector

-- | The handler that the graph calls when the compilation completes.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompilationCompletionHandler:@
setCompilationCompletionHandler :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> Ptr () -> IO ()
setCompilationCompletionHandler mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setCompilationCompletionHandlerSelector value

-- | The dispatch queue used for the compilation.
--
-- Default value is nil.
--
-- ObjC selector: @- dispatchQueue@
dispatchQueue :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO (Id NSObject)
dispatchQueue mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor dispatchQueueSelector

-- | The dispatch queue used for the compilation.
--
-- Default value is nil.
--
-- ObjC selector: @- setDispatchQueue:@
setDispatchQueue :: (IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor, IsNSObject value) => mpsGraphCompilationDescriptor -> value -> IO ()
setDispatchQueue mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setDispatchQueueSelector (toNSObject value)

-- | The optimization profile for the graph optimization.
--
-- Default is MPSGraphOptimizationProfilePerformance.
--
-- ObjC selector: @- optimizationProfile@
optimizationProfile :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO MPSGraphOptimizationProfile
optimizationProfile mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor optimizationProfileSelector

-- | The optimization profile for the graph optimization.
--
-- Default is MPSGraphOptimizationProfilePerformance.
--
-- ObjC selector: @- setOptimizationProfile:@
setOptimizationProfile :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> MPSGraphOptimizationProfile -> IO ()
setOptimizationProfile mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setOptimizationProfileSelector value

-- | The dictionary used during runtime to lookup the ``MPSGraphExecutable`` which correspond to the ``symbolName``.
--
-- ObjC selector: @- callables@
callables :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO RawId
callables mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor callablesSelector

-- | The dictionary used during runtime to lookup the ``MPSGraphExecutable`` which correspond to the ``symbolName``.
--
-- ObjC selector: @- setCallables:@
setCallables :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> RawId -> IO ()
setCallables mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setCallablesSelector value

-- | Across the executable allow reduced precision fast math optimizations.
--
-- ObjC selector: @- reducedPrecisionFastMath@
reducedPrecisionFastMath :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO MPSGraphReducedPrecisionFastMath
reducedPrecisionFastMath mpsGraphCompilationDescriptor =
  sendMessage mpsGraphCompilationDescriptor reducedPrecisionFastMathSelector

-- | Across the executable allow reduced precision fast math optimizations.
--
-- ObjC selector: @- setReducedPrecisionFastMath:@
setReducedPrecisionFastMath :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> MPSGraphReducedPrecisionFastMath -> IO ()
setReducedPrecisionFastMath mpsGraphCompilationDescriptor value =
  sendMessage mpsGraphCompilationDescriptor setReducedPrecisionFastMathSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disableTypeInference@
disableTypeInferenceSelector :: Selector '[] ()
disableTypeInferenceSelector = mkSelector "disableTypeInference"

-- | @Selector@ for @optimizationLevel@
optimizationLevelSelector :: Selector '[] MPSGraphOptimization
optimizationLevelSelector = mkSelector "optimizationLevel"

-- | @Selector@ for @setOptimizationLevel:@
setOptimizationLevelSelector :: Selector '[MPSGraphOptimization] ()
setOptimizationLevelSelector = mkSelector "setOptimizationLevel:"

-- | @Selector@ for @waitForCompilationCompletion@
waitForCompilationCompletionSelector :: Selector '[] Bool
waitForCompilationCompletionSelector = mkSelector "waitForCompilationCompletion"

-- | @Selector@ for @setWaitForCompilationCompletion:@
setWaitForCompilationCompletionSelector :: Selector '[Bool] ()
setWaitForCompilationCompletionSelector = mkSelector "setWaitForCompilationCompletion:"

-- | @Selector@ for @compilationCompletionHandler@
compilationCompletionHandlerSelector :: Selector '[] (Ptr ())
compilationCompletionHandlerSelector = mkSelector "compilationCompletionHandler"

-- | @Selector@ for @setCompilationCompletionHandler:@
setCompilationCompletionHandlerSelector :: Selector '[Ptr ()] ()
setCompilationCompletionHandlerSelector = mkSelector "setCompilationCompletionHandler:"

-- | @Selector@ for @dispatchQueue@
dispatchQueueSelector :: Selector '[] (Id NSObject)
dispatchQueueSelector = mkSelector "dispatchQueue"

-- | @Selector@ for @setDispatchQueue:@
setDispatchQueueSelector :: Selector '[Id NSObject] ()
setDispatchQueueSelector = mkSelector "setDispatchQueue:"

-- | @Selector@ for @optimizationProfile@
optimizationProfileSelector :: Selector '[] MPSGraphOptimizationProfile
optimizationProfileSelector = mkSelector "optimizationProfile"

-- | @Selector@ for @setOptimizationProfile:@
setOptimizationProfileSelector :: Selector '[MPSGraphOptimizationProfile] ()
setOptimizationProfileSelector = mkSelector "setOptimizationProfile:"

-- | @Selector@ for @callables@
callablesSelector :: Selector '[] RawId
callablesSelector = mkSelector "callables"

-- | @Selector@ for @setCallables:@
setCallablesSelector :: Selector '[RawId] ()
setCallablesSelector = mkSelector "setCallables:"

-- | @Selector@ for @reducedPrecisionFastMath@
reducedPrecisionFastMathSelector :: Selector '[] MPSGraphReducedPrecisionFastMath
reducedPrecisionFastMathSelector = mkSelector "reducedPrecisionFastMath"

-- | @Selector@ for @setReducedPrecisionFastMath:@
setReducedPrecisionFastMathSelector :: Selector '[MPSGraphReducedPrecisionFastMath] ()
setReducedPrecisionFastMathSelector = mkSelector "setReducedPrecisionFastMath:"

