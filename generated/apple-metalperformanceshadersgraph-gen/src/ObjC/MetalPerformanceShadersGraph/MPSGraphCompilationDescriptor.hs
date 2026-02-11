{-# LANGUAGE PatternSynonyms #-}
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
  , disableTypeInferenceSelector
  , optimizationLevelSelector
  , setOptimizationLevelSelector
  , waitForCompilationCompletionSelector
  , setWaitForCompilationCompletionSelector
  , compilationCompletionHandlerSelector
  , setCompilationCompletionHandlerSelector
  , dispatchQueueSelector
  , setDispatchQueueSelector
  , optimizationProfileSelector
  , setOptimizationProfileSelector
  , callablesSelector
  , setCallablesSelector
  , reducedPrecisionFastMathSelector
  , setReducedPrecisionFastMathSelector

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
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Turns off type inference and relies on type inference during runtime.
--
-- ObjC selector: @- disableTypeInference@
disableTypeInference :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO ()
disableTypeInference mpsGraphCompilationDescriptor  =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "disableTypeInference") retVoid []

-- | The optimization level for the graph execution, default is MPSGraphOptimizationLevel1.
--
-- ObjC selector: @- optimizationLevel@
optimizationLevel :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO MPSGraphOptimization
optimizationLevel mpsGraphCompilationDescriptor  =
    fmap (coerce :: CULong -> MPSGraphOptimization) $ sendMsg mpsGraphCompilationDescriptor (mkSelector "optimizationLevel") retCULong []

-- | The optimization level for the graph execution, default is MPSGraphOptimizationLevel1.
--
-- ObjC selector: @- setOptimizationLevel:@
setOptimizationLevel :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> MPSGraphOptimization -> IO ()
setOptimizationLevel mpsGraphCompilationDescriptor  value =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "setOptimizationLevel:") retVoid [argCULong (coerce value)]

-- | Flag that makes the compile or specialize call blocking till the entire compilation is complete, defaults to NO.
--
-- ObjC selector: @- waitForCompilationCompletion@
waitForCompilationCompletion :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO Bool
waitForCompilationCompletion mpsGraphCompilationDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphCompilationDescriptor (mkSelector "waitForCompilationCompletion") retCULong []

-- | Flag that makes the compile or specialize call blocking till the entire compilation is complete, defaults to NO.
--
-- ObjC selector: @- setWaitForCompilationCompletion:@
setWaitForCompilationCompletion :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> Bool -> IO ()
setWaitForCompilationCompletion mpsGraphCompilationDescriptor  value =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "setWaitForCompilationCompletion:") retVoid [argCULong (if value then 1 else 0)]

-- | The handler that the graph calls when the compilation completes.
--
-- Default value is nil.
--
-- ObjC selector: @- compilationCompletionHandler@
compilationCompletionHandler :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO (Ptr ())
compilationCompletionHandler mpsGraphCompilationDescriptor  =
    fmap castPtr $ sendMsg mpsGraphCompilationDescriptor (mkSelector "compilationCompletionHandler") (retPtr retVoid) []

-- | The handler that the graph calls when the compilation completes.
--
-- Default value is nil.
--
-- ObjC selector: @- setCompilationCompletionHandler:@
setCompilationCompletionHandler :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> Ptr () -> IO ()
setCompilationCompletionHandler mpsGraphCompilationDescriptor  value =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "setCompilationCompletionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | The dispatch queue used for the compilation.
--
-- Default value is nil.
--
-- ObjC selector: @- dispatchQueue@
dispatchQueue :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO (Id NSObject)
dispatchQueue mpsGraphCompilationDescriptor  =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "dispatchQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The dispatch queue used for the compilation.
--
-- Default value is nil.
--
-- ObjC selector: @- setDispatchQueue:@
setDispatchQueue :: (IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor, IsNSObject value) => mpsGraphCompilationDescriptor -> value -> IO ()
setDispatchQueue mpsGraphCompilationDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mpsGraphCompilationDescriptor (mkSelector "setDispatchQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The optimization profile for the graph optimization.
--
-- Default is MPSGraphOptimizationProfilePerformance.
--
-- ObjC selector: @- optimizationProfile@
optimizationProfile :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO MPSGraphOptimizationProfile
optimizationProfile mpsGraphCompilationDescriptor  =
    fmap (coerce :: CULong -> MPSGraphOptimizationProfile) $ sendMsg mpsGraphCompilationDescriptor (mkSelector "optimizationProfile") retCULong []

-- | The optimization profile for the graph optimization.
--
-- Default is MPSGraphOptimizationProfilePerformance.
--
-- ObjC selector: @- setOptimizationProfile:@
setOptimizationProfile :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> MPSGraphOptimizationProfile -> IO ()
setOptimizationProfile mpsGraphCompilationDescriptor  value =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "setOptimizationProfile:") retVoid [argCULong (coerce value)]

-- | The dictionary used during runtime to lookup the ``MPSGraphExecutable`` which correspond to the ``symbolName``.
--
-- ObjC selector: @- callables@
callables :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO RawId
callables mpsGraphCompilationDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphCompilationDescriptor (mkSelector "callables") (retPtr retVoid) []

-- | The dictionary used during runtime to lookup the ``MPSGraphExecutable`` which correspond to the ``symbolName``.
--
-- ObjC selector: @- setCallables:@
setCallables :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> RawId -> IO ()
setCallables mpsGraphCompilationDescriptor  value =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "setCallables:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Across the executable allow reduced precision fast math optimizations.
--
-- ObjC selector: @- reducedPrecisionFastMath@
reducedPrecisionFastMath :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> IO MPSGraphReducedPrecisionFastMath
reducedPrecisionFastMath mpsGraphCompilationDescriptor  =
    fmap (coerce :: CULong -> MPSGraphReducedPrecisionFastMath) $ sendMsg mpsGraphCompilationDescriptor (mkSelector "reducedPrecisionFastMath") retCULong []

-- | Across the executable allow reduced precision fast math optimizations.
--
-- ObjC selector: @- setReducedPrecisionFastMath:@
setReducedPrecisionFastMath :: IsMPSGraphCompilationDescriptor mpsGraphCompilationDescriptor => mpsGraphCompilationDescriptor -> MPSGraphReducedPrecisionFastMath -> IO ()
setReducedPrecisionFastMath mpsGraphCompilationDescriptor  value =
    sendMsg mpsGraphCompilationDescriptor (mkSelector "setReducedPrecisionFastMath:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disableTypeInference@
disableTypeInferenceSelector :: Selector
disableTypeInferenceSelector = mkSelector "disableTypeInference"

-- | @Selector@ for @optimizationLevel@
optimizationLevelSelector :: Selector
optimizationLevelSelector = mkSelector "optimizationLevel"

-- | @Selector@ for @setOptimizationLevel:@
setOptimizationLevelSelector :: Selector
setOptimizationLevelSelector = mkSelector "setOptimizationLevel:"

-- | @Selector@ for @waitForCompilationCompletion@
waitForCompilationCompletionSelector :: Selector
waitForCompilationCompletionSelector = mkSelector "waitForCompilationCompletion"

-- | @Selector@ for @setWaitForCompilationCompletion:@
setWaitForCompilationCompletionSelector :: Selector
setWaitForCompilationCompletionSelector = mkSelector "setWaitForCompilationCompletion:"

-- | @Selector@ for @compilationCompletionHandler@
compilationCompletionHandlerSelector :: Selector
compilationCompletionHandlerSelector = mkSelector "compilationCompletionHandler"

-- | @Selector@ for @setCompilationCompletionHandler:@
setCompilationCompletionHandlerSelector :: Selector
setCompilationCompletionHandlerSelector = mkSelector "setCompilationCompletionHandler:"

-- | @Selector@ for @dispatchQueue@
dispatchQueueSelector :: Selector
dispatchQueueSelector = mkSelector "dispatchQueue"

-- | @Selector@ for @setDispatchQueue:@
setDispatchQueueSelector :: Selector
setDispatchQueueSelector = mkSelector "setDispatchQueue:"

-- | @Selector@ for @optimizationProfile@
optimizationProfileSelector :: Selector
optimizationProfileSelector = mkSelector "optimizationProfile"

-- | @Selector@ for @setOptimizationProfile:@
setOptimizationProfileSelector :: Selector
setOptimizationProfileSelector = mkSelector "setOptimizationProfile:"

-- | @Selector@ for @callables@
callablesSelector :: Selector
callablesSelector = mkSelector "callables"

-- | @Selector@ for @setCallables:@
setCallablesSelector :: Selector
setCallablesSelector = mkSelector "setCallables:"

-- | @Selector@ for @reducedPrecisionFastMath@
reducedPrecisionFastMathSelector :: Selector
reducedPrecisionFastMathSelector = mkSelector "reducedPrecisionFastMath"

-- | @Selector@ for @setReducedPrecisionFastMath:@
setReducedPrecisionFastMathSelector :: Selector
setReducedPrecisionFastMathSelector = mkSelector "setReducedPrecisionFastMath:"

