{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLOptimizationHints
--
-- An object to hold hints that CoreML could use for further optimization
--
-- Generated bindings for @MLOptimizationHints@.
module ObjC.CoreML.MLOptimizationHints
  ( MLOptimizationHints
  , IsMLOptimizationHints(..)
  , reshapeFrequency
  , setReshapeFrequency
  , specializationStrategy
  , setSpecializationStrategy
  , reshapeFrequencySelector
  , setReshapeFrequencySelector
  , setSpecializationStrategySelector
  , specializationStrategySelector

  -- * Enum types
  , MLReshapeFrequencyHint(MLReshapeFrequencyHint)
  , pattern MLReshapeFrequencyHintFrequent
  , pattern MLReshapeFrequencyHintInfrequent
  , MLSpecializationStrategy(MLSpecializationStrategy)
  , pattern MLSpecializationStrategyDefault
  , pattern MLSpecializationStrategyFastPrediction

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The anticipated reshape frequency
--
-- CoreML framework needs to reshape the model with new shapes for models with flexible input. Specify the anticipated reshape frequency (frequent or infrequent), so that the framework can optimize for fast shape switching or fast prediction on seen shapes.
--
-- The default value is frequent, which means CoreML tries to switch to new shapes as fast as possible
--
-- ObjC selector: @- reshapeFrequency@
reshapeFrequency :: IsMLOptimizationHints mlOptimizationHints => mlOptimizationHints -> IO MLReshapeFrequencyHint
reshapeFrequency mlOptimizationHints =
  sendMessage mlOptimizationHints reshapeFrequencySelector

-- | The anticipated reshape frequency
--
-- CoreML framework needs to reshape the model with new shapes for models with flexible input. Specify the anticipated reshape frequency (frequent or infrequent), so that the framework can optimize for fast shape switching or fast prediction on seen shapes.
--
-- The default value is frequent, which means CoreML tries to switch to new shapes as fast as possible
--
-- ObjC selector: @- setReshapeFrequency:@
setReshapeFrequency :: IsMLOptimizationHints mlOptimizationHints => mlOptimizationHints -> MLReshapeFrequencyHint -> IO ()
setReshapeFrequency mlOptimizationHints value =
  sendMessage mlOptimizationHints setReshapeFrequencySelector value

-- | Optimization strategy for the model specialization.
--
-- Core ML segments the model's compute graph and optimizes each segment for the target compute device. This process can affect the model loading time and the prediction latency.
--
-- Use this option to tailor the specialization strategy for your application.
--
-- ObjC selector: @- specializationStrategy@
specializationStrategy :: IsMLOptimizationHints mlOptimizationHints => mlOptimizationHints -> IO MLSpecializationStrategy
specializationStrategy mlOptimizationHints =
  sendMessage mlOptimizationHints specializationStrategySelector

-- | Optimization strategy for the model specialization.
--
-- Core ML segments the model's compute graph and optimizes each segment for the target compute device. This process can affect the model loading time and the prediction latency.
--
-- Use this option to tailor the specialization strategy for your application.
--
-- ObjC selector: @- setSpecializationStrategy:@
setSpecializationStrategy :: IsMLOptimizationHints mlOptimizationHints => mlOptimizationHints -> MLSpecializationStrategy -> IO ()
setSpecializationStrategy mlOptimizationHints value =
  sendMessage mlOptimizationHints setSpecializationStrategySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reshapeFrequency@
reshapeFrequencySelector :: Selector '[] MLReshapeFrequencyHint
reshapeFrequencySelector = mkSelector "reshapeFrequency"

-- | @Selector@ for @setReshapeFrequency:@
setReshapeFrequencySelector :: Selector '[MLReshapeFrequencyHint] ()
setReshapeFrequencySelector = mkSelector "setReshapeFrequency:"

-- | @Selector@ for @specializationStrategy@
specializationStrategySelector :: Selector '[] MLSpecializationStrategy
specializationStrategySelector = mkSelector "specializationStrategy"

-- | @Selector@ for @setSpecializationStrategy:@
setSpecializationStrategySelector :: Selector '[MLSpecializationStrategy] ()
setSpecializationStrategySelector = mkSelector "setSpecializationStrategy:"

