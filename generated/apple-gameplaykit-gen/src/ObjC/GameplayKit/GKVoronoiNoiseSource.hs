{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Voronoi noise partitions the space into angular, polygonal "cells", which are reminiscent of stained glass or crystal-like structures.
--
-- Generated bindings for @GKVoronoiNoiseSource@.
module ObjC.GameplayKit.GKVoronoiNoiseSource
  ( GKVoronoiNoiseSource
  , IsGKVoronoiNoiseSource(..)
  , voronoiNoiseWithFrequency_displacement_distanceEnabled_seed
  , initWithFrequency_displacement_distanceEnabled_seed
  , frequency
  , setFrequency
  , displacement
  , setDisplacement
  , distanceEnabled
  , setDistanceEnabled
  , seed
  , setSeed
  , displacementSelector
  , distanceEnabledSelector
  , frequencySelector
  , initWithFrequency_displacement_distanceEnabled_seedSelector
  , seedSelector
  , setDisplacementSelector
  , setDistanceEnabledSelector
  , setFrequencySelector
  , setSeedSelector
  , voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:@
voronoiNoiseWithFrequency_displacement_distanceEnabled_seed :: CDouble -> CDouble -> Bool -> CInt -> IO (Id GKVoronoiNoiseSource)
voronoiNoiseWithFrequency_displacement_distanceEnabled_seed frequency displacement distanceEnabled seed =
  do
    cls' <- getRequiredClass "GKVoronoiNoiseSource"
    sendClassMessage cls' voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector frequency displacement distanceEnabled seed

-- | @- initWithFrequency:displacement:distanceEnabled:seed:@
initWithFrequency_displacement_distanceEnabled_seed :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CDouble -> CDouble -> Bool -> CInt -> IO (Id GKVoronoiNoiseSource)
initWithFrequency_displacement_distanceEnabled_seed gkVoronoiNoiseSource frequency displacement distanceEnabled seed =
  sendOwnedMessage gkVoronoiNoiseSource initWithFrequency_displacement_distanceEnabled_seedSelector frequency displacement distanceEnabled seed

-- | @- frequency@
frequency :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO CDouble
frequency gkVoronoiNoiseSource =
  sendMessage gkVoronoiNoiseSource frequencySelector

-- | @- setFrequency:@
setFrequency :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CDouble -> IO ()
setFrequency gkVoronoiNoiseSource value =
  sendMessage gkVoronoiNoiseSource setFrequencySelector value

-- | @- displacement@
displacement :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO CDouble
displacement gkVoronoiNoiseSource =
  sendMessage gkVoronoiNoiseSource displacementSelector

-- | @- setDisplacement:@
setDisplacement :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CDouble -> IO ()
setDisplacement gkVoronoiNoiseSource value =
  sendMessage gkVoronoiNoiseSource setDisplacementSelector value

-- | @- distanceEnabled@
distanceEnabled :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO Bool
distanceEnabled gkVoronoiNoiseSource =
  sendMessage gkVoronoiNoiseSource distanceEnabledSelector

-- | @- setDistanceEnabled:@
setDistanceEnabled :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> Bool -> IO ()
setDistanceEnabled gkVoronoiNoiseSource value =
  sendMessage gkVoronoiNoiseSource setDistanceEnabledSelector value

-- | @- seed@
seed :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO CInt
seed gkVoronoiNoiseSource =
  sendMessage gkVoronoiNoiseSource seedSelector

-- | @- setSeed:@
setSeed :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CInt -> IO ()
setSeed gkVoronoiNoiseSource value =
  sendMessage gkVoronoiNoiseSource setSeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:@
voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector :: Selector '[CDouble, CDouble, Bool, CInt] (Id GKVoronoiNoiseSource)
voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector = mkSelector "voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:"

-- | @Selector@ for @initWithFrequency:displacement:distanceEnabled:seed:@
initWithFrequency_displacement_distanceEnabled_seedSelector :: Selector '[CDouble, CDouble, Bool, CInt] (Id GKVoronoiNoiseSource)
initWithFrequency_displacement_distanceEnabled_seedSelector = mkSelector "initWithFrequency:displacement:distanceEnabled:seed:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @displacement@
displacementSelector :: Selector '[] CDouble
displacementSelector = mkSelector "displacement"

-- | @Selector@ for @setDisplacement:@
setDisplacementSelector :: Selector '[CDouble] ()
setDisplacementSelector = mkSelector "setDisplacement:"

-- | @Selector@ for @distanceEnabled@
distanceEnabledSelector :: Selector '[] Bool
distanceEnabledSelector = mkSelector "distanceEnabled"

-- | @Selector@ for @setDistanceEnabled:@
setDistanceEnabledSelector :: Selector '[Bool] ()
setDistanceEnabledSelector = mkSelector "setDistanceEnabled:"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CInt
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector '[CInt] ()
setSeedSelector = mkSelector "setSeed:"

