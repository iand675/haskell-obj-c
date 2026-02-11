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
  , voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector
  , initWithFrequency_displacement_distanceEnabled_seedSelector
  , frequencySelector
  , setFrequencySelector
  , displacementSelector
  , setDisplacementSelector
  , distanceEnabledSelector
  , setDistanceEnabledSelector
  , seedSelector
  , setSeedSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:@
voronoiNoiseWithFrequency_displacement_distanceEnabled_seed :: CDouble -> CDouble -> Bool -> CInt -> IO (Id GKVoronoiNoiseSource)
voronoiNoiseWithFrequency_displacement_distanceEnabled_seed frequency displacement distanceEnabled seed =
  do
    cls' <- getRequiredClass "GKVoronoiNoiseSource"
    sendClassMsg cls' (mkSelector "voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCDouble (fromIntegral displacement), argCULong (if distanceEnabled then 1 else 0), argCInt (fromIntegral seed)] >>= retainedObject . castPtr

-- | @- initWithFrequency:displacement:distanceEnabled:seed:@
initWithFrequency_displacement_distanceEnabled_seed :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CDouble -> CDouble -> Bool -> CInt -> IO (Id GKVoronoiNoiseSource)
initWithFrequency_displacement_distanceEnabled_seed gkVoronoiNoiseSource  frequency displacement distanceEnabled seed =
  sendMsg gkVoronoiNoiseSource (mkSelector "initWithFrequency:displacement:distanceEnabled:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCDouble (fromIntegral displacement), argCULong (if distanceEnabled then 1 else 0), argCInt (fromIntegral seed)] >>= ownedObject . castPtr

-- | @- frequency@
frequency :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO CDouble
frequency gkVoronoiNoiseSource  =
  sendMsg gkVoronoiNoiseSource (mkSelector "frequency") retCDouble []

-- | @- setFrequency:@
setFrequency :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CDouble -> IO ()
setFrequency gkVoronoiNoiseSource  value =
  sendMsg gkVoronoiNoiseSource (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- | @- displacement@
displacement :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO CDouble
displacement gkVoronoiNoiseSource  =
  sendMsg gkVoronoiNoiseSource (mkSelector "displacement") retCDouble []

-- | @- setDisplacement:@
setDisplacement :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CDouble -> IO ()
setDisplacement gkVoronoiNoiseSource  value =
  sendMsg gkVoronoiNoiseSource (mkSelector "setDisplacement:") retVoid [argCDouble (fromIntegral value)]

-- | @- distanceEnabled@
distanceEnabled :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO Bool
distanceEnabled gkVoronoiNoiseSource  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoronoiNoiseSource (mkSelector "distanceEnabled") retCULong []

-- | @- setDistanceEnabled:@
setDistanceEnabled :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> Bool -> IO ()
setDistanceEnabled gkVoronoiNoiseSource  value =
  sendMsg gkVoronoiNoiseSource (mkSelector "setDistanceEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- seed@
seed :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> IO CInt
seed gkVoronoiNoiseSource  =
  sendMsg gkVoronoiNoiseSource (mkSelector "seed") retCInt []

-- | @- setSeed:@
setSeed :: IsGKVoronoiNoiseSource gkVoronoiNoiseSource => gkVoronoiNoiseSource -> CInt -> IO ()
setSeed gkVoronoiNoiseSource  value =
  sendMsg gkVoronoiNoiseSource (mkSelector "setSeed:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:@
voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector :: Selector
voronoiNoiseWithFrequency_displacement_distanceEnabled_seedSelector = mkSelector "voronoiNoiseWithFrequency:displacement:distanceEnabled:seed:"

-- | @Selector@ for @initWithFrequency:displacement:distanceEnabled:seed:@
initWithFrequency_displacement_distanceEnabled_seedSelector :: Selector
initWithFrequency_displacement_distanceEnabled_seedSelector = mkSelector "initWithFrequency:displacement:distanceEnabled:seed:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @displacement@
displacementSelector :: Selector
displacementSelector = mkSelector "displacement"

-- | @Selector@ for @setDisplacement:@
setDisplacementSelector :: Selector
setDisplacementSelector = mkSelector "setDisplacement:"

-- | @Selector@ for @distanceEnabled@
distanceEnabledSelector :: Selector
distanceEnabledSelector = mkSelector "distanceEnabled"

-- | @Selector@ for @setDistanceEnabled:@
setDistanceEnabledSelector :: Selector
setDistanceEnabledSelector = mkSelector "setDistanceEnabled:"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector
setSeedSelector = mkSelector "setSeed:"

