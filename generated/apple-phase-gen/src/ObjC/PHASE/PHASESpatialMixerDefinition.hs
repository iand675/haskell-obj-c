{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESpatialMixerDefinition
--
-- Spatial mixer definition.
--
-- Spatial mixers render audio with spatialization and environmental effects.
--
-- Generated bindings for @PHASESpatialMixerDefinition@.
module ObjC.PHASE.PHASESpatialMixerDefinition
  ( PHASESpatialMixerDefinition
  , IsPHASESpatialMixerDefinition(..)
  , init_
  , new
  , initWithSpatialPipeline
  , initWithSpatialPipeline_identifier
  , spatialPipeline
  , distanceModelParameters
  , setDistanceModelParameters
  , listenerDirectivityModelParameters
  , setListenerDirectivityModelParameters
  , sourceDirectivityModelParameters
  , setSourceDirectivityModelParameters
  , distanceModelParametersSelector
  , initSelector
  , initWithSpatialPipelineSelector
  , initWithSpatialPipeline_identifierSelector
  , listenerDirectivityModelParametersSelector
  , newSelector
  , setDistanceModelParametersSelector
  , setListenerDirectivityModelParametersSelector
  , setSourceDirectivityModelParametersSelector
  , sourceDirectivityModelParametersSelector
  , spatialPipelineSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASESpatialMixerDefinition)
init_ phaseSpatialMixerDefinition =
  sendOwnedMessage phaseSpatialMixerDefinition initSelector

-- | @+ new@
new :: IO (Id PHASESpatialMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESpatialMixerDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithSpatialPipeline
--
-- Create a new PHASESpatialMixerDefinition
--
-- @spatialPipeline@ — A spatial pipeline.
--
-- Returns: A new PHASESpatialMixerDefinition object
--
-- ObjC selector: @- initWithSpatialPipeline:@
initWithSpatialPipeline :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASESpatialPipeline spatialPipeline) => phaseSpatialMixerDefinition -> spatialPipeline -> IO (Id PHASESpatialMixerDefinition)
initWithSpatialPipeline phaseSpatialMixerDefinition spatialPipeline =
  sendOwnedMessage phaseSpatialMixerDefinition initWithSpatialPipelineSelector (toPHASESpatialPipeline spatialPipeline)

-- | initWithSpatialPipeline:identifier
--
-- Create a new PHASESpatialMixerDefinition
--
-- @spatialPipeline@ — A spatial pipeline.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASESpatialMixerDefinition object
--
-- ObjC selector: @- initWithSpatialPipeline:identifier:@
initWithSpatialPipeline_identifier :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASESpatialPipeline spatialPipeline, IsNSString identifier) => phaseSpatialMixerDefinition -> spatialPipeline -> identifier -> IO (Id PHASESpatialMixerDefinition)
initWithSpatialPipeline_identifier phaseSpatialMixerDefinition spatialPipeline identifier =
  sendOwnedMessage phaseSpatialMixerDefinition initWithSpatialPipeline_identifierSelector (toPHASESpatialPipeline spatialPipeline) (toNSString identifier)

-- | spatialPipeline
--
-- Spatial Pipeline.
--
-- ObjC selector: @- spatialPipeline@
spatialPipeline :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASESpatialPipeline)
spatialPipeline phaseSpatialMixerDefinition =
  sendMessage phaseSpatialMixerDefinition spatialPipelineSelector

-- | distanceModelParameters
--
-- Distance model parameters (optional).
--
-- ObjC selector: @- distanceModelParameters@
distanceModelParameters :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASEDistanceModelParameters)
distanceModelParameters phaseSpatialMixerDefinition =
  sendMessage phaseSpatialMixerDefinition distanceModelParametersSelector

-- | distanceModelParameters
--
-- Distance model parameters (optional).
--
-- ObjC selector: @- setDistanceModelParameters:@
setDistanceModelParameters :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASEDistanceModelParameters value) => phaseSpatialMixerDefinition -> value -> IO ()
setDistanceModelParameters phaseSpatialMixerDefinition value =
  sendMessage phaseSpatialMixerDefinition setDistanceModelParametersSelector (toPHASEDistanceModelParameters value)

-- | listenerDirectivityModelParameters
--
-- Listener directivity model parameters (optional).
--
-- ObjC selector: @- listenerDirectivityModelParameters@
listenerDirectivityModelParameters :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASEDirectivityModelParameters)
listenerDirectivityModelParameters phaseSpatialMixerDefinition =
  sendMessage phaseSpatialMixerDefinition listenerDirectivityModelParametersSelector

-- | listenerDirectivityModelParameters
--
-- Listener directivity model parameters (optional).
--
-- ObjC selector: @- setListenerDirectivityModelParameters:@
setListenerDirectivityModelParameters :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASEDirectivityModelParameters value) => phaseSpatialMixerDefinition -> value -> IO ()
setListenerDirectivityModelParameters phaseSpatialMixerDefinition value =
  sendMessage phaseSpatialMixerDefinition setListenerDirectivityModelParametersSelector (toPHASEDirectivityModelParameters value)

-- | sourceDirectivityModelParameters
--
-- Source directivity model parameters (optional).
--
-- ObjC selector: @- sourceDirectivityModelParameters@
sourceDirectivityModelParameters :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASEDirectivityModelParameters)
sourceDirectivityModelParameters phaseSpatialMixerDefinition =
  sendMessage phaseSpatialMixerDefinition sourceDirectivityModelParametersSelector

-- | sourceDirectivityModelParameters
--
-- Source directivity model parameters (optional).
--
-- ObjC selector: @- setSourceDirectivityModelParameters:@
setSourceDirectivityModelParameters :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASEDirectivityModelParameters value) => phaseSpatialMixerDefinition -> value -> IO ()
setSourceDirectivityModelParameters phaseSpatialMixerDefinition value =
  sendMessage phaseSpatialMixerDefinition setSourceDirectivityModelParametersSelector (toPHASEDirectivityModelParameters value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESpatialMixerDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESpatialMixerDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSpatialPipeline:@
initWithSpatialPipelineSelector :: Selector '[Id PHASESpatialPipeline] (Id PHASESpatialMixerDefinition)
initWithSpatialPipelineSelector = mkSelector "initWithSpatialPipeline:"

-- | @Selector@ for @initWithSpatialPipeline:identifier:@
initWithSpatialPipeline_identifierSelector :: Selector '[Id PHASESpatialPipeline, Id NSString] (Id PHASESpatialMixerDefinition)
initWithSpatialPipeline_identifierSelector = mkSelector "initWithSpatialPipeline:identifier:"

-- | @Selector@ for @spatialPipeline@
spatialPipelineSelector :: Selector '[] (Id PHASESpatialPipeline)
spatialPipelineSelector = mkSelector "spatialPipeline"

-- | @Selector@ for @distanceModelParameters@
distanceModelParametersSelector :: Selector '[] (Id PHASEDistanceModelParameters)
distanceModelParametersSelector = mkSelector "distanceModelParameters"

-- | @Selector@ for @setDistanceModelParameters:@
setDistanceModelParametersSelector :: Selector '[Id PHASEDistanceModelParameters] ()
setDistanceModelParametersSelector = mkSelector "setDistanceModelParameters:"

-- | @Selector@ for @listenerDirectivityModelParameters@
listenerDirectivityModelParametersSelector :: Selector '[] (Id PHASEDirectivityModelParameters)
listenerDirectivityModelParametersSelector = mkSelector "listenerDirectivityModelParameters"

-- | @Selector@ for @setListenerDirectivityModelParameters:@
setListenerDirectivityModelParametersSelector :: Selector '[Id PHASEDirectivityModelParameters] ()
setListenerDirectivityModelParametersSelector = mkSelector "setListenerDirectivityModelParameters:"

-- | @Selector@ for @sourceDirectivityModelParameters@
sourceDirectivityModelParametersSelector :: Selector '[] (Id PHASEDirectivityModelParameters)
sourceDirectivityModelParametersSelector = mkSelector "sourceDirectivityModelParameters"

-- | @Selector@ for @setSourceDirectivityModelParameters:@
setSourceDirectivityModelParametersSelector :: Selector '[Id PHASEDirectivityModelParameters] ()
setSourceDirectivityModelParametersSelector = mkSelector "setSourceDirectivityModelParameters:"

