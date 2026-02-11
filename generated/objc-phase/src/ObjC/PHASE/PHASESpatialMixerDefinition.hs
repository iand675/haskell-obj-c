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
  , initSelector
  , newSelector
  , initWithSpatialPipelineSelector
  , initWithSpatialPipeline_identifierSelector
  , spatialPipelineSelector
  , distanceModelParametersSelector
  , setDistanceModelParametersSelector
  , listenerDirectivityModelParametersSelector
  , setListenerDirectivityModelParametersSelector
  , sourceDirectivityModelParametersSelector
  , setSourceDirectivityModelParametersSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASESpatialMixerDefinition)
init_ phaseSpatialMixerDefinition  =
  sendMsg phaseSpatialMixerDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESpatialMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESpatialMixerDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithSpatialPipeline phaseSpatialMixerDefinition  spatialPipeline =
withObjCPtr spatialPipeline $ \raw_spatialPipeline ->
    sendMsg phaseSpatialMixerDefinition (mkSelector "initWithSpatialPipeline:") (retPtr retVoid) [argPtr (castPtr raw_spatialPipeline :: Ptr ())] >>= ownedObject . castPtr

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
initWithSpatialPipeline_identifier phaseSpatialMixerDefinition  spatialPipeline identifier =
withObjCPtr spatialPipeline $ \raw_spatialPipeline ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg phaseSpatialMixerDefinition (mkSelector "initWithSpatialPipeline:identifier:") (retPtr retVoid) [argPtr (castPtr raw_spatialPipeline :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | spatialPipeline
--
-- Spatial Pipeline.
--
-- ObjC selector: @- spatialPipeline@
spatialPipeline :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASESpatialPipeline)
spatialPipeline phaseSpatialMixerDefinition  =
  sendMsg phaseSpatialMixerDefinition (mkSelector "spatialPipeline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | distanceModelParameters
--
-- Distance model parameters (optional).
--
-- ObjC selector: @- distanceModelParameters@
distanceModelParameters :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASEDistanceModelParameters)
distanceModelParameters phaseSpatialMixerDefinition  =
  sendMsg phaseSpatialMixerDefinition (mkSelector "distanceModelParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | distanceModelParameters
--
-- Distance model parameters (optional).
--
-- ObjC selector: @- setDistanceModelParameters:@
setDistanceModelParameters :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASEDistanceModelParameters value) => phaseSpatialMixerDefinition -> value -> IO ()
setDistanceModelParameters phaseSpatialMixerDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseSpatialMixerDefinition (mkSelector "setDistanceModelParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | listenerDirectivityModelParameters
--
-- Listener directivity model parameters (optional).
--
-- ObjC selector: @- listenerDirectivityModelParameters@
listenerDirectivityModelParameters :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASEDirectivityModelParameters)
listenerDirectivityModelParameters phaseSpatialMixerDefinition  =
  sendMsg phaseSpatialMixerDefinition (mkSelector "listenerDirectivityModelParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | listenerDirectivityModelParameters
--
-- Listener directivity model parameters (optional).
--
-- ObjC selector: @- setListenerDirectivityModelParameters:@
setListenerDirectivityModelParameters :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASEDirectivityModelParameters value) => phaseSpatialMixerDefinition -> value -> IO ()
setListenerDirectivityModelParameters phaseSpatialMixerDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseSpatialMixerDefinition (mkSelector "setListenerDirectivityModelParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sourceDirectivityModelParameters
--
-- Source directivity model parameters (optional).
--
-- ObjC selector: @- sourceDirectivityModelParameters@
sourceDirectivityModelParameters :: IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition => phaseSpatialMixerDefinition -> IO (Id PHASEDirectivityModelParameters)
sourceDirectivityModelParameters phaseSpatialMixerDefinition  =
  sendMsg phaseSpatialMixerDefinition (mkSelector "sourceDirectivityModelParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceDirectivityModelParameters
--
-- Source directivity model parameters (optional).
--
-- ObjC selector: @- setSourceDirectivityModelParameters:@
setSourceDirectivityModelParameters :: (IsPHASESpatialMixerDefinition phaseSpatialMixerDefinition, IsPHASEDirectivityModelParameters value) => phaseSpatialMixerDefinition -> value -> IO ()
setSourceDirectivityModelParameters phaseSpatialMixerDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseSpatialMixerDefinition (mkSelector "setSourceDirectivityModelParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSpatialPipeline:@
initWithSpatialPipelineSelector :: Selector
initWithSpatialPipelineSelector = mkSelector "initWithSpatialPipeline:"

-- | @Selector@ for @initWithSpatialPipeline:identifier:@
initWithSpatialPipeline_identifierSelector :: Selector
initWithSpatialPipeline_identifierSelector = mkSelector "initWithSpatialPipeline:identifier:"

-- | @Selector@ for @spatialPipeline@
spatialPipelineSelector :: Selector
spatialPipelineSelector = mkSelector "spatialPipeline"

-- | @Selector@ for @distanceModelParameters@
distanceModelParametersSelector :: Selector
distanceModelParametersSelector = mkSelector "distanceModelParameters"

-- | @Selector@ for @setDistanceModelParameters:@
setDistanceModelParametersSelector :: Selector
setDistanceModelParametersSelector = mkSelector "setDistanceModelParameters:"

-- | @Selector@ for @listenerDirectivityModelParameters@
listenerDirectivityModelParametersSelector :: Selector
listenerDirectivityModelParametersSelector = mkSelector "listenerDirectivityModelParameters"

-- | @Selector@ for @setListenerDirectivityModelParameters:@
setListenerDirectivityModelParametersSelector :: Selector
setListenerDirectivityModelParametersSelector = mkSelector "setListenerDirectivityModelParameters:"

-- | @Selector@ for @sourceDirectivityModelParameters@
sourceDirectivityModelParametersSelector :: Selector
sourceDirectivityModelParametersSelector = mkSelector "sourceDirectivityModelParameters"

-- | @Selector@ for @setSourceDirectivityModelParameters:@
setSourceDirectivityModelParametersSelector :: Selector
setSourceDirectivityModelParametersSelector = mkSelector "setSourceDirectivityModelParameters:"

