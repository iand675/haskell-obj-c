{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESpatialPipelineEntry
--
-- Spatial Pipeline Entry.
--
-- Generated bindings for @PHASESpatialPipelineEntry@.
module ObjC.PHASE.PHASESpatialPipelineEntry
  ( PHASESpatialPipelineEntry
  , IsPHASESpatialPipelineEntry(..)
  , sendLevel
  , setSendLevel
  , sendLevelMetaParameterDefinition
  , setSendLevelMetaParameterDefinition
  , sendLevelMetaParameterDefinitionSelector
  , sendLevelSelector
  , setSendLevelMetaParameterDefinitionSelector
  , setSendLevelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sendLevel
--
-- Send level.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- sendLevel@
sendLevel :: IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry => phaseSpatialPipelineEntry -> IO CDouble
sendLevel phaseSpatialPipelineEntry =
  sendMessage phaseSpatialPipelineEntry sendLevelSelector

-- | sendLevel
--
-- Send level.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setSendLevel:@
setSendLevel :: IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry => phaseSpatialPipelineEntry -> CDouble -> IO ()
setSendLevel phaseSpatialPipelineEntry value =
  sendMessage phaseSpatialPipelineEntry setSendLevelSelector value

-- | sendLevelMetaParameterDefinition
--
-- An optional metaparameter used to drive the send level during playback.
--
-- ObjC selector: @- sendLevelMetaParameterDefinition@
sendLevelMetaParameterDefinition :: IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry => phaseSpatialPipelineEntry -> IO (Id PHASENumberMetaParameterDefinition)
sendLevelMetaParameterDefinition phaseSpatialPipelineEntry =
  sendMessage phaseSpatialPipelineEntry sendLevelMetaParameterDefinitionSelector

-- | sendLevelMetaParameterDefinition
--
-- An optional metaparameter used to drive the send level during playback.
--
-- ObjC selector: @- setSendLevelMetaParameterDefinition:@
setSendLevelMetaParameterDefinition :: (IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry, IsPHASENumberMetaParameterDefinition value) => phaseSpatialPipelineEntry -> value -> IO ()
setSendLevelMetaParameterDefinition phaseSpatialPipelineEntry value =
  sendMessage phaseSpatialPipelineEntry setSendLevelMetaParameterDefinitionSelector (toPHASENumberMetaParameterDefinition value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendLevel@
sendLevelSelector :: Selector '[] CDouble
sendLevelSelector = mkSelector "sendLevel"

-- | @Selector@ for @setSendLevel:@
setSendLevelSelector :: Selector '[CDouble] ()
setSendLevelSelector = mkSelector "setSendLevel:"

-- | @Selector@ for @sendLevelMetaParameterDefinition@
sendLevelMetaParameterDefinitionSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
sendLevelMetaParameterDefinitionSelector = mkSelector "sendLevelMetaParameterDefinition"

-- | @Selector@ for @setSendLevelMetaParameterDefinition:@
setSendLevelMetaParameterDefinitionSelector :: Selector '[Id PHASENumberMetaParameterDefinition] ()
setSendLevelMetaParameterDefinitionSelector = mkSelector "setSendLevelMetaParameterDefinition:"

