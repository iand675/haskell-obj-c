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
  , sendLevelSelector
  , setSendLevelSelector
  , sendLevelMetaParameterDefinitionSelector
  , setSendLevelMetaParameterDefinitionSelector


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

-- | sendLevel
--
-- Send level.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- sendLevel@
sendLevel :: IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry => phaseSpatialPipelineEntry -> IO CDouble
sendLevel phaseSpatialPipelineEntry  =
  sendMsg phaseSpatialPipelineEntry (mkSelector "sendLevel") retCDouble []

-- | sendLevel
--
-- Send level.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setSendLevel:@
setSendLevel :: IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry => phaseSpatialPipelineEntry -> CDouble -> IO ()
setSendLevel phaseSpatialPipelineEntry  value =
  sendMsg phaseSpatialPipelineEntry (mkSelector "setSendLevel:") retVoid [argCDouble (fromIntegral value)]

-- | sendLevelMetaParameterDefinition
--
-- An optional metaparameter used to drive the send level during playback.
--
-- ObjC selector: @- sendLevelMetaParameterDefinition@
sendLevelMetaParameterDefinition :: IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry => phaseSpatialPipelineEntry -> IO (Id PHASENumberMetaParameterDefinition)
sendLevelMetaParameterDefinition phaseSpatialPipelineEntry  =
  sendMsg phaseSpatialPipelineEntry (mkSelector "sendLevelMetaParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sendLevelMetaParameterDefinition
--
-- An optional metaparameter used to drive the send level during playback.
--
-- ObjC selector: @- setSendLevelMetaParameterDefinition:@
setSendLevelMetaParameterDefinition :: (IsPHASESpatialPipelineEntry phaseSpatialPipelineEntry, IsPHASENumberMetaParameterDefinition value) => phaseSpatialPipelineEntry -> value -> IO ()
setSendLevelMetaParameterDefinition phaseSpatialPipelineEntry  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseSpatialPipelineEntry (mkSelector "setSendLevelMetaParameterDefinition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendLevel@
sendLevelSelector :: Selector
sendLevelSelector = mkSelector "sendLevel"

-- | @Selector@ for @setSendLevel:@
setSendLevelSelector :: Selector
setSendLevelSelector = mkSelector "setSendLevel:"

-- | @Selector@ for @sendLevelMetaParameterDefinition@
sendLevelMetaParameterDefinitionSelector :: Selector
sendLevelMetaParameterDefinitionSelector = mkSelector "sendLevelMetaParameterDefinition"

-- | @Selector@ for @setSendLevelMetaParameterDefinition:@
setSendLevelMetaParameterDefinitionSelector :: Selector
setSendLevelMetaParameterDefinitionSelector = mkSelector "setSendLevelMetaParameterDefinition:"

