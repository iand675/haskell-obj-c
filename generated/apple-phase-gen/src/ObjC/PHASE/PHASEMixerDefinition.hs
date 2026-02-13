{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMixerDefinition
--
-- The base class for a mixer definition.
--
-- Mixer definitions control how audio will be rendered to the output in PHASE.
--
-- Generated bindings for @PHASEMixerDefinition@.
module ObjC.PHASE.PHASEMixerDefinition
  ( PHASEMixerDefinition
  , IsPHASEMixerDefinition(..)
  , init_
  , new
  , gain
  , setGain
  , gainMetaParameterDefinition
  , setGainMetaParameterDefinition
  , gainMetaParameterDefinitionSelector
  , gainSelector
  , initSelector
  , newSelector
  , setGainMetaParameterDefinitionSelector
  , setGainSelector


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
init_ :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> IO (Id PHASEMixerDefinition)
init_ phaseMixerDefinition =
  sendOwnedMessage phaseMixerDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEMixerDefinition"
    sendOwnedClassMessage cls' newSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> IO CDouble
gain phaseMixerDefinition =
  sendMessage phaseMixerDefinition gainSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> CDouble -> IO ()
setGain phaseMixerDefinition value =
  sendMessage phaseMixerDefinition setGainSelector value

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable real-time control of the gain during playback.
--
-- ObjC selector: @- gainMetaParameterDefinition@
gainMetaParameterDefinition :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> IO (Id PHASENumberMetaParameterDefinition)
gainMetaParameterDefinition phaseMixerDefinition =
  sendMessage phaseMixerDefinition gainMetaParameterDefinitionSelector

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable real-time control of the gain during playback.
--
-- ObjC selector: @- setGainMetaParameterDefinition:@
setGainMetaParameterDefinition :: (IsPHASEMixerDefinition phaseMixerDefinition, IsPHASENumberMetaParameterDefinition value) => phaseMixerDefinition -> value -> IO ()
setGainMetaParameterDefinition phaseMixerDefinition value =
  sendMessage phaseMixerDefinition setGainMetaParameterDefinitionSelector (toPHASENumberMetaParameterDefinition value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMixerDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMixerDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CDouble
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector '[CDouble] ()
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @gainMetaParameterDefinition@
gainMetaParameterDefinitionSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
gainMetaParameterDefinitionSelector = mkSelector "gainMetaParameterDefinition"

-- | @Selector@ for @setGainMetaParameterDefinition:@
setGainMetaParameterDefinitionSelector :: Selector '[Id PHASENumberMetaParameterDefinition] ()
setGainMetaParameterDefinitionSelector = mkSelector "setGainMetaParameterDefinition:"

