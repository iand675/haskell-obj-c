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
  , initSelector
  , newSelector
  , gainSelector
  , setGainSelector
  , gainMetaParameterDefinitionSelector
  , setGainMetaParameterDefinitionSelector


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
init_ :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> IO (Id PHASEMixerDefinition)
init_ phaseMixerDefinition  =
  sendMsg phaseMixerDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEMixerDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> IO CDouble
gain phaseMixerDefinition  =
  sendMsg phaseMixerDefinition (mkSelector "gain") retCDouble []

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> CDouble -> IO ()
setGain phaseMixerDefinition  value =
  sendMsg phaseMixerDefinition (mkSelector "setGain:") retVoid [argCDouble (fromIntegral value)]

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable real-time control of the gain during playback.
--
-- ObjC selector: @- gainMetaParameterDefinition@
gainMetaParameterDefinition :: IsPHASEMixerDefinition phaseMixerDefinition => phaseMixerDefinition -> IO (Id PHASENumberMetaParameterDefinition)
gainMetaParameterDefinition phaseMixerDefinition  =
  sendMsg phaseMixerDefinition (mkSelector "gainMetaParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gainMetaParameterDefinition
--
-- Optionally attach a metaparameter definition here to enable real-time control of the gain during playback.
--
-- ObjC selector: @- setGainMetaParameterDefinition:@
setGainMetaParameterDefinition :: (IsPHASEMixerDefinition phaseMixerDefinition, IsPHASENumberMetaParameterDefinition value) => phaseMixerDefinition -> value -> IO ()
setGainMetaParameterDefinition phaseMixerDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseMixerDefinition (mkSelector "setGainMetaParameterDefinition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @gainMetaParameterDefinition@
gainMetaParameterDefinitionSelector :: Selector
gainMetaParameterDefinitionSelector = mkSelector "gainMetaParameterDefinition"

-- | @Selector@ for @setGainMetaParameterDefinition:@
setGainMetaParameterDefinitionSelector :: Selector
setGainMetaParameterDefinitionSelector = mkSelector "setGainMetaParameterDefinition:"

