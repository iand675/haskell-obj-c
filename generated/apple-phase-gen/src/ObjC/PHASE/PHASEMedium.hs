{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMedium
--
-- A PHASEMedium describes the acoustic properties of a medium.
--
-- Generated bindings for @PHASEMedium@.
module ObjC.PHASE.PHASEMedium
  ( PHASEMedium
  , IsPHASEMedium(..)
  , init_
  , new
  , initWithEngine_preset
  , initSelector
  , initWithEngine_presetSelector
  , newSelector

  -- * Enum types
  , PHASEMediumPreset(PHASEMediumPreset)
  , pattern PHASEMediumPresetAir

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEMedium phaseMedium => phaseMedium -> IO (Id PHASEMedium)
init_ phaseMedium =
  sendOwnedMessage phaseMedium initSelector

-- | @+ new@
new :: IO (Id PHASEMedium)
new  =
  do
    cls' <- getRequiredClass "PHASEMedium"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:preset
--
-- Initialize a new medium from a preset.
--
-- ObjC selector: @- initWithEngine:preset:@
initWithEngine_preset :: (IsPHASEMedium phaseMedium, IsPHASEEngine engine) => phaseMedium -> engine -> PHASEMediumPreset -> IO (Id PHASEMedium)
initWithEngine_preset phaseMedium engine preset =
  sendOwnedMessage phaseMedium initWithEngine_presetSelector (toPHASEEngine engine) preset

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMedium)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMedium)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:preset:@
initWithEngine_presetSelector :: Selector '[Id PHASEEngine, PHASEMediumPreset] (Id PHASEMedium)
initWithEngine_presetSelector = mkSelector "initWithEngine:preset:"

