{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMaterial
--
-- A PHASEMaterial describes the acoustic properties of a material.
--
-- Generated bindings for @PHASEMaterial@.
module ObjC.PHASE.PHASEMaterial
  ( PHASEMaterial
  , IsPHASEMaterial(..)
  , init_
  , new
  , initWithEngine_preset
  , initSelector
  , initWithEngine_presetSelector
  , newSelector

  -- * Enum types
  , PHASEMaterialPreset(PHASEMaterialPreset)
  , pattern PHASEMaterialPresetCardboard
  , pattern PHASEMaterialPresetGlass
  , pattern PHASEMaterialPresetBrick
  , pattern PHASEMaterialPresetConcrete
  , pattern PHASEMaterialPresetDrywall
  , pattern PHASEMaterialPresetWood

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
init_ :: IsPHASEMaterial phaseMaterial => phaseMaterial -> IO (Id PHASEMaterial)
init_ phaseMaterial =
  sendOwnedMessage phaseMaterial initSelector

-- | @+ new@
new :: IO (Id PHASEMaterial)
new  =
  do
    cls' <- getRequiredClass "PHASEMaterial"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:preset
--
-- Initialize a new material from a preset.
--
-- ObjC selector: @- initWithEngine:preset:@
initWithEngine_preset :: (IsPHASEMaterial phaseMaterial, IsPHASEEngine engine) => phaseMaterial -> engine -> PHASEMaterialPreset -> IO (Id PHASEMaterial)
initWithEngine_preset phaseMaterial engine preset =
  sendOwnedMessage phaseMaterial initWithEngine_presetSelector (toPHASEEngine engine) preset

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMaterial)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMaterial)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:preset:@
initWithEngine_presetSelector :: Selector '[Id PHASEEngine, PHASEMaterialPreset] (Id PHASEMaterial)
initWithEngine_presetSelector = mkSelector "initWithEngine:preset:"

