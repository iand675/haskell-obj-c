{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initWithEngine_presetSelector

  -- * Enum types
  , PHASEMaterialPreset(PHASEMaterialPreset)
  , pattern PHASEMaterialPresetCardboard
  , pattern PHASEMaterialPresetGlass
  , pattern PHASEMaterialPresetBrick
  , pattern PHASEMaterialPresetConcrete
  , pattern PHASEMaterialPresetDrywall
  , pattern PHASEMaterialPresetWood

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
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEMaterial phaseMaterial => phaseMaterial -> IO (Id PHASEMaterial)
init_ phaseMaterial  =
  sendMsg phaseMaterial (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMaterial)
new  =
  do
    cls' <- getRequiredClass "PHASEMaterial"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithEngine:preset
--
-- Initialize a new material from a preset.
--
-- ObjC selector: @- initWithEngine:preset:@
initWithEngine_preset :: (IsPHASEMaterial phaseMaterial, IsPHASEEngine engine) => phaseMaterial -> engine -> PHASEMaterialPreset -> IO (Id PHASEMaterial)
initWithEngine_preset phaseMaterial  engine preset =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseMaterial (mkSelector "initWithEngine:preset:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argCLong (coerce preset)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:preset:@
initWithEngine_presetSelector :: Selector
initWithEngine_presetSelector = mkSelector "initWithEngine:preset:"

