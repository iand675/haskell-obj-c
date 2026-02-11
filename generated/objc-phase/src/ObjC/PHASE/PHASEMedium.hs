{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initWithEngine_presetSelector

  -- * Enum types
  , PHASEMediumPreset(PHASEMediumPreset)
  , pattern PHASEMediumPresetAir

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
init_ :: IsPHASEMedium phaseMedium => phaseMedium -> IO (Id PHASEMedium)
init_ phaseMedium  =
  sendMsg phaseMedium (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMedium)
new  =
  do
    cls' <- getRequiredClass "PHASEMedium"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithEngine:preset
--
-- Initialize a new medium from a preset.
--
-- ObjC selector: @- initWithEngine:preset:@
initWithEngine_preset :: (IsPHASEMedium phaseMedium, IsPHASEEngine engine) => phaseMedium -> engine -> PHASEMediumPreset -> IO (Id PHASEMedium)
initWithEngine_preset phaseMedium  engine preset =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseMedium (mkSelector "initWithEngine:preset:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argCLong (coerce preset)] >>= ownedObject . castPtr

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

