{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESpatialPipeline
--
-- Spatial Pipeline.
--
-- Generated bindings for @PHASESpatialPipeline@.
module ObjC.PHASE.PHASESpatialPipeline
  ( PHASESpatialPipeline
  , IsPHASESpatialPipeline(..)
  , init_
  , new
  , initWithFlags
  , flags
  , entries
  , entriesSelector
  , flagsSelector
  , initSelector
  , initWithFlagsSelector
  , newSelector

  -- * Enum types
  , PHASESpatialPipelineFlags(PHASESpatialPipelineFlags)
  , pattern PHASESpatialPipelineFlagDirectPathTransmission
  , pattern PHASESpatialPipelineFlagEarlyReflections
  , pattern PHASESpatialPipelineFlagLateReverb

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
init_ :: IsPHASESpatialPipeline phaseSpatialPipeline => phaseSpatialPipeline -> IO (Id PHASESpatialPipeline)
init_ phaseSpatialPipeline =
  sendOwnedMessage phaseSpatialPipeline initSelector

-- | @+ new@
new :: IO (Id PHASESpatialPipeline)
new  =
  do
    cls' <- getRequiredClass "PHASESpatialPipeline"
    sendOwnedClassMessage cls' newSelector

-- | initWithFlags
--
-- Initialize a Spatial Pipeline with the provided flags.
--
-- It's invalid to pass flags == 0 to this function. Doing so will return nil.
--
-- @flags@ — Options for direct path transmission, early reflections, late reverb, etc.
--
-- ObjC selector: @- initWithFlags:@
initWithFlags :: IsPHASESpatialPipeline phaseSpatialPipeline => phaseSpatialPipeline -> PHASESpatialPipelineFlags -> IO (Id PHASESpatialPipeline)
initWithFlags phaseSpatialPipeline flags =
  sendOwnedMessage phaseSpatialPipeline initWithFlagsSelector flags

-- | flags
--
-- Spatial Pipeline Flags.
--
-- ObjC selector: @- flags@
flags :: IsPHASESpatialPipeline phaseSpatialPipeline => phaseSpatialPipeline -> IO PHASESpatialPipelineFlags
flags phaseSpatialPipeline =
  sendMessage phaseSpatialPipeline flagsSelector

-- | entries
--
-- A dictionary of entries in the Spatial Pipeline.
--
-- Upon initialization, an entry will be created for every flag in the PHASESpatialPipelineFlags passed to PHASESpatialPipeline:initWithFlags.
--
-- ObjC selector: @- entries@
entries :: IsPHASESpatialPipeline phaseSpatialPipeline => phaseSpatialPipeline -> IO (Id NSDictionary)
entries phaseSpatialPipeline =
  sendMessage phaseSpatialPipeline entriesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESpatialPipeline)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESpatialPipeline)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithFlags:@
initWithFlagsSelector :: Selector '[PHASESpatialPipelineFlags] (Id PHASESpatialPipeline)
initWithFlagsSelector = mkSelector "initWithFlags:"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] PHASESpatialPipelineFlags
flagsSelector = mkSelector "flags"

-- | @Selector@ for @entries@
entriesSelector :: Selector '[] (Id NSDictionary)
entriesSelector = mkSelector "entries"

