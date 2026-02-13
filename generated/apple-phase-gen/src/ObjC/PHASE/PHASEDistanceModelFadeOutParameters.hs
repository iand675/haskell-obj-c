{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDistanceModelFadeOutParameters
--
-- Distance model fade out parameters.
--
-- Generated bindings for @PHASEDistanceModelFadeOutParameters@.
module ObjC.PHASE.PHASEDistanceModelFadeOutParameters
  ( PHASEDistanceModelFadeOutParameters
  , IsPHASEDistanceModelFadeOutParameters(..)
  , init_
  , new
  , initWithCullDistance
  , cullDistance
  , cullDistanceSelector
  , initSelector
  , initWithCullDistanceSelector
  , newSelector


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
init_ :: IsPHASEDistanceModelFadeOutParameters phaseDistanceModelFadeOutParameters => phaseDistanceModelFadeOutParameters -> IO (Id PHASEDistanceModelFadeOutParameters)
init_ phaseDistanceModelFadeOutParameters =
  sendOwnedMessage phaseDistanceModelFadeOutParameters initSelector

-- | @+ new@
new :: IO (Id PHASEDistanceModelFadeOutParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEDistanceModelFadeOutParameters"
    sendOwnedClassMessage cls' newSelector

-- | initWithCullDistance
--
-- Initialize with a cullDistance.
--
-- @cullDistance@ — The distance beyond which the sound will be culled. Values must be >= 1.
--
-- Note: The cullDistance is scaled by unitsPerMeter internally, so can be provided at the client's native spatial scale.        The system will smoothly fade the sound to zero before reaching this distance to avoid any audible artifacts.
--
-- Returns: An instance, or nil if initialization fails.
--
-- ObjC selector: @- initWithCullDistance:@
initWithCullDistance :: IsPHASEDistanceModelFadeOutParameters phaseDistanceModelFadeOutParameters => phaseDistanceModelFadeOutParameters -> CDouble -> IO (Id PHASEDistanceModelFadeOutParameters)
initWithCullDistance phaseDistanceModelFadeOutParameters cullDistance =
  sendOwnedMessage phaseDistanceModelFadeOutParameters initWithCullDistanceSelector cullDistance

-- | cullDistance
--
-- The distance beyond which the sound will be culled.
--
-- Note: Values are clamped the range [1, DBL_MAX].        This value is scaled by unitsPerMeter internally, so can be provided at the client's native spatial scale.
--
-- ObjC selector: @- cullDistance@
cullDistance :: IsPHASEDistanceModelFadeOutParameters phaseDistanceModelFadeOutParameters => phaseDistanceModelFadeOutParameters -> IO CDouble
cullDistance phaseDistanceModelFadeOutParameters =
  sendMessage phaseDistanceModelFadeOutParameters cullDistanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEDistanceModelFadeOutParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEDistanceModelFadeOutParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithCullDistance:@
initWithCullDistanceSelector :: Selector '[CDouble] (Id PHASEDistanceModelFadeOutParameters)
initWithCullDistanceSelector = mkSelector "initWithCullDistance:"

-- | @Selector@ for @cullDistance@
cullDistanceSelector :: Selector '[] CDouble
cullDistanceSelector = mkSelector "cullDistance"

