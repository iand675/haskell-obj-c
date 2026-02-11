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
  , initSelector
  , newSelector
  , initWithCullDistanceSelector
  , cullDistanceSelector


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
init_ :: IsPHASEDistanceModelFadeOutParameters phaseDistanceModelFadeOutParameters => phaseDistanceModelFadeOutParameters -> IO (Id PHASEDistanceModelFadeOutParameters)
init_ phaseDistanceModelFadeOutParameters  =
  sendMsg phaseDistanceModelFadeOutParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEDistanceModelFadeOutParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEDistanceModelFadeOutParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithCullDistance phaseDistanceModelFadeOutParameters  cullDistance =
  sendMsg phaseDistanceModelFadeOutParameters (mkSelector "initWithCullDistance:") (retPtr retVoid) [argCDouble (fromIntegral cullDistance)] >>= ownedObject . castPtr

-- | cullDistance
--
-- The distance beyond which the sound will be culled.
--
-- Note: Values are clamped the range [1, DBL_MAX].        This value is scaled by unitsPerMeter internally, so can be provided at the client's native spatial scale.
--
-- ObjC selector: @- cullDistance@
cullDistance :: IsPHASEDistanceModelFadeOutParameters phaseDistanceModelFadeOutParameters => phaseDistanceModelFadeOutParameters -> IO CDouble
cullDistance phaseDistanceModelFadeOutParameters  =
  sendMsg phaseDistanceModelFadeOutParameters (mkSelector "cullDistance") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithCullDistance:@
initWithCullDistanceSelector :: Selector
initWithCullDistanceSelector = mkSelector "initWithCullDistance:"

-- | @Selector@ for @cullDistance@
cullDistanceSelector :: Selector
cullDistanceSelector = mkSelector "cullDistance"

