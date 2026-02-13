{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDistanceModelParameters
--
-- Distance model parameters.
--
-- Generated bindings for @PHASEDistanceModelParameters@.
module ObjC.PHASE.PHASEDistanceModelParameters
  ( PHASEDistanceModelParameters
  , IsPHASEDistanceModelParameters(..)
  , init_
  , new
  , fadeOutParameters
  , setFadeOutParameters
  , fadeOutParametersSelector
  , initSelector
  , newSelector
  , setFadeOutParametersSelector


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
init_ :: IsPHASEDistanceModelParameters phaseDistanceModelParameters => phaseDistanceModelParameters -> IO (Id PHASEDistanceModelParameters)
init_ phaseDistanceModelParameters =
  sendOwnedMessage phaseDistanceModelParameters initSelector

-- | @+ new@
new :: IO (Id PHASEDistanceModelParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEDistanceModelParameters"
    sendOwnedClassMessage cls' newSelector

-- | fadeOutParameters
--
-- Fade out parameters (optional).
--
-- ObjC selector: @- fadeOutParameters@
fadeOutParameters :: IsPHASEDistanceModelParameters phaseDistanceModelParameters => phaseDistanceModelParameters -> IO (Id PHASEDistanceModelFadeOutParameters)
fadeOutParameters phaseDistanceModelParameters =
  sendMessage phaseDistanceModelParameters fadeOutParametersSelector

-- | fadeOutParameters
--
-- Fade out parameters (optional).
--
-- ObjC selector: @- setFadeOutParameters:@
setFadeOutParameters :: (IsPHASEDistanceModelParameters phaseDistanceModelParameters, IsPHASEDistanceModelFadeOutParameters value) => phaseDistanceModelParameters -> value -> IO ()
setFadeOutParameters phaseDistanceModelParameters value =
  sendMessage phaseDistanceModelParameters setFadeOutParametersSelector (toPHASEDistanceModelFadeOutParameters value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEDistanceModelParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEDistanceModelParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @fadeOutParameters@
fadeOutParametersSelector :: Selector '[] (Id PHASEDistanceModelFadeOutParameters)
fadeOutParametersSelector = mkSelector "fadeOutParameters"

-- | @Selector@ for @setFadeOutParameters:@
setFadeOutParametersSelector :: Selector '[Id PHASEDistanceModelFadeOutParameters] ()
setFadeOutParametersSelector = mkSelector "setFadeOutParameters:"

