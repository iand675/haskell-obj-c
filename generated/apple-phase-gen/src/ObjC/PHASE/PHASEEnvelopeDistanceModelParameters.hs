{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEEnvelopeDistanceModelParameters
--
-- Envelope distance model parameters.
--
-- Envelope-driven attenuation over distance.
--
-- Generated bindings for @PHASEEnvelopeDistanceModelParameters@.
module ObjC.PHASE.PHASEEnvelopeDistanceModelParameters
  ( PHASEEnvelopeDistanceModelParameters
  , IsPHASEEnvelopeDistanceModelParameters(..)
  , init_
  , new
  , initWithEnvelope
  , envelope
  , envelopeSelector
  , initSelector
  , initWithEnvelopeSelector
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
init_ :: IsPHASEEnvelopeDistanceModelParameters phaseEnvelopeDistanceModelParameters => phaseEnvelopeDistanceModelParameters -> IO (Id PHASEEnvelopeDistanceModelParameters)
init_ phaseEnvelopeDistanceModelParameters =
  sendOwnedMessage phaseEnvelopeDistanceModelParameters initSelector

-- | @+ new@
new :: IO (Id PHASEEnvelopeDistanceModelParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEEnvelopeDistanceModelParameters"
    sendOwnedClassMessage cls' newSelector

-- | initWithEnvelope
--
-- Initialize a PHASEEnvelopeDistanceModelParameters with a PHASEEnvelope
--
-- @envelope@ — An envelope object where x values are interpreted as distance and the y values interpreted as gain.
--
-- Returns: An instance, or nil if initialization failed.
--
-- ObjC selector: @- initWithEnvelope:@
initWithEnvelope :: (IsPHASEEnvelopeDistanceModelParameters phaseEnvelopeDistanceModelParameters, IsPHASEEnvelope envelope) => phaseEnvelopeDistanceModelParameters -> envelope -> IO (Id PHASEEnvelopeDistanceModelParameters)
initWithEnvelope phaseEnvelopeDistanceModelParameters envelope =
  sendOwnedMessage phaseEnvelopeDistanceModelParameters initWithEnvelopeSelector (toPHASEEnvelope envelope)

-- | envelope
--
-- A PHASEEnvelope object
--
-- Note: The x values are interpreted as distance and the y values are interpreted as gain.
--
-- ObjC selector: @- envelope@
envelope :: IsPHASEEnvelopeDistanceModelParameters phaseEnvelopeDistanceModelParameters => phaseEnvelopeDistanceModelParameters -> IO (Id PHASEEnvelope)
envelope phaseEnvelopeDistanceModelParameters =
  sendMessage phaseEnvelopeDistanceModelParameters envelopeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEEnvelopeDistanceModelParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEEnvelopeDistanceModelParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEnvelope:@
initWithEnvelopeSelector :: Selector '[Id PHASEEnvelope] (Id PHASEEnvelopeDistanceModelParameters)
initWithEnvelopeSelector = mkSelector "initWithEnvelope:"

-- | @Selector@ for @envelope@
envelopeSelector :: Selector '[] (Id PHASEEnvelope)
envelopeSelector = mkSelector "envelope"

