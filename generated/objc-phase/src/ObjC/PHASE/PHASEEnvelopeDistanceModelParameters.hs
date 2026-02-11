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
  , initSelector
  , newSelector
  , initWithEnvelopeSelector
  , envelopeSelector


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
init_ :: IsPHASEEnvelopeDistanceModelParameters phaseEnvelopeDistanceModelParameters => phaseEnvelopeDistanceModelParameters -> IO (Id PHASEEnvelopeDistanceModelParameters)
init_ phaseEnvelopeDistanceModelParameters  =
  sendMsg phaseEnvelopeDistanceModelParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEEnvelopeDistanceModelParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEEnvelopeDistanceModelParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithEnvelope phaseEnvelopeDistanceModelParameters  envelope =
withObjCPtr envelope $ \raw_envelope ->
    sendMsg phaseEnvelopeDistanceModelParameters (mkSelector "initWithEnvelope:") (retPtr retVoid) [argPtr (castPtr raw_envelope :: Ptr ())] >>= ownedObject . castPtr

-- | envelope
--
-- A PHASEEnvelope object
--
-- Note: The x values are interpreted as distance and the y values are interpreted as gain.
--
-- ObjC selector: @- envelope@
envelope :: IsPHASEEnvelopeDistanceModelParameters phaseEnvelopeDistanceModelParameters => phaseEnvelopeDistanceModelParameters -> IO (Id PHASEEnvelope)
envelope phaseEnvelopeDistanceModelParameters  =
  sendMsg phaseEnvelopeDistanceModelParameters (mkSelector "envelope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEnvelope:@
initWithEnvelopeSelector :: Selector
initWithEnvelopeSelector = mkSelector "initWithEnvelope:"

-- | @Selector@ for @envelope@
envelopeSelector :: Selector
envelopeSelector = mkSelector "envelope"

