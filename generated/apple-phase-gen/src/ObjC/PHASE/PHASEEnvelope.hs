{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEEnvelope
--
-- A segmented envelope.
--
-- Generated bindings for @PHASEEnvelope@.
module ObjC.PHASE.PHASEEnvelope
  ( PHASEEnvelope
  , IsPHASEEnvelope(..)
  , init_
  , new
  , evaluateForValue
  , segments
  , domain
  , range
  , domainSelector
  , evaluateForValueSelector
  , initSelector
  , newSelector
  , rangeSelector
  , segmentsSelector


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
init_ :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id PHASEEnvelope)
init_ phaseEnvelope =
  sendOwnedMessage phaseEnvelope initSelector

-- | @+ new@
new :: IO (Id PHASEEnvelope)
new  =
  do
    cls' <- getRequiredClass "PHASEEnvelope"
    sendOwnedClassMessage cls' newSelector

-- | evaluateForValue
--
-- Evaluates the envelope.
--
-- If required, x will be clamped to the envelope's domain.
--
-- @x@ — The input along the x-axis.
--
-- Returns: The output along the y-axis.
--
-- ObjC selector: @- evaluateForValue:@
evaluateForValue :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> CDouble -> IO CDouble
evaluateForValue phaseEnvelope x =
  sendMessage phaseEnvelope evaluateForValueSelector x

-- | segments
--
-- The segments of the envelope.
--
-- ObjC selector: @- segments@
segments :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id NSArray)
segments phaseEnvelope =
  sendMessage phaseEnvelope segmentsSelector

-- | domain
--
-- The domain (along the x-axis).
--
-- The first value in the pair is the minimum value of the domain.        The second value in the pair is the maximum value of the domain.
--
-- ObjC selector: @- domain@
domain :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id PHASENumericPair)
domain phaseEnvelope =
  sendMessage phaseEnvelope domainSelector

-- | range
--
-- The range (along the y-axis).
--
-- The first value in the pair is the minimum value of the range.        The second value in the pair is the maximum value of the range.
--
-- ObjC selector: @- range@
range :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id PHASENumericPair)
range phaseEnvelope =
  sendMessage phaseEnvelope rangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEEnvelope)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEEnvelope)
newSelector = mkSelector "new"

-- | @Selector@ for @evaluateForValue:@
evaluateForValueSelector :: Selector '[CDouble] CDouble
evaluateForValueSelector = mkSelector "evaluateForValue:"

-- | @Selector@ for @segments@
segmentsSelector :: Selector '[] (Id NSArray)
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id PHASENumericPair)
domainSelector = mkSelector "domain"

-- | @Selector@ for @range@
rangeSelector :: Selector '[] (Id PHASENumericPair)
rangeSelector = mkSelector "range"

