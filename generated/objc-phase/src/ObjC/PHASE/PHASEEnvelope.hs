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
  , initSelector
  , newSelector
  , evaluateForValueSelector
  , segmentsSelector
  , domainSelector
  , rangeSelector


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
init_ :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id PHASEEnvelope)
init_ phaseEnvelope  =
  sendMsg phaseEnvelope (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEEnvelope)
new  =
  do
    cls' <- getRequiredClass "PHASEEnvelope"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
evaluateForValue phaseEnvelope  x =
  sendMsg phaseEnvelope (mkSelector "evaluateForValue:") retCDouble [argCDouble (fromIntegral x)]

-- | segments
--
-- The segments of the envelope.
--
-- ObjC selector: @- segments@
segments :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id NSArray)
segments phaseEnvelope  =
  sendMsg phaseEnvelope (mkSelector "segments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | domain
--
-- The domain (along the x-axis).
--
-- The first value in the pair is the minimum value of the domain.        The second value in the pair is the maximum value of the domain.
--
-- ObjC selector: @- domain@
domain :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id PHASENumericPair)
domain phaseEnvelope  =
  sendMsg phaseEnvelope (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | range
--
-- The range (along the y-axis).
--
-- The first value in the pair is the minimum value of the range.        The second value in the pair is the maximum value of the range.
--
-- ObjC selector: @- range@
range :: IsPHASEEnvelope phaseEnvelope => phaseEnvelope -> IO (Id PHASENumericPair)
range phaseEnvelope  =
  sendMsg phaseEnvelope (mkSelector "range") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @evaluateForValue:@
evaluateForValueSelector :: Selector
evaluateForValueSelector = mkSelector "evaluateForValue:"

-- | @Selector@ for @segments@
segmentsSelector :: Selector
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

-- | @Selector@ for @range@
rangeSelector :: Selector
rangeSelector = mkSelector "range"

