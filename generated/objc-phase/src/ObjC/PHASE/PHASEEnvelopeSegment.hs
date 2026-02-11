{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEEnvelopeSegment
--
-- An envelope segment defined by an end point and a curve type.
--
-- Envelope segments do 'not' contain a start point.        We do this so we can connect envelope segments together end to end and gaurantee continuity along the x and y axes.
--
-- Generated bindings for @PHASEEnvelopeSegment@.
module ObjC.PHASE.PHASEEnvelopeSegment
  ( PHASEEnvelopeSegment
  , IsPHASEEnvelopeSegment(..)
  , curveType
  , setCurveType
  , curveTypeSelector
  , setCurveTypeSelector

  -- * Enum types
  , PHASECurveType(PHASECurveType)
  , pattern PHASECurveTypeLinear
  , pattern PHASECurveTypeSquared
  , pattern PHASECurveTypeInverseSquared
  , pattern PHASECurveTypeCubed
  , pattern PHASECurveTypeInverseCubed
  , pattern PHASECurveTypeSine
  , pattern PHASECurveTypeInverseSine
  , pattern PHASECurveTypeSigmoid
  , pattern PHASECurveTypeInverseSigmoid
  , pattern PHASECurveTypeHoldStartValue
  , pattern PHASECurveTypeJumpToEndValue

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

-- | curveType
--
-- The curve type of the envelope segment.
--
-- The default value is PHASECurveTypeLinear.
--
-- ObjC selector: @- curveType@
curveType :: IsPHASEEnvelopeSegment phaseEnvelopeSegment => phaseEnvelopeSegment -> IO PHASECurveType
curveType phaseEnvelopeSegment  =
  fmap (coerce :: CLong -> PHASECurveType) $ sendMsg phaseEnvelopeSegment (mkSelector "curveType") retCLong []

-- | curveType
--
-- The curve type of the envelope segment.
--
-- The default value is PHASECurveTypeLinear.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsPHASEEnvelopeSegment phaseEnvelopeSegment => phaseEnvelopeSegment -> PHASECurveType -> IO ()
setCurveType phaseEnvelopeSegment  value =
  sendMsg phaseEnvelopeSegment (mkSelector "setCurveType:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @curveType@
curveTypeSelector :: Selector
curveTypeSelector = mkSelector "curveType"

-- | @Selector@ for @setCurveType:@
setCurveTypeSelector :: Selector
setCurveTypeSelector = mkSelector "setCurveType:"

