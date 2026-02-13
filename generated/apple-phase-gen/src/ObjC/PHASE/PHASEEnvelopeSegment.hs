{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
curveType phaseEnvelopeSegment =
  sendMessage phaseEnvelopeSegment curveTypeSelector

-- | curveType
--
-- The curve type of the envelope segment.
--
-- The default value is PHASECurveTypeLinear.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsPHASEEnvelopeSegment phaseEnvelopeSegment => phaseEnvelopeSegment -> PHASECurveType -> IO ()
setCurveType phaseEnvelopeSegment value =
  sendMessage phaseEnvelopeSegment setCurveTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @curveType@
curveTypeSelector :: Selector '[] PHASECurveType
curveTypeSelector = mkSelector "curveType"

-- | @Selector@ for @setCurveType:@
setCurveTypeSelector :: Selector '[PHASECurveType] ()
setCurveTypeSelector = mkSelector "setCurveType:"

