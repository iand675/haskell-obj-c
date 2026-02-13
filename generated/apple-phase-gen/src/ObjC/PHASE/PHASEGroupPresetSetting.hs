{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEGroupPresetSetting
--
-- A PHASEGroupPresetSetting is an object that holds settings that can be applied to a PHASEGroup object.
--
-- These can be either be manually created and added to a PHASEGroupPreset object, or created inline using PHASEGroupPreset addGroup.
--
-- Generated bindings for @PHASEGroupPresetSetting@.
module ObjC.PHASE.PHASEGroupPresetSetting
  ( PHASEGroupPresetSetting
  , IsPHASEGroupPresetSetting(..)
  , init_
  , new
  , initWithGain_rate_gainCurveType_rateCurveType
  , gain
  , rate
  , gainCurveType
  , rateCurveType
  , gainCurveTypeSelector
  , gainSelector
  , initSelector
  , initWithGain_rate_gainCurveType_rateCurveTypeSelector
  , newSelector
  , rateCurveTypeSelector
  , rateSelector

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

-- | @- init@
init_ :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO (Id PHASEGroupPresetSetting)
init_ phaseGroupPresetSetting =
  sendOwnedMessage phaseGroupPresetSetting initSelector

-- | @+ new@
new :: IO (Id PHASEGroupPresetSetting)
new  =
  do
    cls' <- getRequiredClass "PHASEGroupPresetSetting"
    sendOwnedClassMessage cls' newSelector

-- | initWithGain:rate:gainCurveType:rateCurveType
--
-- Initialize the PHASEGroupPresetSetting object with an existing PHASEGroup object.
--
-- @gain@ — The main gain setting to apply to the group. Values are clamped to the range [0, 1]. Default value is 1.
--
-- @rate@ — The playback rate setting to apply to the group. Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- @gainCurveType@ — The type of curve to apply to the gain as the preset changes to this new setting.
--
-- @rateCurveType@ — The type of curve to apply to the rate as the preset changes to this new setting.
--
-- ObjC selector: @- initWithGain:rate:gainCurveType:rateCurveType:@
initWithGain_rate_gainCurveType_rateCurveType :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> CDouble -> CDouble -> PHASECurveType -> PHASECurveType -> IO (Id PHASEGroupPresetSetting)
initWithGain_rate_gainCurveType_rateCurveType phaseGroupPresetSetting gain rate gainCurveType rateCurveType =
  sendOwnedMessage phaseGroupPresetSetting initWithGain_rate_gainCurveType_rateCurveTypeSelector gain rate gainCurveType rateCurveType

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO CDouble
gain phaseGroupPresetSetting =
  sendMessage phaseGroupPresetSetting gainSelector

-- | rate
--
-- Linear rate scalar.
--
-- ObjC selector: @- rate@
rate :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO CDouble
rate phaseGroupPresetSetting =
  sendMessage phaseGroupPresetSetting rateSelector

-- | gainCurveType
--
-- The type of curve to apply to the gain as the preset changes to this new setting.
--
-- ObjC selector: @- gainCurveType@
gainCurveType :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO PHASECurveType
gainCurveType phaseGroupPresetSetting =
  sendMessage phaseGroupPresetSetting gainCurveTypeSelector

-- | rateCurveType
--
-- The type of curve to apply to the rate as the preset changes to this new setting.
--
-- ObjC selector: @- rateCurveType@
rateCurveType :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO PHASECurveType
rateCurveType phaseGroupPresetSetting =
  sendMessage phaseGroupPresetSetting rateCurveTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEGroupPresetSetting)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEGroupPresetSetting)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithGain:rate:gainCurveType:rateCurveType:@
initWithGain_rate_gainCurveType_rateCurveTypeSelector :: Selector '[CDouble, CDouble, PHASECurveType, PHASECurveType] (Id PHASEGroupPresetSetting)
initWithGain_rate_gainCurveType_rateCurveTypeSelector = mkSelector "initWithGain:rate:gainCurveType:rateCurveType:"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CDouble
gainSelector = mkSelector "gain"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CDouble
rateSelector = mkSelector "rate"

-- | @Selector@ for @gainCurveType@
gainCurveTypeSelector :: Selector '[] PHASECurveType
gainCurveTypeSelector = mkSelector "gainCurveType"

-- | @Selector@ for @rateCurveType@
rateCurveTypeSelector :: Selector '[] PHASECurveType
rateCurveTypeSelector = mkSelector "rateCurveType"

