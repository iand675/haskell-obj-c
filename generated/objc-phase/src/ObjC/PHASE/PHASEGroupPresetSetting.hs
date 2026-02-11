{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithGain_rate_gainCurveType_rateCurveTypeSelector
  , gainSelector
  , rateSelector
  , gainCurveTypeSelector
  , rateCurveTypeSelector

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

-- | @- init@
init_ :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO (Id PHASEGroupPresetSetting)
init_ phaseGroupPresetSetting  =
  sendMsg phaseGroupPresetSetting (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEGroupPresetSetting)
new  =
  do
    cls' <- getRequiredClass "PHASEGroupPresetSetting"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithGain_rate_gainCurveType_rateCurveType phaseGroupPresetSetting  gain rate gainCurveType rateCurveType =
  sendMsg phaseGroupPresetSetting (mkSelector "initWithGain:rate:gainCurveType:rateCurveType:") (retPtr retVoid) [argCDouble (fromIntegral gain), argCDouble (fromIntegral rate), argCLong (coerce gainCurveType), argCLong (coerce rateCurveType)] >>= ownedObject . castPtr

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO CDouble
gain phaseGroupPresetSetting  =
  sendMsg phaseGroupPresetSetting (mkSelector "gain") retCDouble []

-- | rate
--
-- Linear rate scalar.
--
-- ObjC selector: @- rate@
rate :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO CDouble
rate phaseGroupPresetSetting  =
  sendMsg phaseGroupPresetSetting (mkSelector "rate") retCDouble []

-- | gainCurveType
--
-- The type of curve to apply to the gain as the preset changes to this new setting.
--
-- ObjC selector: @- gainCurveType@
gainCurveType :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO PHASECurveType
gainCurveType phaseGroupPresetSetting  =
  fmap (coerce :: CLong -> PHASECurveType) $ sendMsg phaseGroupPresetSetting (mkSelector "gainCurveType") retCLong []

-- | rateCurveType
--
-- The type of curve to apply to the rate as the preset changes to this new setting.
--
-- ObjC selector: @- rateCurveType@
rateCurveType :: IsPHASEGroupPresetSetting phaseGroupPresetSetting => phaseGroupPresetSetting -> IO PHASECurveType
rateCurveType phaseGroupPresetSetting  =
  fmap (coerce :: CLong -> PHASECurveType) $ sendMsg phaseGroupPresetSetting (mkSelector "rateCurveType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithGain:rate:gainCurveType:rateCurveType:@
initWithGain_rate_gainCurveType_rateCurveTypeSelector :: Selector
initWithGain_rate_gainCurveType_rateCurveTypeSelector = mkSelector "initWithGain:rate:gainCurveType:rateCurveType:"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @gainCurveType@
gainCurveTypeSelector :: Selector
gainCurveTypeSelector = mkSelector "gainCurveType"

-- | @Selector@ for @rateCurveType@
rateCurveTypeSelector :: Selector
rateCurveTypeSelector = mkSelector "rateCurveType"

