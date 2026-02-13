{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVisionPrism
--
-- An object subclass representing prism vision fields used in eye glasses to correct double vision.                The prism aligns the two images so only one is seen.
--
-- Generated bindings for @HKVisionPrism@.
module ObjC.HealthKit.HKVisionPrism
  ( HKVisionPrism
  , IsHKVisionPrism(..)
  , initWithAmount_angle_eye
  , initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eye
  , init_
  , new
  , amount
  , angle
  , verticalAmount
  , horizontalAmount
  , verticalBase
  , horizontalBase
  , eye
  , amountSelector
  , angleSelector
  , eyeSelector
  , horizontalAmountSelector
  , horizontalBaseSelector
  , initSelector
  , initWithAmount_angle_eyeSelector
  , initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector
  , newSelector
  , verticalAmountSelector
  , verticalBaseSelector

  -- * Enum types
  , HKPrismBase(HKPrismBase)
  , pattern HKPrismBaseNone
  , pattern HKPrismBaseUp
  , pattern HKPrismBaseDown
  , pattern HKPrismBaseIn
  , pattern HKPrismBaseOut
  , HKVisionEye(HKVisionEye)
  , pattern HKVisionEyeLeft
  , pattern HKVisionEyeRight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithAmount:angle:eye
--
-- @amount@ — The compensation for amount eye misalignment
--
-- @angle@ — The angle of the lens required to correct diplopia
--
-- @eye@ — The eye associated with the prism values
--
-- ObjC selector: @- initWithAmount:angle:eye:@
initWithAmount_angle_eye :: (IsHKVisionPrism hkVisionPrism, IsHKQuantity amount, IsHKQuantity angle) => hkVisionPrism -> amount -> angle -> HKVisionEye -> IO (Id HKVisionPrism)
initWithAmount_angle_eye hkVisionPrism amount angle eye =
  sendOwnedMessage hkVisionPrism initWithAmount_angle_eyeSelector (toHKQuantity amount) (toHKQuantity angle) eye

-- | initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye
--
-- @verticalAmount@ — The vertical component of compensation in prism diopters
--
-- @verticalBase@ — The direction of the prism base relative to the vertical axis of the lens;                                    base up or base down.
--
-- @horizontalAmount@ — The horizontal component of compensation in prism diopters
--
-- @horizontalBase@ — The direction of the prism base relative to the horizontal axis of the lens;                                    base in (toward the nose) or base out (away from the nose).
--
-- @eye@ — The eye associated with the prism values
--
-- ObjC selector: @- initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:@
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eye :: (IsHKVisionPrism hkVisionPrism, IsHKQuantity verticalAmount, IsHKQuantity horizontalAmount) => hkVisionPrism -> verticalAmount -> HKPrismBase -> horizontalAmount -> HKPrismBase -> HKVisionEye -> IO (Id HKVisionPrism)
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eye hkVisionPrism verticalAmount verticalBase horizontalAmount horizontalBase eye =
  sendOwnedMessage hkVisionPrism initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector (toHKQuantity verticalAmount) verticalBase (toHKQuantity horizontalAmount) horizontalBase eye

-- | @- init@
init_ :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKVisionPrism)
init_ hkVisionPrism =
  sendOwnedMessage hkVisionPrism initSelector

-- | @+ new@
new :: IO (Id HKVisionPrism)
new  =
  do
    cls' <- getRequiredClass "HKVisionPrism"
    sendOwnedClassMessage cls' newSelector

-- | amount
--
-- The compensation in prism diopters to correct eye misalignment [polar coordinates]
--
-- ObjC selector: @- amount@
amount :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
amount hkVisionPrism =
  sendMessage hkVisionPrism amountSelector

-- | angle
--
-- The direction of the prism base [polar coordinates]
--
-- ObjC selector: @- angle@
angle :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
angle hkVisionPrism =
  sendMessage hkVisionPrism angleSelector

-- | verticalAmount
--
-- The vertical component of compensation in prism diopters [rectangular coordinates]
--
-- ObjC selector: @- verticalAmount@
verticalAmount :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
verticalAmount hkVisionPrism =
  sendMessage hkVisionPrism verticalAmountSelector

-- | horizontalAmount
--
-- The horizontal component of compensation in prism diopters [rectangular coordinates]
--
-- ObjC selector: @- horizontalAmount@
horizontalAmount :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
horizontalAmount hkVisionPrism =
  sendMessage hkVisionPrism horizontalAmountSelector

-- | verticalBase
--
-- The direction of the prism base relative to the vertical axis of the lens;                base up or base down. [rectangular coordinates]
--
-- ObjC selector: @- verticalBase@
verticalBase :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO HKPrismBase
verticalBase hkVisionPrism =
  sendMessage hkVisionPrism verticalBaseSelector

-- | horizontalBase
--
-- The direction of the prism base relative to the horizontal axis of the lens;                base in (toward the nose) or base out (away from the nose). [rectangular coordinates]
--
-- ObjC selector: @- horizontalBase@
horizontalBase :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO HKPrismBase
horizontalBase hkVisionPrism =
  sendMessage hkVisionPrism horizontalBaseSelector

-- | eye
--
-- Which eye (left or right)
--
-- ObjC selector: @- eye@
eye :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO HKVisionEye
eye hkVisionPrism =
  sendMessage hkVisionPrism eyeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAmount:angle:eye:@
initWithAmount_angle_eyeSelector :: Selector '[Id HKQuantity, Id HKQuantity, HKVisionEye] (Id HKVisionPrism)
initWithAmount_angle_eyeSelector = mkSelector "initWithAmount:angle:eye:"

-- | @Selector@ for @initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:@
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector :: Selector '[Id HKQuantity, HKPrismBase, Id HKQuantity, HKPrismBase, HKVisionEye] (Id HKVisionPrism)
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector = mkSelector "initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKVisionPrism)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKVisionPrism)
newSelector = mkSelector "new"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id HKQuantity)
amountSelector = mkSelector "amount"

-- | @Selector@ for @angle@
angleSelector :: Selector '[] (Id HKQuantity)
angleSelector = mkSelector "angle"

-- | @Selector@ for @verticalAmount@
verticalAmountSelector :: Selector '[] (Id HKQuantity)
verticalAmountSelector = mkSelector "verticalAmount"

-- | @Selector@ for @horizontalAmount@
horizontalAmountSelector :: Selector '[] (Id HKQuantity)
horizontalAmountSelector = mkSelector "horizontalAmount"

-- | @Selector@ for @verticalBase@
verticalBaseSelector :: Selector '[] HKPrismBase
verticalBaseSelector = mkSelector "verticalBase"

-- | @Selector@ for @horizontalBase@
horizontalBaseSelector :: Selector '[] HKPrismBase
horizontalBaseSelector = mkSelector "horizontalBase"

-- | @Selector@ for @eye@
eyeSelector :: Selector '[] HKVisionEye
eyeSelector = mkSelector "eye"

