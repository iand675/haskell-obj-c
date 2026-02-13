{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMTremorResult
--
-- A CMTremorResult object describes the presence and prevalence of tremor symptoms (specifically, resting tremor) during a one minute result period when subjects wear the Apple Watch on their most affected arm. percentUnknown + percentNoTremor + percentTremorSlight + percentTremorMild + percentTremorModerate + percentTremorStrong = 1.0
--
-- Generated bindings for @CMTremorResult@.
module ObjC.CoreMotion.CMTremorResult
  ( CMTremorResult
  , IsCMTremorResult(..)
  , startDate
  , endDate
  , percentUnknown
  , percentNone
  , percentSlight
  , percentMild
  , percentModerate
  , percentStrong
  , endDateSelector
  , percentMildSelector
  , percentModerateSelector
  , percentNoneSelector
  , percentSlightSelector
  , percentStrongSelector
  , percentUnknownSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The date and time representing the start of the result.
--
-- ObjC selector: @- startDate@
startDate :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO (Id NSDate)
startDate cmTremorResult =
  sendMessage cmTremorResult startDateSelector

-- | The date and time representing the end of the result.
--
-- ObjC selector: @- endDate@
endDate :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO (Id NSDate)
endDate cmTremorResult =
  sendMessage cmTremorResult endDateSelector

-- | The percentage of time tremor was unknown for the result. Unknown periods include times when:   1. the subject is moving and therefore a resting tremor cannot be assessed, and   2. the signal strength is too low to measure tremor confidently.
--
-- ObjC selector: @- percentUnknown@
percentUnknown :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentUnknown cmTremorResult =
  sendMessage cmTremorResult percentUnknownSelector

-- | The percentage of time no tremor was detected for the result.
--
-- ObjC selector: @- percentNone@
percentNone :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentNone cmTremorResult =
  sendMessage cmTremorResult percentNoneSelector

-- | The percentage of time tremor was likely and displacement amplitude was slight for the result.
--
-- ObjC selector: @- percentSlight@
percentSlight :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentSlight cmTremorResult =
  sendMessage cmTremorResult percentSlightSelector

-- | The percentage of time tremor was likely and displacement amplitude was mild for the result.
--
-- ObjC selector: @- percentMild@
percentMild :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentMild cmTremorResult =
  sendMessage cmTremorResult percentMildSelector

-- | The percentage of time tremor was likely and displacement amplitude was moderate for the result.
--
-- ObjC selector: @- percentModerate@
percentModerate :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentModerate cmTremorResult =
  sendMessage cmTremorResult percentModerateSelector

-- | The percentage of time tremor was likely and displacement amplitude was strong for the result.
--
-- ObjC selector: @- percentStrong@
percentStrong :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentStrong cmTremorResult =
  sendMessage cmTremorResult percentStrongSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @percentUnknown@
percentUnknownSelector :: Selector '[] CFloat
percentUnknownSelector = mkSelector "percentUnknown"

-- | @Selector@ for @percentNone@
percentNoneSelector :: Selector '[] CFloat
percentNoneSelector = mkSelector "percentNone"

-- | @Selector@ for @percentSlight@
percentSlightSelector :: Selector '[] CFloat
percentSlightSelector = mkSelector "percentSlight"

-- | @Selector@ for @percentMild@
percentMildSelector :: Selector '[] CFloat
percentMildSelector = mkSelector "percentMild"

-- | @Selector@ for @percentModerate@
percentModerateSelector :: Selector '[] CFloat
percentModerateSelector = mkSelector "percentModerate"

-- | @Selector@ for @percentStrong@
percentStrongSelector :: Selector '[] CFloat
percentStrongSelector = mkSelector "percentStrong"

