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
  , startDateSelector
  , endDateSelector
  , percentUnknownSelector
  , percentNoneSelector
  , percentSlightSelector
  , percentMildSelector
  , percentModerateSelector
  , percentStrongSelector


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

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The date and time representing the start of the result.
--
-- ObjC selector: @- startDate@
startDate :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO (Id NSDate)
startDate cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time representing the end of the result.
--
-- ObjC selector: @- endDate@
endDate :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO (Id NSDate)
endDate cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The percentage of time tremor was unknown for the result. Unknown periods include times when:   1. the subject is moving and therefore a resting tremor cannot be assessed, and   2. the signal strength is too low to measure tremor confidently.
--
-- ObjC selector: @- percentUnknown@
percentUnknown :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentUnknown cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "percentUnknown") retCFloat []

-- | The percentage of time no tremor was detected for the result.
--
-- ObjC selector: @- percentNone@
percentNone :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentNone cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "percentNone") retCFloat []

-- | The percentage of time tremor was likely and displacement amplitude was slight for the result.
--
-- ObjC selector: @- percentSlight@
percentSlight :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentSlight cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "percentSlight") retCFloat []

-- | The percentage of time tremor was likely and displacement amplitude was mild for the result.
--
-- ObjC selector: @- percentMild@
percentMild :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentMild cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "percentMild") retCFloat []

-- | The percentage of time tremor was likely and displacement amplitude was moderate for the result.
--
-- ObjC selector: @- percentModerate@
percentModerate :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentModerate cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "percentModerate") retCFloat []

-- | The percentage of time tremor was likely and displacement amplitude was strong for the result.
--
-- ObjC selector: @- percentStrong@
percentStrong :: IsCMTremorResult cmTremorResult => cmTremorResult -> IO CFloat
percentStrong cmTremorResult  =
  sendMsg cmTremorResult (mkSelector "percentStrong") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @percentUnknown@
percentUnknownSelector :: Selector
percentUnknownSelector = mkSelector "percentUnknown"

-- | @Selector@ for @percentNone@
percentNoneSelector :: Selector
percentNoneSelector = mkSelector "percentNone"

-- | @Selector@ for @percentSlight@
percentSlightSelector :: Selector
percentSlightSelector = mkSelector "percentSlight"

-- | @Selector@ for @percentMild@
percentMildSelector :: Selector
percentMildSelector = mkSelector "percentMild"

-- | @Selector@ for @percentModerate@
percentModerateSelector :: Selector
percentModerateSelector = mkSelector "percentModerate"

-- | @Selector@ for @percentStrong@
percentStrongSelector :: Selector
percentStrongSelector = mkSelector "percentStrong"

