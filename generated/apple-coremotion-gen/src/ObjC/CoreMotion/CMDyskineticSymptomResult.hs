{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMDyskineticSymptomResult
--
-- A CMDyskineticSymptomResult object describes the presence and prevalence of dyskinetic symptoms (specifically, choreiform movements) during a one minute result period when subjects wear the Apple Watch on their most affected arm. percentUnlikely + percentLikely = 1.0 Please note dyskinetic symptom measurements are designed for subjects with known presence of chorea in the arm and should not be displayed to users who do not report episodes of dyskinetic symptoms.
--
-- Generated bindings for @CMDyskineticSymptomResult@.
module ObjC.CoreMotion.CMDyskineticSymptomResult
  ( CMDyskineticSymptomResult
  , IsCMDyskineticSymptomResult(..)
  , startDate
  , endDate
  , percentUnlikely
  , percentLikely
  , endDateSelector
  , percentLikelySelector
  , percentUnlikelySelector
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
startDate :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO (Id NSDate)
startDate cmDyskineticSymptomResult =
  sendMessage cmDyskineticSymptomResult startDateSelector

-- | The date and time representing the end of the result.
--
-- ObjC selector: @- endDate@
endDate :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO (Id NSDate)
endDate cmDyskineticSymptomResult =
  sendMessage cmDyskineticSymptomResult endDateSelector

-- | The percentage of time dyskinetic symptoms were unlikely for the result.
--
-- ObjC selector: @- percentUnlikely@
percentUnlikely :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO CFloat
percentUnlikely cmDyskineticSymptomResult =
  sendMessage cmDyskineticSymptomResult percentUnlikelySelector

-- | The percentage of time dyskinetic symptoms were likely for the result.
--
-- ObjC selector: @- percentLikely@
percentLikely :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO CFloat
percentLikely cmDyskineticSymptomResult =
  sendMessage cmDyskineticSymptomResult percentLikelySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @percentUnlikely@
percentUnlikelySelector :: Selector '[] CFloat
percentUnlikelySelector = mkSelector "percentUnlikely"

-- | @Selector@ for @percentLikely@
percentLikelySelector :: Selector '[] CFloat
percentLikelySelector = mkSelector "percentLikely"

