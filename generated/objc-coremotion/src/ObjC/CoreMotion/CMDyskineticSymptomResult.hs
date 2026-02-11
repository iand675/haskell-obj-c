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
  , startDateSelector
  , endDateSelector
  , percentUnlikelySelector
  , percentLikelySelector


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
startDate :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO (Id NSDate)
startDate cmDyskineticSymptomResult  =
  sendMsg cmDyskineticSymptomResult (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time representing the end of the result.
--
-- ObjC selector: @- endDate@
endDate :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO (Id NSDate)
endDate cmDyskineticSymptomResult  =
  sendMsg cmDyskineticSymptomResult (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The percentage of time dyskinetic symptoms were unlikely for the result.
--
-- ObjC selector: @- percentUnlikely@
percentUnlikely :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO CFloat
percentUnlikely cmDyskineticSymptomResult  =
  sendMsg cmDyskineticSymptomResult (mkSelector "percentUnlikely") retCFloat []

-- | The percentage of time dyskinetic symptoms were likely for the result.
--
-- ObjC selector: @- percentLikely@
percentLikely :: IsCMDyskineticSymptomResult cmDyskineticSymptomResult => cmDyskineticSymptomResult -> IO CFloat
percentLikely cmDyskineticSymptomResult  =
  sendMsg cmDyskineticSymptomResult (mkSelector "percentLikely") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @percentUnlikely@
percentUnlikelySelector :: Selector
percentUnlikelySelector = mkSelector "percentUnlikely"

-- | @Selector@ for @percentLikely@
percentLikelySelector :: Selector
percentLikelySelector = mkSelector "percentLikely"

