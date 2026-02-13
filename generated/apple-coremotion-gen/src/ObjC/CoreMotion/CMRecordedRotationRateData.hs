{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMRecordedRotationRateData@.
module ObjC.CoreMotion.CMRecordedRotationRateData
  ( CMRecordedRotationRateData
  , IsCMRecordedRotationRateData(..)
  , startDate
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

-- | @- startDate@
startDate :: IsCMRecordedRotationRateData cmRecordedRotationRateData => cmRecordedRotationRateData -> IO (Id NSDate)
startDate cmRecordedRotationRateData =
  sendMessage cmRecordedRotationRateData startDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

