{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Representation of metric data corresponding to a metric event.
--
-- Generated bindings for @MTRMetricData@.
module ObjC.Matter.MTRMetricData
  ( MTRMetricData
  , IsMTRMetricData(..)
  , value
  , errorCode
  , duration
  , durationSelector
  , errorCodeSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Value for the metric data. The value may be nil depending on the event emitted.
--
-- ObjC selector: @- value@
value :: IsMTRMetricData mtrMetricData => mtrMetricData -> IO (Id NSNumber)
value mtrMetricData =
  sendMessage mtrMetricData valueSelector

-- | Error code for the metric data. This value, when not nil, holds the error code value of the operation associated with the event. Interpretation of the error code value dependents on the metric being emitted.
--
-- ObjC selector: @- errorCode@
errorCode :: IsMTRMetricData mtrMetricData => mtrMetricData -> IO (Id NSNumber)
errorCode mtrMetricData =
  sendMessage mtrMetricData errorCodeSelector

-- | Duration of event associated with the metric. This value may be nil depending on the event emitted. When not nil, the value of duration is of type NSTimeInterval.
--
-- ObjC selector: @- duration@
duration :: IsMTRMetricData mtrMetricData => mtrMetricData -> IO (Id NSNumber)
duration mtrMetricData =
  sendMessage mtrMetricData durationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @errorCode@
errorCodeSelector :: Selector '[] (Id NSNumber)
errorCodeSelector = mkSelector "errorCode"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

