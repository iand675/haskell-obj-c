{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREventReport@.
module ObjC.Matter.MTREventReport
  ( MTREventReport
  , IsMTREventReport(..)
  , initWithResponseValue_error
  , path
  , eventNumber
  , priority
  , eventTimeType
  , systemUpTime
  , timestampDate
  , value
  , error_
  , timestamp
  , errorSelector
  , eventNumberSelector
  , eventTimeTypeSelector
  , initWithResponseValue_errorSelector
  , pathSelector
  , prioritySelector
  , systemUpTimeSelector
  , timestampDateSelector
  , timestampSelector
  , valueSelector

  -- * Enum types
  , MTREventTimeType(MTREventTimeType)
  , pattern MTREventTimeTypeSystemUpTime
  , pattern MTREventTimeTypeTimestampDate

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTREventReport with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not an event response.
--
-- Will set the value property to nil and the error property to non-nil, even if the schema for the value is not known, if the response-value is an error, not data.
--
-- Will return nil and hand out an error if the response-value is data in the following cases:
--
-- * The response is for a cluster/event combination for which the schema is   unknown and hence the type of the data is not known. * The data does not match the known schema.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTREventReport mtrEventReport, IsNSDictionary responseValue, IsNSError error_) => mtrEventReport -> responseValue -> error_ -> IO (Id MTREventReport)
initWithResponseValue_error mtrEventReport responseValue error_ =
  sendOwnedMessage mtrEventReport initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- path@
path :: IsMTREventReport mtrEventReport => mtrEventReport -> IO (Id MTREventPath)
path mtrEventReport =
  sendMessage mtrEventReport pathSelector

-- | eventNumber will only have a useful value if "error" is nil.
--
-- ObjC selector: @- eventNumber@
eventNumber :: IsMTREventReport mtrEventReport => mtrEventReport -> IO (Id NSNumber)
eventNumber mtrEventReport =
  sendMessage mtrEventReport eventNumberSelector

-- | priority will only have a useful value if "error" is nil.
--
-- ObjC selector: @- priority@
priority :: IsMTREventReport mtrEventReport => mtrEventReport -> IO (Id NSNumber)
priority mtrEventReport =
  sendMessage mtrEventReport prioritySelector

-- | Either systemUpTime or timestampDate will be valid depending on eventTimeType, if "error" is nil.  If "error" is not nil, none of eventTimeType, systemUpTime, timestampDate should be expected to have useful values.
--
-- ObjC selector: @- eventTimeType@
eventTimeType :: IsMTREventReport mtrEventReport => mtrEventReport -> IO MTREventTimeType
eventTimeType mtrEventReport =
  sendMessage mtrEventReport eventTimeTypeSelector

-- | @- systemUpTime@
systemUpTime :: IsMTREventReport mtrEventReport => mtrEventReport -> IO CDouble
systemUpTime mtrEventReport =
  sendMessage mtrEventReport systemUpTimeSelector

-- | @- timestampDate@
timestampDate :: IsMTREventReport mtrEventReport => mtrEventReport -> IO (Id NSDate)
timestampDate mtrEventReport =
  sendMessage mtrEventReport timestampDateSelector

-- | An instance of the event payload interface that corresponds to the report's path (e.g. MTRBasicInformationClusterStartUpEvent if the path's cluster 0x0028 "Basic Information" and the path's event is 0x00 "StartUp"), or nil if error is not nil (in which case there is no payload available).
--
-- ObjC selector: @- value@
value :: IsMTREventReport mtrEventReport => mtrEventReport -> IO RawId
value mtrEventReport =
  sendMessage mtrEventReport valueSelector

-- | If this specific path resulted in an error, the error (in the MTRInteractionErrorDomain or MTRErrorDomain) that corresponds to this path.
--
-- ObjC selector: @- error@
error_ :: IsMTREventReport mtrEventReport => mtrEventReport -> IO (Id NSError)
error_ mtrEventReport =
  sendMessage mtrEventReport errorSelector

-- | @- timestamp@
timestamp :: IsMTREventReport mtrEventReport => mtrEventReport -> IO (Id NSNumber)
timestamp mtrEventReport =
  sendMessage mtrEventReport timestampSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTREventReport)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id MTREventPath)
pathSelector = mkSelector "path"

-- | @Selector@ for @eventNumber@
eventNumberSelector :: Selector '[] (Id NSNumber)
eventNumberSelector = mkSelector "eventNumber"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] (Id NSNumber)
prioritySelector = mkSelector "priority"

-- | @Selector@ for @eventTimeType@
eventTimeTypeSelector :: Selector '[] MTREventTimeType
eventTimeTypeSelector = mkSelector "eventTimeType"

-- | @Selector@ for @systemUpTime@
systemUpTimeSelector :: Selector '[] CDouble
systemUpTimeSelector = mkSelector "systemUpTime"

-- | @Selector@ for @timestampDate@
timestampDateSelector :: Selector '[] (Id NSDate)
timestampDateSelector = mkSelector "timestampDate"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSNumber)
timestampSelector = mkSelector "timestamp"

