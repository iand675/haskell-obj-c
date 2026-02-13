{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKHeartbeatSeriesQuery@.
module ObjC.HealthKit.HKHeartbeatSeriesQuery
  ( HKHeartbeatSeriesQuery
  , IsHKHeartbeatSeriesQuery(..)
  , initWithHeartbeatSeries_dataHandler
  , initWithHeartbeatSeries_dataHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithHeartbeatSeries:dataHandler:
--
-- Returns a query that will retrieve heartbeat timestamps for the specified HKHeartbeatSeriesSample.
--
-- @heartbeatSeries@ — The HKHeartbeatSeriesSample for which the heartbeat data will be returned.
--
-- @dataHandler@ — The block to invoke with results from the query. It is called repeatedly for each                                   heartbeat in the series. timeSinceSeriesStart is the time elapsed in seconds after the                                   series startDate that represents when the heartbeat occured. precededByGap indicates if                                   there was a gap in data collection before the current heartbeat, meaning that one or more                                   heartbeats may have occured since the previous heartbeat in the series. Once done is YES,                                   or stopQuery called, the query is complete and no more calls to the handler will be made.
--
-- ObjC selector: @- initWithHeartbeatSeries:dataHandler:@
initWithHeartbeatSeries_dataHandler :: (IsHKHeartbeatSeriesQuery hkHeartbeatSeriesQuery, IsHKHeartbeatSeriesSample heartbeatSeries) => hkHeartbeatSeriesQuery -> heartbeatSeries -> Ptr () -> IO (Id HKHeartbeatSeriesQuery)
initWithHeartbeatSeries_dataHandler hkHeartbeatSeriesQuery heartbeatSeries dataHandler =
  sendOwnedMessage hkHeartbeatSeriesQuery initWithHeartbeatSeries_dataHandlerSelector (toHKHeartbeatSeriesSample heartbeatSeries) dataHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHeartbeatSeries:dataHandler:@
initWithHeartbeatSeries_dataHandlerSelector :: Selector '[Id HKHeartbeatSeriesSample, Ptr ()] (Id HKHeartbeatSeriesQuery)
initWithHeartbeatSeries_dataHandlerSelector = mkSelector "initWithHeartbeatSeries:dataHandler:"

