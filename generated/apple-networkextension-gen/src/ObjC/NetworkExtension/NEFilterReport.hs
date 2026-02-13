{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterReport
--
-- The NEFilterReport declares the programmatic interface of an object that is a report of actions taken by the data provider.
--
-- NEFilterReport is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterReport@.
module ObjC.NetworkExtension.NEFilterReport
  ( NEFilterReport
  , IsNEFilterReport(..)
  , flow
  , action
  , event
  , bytesInboundCount
  , bytesOutboundCount
  , actionSelector
  , bytesInboundCountSelector
  , bytesOutboundCountSelector
  , eventSelector
  , flowSelector

  -- * Enum types
  , NEFilterAction(NEFilterAction)
  , pattern NEFilterActionInvalid
  , pattern NEFilterActionAllow
  , pattern NEFilterActionDrop
  , pattern NEFilterActionRemediate
  , pattern NEFilterActionFilterData
  , NEFilterReportEvent(NEFilterReportEvent)
  , pattern NEFilterReportEventNewFlow
  , pattern NEFilterReportEventDataDecision
  , pattern NEFilterReportEventFlowClosed
  , pattern NEFilterReportEventStatistics

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | flow
--
-- The flow on which the described action was taken.
--
-- ObjC selector: @- flow@
flow :: IsNEFilterReport neFilterReport => neFilterReport -> IO (Id NEFilterFlow)
flow neFilterReport =
  sendMessage neFilterReport flowSelector

-- | action
--
-- The action taken upon the reported flow.
--
-- ObjC selector: @- action@
action :: IsNEFilterReport neFilterReport => neFilterReport -> IO NEFilterAction
action neFilterReport =
  sendMessage neFilterReport actionSelector

-- | event
--
-- The type of event that the report is reporting.
--
-- ObjC selector: @- event@
event :: IsNEFilterReport neFilterReport => neFilterReport -> IO NEFilterReportEvent
event neFilterReport =
  sendMessage neFilterReport eventSelector

-- | bytesInboundCount
--
-- The number of inbound bytes received from the flow. This property is only non-zero when the report event is NEFilterReportEventFlowClosed or NEFilterReportEventFlowStatistics.
--
-- ObjC selector: @- bytesInboundCount@
bytesInboundCount :: IsNEFilterReport neFilterReport => neFilterReport -> IO CULong
bytesInboundCount neFilterReport =
  sendMessage neFilterReport bytesInboundCountSelector

-- | bytesOutboundCount
--
-- The number of outbound bytes sent on the flow. This property is only non-zero when the report event is NEFilterReportEventFlowClosed or NEFilterReportEventFlowStatistics.
--
-- ObjC selector: @- bytesOutboundCount@
bytesOutboundCount :: IsNEFilterReport neFilterReport => neFilterReport -> IO CULong
bytesOutboundCount neFilterReport =
  sendMessage neFilterReport bytesOutboundCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flow@
flowSelector :: Selector '[] (Id NEFilterFlow)
flowSelector = mkSelector "flow"

-- | @Selector@ for @action@
actionSelector :: Selector '[] NEFilterAction
actionSelector = mkSelector "action"

-- | @Selector@ for @event@
eventSelector :: Selector '[] NEFilterReportEvent
eventSelector = mkSelector "event"

-- | @Selector@ for @bytesInboundCount@
bytesInboundCountSelector :: Selector '[] CULong
bytesInboundCountSelector = mkSelector "bytesInboundCount"

-- | @Selector@ for @bytesOutboundCount@
bytesOutboundCountSelector :: Selector '[] CULong
bytesOutboundCountSelector = mkSelector "bytesOutboundCount"

