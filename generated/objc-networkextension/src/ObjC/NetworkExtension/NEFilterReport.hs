{-# LANGUAGE PatternSynonyms #-}
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
  , flowSelector
  , actionSelector
  , eventSelector
  , bytesInboundCountSelector
  , bytesOutboundCountSelector

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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | flow
--
-- The flow on which the described action was taken.
--
-- ObjC selector: @- flow@
flow :: IsNEFilterReport neFilterReport => neFilterReport -> IO (Id NEFilterFlow)
flow neFilterReport  =
  sendMsg neFilterReport (mkSelector "flow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | action
--
-- The action taken upon the reported flow.
--
-- ObjC selector: @- action@
action :: IsNEFilterReport neFilterReport => neFilterReport -> IO NEFilterAction
action neFilterReport  =
  fmap (coerce :: CLong -> NEFilterAction) $ sendMsg neFilterReport (mkSelector "action") retCLong []

-- | event
--
-- The type of event that the report is reporting.
--
-- ObjC selector: @- event@
event :: IsNEFilterReport neFilterReport => neFilterReport -> IO NEFilterReportEvent
event neFilterReport  =
  fmap (coerce :: CLong -> NEFilterReportEvent) $ sendMsg neFilterReport (mkSelector "event") retCLong []

-- | bytesInboundCount
--
-- The number of inbound bytes received from the flow. This property is only non-zero when the report event is NEFilterReportEventFlowClosed or NEFilterReportEventFlowStatistics.
--
-- ObjC selector: @- bytesInboundCount@
bytesInboundCount :: IsNEFilterReport neFilterReport => neFilterReport -> IO CULong
bytesInboundCount neFilterReport  =
  sendMsg neFilterReport (mkSelector "bytesInboundCount") retCULong []

-- | bytesOutboundCount
--
-- The number of outbound bytes sent on the flow. This property is only non-zero when the report event is NEFilterReportEventFlowClosed or NEFilterReportEventFlowStatistics.
--
-- ObjC selector: @- bytesOutboundCount@
bytesOutboundCount :: IsNEFilterReport neFilterReport => neFilterReport -> IO CULong
bytesOutboundCount neFilterReport  =
  sendMsg neFilterReport (mkSelector "bytesOutboundCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flow@
flowSelector :: Selector
flowSelector = mkSelector "flow"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

-- | @Selector@ for @bytesInboundCount@
bytesInboundCountSelector :: Selector
bytesInboundCountSelector = mkSelector "bytesInboundCount"

-- | @Selector@ for @bytesOutboundCount@
bytesOutboundCountSelector :: Selector
bytesOutboundCountSelector = mkSelector "bytesOutboundCount"

