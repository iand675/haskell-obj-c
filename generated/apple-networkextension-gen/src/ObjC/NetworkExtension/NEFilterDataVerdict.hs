{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterDataVerdict
--
-- The NEFilterDataVerdict class declares the programmatic interface of an object that is the verdict for a flow of network data after some of the data has been seen by the filter.
--
-- NEFilterDataVerdict is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterDataVerdict@.
module ObjC.NetworkExtension.NEFilterDataVerdict
  ( NEFilterDataVerdict
  , IsNEFilterDataVerdict(..)
  , allowVerdict
  , dropVerdict
  , remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKey
  , dataVerdictWithPassBytes_peekBytes
  , needRulesVerdict
  , pauseVerdict
  , statisticsReportFrequency
  , setStatisticsReportFrequency
  , allowVerdictSelector
  , dataVerdictWithPassBytes_peekBytesSelector
  , dropVerdictSelector
  , needRulesVerdictSelector
  , pauseVerdictSelector
  , remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector
  , setStatisticsReportFrequencySelector
  , statisticsReportFrequencySelector

  -- * Enum types
  , NEFilterReportFrequency(NEFilterReportFrequency)
  , pattern NEFilterReportFrequencyNone
  , pattern NEFilterReportFrequencyLow
  , pattern NEFilterReportFrequencyMedium
  , pattern NEFilterReportFrequencyHigh

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

-- | allowVerdict
--
-- This class method returns a verdict indicating that the flow should be allowed.
--
-- Returns: The NEFilterDataVerdict object.
--
-- ObjC selector: @+ allowVerdict@
allowVerdict :: IO (Id NEFilterDataVerdict)
allowVerdict  =
  do
    cls' <- getRequiredClass "NEFilterDataVerdict"
    sendClassMessage cls' allowVerdictSelector

-- | dropVerdict
--
-- This class method returns a verdict indicating that the flow should be dropped.
--
-- Returns: The NEFilterDataVerdict object.
--
-- ObjC selector: @+ dropVerdict@
dropVerdict :: IO (Id NEFilterDataVerdict)
dropVerdict  =
  do
    cls' <- getRequiredClass "NEFilterDataVerdict"
    sendClassMessage cls' dropVerdictSelector

-- | remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:
--
-- This class method returns a verdict indicating that a "content blocked" page should be displayed to the user. The block page should contain a link to the given URL.
--
-- @remediationURLMapKey@ — Remediation map key used by data plugin to get remediation url. Passing nil will result into data provider being notified with the callback handleRemediationForFlow:
--
-- @remediationButtonTextMapKey@ — Remediation button map key used by the data plugin to get the remediation button text. Passing nil will set the button text to "Request Access"
--
-- Returns: The NEFilterDataVerdict object.
--
-- ObjC selector: @+ remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:@
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKey :: (IsNSString remediationURLMapKey, IsNSString remediationButtonTextMapKey) => remediationURLMapKey -> remediationButtonTextMapKey -> IO (Id NEFilterDataVerdict)
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKey remediationURLMapKey remediationButtonTextMapKey =
  do
    cls' <- getRequiredClass "NEFilterDataVerdict"
    sendClassMessage cls' remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector (toNSString remediationURLMapKey) (toNSString remediationButtonTextMapKey)

-- | dataVerdictWithPassBytes:peekBytes:
--
-- This class method returns a data verdict indicating that the filter is passing a given number of bytes through the filter and needs to see a given number of bytes after the bytes that are passed.
--
-- @passBytes@ — The number of bytes to pass through the filter.
--
-- @peekBytes@ — The number of bytes after the end of the bytes passed that the filter wants to see in the next call to -[NEFilterDataProvider handleOutboundDataFromFlow:readBytesStartOffset:readBytes:] or -[NEFilterDataProvider handleInboundDataFromFlow:readBytesStartOffset:readBytes:].
--
-- Returns: The data flow verdict.
--
-- ObjC selector: @+ dataVerdictWithPassBytes:peekBytes:@
dataVerdictWithPassBytes_peekBytes :: CULong -> CULong -> IO (Id NEFilterDataVerdict)
dataVerdictWithPassBytes_peekBytes passBytes peekBytes =
  do
    cls' <- getRequiredClass "NEFilterDataVerdict"
    sendClassMessage cls' dataVerdictWithPassBytes_peekBytesSelector passBytes peekBytes

-- | needRulesVerdict
--
-- This class method returns a verdict indicating that control provider needs to be asked how to handle the data flow. The control provider can either drop or allow the flow, or update the rules and ask the data provider to decide on the data flow again.
--
-- Returns: The NEFilterDataVerdict object.
--
-- ObjC selector: @+ needRulesVerdict@
needRulesVerdict :: IO (Id NEFilterDataVerdict)
needRulesVerdict  =
  do
    cls' <- getRequiredClass "NEFilterDataVerdict"
    sendClassMessage cls' needRulesVerdictSelector

-- | pauseVerdict
--
-- This class method returns a verdict indicating that none of the data provider's handler callbacks shall be called for the flow until after the flow is resumed     by a call to -[NEFilterDataProvider resumeFlow:withVerdict:]. TCP flows may be paused indefinitely. UDP flows will be dropped if not resumed within 10 seconds of     being paused. It is invalid to pause a flow that is already paused.
--
-- Returns: The NEFilterDataVerdict object.
--
-- ObjC selector: @+ pauseVerdict@
pauseVerdict :: IO (Id NEFilterDataVerdict)
pauseVerdict  =
  do
    cls' <- getRequiredClass "NEFilterDataVerdict"
    sendClassMessage cls' pauseVerdictSelector

-- | statisticsReportFrequency
--
-- The frequency at which the data provider's -[NEFilterProvider handleReport:] method is called with a NEFilterReport instance with an event of NEFilterReportEventFlowStatistics.     The default value is NEFilterReportFrequencyNone, so by default no statistics are reported.
--
-- ObjC selector: @- statisticsReportFrequency@
statisticsReportFrequency :: IsNEFilterDataVerdict neFilterDataVerdict => neFilterDataVerdict -> IO NEFilterReportFrequency
statisticsReportFrequency neFilterDataVerdict =
  sendMessage neFilterDataVerdict statisticsReportFrequencySelector

-- | statisticsReportFrequency
--
-- The frequency at which the data provider's -[NEFilterProvider handleReport:] method is called with a NEFilterReport instance with an event of NEFilterReportEventFlowStatistics.     The default value is NEFilterReportFrequencyNone, so by default no statistics are reported.
--
-- ObjC selector: @- setStatisticsReportFrequency:@
setStatisticsReportFrequency :: IsNEFilterDataVerdict neFilterDataVerdict => neFilterDataVerdict -> NEFilterReportFrequency -> IO ()
setStatisticsReportFrequency neFilterDataVerdict value =
  sendMessage neFilterDataVerdict setStatisticsReportFrequencySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowVerdict@
allowVerdictSelector :: Selector '[] (Id NEFilterDataVerdict)
allowVerdictSelector = mkSelector "allowVerdict"

-- | @Selector@ for @dropVerdict@
dropVerdictSelector :: Selector '[] (Id NEFilterDataVerdict)
dropVerdictSelector = mkSelector "dropVerdict"

-- | @Selector@ for @remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:@
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector :: Selector '[Id NSString, Id NSString] (Id NEFilterDataVerdict)
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector = mkSelector "remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:"

-- | @Selector@ for @dataVerdictWithPassBytes:peekBytes:@
dataVerdictWithPassBytes_peekBytesSelector :: Selector '[CULong, CULong] (Id NEFilterDataVerdict)
dataVerdictWithPassBytes_peekBytesSelector = mkSelector "dataVerdictWithPassBytes:peekBytes:"

-- | @Selector@ for @needRulesVerdict@
needRulesVerdictSelector :: Selector '[] (Id NEFilterDataVerdict)
needRulesVerdictSelector = mkSelector "needRulesVerdict"

-- | @Selector@ for @pauseVerdict@
pauseVerdictSelector :: Selector '[] (Id NEFilterDataVerdict)
pauseVerdictSelector = mkSelector "pauseVerdict"

-- | @Selector@ for @statisticsReportFrequency@
statisticsReportFrequencySelector :: Selector '[] NEFilterReportFrequency
statisticsReportFrequencySelector = mkSelector "statisticsReportFrequency"

-- | @Selector@ for @setStatisticsReportFrequency:@
setStatisticsReportFrequencySelector :: Selector '[NEFilterReportFrequency] ()
setStatisticsReportFrequencySelector = mkSelector "setStatisticsReportFrequency:"

