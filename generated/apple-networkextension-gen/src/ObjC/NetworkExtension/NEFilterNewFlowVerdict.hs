{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterNewFlowVerdict
--
-- The NEFilterNewFlowVerdict declares the programmatic interface of an object that is the verdict for a new flow of network data before any of the flow's data has been seen by the filter.
--
-- NEFilterNewFlowVerdict is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterNewFlowVerdict@.
module ObjC.NetworkExtension.NEFilterNewFlowVerdict
  ( NEFilterNewFlowVerdict
  , IsNEFilterNewFlowVerdict(..)
  , needRulesVerdict
  , allowVerdict
  , dropVerdict
  , remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKey
  , urlAppendStringVerdictWithMapKey
  , filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytes
  , pauseVerdict
  , statisticsReportFrequency
  , setStatisticsReportFrequency
  , allowVerdictSelector
  , dropVerdictSelector
  , filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector
  , needRulesVerdictSelector
  , pauseVerdictSelector
  , remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector
  , setStatisticsReportFrequencySelector
  , statisticsReportFrequencySelector
  , urlAppendStringVerdictWithMapKeySelector

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

-- | needRulesVerdict
--
-- This class method returns a verdict indicating that control provider needs to be asked how to handle the new flow. The control provider can either drop or allow the flow, or update the rules and ask the data provider to decide on the new flow again.
--
-- Returns: The NEFilterNewFlowVerdict object.
--
-- ObjC selector: @+ needRulesVerdict@
needRulesVerdict :: IO (Id NEFilterNewFlowVerdict)
needRulesVerdict  =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' needRulesVerdictSelector

-- | allowVerdict
--
-- This class method returns a verdict indicating that the flow should be allowed.
--
-- Returns: The NEFilterNewFlowVerdict object.
--
-- ObjC selector: @+ allowVerdict@
allowVerdict :: IO (Id NEFilterNewFlowVerdict)
allowVerdict  =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' allowVerdictSelector

-- | dropVerdict
--
-- This class method returns a verdict indicating that the flow should be dropped.
--
-- Returns: The NEFilterNewFlowVerdict object.
--
-- ObjC selector: @+ dropVerdict@
dropVerdict :: IO (Id NEFilterNewFlowVerdict)
dropVerdict  =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' dropVerdictSelector

-- | remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:
--
-- This class method returns a verdict indicating that a "content blocked" page should be displayed to the user. The block page should contain a link to the given URL.
--
-- @remediationURLMapKey@ — Remediation map key used by data plugin to get remediation url
--
-- Returns: The NEFilterNewFlowVerdict object.
--
-- ObjC selector: @+ remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:@
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKey :: (IsNSString remediationURLMapKey, IsNSString remediationButtonTextMapKey) => remediationURLMapKey -> remediationButtonTextMapKey -> IO (Id NEFilterNewFlowVerdict)
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKey remediationURLMapKey remediationButtonTextMapKey =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector (toNSString remediationURLMapKey) (toNSString remediationButtonTextMapKey)

-- | URLAppendStringVerdictWithMapKey
--
-- This class method returns a verdict indicating that safe search URL for the new should be specified
--
-- @urlAppendMapKey@ — URL Append map key to be used by the data plugin to notify what the url should be appended with
--
-- Returns: The NEFilterNewFlowVerdict object.
--
-- ObjC selector: @+ URLAppendStringVerdictWithMapKey:@
urlAppendStringVerdictWithMapKey :: IsNSString urlAppendMapKey => urlAppendMapKey -> IO (Id NEFilterNewFlowVerdict)
urlAppendStringVerdictWithMapKey urlAppendMapKey =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' urlAppendStringVerdictWithMapKeySelector (toNSString urlAppendMapKey)

-- | filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:
--
-- This class method returns a new flow verdict indicating that the filter needs to make a decision about a new flow after seeing a portion of the flow's data.
--
-- @filterInbound@ — A boolean indicating if the filter needs to see inbound data
--
-- @peekInboundBytes@ — The number of inbound bytes that the filter needs to see in the subsequent call to -[NEFilterDataProvider handleInboundDataFromFlow:readBytesStartOffset:readBytes:].
--
-- @filterOutbound@ — boolean indicating if the filter needs to see outbound data
--
-- @peekOutboundBytes@ — The number of outbound bytes that the filter needs to see in the subsequent call to -[NEFilterDataProvider handleOutboundDataFromFlow:readBytesStartOffset:readBytes:].
--
-- Returns: The new flow verdict.
--
-- ObjC selector: @+ filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:@
filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytes :: Bool -> CULong -> Bool -> CULong -> IO (Id NEFilterNewFlowVerdict)
filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytes filterInbound peekInboundBytes filterOutbound peekOutboundBytes =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector filterInbound peekInboundBytes filterOutbound peekOutboundBytes

-- | pauseVerdict
--
-- This class method returns a verdict indicating that none of the data provider's handler callbacks shall be called for the flow until after the flow is resumed     by a call to -[NEFilterDataProvider resumeFlow:withVerdict:]. TCP flows may be paused indefinitely. UDP flows will be dropped if not resumed within 10 seconds of     being paused. It is invalid to pause a flow that is already paused.
--
-- Returns: The NEFilterNewFlowVerdict object.
--
-- ObjC selector: @+ pauseVerdict@
pauseVerdict :: IO (Id NEFilterNewFlowVerdict)
pauseVerdict  =
  do
    cls' <- getRequiredClass "NEFilterNewFlowVerdict"
    sendClassMessage cls' pauseVerdictSelector

-- | statisticsReportFrequency
--
-- The frequency at which the data provider's -[NEFilterProvider handleReport:] method is called with a NEFilterReport instance with an event of NEFilterReportEventFlowStatistics.     The default value is NEFilterReportFrequencyNone, so by default no statistics are reported.
--
-- ObjC selector: @- statisticsReportFrequency@
statisticsReportFrequency :: IsNEFilterNewFlowVerdict neFilterNewFlowVerdict => neFilterNewFlowVerdict -> IO NEFilterReportFrequency
statisticsReportFrequency neFilterNewFlowVerdict =
  sendMessage neFilterNewFlowVerdict statisticsReportFrequencySelector

-- | statisticsReportFrequency
--
-- The frequency at which the data provider's -[NEFilterProvider handleReport:] method is called with a NEFilterReport instance with an event of NEFilterReportEventFlowStatistics.     The default value is NEFilterReportFrequencyNone, so by default no statistics are reported.
--
-- ObjC selector: @- setStatisticsReportFrequency:@
setStatisticsReportFrequency :: IsNEFilterNewFlowVerdict neFilterNewFlowVerdict => neFilterNewFlowVerdict -> NEFilterReportFrequency -> IO ()
setStatisticsReportFrequency neFilterNewFlowVerdict value =
  sendMessage neFilterNewFlowVerdict setStatisticsReportFrequencySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @needRulesVerdict@
needRulesVerdictSelector :: Selector '[] (Id NEFilterNewFlowVerdict)
needRulesVerdictSelector = mkSelector "needRulesVerdict"

-- | @Selector@ for @allowVerdict@
allowVerdictSelector :: Selector '[] (Id NEFilterNewFlowVerdict)
allowVerdictSelector = mkSelector "allowVerdict"

-- | @Selector@ for @dropVerdict@
dropVerdictSelector :: Selector '[] (Id NEFilterNewFlowVerdict)
dropVerdictSelector = mkSelector "dropVerdict"

-- | @Selector@ for @remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:@
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector :: Selector '[Id NSString, Id NSString] (Id NEFilterNewFlowVerdict)
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector = mkSelector "remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:"

-- | @Selector@ for @URLAppendStringVerdictWithMapKey:@
urlAppendStringVerdictWithMapKeySelector :: Selector '[Id NSString] (Id NEFilterNewFlowVerdict)
urlAppendStringVerdictWithMapKeySelector = mkSelector "URLAppendStringVerdictWithMapKey:"

-- | @Selector@ for @filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:@
filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector :: Selector '[Bool, CULong, Bool, CULong] (Id NEFilterNewFlowVerdict)
filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector = mkSelector "filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:"

-- | @Selector@ for @pauseVerdict@
pauseVerdictSelector :: Selector '[] (Id NEFilterNewFlowVerdict)
pauseVerdictSelector = mkSelector "pauseVerdict"

-- | @Selector@ for @statisticsReportFrequency@
statisticsReportFrequencySelector :: Selector '[] NEFilterReportFrequency
statisticsReportFrequencySelector = mkSelector "statisticsReportFrequency"

-- | @Selector@ for @setStatisticsReportFrequency:@
setStatisticsReportFrequencySelector :: Selector '[NEFilterReportFrequency] ()
setStatisticsReportFrequencySelector = mkSelector "setStatisticsReportFrequency:"

