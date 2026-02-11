{-# LANGUAGE PatternSynonyms #-}
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
  , needRulesVerdictSelector
  , allowVerdictSelector
  , dropVerdictSelector
  , remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector
  , urlAppendStringVerdictWithMapKeySelector
  , filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector
  , pauseVerdictSelector
  , statisticsReportFrequencySelector
  , setStatisticsReportFrequencySelector

  -- * Enum types
  , NEFilterReportFrequency(NEFilterReportFrequency)
  , pattern NEFilterReportFrequencyNone
  , pattern NEFilterReportFrequencyLow
  , pattern NEFilterReportFrequencyMedium
  , pattern NEFilterReportFrequencyHigh

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
    sendClassMsg cls' (mkSelector "needRulesVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "allowVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "dropVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    withObjCPtr remediationURLMapKey $ \raw_remediationURLMapKey ->
      withObjCPtr remediationButtonTextMapKey $ \raw_remediationButtonTextMapKey ->
        sendClassMsg cls' (mkSelector "remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:") (retPtr retVoid) [argPtr (castPtr raw_remediationURLMapKey :: Ptr ()), argPtr (castPtr raw_remediationButtonTextMapKey :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr urlAppendMapKey $ \raw_urlAppendMapKey ->
      sendClassMsg cls' (mkSelector "URLAppendStringVerdictWithMapKey:") (retPtr retVoid) [argPtr (castPtr raw_urlAppendMapKey :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:") (retPtr retVoid) [argCULong (if filterInbound then 1 else 0), argCULong (fromIntegral peekInboundBytes), argCULong (if filterOutbound then 1 else 0), argCULong (fromIntegral peekOutboundBytes)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "pauseVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | statisticsReportFrequency
--
-- The frequency at which the data provider's -[NEFilterProvider handleReport:] method is called with a NEFilterReport instance with an event of NEFilterReportEventFlowStatistics.     The default value is NEFilterReportFrequencyNone, so by default no statistics are reported.
--
-- ObjC selector: @- statisticsReportFrequency@
statisticsReportFrequency :: IsNEFilterNewFlowVerdict neFilterNewFlowVerdict => neFilterNewFlowVerdict -> IO NEFilterReportFrequency
statisticsReportFrequency neFilterNewFlowVerdict  =
  fmap (coerce :: CLong -> NEFilterReportFrequency) $ sendMsg neFilterNewFlowVerdict (mkSelector "statisticsReportFrequency") retCLong []

-- | statisticsReportFrequency
--
-- The frequency at which the data provider's -[NEFilterProvider handleReport:] method is called with a NEFilterReport instance with an event of NEFilterReportEventFlowStatistics.     The default value is NEFilterReportFrequencyNone, so by default no statistics are reported.
--
-- ObjC selector: @- setStatisticsReportFrequency:@
setStatisticsReportFrequency :: IsNEFilterNewFlowVerdict neFilterNewFlowVerdict => neFilterNewFlowVerdict -> NEFilterReportFrequency -> IO ()
setStatisticsReportFrequency neFilterNewFlowVerdict  value =
  sendMsg neFilterNewFlowVerdict (mkSelector "setStatisticsReportFrequency:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @needRulesVerdict@
needRulesVerdictSelector :: Selector
needRulesVerdictSelector = mkSelector "needRulesVerdict"

-- | @Selector@ for @allowVerdict@
allowVerdictSelector :: Selector
allowVerdictSelector = mkSelector "allowVerdict"

-- | @Selector@ for @dropVerdict@
dropVerdictSelector :: Selector
dropVerdictSelector = mkSelector "dropVerdict"

-- | @Selector@ for @remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:@
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector :: Selector
remediateVerdictWithRemediationURLMapKey_remediationButtonTextMapKeySelector = mkSelector "remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:"

-- | @Selector@ for @URLAppendStringVerdictWithMapKey:@
urlAppendStringVerdictWithMapKeySelector :: Selector
urlAppendStringVerdictWithMapKeySelector = mkSelector "URLAppendStringVerdictWithMapKey:"

-- | @Selector@ for @filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:@
filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector :: Selector
filterDataVerdictWithFilterInbound_peekInboundBytes_filterOutbound_peekOutboundBytesSelector = mkSelector "filterDataVerdictWithFilterInbound:peekInboundBytes:filterOutbound:peekOutboundBytes:"

-- | @Selector@ for @pauseVerdict@
pauseVerdictSelector :: Selector
pauseVerdictSelector = mkSelector "pauseVerdict"

-- | @Selector@ for @statisticsReportFrequency@
statisticsReportFrequencySelector :: Selector
statisticsReportFrequencySelector = mkSelector "statisticsReportFrequency"

-- | @Selector@ for @setStatisticsReportFrequency:@
setStatisticsReportFrequencySelector :: Selector
setStatisticsReportFrequencySelector = mkSelector "setStatisticsReportFrequency:"

