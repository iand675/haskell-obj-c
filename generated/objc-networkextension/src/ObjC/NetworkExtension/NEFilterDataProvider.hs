{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterDataProvider
--
-- The NEFilterDataProvider class declares the programmatic interface for an object that evaluates network data flows based on a set of locally-available rules and makes decisions about whether to block or allow the flows.
--
-- Generated bindings for @NEFilterDataProvider@.
module ObjC.NetworkExtension.NEFilterDataProvider
  ( NEFilterDataProvider
  , IsNEFilterDataProvider(..)
  , handleNewFlow
  , handleInboundDataFromFlow_readBytesStartOffset_readBytes
  , handleOutboundDataFromFlow_readBytesStartOffset_readBytes
  , handleInboundDataCompleteForFlow
  , handleOutboundDataCompleteForFlow
  , handleRemediationForFlow
  , handleRulesChanged
  , applySettings_completionHandler
  , resumeFlow_withVerdict
  , updateFlow_usingVerdict_forDirection
  , handleNewFlowSelector
  , handleInboundDataFromFlow_readBytesStartOffset_readBytesSelector
  , handleOutboundDataFromFlow_readBytesStartOffset_readBytesSelector
  , handleInboundDataCompleteForFlowSelector
  , handleOutboundDataCompleteForFlowSelector
  , handleRemediationForFlowSelector
  , handleRulesChangedSelector
  , applySettings_completionHandlerSelector
  , resumeFlow_withVerdictSelector
  , updateFlow_usingVerdict_forDirectionSelector

  -- * Enum types
  , NETrafficDirection(NETrafficDirection)
  , pattern NETrafficDirectionAny
  , pattern NETrafficDirectionInbound
  , pattern NETrafficDirectionOutbound

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

-- | handleNewFlow:
--
-- This function is called by the framework when a filtering decision needs to be made about a new network data flow. Subclasses must override this method to implement the steps necessary to match the flow against some locally stored rules and return an appropriate verdict.
--
-- @flow@ — An NEFilterFlow object containing details about the new flow.
--
-- Returns: An NEFilterNewFlowVerdict object containing the verdict for the new flow.
--
-- ObjC selector: @- handleNewFlow:@
handleNewFlow :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow) => neFilterDataProvider -> flow -> IO (Id NEFilterNewFlowVerdict)
handleNewFlow neFilterDataProvider  flow =
withObjCPtr flow $ \raw_flow ->
    sendMsg neFilterDataProvider (mkSelector "handleNewFlow:") (retPtr retVoid) [argPtr (castPtr raw_flow :: Ptr ())] >>= retainedObject . castPtr

-- | handleInboundDataFromFlow:readBytesStartOffset:readBytes:
--
-- This function is called by the framework when a filtering decision needs to be made about some inbound data that the filter previously requested access to via the NEFilterFlowDataVerdict or the NEFilterNewFlowVerdict. Subclasses must override this method.
--
-- @flow@ — The NEFilterFlow from which the data was read.
--
-- @offset@ — The offset in bytes from the start of the flow's inbound data at which readBytes begins.
--
-- @readBytes@ — The data that was read.  For non-UDP/TCP flows, since data may optionally include the IP header, readBytes includes a 4-bytes NEFilterDataAttribute field preceding the user data.  Handler must examine the NEFilterDataAttribute field and handle the data accordingly.
--
-- Returns: An NEFilterFlowDataVerdict containing the verdict for the flow.
--
-- ObjC selector: @- handleInboundDataFromFlow:readBytesStartOffset:readBytes:@
handleInboundDataFromFlow_readBytesStartOffset_readBytes :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow, IsNSData readBytes) => neFilterDataProvider -> flow -> CULong -> readBytes -> IO (Id NEFilterDataVerdict)
handleInboundDataFromFlow_readBytesStartOffset_readBytes neFilterDataProvider  flow offset readBytes =
withObjCPtr flow $ \raw_flow ->
  withObjCPtr readBytes $ \raw_readBytes ->
      sendMsg neFilterDataProvider (mkSelector "handleInboundDataFromFlow:readBytesStartOffset:readBytes:") (retPtr retVoid) [argPtr (castPtr raw_flow :: Ptr ()), argCULong (fromIntegral offset), argPtr (castPtr raw_readBytes :: Ptr ())] >>= retainedObject . castPtr

-- | handleOutboundDataFromFlow:readBytesStartOffset:readBytes:
--
-- This function is called by the framework when a filtering decision needs to be made about some outbound data that the filter previously requested access to via the NEFilterFlowDataVerdict or the NEFilterNewFlowVerdict. Subclasses must override this method.
--
-- @flow@ — The NEFilterFlow from which the data was read.
--
-- @offset@ — The offset in bytes from the start of the flow's outbound data at which readBytes begins.
--
-- @readBytes@ — The data that was read.  For non-UDP/TCP flows, since data may optionally include the IP header, readBytes includes a 4-bytes NEFilterDataAttribute field preceding the user data.  Handler must examine the NEFilterDataAttribute field and handle the data accordingly.
--
-- Returns: An NEFilterFlowDataVerdict containing the verdict for the flow.
--
-- ObjC selector: @- handleOutboundDataFromFlow:readBytesStartOffset:readBytes:@
handleOutboundDataFromFlow_readBytesStartOffset_readBytes :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow, IsNSData readBytes) => neFilterDataProvider -> flow -> CULong -> readBytes -> IO (Id NEFilterDataVerdict)
handleOutboundDataFromFlow_readBytesStartOffset_readBytes neFilterDataProvider  flow offset readBytes =
withObjCPtr flow $ \raw_flow ->
  withObjCPtr readBytes $ \raw_readBytes ->
      sendMsg neFilterDataProvider (mkSelector "handleOutboundDataFromFlow:readBytesStartOffset:readBytes:") (retPtr retVoid) [argPtr (castPtr raw_flow :: Ptr ()), argCULong (fromIntegral offset), argPtr (castPtr raw_readBytes :: Ptr ())] >>= retainedObject . castPtr

-- | handleInboundDataCompleteForFlow:
--
-- This function is called by the framework after all of the inbound data for a flow has been seen by the filter. Subclasses must override this method to return an appropriate pass/block result.
--
-- @flow@ — The flow
--
-- Returns: The final NEFilterFlowDataVerdict verdict for the flow.
--
-- ObjC selector: @- handleInboundDataCompleteForFlow:@
handleInboundDataCompleteForFlow :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow) => neFilterDataProvider -> flow -> IO (Id NEFilterDataVerdict)
handleInboundDataCompleteForFlow neFilterDataProvider  flow =
withObjCPtr flow $ \raw_flow ->
    sendMsg neFilterDataProvider (mkSelector "handleInboundDataCompleteForFlow:") (retPtr retVoid) [argPtr (castPtr raw_flow :: Ptr ())] >>= retainedObject . castPtr

-- | handleOutboundDataCompleteForFlow:
--
-- This function is called by the framework after all of the outbound data for a flow has been seen by the filter. Subclasses must override this method to return an appropriate pass/block result.
--
-- @flow@ — The flow
--
-- Returns: The final NEFilterFlowDataVerdict verdict for the flow.
--
-- ObjC selector: @- handleOutboundDataCompleteForFlow:@
handleOutboundDataCompleteForFlow :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow) => neFilterDataProvider -> flow -> IO (Id NEFilterDataVerdict)
handleOutboundDataCompleteForFlow neFilterDataProvider  flow =
withObjCPtr flow $ \raw_flow ->
    sendMsg neFilterDataProvider (mkSelector "handleOutboundDataCompleteForFlow:") (retPtr retVoid) [argPtr (castPtr raw_flow :: Ptr ())] >>= retainedObject . castPtr

-- | handleRemediationForFlow:
--
-- This function is called by the framework after the user requests remediation for a blocked flow. Subclasses must override this method to return an appropriate pass/block result.
--
-- @flow@ — The flow
--
-- Returns: The final NEFilterRemediationVerdict verdict for the flow.
--
-- ObjC selector: @- handleRemediationForFlow:@
handleRemediationForFlow :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow) => neFilterDataProvider -> flow -> IO (Id NEFilterRemediationVerdict)
handleRemediationForFlow neFilterDataProvider  flow =
withObjCPtr flow $ \raw_flow ->
    sendMsg neFilterDataProvider (mkSelector "handleRemediationForFlow:") (retPtr retVoid) [argPtr (castPtr raw_flow :: Ptr ())] >>= retainedObject . castPtr

-- | handleRulesChanged
--
-- This function is called by the framework when -[NEFilterControlProvider notifyRulesChanged] is called. Subclasses should override this method to reload new rules from disk.
--
-- ObjC selector: @- handleRulesChanged@
handleRulesChanged :: IsNEFilterDataProvider neFilterDataProvider => neFilterDataProvider -> IO ()
handleRulesChanged neFilterDataProvider  =
  sendMsg neFilterDataProvider (mkSelector "handleRulesChanged") retVoid []

-- | applyFilterRules:defaultAction:withCompletionHandler:
--
-- The provider calls this function to apply the current set of filtering rules associated with the provider and also change the default filtering action.
--
-- @settings@ — A NEFilterSettings object containing the filter settings to apply to the system. Pass nil to revert to the default settings, which are an     empty list of rules and a default action of NEFilterActionFilterData.
--
-- @completionHandler@ — A block that will be executed when the settings have been applied to the system. If an error occurs then the error parameter will be non-nil.
--
-- ObjC selector: @- applySettings:completionHandler:@
applySettings_completionHandler :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterSettings settings) => neFilterDataProvider -> settings -> Ptr () -> IO ()
applySettings_completionHandler neFilterDataProvider  settings completionHandler =
withObjCPtr settings $ \raw_settings ->
    sendMsg neFilterDataProvider (mkSelector "applySettings:completionHandler:") retVoid [argPtr (castPtr raw_settings :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | resumeFlow:withVerdict:
--
-- This function is called by the provider to resume a flow that was previously paused by the provider returning a pause verdict.
--
-- @flow@ — The flow to resume
--
-- @verdict@ — The next NEFilterDataVerdict for the flow. This verdict is used as the verdict corresponding to the    flow handler callback (handleNewFlow:, handleInboundDataFromFlow:, etc.) that returned the pause verdict that    paused the flow. This must be either a NEFilterDataVerdict or a NEFilterNewFlowVerdict. It is invalid to resume    a flow that is not paused.
--
-- ObjC selector: @- resumeFlow:withVerdict:@
resumeFlow_withVerdict :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterFlow flow, IsNEFilterVerdict verdict) => neFilterDataProvider -> flow -> verdict -> IO ()
resumeFlow_withVerdict neFilterDataProvider  flow verdict =
withObjCPtr flow $ \raw_flow ->
  withObjCPtr verdict $ \raw_verdict ->
      sendMsg neFilterDataProvider (mkSelector "resumeFlow:withVerdict:") retVoid [argPtr (castPtr raw_flow :: Ptr ()), argPtr (castPtr raw_verdict :: Ptr ())]

-- | updateFlow:withVerdict:forDirection:
--
-- This function is called by the provider to update the verdict for a flow outside the context of any NEFilterDataProvider callback.
--
-- @flow@ — The NEFilterSocketFlow to update the verdict for.
--
-- @verdict@ — The NEFilterDataVerdict. Must be a +[NEFilterDataVerdict allowVerdict], a +[NEFilterDataVerdict dropVerdict], or a +[NEFilterDataVerdict dataVerdictWithPassBytes:peekBytes:].
--
-- @direction@ — The direction to which the verdict applies. Pass NETrafficDirectionAny to update the verdict for both the inbound and outbound directions. This parameter is ignored if the verdict is +[NEFilterDataVerdict dropVerdict].
--
-- ObjC selector: @- updateFlow:usingVerdict:forDirection:@
updateFlow_usingVerdict_forDirection :: (IsNEFilterDataProvider neFilterDataProvider, IsNEFilterSocketFlow flow, IsNEFilterDataVerdict verdict) => neFilterDataProvider -> flow -> verdict -> NETrafficDirection -> IO ()
updateFlow_usingVerdict_forDirection neFilterDataProvider  flow verdict direction =
withObjCPtr flow $ \raw_flow ->
  withObjCPtr verdict $ \raw_verdict ->
      sendMsg neFilterDataProvider (mkSelector "updateFlow:usingVerdict:forDirection:") retVoid [argPtr (castPtr raw_flow :: Ptr ()), argPtr (castPtr raw_verdict :: Ptr ()), argCLong (coerce direction)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @handleNewFlow:@
handleNewFlowSelector :: Selector
handleNewFlowSelector = mkSelector "handleNewFlow:"

-- | @Selector@ for @handleInboundDataFromFlow:readBytesStartOffset:readBytes:@
handleInboundDataFromFlow_readBytesStartOffset_readBytesSelector :: Selector
handleInboundDataFromFlow_readBytesStartOffset_readBytesSelector = mkSelector "handleInboundDataFromFlow:readBytesStartOffset:readBytes:"

-- | @Selector@ for @handleOutboundDataFromFlow:readBytesStartOffset:readBytes:@
handleOutboundDataFromFlow_readBytesStartOffset_readBytesSelector :: Selector
handleOutboundDataFromFlow_readBytesStartOffset_readBytesSelector = mkSelector "handleOutboundDataFromFlow:readBytesStartOffset:readBytes:"

-- | @Selector@ for @handleInboundDataCompleteForFlow:@
handleInboundDataCompleteForFlowSelector :: Selector
handleInboundDataCompleteForFlowSelector = mkSelector "handleInboundDataCompleteForFlow:"

-- | @Selector@ for @handleOutboundDataCompleteForFlow:@
handleOutboundDataCompleteForFlowSelector :: Selector
handleOutboundDataCompleteForFlowSelector = mkSelector "handleOutboundDataCompleteForFlow:"

-- | @Selector@ for @handleRemediationForFlow:@
handleRemediationForFlowSelector :: Selector
handleRemediationForFlowSelector = mkSelector "handleRemediationForFlow:"

-- | @Selector@ for @handleRulesChanged@
handleRulesChangedSelector :: Selector
handleRulesChangedSelector = mkSelector "handleRulesChanged"

-- | @Selector@ for @applySettings:completionHandler:@
applySettings_completionHandlerSelector :: Selector
applySettings_completionHandlerSelector = mkSelector "applySettings:completionHandler:"

-- | @Selector@ for @resumeFlow:withVerdict:@
resumeFlow_withVerdictSelector :: Selector
resumeFlow_withVerdictSelector = mkSelector "resumeFlow:withVerdict:"

-- | @Selector@ for @updateFlow:usingVerdict:forDirection:@
updateFlow_usingVerdict_forDirectionSelector :: Selector
updateFlow_usingVerdict_forDirectionSelector = mkSelector "updateFlow:usingVerdict:forDirection:"

