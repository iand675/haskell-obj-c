{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterControlProvider
--
-- The NEFilterControlProvider class declares the programmatic interface for an object that is responsible for installing filtering rules on the device.
--
-- Generated bindings for @NEFilterControlProvider@.
module ObjC.NetworkExtension.NEFilterControlProvider
  ( NEFilterControlProvider
  , IsNEFilterControlProvider(..)
  , handleRemediationForFlow_completionHandler
  , handleNewFlow_completionHandler
  , notifyRulesChanged
  , remediationMap
  , setRemediationMap
  , urlAppendStringMap
  , setURLAppendStringMap
  , handleRemediationForFlow_completionHandlerSelector
  , handleNewFlow_completionHandlerSelector
  , notifyRulesChangedSelector
  , remediationMapSelector
  , setRemediationMapSelector
  , urlAppendStringMapSelector
  , setURLAppendStringMapSelector


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
import ObjC.Foundation.Internal.Classes

-- | handleRemediationForFlow:completionHandler:
--
-- This function is called by the framework when the NEFilterDataProvider indicates that the filtering verdict for the given flow is NEFilterRemediateVerdictNeedRules. Subclass implementations must override this method and implement whatever steps are necessary to remediate the given flow.
--
-- @flow@ — An NEFilterFlow object containing details about the flow that requires remediation.
--
-- @completionHandler@ — A block that must be called when the NEFilterControlProvider is ready for the NEFilterDataProvider to re-process the new flow. NEFilterControlVerdict stores the verdict through which the control provider determines if a flow needs to be dropped or allowed. The verdict also indicates if the control plugin wants the data plugin to update its rules and handle the verdict.
--
-- ObjC selector: @- handleRemediationForFlow:completionHandler:@
handleRemediationForFlow_completionHandler :: (IsNEFilterControlProvider neFilterControlProvider, IsNEFilterFlow flow) => neFilterControlProvider -> flow -> Ptr () -> IO ()
handleRemediationForFlow_completionHandler neFilterControlProvider  flow completionHandler =
  withObjCPtr flow $ \raw_flow ->
      sendMsg neFilterControlProvider (mkSelector "handleRemediationForFlow:completionHandler:") retVoid [argPtr (castPtr raw_flow :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | handleNewFlow:completionHandler:
--
-- This function is called by the framework when the NEFilterDataProvider indicates that the filtering verdict for the given flow is NEFilterNewFlowVerdictNeedRules. Subclass implementations must override this method and implement whatever steps are necessary to fetch new rules pertaining to the given flow and place them on disk in a location accessible by the NEFilterDataProvider.
--
-- @flow@ — An NEFilterFlow object containing details about the flow that requires a rules update.
--
-- @completionHandler@ — A block that must be called when the NEFilterControlProvider is ready for the NEFilterDataProvider to re-process the new flow. NEFilterControlVerdict stores the verdict through which the control provider determines if a flow needs to be dropped or allowed. The verdict also indicates if the control plugin wants the data plugin to update its rules and handle the verdict.
--
-- ObjC selector: @- handleNewFlow:completionHandler:@
handleNewFlow_completionHandler :: (IsNEFilterControlProvider neFilterControlProvider, IsNEFilterFlow flow) => neFilterControlProvider -> flow -> Ptr () -> IO ()
handleNewFlow_completionHandler neFilterControlProvider  flow completionHandler =
  withObjCPtr flow $ \raw_flow ->
      sendMsg neFilterControlProvider (mkSelector "handleNewFlow:completionHandler:") retVoid [argPtr (castPtr raw_flow :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | notifyRulesChanged
--
-- This function is called by filter control implementations to notify the data provider "out of band" that the rules changed.
--
-- ObjC selector: @- notifyRulesChanged@
notifyRulesChanged :: IsNEFilterControlProvider neFilterControlProvider => neFilterControlProvider -> IO ()
notifyRulesChanged neFilterControlProvider  =
    sendMsg neFilterControlProvider (mkSelector "notifyRulesChanged") retVoid []

-- | remediationMap
--
-- A dictionary containing custom strings to be inserted into the "content blocked" page displayed in WebKit. Each key in this dictionary corresponds to a string in the "content blocked" page. The value of each key is a dictionary that maps keys to the custom strings to be inserted into the "content blocked" page. The keys for the sub-dictionaries are defined by the control provider. When the data provider creates a "remediate" verdict using [NEFilterDataVerdict remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:], it passes the key corresponding to the custom string to be inserted into the "content blocked" page.
--
-- Here is a sample remediationMap dictionary:
--
-- remediationMap = \@{ NEFilterProviderRemediationMapRemediationURLs :							\@{								"RemediateKey1" : \@"http://www.remediation_url_1.com",								"RemediateKey2" : \@"http://www.remediation_url_2.com"							 },						NEFilterProviderRemediationMapRemediationButtonTexts :							\@{								"RemediationButtonText1" : \@"Remediate URL"							 }					  };
--
-- ObjC selector: @- remediationMap@
remediationMap :: IsNEFilterControlProvider neFilterControlProvider => neFilterControlProvider -> IO (Id NSDictionary)
remediationMap neFilterControlProvider  =
    sendMsg neFilterControlProvider (mkSelector "remediationMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | remediationMap
--
-- A dictionary containing custom strings to be inserted into the "content blocked" page displayed in WebKit. Each key in this dictionary corresponds to a string in the "content blocked" page. The value of each key is a dictionary that maps keys to the custom strings to be inserted into the "content blocked" page. The keys for the sub-dictionaries are defined by the control provider. When the data provider creates a "remediate" verdict using [NEFilterDataVerdict remediateVerdictWithRemediationURLMapKey:remediationButtonTextMapKey:], it passes the key corresponding to the custom string to be inserted into the "content blocked" page.
--
-- Here is a sample remediationMap dictionary:
--
-- remediationMap = \@{ NEFilterProviderRemediationMapRemediationURLs :							\@{								"RemediateKey1" : \@"http://www.remediation_url_1.com",								"RemediateKey2" : \@"http://www.remediation_url_2.com"							 },						NEFilterProviderRemediationMapRemediationButtonTexts :							\@{								"RemediationButtonText1" : \@"Remediate URL"							 }					  };
--
-- ObjC selector: @- setRemediationMap:@
setRemediationMap :: (IsNEFilterControlProvider neFilterControlProvider, IsNSDictionary value) => neFilterControlProvider -> value -> IO ()
setRemediationMap neFilterControlProvider  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neFilterControlProvider (mkSelector "setRemediationMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | URLAppendStringMap
--
-- A dictionary containing strings to be appended to URLs.
--
-- ObjC selector: @- URLAppendStringMap@
urlAppendStringMap :: IsNEFilterControlProvider neFilterControlProvider => neFilterControlProvider -> IO (Id NSDictionary)
urlAppendStringMap neFilterControlProvider  =
    sendMsg neFilterControlProvider (mkSelector "URLAppendStringMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URLAppendStringMap
--
-- A dictionary containing strings to be appended to URLs.
--
-- ObjC selector: @- setURLAppendStringMap:@
setURLAppendStringMap :: (IsNEFilterControlProvider neFilterControlProvider, IsNSDictionary value) => neFilterControlProvider -> value -> IO ()
setURLAppendStringMap neFilterControlProvider  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neFilterControlProvider (mkSelector "setURLAppendStringMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @handleRemediationForFlow:completionHandler:@
handleRemediationForFlow_completionHandlerSelector :: Selector
handleRemediationForFlow_completionHandlerSelector = mkSelector "handleRemediationForFlow:completionHandler:"

-- | @Selector@ for @handleNewFlow:completionHandler:@
handleNewFlow_completionHandlerSelector :: Selector
handleNewFlow_completionHandlerSelector = mkSelector "handleNewFlow:completionHandler:"

-- | @Selector@ for @notifyRulesChanged@
notifyRulesChangedSelector :: Selector
notifyRulesChangedSelector = mkSelector "notifyRulesChanged"

-- | @Selector@ for @remediationMap@
remediationMapSelector :: Selector
remediationMapSelector = mkSelector "remediationMap"

-- | @Selector@ for @setRemediationMap:@
setRemediationMapSelector :: Selector
setRemediationMapSelector = mkSelector "setRemediationMap:"

-- | @Selector@ for @URLAppendStringMap@
urlAppendStringMapSelector :: Selector
urlAppendStringMapSelector = mkSelector "URLAppendStringMap"

-- | @Selector@ for @setURLAppendStringMap:@
setURLAppendStringMapSelector :: Selector
setURLAppendStringMapSelector = mkSelector "setURLAppendStringMap:"

