{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterProvider
--
-- The NEFilterProvider class is an abstract base class that declares the programmatic interface of an object that implements a socket filter.
--
-- NEFilterProvider is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterProvider@.
module ObjC.NetworkExtension.NEFilterProvider
  ( NEFilterProvider
  , IsNEFilterProvider(..)
  , startFilterWithCompletionHandler
  , stopFilterWithReason_completionHandler
  , handleReport
  , filterConfiguration
  , filterConfigurationSelector
  , handleReportSelector
  , startFilterWithCompletionHandlerSelector
  , stopFilterWithReason_completionHandlerSelector

  -- * Enum types
  , NEProviderStopReason(NEProviderStopReason)
  , pattern NEProviderStopReasonNone
  , pattern NEProviderStopReasonUserInitiated
  , pattern NEProviderStopReasonProviderFailed
  , pattern NEProviderStopReasonNoNetworkAvailable
  , pattern NEProviderStopReasonUnrecoverableNetworkChange
  , pattern NEProviderStopReasonProviderDisabled
  , pattern NEProviderStopReasonAuthenticationCanceled
  , pattern NEProviderStopReasonConfigurationFailed
  , pattern NEProviderStopReasonIdleTimeout
  , pattern NEProviderStopReasonConfigurationDisabled
  , pattern NEProviderStopReasonConfigurationRemoved
  , pattern NEProviderStopReasonSuperceded
  , pattern NEProviderStopReasonUserLogout
  , pattern NEProviderStopReasonUserSwitch
  , pattern NEProviderStopReasonConnectionFailed
  , pattern NEProviderStopReasonSleep
  , pattern NEProviderStopReasonAppUpdate
  , pattern NEProviderStopReasonInternalError

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

-- | startFilterWithCompletionHandler:
--
-- This function is called by the framework when the content filter is being started. Subclasses must override this method and perform whatever steps are necessary to start the filter.
--
-- @completionHandler@ — A block that must be called when the process of starting the filter is complete. If the filter was started successfully, subclass implementations must pass the nil value to this block. If an error occurred while starting the filter, sublcass implementations must pass a non-nil NSError containing more details about the error.
--
-- ObjC selector: @- startFilterWithCompletionHandler:@
startFilterWithCompletionHandler :: IsNEFilterProvider neFilterProvider => neFilterProvider -> Ptr () -> IO ()
startFilterWithCompletionHandler neFilterProvider completionHandler =
  sendMessage neFilterProvider startFilterWithCompletionHandlerSelector completionHandler

-- | stopFilterWithReason:completionHandler:
--
-- This function is called by the framework when the content filter is being stopped. Subclasses must override this method and perform whatever steps are necessary to stop the filter.
--
-- @reason@ — An NEProviderStopReason indicating why the filter is being stopped.
--
-- @completionHandler@ — A block that must be called when the process of stopping the filter is complete.
--
-- ObjC selector: @- stopFilterWithReason:completionHandler:@
stopFilterWithReason_completionHandler :: IsNEFilterProvider neFilterProvider => neFilterProvider -> NEProviderStopReason -> Ptr () -> IO ()
stopFilterWithReason_completionHandler neFilterProvider reason completionHandler =
  sendMessage neFilterProvider stopFilterWithReason_completionHandlerSelector reason completionHandler

-- | handleReport:
--
-- This function is called by the framework when the data provider extension returns a verdict with the report property set to True.     Subclass implementations may override this method to handle the flow report.
--
-- @report@ — The report being delivered.
--
-- ObjC selector: @- handleReport:@
handleReport :: (IsNEFilterProvider neFilterProvider, IsNEFilterReport report) => neFilterProvider -> report -> IO ()
handleReport neFilterProvider report =
  sendMessage neFilterProvider handleReportSelector (toNEFilterReport report)

-- | filterConfiguration
--
-- An NEContentFilterConfiguration object containing the current filter configuration. The value of this property can change during the lifetime of a filter. Filter implementations can use KVO to be notified when the configuration changes.
--
-- ObjC selector: @- filterConfiguration@
filterConfiguration :: IsNEFilterProvider neFilterProvider => neFilterProvider -> IO (Id NEFilterProviderConfiguration)
filterConfiguration neFilterProvider =
  sendMessage neFilterProvider filterConfigurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startFilterWithCompletionHandler:@
startFilterWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startFilterWithCompletionHandlerSelector = mkSelector "startFilterWithCompletionHandler:"

-- | @Selector@ for @stopFilterWithReason:completionHandler:@
stopFilterWithReason_completionHandlerSelector :: Selector '[NEProviderStopReason, Ptr ()] ()
stopFilterWithReason_completionHandlerSelector = mkSelector "stopFilterWithReason:completionHandler:"

-- | @Selector@ for @handleReport:@
handleReportSelector :: Selector '[Id NEFilterReport] ()
handleReportSelector = mkSelector "handleReport:"

-- | @Selector@ for @filterConfiguration@
filterConfigurationSelector :: Selector '[] (Id NEFilterProviderConfiguration)
filterConfigurationSelector = mkSelector "filterConfiguration"

