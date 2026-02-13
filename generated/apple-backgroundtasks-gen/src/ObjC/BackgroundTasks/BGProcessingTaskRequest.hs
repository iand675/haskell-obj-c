{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to launch your app in the background to execute a processing task that can take minutes to complete.
--
-- Schedule a processing task request to ask that the system launch your app when conditions are favorable for battery life to handle deferrable, longer-running processing, such as syncing, database maintenance, or similar tasks. The system will attempt to fulfill this request to the best of its ability within the next two days as long as the user has used your app within the past week.
--
-- Generated bindings for @BGProcessingTaskRequest@.
module ObjC.BackgroundTasks.BGProcessingTaskRequest
  ( BGProcessingTaskRequest
  , IsBGProcessingTaskRequest(..)
  , initWithIdentifier
  , requiresNetworkConnectivity
  , setRequiresNetworkConnectivity
  , requiresExternalPower
  , setRequiresExternalPower
  , initWithIdentifierSelector
  , requiresExternalPowerSelector
  , requiresNetworkConnectivitySelector
  , setRequiresExternalPowerSelector
  , setRequiresNetworkConnectivitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Return a new processing task request for the specified identifier.
--
-- - Parameters:     - identifier: The string identifier of the processing task associated with the request.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsBGProcessingTaskRequest bgProcessingTaskRequest, IsNSString identifier) => bgProcessingTaskRequest -> identifier -> IO (Id BGProcessingTaskRequest)
initWithIdentifier bgProcessingTaskRequest identifier =
  sendOwnedMessage bgProcessingTaskRequest initWithIdentifierSelector (toNSString identifier)

-- | A Boolean specifying if the processing task requires network connectivity.
--
-- If this property is set to YES, the system will only launch your app to fulfill this request when the device has a network connection. If this is set to NO, your app may not have network access. - Note: The default value is @NO@.
--
-- ObjC selector: @- requiresNetworkConnectivity@
requiresNetworkConnectivity :: IsBGProcessingTaskRequest bgProcessingTaskRequest => bgProcessingTaskRequest -> IO Bool
requiresNetworkConnectivity bgProcessingTaskRequest =
  sendMessage bgProcessingTaskRequest requiresNetworkConnectivitySelector

-- | A Boolean specifying if the processing task requires network connectivity.
--
-- If this property is set to YES, the system will only launch your app to fulfill this request when the device has a network connection. If this is set to NO, your app may not have network access. - Note: The default value is @NO@.
--
-- ObjC selector: @- setRequiresNetworkConnectivity:@
setRequiresNetworkConnectivity :: IsBGProcessingTaskRequest bgProcessingTaskRequest => bgProcessingTaskRequest -> Bool -> IO ()
setRequiresNetworkConnectivity bgProcessingTaskRequest value =
  sendMessage bgProcessingTaskRequest setRequiresNetworkConnectivitySelector value

-- | Whether the background task represented by this request should only be done while the device is connected to external power.
--
-- If this property is set to @YES@, the system will launch your app to fulfill this request only while the device is connected to external power. Setting this to @YES@ will also disable the CPU Monitor feature. Specify @YES@ if this task is resource intensive to minimize impact to battery life. Please note that, even if this value is @NO@, the system will not necessarily schedule this task while the device is on battery power, depending on the type of device and system conditions. - Note: The default value is @NO@.
--
-- ObjC selector: @- requiresExternalPower@
requiresExternalPower :: IsBGProcessingTaskRequest bgProcessingTaskRequest => bgProcessingTaskRequest -> IO Bool
requiresExternalPower bgProcessingTaskRequest =
  sendMessage bgProcessingTaskRequest requiresExternalPowerSelector

-- | Whether the background task represented by this request should only be done while the device is connected to external power.
--
-- If this property is set to @YES@, the system will launch your app to fulfill this request only while the device is connected to external power. Setting this to @YES@ will also disable the CPU Monitor feature. Specify @YES@ if this task is resource intensive to minimize impact to battery life. Please note that, even if this value is @NO@, the system will not necessarily schedule this task while the device is on battery power, depending on the type of device and system conditions. - Note: The default value is @NO@.
--
-- ObjC selector: @- setRequiresExternalPower:@
setRequiresExternalPower :: IsBGProcessingTaskRequest bgProcessingTaskRequest => bgProcessingTaskRequest -> Bool -> IO ()
setRequiresExternalPower bgProcessingTaskRequest value =
  sendMessage bgProcessingTaskRequest setRequiresExternalPowerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id BGProcessingTaskRequest)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @requiresNetworkConnectivity@
requiresNetworkConnectivitySelector :: Selector '[] Bool
requiresNetworkConnectivitySelector = mkSelector "requiresNetworkConnectivity"

-- | @Selector@ for @setRequiresNetworkConnectivity:@
setRequiresNetworkConnectivitySelector :: Selector '[Bool] ()
setRequiresNetworkConnectivitySelector = mkSelector "setRequiresNetworkConnectivity:"

-- | @Selector@ for @requiresExternalPower@
requiresExternalPowerSelector :: Selector '[] Bool
requiresExternalPowerSelector = mkSelector "requiresExternalPower"

-- | @Selector@ for @setRequiresExternalPower:@
setRequiresExternalPowerSelector :: Selector '[Bool] ()
setRequiresExternalPowerSelector = mkSelector "setRequiresExternalPower:"

