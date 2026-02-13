{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterScanNetworksResponseParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterScanNetworksResponseParams
  ( MTRNetworkCommissioningClusterScanNetworksResponseParams
  , IsMTRNetworkCommissioningClusterScanNetworksResponseParams(..)
  , initWithResponseValue_error
  , networkingStatus
  , setNetworkingStatus
  , debugText
  , setDebugText
  , wiFiScanResults
  , setWiFiScanResults
  , threadScanResults
  , setThreadScanResults
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , debugTextSelector
  , initWithResponseValue_errorSelector
  , networkingStatusSelector
  , setDebugTextSelector
  , setNetworkingStatusSelector
  , setThreadScanResultsSelector
  , setTimedInvokeTimeoutMsSelector
  , setWiFiScanResultsSelector
  , threadScanResultsSelector
  , timedInvokeTimeoutMsSelector
  , wiFiScanResultsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRNetworkCommissioningClusterScanNetworksResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> responseValue -> error_ -> IO (Id MTRNetworkCommissioningClusterScanNetworksResponseParams)
initWithResponseValue_error mtrNetworkCommissioningClusterScanNetworksResponseParams responseValue error_ =
  sendOwnedMessage mtrNetworkCommissioningClusterScanNetworksResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- networkingStatus@
networkingStatus :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSNumber)
networkingStatus mtrNetworkCommissioningClusterScanNetworksResponseParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams networkingStatusSelector

-- | @- setNetworkingStatus:@
setNetworkingStatus :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setNetworkingStatus mtrNetworkCommissioningClusterScanNetworksResponseParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams setNetworkingStatusSelector (toNSNumber value)

-- | @- debugText@
debugText :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSString)
debugText mtrNetworkCommissioningClusterScanNetworksResponseParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams debugTextSelector

-- | @- setDebugText:@
setDebugText :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSString value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setDebugText mtrNetworkCommissioningClusterScanNetworksResponseParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams setDebugTextSelector (toNSString value)

-- | @- wiFiScanResults@
wiFiScanResults :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSArray)
wiFiScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams wiFiScanResultsSelector

-- | @- setWiFiScanResults:@
setWiFiScanResults :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSArray value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setWiFiScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams setWiFiScanResultsSelector (toNSArray value)

-- | @- threadScanResults@
threadScanResults :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSArray)
threadScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams threadScanResultsSelector

-- | @- setThreadScanResults:@
setThreadScanResults :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSArray value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setThreadScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams setThreadScanResultsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterScanNetworksResponseParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterScanNetworksResponseParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRNetworkCommissioningClusterScanNetworksResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @networkingStatus@
networkingStatusSelector :: Selector '[] (Id NSNumber)
networkingStatusSelector = mkSelector "networkingStatus"

-- | @Selector@ for @setNetworkingStatus:@
setNetworkingStatusSelector :: Selector '[Id NSNumber] ()
setNetworkingStatusSelector = mkSelector "setNetworkingStatus:"

-- | @Selector@ for @debugText@
debugTextSelector :: Selector '[] (Id NSString)
debugTextSelector = mkSelector "debugText"

-- | @Selector@ for @setDebugText:@
setDebugTextSelector :: Selector '[Id NSString] ()
setDebugTextSelector = mkSelector "setDebugText:"

-- | @Selector@ for @wiFiScanResults@
wiFiScanResultsSelector :: Selector '[] (Id NSArray)
wiFiScanResultsSelector = mkSelector "wiFiScanResults"

-- | @Selector@ for @setWiFiScanResults:@
setWiFiScanResultsSelector :: Selector '[Id NSArray] ()
setWiFiScanResultsSelector = mkSelector "setWiFiScanResults:"

-- | @Selector@ for @threadScanResults@
threadScanResultsSelector :: Selector '[] (Id NSArray)
threadScanResultsSelector = mkSelector "threadScanResults"

-- | @Selector@ for @setThreadScanResults:@
setThreadScanResultsSelector :: Selector '[Id NSArray] ()
setThreadScanResultsSelector = mkSelector "setThreadScanResults:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

