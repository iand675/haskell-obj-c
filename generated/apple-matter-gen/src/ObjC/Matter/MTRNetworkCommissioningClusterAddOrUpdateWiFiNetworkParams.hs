{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams
  ( MTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams
  , IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams(..)
  , ssid
  , setSsid
  , credentials
  , setCredentials
  , breadcrumb
  , setBreadcrumb
  , networkIdentity
  , setNetworkIdentity
  , clientIdentifier
  , setClientIdentifier
  , possessionNonce
  , setPossessionNonce
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , breadcrumbSelector
  , clientIdentifierSelector
  , credentialsSelector
  , networkIdentitySelector
  , possessionNonceSelector
  , serverSideProcessingTimeoutSelector
  , setBreadcrumbSelector
  , setClientIdentifierSelector
  , setCredentialsSelector
  , setNetworkIdentitySelector
  , setPossessionNonceSelector
  , setServerSideProcessingTimeoutSelector
  , setSsidSelector
  , setTimedInvokeTimeoutMsSelector
  , ssidSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams ssidSelector

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setSsid mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setSsidSelector (toNSData value)

-- | @- credentials@
credentials :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
credentials mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams credentialsSelector

-- | @- setCredentials:@
setCredentials :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setCredentials mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setCredentialsSelector (toNSData value)

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setBreadcrumbSelector (toNSNumber value)

-- | @- networkIdentity@
networkIdentity :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
networkIdentity mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams networkIdentitySelector

-- | @- setNetworkIdentity:@
setNetworkIdentity :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setNetworkIdentity mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setNetworkIdentitySelector (toNSData value)

-- | @- clientIdentifier@
clientIdentifier :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
clientIdentifier mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams clientIdentifierSelector

-- | @- setClientIdentifier:@
setClientIdentifier :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setClientIdentifier mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setClientIdentifierSelector (toNSData value)

-- | @- possessionNonce@
possessionNonce :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
possessionNonce mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams possessionNonceSelector

-- | @- setPossessionNonce:@
setPossessionNonce :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setPossessionNonce mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setPossessionNonceSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ssid@
ssidSelector :: Selector '[] (Id NSData)
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @setSsid:@
setSsidSelector :: Selector '[Id NSData] ()
setSsidSelector = mkSelector "setSsid:"

-- | @Selector@ for @credentials@
credentialsSelector :: Selector '[] (Id NSData)
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector '[Id NSData] ()
setCredentialsSelector = mkSelector "setCredentials:"

-- | @Selector@ for @breadcrumb@
breadcrumbSelector :: Selector '[] (Id NSNumber)
breadcrumbSelector = mkSelector "breadcrumb"

-- | @Selector@ for @setBreadcrumb:@
setBreadcrumbSelector :: Selector '[Id NSNumber] ()
setBreadcrumbSelector = mkSelector "setBreadcrumb:"

-- | @Selector@ for @networkIdentity@
networkIdentitySelector :: Selector '[] (Id NSData)
networkIdentitySelector = mkSelector "networkIdentity"

-- | @Selector@ for @setNetworkIdentity:@
setNetworkIdentitySelector :: Selector '[Id NSData] ()
setNetworkIdentitySelector = mkSelector "setNetworkIdentity:"

-- | @Selector@ for @clientIdentifier@
clientIdentifierSelector :: Selector '[] (Id NSData)
clientIdentifierSelector = mkSelector "clientIdentifier"

-- | @Selector@ for @setClientIdentifier:@
setClientIdentifierSelector :: Selector '[Id NSData] ()
setClientIdentifierSelector = mkSelector "setClientIdentifier:"

-- | @Selector@ for @possessionNonce@
possessionNonceSelector :: Selector '[] (Id NSData)
possessionNonceSelector = mkSelector "possessionNonce"

-- | @Selector@ for @setPossessionNonce:@
setPossessionNonceSelector :: Selector '[Id NSData] ()
setPossessionNonceSelector = mkSelector "setPossessionNonce:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

