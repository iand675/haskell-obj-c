{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterQueryIdentityParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterQueryIdentityParams
  ( MTRNetworkCommissioningClusterQueryIdentityParams
  , IsMTRNetworkCommissioningClusterQueryIdentityParams(..)
  , keyIdentifier
  , setKeyIdentifier
  , possessionNonce
  , setPossessionNonce
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , keyIdentifierSelector
  , possessionNonceSelector
  , serverSideProcessingTimeoutSelector
  , setKeyIdentifierSelector
  , setPossessionNonceSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
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

-- | @- keyIdentifier@
keyIdentifier :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSData)
keyIdentifier mtrNetworkCommissioningClusterQueryIdentityParams =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams keyIdentifierSelector

-- | @- setKeyIdentifier:@
setKeyIdentifier :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setKeyIdentifier mtrNetworkCommissioningClusterQueryIdentityParams value =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams setKeyIdentifierSelector (toNSData value)

-- | @- possessionNonce@
possessionNonce :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSData)
possessionNonce mtrNetworkCommissioningClusterQueryIdentityParams =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams possessionNonceSelector

-- | @- setPossessionNonce:@
setPossessionNonce :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setPossessionNonce mtrNetworkCommissioningClusterQueryIdentityParams value =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams setPossessionNonceSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterQueryIdentityParams =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSNumber value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterQueryIdentityParams value =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterQueryIdentityParams =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSNumber value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterQueryIdentityParams value =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyIdentifier@
keyIdentifierSelector :: Selector '[] (Id NSData)
keyIdentifierSelector = mkSelector "keyIdentifier"

-- | @Selector@ for @setKeyIdentifier:@
setKeyIdentifierSelector :: Selector '[Id NSData] ()
setKeyIdentifierSelector = mkSelector "setKeyIdentifier:"

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

