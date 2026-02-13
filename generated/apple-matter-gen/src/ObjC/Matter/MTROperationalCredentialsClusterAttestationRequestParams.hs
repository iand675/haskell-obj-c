{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAttestationRequestParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAttestationRequestParams
  ( MTROperationalCredentialsClusterAttestationRequestParams
  , IsMTROperationalCredentialsClusterAttestationRequestParams(..)
  , attestationNonce
  , setAttestationNonce
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , attestationNonceSelector
  , serverSideProcessingTimeoutSelector
  , setAttestationNonceSelector
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

-- | @- attestationNonce@
attestationNonce :: IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams => mtrOperationalCredentialsClusterAttestationRequestParams -> IO (Id NSData)
attestationNonce mtrOperationalCredentialsClusterAttestationRequestParams =
  sendMessage mtrOperationalCredentialsClusterAttestationRequestParams attestationNonceSelector

-- | @- setAttestationNonce:@
setAttestationNonce :: (IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationRequestParams -> value -> IO ()
setAttestationNonce mtrOperationalCredentialsClusterAttestationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationRequestParams setAttestationNonceSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams => mtrOperationalCredentialsClusterAttestationRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationRequestParams =
  sendMessage mtrOperationalCredentialsClusterAttestationRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterAttestationRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams => mtrOperationalCredentialsClusterAttestationRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterAttestationRequestParams =
  sendMessage mtrOperationalCredentialsClusterAttestationRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterAttestationRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterAttestationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attestationNonce@
attestationNonceSelector :: Selector '[] (Id NSData)
attestationNonceSelector = mkSelector "attestationNonce"

-- | @Selector@ for @setAttestationNonce:@
setAttestationNonceSelector :: Selector '[Id NSData] ()
setAttestationNonceSelector = mkSelector "setAttestationNonce:"

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

