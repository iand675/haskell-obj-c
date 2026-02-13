{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterLookupRootCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterLookupRootCertificateParams
  ( MTRTLSCertificateManagementClusterLookupRootCertificateParams
  , IsMTRTLSCertificateManagementClusterLookupRootCertificateParams(..)
  , fingerprint
  , setFingerprint
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , fingerprintSelector
  , serverSideProcessingTimeoutSelector
  , setFingerprintSelector
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

-- | @- fingerprint@
fingerprint :: IsMTRTLSCertificateManagementClusterLookupRootCertificateParams mtrtlsCertificateManagementClusterLookupRootCertificateParams => mtrtlsCertificateManagementClusterLookupRootCertificateParams -> IO (Id NSData)
fingerprint mtrtlsCertificateManagementClusterLookupRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateParams fingerprintSelector

-- | @- setFingerprint:@
setFingerprint :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateParams mtrtlsCertificateManagementClusterLookupRootCertificateParams, IsNSData value) => mtrtlsCertificateManagementClusterLookupRootCertificateParams -> value -> IO ()
setFingerprint mtrtlsCertificateManagementClusterLookupRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateParams setFingerprintSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterLookupRootCertificateParams mtrtlsCertificateManagementClusterLookupRootCertificateParams => mtrtlsCertificateManagementClusterLookupRootCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterLookupRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateParams mtrtlsCertificateManagementClusterLookupRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupRootCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterLookupRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterLookupRootCertificateParams mtrtlsCertificateManagementClusterLookupRootCertificateParams => mtrtlsCertificateManagementClusterLookupRootCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterLookupRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateParams mtrtlsCertificateManagementClusterLookupRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupRootCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterLookupRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fingerprint@
fingerprintSelector :: Selector '[] (Id NSData)
fingerprintSelector = mkSelector "fingerprint"

-- | @Selector@ for @setFingerprint:@
setFingerprintSelector :: Selector '[Id NSData] ()
setFingerprintSelector = mkSelector "setFingerprint:"

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

