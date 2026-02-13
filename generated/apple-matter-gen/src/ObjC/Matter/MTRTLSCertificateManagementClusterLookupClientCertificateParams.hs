{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterLookupClientCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterLookupClientCertificateParams
  ( MTRTLSCertificateManagementClusterLookupClientCertificateParams
  , IsMTRTLSCertificateManagementClusterLookupClientCertificateParams(..)
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
fingerprint :: IsMTRTLSCertificateManagementClusterLookupClientCertificateParams mtrtlsCertificateManagementClusterLookupClientCertificateParams => mtrtlsCertificateManagementClusterLookupClientCertificateParams -> IO (Id NSData)
fingerprint mtrtlsCertificateManagementClusterLookupClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateParams fingerprintSelector

-- | @- setFingerprint:@
setFingerprint :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateParams mtrtlsCertificateManagementClusterLookupClientCertificateParams, IsNSData value) => mtrtlsCertificateManagementClusterLookupClientCertificateParams -> value -> IO ()
setFingerprint mtrtlsCertificateManagementClusterLookupClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateParams setFingerprintSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterLookupClientCertificateParams mtrtlsCertificateManagementClusterLookupClientCertificateParams => mtrtlsCertificateManagementClusterLookupClientCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterLookupClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateParams mtrtlsCertificateManagementClusterLookupClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupClientCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterLookupClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterLookupClientCertificateParams mtrtlsCertificateManagementClusterLookupClientCertificateParams => mtrtlsCertificateManagementClusterLookupClientCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterLookupClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateParams mtrtlsCertificateManagementClusterLookupClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupClientCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterLookupClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

