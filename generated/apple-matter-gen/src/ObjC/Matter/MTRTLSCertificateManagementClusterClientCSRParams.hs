{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterClientCSRParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterClientCSRParams
  ( MTRTLSCertificateManagementClusterClientCSRParams
  , IsMTRTLSCertificateManagementClusterClientCSRParams(..)
  , nonce
  , setNonce
  , ccdid
  , setCcdid
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , ccdidSelector
  , nonceSelector
  , serverSideProcessingTimeoutSelector
  , setCcdidSelector
  , setNonceSelector
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

-- | @- nonce@
nonce :: IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams => mtrtlsCertificateManagementClusterClientCSRParams -> IO (Id NSData)
nonce mtrtlsCertificateManagementClusterClientCSRParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams nonceSelector

-- | @- setNonce:@
setNonce :: (IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams, IsNSData value) => mtrtlsCertificateManagementClusterClientCSRParams -> value -> IO ()
setNonce mtrtlsCertificateManagementClusterClientCSRParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams setNonceSelector (toNSData value)

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams => mtrtlsCertificateManagementClusterClientCSRParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterClientCSRParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams, IsNSNumber value) => mtrtlsCertificateManagementClusterClientCSRParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterClientCSRParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams setCcdidSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams => mtrtlsCertificateManagementClusterClientCSRParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterClientCSRParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams, IsNSNumber value) => mtrtlsCertificateManagementClusterClientCSRParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterClientCSRParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams => mtrtlsCertificateManagementClusterClientCSRParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterClientCSRParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterClientCSRParams mtrtlsCertificateManagementClusterClientCSRParams, IsNSNumber value) => mtrtlsCertificateManagementClusterClientCSRParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterClientCSRParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nonce@
nonceSelector :: Selector '[] (Id NSData)
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector '[Id NSData] ()
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

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

