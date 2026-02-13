{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterFindClientCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterFindClientCertificateParams
  ( MTRTLSCertificateManagementClusterFindClientCertificateParams
  , IsMTRTLSCertificateManagementClusterFindClientCertificateParams(..)
  , ccdid
  , setCcdid
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , ccdidSelector
  , serverSideProcessingTimeoutSelector
  , setCcdidSelector
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

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterFindClientCertificateParams mtrtlsCertificateManagementClusterFindClientCertificateParams => mtrtlsCertificateManagementClusterFindClientCertificateParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterFindClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterFindClientCertificateParams mtrtlsCertificateManagementClusterFindClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterFindClientCertificateParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterFindClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateParams setCcdidSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterFindClientCertificateParams mtrtlsCertificateManagementClusterFindClientCertificateParams => mtrtlsCertificateManagementClusterFindClientCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterFindClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterFindClientCertificateParams mtrtlsCertificateManagementClusterFindClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterFindClientCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterFindClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterFindClientCertificateParams mtrtlsCertificateManagementClusterFindClientCertificateParams => mtrtlsCertificateManagementClusterFindClientCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterFindClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterFindClientCertificateParams mtrtlsCertificateManagementClusterFindClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterFindClientCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterFindClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

