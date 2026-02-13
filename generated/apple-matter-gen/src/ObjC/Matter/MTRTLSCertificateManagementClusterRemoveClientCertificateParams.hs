{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterRemoveClientCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterRemoveClientCertificateParams
  ( MTRTLSCertificateManagementClusterRemoveClientCertificateParams
  , IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams(..)
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
ccdid :: IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams mtrtlsCertificateManagementClusterRemoveClientCertificateParams => mtrtlsCertificateManagementClusterRemoveClientCertificateParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterRemoveClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterRemoveClientCertificateParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams mtrtlsCertificateManagementClusterRemoveClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterRemoveClientCertificateParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterRemoveClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterRemoveClientCertificateParams setCcdidSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams mtrtlsCertificateManagementClusterRemoveClientCertificateParams => mtrtlsCertificateManagementClusterRemoveClientCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterRemoveClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterRemoveClientCertificateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams mtrtlsCertificateManagementClusterRemoveClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterRemoveClientCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterRemoveClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterRemoveClientCertificateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams mtrtlsCertificateManagementClusterRemoveClientCertificateParams => mtrtlsCertificateManagementClusterRemoveClientCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterRemoveClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterRemoveClientCertificateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams mtrtlsCertificateManagementClusterRemoveClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterRemoveClientCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterRemoveClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterRemoveClientCertificateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

