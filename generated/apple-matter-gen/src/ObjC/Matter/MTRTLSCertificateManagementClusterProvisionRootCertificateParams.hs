{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterProvisionRootCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterProvisionRootCertificateParams
  ( MTRTLSCertificateManagementClusterProvisionRootCertificateParams
  , IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams(..)
  , certificate
  , setCertificate
  , caid
  , setCaid
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , caidSelector
  , certificateSelector
  , serverSideProcessingTimeoutSelector
  , setCaidSelector
  , setCertificateSelector
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

-- | @- certificate@
certificate :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSData)
certificate mtrtlsCertificateManagementClusterProvisionRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams certificateSelector

-- | @- setCertificate:@
setCertificate :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSData value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setCertificate mtrtlsCertificateManagementClusterProvisionRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams setCertificateSelector (toNSData value)

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterProvisionRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterProvisionRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams setCaidSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionRootCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionRootCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @certificate@
certificateSelector :: Selector '[] (Id NSData)
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector '[Id NSData] ()
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

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

