{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterProvisionClientCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterProvisionClientCertificateParams
  ( MTRTLSCertificateManagementClusterProvisionClientCertificateParams
  , IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams(..)
  , ccdid
  , setCcdid
  , clientCertificate
  , setClientCertificate
  , intermediateCertificates
  , setIntermediateCertificates
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , ccdidSelector
  , clientCertificateSelector
  , intermediateCertificatesSelector
  , serverSideProcessingTimeoutSelector
  , setCcdidSelector
  , setClientCertificateSelector
  , setIntermediateCertificatesSelector
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
ccdid :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterProvisionClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterProvisionClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams setCcdidSelector (toNSNumber value)

-- | @- clientCertificate@
clientCertificate :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSData)
clientCertificate mtrtlsCertificateManagementClusterProvisionClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams clientCertificateSelector

-- | @- setClientCertificate:@
setClientCertificate :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSData value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setClientCertificate mtrtlsCertificateManagementClusterProvisionClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams setClientCertificateSelector (toNSData value)

-- | @- intermediateCertificates@
intermediateCertificates :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSArray)
intermediateCertificates mtrtlsCertificateManagementClusterProvisionClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams intermediateCertificatesSelector

-- | @- setIntermediateCertificates:@
setIntermediateCertificates :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSArray value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setIntermediateCertificates mtrtlsCertificateManagementClusterProvisionClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams setIntermediateCertificatesSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionClientCertificateParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionClientCertificateParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionClientCertificateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @clientCertificate@
clientCertificateSelector :: Selector '[] (Id NSData)
clientCertificateSelector = mkSelector "clientCertificate"

-- | @Selector@ for @setClientCertificate:@
setClientCertificateSelector :: Selector '[Id NSData] ()
setClientCertificateSelector = mkSelector "setClientCertificate:"

-- | @Selector@ for @intermediateCertificates@
intermediateCertificatesSelector :: Selector '[] (Id NSArray)
intermediateCertificatesSelector = mkSelector "intermediateCertificates"

-- | @Selector@ for @setIntermediateCertificates:@
setIntermediateCertificatesSelector :: Selector '[Id NSArray] ()
setIntermediateCertificatesSelector = mkSelector "setIntermediateCertificates:"

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

